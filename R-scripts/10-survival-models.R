# install.packages(c("survival", "survminer", "dplyr", "ggplot2", "broom", "pec"))
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)
library(broom)
library(pec)

set.seed(20251122)


## 1. Simulated Data
## Features:
## 1. 3 plants with different baseline reliability (cluster effect)
## 2. Continuous covariates: temperature, load
## 3. Categorical: material (A/B), maintenance program (0/1)
## Right censoring, true model has a plant-level random effect (frailty)


numberOFplants <- 3
plant    <- factor(sample(paste0("P", 1:numberOFplants), size = 500, replace = TRUE))
temp     <- rnorm(500, mean = 25, sd = 4)         # degrees C above baseline
load     <- runif(500, 0.7, 1.3)                  # design load
material <- factor(sample(c("A","B"), 500, TRUE)) # material it was made from
maint    <- rbinom(500, 1, 0.5)                   # maintenance program (think of regular or sparse)

## plant frailty: log-normal random effect on hazard
## this can be though of as unobserved plant-level stressors
plantsEFFECT <- rnorm(numberOFplants, mean = 0, sd = 0.5)
names(plantsEFFECT) <- paste0("P", 1:numberOFplants)

## baseline hazards and their respective coefficients
lambda0 <- 0.005          # baseline rate
baselineTEMP  <- 0.04     # per °C
baselineLOAD  <- 1.2
baselineMATERIALB  <- -0.5
baselineMAINTENANCE <- -0.7

## predictor for the hazard occurring
linpred <- baselineTEMP*(temp-mean(temp)) +
  baselineLOAD*(load-1) +
  baselineMATERIALB*(material=="B") +
  baselineMAINTENANCE*maint +
  plantsEFFECT[plant]
lambdaI <- lambda0*exp(linpred)

## we will model with exponential survival times
U <- runif(500)
trueTIME <- -log(U)/lambdaI

## Random censoring between 40 and 120 months
censorTIME <- runif(500, 40, 120)
time   <- pmin(trueTIME, censorTIME)
status <- as.integer(trueTIME <= censorTIME)  # 1=failure, 0=censored

dat <- data.frame(
  pump_id  = 1:500,
  plant,
  time,
  status,
  temp,
  load,
  material,
  maint = factor(maint)
)

head(dat)


## 2. KM by plant
survivalOBJECT <- Surv(dat$time, dat$status)
kmPLANT <- survfit(survivalOBJECT ~ plant, data = dat)

ggsurvplot(
  kmPLANT,
  conf.int   = TRUE,
  risk.table = TRUE,
  ggtheme    = theme_minimal(),
  xlab       = "Time since installation (months)",
  ylab       = "Survival probability",
  title      = "Kaplan-Meier Curves by Plant"
)


## 3. Cox PH model
coxBASIC <- coxph(
  Surv(time, status) ~ scale(temp)+scale(load)+material+maint,
  data = dat,
  x = TRUE, 
  y = TRUE
)
summary(coxBASIC)
broom::tidy(coxBASIC, exponentiate = TRUE, conf.int = TRUE)

## check of the proportional hazards assumption (PH)
coxBASIC_zph <- cox.zph(coxBASIC)
coxBASIC_zph
par(mfrow = c(4,1))
plot(coxBASIC_zph)   
dev.off()

## 4. Cox model with plant-level frailty
coxFRAILTY <- coxph(
  Surv(time, status) ~ scale(temp)+scale(load)+material+maint + frailty(plant),
  data = dat,
  x = TRUE, 
  y = TRUE
)
summary(coxFRAILTY)

## compare log-likelihood/AIC with and without frailty
AIC(coxBASIC, coxFRAILTY)

## random effect estimates (frailties)
frailtyVALS <- data.frame(
  plant = c("p1", "p2", "p3"),
  frailty = coxFRAILTY$coefficients[c("gamma:P1","gamma:P2","gamma:P3")]
)
frailtyVALS

ggplot(frailtyVALS, aes(x = plant, y = frailty)) +
  geom_col() +
  theme_minimal() +
  labs(
    x = "Plant",
    y = "Estimated frailty (log hazard multiplier)",
    title = "Estimated Plant-Level Frailties (Cox Model)"
  )


## 5. non-proportional hazards: time-varying effect
## suppose diagnostic suggested load has time-varying effect.
## We will now allow an effect of load that changes ~ log(time).
## tt() gets covariate*function of time; 
## here: load*log(t)

coxTV <- coxph(
  Surv(time, status) ~ scale(temp)+material+maint+tt(load),
  data = dat,
  tt = function(x, t, ...) x*log(t+1) 
)
summary(coxTV)

## the coefficient of tt(load) describes how effect of load
## increases/decreases over log(time). 
## In reality, this is interpreted as load matters more/less as pumps age.


## 6. Parametric models: Weibull and Lognormal
weibullAFT <- survreg(
  Surv(time, status) ~ temp+load+material+maint,
  data = dat,
  dist = "weibull"
)

lognormalAFT <- survreg(
  Surv(time, status) ~ temp+load+material+maint,
  data = dat,
  dist = "lognormal"
)

summary(weibullAFT)
summary(lognormalAFT)

## compare AIC
AIC(weibullAFT, lognormalAFT)

## convert AFT coefficients to time ratios (exp on log-time scale)
weibullAFT_timeRatio <- broom::tidy(weibullAFT) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    timeRatio = exp(estimate),
    lower      = exp(estimate-1.96*std.error),
    upper      = exp(estimate+1.96*std.error)
  )
weibullAFT_timeRatio

## plot time ratios with CI 
ggplot(weibullAFT_timeRatio,
       aes(x = term, y = timeRatio, ymin = lower, ymax = upper)) +
  geom_pointrange() +
  coord_flip() +
  theme_minimal() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(
    x = "Covariate",
    y = "Time ratio (Weibull AFT)",
    title = "AFT (Time Ratios)"
  )


## 7. Weibull and KM visual comparison
## we will look at the profile associated with
## Plant P1, material B, maint=1, avg temp/load
profile <- data.frame(
  temp     = mean(dat$temp),
  load     = mean(dat$load),
  material = factor("B", levels = levels(dat$material)),
  maint    = factor(1, levels = levels(dat$maint))
)

## Predicted median survival time (months)
predict(weibullAFT, newdata = profile, type = "quantile", p = 0.5)

## Weibull survival curve from survreg params
t_grid <- seq(1, max(dat$time), length.out = 200)
xb    <- predict(weibullAFT, newdata = profile, type = "lp")   
sigma <- weibullAFT$scale                                     
survivalWeibull <- exp(-(t_grid*exp(-xb))^(1/sigma))
weibullDF <- data.frame(time = t_grid, S = survivalWeibull)

kmSUBSET <- survfit(
  Surv(time, status) ~ 1,
  data = dat %>% filter(material == "B", maint == 1)
)

gKM <- ggsurvplot(
  kmSUBSET,
  conf.int   = TRUE,
  risk.table = FALSE,
  ggtheme    = theme_minimal(),
  xlab       = "Time since installation (months)",
  ylab       = "Survival probability",
  title      = "KM vs Weibull AFT (Material B, Maintained)"
)

gKM$plot +
  geom_line(data = weibullDF, aes(x = time, y = S), linetype = "dashed") +
  annotate("text",
           x = max(dat$time)*0.6,
           y = 0.25,
           label = "Dashed is Weibull AFT")


## 8. Piecewise Exponential Model 
## We'll split follow-up time into 0–40, 40–80, 80+ months
cuts <- c(0, 40, 80, Inf)

datSPLIT <- survSplit(
  Surv(time, status) ~ .,
  data = dat,
  cut = cuts[-c(1, length(cuts))],
  episode = "interval"
)

## interval factor labels
datSPLIT$interval <- factor(datSPLIT$interval,
                             labels = c("0-40", "40-80", "80+"))

## poisson regression on event counts with log(time) offset
poissonWeightedGLM <- glm(
  status ~ interval+temp+load+material+maint,
  family = poisson(),
  offset = log(time),
  data = datSPLIT
)
summary(poissonWeightedGLM)

## coefficients conversion
pweRR <- broom::tidy(poissonWeightedGLM) %>%
  mutate(
    rate_ratio = exp(estimate),
    lower      = exp(estimate-1.96*std.error),
    upper      = exp(estimate+1.96*std.error)
  )

pweRR
## interval-specific baseline rates
intervalEFFECTS <- pweRR %>%
  filter(grepl("^interval", term))

ggplot(intervalEFFECTS,
       aes(x = term, y = rate_ratio, ymin = lower, ymax = upper)) +
  geom_pointrange() +
  coord_flip() +
  theme_minimal() +
  labs(
    x = "Interval",
    y = "Rate ratio versus reference",
    title = "Failure Rate by Time Interval"
  )

## 9. Which model predicts best?
## Cox models use partial likelihood AIC; AFT models use full likelihood AIC.
AIC(coxBASIC, coxFRAILTY, weibullAFT, lognormalAFT)

## Predictive accuracy with Harrell’s C-index
## C-index measures rank-order predictive accuracy (0.5 = random, 1.0 = perfect)
cINDEX <- function(fit, data) {
  s <- Surv(data$time, data$status)
  lp <- predict(fit, newdata = data, type = "lp")
  conc <- concordance(s ~ lp)$concordance
  return(conc)
}

c_basic   <- cINDEX(coxBASIC,   dat)
c_frail   <- cINDEX(coxFRAILTY, dat)
c_weibull <- cINDEX(weibullAFT,    dat)
c_logn    <- cINDEX(lognormalAFT,    dat)
data.frame(
  model = c("Cox (no frailty)", "Cox (frailty)", "Weibull AFT", "Lognormal AFT"),
  cINDEX = c(c_basic, c_frail, c_weibull, c_logn)
)


## Brier score (prediction error): absolute prediction error at time t.
## still figuring out how to do this for the other two models
models <- list(
  coxBASIC   = coxBASIC,
  coxFRAILTY = coxFRAILTY
)

## prediction error curves
pecFIT <- pec(
  object  = models,
  formula = Surv(time, status) ~ 1,
  data    = dat,
  times   = quantile(dat$time, probs = seq(0.1, 0.9, by = 0.1))
)

plot(pecFIT,
     xlab = "Time (months)",
     ylab = "Prediction error",
     legend = TRUE,
     main = "Prediction Error Curves"
)


