# install.packages(c("ggplot2", "dplyr", "FNN", "np", "patchwork"))
library(ggplot2)
library(dplyr)
library(FNN)       # kNN regression
library(np)        # kernel regression
library(tidyr)
library(mgcv)

## 1. load+clean data
data("airquality") 
aq <- airquality %>%
  select(Ozone, Solar.R, Wind, Temp, Month, Day) %>%
  na.omit()

str(aq)
summary(aq)

## GOAL: predict ground-level Ozones for meteorological variables.

## 2. exploring data
## correlation matrix for continuous vars
cor(aq[, c("Ozone", "Solar.R", "Wind", "Temp")])

## histograms for each variable
pOZ   <- ggplot(aq, aes(x = Ozone))+
  geom_histogram(bins = 30, fill = "grey70")+
  theme_minimal() +
  labs(title = "Ozone", x = "Ozone (ppb)")+
  theme(panel.border = element_rect(NA, "black", 1), 
        panel.grid = element_blank())
pSOL  <- ggplot(aq, aes(x = Solar.R))+
  geom_histogram(bins = 30, fill = "grey70")+
  theme_minimal() +
  labs(title = "Solar.R", x = "Solar radiation")+
  theme(panel.border = element_rect(NA, "black", 1), 
        panel.grid = element_blank())
pWIND <- ggplot(aq, aes(x = Wind))+
  geom_histogram(bins = 30, fill = "grey70")+theme_minimal() +
  labs(title = "Wind", x = "Wind speed (mph)")+
  theme(panel.border = element_rect(NA, "black", 1), 
        panel.grid = element_blank())
pTEMP <- ggplot(aq, aes(x = Temp))+
  geom_histogram(bins = 30, fill = "grey70")+
  theme_minimal() +
  labs(title = "Temp", x = "Temperature (F)")+
  theme(panel.border = element_rect(NA, "black", 1), 
        panel.grid = element_blank())

pOZ
pSOL
pWIND
pTEMP


# ozone quartile groups
aqDENSITY <- aq %>%
  mutate(
    Ozone_group = cut(
      Ozone,
      breaks = quantile(Ozone, 
                        probs = seq(0, 1, by = 0.25), 
                        na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("Q1 (low)", "Q2", "Q3", "Q4 (high)")
    )
  )

predVARS <- c("Solar.R", "Wind", "Temp")
aqLONG <- aqDENSITY %>%
  pivot_longer(
    cols = all_of(predVARS),
    names_to = "variable",
    values_to = "value"
  )

# faceted density plots of predictors
ggplot(aqLONG, aes(x = value, colour = Ozone_group)) +
  geom_density() +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(
    title = "Predictor distributions by Ozone level",
    x = "Predictor Value",
    y = "Density",
    colour = "Ozone Group"
  )+
  theme(panel.border = element_rect(NA, "black", 1), 
        panel.grid = element_blank(),
        legend.position = "bottom")


aqLONGscatter <- aq %>%
  pivot_longer(
    cols = all_of(predVARS),
    names_to = "variable",
    values_to = "value"
  )

ggplot(aqLONGscatter, aes(x = value, y = Ozone)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE) +
  facet_wrap(~ variable, scales = "free_x") +
  theme_minimal() +
  labs(
    title = "Ozone vs Predictors w/ Smoothers",
    x = "Predictor Value",
    y = "Ozone (ppb)"
  )+
  theme(panel.border = element_rect(NA, "black", 1),
        panel.grid = element_blank())


## 3. Train/test split
set.seed(112025)
n      <- nrow(aq)
trainIDX <- sample(seq_len(n), size = round(0.7*n))
train <- aq[trainIDX, ]
test  <- aq[-trainIDX, ]

# nrow(train); nrow(test)


## 4. utilities
rmse <- function(y, yhat) sqrt(mean((y-yhat)^2))
r2 <- function(y, yhat) 1-sum((y-yhat)^2)/sum((y-mean(y))^2)


## 5. baseline: Linear regression
## Ozone ~ Wind+Temp+Solar.R+Month
lmMod <- lm(Ozone ~ Wind+Temp+Solar.R+factor(Month), data = train)
summary(lmMod)
## month is not statistically significant so remove it moving forward

lmTrainPred <- predict(lmMod, newdata = train)
lmTestPred  <- predict(lmMod, newdata = test)

data.frame(
  dataset = c("train", "test"),
  RMSE    = c(rmse(train$Ozone, lmTrainPred),
              rmse(test$Ozone,  lmTestPred)),
  R2      = c(r2(train$Ozone, lmTrainPred),
              r2(test$Ozone,  lmTestPred))
)


## 6. k-Nearest Neighbour (kNN) regression
## kNN needs numeric matrix; scale predictors
predVARS <- c("Wind", "Temp", "Solar.R")
scalingFun <- function(df, cols) {
  mu <- sapply(df[, cols, drop = FALSE], mean)
  sd <- sapply(df[, cols, drop = FALSE], sd)
  x  <- scale(df[, cols, drop = FALSE], 
              center = mu, scale = sd)
  list(x = x, mu = mu, sd = sd)
}

scaleTrain <- scalingFun(train, predVARS)
xTrain  <- scaleTrain$x
xTest <- scale(test[, predVARS, drop = FALSE],
               center = scaleTrain$mu,
               scale  = scaleTrain$sd)
yTrain <- train$Ozone
yTest  <- test$Ozone

## line search k-values to find the best one to minimize RMSE
kGrid <- c(1:nrow(xTrain))

knnGridRes <- lapply(kGrid, function(k) {
  out <- knn.reg(train = xTrain, 
                 test = xTest, 
                 y = yTrain, 
                 k = k)
  pred <- out$pred
  data.frame(
    k    = k,
    RMSE = rmse(yTest, pred),
    R2   = r2(yTest, pred)
  )
})

knnGridSum <- do.call(rbind, knnGridRes)
knnGridSum

plot(knnGridSum$k, knnGridSum$RMSE, 
     type = "l", xlab = "k", ylab = "RMSE")
points(knnGridSum$k, knnGridSum$RMSE)
abline(v = knnGridSum$k[which.min(knnGridSum$RMSE)], 
       lty = "dashed", col = "red")
text(x = knnGridSum$k[which.min(knnGridSum$RMSE)]+25, 
     y = mean(c(max(knnGridSum$RMSE),
                min(knnGridSum$RMSE))) + 5, 
     labels = paste0("At k = ", 
                     knnGridSum$k[which.min(knnGridSum$RMSE)], 
                     " the RMSE is ", 
                     round(min(knnGridSum$RMSE),3)),
     col = "red")

plot(knnGridSum$k, knnGridSum$R2, 
     type = "l", xlab = "k", ylab = "R2")
points(knnGridSum$k, knnGridSum$R2)
abline(v = knnGridSum$k[which.max(knnGridSum$R2)], 
       lty = "dashed", col = "red")
text(x = knnGridSum$k[which.max(knnGridSum$R2)]+8, 
     y = mean(c(max(knnGridSum$R2),min(knnGridSum$R2))), 
     labels = paste0("At k = ", 
                     knnGridSum$k[which.max(knnGridSum$R2)], 
                     " the R2 is ", 
                     round(max(knnGridSum$R2),3)),
     col = "red")

## choose best k by RMSE
bestkIDX <- which.min(knnGridSum$RMSE)
bestK     <- knnGridSum$k[bestkIDX]
bestK

## refit best k to get predictions
knnMod <- knn.reg(train = xTrain, test = xTest, y = yTrain, k = bestK)
knnTestPred <- knnMod$pred
plot(yTest, knnTestPred, xlab="y", ylab=expression(hat(y)))
abline(a = 0, b = 1, col = "red")


# Comparing kernel functions
x <- seq(-3, 3, length.out = 200)
# Gaussian kernel
gaussian <- dnorm(x)
# Epanechnikov kernel
epanech <- ifelse(abs(x) <= sqrt(5), 
                  3/(4*sqrt(5)) * (1 - x^2/5), 
                  0)
# Uniform kernel
uniform <- ifelse(abs(x) <= 1, 0.5, 0)
plot(x, uniform, type = "l", lwd = 2,
     xlab = "Distance from target", 
     ylab = "Weight",
     main = "Kernel Functions",
     col = "red") +
lines(x, epanech, col = "blue", lwd = 2) +
lines(x, gaussian, col = "black", lwd = 2) +
legend("topright", 
       c("Gaussian", "Epanechnikov", "Uniform"),
       col = c("black", "blue", "red"), 
       lwd = 2)

set.seed(11262025)
n <- 50
x <- seq(0, 2*pi, length.out = n)
y <- sin(x) + rnorm(n, 0, 0.3)
bwVALS <- c(0.1, 0.5, 2)
xGRID <- seq(0, 2*pi, length.out = 100)
plot(x, y, pch = 16, col = "gray",
     main = "Effect of Bandwidth",
     xlab = "x", ylab = "y")
colors <- c("blue", "black", "red")
for(i in 1:3) {
  h <- bwVALS[i]
  ySMOOTH <- ksmooth(x, y, 
                     bandwidth = h,
                     x.points = xGRID)
  lines(ySMOOTH, col = colors[i], lwd = 2)
}
legend("topright", 
       c("h = 0.1",
         "h = 0.5", 
         "h = 2.0"),
       col = colors, lwd = 2)

## 7. Kernel regression (npreg)
set.seed(112025)
npMod <- npreg(
  tydat  = train$Ozone,
  txdat  = train[, predVARS, drop = FALSE],
  regtype  = "ll",       
  ckertype = "gaussian",
  newdata = xTest,
  y.eval = yTest,
  gradients = TRUE
)

plot(npMod$bws, plot.errors.method = "bootstrap")
summary(npMod)  

npTrainPred <- predict(npMod, exdat = train[, predVARS, drop = FALSE])
npTestPred  <- predict(npMod, exdat = test[,  predVARS, drop = FALSE])

data.frame(
  dataset = c("train", "test"),
  RMSE    = c(rmse(train$Ozone, npTrainPred),
              rmse(test$Ozone,  npTestPred)),
  R2      = c(r2(train$Ozone, npTrainPred),
              r2(test$Ozone,  npTestPred))
)


## 8. Sliced effects: how do models capture Ozone vs Temp?
## construct a slice at typical Wind and Solar.R and compare
medianWind <- median(aq$Wind)
medianSolar <- median(aq$Solar.R)
tempGrid <- seq(min(aq$Temp), max(aq$Temp), length.out = 100)
slideDF <- data.frame(
  Temp    = tempGrid,
  Wind    = medianWind,
  Solar.R = medianSolar,
  Month   = factor(7) # arbitrary factor for month
)

## lm prediction
slideDF$lmPreds <- predict(lmMod, newdata = slideDF)
## knn prediction
sliceX <- scale(slideDF[, predVARS, drop = FALSE],
                center = scaleTrain$mu,
                scale  = scaleTrain$sd)
sliceKNN <- knn.reg(
  train = xTrain,
  test  = sliceX,
  y     = yTrain,
  k     = bestK
)
slideDF$knnPreds <- sliceKNN$pred

## kernel regression prediction
slideDF$npPreds <- predict(npMod, exdat = slideDF[, predVARS, drop = FALSE])

ggplot(slideDF, aes(x = Temp)) +
  geom_line(aes(y = lmPreds),  linetype = "solid") +
  geom_line(aes(y = knnPreds), linetype = "dashed") +
  geom_line(aes(y = npPreds),  linetype = "dotdash") +
  theme_minimal() +
  labs(title = "Ozone vs Temp\n(slice at typical Wind & Solar.R)",
       x = "Temperature (F)",
       y = "Predicted Ozone") +
  annotate("text", x = min(tempGrid)+1, 
           y = max(slideDF$lmPreds, na.rm=TRUE),
           label = "Solid: LM\nDashed: kNN\nDotdash: kernel",
           hjust = 0)+
  theme(panel.border = element_rect(NA, "black", 1), 
        panel.grid = element_blank())



## 9. knn+kernel effect curves 
## bootstrap for knn, analytical standard error for kernel
vars <- c("Temp","Wind","Solar.R")
B   <- 1000 # number of bootstrap samples

for (v in vars) {
  grid <- seq(min(aq[[v]]), max(aq[[v]]), 
              length.out = 1000)
  base <- data.frame(
    Temp    = median(aq$Temp),
    Wind    = median(aq$Wind),
    Solar.R = median(aq$Solar.R)
  )[rep(1,1000),]
  base[[v]] <- grid
  ## kernel prediction and conf intervals
  outNP  <- predict(npMod, 
                    exdat = base[predVARS], 
                    se.fit = TRUE)
  meanNP <- outNP$fit
  lowNP  <- meanNP-1.96*outNP$se.fit
  highNP <- meanNP+1.96*outNP$se.fit
  ## knn bootstrap and estimated error bars
  scaledBASE <- scale(base[predVARS],
                      center = scaleTrain$mu,
                      scale  = scaleTrain$sd)
  bootMAT <- matrix(NA, nrow = B, ncol = length(grid))
  set.seed(112025)
  for (b in 1:B) {
    bIDX <- sample(seq_along(yTrain), replace = TRUE)
    Xb    <- xTrain[bIDX, ] 
    yb    <- yTrain[bIDX]
    bootMAT[b, ] <- knn.reg(train = Xb, 
                            test = scaledBASE, 
                            y = yb, 
                            k = bestK)$pred
  }
  meanKNN <- colMeans(bootMAT)
  lowKNN  <- apply(bootMAT, 2, quantile, .025)
  highKNN <- apply(bootMAT, 2, quantile, .975)
  
  p <- ggplot() +
    geom_point(data = aq, aes_string(x=v, y="Ozone"),
               alpha=.3, colour="grey60") +
    geom_ribbon(aes(x=grid, ymin=lowNP, ymax=highNP),
                fill="steelblue", alpha=.22) +
    geom_line(aes(x=grid, y=meanNP),
              colour="steelblue", size=1.05) +
    geom_ribbon(aes(x=grid, ymin=lowKNN, ymax=highKNN),
                fill="tomato",  alpha=.18) +
    geom_line(aes(x=grid, y=meanKNN), 
              colour="tomato", size=1.05, linetype="dashed") +
    theme_minimal() +
    labs(
      title = paste0("Estimated Effect of ", v, " on Ozone"),
      subtitle = "Blue = Kernel (analytic CI) | Red = kNN (bootstrap CI)",
      x = v,
      y = "Predicted Ozone"
    )+
    theme(panel.border = element_rect(NA, "black", 1), 
          panel.grid = element_blank())
  print(p)
}


###### FINAL SUMMARY #######
## Model comparison table: LM, GAM, kNN, Kernel (npreg)
gamMod <- gam(
  Ozone ~ s(Wind)+s(Temp)+s(Solar.R)+factor(Month),
  data   = train,
  method = "REML"
)
summary(gamMod)
gamTrainPred <- predict(gamMod, newdata = train)
gamTestPred  <- predict(gamMod, newdata = test)
trainKNN <- knn.reg(train = xTrain, test = xTrain, 
                    y = yTrain, k = bestK)$pred
knn_test  <- knn.reg(train = xTrain, test = xTest,  
                     y = yTrain, k = bestK)$pred
npTrainPred <- predict(npMod, exdat = train[, predVARS, drop = FALSE])
npTestPred  <- predict(npMod, exdat = test[,  predVARS, drop = FALSE])
metrics <- function(y, yhat) {
  rmse <- sqrt(mean((y-yhat)^2))
  mae  <- mean(abs(y-yhat))
  r2   <- 1-sum((y-yhat)^2)/sum((y-mean(y))^2)
  cor_ <- suppressWarnings(cor(y, yhat))  
  c(RMSE = rmse, MAE  = mae,
    R2   = r2, COR  = cor_)
}
finRESULTS <- list(
  data.frame(
    Model   = "LM",
    Dataset = "Train",
    t(metrics(train$Ozone, lmTrainPred))
  ),
  data.frame(
    Model   = "LM",
    Dataset = "Test",
    t(metrics(test$Ozone, lmTestPred))
  ),
  
  data.frame(
    Model   = "GAM",
    Dataset = "Train",
    t(metrics(train$Ozone, gamTrainPred))
  ),
  data.frame(
    Model   = "GAM",
    Dataset = "Test",
    t(metrics(test$Ozone, gamTestPred))
  ),
  
  data.frame(
    Model   = paste0("kNN (k=", bestK, ")"),
    Dataset = "Train",
    t(metrics(train$Ozone, trainKNN))
  ),
  data.frame(
    Model   = paste0("kNN (k=", bestK, ")"),
    Dataset = "Test",
    t(metrics(test$Ozone, knn_test))
  ),
  
  data.frame(
    Model   = "Kernel (npreg)",
    Dataset = "Train",
    t(metrics(train$Ozone, npTrainPred))
  ),
  data.frame(
    Model   = "Kernel (npreg)",
    Dataset = "Test",
    t(metrics(test$Ozone, npTestPred))
  )
)

modelCOMPARE <- do.call(rbind, finRESULTS)
modelCOMPARE <- modelCOMPARE %>%
  mutate(
    RMSE = round(RMSE, 2),
    MAE  = round(MAE,  2),
    R2   = round(R2,   3),
    COR  = round(COR,  3)
  )
modelCOMPARE


