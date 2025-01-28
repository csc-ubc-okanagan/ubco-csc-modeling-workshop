#' *Workshop 1: Simple linear regression*
#' Copyright 2025 Stefano Mezzini
#' Published under the MIT licence
#' https://github.com/csc-ubc-okanagan/ubco-csc-modeling-workshop/tree/main
#' https://events.ok.ubc.ca/series/fitting-models-to-data-not-data-to-models-workshop-series/
library('dplyr')   # for data wrangling
library('mgcv')    # for modeling
library('ggplot2') # for fancy plots
library('gratia')  # for ggplot-based model graphics
theme_set(theme_classic(base_size = 15))

set.seed(0)
d0 <- tibble(x = runif(n = 100), # predictor of Y
             mu = 4 - 3 * x, # true mean of Y
             e = rnorm(n = length(x), mean = 0, sd = 1), # Gaussian error
             Y = mu + e) # values of Y

# plotting data with the true trend
ggplot(d0) +
  geom_line(aes(x, mu), col = 'red', lwd = 1) +
  geom_point(aes(x, Y)) +
  labs(x = 'Predictor (indepedent variable), x',
       y = 'Response (dependent variable), Y',
       title = expression('Y = 4 - 3x +'~epsilon))

# Assumptions of Linear Models (i.e., ANOVAs) ----
#' 1. *Certainty in x*: unlike Y, there is no error or uncertainty in x.
ggplot(d0) +
  geom_errorbar(aes(x, ymin = Y - 1, ymax = Y + 1), color = 'grey') +
  geom_point(aes(x, Y)) +
  labs(x = 'Predictor (indepedent variable), x',
       y = 'Response (dependent variable), Y',
       title = expression('\U1D53C(Y) ='~mu~'but \U1D53C(x) = x'))

#' 2. *Linearity*: The relationship between X and the mean of Y is linear.
ggplot(d0) +
  geom_line(aes(x, mu), col = 'red', lwd = 1) +
  geom_point(aes(x, Y)) +
  labs(x = 'Predictor (indepedent variable), x',
       y = 'Response (dependent variable), Y',
       title = expression('\U1D53C(Y) ='~mu~'='~beta[0]~+~beta[1]~x))

#' 3. *Homoscedasticity*: The variance of the residuals is constant.
ggplot(d0) +
  geom_hline(yintercept = 0, color = 'grey') +
  geom_smooth(aes(x, e), col = 'darkorange', lwd = 1, method = 'lm',
              formula = y ~ x, se = TRUE) +
  geom_point(aes(x, e)) +
  labs(x = 'Predictor (indepedent variable), x',
       y = expression(Residuals~(e~'='~Y~-~hat(mu))),
       title = expression('Var(Y) ='~sigma^2))

#' 4. *Independence*: Observations are independent of each other.
ggplot(d0, aes(seq(nrow(d0)), Y)) +
  geom_point() +
  geom_path(alpha = 0.1) +
  labs(x = 'Observation order',
       y = 'Response (dependent variable), Y',
       title = expression(Y[italic(i)]~'\U2AEB'~Y[italic(j)]~
                            'for'~italic(i)~'\U2260'~italic(j)))

#' 5. *Normality*: For a given value of x, the error in Y is Gaussian.
#' (This implies Y is also Gaussian)
ggplot(d0, aes(e)) +
  geom_histogram(color = 'black', fill = 'grey', bins = 8) +
  geom_vline(xintercept = 0, color = 'red') +
  labs(x = expression('Residuals,'~e~'='~Y~-~hat(mu)),
       y = 'Count',
       title = expression(epsilon~'~'~N('0,'~sigma^2)))

# Applying Simple Linear Regression ----
# what is the variance in Y if we don't know about x?
# e.g. "How variable is height between people?"
ggplot(d0, aes(Y)) +
  geom_histogram(color = 'black', fill = 'grey', bins = 8) +
  geom_rug() +
  labs(x = 'Response (dependent variable), Y',
       y = 'Count',
       title = 'Variance in Y ignoring x')

# let's account for the effect of x
# e.g., "How variable is height between people if we account for their age?"
m0 <- gam(Y ~ x, data = d0)

# look at the coefficient estimates
# the true relationship is mu = 4 - 3 * x
summary(m0)

beta0_hat <- coefficients(m0)[1]; beta0_hat # true beta0 is 4
beta1_hat <- coefficients(m0)[2]; beta1_hat # true beta1 is -3

# Predicting from the model ----
# predict from the model using coefficients
newd0 <- tibble(x = seq(0, 1, by = 0.0001))
pred0 <- mutate(newd0,
                mu_hat = beta0_hat + beta1_hat * x)

ggplot() +
  geom_point(aes(x, Y), d0) +
  geom_line(aes(x, mu_hat), pred0, color = 'darkorange', linewidth = 2) +
  labs(x = 'Predictor (indepedent variable)',
       y = 'Response (dependent variable)',
       title = 'Regression line calculated using estimated coefficients',
       subtitle = expression('Y ='~hat(mu)~'+ e ='~
                               hat(beta)[0]~+~hat(beta)[1]~x~+~e))

#' predict from the model using `predict()`
predict(object = m0, newdata = head(newd0), se.fit = TRUE) # returns a list
predict(m0, newdata = head(newd0), se.fit = TRUE) %>% as.data.frame() # as a df

# bind new data and predictions together, then add 95% credible intervals
pred0 <-
  bind_cols(newd0,
            as.data.frame(predict(m0, newdata = newd0, se.fit = TRUE))) %>%
  rename(mu_hat = fit) %>%
  mutate(lwr_95 = mu_hat + se.fit * qnorm(0.025),
         upr_95 = mu_hat + se.fit * qnorm(0.975))

# plot the estimated relationship
ggplot() +
  geom_ribbon(aes(x, ymin = lwr_95, ymax = upr_95), pred0,
              fill = 'darkorange', alpha = 0.3) +
  geom_line(aes(x, mu_hat), pred0, color = 'darkorange', linewidth = 2) +
  geom_point(aes(x, Y), d0) +
  labs(x = 'Predictor (indepedent variable)',
       y = 'Response (dependent variable)')

# how well is our model doing? ----
#' add predicted values to `d0`
d0$mu_hat <- predict(m0)

#' **measuring the proportion of explained variance**
#' *overall variance in Y, Var(Y)*
ggplot() +
  geom_errorbar(aes(x, ymin = Y, ymax = mean(Y)), d0) +
  geom_point(aes(x, Y), d0) +
  geom_hline(yintercept = mean(d0$Y), color = 'grey') +
  labs(x = 'Predictor (indepedent variable)',
       y = 'Response (dependent variable)',
       title = expression(
         'Estimated overall variance in Y ignoring x, SST ='~
           sum(''['i=1']^n~(y[i]~-~bar(y))^2)))

#' *estimated variance in Y explained by x*
#' *Var(Y | X)*
#' this is what we want to *maximize* when fitting a LM
ggplot() +
  geom_ribbon(aes(x, ymin = lwr_95, ymax = upr_95), pred0,
              fill = 'darkorange', alpha = 0.3) +
  geom_line(aes(x, mu_hat), pred0, color = 'darkorange', linewidth = 2) +
  geom_errorbar(aes(x, ymin = mean(Y), ymax = mu_hat), d0, color = 'grey')+
  geom_point(aes(x, Y), d0) +
  labs(x = 'Predictor (indepedent variable)',
       y = 'Response (dependent variable)',
       title = 'Estimated variance in Y accounting for x, SSR ='~
         ~sum(''['i=1']^n~(hat(mu[i])~-~bar(y))^2))

#' *estimated variance in Y NOT explained by x*
#' *Var(e) = Var(Y) - Var(Y | X)*
#' this is what we want to *minimize* when fitting a LM
ggplot() +
  geom_ribbon(aes(x, ymin = lwr_95, ymax = upr_95), pred0,
              fill = 'darkorange', alpha = 0.3) +
  geom_line(aes(x, mu_hat), pred0, color = 'darkorange', linewidth = 2) +
  geom_errorbar(aes(x, ymin = Y, ymax = mu_hat), d0, color = 'grey') +
  geom_point(aes(x, Y), d0) +
  labs(x = 'Predictor (indepedent variable)',
       y = 'Response (dependent variable)',
       title = 'Estimated variance in Y not explained by x, SSE ='~
         sum(''['i=1']^n~(y[i]~-~hat(mu[i])))^2)

# SST = SSR + SSE
SST <- sum((d0$Y - mean(d0$Y))^2) # Total sum of squares
SSR <- sum((d0$mu_hat - mean(d0$Y))^2) # Sum of squares (Regression)
SSE <- sum((d0$Y - d0$mu_hat)^2) # Sum of squares (Error)

SST
SSR + SSE

# R^2 = SSR / SST
SSR / SST
summary(m0)$r.sq # adjusted R^2 (lower than reular R^2)
summary(m0)$dev.expl # for LMs, deviance explained is R^2

# check assumptions of LMs
#' 1. *Certainty in x*: unlike Y, there is no error or uncertainty in x.
#' 2. *Linearity*: The relationship between X and the mean of Y is linear.
#' 3. *Homoscedasticity*: The variance of the residuals is constant.
#' 4. *Independence*: Observations are independent of each other.
#' 5. *Normality*: For a given value of X, the error in Y is Gaussian.
appraise(model = m0, method = 'simulate', n_simulate = 1e3)

##' **Limitations of simple linear regression**
##' When do LMs break?
m <- gam(weight ~ Time, data = ChickWeight)

summary(m)

#' there are many issues:
#' - nonlinear relationship between X and Y (see right column)
#' - heteroskedarsiticy (see right column)
#' - non-independence within individuals (not shown in this figure)
#' - residuals are clearly non-gaussian (this causes bad prediction intervals but may still result in reasonable CIs for the mean)
appraise(m, method = 'simulate', n_simulate = 1e3)

ggplot(ChickWeight, aes(Time, weight)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm', se = FALSE, col = 'red') +
  geom_smooth(method = 'gam', se = FALSE)
