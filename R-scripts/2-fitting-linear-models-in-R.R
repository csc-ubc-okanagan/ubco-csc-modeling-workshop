#' *Workshop 2: Fitting linear models in R*
#' Copyright 2024 Stefano Mezzini
#' Published under the MIT licence
#' https://github.com/csc-ubc-okanagan/ubco-csc-modeling-workshop/tree/main
#' https://events.ok.ubc.ca/event/fitting-models-to-data-not-data-to-models-eight-workshop-series/
library('dplyr')   # for data wrangling
library('mgcv')    # for modeling
library('ggplot2') # for fancy plots
library('gratia')  # for ggplot-based model graphics
library('faraway') # for datasets
theme_set(theme_classic(base_size = 15))

# diagnosing any issues with model assumption violations ----
plot_example_diagnostics <- function(seed = as.numeric(Sys.time())) {
  set.seed(seed)
  d0 <- tibble(x = runif(n = 20), # predictor of Y
               mu = 4 - 3 * x, # true mean of Y
               epsilon = rnorm(n = length(x), mean = 0, sd = 1), # Gaussian error
               Y = mu + epsilon, # values of Y
               mu_hat = predict(lm(Y ~ x)),
               e = Y - mu_hat)
  cowplot::plot_grid(
    #' 1. *Certainty in x*: unlike Y, there is no error or uncertainty in x.
    ggplot(d0) +
      geom_errorbar(aes(x, ymin = Y - 1, ymax = Y + 1), color = 'grey') +
      geom_point(aes(x, Y)) +
      labs(x = 'x',  y = 'Y',
           title = expression(E(Y)~'='~mu~but~E(x)~'='~x)),
    #' 2. *Linearity*: The relationship between X and the mean of Y is linear.
    ggplot(d0) +
      geom_line(aes(x, mu), col = 'red', lwd = 1, alpha = 0.5) +
      geom_smooth(aes(x, Y), lwd = 1, method = 'gam', formula = y ~ s(x),
                  color = 'darkorange') +
      geom_point(aes(x, Y)) +
      labs(x = 'x',  y = 'Y',
           title = expression(E(Y)~'='~mu~'='~beta[0]~+~beta[1]~x)),
    #' 3. *Homoscedasticity*: The variance of the residuals is constant.
    ggplot(d0) +
      geom_hline(yintercept = 0, color = 'grey') +
      geom_smooth(aes(x, e), col = 'darkorange', lwd = 1, method = 'gam',
                  formula = y ~ s(x), se = FALSE) +
      geom_point(aes(x, e)) +
      labs(x = 'x', y = 'Residuals (e)',
           title = expression(Var(epsilon)~'='~sigma^2)),
    #' 4. *Independence*: Observations are independent of each other.
    ggplot(d0) +
      geom_point(aes(seq(nrow(d0)), Y)) +
      labs(x = 'Observation order', y = 'Y',
           title = expression(Y[italic(i)]~'\U2AEB'~Y[italic(j)]~
                                'for'~italic(i)~'\U2260'~italic(j))),
    #' 5. *Normality*: For a given value of x, the error in Y is Gaussian.
    ggplot(d0, aes(e)) +
      # geom_density(color = 'black', fill = 'grey', bw = 0.25) +
      geom_histogram(aes(y = after_stat(density)), color = 'black',
                     fill = 'grey', binwidth = 1) +
      geom_line(aes(x, dens), color = 'red', lwd = 1,
                tibble(x = seq(-3, 3, by = 0.001),
                       dens = dnorm(seq(-3, 3, by = 0.001)))) +
      labs(x = expression('Residuals,'~e~'='~Y~-~hat(mu)),
           y = 'Density',
           title = expression(epsilon~'~'~N('0,'~sigma^2))),
    ggplot(d0, aes(sample = e)) +
      geom_qq_line(color = 'red') +
      geom_qq(color = 'black') +
      labs(x = 'Expected quantiles',
           y = 'Obseved quantiles',
           title = expression(epsilon~'~'~N('0,'~sigma^2))))
}

plot_example_diagnostics() # run a few times for some examples

# how to fit linear models in R ----
## chick weight data
?ChickWeight
m_cw <- gam(formula = weight ~ Time, # Y ~ x
            family = gaussian(), # because it's a linear model
            data = ChickWeight,
            method = 'ML') # find most likely coefficients given the data

# diagnosing model violations ----
# plot the data
ggplot(ChickWeight, aes(Time, weight)) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x)) +
  labs(x = 'Time (days)', y = 'Weight (g)')

# create diagnostic plots
appraise(model = m_cw, method = 'simulate', n_simulate = 1e4)

### log-transforming data does not fix things
min(ChickWeight$weight)
m_cw_log <- gam(formula = log(weight) ~ Time, # Y ~ x
                family = gaussian(), # because it's a linear model
                data = ChickWeight,
                method = 'ML') # find most likely coefficients given the data
appraise(m_cw_log, method = 'simulate', n_simulate = 1e4)

# plot the data
ggplot(ChickWeight, aes(Time, log(weight))) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x)) +
  labs(x = 'Time (days)', y = 'Weight (g)')

### a quick note Jensen's inequality
Y <- rpois(1e5, lambda = 3)
hist(Y, breaks = seq(0, max(Y)))
mean(Y) # calculating mean directly
abline(v = mean(Y), col = 'red', lwd = 2)
log(Y) %>% mean() %>% exp() # calculating mean after log transformation
log1p(Y) %>% mean() %>% exp() # calculating mean after log1p transformation
abline(v = exp(mean(log1p(Y))), col = 'darkorange', lwd = 2)

#' **fit the correct model to the data, don't fit the data to the model**

# prostate cancer data ----
?prostate
prostate$cavol <- exp(prostate$lcavol)
m_pc <- gam(formula = cavol ~ age, # Y ~ x
            family = gaussian(),
            data = prostate,
            method = 'ML')
appraise(m_pc, method = 'simulate', n_simulate = 1e4)

# height and weight of American women ----
?women # see notes in details

ggplot(women, aes(height, weight)) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x)) +
  labs(x = 'Height (in)', y = 'Weight (lb)')

m_wh <- gam(formula = weight ~ height,
            family = gaussian(),
            data = women,
            method = 'ML')
appraise(m_wh, method = 'simulate', n_simulate = 1e4)

# income and illiteracy of US states in the 1970s ----
?state.x77 # see notes in details
states <- as.data.frame(state.x77)
# Income: per capita income (1974)
# Illiteracy: illiteracy (1970, percent of population)

ggplot(states, aes(Income, Illiteracy)) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x)) +
  labs(x = 'Per capita income (USD)', y = 'Illiteracy (%)')

m_ii <- gam(formula = Illiteracy ~ Income,
            family = gaussian(),
            data = states,
            method = 'ML')
appraise(m_ii, method = 'simulate', n_simulate = 1e4)
appraise(m_ii, method = 'simulate', n_simulate = 1e4, n_bins = 5)

# drop the state with the highest income
ggplot(filter(states, Income < 6000), aes(Income, Illiteracy)) +
  geom_point() +
  geom_point(data = filter(states, Income > 6000), color = 'red') +
  geom_smooth(method = 'gam', formula = y ~ s(x)) +
  labs(x = 'Per capita income (USD)', y = 'Illiteracy (%)')

m_ii2 <- gam(formula = Illiteracy ~ Income,
             family = gaussian(),
             data = states,
             subset = Income < 6000,
             method = 'ML')
appraise(m_ii2, method = 'simulate', n_simulate = 1e4, n_bins = 5)

ggplot(filter(states, Income < 6000), aes(Income, Illiteracy)) +
  geom_point() +
  geom_point(data = filter(states, Income > 6000), color = 'red') +
  geom_smooth(method = 'gam', formula = y ~ s(x)) +
  geom_smooth(method = 'gam', formula = y ~ x, color = 'darkorange') +
  labs(x = 'Per capita income (USD)', y = 'Illiteracy (%)')

# interpret linear model summaries ----
# coefficients, df, SE, t statistics, p-values, R^2, R^2_adj,
# statistical significance
summary(m_ii2)

#' note: there are no F statistics are in the summary, but the F statistic
#' for the slope is equal to the squared t statistic.
#' *This does not hold for multiple t statistics at once.*
#' https://stats.stackexchange.com/questions/55236/prove-f-test-is-equal-to-t-test-squared
#' Generally, the F statistic compares two models and assesses whether the
#' addition of at least one of the terms in the larger model that is not
#' in the simpler model is significant.
