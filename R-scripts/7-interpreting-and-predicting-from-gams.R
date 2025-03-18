#' *Workshop 7: Interpreting and predicting from Generalized Additive Models*
#' Copyright 2024 Stefano Mezzini
#' Published under the MIT licence
#' https://github.com/csc-ubc-okanagan/ubco-csc-modeling-workshop/tree/main
#' https://events.ok.ubc.ca/event/fitting-models-to-data-not-data-to-models-eight-workshop-series/
library('dplyr')   # for data wrangling
library('tidyr')   #'for `expand_grid`
library('mgcv')    # for modeling
library('ggplot2') # for fancy plots
library('gratia')  # for ggplot-based model graphics
theme_set(theme_classic(base_size = 15))

# we were using Gamma() last time, partially because it's fast
m_gamma <-
  bam(formula = weight ~
        Diet +
        s(Time, by = Diet, k = 5) + #'deviations from common `s()`
        s(Time, Chick, bs = 'fs', k = 5), #' chick-level deviations
      family = Gamma(link = 'log'),
      data = ChickWeight,
      method = 'fREML', # fast REML
      discrete = TRUE, # drastically speeds up computation
      control = gam.control(trace = TRUE))
appraise(m_gamma, method = 'simulate')

# tw() fits better but slower
m_tw <-
  bam(formula = weight ~
        Diet +
        s(Time, by = Diet, k = 5) + #'deviations from common `s()`
        s(Time, Chick, bs = 'fs', k = 5), #' chick-level deviations
      family = tw(link = 'log'),
      data = ChickWeight,
      method = 'fREML', # fast REML
      discrete = TRUE, # drastically speeds up computation
      control = gam.control(trace = TRUE))
appraise(m_tw, method = 'simulate') # power parameter: p = 1.784 < 2
summary(m_tw)

# terms on the log link scale: centered at the average (0), additive change
draw(m_tw, scales = 'free', parametric = TRUE)

# terms on the response scale: centered at the average (1), multiplicative change
draw(m_tw, scales = 'free', parametric = TRUE, fun = exp)

# plot predictions
range(ChickWeight$Time)

# create new dataset for predictions
newd <- expand_grid(
  Time = seq(0, 21, length.out = 400),
  Diet = unique(ChickWeight$Diet), # since we are using fixed effects
  Chick = ChickWeight$Chick[1]) # since we are using random effects

# predictions with 95% credible intervals assuming Gaussian posterior on the log scale ----
appraise(m_tw, method = 'simulate')

preds <- 
  bind_cols( # bind columns together
    # data used for predictions
    newd,
    # GAM predictions
    #' exclude `s(Time,Chick)` if you don't want the predictions to be for
    #' that specific chick (exclude = `c('s(Time, Chick)')`)
    #' 
    #' if predicting for a RE level in the dataset, as we are doing:
    #' can use `discrete = TRUE` for fast computation (only works with RE
    #' levels that are in the dataset)
    #' 
    #' if predicting for a RE level *not* in the dataset, like 'new chick':
    #' need to specify `discrete = FALSE`
    predict(object = m_tw, # our model
            newdata = newd, # the new data to predict for
            type = 'link', # keep predictions on the (log) link scale
            se.fit = TRUE, # include a column of standard error
            discrete = TRUE, # for faster predictions
            exclude = c('s(Time,Chick)')) %>% #' ignore individuals
      as.data.frame() %>%
      mutate(mu_hat = exp(fit),
             # assuming Gaussian CIs on link scale (see residual qqplot)
             lwr_95 = exp(fit - 1.96 * se.fit),
             upr_95 = exp(fit + 1.96 * se.fit)))
preds

# plotting the predictions ----
# together
p_mean <- ggplot(preds) +
  geom_ribbon(aes(Time, ymin = lwr_95, ymax = upr_95, fill = Diet),
              alpha = 0.2) +
  geom_line(aes(Time, mu_hat, color = Diet), lwd = 1) +
  xlab('Time (days)') +
  scale_y_continuous('Weight (g)', limits = c(0, NA)) +
  khroma::scale_color_bright() +
  khroma::scale_fill_bright()
p_mean

# faceted
p_mean + facet_wrap(~ paste('Diet', Diet))

# predictions for a specific chick
ChickWeight[1, ] # first chick is '1'

preds_1 <-
  bind_cols( # bind columns together
    # data used for predictions
    mutate(newd, Chick = '1'),
    # GAM predictions
    #' exclude `s(Time,Chick)` if you don't want the predictions to be for
    #' that specific chick (exclude = `c('s(Time, Chick)')`)
    #' 
    #' if predicting for a RE level in the dataset, as we are doing:
    #' can use `discrete = TRUE` for fast computation
    #' 
    #' if predicting for a RE level *not* in the dataset, like 'new chick':
    #' need to specify `discrete = FALSE`
    predict(object = m_tw, # our model
            newdata = mutate(newd, Chick = '1'), # with Chick 1
            type = 'link', # keep predictions on the (log) link scale
            se.fit = TRUE, # include a column of standard error
            discrete = TRUE) %>% # for faster predictions
      as.data.frame() %>%
      mutate(mu_hat = exp(fit),
             # assuming Gaussian CIs on link scale (see residual qqplot)
             lwr_95 = exp(fit - 1.96 * se.fit),
             upr_95 = exp(fit + 1.96 * se.fit)))
preds_1

# trends are a bit different because they follow the chick-level trends
# CIs are narrower for diet 1 because chick 1 ate diet 1
p_1 <-
  ggplot(preds_1) +
  geom_ribbon(aes(Time, ymin = lwr_95, ymax = upr_95, fill = Diet),
              alpha = 0.2) +
  geom_line(aes(Time, mu_hat, color = Diet), lwd = 1) +
  xlab('Time (days)') +
  scale_y_continuous('Weight (g)', limits = c(0, NA)) +
  khroma::scale_color_bright() +
  khroma::scale_fill_bright()
p_1

p_mean +
  facet_wrap(~ paste('Diet', Diet)) +
  geom_ribbon(aes(Time, ymin = lwr_95, ymax = upr_95, fill = Diet),
              preds_1, alpha = 0.2) +
  geom_line(aes(Time, mu_hat, color = Diet), preds_1, lwd = 1)
