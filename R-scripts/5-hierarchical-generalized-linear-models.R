#' *Workshop 5: Hierarchical Generalized Linear Models in R*
#' Copyright 2024 Stefano Mezzini
#' Published under the MIT licence
#' https://github.com/csc-ubc-okanagan/ubco-csc-modeling-workshop/tree/main
#' https://events.ok.ubc.ca/event/fitting-models-to-data-not-data-to-models-eight-workshop-series/
library('dplyr')   # for data wrangling
library('tidyr')   #'for `expand_grid`
library('mgcv')    # for modeling
library('ggplot2') # for fancy plots
library('gratia')  # for ggplot-based model graphics
theme_set(theme_bw(base_size = 15))

# remove ordering of the Chick factor
ChickWeight$Chick <- factor(ChickWeight$Chick, ordered = FALSE)

# last week we used linear models
ggplot(ChickWeight, aes(Time, weight)) +
  facet_wrap(~ Diet) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x)

ggplot(ChickWeight, aes(Time, weight)) +
  facet_wrap(~ Diet) +
  geom_point() +
  geom_smooth(method = 'glm', formula = y ~ x,
              method.args = list(family = Gamma(link = 'log')))

# best linear model from last week
# (all LMs are GLMs, but not all GLMs are LMs)
m_cw_lm <- gam(formula = weight ~
                 Time + # linear effect of time
                 Diet + # each diet has a different value at Time = 0
                 Time:Diet + # time works differently for each diet
                 s(Chick, bs = 're'),
               family = gaussian(), # because it's a linear model
               data = ChickWeight,
               method = 'ML') # find most likely coefficients given the data
appraise(m_cw_lm)
summary(m_cw_lm)

# now fit a non-gaussian GLM instead of a LM
# choosing Gamma family because weight is > 0 but has no clear upper limit
# link function because we want to map (-Inf, Inf) to (0, Inf) using exp()
m_cw_glm <- gam(formula = weight ~
                  Time + #' effect of time is linear *on the link scale*
                  Diet + # each diet has a different value at Time = 0
                  Time:Diet + # time works differently for each diet
                  s(Chick, bs = 're'),
                family = Gamma(link = 'log'), # no longer a linear model
                data = ChickWeight,
                method = 'ML') # find most likely coefficients given the data
appraise(m_cw_glm)
summary(m_cw_glm)

# check predictions
range(ChickWeight$Time)

newd <- expand_grid(
  Time = seq(0, 21, length.out = 400),
  Diet = unique(ChickWeight$Diet), # since we are using fixed effects
  Chick = 'new chick') # since we are using random effects

preds <- 
  bind_cols( # bind columns together
    # data used for predictions
    newd,
    # LM predictions
    predict(object = m_cw_lm, newdata = newd, type = 'response',
            se.fit = TRUE) %>%
      as.data.frame() %>%
      rename(lm_fit = fit, lm_se = se.fit) %>%
      mutate(lm_est = lm_fit,
             lm_95_lwr = lm_fit - 1.96 * lm_se,
             lm_95_upr = lm_fit + 1.96 * lm_se),
    # Gamma GLM predictions
    predict(object = m_cw_glm, newdata = newd, type = 'link',
            se.fit = TRUE) %>%
      as.data.frame() %>%
      rename(glm_fit = fit, glm_se = se.fit) %>%
      mutate(glm_est = exp(glm_fit),
             # assuming Gaussian CIs on link scale (see residual qqplot)
             glm_95_lwr = exp(glm_fit - 1.96 * glm_se),
             glm_95_upr = exp(glm_fit + 1.96 * glm_se)))

# plotting the predictions
ggplot(preds) +
  facet_wrap(~ paste('Diet', Diet)) +
  geom_point(aes(Time, weight), ChickWeight, alpha = 0.3) +
  geom_ribbon(aes(Time, ymin = lm_95_lwr, ymax = lm_95_upr), alpha = 0.3,
              fill = '#4477AA') +
  geom_ribbon(aes(Time, ymin = glm_95_lwr, ymax = glm_95_upr), alpha = 0.3,
              fill = '#EE6677') +
  geom_line(aes(Time, lm_est), color = '#4477AA', lwd = 2) +
  geom_line(aes(Time, glm_est), color = '#EE6677', lwd = 2) +
  geom_hline(yintercept = 0, color = 'grey') +
  xlab('Time (days)') +
  scale_y_continuous('Weight (g)', expand = c(0, 0))
