#' *Workshop 6: Generalized Additive Models*
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

# plot the data and add some exploratory models
ggplot(ChickWeight, aes(Time, weight)) +
  facet_wrap(~ Diet) +
  geom_point(alpha = 0.3) +
  # two weeks ago we used a LM
  geom_smooth(method = 'lm', formula = y ~ x,  aes(color = 'LM'),
              se = FALSE, lwd = 1) +
  # last week we used a GLM
  geom_smooth(method = 'glm', formula = y ~ x, aes(color = 'GLM'),
              method.args = list(family = Gamma(link = 'log')),
              se = FALSE, lwd = 1) +
  # this week we are using a GAM
  geom_smooth(method = 'gam', formula = y ~ s(x), aes(color = 'GAM'),
              method.args = list(family = Gamma(link = 'log')),
              se = FALSE, lwd = 1) +
  khroma::scale_color_bright(name = 'Model') +
  labs(x = 'Time (days)', y = 'Weight (g)')

# best linear model from two weeks ago
# (all LMs are GLMs, but not all GLMs are LMs)
#' *NOTE:* random effects are random deviations from mu
m_cw_lm <- gam(formula = weight ~
                 Time + # linear effect of time
                 Diet + # different intercept (starting weight) Time = 0
                 Time:Diet + # different slope (growth rate) for each diet
                 s(Chick, bs = 're'),
               family = gaussian(), # because it's a linear model
               data = ChickWeight,
               method = 'ML')
appraise(m_cw_lm, point_alpha = 0.25)

ggplot(mapping = aes(x = predict(m_cw_lm, type = 'link'),
                     y = residuals(m_cw_lm))) +
  geom_point() +
  geom_smooth(method = 'gam') +
  labs(x = expression(Linear~predictor~(eta~'='~mu)),
       y = 'Residuals')

# Gamma HGLM from last week
# (all GLMs are GAMs, but not all GAMs are GLMs)
# choosing Gamma family because weight is > 0 but has no clear upper limit
# link function because we want to map (-Inf, Inf) to (0, Inf) using exp()
m_cw_glm <- gam(formula = weight ~
                  Time + #' effect of time is linear *on the link scale*
                  Diet +
                  Time:Diet +
                  s(Chick, bs = 're'),
                family = Gamma(link = 'log'), # no longer a linear model
                data = ChickWeight,
                method = 'ML')
appraise(m_cw_glm, point_alpha = 0.25)

ggplot(mapping = aes(x = predict(m_cw_glm, type = 'link'),
                     y = residuals(m_cw_glm))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'gam') +
  geom_hline(yintercept = 0) +
  labs(x = expression(Linear~predictor~(eta~'='~log(mu))),
       y = 'Link-scale residuals')

# Gamma HGAM with no common smooth of time
m_cw_gam <- gam(formula = weight ~
                  s(Time, Diet, bs = 'fs') + # smooth effect for each diet
                  Diet +
                  s(Chick, bs = 're'),
                family = Gamma(link = 'log'),
                data = ChickWeight,
                method = 'REML') #' *Restricted ML*, see `?mgcv::gam`
appraise(m_cw_gam, point_alpha = 0.25)

draw(m_cw_gam) #' see `s(Time,Diet)`

# Gamma HGAM with common smooth of time
#' using `bam()`, `method = 'fREMML'`, and `discrete = TRUE` to reduce
#' computation time
m_cw_gam <- bam(formula = weight ~
                  s(Time) + # common smooth effect of time
                  s(Time, Diet, bs = 'fs') + #'deviations from common `s()`
                  s(Time, Chick, bs = 'fs') + #' chick-level deviations
                  Diet ,
                family = Gamma(link = 'log'),
                data = ChickWeight,
                method = 'fREML', # fast REML
                discrete = TRUE) # required for fREML

draw(m_cw_gam) #' see `s(Time,Diet)`
draw(m_cw_gam, scales = 'fixed') #' see `s(Time,Diet)`

appraise(m_cw_gam, n_simulate = 1000, method = 'simulate', point_alpha = 0.25)

ggplot(mapping = aes(x = predict(m_cw_gam, type = 'link'),
                     y = residuals(m_cw_gam))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'gam') +
  geom_hline(yintercept = 0) +
  labs(x = expression(Linear~predictor~(eta~'='~log(mu))),
       y = 'Residuals')

# dev.expl = 99.9% => almost no error => almost no longer stats, just math
summary(m_cw_gam)
draw(m_cw_gam, parametric = TRUE)

# why are estimated intercepts so different?
coef(m_cw_lm)['(Intercept)'] #' mean weight for diet 1 at `t = 0`
exp(coef(m_cw_glm)['(Intercept)']) #' mean weight for diet 1 at `t = 0`
exp(coef(m_cw_gam)['(Intercept)']) #' mean weight for the `t = 21 / 2`

# plot predictions
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
             lm_lwr95 = lm_fit - 1.96 * lm_se,
             lm_upr95 = lm_fit + 1.96 * lm_se),
    # Gamma GLM predictions
    predict(object = m_cw_glm, newdata = newd, type = 'link',
            se.fit = TRUE) %>%
      as.data.frame() %>%
      rename(glm_fit = fit, glm_se = se.fit) %>%
      mutate(glm_est = exp(glm_fit),
             # assuming Gaussian CIs on link scale (see residual qqplot)
             glm_lwr95 = exp(glm_fit - 1.96 * glm_se),
             glm_upr95 = exp(glm_fit + 1.96 * glm_se)),
    # Gamma GAM predictions
    #' need to specify `discrete = FALSE` and exclude `s(Time,Chick)` to
    #' include a new random effect since we are using `discrete = TRUE` in
    #' `bam()`
    predict(object = m_cw_gam, newdata = newd, type = 'link',
            se.fit = TRUE, discrete = FALSE, exclude = c('s(Time,Chick)')) %>%
      as.data.frame() %>%
      rename(gam_fit = fit, gam_se = se.fit) %>%
      mutate(gam_est = exp(gam_fit),
             # assuming Gaussian CIs on link scale (see residual qqplot)
             gam_lwr95 = exp(gam_fit - 1.96 * gam_se),
             gam_upr95 = exp(gam_fit + 1.96 * gam_se))) %>%
  # switch to long format
  pivot_longer(cols = c(lm_fit, lm_se, lm_est, lm_lwr95, lm_upr95,
                        glm_fit, glm_se, glm_est, glm_lwr95, glm_upr95,
                        gam_fit, gam_se, gam_est, gam_lwr95, gam_upr95),
               names_to = c('model', 'parameter'), names_sep = '_') %>%
  pivot_wider(names_from = parameter, values_from = value) %>%
  mutate(model = toupper(model))

preds

# plotting the predictions
ggplot(preds) +
  facet_wrap(~ paste('Diet', Diet)) +
  geom_point(aes(Time, weight), ChickWeight, alpha = 0.2) +
  geom_ribbon(aes(Time, ymin = lwr95, ymax = upr95, fill = model),
              alpha = 0.2) +
  geom_line(aes(Time, est, color = model), lwd = 1) +
  geom_hline(yintercept = 0, color = 'grey') +
  xlab('Time (days)') +
  scale_y_continuous('Weight (g)', expand = c(0, 0)) +
  khroma::scale_color_bright(name = 'Model') +
  khroma::scale_fill_bright(name = 'Model') +
  theme(legend.position = 'top')
