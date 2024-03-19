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

m <- bam(formula = weight ~
           s(Time, k = 5) + # common smooth effect of time
           s(Time, Diet, bs = 'fs', k = 5) + #'deviations from common `s()`
           s(Time, Chick, bs = 'fs', k = 5) + #' chick-level deviations
           Diet ,
         family = Gamma(link = 'log'),
         data = ChickWeight,
         method = 'fREML', # fast REML
         discrete = TRUE) # required for fREML

draw(m, scales = 'fixed', parametric = TRUE) # view terms on the link scale

summary(m)

# plot predictions
range(ChickWeight$Time)

# create new dataset for predictions
newd <- expand_grid(
  Time = seq(0, 21, length.out = 400),
  Diet = unique(ChickWeight$Diet), # since we are using fixed effects
  Chick = 'new chick') # since we are using random effects

# predictions with 95% credible intervals assuming Gaussian residuals ----
preds_gaus <- 
  bind_cols( # bind columns together
    # data used for predictions
    newd,
    # Gamma GAM predictions
    #' need to specify `discrete = FALSE` and exclude `s(Time,Chick)` to
    #' include a new random effect since we are using `discrete = TRUE` in
    #' `bam()`
    predict(object = m, # our model
            newdata = newd, # the new data to predict for
            type = 'link', # keep predictions on the (log) link scale
            se.fit = TRUE, # include a column of standard error
            discrete = FALSE, #' since we have REs and `discrete = TRUE`
            exclude = c('s(Time,Chick)')) %>% #' not excluding gives `NA`s
      as.data.frame() %>%
      rename(gam_fit = fit, gam_se = se.fit) %>%
      mutate(mu_hat = exp(gam_fit),
             # assuming Gaussian CIs on link scale (see residual qqplot)
             lwr_95 = exp(gam_fit - 1.96 * gam_se),
             upr_95 = exp(gam_fit + 1.96 * gam_se)))
preds_gaus

# residuals are underdispersed, so assumption of Gaussian CIs is invalid
appraise(m, method = 'simulate')

# plotting the predictions
p_gaus_cis <-
  ggplot(preds_gaus) +
  facet_wrap(~ paste('Diet', Diet)) +
  geom_point(aes(Time, weight), ChickWeight, alpha = 0.2) +
  geom_ribbon(aes(Time, ymin = lwr_95, ymax = upr_95), alpha = 0.2) +
  geom_line(aes(Time, mu_hat), lwd = 1) +
  xlab('Time (days)') +
  scale_y_continuous('Weight (g)', expand = c(0, 0), limits = c(0, NA)) +
  theme(legend.position = 'top'); p_gaus_cis

# predictions with 95% credible intervals estimated from the posterior ----
preds_sim <-
  bind_cols(
    newd,
    # Gamma GAM predictions
    #' need to specify `discrete = FALSE` and exclude `s(Time,Chick)` to
    #' include a new random effect since we are using `discrete = TRUE` in
    #' `bam()`
    simulate(object = m, # our model
             data = newd, # the new data to predict for
             nsim = 1e4, # need >= 10,000 for reasonable estimates
             discrete = FALSE, #' since we have REs and `discrete = TRUE`
             exclude = c('s(Time,Chick)')) %>% #' not excluding gives `NA`s
      as.data.frame() %>%
      as_tibble()) %>%
      pivot_longer(cols = ! c('Time', 'Diet', 'Chick'),
                   values_to = 'value', names_to = 'simulation') %>%
      mutate(simulation = substr(simulation, 2, nchar(simulation))) %>%
      group_by(Diet, Time) %>%
      summarize(lwr_95 = quantile(value, 0.025), # lower 95% CI
                mu_hat = quantile(value, 0.5), # median
                upr_95 = quantile(value, 0.975), # upper 95% CI
                .groups = 'drop')
preds_sim

# credible intervals are much narrower
p_sim_cis <-
  ggplot(preds_sim) +
  facet_wrap(~ paste('Diet', Diet)) +
  geom_point(aes(Time, weight), ChickWeight, alpha = 0.2) +
  geom_ribbon(aes(Time, ymin = lwr_95, ymax = upr_95), alpha = 0.2) +
  geom_line(aes(Time, mu_hat), lwd = 1) +
  xlab('Time (days)') +
  scale_y_continuous('Weight (g)', expand = c(0, 0), limits = c(0, NA)) +
  theme(legend.position = 'top'); p_sim_cis

cowplot::plot_grid(p_gaus_cis, p_sim_cis, nrow = 1)
