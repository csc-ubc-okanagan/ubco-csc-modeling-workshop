#' *Workshop 4: Interaction terms and hierarchical linear models and  in R*
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

ggplot(ChickWeight, aes(Time, weight)) +
  facet_wrap(~ Diet) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x)

# model that only accounts for time
m_cw_0 <- gam(formula = weight ~ Time,
              family = gaussian(), # because it's a linear model
              data = ChickWeight,
              method = 'ML') # find most likely coefficients given the data
appraise(m_cw_0)

# model that also accounts for diet type ("treatment") and differences
# among chick means ("blocks")
m_cw_1 <- gam(formula = weight ~
                Time + # account for temporal autocorrelation
                Diet + # account for treatment
                s(Chick, bs = 're'), # random effect of chicks
              family = gaussian(),
              data = ChickWeight,
              method = 'ML')
appraise(m_cw_1)

# adding interaction terms of time and diet (diet affects growth)
m_cw_2 <- gam(formula = weight ~
                Time + # linear effect of time
                Diet + # each diet has a different value at Time = 0
                Time:Diet + # time works differently for each diet
                s(Chick, bs = 're'),
              family = gaussian(), # because it's a linear model
              data = ChickWeight,
              method = 'ML') # find most likely coefficients given the data
appraise(m_cw_2)

# interpret the model coefficients
summary(m_cw_3)

# plot data again for some final considerations
ggplot(ChickWeight, aes(Time, weight)) +
  facet_wrap(~ Diet) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  geom_smooth(method = 'glm', formula = y ~ x,
              method.args = list(family = Gamma('log')),
              color = 'darkorange')

# continuous-continuous interaction terms ----
# building a hypothetical dataset where temperature affects growth
ChickWeight$temperature <- log(ChickWeight$Time + 3) * 10 +
  rnorm(nrow(ChickWeight))

m_cw_3 <- gam(formula = weight ~
                Time +
                Diet +
                Time:Diet +
                Time:temperature +
                s(Chick, bs = 're'),
              family = gaussian(), # because it's a linear model
              data = ChickWeight,
              method = 'ML') # find most likely coefficients given the data

plot(m_cw_3, pages = 1, all.terms = TRUE) # no Time:temperature plot

pred_int <- expand.grid(
  Time = seq(0, max(ChickWeight$Time), length.out = 100),
  temperature = seq(min(ChickWeight$temperature),
                    max(ChickWeight$temperature), length.out = 100),
  Diet = 1,
  Chick = 1) %>%
  mutate(.,
         mu = predict(m_cw_3, newdata = ., type = 'response',
                      se.fit = FALSE))

ggplot(pred_int, aes(Time, temperature, fill = mu)) +
  geom_raster() +
  scale_fill_viridis_c('Estimated weight') +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

set.seed(3)
filter(pred_int, temperature %in% sample(unique(temperature), 4)) %>%
  mutate(temperature = factor(sort(round(temperature)))) %>%
  ggplot(aes(Time, mu, color = temperature, group = temperature)) +
  geom_line(lwd = 1) +
  scale_color_brewer('Estimated weight', palette = 5, type = 'div',
                     direction = -1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))
