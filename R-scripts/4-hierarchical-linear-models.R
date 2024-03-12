#' *Workshop 4: Interaction terms and Hierarchical Linear Models in R*
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

# remove ordering of the Chick factor
ChickWeight$Chick <- factor(ChickWeight$Chick, ordered = FALSE)

# each treatment has a different trend
ggplot(ChickWeight, aes(Time, weight)) +
  facet_wrap(~ Diet) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x)

# linear model that only accounts for time
m_cw_0 <- gam(formula = weight ~ Time,
              family = gaussian(), # because it's a linear model
              data = ChickWeight,
              method = 'ML') # find most likely coefficients given the data
appraise(m_cw_0)
summary(m_cw_0)

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
summary(m_cw_1)

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
summary(m_cw_2)

# continuous-continuous interaction terms
# count number of chicks per diet
ChickWeight %>%
  group_by(Diet) %>%
  summarize(n = n_distinct(Chick))

# using a hypothetical dataset where amount of food also affects growth
ChickWeight <- ChickWeight %>%
  group_by(Diet) %>%
  mutate(food_g = sample(c(10, 20, 30, 40, 50), size = n(), replace = TRUE),
         weight2 = weight - 25 + food_g * 0.5 + food_g * Time * 0.1)

m_cw_3 <- gam(formula = weight2 ~
                Time +
                Diet +
                food_g +
                Time:Diet +
                Time:food_g +
                s(Chick, bs = 're'),
              family = gaussian(), # because it's a linear model
              data = ChickWeight,
              method = 'ML') # find most likely coefficients given the data

# writing the model using the asterisk format as opposed to the colon format
m_cw_3 <- gam(formula = weight2 ~
                Time * food_g +
                Diet +
                Time:Diet +
                s(Chick, bs = 're'),
              family = gaussian(), # because it's a linear model
              data = ChickWeight,
              method = 'ML') # find most likely coefficients given the data

appraise(m_cw_3)
summary(m_cw_3)
plot(m_cw_3, pages = 1, all.terms = TRUE) # no Time:food_g plot

# make a plot of the predicted effect of food and time for each diet
pred_int <- expand_grid(
  Time = seq(0, max(ChickWeight$Time), length.out = 100),
  food_g = seq(min(ChickWeight$food_g),
                    max(ChickWeight$food_g), length.out = 100),
  Diet = unique(ChickWeight$Diet),
  Chick = 'new chick') %>%
  mutate(.,
         mu = predict(m_cw_3, newdata = ., type = 'response',
                      se.fit = FALSE))

# plot weight over time by diet
pred_int %>%
  filter(food_g == min(food_g)) %>%
  ggplot(aes(Time, mu, color = Diet)) +
  geom_line(linewidth = 1) +
  khroma::scale_color_bright(name = 'Diet type') +
  theme(legend.position = 'top')

# plot weight over time by food_g
set.seed(3)
pred_int %>%
  filter(Diet == 1) %>%
  filter(food_g %in% sample(unique(food_g), 4)) %>%
  ggplot(aes(Time, mu, color = food_g, group = food_g)) +
  geom_line(linewidth = 2) +
  scale_color_distiller('Diet type', type = 'div', palette = 7) +
  theme(legend.position = 'top')

#' plot `Time * food_g` interaction for each diet 
ggplot(pred_int, aes(Time, food_g, fill = mu)) +
  facet_wrap(~ Diet) +
  geom_raster() +
  scale_fill_viridis_c('Estimated weight (g)', breaks = c(50, 150, 250)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = 'top')
