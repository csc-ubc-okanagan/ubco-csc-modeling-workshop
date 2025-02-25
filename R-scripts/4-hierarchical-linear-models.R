#' *Workshop 4: Interaction terms and Hierarchical Linear Models in R*
#' Copyright 2024 Stefano Mezzini
#' Published under the MIT licence
#' https://github.com/csc-ubc-okanagan/ubco-csc-modeling-workshop/tree/main
#' https://events.ok.ubc.ca/event/fitting-models-to-data-not-data-to-models-eight-workshop-series/
library('dplyr')   #  for data wrangling
library('mgcv')    #  for modeling
library('ggplot2') #  for fancy plots
library('gratia')  #  for ggplot-based model graphics
theme_set(theme_classic(base_size = 15))

ChickWeight # column names are inconsistent
cw <- janitor::clean_names(ChickWeight) %>% # make all names lowercase
  as_tibble() # for a more user-friendly format

# each treatment has a different trend
ggplot(cw, aes(time, weight)) +
  facet_wrap(~ diet) +
  geom_line(aes(group = chick)) +
  geom_smooth(method = 'lm', formula = y ~ x, color = 'darkorange')

# linear model that only accounts for time
m_cw_0 <- gam(formula = weight ~ time,
              family = gaussian(), # because it's a linear model
              data = cw,
              method = 'ML') # find most likely coefficients given the data
appraise(m_cw_0)
summary(m_cw_0)
draw(m_cw_0, parametric = TRUE)

# model that also accounts for diet type ("treatment") and differences
# among chick means ("blocks")
#' *fixed effects*:  independent categories
#' *random effects*: levels that can "learn" from each other (data pooling)
m_cw_1 <- gam(formula = weight ~
                time + # account for temporal autocorrelation
                diet + # intercept for each treatment
                s(chick, bs = 're'), # random intercept of chicks
              family = gaussian(),
              data = cw,
              method = 'ML')
appraise(m_cw_1)
summary(m_cw_1) #' see changes in R-sq, Dev. expl., and scale est. (SD)
draw(m_cw_1, parametric = TRUE)

# adding interaction terms of time and diet (diet affects growth)
m_cw_2 <- gam(formula = weight ~
                time + # linear effect of time
                diet + # each diet has a different value at time = 0
                time:diet + # time works differently for each diet
                s(chick, bs = 're'),
              family = gaussian(), # because it's a linear model
              data = cw,
              method = 'ML') # find most likely coefficients given the data
appraise(m_cw_2)
summary(m_cw_2)
draw(m_cw_2, parametric = TRUE) # won't plot diets 2-4

# plot estimated trends for each diet for average chick (not in the data)
m_cw_2 %>%
  data_slice(time = evenly(time), diet = evenly(diet), chick = 'new') %>%
  fitted_values(object = m_cw_2, data = .) %>% # get predictions
  ggplot() +
  facet_wrap(~ diet) +
  geom_line(aes(time, weight, group = chick), cw, alpha = 0.3) +
  geom_ribbon(aes(time, ymin = .lower_ci, ymax = .upper_ci), alpha = 0.3) +
  geom_line(aes(time, .fitted), color = 'darkorange', lwd = 1)

# continuous-continuous interaction terms ----
# using a hypothetical dataset where amount of food also affects growth
# assign a random amount of food for each chick
cw <- cw %>%
  mutate(food_g = sample(c(10, 20, 30, 40, 50), size = n(), replace = TRUE),
         #' new values of `weight` that include the effect of `food_g`
         weight2 = weight - 25 + food_g * 0.5 + food_g * time * 0.1)

m_cw_3 <- gam(formula = weight2 ~
                diet +
                time +
                food_g +
                time:food_g + # time works differently depending on food_g
                time:diet +
                s(chick, bs = 're'),
              family = gaussian(), # because it's a linear model
              data = cw,
              method = 'ML') # find most likely coefficients given the data
summary(m_cw_3)

# writing the model using the asterisk format as opposed to the colon format
m_cw_3 <- gam(formula = weight2 ~
                diet +
                time * food_g + #' same as `time + food_g + time:food_g`
                time:diet +
                s(chick, bs = 're'),
              family = gaussian(), # because it's a linear model
              data = cw,
              method = 'ML') # find most likely coefficients given the data

summary(m_cw_3)
appraise(m_cw_3)
draw(m_cw_3, parametric = TRUE) # no time:food_g plot

# make a plot of the predicted effect of food and time for each diet
preds_3 <- m_cw_3 %>%
  data_slice(time = evenly(time), diet = evenly(diet),
             food_g = evenly(food_g), chick = 'new') %>%
  fitted_values(object = m_cw_3, data = .) # get predictions
  
# plot weight over time by diet
preds_3 %>%
  filter(food_g == min(food_g)) %>%
  ggplot(aes(time, .fitted, color = diet)) +
  geom_line(linewidth = 1) +
  labs(x = 'Time (days)', y = 'Weight (g)') +
  khroma::scale_color_bright(name = 'Diet type') +
  theme(legend.position = 'top')

# plot weight over time by food_g
m_cw_3 %>%
  data_slice(time = evenly(time), diet = evenly(diet),
             food_g = unique(food_g), chick = 'new') %>%
  fitted_values(object = m_cw_3, data = .) %>%
  filter(diet == 1) %>%
  filter(food_g %in% sample(unique(food_g), 4)) %>%
  ggplot(aes(time, .fitted, group = food_g)) +
  geom_line(linewidth = 2) +
  geom_line(aes(color = food_g), linewidth = 1) +
  labs(x = 'Time (days)', y = 'Weight (g)') +
  scale_color_distiller('Food (g)', type = 'seq', palette = 1,
                        direction = 1) +
  theme(legend.position = 'top')

#' plot `time * food_g` interaction for each diet 
ggplot(preds_3, aes(time, food_g, fill = .fitted)) +
  facet_wrap(~ diet) +
  geom_raster() +
  scale_fill_viridis_c('Estimated weight (g)', limits = c(0, NA)) +
  scale_x_continuous('Time (days)', expand = c(0, 0)) +
  scale_y_continuous('Food portion (g)', expand = c(0, 0)) +
  theme(legend.position = 'top')
