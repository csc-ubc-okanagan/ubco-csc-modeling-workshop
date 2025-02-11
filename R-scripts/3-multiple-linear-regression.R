#' *Workshop 3: Multiple linear regression in R*
#' Copyright 2024 Stefano Mezzini
#' Published under the MIT licence
#' https://github.com/csc-ubc-okanagan/ubco-csc-modeling-workshop/tree/main
#' https://events.ok.ubc.ca/event/fitting-models-to-data-not-data-to-models-eight-workshop-series/
library('dplyr')   # for data wrangling
library('mgcv')    # for modeling
library('ggplot2') # for fancy plots
library('gratia')  # for ggplot-based model graphics
library('dagitty') # for drawing Directed Acyclical Graphs (DAGs)
theme_set(theme_classic(base_size = 15))

?state.x77

states <-
  state.x77 %>%
  as.data.frame() %>%
  mutate(State = rownames(.)) %>% # state.x77 has state names as row names
  `rownames<-`(NULL) %>% # drop rownames
  relocate(State, .before = 1) %>% # make the States column the first one
  rename(Life_exp = `Life Exp`,
         Murder_1e5 = Murder,
         HS_grad_perc = `HS Grad`) %>%
  as_tibble()
states

# predict average life expectancy by per capita income in 1974 ----
# create the DAG
dag_1 <- dagitty('Income -> Life_exp')
plot(dag_1)

# fit the model
m_1 <- gam(Life_exp ~ Income,
           family = gaussian(),
           data = states,
           method = 'ML')

# check assumptions and fit
appraise(m_1) # acceptably good fit
draw(m_1, parametric = TRUE)
summary(m_1) # relatively low R^2_adj

# add a predictor for murder per 100,000 in 1976 ----
# plot DAG
dag_2 <- dagitty('Income -> Life_exp
                 Murder_1e5 -> Life_exp
                 Income -> Murder_1e5')
plot(dag_2)

# fit model
m_2 <- gam(Life_exp ~ Income + Murder_1e5,
           family = gaussian(),
           data = states,
           method = 'ML')

appraise(m_2) # oddly high density of residuals ~= 0.5
draw(m_2, parametric = TRUE)
summary(m_2) # relatively good R^2_adj

# the p-value for Income went up. Why?
# murder rate and income are correlated, but that's ok (see the DAG)
ggplot(states, aes(Income, Murder_1e5)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x)

# add a predictor high school grads in 1976 ----
dag_3 <- dagitty('Income -> Life_exp
                 Income -> Murder_1e5
                 Murder_1e5 -> Life_exp
                 HS_grad_perc -> Life_exp
                 HS_grad_perc -> Murder_1e5
                 HS_grad_perc -> Income')
plot(dag_3)

# fit the model
m_3 <- gam(Life_exp ~ Income + Murder_1e5 + HS_grad_perc,
           family = gaussian(),
           data = states,
           method = 'ML')

appraise(m_3, method = 'simulate', n_simulate = 1e4) # somewhat better
draw(m_3, parametric = TRUE)
summary(m_3) # R^2_adj did not improve much

# % grad and income are correlated, so interpret coefs carefully
ggplot(states, aes(Income, HS_grad_perc)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x)

# same goes for % grad and murder rate for 1e5 people
ggplot(states, aes(Murder_1e5, HS_grad_perc)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x)
