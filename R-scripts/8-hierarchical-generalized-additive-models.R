#' *Workshop 8: Interaction terms and hierarchical GAMS in R*
#' Copyright 2024 Stefano Mezzini
#' Published under the MIT licence
#' https://github.com/csc-ubc-okanagan/ubco-csc-modeling-workshop/tree/main
#' https://events.ok.ubc.ca/event/fitting-models-to-data-not-data-to-models-eight-workshop-series/
library('dplyr')     # for data wrangling
library('mgcv')      # for modeling
library('ggplot2')   # for fancy plots
library('gratia')    # for ggplot-based model graphics
library('lubridate') # for working with dates
theme_set(theme_bw(base_size = 15) + theme(panel.grid = element_blank()))

# see the different smooth types available with mgcv (version 1.9-1)
?mgcv::smooth.terms

# update mgcv, if necessary (see the version at the bottom of the help page)
install.packages('mgcv')

# generate some fake data
set.seed(-10)
true_values <- tibble(x = seq(0, 1, by = 1e-3),
                      mu = 3 * sinpi(x)) # true response

d <- tibble(x = runif(15), # 15 random uniform(0, 1) samples
            mu = 3 * sinpi(x), # true response
            y = rnorm(n = length(mu), mean = mu, sd = 1))

ggplot() +
  geom_line(aes(x, mu), true_values, color = 'darkorange1', lwd = 1) +
  geom_point(aes(x, y), d) +
  labs(y = 'Y', 
       caption = 'True relationship in orange, points are simulated data')

# one-dimensional smooths ----
#' `bs='tp'`; thin plate regression splines
#' default smooth for `s()`
#' optimal smoother of any given basis dimension/rank
#' can be slow to fit if you have a lot of data
m <- gam(formula = y ~ s(x, k = 10),
         family = gaussian(link = 'identity'),
         data = d,
         method = 'REML')
draw(m, residuals = TRUE) # note: link and response scale are the same

#' `bs='ts'`; like `bs = 'tp'`, but can be shrunken to a null effect
#' additional penalty can be severe, but useful for model selection
m <- gam(formula = y ~ s(x, bs = 'ts'),
         family = gaussian(link = 'identity'),
         data = d,
         method = 'REML')
draw(m, residuals = TRUE)

#' `bs = 'cr'`; Cubic regression splines
#' uses the intergrated square second derivative cubic spline penalty
#' faster than TPRS but may not fit as well for low sample size
#' https://stats.stackexchange.com/questions/257228/gam-choosing-between-cubic-and-thin-plate-splines
m <- gam(formula = y ~ s(x, bs = 'cr'),
         family = gaussian(link = 'identity'),
         data = d,
         method = 'REML')
draw(m, residuals = TRUE)

#' `bs = 'cs'`; shrinkage version of `bs = 'cr'`
m <- gam(formula = y ~ s(x, bs = 'cs'),
         family = gaussian(link = 'identity'),
         data = d,
         method = 'REML')
draw(m, residuals = TRUE)

#' `bs = 'cc'`; cyclic version of `bs = 'cr'`
m <- gam(formula = y ~ s(x, bs = 'cc'),
         family = gaussian(link = 'identity'),
         data = d,
         method = 'REML',
         knots = list(x = c(0, 1)))
draw(m, residuals = TRUE)

# show the predictions are cyclical
tibble(x = seq(0, 3, by = 0.01), # predict past the [0, 1] interval
       mu_hat = predict(m, newdata = tibble(x))) %>%
  ggplot() +
  geom_line(aes(x, mu_hat))

m <- gam(formula = y ~ s(x, bs = 'cc'),
         family = gaussian(link = 'identity'),
         data = d,
         method = 'REML',
         knots = list(x = c(0, 1)))
draw(m, residuals = TRUE)

#' `bs = 'ad'`; wiggliness can vary over x -- only do this if necessary!
d_ad <- tibble(x = seq(0, 10, by = 0.1),
               y = sinpi(0.1 * x^2) + rnorm(length(x), sd = 0.1))
ggplot(d_ad, aes(x, y)) +
  geom_point()

# tp smoother can't fit the data well because wigglyness is fixed
m <- gam(formula = y ~ s(x, bs = 'tp'),
         family = gaussian(link = 'identity'),
         data = d_ad,
         method = 'REML')
draw(m, residuals = TRUE)

# ad can adapt to the pattern well
m <- gam(formula = y ~ s(x, bs = 'ad'),
         family = gaussian(link = 'identity'),
         data = d_ad,
         method = 'REML')
draw(m, residuals = TRUE, n = 1e4)

# skipping B-splines, P-splines, Gaussian processes

# multi-dimensional smooths ----
#' `bs = 'ds'`; Duchon splines
#' like TPRS, but for > 1 dimension
#' good when only part of the world was sampled

#' `bs = 'sos'`; splines on the sphere
#' 2D splines; like `bs = 'tp'` but on a sphere
#' good when a large part of the world was sampled (maybe including oceans)

#' `bs = 'so'`; Soap film smooths
#' 2D splines that are good for areas with sharp boundaries
#' see https://fromthebottomoftheheap.net/2016/03/27/soap-film-smoothers/

# random effects ----
#' `bs = 're'`; gaussian random effects
set.seed(0)
d <- tibble(l = rep(-3:3, 15), # random effect levels
            x = runif(length(l)), # 15 random U(0, 1) samples for 3 factors
            mu = 3 * sinpi(x) + l, # true response
            y = rnorm(n = length(mu), mean = mu, sd = 1)) %>%
  mutate(l = factor(l)) # need factors for REs (can't use characters)
d

ggplot(d, aes(x, y)) +
  facet_wrap(~ l) +
  geom_point()

m <- gam(formula = y ~ s(x) + s(l, bs = 're'),
         family = gaussian(link = 'identity'),
         data = d,
         method = 'REML')
draw(m)

#' `bs = 'mrf'`; Markov Random Fields
#' REs good for discrete contiguous geographic units
#' see https://fromthebottomoftheheap.net/2017/10/19/first-steps-with-mrf-smooths/
#' from `?mgcv::mrf`:
## Load Columbus Ohio crime data (see ?columbus for details and credits)
data(columb)       ## data frame
data(columb.polys) ## district shapes list
xt <- list(polys=columb.polys) ## neighbourhood structure info for MRF
par(mfrow=c(2,2))
## First a full rank MRF...
b <- gam(crime ~ s(district,bs="mrf",xt=xt),data=columb,method="REML")
plot(b,scheme=0)
## Compare to reduced rank version...
b <- gam(crime ~ s(district,bs="mrf",k=20,xt=xt),data=columb,method="REML")
plot(b,scheme=0)
## An important covariate added...
b <- gam(crime ~ s(district,bs="mrf",k=20,xt=xt)+s(income),
         data=columb,method="REML")
plot(b,scheme=c(0,1))

# one-dimensional factor smooths ----
#' `bs = 'fs'`; unconstrained factor smooths
#' good for smoooths for each factor with common smoothness parameter
#' uses `bs = 'tp'` by default in the background
#' includes random intercepts
m <- gam(formula = y ~ s(x, l, bs = 'fs'),
         family = gaussian(link = 'identity'),
         data = d,
         method = 'REML')
draw(m, residuals = TRUE) # residuals won't be plotted

#' `bs = 'sz'`; sum-to-zero factor smooths
#' good for smoooths measuring deviation from the average trend
#' uses `bs = 'tp'` by default in the background
#' includes random intercepts
m <- gam(formula = y ~ s(x) + s(x, l, bs = 'sz'),
         family = gaussian(link = 'identity'),
         data = d,
         method = 'REML')
draw(m, pages = 1)

#' a note on `by` smooths
#' good for smoooths for each factor with different smoothness parameters
#' *does not include fixed intercepts: need to include them explicitly!*
m <- gam(formula = y ~ l + #' *very important!*
           s(x, by = l),
         family = gaussian(link = 'identity'),
         data = d,
         method = 'REML')
draw(m, residuals = TRUE) # see different levels of smoothness; plotted separately

# interaction terms ----
# kelowna temperature data downloaded from https://kelowna.weatherstats.ca/download.html
weather <-
  read.csv('data/weatherstats_kelowna_daily.csv') %>%
  transmute(avg_temp_c = avg_temperature,
            date = as.POSIXct(date),
            year = year(date),
            doy = yday(date)) %>%
  filter(year >= 1975) # data prior to 1975 is odd
head(weather)

 #' using `s()` only
m <- bam(avg_temp_c ~ s(doy, bs = 'cc') + s(year, bs = 'tp', k = 10),
         family = gaussian(link = 'identity'),
         data = weather,
         method = 'REML',
         knots = list(doy = c(0.5, 366.5)))
# response scale = link scale, so plotting difference from the average
draw(m, rug = FALSE, n = 200)
summary(m)

#' using `te()`; similar to `year * doy`
m <- bam(avg_temp_c ~
           te(doy, year, k = c(10, 10), bs = c('cc', 'tp')),
         family = gaussian(link = 'identity'),
         data = weather,
         method = 'REML',
         knots = list(doy = c(0.5, 366.5)))
# response scale = link scale, so plotting difference from the average
draw(m, rug = FALSE, n = 200)
summary(m) # significance of the term is hard to interpret

#' using `ti()`; similar to `year + doy + year:doy`
m <- bam(avg_temp_c ~
           s(doy, k = 10, bs = 'cc') +
           s(year, k = 10, bs = 'tp') +
           ti(doy, year, k = c(5, 5), bs = c('cc', 'cr')),
         family = gaussian(link = 'identity'),
         data = weather,
         method = 'REML',
         knots = list(doy = c(0.5, 366.5)))
draw(m, rug = FALSE, scales = 'free', n = 200)
summary(m) # terms are indicated separately

#' increasing `k` for `year` to allow for more fluctuations
m <- bam(avg_temp_c ~
           s(doy, k = 10, bs = 'cc') +
           s(year, k = 50, bs = 'tp') +
           ti(doy, year, k = c(5, 5), bs = c('cc', 'cr')),
         family = gaussian(link = 'identity'),
         data = weather,
         method = 'REML',
         knots = list(doy = c(0.5, 366.5)))
draw(m, rug = FALSE, scales = 'free', n = 400) 
summary(m)
