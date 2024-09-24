lapply(c("tidyverse", "ggcorrplot", "brms"), library, character.only = TRUE)


dat <- read_csv("Burkina Faso/Burkina_Faso_SPEI-3_2005-2009_10percentile_drought.csv")

# Some notes about data: (1) Though there is lack of clarity in surveys and in metadata/recorded protocol, it appears that "household size" refers to people *currently* in the home, not migrants -- which is why migrant number can be bigger than household size. (2) NA for remittances is a zero -- these are households that did not report receiving money or goods from migrants but answered other questions, so there is not reason to think questions about remittances were skipped.

##### Data processing #####

# Get a usable date format
dat$interview_date <- as.Date(dat$date, format = "%m/%d/%Y")

# NAs are meaningful for remit
dat <- dat %>% mutate(remit = replace_na(remit, 0))

# Correlation structure between environmental variables of interest. By definition, these should be correlated a bit - and we don't see probelmatically high levels of correlation. Per best practices from Bayesian approaches (see discussions referenced in the URLs that follow), we just set the prior for each environmental variable to a normal distribution with constant variance to move the model away from a ridge in the posterior betweeen two or more environmental variables.
# https://statmodeling.stat.columbia.edu/2019/07/07/collinearity-in-bayesian-models/; https://mc-stan.org/docs/stan-users-guide/problematic-posteriors.html

cor.mat <- cor(dat[, c("dispersion", "frequency", "duration", "intensity")])
ggcorrplot(cor.mat)

##### Model #####

# Informative priors given some ridges in the posterior between intercept and intensity (again, not surprising given collinearity concerns raised above).

mod1 <- brm(remit ~ dispersion + frequency + intensity + duration + wealth_index + hh_size + migrant_num + (1 | date + census_tract), 
          data = dat,
          family = bernoulli,
          prior =
            prior(normal(2.5, 2.5), class = "Intercept") +
            prior(normal(0, 2.5), coef = "dispersion") +
            prior(normal(0, 2.5), coef = "frequency") +
            prior(normal(2.5, 2.5), coef = "intensity") +
            prior(normal(0, 2.5), coef = "duration") +
            prior(normal(0, 2.5), coef = "wealth_index") +
            prior(normal(0, 2.5), coef = "hh_size") +
            prior(normal(0, 2.5), coef = "migrant_num") +
            prior(cauchy(0,2), class = "sd")
          ) # Not included on this iteration: NDVI, distance to center, country.

# Ridge between intensity and intercept.