# Download data files first from: https://github.com/annethro/remittances/tree/main/SPEI%20CSVs

lapply(c("tidyverse", "ggcorrplot", "brms"), library, character.only = TRUE)


burkina <- read_csv("data/burkina faso.csv")
kenya <- read_csv("data/kenya.csv")
nigeria <- read_csv("data/nigeria.csv")

# Some notes about data: (1) Though there is lack of clarity in surveys and in metadata/recorded protocol, it appears that "household size" refers to people *currently* in the home, not migrants -- which is why migrant number can be bigger than household size. (2) NA for remittances is a zero -- these are households that did not report receiving money or goods from migrants but answered other questions, so there is not reason to think questions about remittances were skipped.

##### Data processing #####

# Date is unavailable for Nigeria, but data were collected between took place between Oct 5 and November 6, 2009

nigeria$interview_date <- as.Date("2009-10-21") # Halfway between Oct 5 and Nov 6

### Six households missing an interview date for Burkina (to be fixed with imputation during manuscript preparation)

# All interviews in this tract were Oct 30-31, so assigning each house one of the two based on a random draw.
#sample(1:2, 5, replace = T) # 2 2 2 1 2

burkina[which(is.na(burkina$date) & burkina$census_tract == "kawara"), c("date")] <- c("10/31/2009","10/31/2009","10/31/2009","10/30/2009","10/31/2009")

# All interviews in this tract 11/11 or 11/12, so draw one and replace:
#sample(1:2, 1, replace = T) # 2

burkina[which(is.na(burkina$date) & burkina$census_tract == "lantaga"), c("date")] <- c("11/12/2009")

# Get a usable date format
burkina$interview_date <- as.Date(burkina$date, format = "%m/%d/%Y")
kenya$interview_date <- as.Date(kenya$date, format = "%m/%d/%Y")

### Other date issues (to be fixed during cleaning during manuscript preparation) 

# One year was marked 1991 -- fix to be same year as other years in this tract
kenya$interview_date[(which(kenya$interview_date == "1991-12-13"))] <- "2009-12-13"

# One year was marked 2008 but with same day and month as other households in the census tract; fixing year
kenya$interview_date[(which(kenya$interview_date == "2008-12-10"))] <- "2009-12-10"

# One month was marked 02 but with same day and year as other households in the census tract; fixing month
kenya$interview_date[(which(kenya$interview_date == "2009-02-14"))] <- "2009-12-14"

# One month was marked 09 - looks to be 11, as day is 11 and many interviews took place on 11-11 but not 11-09
kenya$interview_date[(which(kenya$interview_date == "2009-09-11"))] <- "2009-11-11"

# Unlikely dates from Burkina (10-12 also suspect but I don't have a good explanation for that one)
burkina$interview_date[(which(burkina$interview_date == "2009-10-10"))] <- "2009-10-30"
burkina$interview_date[(which(burkina$interview_date == "2009-11-30"))] <- "2009-10-30"
burkina$interview_date[(which(burkina$interview_date == "2009-11-30"))] <- "2009-10-30"

# Merge data
burkina <- burkina %>%
  select(house, census_tract, country, interview_date, lat, long, hh_size, migrant_num, wealth_index, remit, frequency, severity, dispersion)

kenya <- kenya %>%
  select(house, census_tract, country, interview_date, lat, long, hh_size, migrant_num, wealth_index, remit, frequency, severity, dispersion)

nigeria <- nigeria %>%
  select(house, census_tract, country, interview_date, lat, long, hh_size, migrant_num, wealth_index, remit, frequency, severity, dispersion)

dat <- bind_rows(burkina, kenya, nigeria)

### Missing data for Jangachi, Nigeria was a coding error to be fixed during manuscript preparation

dat <- dat %>%
  filter(census_tract != "jangachi")

# NAs are meaningful for remit
dat <- dat %>% mutate(remit = replace_na(remit, 0))

##### Center and standardize variables #####

dat <- dat %>%
  mutate(date_s = scale(interview_date), lat_s = scale(lat), long_s = scale(long), hh_size_s = scale(hh_size), migrant_num_s = scale(migrant_num), wealth_index_s = scale(wealth_index), frequency_s = scale(frequency), severity_s = scale(severity), dispersion_s = scale(dispersion))

# Correlation structure between environmental variables of interest. By definition, these should be correlated a bit - and we don't see problematically high levels of correlation. Per best practices from Bayesian approaches (see discussions referenced in the URLs that follow), we just set the prior for each environmental variable to a normal distribution with constant variance to move the model away from a ridge in the posterior between two or more environmental variables.
# https://statmodeling.stat.columbia.edu/2019/07/07/collinearity-in-bayesian-models/; https://mc-stan.org/docs/stan-users-guide/problematic-posteriors.html

cor.mat <- cor(dat[, c("dispersion_s", "frequency_s", "severity_s")])
ggcorrplot(cor.mat)

##### Model #####

mod1 <- brm(remit ~ dispersion_s + frequency_s + severity_s + wealth_index_s + hh_size_s + migrant_num_s + (1 | date_s + census_tract + country), 
          data = dat,
          family = bernoulli,
          control = list(adapt_delta = 0.99),
          prior(cauchy(0,2), class = "sd")
          ) # Not included yet: NDVI, distance to center, country, spatial extent; data from three additional countries.

# Some mild (but not concerning) issues with effective sample size in interview date and country, driven by the fact that we don't have date for Nigeria and there are only three countries. This will be less of an issue with the full model.

# Convert to odds ratios

exp(cbind(Odds_Ratio = fixef(mod1)[,1], Lower = fixef(mod1, probs = c(.05, .95))[,3], Upper = fixef(mod1, probs = c(.5, .95))[,4]))
