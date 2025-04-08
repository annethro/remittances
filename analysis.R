# Download data files first from: https://github.com/annethro/remittances/

lapply(c("tidyverse", "geosphere", "ggcorrplot", "rnaturalearth", "sf", "ggpubr", "brms", "bayesplot", "officer", "flextable", "plotly", "webshot", "htmlwidgets"), library, character.only = TRUE)

#webshot::install_phantomjs(force = TRUE) # Only run once, if you want to export the interactive interaction plot in html. Force = TRUE because was available for R 4.4.0 but not 4.4.3

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

set.seed(1742049459) # Seed selected from date/time on 3-15-25


burkina <- read_csv("data/burkinafaso_SPEI-3_2005-2009_10percentile_1981-2009_means.csv")
kenya <- read_csv("data/kenya_SPEI-3_2005-2009_10percentile_1981-2009_means.csv")
nigeria <- read_csv("data/nigeria_SPEI-3_2005-2009_10percentile_1981-2009_means.csv")
senegal <- read_csv("data/senegal_SPEI-3_2005-2009_10percentile_1981-2009_means.csv")
south_africa <- read_csv("data/southafrica_SPEI-3_2005-2009_10percentile_1981-2009_means.csv")
uganda <- read_csv("data/uganda_SPEI-3_2005-2009_10percentile_1981-2009_means.csv")

# We include a data dictionary on GitHub, but some notes here: lat and long are where the participant's census tract is located, based on coding done by JHJ and AP; lat_SPEI and long_SPEI refer to the nearest point we have for precipitation for SPEI, so one can examine the distance as the crow flies between participant's census tract and the nearest precipitation point; pop_cent_lat and pop_cent_long refer to the nearest population center according to nighttime lights data; we use this to calculate distance as the crow flies below.

# Some notes about data: (1) Though there is some lack of clarity in surveys and in metadata/recorded protocol, it appears that "household size" refers to people *currently* in the home, not migrants -- which is why migrant number can be bigger than household size. (2) NA for remittances is a zero -- these are households that did not report receiving money or goods from migrants but answered other questions, so there is not reason to think questions about remittances were skipped. (3) Reminder that we're using avg_area and distance in meters...!

##### Column renaming with caveats #####

# We did not have census-tract-level data for South Africa -- nothing more fine-grained than municipality. Renaming the municipality calling while flagging this for everyone.

south_africa$census_tract <- south_africa$municipality


##### Data processing #####

### Date is unavailable for some countries, 

# Nigeria: data were collected between took place between Oct 5 and November 6, 2009

nigeria$interview_date <- as.Date("2009-10-21") # Halfway between Oct 5 and Nov 6

# Senegal: All data colection took place in Oct and Nov 2009

senegal$interview_date <- as.Date("2009-11-01") # Halfway

# South Africa: All data collection took place between Nov 13 and Dec 23 2009

south_africa$interview_date <- as.Date("2009-12-07") # Halfway


### Six households missing an interview date for Burkina (to be fixed with imputation during manuscript preparation)

# All interviews in this tract were Oct 30-31, so assigning each house one of the two based on this random draw.
#sample(1:2, 5, replace = T) # 2 2 2 1 2

burkina[which(is.na(burkina$date) & burkina$census_tract == "kawara"), c("date")] <- c("10/31/2009","10/31/2009","10/31/2009","10/30/2009","10/31/2009")

# All interviews in this tract 11/11 or 11/12, so assigning each house one of the two based on this random draw:
#sample(1:2, 1, replace = T) # 2

burkina[which(is.na(burkina$date) & burkina$census_tract == "lantaga"), c("date")] <- c("11/12/2009")

# Likely error
uganda$date[uganda$date == "2010-08-18"] <- "2010-03-18"

# Get a usable date format
burkina$interview_date <- as.Date(burkina$date, format = "%m/%d/%Y")
kenya$interview_date <- as.Date(kenya$date, format = "%m/%d/%Y")
uganda$interview_date <- as.Date(uganda$date, format = "%m/%d/%Y")

### Merge data ###

burkina <- burkina %>%
  select(house, census_tract, country, interview_date, lat, long, hh_size, migrant_num, wealth_index, remit, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity, pop_cent_lat, pop_cent_long)

kenya <- kenya %>%
  select(house, census_tract, country, interview_date, lat, long, hh_size, migrant_num, wealth_index, remit, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity, pop_cent_lat, pop_cent_long)

nigeria <- nigeria %>%
  select(house, census_tract, country, interview_date, lat, long, hh_size, migrant_num, wealth_index, remit, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity, pop_cent_lat, pop_cent_long)

senegal <- senegal %>%
  select(house, census_tract, country, interview_date, lat, long, hh_size, migrant_num, wealth_index, remit, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity, pop_cent_lat, pop_cent_long)

south_africa <- south_africa %>%
  select(house, census_tract, country, interview_date, lat, long, hh_size, migrant_num, wealth_index, remit, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity, pop_cent_lat, pop_cent_long)

uganda <- uganda %>%
  select(house, census_tract, country, interview_date, lat, long, hh_size, migrant_num, wealth_index, remit, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity, pop_cent_lat, pop_cent_long)

dat <- bind_rows(burkina, kenya, nigeria, senegal, south_africa, uganda)


# NAs are meaningful for remit
dat <- dat %>% mutate(remit = replace_na(remit, 0))

# All NAs for autocorrelation are zeroes -- ACF returns NAs when there's no 1s for a binary time series

dat$autocorrelation[is.na(dat$autocorrelation)] <- 0

# Remove rows with no location data

dat <- dat[!is.na(dat$lat), ]

##### Calculate distance as the crow flies to the nearest population center #####

dat <- dat %>%
  mutate(pop_center = distHaversine(as.matrix(dat[ , c("long", "lat")]), as.matrix(dat[ , c("pop_cent_long", "pop_cent_lat")]))/1000) # Convert to km

# Converting avg_area to km, it's just too big to display

dat$avg_area_km <- dat$avg_area/1000

### Important: flipping the sign on severity to improve interpretability: higher values means more severity

dat$severity_pos <- -dat$severity

# Observations: some of the big clumps of data with no variability are South Africa, where we didn't have anything finer-grained than municipality (so LOTS of people with the same lat-long).

##### Center and standardize variables #####

dat <- dat %>%
  mutate(date_s = scale(interview_date), hh_size_s = scale(hh_size), migrant_num_s = scale(migrant_num), wealth_index_s = scale(wealth_index), annual_frequency_s = scale(annual_frequency), severity_s = scale(severity_pos), autocorrelation_s = scale(autocorrelation), avg_area_km_s = scale(avg_area_km), NDVI_mean_s = scale(NDVI_mean), SVI_severity_s = scale(SVI_severity), pop_center_s = scale(pop_center))


##### Descriptive stats #####

# Correlation structure between environmental variables of interest. By definition, these should be correlated a bit - and we don't see problematically high levels of correlation. Per best practices from Bayesian approaches (see discussions referenced in the URLs that follow), we just set the prior for each environmental variable to a normal distribution with constant variance to move the model away from a ridge in the posterior between two or more environmental variables.
# https://statmodeling.stat.columbia.edu/2019/07/07/collinearity-in-bayesian-models/; https://mc-stan.org/docs/stan-users-guide/problematic-posteriors.html

cor.mat <- cor(dat[, c("NDVI_mean_s", "pop_center_s","migrant_num_s", "hh_size_s", "wealth_index_s", "avg_area_km_s", "autocorrelation_s", "annual_frequency_s", "severity_s")], use = "complete.obs")
ggcorrplot(cor.mat)

# Highest correlation (from me checking various things) is severity with area affected but coming in at 58%, so feeling okay running them together in the same model -- but including normal priors to attempt to avoid ridges in posterior estimates


### Overall descriptives

contin <- dat [ , c("country", "remit", "autocorrelation", "annual_frequency", "severity_pos", "avg_area_km", "NDVI_mean", "wealth_index_s", "pop_center", "hh_size", "migrant_num")]

variable_names <- c( # Lookup table for renaming
  "country" = "Country", 
  "remit" = "Remittance (pres/abs)", 
  "autocorrelation" = "Autocorrelation",
  "annual_frequency" = "Frequency",
  "severity_pos" = "Severity",
  "avg_area_km" = "Spatial extent",
  "NDVI_mean" = "Mean NDVI",
  "wealth_index_s" = "Wealth index (std.)",
  "pop_center" = "Dist. to pop. center",
  "hh_size" = "Household size",
  "migrant_num" = "Number of migrants")

# Function to compute descriptive stats for the whole dataset
compute_stats_whole_sample <- function(data) {
  data %>%
    select(-country) %>%  # Remove country since we're aggregating everything
    pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
    mutate(Variable = recode(Variable, !!!variable_names)) %>%
    group_by(Variable) %>%
    summarise(
      Mean = round(mean(Value, na.rm = TRUE), 2),
      SD = round(sd(Value, na.rm = TRUE), 2),
      Min = round(min(Value, na.rm = TRUE), 2),
      Max = round(max(Value, na.rm = TRUE), 2),
      .groups = "drop"
    )
}

# Generate summary table for whole dataset
summary_table_whole <- compute_stats_whole_sample(contin)

# Create a flextable for Word formatting
ft_whole <- flextable(summary_table_whole) %>%
  theme_vanilla() %>%
  bold(part = "header") %>%
  align(j = 2:5, align = "center", part = "all") %>%
  colformat_num(j = 2:5, digits = 2) %>%
  autofit()

# Create a Word document and add the table
doc <- read_docx() %>%
  body_add_flextable(ft_whole)

# Save the Word document
print(doc, target = "Descriptives_Total.docx")

### By country

contin_c <- dat [ , c("country", "remit", "autocorrelation", "annual_frequency", "severity_pos", "avg_area_km", "NDVI_mean", "wealth_index", "pop_center", "hh_size", "migrant_num")]

variable_names_c <- c( # Lookup table for renaming
  "country" = "Country", 
  "remit" = "Remittance (pres/abs)", 
  "autocorrelation" = "Autocorrelation",
  "annual_frequency" = "Frequency",
  "severity_pos" = "Severity",
  "avg_area_km" = "Spatial extent",
  "NDVI_mean" = "Mean NDVI",
  "wealth_index" = "Wealth index",
  "pop_center" = "Dist. to pop. center",
  "hh_size" = "Household size",
  "migrant_num" = "Number of migrants")

country_names <- c(
  "burkina_faso" = "Burkina Faso",
  "kenya" = "Kenya",
  "nigeria" = "Nigeria",
  "senegal" = "Senegal",
  "south_africa" = "South Africa",
  "uganda" = "Uganda"
)

compute_stats <- function(data) {
  data %>%
    pivot_longer(-country, names_to = "Variable", values_to = "Value") %>%
    mutate(Variable = recode(Variable, !!!variable_names_c)) %>%  # Rename variables
    mutate(country = recode(country, !!!country_names)) %>%  #
    group_by(Variable, country) %>%
    summarise(
      Mean = round(mean(Value, na.rm = TRUE), 2),
      SD = round(sd(Value, na.rm = TRUE), 2),
      Min = round(min(Value, na.rm = TRUE), 2),
      Max = round(max(Value, na.rm = TRUE), 2),
      .groups = "drop"
    )
}

ds_country <- data.frame(compute_stats(contin_c))

# Create a flextable for Word formatting
ft <- flextable(ds_country) %>%
  theme_vanilla() %>%  # Apply a clean style
  bold(part = "header") %>%
  align(j = 3:6, align = "center", part = "all") %>%  # Center numeric columns
  colformat_num(j = 3:6, digits = 2) %>% # Two decimal places
  autofit()  # Adjust column sizes automatically

# Create a Word document and add the table
doc <- read_docx() %>%
  body_add_flextable(ft)

# Save the Word document
print(doc, target = "Descriptives_Country.docx")

##### Data coverage 3d scatterplot #####

# As we'll see later, there's some action for households experienced high severity, high annual_frequency, and large spatial extent droughts -- let's make sure there's good data coverage in that part of the state space and it's not being driven by few observations (spoiler: looks like it is)

descrip_3d <- plot_ly(dat, 
        x = ~severity_s, 
        y = ~annual_frequency_s, 
        z = ~avg_area_km_s, 
        color = ~country, 
        colors = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02"), 
        type = "scatter3d", 
        mode = "markers", 
        marker = list(size = 4, opacity = 0.7)) %>%
  layout(
  title = NULL,
  showlegend = TRUE,
  legend = list(font = list(size = 20)),
  scene = list(
    xaxis = list(title = "Severity", titlefont = list(size = 18), tickfont = list(size = 14)),
    yaxis = list(title = "Frequency", titlefont = list(size = 18), tickfont = list(size = 14)),
    zaxis = list(title = "Spatial Extent", titlefont = list(size = 18), tickfont = list(size = 14)),
    coloraxis = list(title = "Country", titlefont = list(size = 18), tickfont = list(size = 14))
  )
 )

saveWidget(descrip_3d, "3D_Descriptive_Plot.html", selfcontained = TRUE)

##### Heat map figures #####

africa <- ne_countries(continent = "africa")

### annual_frequency

freq <- dat[ , c("long", "lat", "annual_frequency")]
freq_sf <- st_as_sf(freq, coords = c("long", "lat"), crs = 4326)

freq_plot <- ggplot() +
  geom_sf(data = africa, fill = "lightgray", color = "black") +
  geom_point(data = freq, aes(x = long, y = lat, color = annual_frequency), size = 3) +
  scale_color_viridis_c(name = "Frequency") +
  theme_classic() +
  labs(#title = "annual_frequency",
       x = NULL, 
       y = NULL) +
  theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), 
        #legend.position = "none")
        title = element_text(size = 25), 
        legend.title = element_text(size = 20), legend.text = element_text(size = 15))

### autocorrelation

auto <- dat[ , c("long", "lat", "autocorrelation")]
auto_sf <- st_as_sf(auto, coords = c("long", "lat"), crs = 4326)

auto_plot <- ggplot() +
  geom_sf(data = africa, fill = "lightgray", color = "black") +
  geom_point(data = auto, aes(x = long, y = lat, color = autocorrelation), size = 3) +
  scale_color_viridis_c(name = "Autocorrelation") +
  theme_classic() +
  labs(#title = "Temporal Autocorrelation",
       x = NULL, 
       y = NULL) +
  theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(),
        #legend.position = "none")
        title = element_text(size = 25), 
        legend.title = element_text(size = 20), legend.text = element_text(size = 15))

### Severity

sev <- dat[ , c("long", "lat", "severity_pos")]
sev_sf <- st_as_sf(sev, coords = c("long", "lat"), crs = 4326)

sev_plot <- ggplot() +
  geom_sf(data = africa, fill = "lightgray", color = "black") +
  geom_point(data = sev, aes(x = long, y = lat, color = severity_pos), size = 3) +
  scale_color_viridis_c(name = "Severity") +
  theme_classic() +
  labs(#title = "Severity",
    x = NULL, 
    y = NULL) +
  theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(),
        #legend.position = "none")
        title = element_text(size = 25), 
        legend.title = element_text(size = 20), legend.text = element_text(size = 15))

### Spatial extent

spa <- dat[dat$avg_area_km < 700000, c("long", "lat", "avg_area_km")]
spa_sf <- st_as_sf(spa, coords = c("long", "lat"), crs = 4326)

spa_plot <- ggplot() +
  geom_sf(data = africa, fill = "lightgray", color = "black") +
  geom_point(data = spa, aes(x = long, y = lat, color = avg_area_km), size = 3) +
  scale_color_viridis_c(name = "Spatial extent") +
  theme_classic() +
  labs(#title = "Spatial extent",
    x = NULL, 
    y = NULL) +
  theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(),
       # legend.position = "none")
        title = element_text(size = 25), 
        legend.title = element_text(size = 20), legend.text = element_text(size = 15))


ggarrange(freq_plot, auto_plot, sev_plot, spa_plot,
          ncol = 2, nrow = 2)

############# MAIN MODEL ################

mod1 <- brm(remit ~ 
              severity_s + annual_frequency_s + autocorrelation_s + avg_area_km_s + # Environmental predictors of interest
              wealth_index_s + hh_size_s + migrant_num_s + pop_center_s + NDVI_mean_s + # Controls
              (1 | date_s + census_tract + country), 
          data = dat,
          family = bernoulli,
          control = list(adapt_delta = 0.99),
          prior = c(prior(cauchy(0, 2), class = "sd"),
                    prior(normal(0, 1), class = "b")
          )
)


# Convert to odds ratios

ests_mod1 <- data.frame(exp(cbind(Odds_Ratio = fixef(mod1)[,1], Lower = fixef(mod1, probs = c(.05, .95))[,3], Upper = fixef(mod1, probs = c(.5, .95))[,4])))

ests_mod1$parameters <- c("Intercept", "Severity", "Frequency", "Autocorrelation", "Spatial extent", "Wealth", "Household size", "Migrant number", "Dist. to pop. center", "Mean NDVI")

### Posterior checks ###

# For variables moderately correlated, check for signs of ridges

posterior <- as.array(mod1)

color_scheme_set("pink")
mcmc_pairs(posterior, pars = c("b_NDVI_mean_s", "b_avg_area_km_s", "b_annual_frequency_s", "b_autocorrelation_s", "b_severity_s"),
           off_diag_args = list(size = 1.5))

bayes_R2(mod1)

### Caterpillar plot ###

ests_mod1 <- ests_mod1 %>%
  mutate(parameters = factor(parameters, levels = c("Mean NDVI", "Dist. to pop. center", "Migrant number", "Household size", "Wealth", "Spatial extent", "Autocorrelation", "Frequency", "Severity", "Intercept")))

ggplot(ests_mod1, aes(x = parameters, y = Odds_Ratio)) +
  geom_pointrange(aes(ymin = Lower, ymax = Upper), color = "blue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, face = "bold"),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 15, face = "bold"),
  ) +
  labs(y = "Odds Ratio with 90% Credible Interval")


############# EXPLORATORY ANALYSES ################

##### Check main model fit robustness to exclusion of extreme values #####

# High values of for spatial extent...

dat_noext <- dat[dat$avg_area_km_s <= 3,] # 371 observations. Only have one annual_frequency (medium low; -1.08251295545341), one autocorrelation (0.2), though a range of severities. 2/3 remit, all are from Nigeria

# High severity...

dat_noext <- dat_noext[dat_noext$severity_s <= 3,] # 9 observations. All identical on spatial extent, annual_frequency, severity, and autocorrelation; 2 remit; all uganda. 

mod1_noext <- brm(remit ~ 
              severity_s + annual_frequency_s + autocorrelation_s + avg_area_km_s + # Environmental predictors of interest
              wealth_index_s + hh_size_s + migrant_num_s + pop_center_s + NDVI_mean_s + # Controls
              (1 | date_s + census_tract + country), 
            data = dat_noext,
            family = bernoulli,
            control = list(adapt_delta = 0.99),
            prior = c(prior(cauchy(0, 2), class = "sd"),
                      prior(normal(0, 1), class = "b")
            )
)

ests_mod1_noext <- data.frame(exp(cbind(Odds_Ratio = fixef(mod1_noext)[,1], Lower = fixef(mod1_noext, probs = c(.05, .95))[,3], Upper = fixef(mod1_noext, probs = c(.5, .95))[,4])))

bayes_R2(mod1_noext) #0.334

posterior <- as.array(mod1_noext)

color_scheme_set("pink")
mcmc_pairs(posterior, pars = c("b_NDVI_mean_s", "b_avg_area_km_s", "b_annual_frequency_s", "b_autocorrelation_s", "b_severity_s"),
           off_diag_args = list(size = 1.5))

### Caterpillar plot ###

ests_mod1_noext$parameters <- c("Intercept", "Severity", "Frequency", "Autocorrelation", "Spatial extent", "Wealth", "Household size", "Migrant number", "Dist. to pop. center", "Mean NDVI")

ests_mod1_noext <- ests_mod1_noext %>%
  mutate(parameters = factor(parameters, levels = c("Mean NDVI", "Dist. to pop. center", "Migrant number", "Household size", "Wealth", "Spatial extent", "Autocorrelation", "Frequency", "Severity", "Intercept")))

ggplot(ests_mod1_noext, aes(x = parameters, y = Odds_Ratio)) +
  geom_pointrange(aes(ymin = Lower, ymax = Upper), color = "blue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, face = "bold"),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 15, face = "bold"),
  ) +
  labs(y = "Odds Ratio with 90% Credible Interval")

##### Time window #####

# What if hydroclimate patterns are considered over 10 years instead of 5?

### Load data ###
burkina_10 <- read_csv("data/burkinafaso_SPEI-3_2000-2009_10percentile_1981-2009_means.csv")
kenya_10 <- read_csv("data/kenya_SPEI-3_2000-2009_10percentile_1981-2009_means.csv")
nigeria_10 <- read_csv("data/nigeria_SPEI-3_2000-2009_10percentile_1981-2009_means.csv")
senegal_10 <- read_csv("data/senegal_SPEI-3_2000-2009_10percentile_1981-2009_means.csv")
south_africa_10 <- read_csv("data/southafrica_SPEI-3_2000-2009_10percentile_1981-2009_means.csv")
uganda_10 <- read_csv("data/uganda_SPEI-3_2000-2009_10percentile_1981-2009_means.csv")

south_africa_10$census_tract <- south_africa_10$municipality

### Subset; make adjustments; merge with already-processed columns from above ###

burkina_10 <-  burkina_10 %>%
  select(house, country, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity)
kenya_10 <-  kenya_10 %>%
  select(house, country, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity)
nigeria_10 <-  nigeria_10 %>%
  select(house, country, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity)
senegal_10 <-  senegal_10 %>%
  select(house, country, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity)
south_africa_10 <-  south_africa_10 %>%
  select(house, country, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity)
uganda_10 <-  uganda_10 %>%
  select(house, country, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity)

sub_10 <- bind_rows(burkina_10, kenya_10, nigeria_10, senegal_10, south_africa_10, uganda_10)

sub_10$autocorrelation[is.na(sub_10$autocorrelation)] <- 0

sub_10$avg_area_km <- sub_10$avg_area/1000

sub_10$severity_pos <- -sub_10$severity # When things look clustered at the lowest or highest values, reminder that those are zeroes

sub_10 <- sub_10 %>%
  mutate(annual_frequency_s = scale(annual_frequency), severity_s = scale(severity_pos), autocorrelation_s = scale(autocorrelation), avg_area_km_s = scale(avg_area_km), NDVI_mean_s = scale(NDVI_mean), SVI_severity_s = scale(SVI_severity))

sub_10 <-  sub_10 %>%
  select(house, country, annual_frequency_s, severity_s, autocorrelation_s, avg_area_km_s, NDVI_mean_s, SVI_severity_s)

# Going to put a prefix on columns before merge

columns_to_prefix <- c("annual_frequency_s", "severity_s", "autocorrelation_s", "avg_area_km_s", "NDVI_mean_s", "SVI_severity_s")

sub_10a <- sub_10 %>% 
  rename_with(~ paste0("yr10_", .), all_of(columns_to_prefix))

# Merge

dat_10 <- dat %>%
  select(house, census_tract, country, remit, date_s, hh_size_s, migrant_num_s, wealth_index_s, pop_center_s)

dat_sub_10 <- inner_join(dat_10, sub_10a, by = c("house", "country"))

### Check correlations ###

# A correlation about 0.7, but the weakly informative prior preventing ridges in the posterior suggesting fits are still fine.

cor.mat_10 <- cor(dat_sub_10[, c("yr10_severity_s", "yr10_annual_frequency_s", "yr10_autocorrelation_s", "yr10_avg_area_km_s", "wealth_index_s", "hh_size_s", "migrant_num_s", "pop_center_s", "yr10_NDVI_mean_s")], use = "complete.obs")
# Still some moderate correlations to look out for (posterior checks), up to 0.6

### Analysis ###

mod_10 <- brm(remit ~ 
              yr10_severity_s + yr10_annual_frequency_s + yr10_autocorrelation_s + yr10_avg_area_km_s + # Environmental predictors of interest
              wealth_index_s + hh_size_s + migrant_num_s + pop_center_s + yr10_NDVI_mean_s + # Controls
              (1 | date_s + census_tract + country), 
            data = dat_sub_10,
            family = bernoulli,
            control = list(adapt_delta = 0.99),
            prior = c(prior(cauchy(0, 2), class = "sd"),
                      prior(normal(0, 1), class = "b")
            )
)

posterior_10 <- as.array(mod_10)

color_scheme_set("pink")
mcmc_pairs(posterior_10, pars = c("b_yr10_NDVI_mean_s", "b_yr10_avg_area_km_s", "b_yr10_annual_frequency_s", "b_yr10_autocorrelation_s", "b_yr10_severity_s"),
           off_diag_args = list(size = 1.5))

ests_mod10 <- data.frame(exp(cbind(Odds_Ratio = fixef(mod_10)[,1], Lower = fixef(mod_10, probs = c(.05, .95))[,3], Upper = fixef(mod_10, probs = c(.5, .95))[,4])))

ests_mod10$parameters <- c("Intercept", "Severity", "Frequency",  "Autocorrelation", "Spatial extent", "Wealth", "Household size", "Migrant number", "Dist. to pop. center", "Mean NDVI")


ests_mod10 <- ests_mod10 %>%
  mutate(parameters = factor(parameters, levels = c("Mean NDVI", "Dist. to pop. center", "Migrant number", "Household size", "Wealth", "Spatial extent", "Autocorrelation", "Frequency", "Severity",  "Intercept")))

ggplot(ests_mod10, aes(x = parameters, y = Odds_Ratio)) +
  geom_pointrange(aes(ymin = Lower, ymax = Upper), color = "blue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, face = "bold"),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 15, face = "bold"),
  ) +
  labs(y = "Odds Ratio with 90% Credible Interval")

bayes_R2(mod_10)


##### Thresholds #####

# What if we use -1.5 SD thresholds, as SD approaches are common in the literature, instead of a percentile threshold?

### Load data ###
burkina_15 <- read_csv("data/burkinafaso_SPEI-3_2005-2009_-1.5stddev_1981-2009_means.csv")
kenya_15 <- read_csv("data/kenya_SPEI-3_2005-2009_-1.5stddev_1981-2009_means.csv")
nigeria_15 <- read_csv("data/nigeria_SPEI-3_2005-2009_-1.5stddev_1981-2009_means.csv")
senegal_15 <- read_csv("data/senegal_SPEI-3_2005-2009_-1.5stddev_1981-2009_means.csv")
south_africa_15 <- read_csv("data/southafrica_SPEI-3_2005-2009_-1.5stddev_1981-2009_means.csv")
uganda_15 <- read_csv("data/uganda_SPEI-3_2005-2009_-1.5stddev_1981-2009_means.csv")

### Subset; make adjustments; merge with already-processed columns from above ###

burkina_15 <-  burkina_15 %>%
  select(house, country, annual_frequency, severity, autocorrelation, avg_area)
kenya_15 <-  kenya_15 %>%
  select(house, country, annual_frequency, severity, autocorrelation, avg_area)
nigeria_15 <-  nigeria_15 %>%
  select(house, country, annual_frequency, severity, autocorrelation, avg_area)
senegal_15 <-  senegal_15 %>%
  select(house, country, annual_frequency, severity, autocorrelation, avg_area)
south_africa_15 <-  south_africa_15 %>%
  select(house, country, annual_frequency, severity, autocorrelation, avg_area)
uganda_15 <-  uganda_15 %>%
  select(house, country, annual_frequency, severity, autocorrelation, avg_area)

sub_15 <- bind_rows(burkina_15, kenya_15, nigeria_15, senegal_15, south_africa_15, uganda_15)

sub_15$autocorrelation[is.na(sub_15$autocorrelation)] <- 0

sub_15$avg_area_km <- sub_15$avg_area/1000

sub_15$severity_pos <- -sub_15$severity # When things look clustered at the lowest or highest values, reminder that those are zeroes

sub_15 <- sub_15 %>%
  mutate(annual_frequency_s = scale(annual_frequency), severity_s = scale(severity_pos), autocorrelation_s = scale(autocorrelation), avg_area_km_s = scale(avg_area_km))

sub_15 <-  sub_15 %>%
  select(house, country, annual_frequency_s, severity_s, autocorrelation_s, avg_area_km_s)

# Going to put a prefix on columns before merge

columns_to_prefix <- c("annual_frequency_s", "severity_s", "autocorrelation_s", "avg_area_km_s")

sub_15a <- sub_15 %>% 
  rename_with(~ paste0("sd_", .), all_of(columns_to_prefix))

# Merge

dat_15 <- dat %>%
  select(house, census_tract, country, remit, date_s, hh_size_s, migrant_num_s, wealth_index_s, pop_center_s, NDVI_mean_s, SVI_severity_s)

dat_sub_15 <- inner_join(dat_15, sub_15a, by = c("house", "country"))

### Check correlations ###

cor.mat_15 <- cor(dat_sub_15[, c("sd_severity_s",  "sd_annual_frequency_s", "sd_autocorrelation_s", "sd_avg_area_km_s", "wealth_index_s", "hh_size_s", "migrant_num_s", "pop_center_s",  "NDVI_mean_s")], use = "complete.obs")
# Still some moderate correlations to look out for (posterior checks), up to 0.51

### Analysis ###

mod_15 <- brm(remit ~ 
                sd_severity_s + sd_annual_frequency_s + sd_autocorrelation_s + sd_avg_area_km_s + # Environmental predictors of interest
                wealth_index_s + hh_size_s + migrant_num_s + pop_center_s + NDVI_mean_s + # Controls
                (1 | date_s + census_tract + country), 
              data = dat_sub_15,
              family = bernoulli,
              control = list(adapt_delta = 0.99),
              prior = c(prior(cauchy(0, 2), class = "sd"),
                        prior(normal(0, 1), class = "b")
              )
)

bayes_R2(mod_15)

posterior_15 <- as.array(mod_15)

color_scheme_set("pink")
mcmc_pairs(posterior_15, pars = c("b_NDVI_mean_s", "b_sd_avg_area_km_s", "b_sd_annual_frequency_s", "b_sd_autocorrelation_s", "b_sd_severity_s"),
           off_diag_args = list(size = 1.5))

ests_mod15 <- data.frame(exp(cbind(Odds_Ratio = fixef(mod_15)[,1], Lower = fixef(mod_15, probs = c(.05, .95))[,3], Upper = fixef(mod_15, probs = c(.5, .95))[,4])))

ests_mod15$parameters <- c("Intercept", "Severity", "Frequency", "Autocorrelation", "Spatial extent", "Wealth", "Household size", "Migrant number", "Dist. to pop. center", "Mean NDVI")


ests_mod15 <- ests_mod15 %>%
  mutate(parameters = factor(parameters, levels = c("Mean NDVI", "Dist. to pop. center", "Migrant number", "Household size", "Wealth", "Spatial extent", "Autocorrelation", "Frequency", "Severity", "Intercept")))

ggplot(ests_mod15, aes(x = parameters, y = Odds_Ratio)) +
  geom_pointrange(aes(ymin = Lower, ymax = Upper), color = "blue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, face = "bold"),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 15, face = "bold"),
  ) +
  labs(y = "Odds Ratio with 90% Credible Interval")


##### SPEI length #####

# What if we consider 12-month SPEI, which is more similar to the Palmer Drought Index and can better capture effects on water supply, instead of the 3-month?

### Load data ###
burkina_12 <- read_csv("data/burkinafaso_SPEI-12_2005-2009_10percentile_1981-2009_means.csv")
kenya_12 <- read_csv("data/kenya_SPEI-12_2005-2009_10percentile_1981-2009_means.csv")
nigeria_12 <- read_csv("data/nigeria_SPEI-12_2005-2009_10percentile_1981-2009_means.csv")
senegal_12 <- read_csv("data/senegal_SPEI-12_2005-2009_10percentile_1981-2009_means.csv")
south_africa_12 <- read_csv("data/southafrica_SPEI-12_2005-2009_10percentile_1981-2009_means.csv")
uganda_12 <- read_csv("data/uganda_SPEI-12_2005-2009_10percentile_1981-2009_means.csv")


### Subset; make adjustments; merge with already-processed columns from above ###

burkina_12 <-  burkina_12 %>%
  select(house, country, annual_frequency, severity, autocorrelation, avg_area)
kenya_12 <-  kenya_12 %>%
  select(house, country, annual_frequency, severity, autocorrelation, avg_area)
nigeria_12 <-  nigeria_12 %>%
  select(house, country, annual_frequency, severity, autocorrelation, avg_area)
senegal_12 <-  senegal_12 %>%
  select(house, country, annual_frequency, severity, autocorrelation, avg_area)
south_africa_12 <-  south_africa_12 %>%
  select(house, country, annual_frequency, severity, autocorrelation, avg_area)
uganda_12 <-  uganda_12 %>%
  select(house, country, annual_frequency, severity, autocorrelation, avg_area)

sub_12 <- bind_rows(burkina_12, kenya_12, nigeria_12, senegal_12, south_africa_12, uganda_12)

sub_12$autocorrelation[is.na(sub_12$autocorrelation)] <- 0

sub_12$avg_area_km <- sub_12$avg_area/1000

sub_12$severity_pos <- -sub_12$severity # When things look clustered at the lowest or highest values, reminder that those are zeroes

sub_12 <- sub_12 %>%
  mutate(annual_frequency_s = scale(annual_frequency), severity_s = scale(severity_pos), autocorrelation_s = scale(autocorrelation), avg_area_km_s = scale(avg_area_km))

sub_12 <-  sub_12 %>%
  select(house, country, annual_frequency_s, severity_s, autocorrelation_s, avg_area_km_s)
# Notes from plotting checks: unsurprisingly, there's only seven "levels" of annual_frequency here since we're working with a 12-month SPEI over a window of 5 years.
# You can see the zeroes hanging out at extreme high values for autocorrelation -- this is not a surprise and the zeroes are meaningful.


# Going to put a prefix on columns before merge

columns_to_prefix <- c("annual_frequency_s", "severity_s", "autocorrelation_s", "avg_area_km_s")

sub_12a <- sub_12 %>% 
  rename_with(~ paste0("spei12_", .), all_of(columns_to_prefix))

# Merge

dat_12 <- dat %>%
  select(house, census_tract, country, remit, date_s, hh_size_s, migrant_num_s, wealth_index_s, pop_center_s, NDVI_mean_s, SVI_severity_s)

dat_sub_12 <- inner_join(dat_12, sub_12a, by = c("house", "country"))

### Check correlations ###

# A correlation about 0.7, but the weakly informative prior preventing ridges in the posterior suggesting fits are still fine.

cor.mat_12 <- cor(dat_sub_12[, c("spei12_severity_s", "spei12_annual_frequency_s", "spei12_autocorrelation_s", "spei12_avg_area_km_s", "hh_size_s", "migrant_num_s", "wealth_index_s", "pop_center_s", "NDVI_mean_s")], use = "complete.obs")
# Moderate correlations to look out for (posterior checks), up to 0.63

### Analysis ###

mod_12 <- brm(remit ~ 
                spei12_severity_s + spei12_annual_frequency_s + spei12_autocorrelation_s + spei12_avg_area_km_s + # Environmental predictors of interest
                wealth_index_s + hh_size_s + migrant_num_s + pop_center_s + NDVI_mean_s + # Controls
                (1 | date_s + census_tract + country), 
              data = dat_sub_12,
              family = bernoulli,
              control = list(adapt_delta = 0.99),
              prior = c(prior(cauchy(0, 2), class = "sd"),
                        prior(normal(0, 1), class = "b")
              )
)

bayes_R2(mod_12)

posterior_12 <- as.array(mod_12)

color_scheme_set("pink")
mcmc_pairs(posterior_12, pars = c("b_NDVI_mean_s", "b_spei12_avg_area_km_s", "b_spei12_annual_frequency_s", "b_spei12_autocorrelation_s", "b_spei12_severity_s"),
           off_diag_args = list(size = 1.5))

ests_mod12 <- data.frame(exp(cbind(Odds_Ratio = fixef(mod_12)[,1], Lower = fixef(mod_12, probs = c(.05, .95))[,3], Upper = fixef(mod_12, probs = c(.5, .95))[,4])))

ests_mod12$parameters <- c("Intercept", "Severity", "Frequency", "Autocorrelation", "Spatial extent", "Wealth", "Household size", "Migrant number", "Dist. to pop. center", "Mean NDVI")


ests_mod12 <- ests_mod12 %>%
  mutate(parameters = factor(parameters, levels = c("Mean NDVI", "Dist. to pop. center", "Migrant number", "Household size", "Wealth", "Spatial extent", "Autocorrelation", "Frequency", "Severity", "Intercept")))

ggplot(ests_mod12, aes(x = parameters, y = Odds_Ratio)) +
  geom_pointrange(aes(ymin = Lower, ymax = Upper), color = "blue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, face = "bold"),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 15, face = "bold"),
  ) +
  labs(y = "Odds Ratio with 90% Credible Interval")



##### Severity in vegetation cover #####
# Correlation with other model variables is no higher than 0.19, so add to full model with no removals

svi_sev <- brm(remit ~ 
              SVI_severity_s +
              autocorrelation_s + annual_frequency_s + severity_s + avg_area_km_s + # Environmental predictors of interest
              wealth_index_s + hh_size_s + migrant_num_s + pop_center_s + NDVI_mean_s + # Controls
              (1 | date_s + census_tract + country), 
            data = dat,
            family = bernoulli,
            control = list(adapt_delta = 0.99),
            prior = c(prior(cauchy(0, 2), class = "sd"),
            prior(normal(0, 1), class = "b")
            )
)

bayes_R2(svi_sev)

posterior_svi <- as.array(svi_sev)

color_scheme_set("pink")
mcmc_pairs(posterior_svi, pars = c("b_NDVI_mean_s", "b_SVI_severity_s", "b_avg_area_km_s", "b_annual_frequency_s", "b_autocorrelation_s", "b_severity_s", "b_SVI_severity_s"),
           off_diag_args = list(size = 1.5))

ests_svi <- data.frame(exp(cbind(Odds_Ratio = fixef(svi_sev)[,1], Lower = fixef(svi_sev, probs = c(.05, .95))[,3], Upper = fixef(svi_sev, probs = c(.5, .95))[,4])))

ests_svi$parameters <- c("Intercept", "SVI severity", "Autocorrelation", "Frequency", "Severity", "Spatial extent", "Wealth", "Household size", "Migrant number", "Dist. to pop. center", "Mean NDVI")


ests_svi <- ests_svi %>%
  mutate(parameters = factor(parameters, levels = c("Mean NDVI", "Dist. to pop. center", "Migrant number", "Household size", "Wealth", "Spatial extent", "Autocorrelation", "Frequency", "Severity", "SVI severity", "Intercept")))

ggplot(ests_svi, aes(x = parameters, y = Odds_Ratio)) +
  geom_pointrange(aes(ymin = Lower, ymax = Upper), color = "blue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, face = "bold"),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 15, face = "bold"),
  ) +
  labs(y = "Odds Ratio with 90% Credible Interval")


##### Three-way interaction plot #####

mod_ixn <- brm(remit ~ 
              annual_frequency_s * severity_s * avg_area_km_s + autocorrelation_s + # Environmental predictors of interest
              wealth_index_s + hh_size_s + migrant_num_s + pop_center_s + NDVI_mean_s + # Controls
              (1 | date_s + census_tract + country), 
            data = dat,
            family = bernoulli,
            control = list(adapt_delta = 0.99),
            prior = c(prior(cauchy(0, 2), class = "sd"),
                      prior(normal(0, 1), class = "b")
            )
)


# Convert to odds ratios

ests_mod_ixn <- data.frame(exp(cbind(Odds_Ratio = fixef(mod_ixn)[,1], Lower = fixef(mod_ixn, probs = c(.05, .95))[,3], Upper = fixef(mod_ixn, probs = c(.5, .95))[,4])))

ests_mod_ixn$parameters <- c("Intercept", "Frequency", "Severity", "Spatial extent", "Autocorrelation", "Wealth", "Household size", "Migrant number", "Dist. to pop. center", "Mean NDVI", "Frequency * Severity", "Frequency * Spatial extent", "Severity * Spatial extent", "Frequency * Severity * Spatial extent")

### Posterior checks ###

# For variables moderately correlated, check for signs of ridges

posterior <- as.array(mod_ixn)

color_scheme_set("pink")
mcmc_pairs(posterior, pars = c("b_annual_frequency_s:severity_s:avg_area_km_s", "b_severity_s:avg_area_km_s", "b_annual_frequency_s:severity_s", "b_annual_frequency_s:avg_area_km_s"), # main effects look fine from first pairs plot; focus here on interaction terms
           off_diag_args = list(size = 1.5))
# The only clear positive trends are between interaction terms, so not surprising.

bayes_R2(mod_ixn)

ests_mod_ixn <- ests_mod_ixn %>%
  mutate(parameters = factor(parameters, levels = c("Mean NDVI", "Dist. to pop. center", "Migrant number", "Household size", "Wealth", "Frequency * Severity * Spatial extent", "Severity * Spatial extent", "Frequency * Spatial extent", "Frequency * Severity", "Spatial extent", "Autocorrelation", "Frequency", "Severity", "Intercept")))

ggplot(ests_mod_ixn, aes(x = parameters, y = Odds_Ratio)) +
  geom_pointrange(aes(ymin = Lower, ymax = Upper), color = "blue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, face = "bold"),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 15, face = "bold"),
  ) +
  labs(y = "Odds Ratio with 90% Credible Interval")


### "Interaction plot" (really 3D plot) ###

grid <- expand.grid(
  annual_frequency_s = seq(min(dat$annual_frequency_s), max(dat$annual_frequency_s), length.out = 20),
  severity_s = seq(min(dat$severity_s), max(dat$severity_s), length.out = 20),
  avg_area_km_s = seq(min(dat$avg_area_km_s), max(dat$avg_area_km_s), length.out = 20),
  autocorrelation_s = mean(dat$autocorrelation_s, na.rm = TRUE),
  wealth_index_s = mean(dat$wealth_index_s, na.rm = TRUE),
  hh_size_s = mean(dat$hh_size_s, na.rm = TRUE),
  migrant_num_s = mean(dat$migrant_num_s, na.rm = TRUE),
  pop_center_s = mean(dat$pop_center_s, na.rm = TRUE),
  NDVI_mean_s = mean(dat$NDVI_mean_s, na.rm = TRUE),
  country = "nigeria", # Most common country
  census_tract = "minjibir", # Most common census tract in that country
  date_s = "-0.877021468188721" # Only "date" in Nigeria
  # CAN DO COUNTRY BY COUNTRY CHECK HERE TOO!
  # All variables not of interest held at mean or modal category.
)


grid$remit <- predict(mod_ixn, newdata = grid, type = "response", allow_new_levels = TRUE)[, 1]

ggplot(grid, aes(x = annual_frequency_s, y = severity_s, fill = remit)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(x = "Drought Frequency", y = "Drought Severity", 
       fill = "Predicted Probability", 
       title = "Heatmap of Predicted Remittance Receipt") +
  theme_minimal()

fig <- plot_ly(grid, 
               x = ~avg_area_km_s, 
               y = ~severity_s, 
               z = ~remit, 
               color = ~annual_frequency_s,  # Use color for the third variable
               type = "scatter3d", mode = "markers") %>%
  layout(
  title = NULL,
  scene = list(
    xaxis = list(title = "Spatial Extent", titlefont = list(size = 18), tickfont = list(size = 14)),
    yaxis = list(title = "Severity", titlefont = list(size = 18), tickfont = list(size = 14)),
    zaxis = list(title = "Predicted Probability of Remittance", titlefont = list(size = 18), tickfont = list(size = 14)),
    coloraxis = list(title = "Frequency", titlefont = list(size = 18), tickfont = list(size = 14))
  )
 ) %>%
  colorbar(title = "Frequency", titlefont = list(size = 18), tickfont = list(size = 14))

# Save interactive plot as HDML

saveWidget(fig, "3D_Remittance_Plot.html", selfcontained = TRUE)




##### Three-way interaction with no extreme values #####

mod_ixn_noext <- brm(remit ~ 
                 annual_frequency_s * severity_s * avg_area_km_s + autocorrelation_s + # Environmental predictors of interest
                 wealth_index_s + hh_size_s + migrant_num_s + pop_center_s + NDVI_mean_s + # Controls
                 (1 | date_s + census_tract + country), 
               data = dat_noext,
               family = bernoulli,
               control = list(adapt_delta = 0.99),
               prior = c(prior(cauchy(0, 2), class = "sd"),
                         prior(normal(0, 1), class = "b")
               )
)

# Convert to odds ratios

ests_mod_ixn_noext <- data.frame(exp(cbind(Odds_Ratio = fixef(mod_ixn_noext)[,1], Lower = fixef(mod_ixn_noext, probs = c(.05, .95))[,3], Upper = fixef(mod_ixn_noext, probs = c(.5, .95))[,4])))

ests_mod_ixn_noext$parameters <- c("Intercept", "Frequency", "Severity", "Spatial extent", "Autocorrelation", "Wealth", "Household size", "Migrant number", "Dist. to pop. center", "Mean NDVI", "Frequency * Severity", "Frequency * Spatial extent", "Severity * Spatial extent", "Frequency * Severity * Spatial extent")

### Posterior checks ###

# For variables moderately correlated, check for signs of ridges

posterior <- as.array(mod_ixn_noext)

color_scheme_set("pink")
mcmc_pairs(posterior, pars = c("b_annual_frequency_s:severity_s:avg_area_km_s", "b_severity_s:avg_area_km_s", "b_annual_frequency_s:severity_s", "b_annual_frequency_s:avg_area_km_s"), # main effects look fine from first pairs plot; focus here on interaction terms
           off_diag_args = list(size = 1.5))
# The only clear positive trends are between interaction terms, so not surprising.

bayes_R2(mod_ixn_noext)

ests_mod_ixn_noext <- ests_mod_ixn_noext %>%
  mutate(parameters = factor(parameters, levels = c("Mean NDVI", "Dist. to pop. center", "Migrant number", "Household size", "Wealth", "Frequency * Severity * Spatial extent", "Severity * Spatial extent", "Frequency * Spatial extent", "Frequency * Severity", "Spatial extent", "Autocorrelation", "Frequency", "Severity", "Intercept")))

ggplot(ests_mod_ixn_noext, aes(x = parameters, y = Odds_Ratio)) +
  geom_pointrange(aes(ymin = Lower, ymax = Upper), color = "blue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, face = "bold"),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 15, face = "bold"),
  ) +
  labs(y = "Odds Ratio with 90% Credible Interval")

grid_noext <- expand.grid(
  annual_frequency_s = seq(min(dat_noext$annual_frequency_s), max(dat_noext$annual_frequency_s), length.out = 20),
  severity_s = seq(min(dat_noext$severity_s), max(dat_noext$severity_s), length.out = 20),
  avg_area_km_s = seq(min(dat_noext$avg_area_km_s), max(dat_noext$avg_area_km_s), length.out = 20),
  autocorrelation_s = mean(dat_noext$autocorrelation_s, na.rm = TRUE),
  wealth_index_s = mean(dat_noext$wealth_index_s, na.rm = TRUE),
  hh_size_s = mean(dat_noext$hh_size_s, na.rm = TRUE),
  migrant_num_s = mean(dat_noext$migrant_num_s, na.rm = TRUE),
  pop_center_s = mean(dat_noext$pop_center_s, na.rm = TRUE),
  NDVI_mean_s = mean(dat_noext$NDVI_mean_s, na.rm = TRUE),
  country = "nigeria", # Most common country
  census_tract = "minjibir", # Most common census tract in that country
  date_s = "-0.877021468188721" # Only "date" in Nigeria
  # CAN DO COUNTRY BY COUNTRY CHECK HERE TOO!
  # All variables not of interest held at mean or modal category.
)


grid_noext$remit <- predict(mod_ixn_noext, newdata = grid_noext, type = "response", allow_new_levels = TRUE)[, 1]

fig_noext <- plot_ly(grid_noext, 
               x = ~avg_area_km_s, 
               y = ~severity_s, 
               z = ~remit, 
               color = ~annual_frequency_s,  # Use color for the third variable
               type = "scatter3d", mode = "markers") %>%
  layout(
    title = NULL,
    scene = list(
      xaxis = list(title = "Spatial Extent", titlefont = list(size = 18), tickfont = list(size = 14)),
      yaxis = list(title = "Severity", titlefont = list(size = 18), tickfont = list(size = 14)),
      zaxis = list(title = "Predicted Probability of Remittance", titlefont = list(size = 18), tickfont = list(size = 14)),
      coloraxis = list(title = "Frequency", titlefont = list(size = 18), tickfont = list(size = 14))
    )
  ) %>%
  colorbar(title = "Frequency", titlefont = list(size = 18), tickfont = list(size = 14))


saveWidget(fig_noext, "3D_Remittance_Plot_No_Extremes.html", selfcontained = TRUE)


##### Senegal subs processing #####

senegal_subs <- read_csv("data/senegal_SPEI-3_2005-2009_10percentile_1981-2009_subs.csv")

senegal_subs <- senegal_subs %>%
  select(house, country, NDVI_mean)

senegal <- senegal %>%
  mutate(NDVI_mean = NULL)

senegal_1 <- left_join(senegal, senegal_subs, by = c("house", "country"))

write_csv(senegal_1, "senegal_SPEI-3_2005-2009_10percentile_1981-2009.csv")