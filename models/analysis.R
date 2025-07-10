# Download data files first from: https://github.com/annethro/remittances/

lapply(c("tidyverse", "geosphere", "ggcorrplot", "rnaturalearth", "sf", "ggpubr", "brms", "bayesplot", "officer", "flextable", "cmdstanr", "plotly", "webshot", "htmlwidgets", "tidybayes", "Hmisc", "cowplot"), library, character.only = TRUE)

#webshot::install_phantomjs(force = TRUE) # Only run once, if you want to export the interactive interaction plot in html. Force = TRUE because was available for R 4.4.0 but not 4.4.3

# If you don't have cmdstanr installed you can get it this way:
#install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))
#cmdstanr::install_cmdstan() #This actually does the install -- takes a while!


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

set.seed(1742049459) # Seed selected from date/time on 3-15-25

dat <- read_csv("data/spei3_yr5_perc.csv")

# We include a data dictionary on GitHub, but some notes here: lat and long are where the participant's census tract is located, based on coding done by JHJ and AP; lat_SPEI and long_SPEI refer to the nearest point we have for precipitation for SPEI, so one can examine the distance as the crow flies between participant's census tract and the nearest precipitation point; pop_cent_lat and pop_cent_long refer to the nearest population center according to nighttime lights data; we use this to calculate distance as the crow flies below.

# Some notes about data: (1) Though there is some lack of clarity in surveys and in metadata/recorded protocol, it appears that "household size" refers to people *currently* in the home, not migrants -- which is why migrant number can be bigger than household size. (2) NA for remittances is a zero -- these are households that did not report receiving money or goods from migrants but answered other questions, so there is not reason to think questions about remittances were skipped. (3) Reminder that we're using avg_area and distance in meters...!

##### Data processing #####

### Interview date

# Date is unavailable for some countries but important because of seasonality, global politics, etc

dat$interview_date <- as.Date(dat$date, format = "%m/%d/%Y")

# Nigeria: data were collected between took place between Oct 5 and November 6, 2009

dat$interview_date[dat$country == "nigeria"] <- as.Date("2009-10-21") # Halfway between Oct 5 and Nov 6

# Senegal: All data colection took place in Oct and Nov 2009

dat$interview_date[dat$country == "senegal"] <- as.Date("2009-11-01") # Halfway

# South Africa: All data collection took place between Nov 13 and Dec 23 2009

dat$interview_date[dat$country == "south_africa"] <- as.Date("2009-12-07") # Halfway


### Six households missing an interview date for Burkina (to be fixed with imputation during manuscript preparation)

# All interviews in this tract were Oct 30-31, so assigning each house one of the two based on this random draw.
#sample(1:2, 5, replace = T) # 2 2 2 1 2 (for 5 houses but two have multiple rows for migrants so it's 2 2 2 2 2 1 2)

dat$interview_date[which(is.na(dat$interview_date) & dat$census_tract == "kawara")] <- c("2009-10-31","2009-10-31","2009-10-31","2009-10-31","2009-10-31","2009-10-30","2009-10-31")

# All interviews in this tract 11/11 or 11/12, so assigning each house one of the two based on this random draw:
#sample(1:2, 1, replace = T) # 2

dat$interview_date[which(is.na(dat$date) & dat$census_tract == "lantaga")] <- c("2009-11-12")

# Likely error
dat$interview_date[!is.na(dat$interview_date) & dat$interview_date == "2010-08-18"] <- "2010-03-18"

# All NAs for autocorrelation are zeroes -- ACF returns NAs when there's no 1s for a binary time series

dat$autocorrelation[is.na(dat$autocorrelation)] <- 0

# Remove rows that didn't have location data

dat <- dat[!is.na(dat$lat), ]

# NAs are meaningful for remit: nonhousehold migrants are generally only reported if they're sending remittances, given the nature of this survey, and we ensure that rows for household migrants are valid by ensuring they had remittance, current location, and/or time in location data; all remaining NAs are meaningful, so converting to zero for analysis [CHECK - if keep this, what am I going to do for ALL remittance measures? cross-check...!]
dat <- dat %>% 
  mutate(remit = replace_na(remit, 0),
         any_remit = replace_na(any_remit, 0))


##### Calculate distance as the crow flies to the nearest population center #####

dat <- dat %>%
  mutate(pop_center = distHaversine(as.matrix(dat[ , c("long", "lat")]), as.matrix(dat[ , c("pop_cent_long", "pop_cent_lat")]))/1000) # Convert to km

# Converting avg_area to km, it's just too big to display

dat$avg_area_km <- dat$avg_area/1000

### Important: flipping the sign on severity to improve interpretability: higher values means more severity

dat$severity_pos <- -dat$severity

# Observations: some of the big clumps of data with no variability are South Africa, where we didn't have anything finer-grained than municipality (so LOTS of people with the same lat-long).


### Create a categorical variable for whom receiving remittances from

# Originally I thought I'd run a model with two sets of predictors, one for household senders and one for non-households senders
# However, non-household senders are only listed if they remitted
# I could add a row to every household with no non-hh remitter with a zero, but more straightforward:
# Prepare to run a categorical model: this achieves basically the same thing, as there's a different model for:
# households with hh senders, with non-hh senders, with both, and with neither

dat <- dat %>%
  group_by(country, house) %>%
  mutate(source = case_when(
    any(migrant_hh == "hh" & remit == 1) & any(migrant_hh == "non-hh" & remit == 1) ~ "hh_non-hh",
    any(migrant_hh == "hh" & remit == 1) ~ "hh",
    any(migrant_hh == "non-hh" & remit == 1) ~ "non-hh",
    .default = "none"
  ))

### Collapse to hh in-country, hh out-country, non-hh in-country, non-hh out-country to reduce chaos

dat$hh_by_loc <- case_when(
  dat$migrant_hh == "hh" & dat$migrant_loc == "national" ~ "hh_natl",
  dat$migrant_hh == "hh" & dat$migrant_loc == "international" ~ "hh_intl",
  dat$migrant_hh == "non-hh" & dat$migrant_loc == "national" ~ "non-hh_natl",
  dat$migrant_hh == "non-hh" & dat$migrant_loc == "international" ~ "non-hh_intl"
) # Leaving NAs in because it's really "not applicable" if there's no remitter and this will lead to automatic exclusion from models looking at remitters (which we want)


### Recode time in location

dat$hh_time <- case_when(
  dat$hh_months > (12*5) ~ "early",
  dat$hh_months <= (12*5) & dat$hh_months > 12 ~ "interval",
  dat$hh_months <= 12 ~ "late"
)

##### Center and standardize variables #####

# Ones with NAs don't play well with scale inside of mutate so old school here:

dat$date_s <- as.numeric(scale(dat$interview_date))
dat$hh_size_s <- as.numeric(scale(dat$hh_size))
dat$migrant_num_s <- as.numeric(scale(dat$migrant_num))
dat$hh_months_s <- as.numeric(scale(dat$hh_months))
dat$autocorrelation_s <- as.numeric(scale(dat$autocorrelation))
dat$NDVI_mean_s <- as.numeric(scale(dat$NDVI_mean))
dat$SVI_severity_s <- as.numeric(scale(dat$SVI_severity))
dat$interview_date_s <- as.numeric(scale(dat$interview_date))
dat$pop_center_s <- as.numeric(scale(dat$pop_center))
dat$avg_area_km_s <- as.numeric(scale(dat$avg_area_km))
dat$severity_s <- as.numeric(scale(dat$severity_pos))
dat$hh_months_s <- as.numeric(scale(dat$hh_months))
dat$wealth_index_s <- as.numeric(scale(dat$wealth_index))
dat$annual_frequency_s <- as.numeric(scale(dat$annual_frequency))

# Subset to unique (one row per house) to avoid inflating statistics with houses that have many migrants

dat_onerow <- distinct(dat, house, country, .keep_all = TRUE)


##### Descriptive stats #####

# Correlation structure between environmental variables of interest. By definition, these should be correlated a bit - and we don't see problematically high levels of correlation. Per best practices from Bayesian approaches (see discussions referenced in the URLs that follow), we just set the prior for each environmental variable to a normal distribution with constant variance to move the model away from a ridge in the posterior between two or more environmental variables.
# https://statmodeling.stat.columbia.edu/2019/07/07/collinearity-in-bayesian-models/; https://mc-stan.org/docs/stan-users-guide/problematic-posteriors.html

cor.mat <- cor(dat_onerow[, c("NDVI_mean_s", "pop_center_s","migrant_num_s", "hh_size_s", "wealth_index_s", "avg_area_km_s", "autocorrelation_s", "annual_frequency_s", "severity_s")], use = "complete.obs")
ggcorrplot(cor.mat)

# Highest correlation (from me checking various things) is severity with area affected but coming in at 58%, so feeling okay running them together in the same model -- but including normal priors to attempt to avoid ridges in posterior estimates

### Average number of migrants per household

senders_noremit <- dat %>%
  filter(any_remit == 0) %>%
  select(country, house) %>%
  distinct(country, house) %>%
  mutate(senders = 0)

senders_remit <- dat %>%
  filter(any_remit == 1)

senders_remit <- count(senders_remit, country, house) %>%
  mutate(senders = n) %>%
  select(-n)

senders <- bind_rows(senders_noremit, senders_remit)

mean(senders$senders) # Average of 0.7005774 remittance senders per household

### Overall descriptives

contin <- dat_onerow [ , c("country", "any_remit", "autocorrelation", "annual_frequency", "severity_pos", "avg_area_km", "NDVI_mean", "wealth_index_s", "pop_center", "hh_size", "migrant_num")]

variable_names <- c( # Lookup table for renaming
  "country" = "Country", 
  "any_remit" = "Any remittance (pres/abs)", 
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

### Just for high levels of spatial extent

high_extent <- dat_onerow [ , c("any_remit", "autocorrelation", "annual_frequency", "severity_pos", "avg_area_km", "NDVI_mean", "wealth_index_s", "pop_center", "hh_size", "migrant_num")]

high_extent <- high_extent[high_extent$avg_area_km > 1000000,]

variable_names <- c( # Lookup table for renaming
  "country" = "Country", 
  "any_remit" = "Any remittance (pres/abs)", 
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
summary_table_high_extent <- compute_stats_whole_sample(high_extent)

# Create a flextable for Word formatting
ft_high_extent <- flextable(summary_table_high_extent) %>%
  theme_vanilla() %>%
  bold(part = "header") %>%
  align(j = 2:5, align = "center", part = "all") %>%
  colformat_num(j = 2:5, digits = 2) %>%
  autofit()

# Create a Word document and add the table
doc <- read_docx() %>%
  body_add_flextable(ft_high_extent)

# Save the Word document
print(doc, target = "High extent_Total.docx")

### By country

contin_c <- dat_onerow [ , c("country", "any_remit", "autocorrelation", "annual_frequency", "severity_pos", "avg_area_km", "NDVI_mean", "wealth_index", "pop_center", "hh_size", "migrant_num")]

variable_names_c <- c( # Lookup table for renaming
  "country" = "Country", 
  "any_remit" = "Any remittance (pres/abs)", 
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


### By migrants -- these are inflated by repeat measurement so only showing migrant-specific data...!
contin_mig <- dat[dat$country != "south_africa" , c("country", "remit", "hh_months")]

cat_mig <- dat [ , c("country", "migrant_hh", "migrant_loc")]

variable_names_m <- c( # Lookup table for renaming
  "country" = "Country", 
  "remit" = "Remittance from specific migrant (pres/abs)",
  "migrant_hh" = "Migrant from household or not",
  "migrant_loc" = "Migrant current location",
  "hh_months" = "Migrant months in current location")

variable_names_cont <- c( # Lookup table for renaming
  "country" = "Country", 
  "remit" = "Remittance from specific migrant (pres/abs)",
  "hh_months" = "Migrant months in current location")

country_names <- c(
  "burkina_faso" = "Burkina Faso",
  "kenya" = "Kenya",
  "nigeria" = "Nigeria",
  "senegal" = "Senegal",
  "south_africa" = "South Africa",
  "uganda" = "Uganda"
)

compute_contin_stats <- function(data) {
  data %>%
    pivot_longer(-country, names_to = "Variable", values_to = "Value") %>%
    mutate(
      Value = as.numeric(Value),  # Ensure numeric
      Variable = recode(Variable, !!!variable_names_m, .default = Variable),
      country  = recode(country, !!!country_names, .default = country)
    ) %>%
    group_by(Variable, country) %>%
    summarise(
      Mean = round(mean(Value, na.rm = TRUE), 2),
      SD   = round(sd(Value, na.rm = TRUE), 2),
      Min  = round(min(Value, na.rm = TRUE), 2),
      Max  = round(max(Value, na.rm = TRUE), 2),
      .groups = "drop"
    )
}

ds_country_contin <- compute_contin_stats(contin_mig)

compute_cat_stats <- function(data) {
  data %>%
    pivot_longer(-country, names_to = "Variable", values_to = "Category") %>%
    mutate(
      Variable = recode(Variable, !!!variable_names_m),
      country = recode(country, !!!country_names)
    ) %>%
    group_by(Variable, country, Category) %>%
    summarise(N = n(), .groups = "drop_last") %>%
    mutate(
      Total = sum(N),
      Percent = round(100 * N / Total, 1)
    ) %>%
    ungroup() %>%
    select(Variable, country, Category, N, Percent)
}

ds_country_cat <- data.frame(compute_cat_stats(cat_mig))


# Create a flextable for Word formatting
ft_country_contin <- flextable(ds_country_contin) %>%
  theme_vanilla() %>%  # Apply a clean style
  bold(part = "header") %>%
  align(j = 3:6, align = "center", part = "all") %>%  # Center numeric columns
  colformat_num(j = 3:6, digits = 2) %>% # Two decimal places
  autofit()  # Adjust column sizes automatically

# Create a Word document and add the table
doc <- read_docx() %>%
  body_add_flextable(ft_country_contin)

# Save the Word document
print(doc, target = "Descriptives_Migrant_Continuous.docx")


# Create a flextable for Word formatting
ft_country_cat <- flextable(ds_country_cat) %>%
  theme_vanilla() %>%  # Apply a clean style
  bold(part = "header") %>%
  align(j = 4:5, align = "center", part = "all") %>%  # Center numeric columns
  colformat_num(j = 4, digits = 0) %>% # No decimals
  colformat_num(j = 5, digits = 2) %>% # Two decimal places
  autofit()  # Adjust column sizes automatically

# Create a Word document and add the table
doc <- read_docx() %>%
  body_add_flextable(ft_country_cat)

# Save the Word document
print(doc, target = "Descriptives_Migrant_Categorical.docx")


###### Remittances by country #####
remit_by_country <- as.data.frame.matrix(table(dat_onerow$country, dat_onerow$any_remit)) # Yes an ANOVA suggests they're significantly different, but distracting from main message of which country has most households receiving

remit_by_country$perc <- remit_by_country$`1`/(remit_by_country$`0` + remit_by_country$`1`)


##### Data coverage 3d scatterplot #####

# As we'll see later, there's some action for households experienced high severity, high annual_frequency, and large spatial extent droughts -- let's make sure there's good data coverage in that part of the state space and it's not being driven by few observations (spoiler: looks like it is)

descrip_3d <- plot_ly(dat_onerow, 
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

### Severity

sev <- dat_onerow[ , c("long", "lat", "severity_pos")]
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


### annual_frequency

freq <- dat_onerow[ , c("long", "lat", "annual_frequency")]
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

auto <- dat_onerow[ , c("long", "lat", "autocorrelation")]
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


### Spatial extent

spa <- dat_onerow[dat_onerow$avg_area_km < 700000, c("long", "lat", "avg_area_km")]
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


ggarrange(sev_plot, freq_plot, auto_plot, spa_plot,
          ncol = 2, nrow = 2)

############# MAIN MODEL ################

mod1 <- brm(any_remit ~ 
              avg_area_km_s + severity_s + annual_frequency_s + autocorrelation_s + # Environmental predictors of interest
              wealth_index_s + hh_size_s + migrant_num_s + pop_center_s + NDVI_mean_s + # Controls
              (1 | date_s + census_tract + country), 
          data = dat_onerow,
          family = bernoulli,
          control = list(adapt_delta = 0.99),
          prior = c(prior(cauchy(0, 2), class = "sd"),
                    prior(normal(0, 1), class = "b")
          ),
          backend = "cmdstanr", threads = threading(2, static = TRUE), cores = 4,
          chains = 4 # Reminder that I have a seed set for the entire session (see top)
)


# Convert to odds ratios

ests_mod1 <- data.frame(exp(cbind(Odds_Ratio = fixef(mod1)[,1], Lower = fixef(mod1, probs = c(.05, .95))[,3], Upper = fixef(mod1, probs = c(.5, .95))[,4])))

ests_mod1$parameters <- c("Intercept", "Spatial extent", "Severity", "Frequency", "Autocorrelation", "Wealth", "Household size", "Migrant number", "Dist. to pop. center", "Mean NDVI")

### Posterior checks ###

# For variables moderately correlated, check for signs of ridges

posterior <- as.array(mod1)

color_scheme_set("pink")
mcmc_pairs(posterior, pars = c("b_NDVI_mean_s", "b_avg_area_km_s", "b_annual_frequency_s", "b_autocorrelation_s", "b_severity_s"),
           off_diag_args = list(size = 1.5))

bayes_R2(mod1)

### Caterpillar plot ###

ests_mod1 <- ests_mod1 %>%
  mutate(parameters = factor(parameters, levels = c("Mean NDVI", "Dist. to pop. center", "Migrant number", "Household size", "Wealth", "Spatial extent", "Autocorrelation", "Frequency", "Severity", "Intercept"))) %>%
  filter(!(parameters %in% c("Migrant number")))

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

# So you don't have to re-run this thing!
save.image("patterning_remittances.RData")

load("patterning_remittances.RData")

############# EXPLORATORY ANALYSES ################

# Subset to household senders only: months migrant has been in current location (should be more than 12) and receipt in last 12 months

##### Check main model fit robustness to exclusion of extreme values #####

# High values of for spatial extent...

dat_onerow_noext <- dat_onerow[dat_onerow$avg_area_km_s <= 3,] # 371 observations. Only have one annual_frequency (medium low; -1.08251295545341), one autocorrelation (0.2), though a range of severities. 2/3 remit, all are from Nigeria

# High severity...

dat_onerow_noext <- dat_onerow_noext[dat_onerow_noext$severity_s <= 3,] # 9 observations. All identical on spatial extent, annual_frequency, severity, and autocorrelation; 2 remit; all uganda. 

mod1_noext <- brm(any_remit ~ 
              severity_s + annual_frequency_s + autocorrelation_s + avg_area_km_s + # Environmental predictors of interest
              wealth_index_s + hh_size_s + migrant_num_s + pop_center_s + NDVI_mean_s + # Controls
              (1 | date_s + census_tract + country), 
            data = dat_onerow_noext,
            family = bernoulli,
            control = list(adapt_delta = 0.99),
            prior = c(prior(cauchy(0, 2), class = "sd"),
                      prior(normal(0, 1), class = "b")
            ),
            backend = "cmdstanr", threads = threading(2, static = TRUE), cores = 4,
            chains = 4
)

save.image("patterning_remittances.RData")

ests_mod1_noext <- data.frame(exp(cbind(Odds_Ratio = fixef(mod1_noext)[,1], Lower = fixef(mod1_noext, probs = c(.05, .95))[,3], Upper = fixef(mod1_noext, probs = c(.5, .95))[,4])))

bayes_R2(mod1_noext) #0.334

posterior <- as.array(mod1_noext)

color_scheme_set("pink")
mcmc_pairs(posterior, pars = c("b_NDVI_mean_s", "b_avg_area_km_s", "b_annual_frequency_s", "b_autocorrelation_s", "b_severity_s"),
           off_diag_args = list(size = 1.5))

### Caterpillar plot ###

ests_mod1_noext$parameters <- c("Intercept", "Severity", "Frequency", "Autocorrelation", "Spatial extent", "Wealth", "Household size", "Migrant number", "Dist. to pop. center", "Mean NDVI")

ests_mod1_noext <- ests_mod1_noext %>%
  mutate(parameters = factor(parameters, levels = c("Mean NDVI", "Dist. to pop. center", "Migrant number", "Household size", "Wealth", "Spatial extent", "Autocorrelation", "Frequency", "Severity", "Intercept"))) %>%
  filter(!(parameters %in% c("Migrant number"))) # So large can't see the other credible intervals well!

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


##### 2.3.1.1 Send a migrant? #####

# Change contrast category

dat_onerow$source <- relevel(as.factor(dat_onerow$source), "none")

dat_onerow$source[dat_onerow$source == "hh_non-hh"] <- "hh"
dat_onerow$source <- droplevels(dat_onerow$source)

# Model

mod_migrate <- brm(source ~ 
              severity_s + annual_frequency_s + autocorrelation_s + avg_area_km_s + # Environmental predictors of interest
              wealth_index_s + hh_size_s + migrant_num_s + pop_center_s + NDVI_mean_s + # Controls
              (1 | date_s + census_tract + country + house), 
            data = dat_onerow,
            family = categorical(),
            prior = c(
              prior(cauchy(0, 1), class = "sd", dpar = "munonhh"),
              prior(cauchy(0, 1), class = "sd", dpar = "muhh"),
              prior(normal(0, 1), class = "b", dpar = "munonhh"),
              prior(normal(0, 1), class = "b", dpar = "muhh")
            ),
            control = list(adapt_delta = 0.99),
            backend = "cmdstanr", threads = threading(2, static = TRUE), cores = 4, 
            chains = 4
)

save.image("patterning_remittances.RData")

# Convert to odds ratios

ests_mod_migrate <- data.frame(exp(cbind(Odds_Ratio = fixef(mod_migrate)[,1], Lower = fixef(mod_migrate, probs = c(.05, .95))[,3], Upper = fixef(mod_migrate, probs = c(.5, .95))[,4])))

ests_mod_migrate$parameters <- c("HH-Intercept", "NonHH-Intercept", "HH-Severity", "HH-Frequency", "HH-Autocorrelation", "HH-Spatial extent", "HH-Wealth", "HH-Household size", "HH-Migrant number", "HH-Dist. to pop. center", "HH-Mean NDVI", "NonHH-Severity", "NonHH-Frequency", "NonHH-Autocorrelation", "NonHH-Spatial extent", "NonHH-Wealth", "NonHH-Household size", "NonHH-Migrant number", "NonHH-Dist. to pop. center", "NonHH-Mean NDVI")

### Posterior checks ###

# For variables moderately correlated, check for signs of ridges

posterior <- as.array(mod_migrate)

color_scheme_set("pink")
mcmc_pairs(posterior, pars = c("b_muhh_NDVI_mean_s", "b_muhh_avg_area_km_s", "b_muhh_annual_frequency_s", "b_muhh_autocorrelation_s", "b_muhh_severity_s"),
           off_diag_args = list(size = 1.5))

mcmc_pairs(posterior, pars = c("b_munonhh_NDVI_mean_s", "b_munonhh_avg_area_km_s", "b_munonhh_annual_frequency_s", "b_munonhh_autocorrelation_s", "b_munonhh_severity_s"),
           off_diag_args = list(size = 1.5))

# Can't do Bayes R2 for categorical models

### Caterpillar plot ###

ests_mod_migrate <- ests_mod_migrate %>%
  mutate(parameters = factor(parameters, levels = rev(ests_mod_migrate$parameters))) %>%
  filter(!(parameters %in% c("HH-Intercept", "HH-Migrant number"))) # HH-migrant-num estimate is off the charts (as we already knew from other models) and there's a lot of uncertainty around the household estimate; remove so can see others

ggplot(ests_mod_migrate, aes(x = parameters, y = Odds_Ratio)) +
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



##### 2.3.1.2 Decorrelating risk and expense of moving #####

# Interaction of hh/non-hh migrant and their location with spatial extent

dat$hh_by_loc <- relevel(as.factor(dat$hh_by_loc), "hh_natl") # Cheapest to get probably

# Model

mod_decorr <- brm(remit ~ 
              severity_s + annual_frequency_s + autocorrelation_s + avg_area_km_s * hh_by_loc + # Environmental predictors of interest
              wealth_index_s + hh_size_s + migrant_num_s + pop_center_s + NDVI_mean_s + # Controls
              (1 | date_s + house + census_tract + country), 
            data = dat,
            family = bernoulli,
            control = list(adapt_delta = 0.99),
            prior = c(prior(cauchy(0, 2), class = "sd"),
                      prior(normal(0, 1), class = "b"),
                      prior(normal(0, 2.5), class = "Intercept")
            ),
            backend = "cmdstanr", threads = threading(2, static = TRUE), cores = 4,
            chains = 4 # Reminder that I have a seed set for the entire session (see top)
)

save.image("patterning_remittances.RData")

# Convert to odds ratios

ests_mod_decorr <- data.frame(exp(cbind(Odds_Ratio = fixef(mod_decorr)[,1], Lower = fixef(mod_decorr, probs = c(.05, .95))[,3], Upper = fixef(mod_decorr, probs = c(.5, .95))[,4])))

ests_mod_decorr$parameters <- c("Intercept", "Severity", "Frequency", "Autocorrelation", "Spatial extent", "HH-International", "HH-National", "Non-HH-International", "Wealth", "Household size", "Migrant number", "Dist. to pop. center", "Mean NDVI", "HH-Interational:Spatial extent", "HH-National:Spatial extent", "Non-HH-International:Spatial extent")

### Posterior checks ###

# For variables moderately correlated, check for signs of ridges

posterior <- as.array(mod_decorr)

color_scheme_set("pink")
mcmc_pairs(posterior, pars = c("b_NDVI_mean_s", "b_avg_area_km_s", "b_annual_frequency_s", "b_autocorrelation_s", "b_severity_s"),
           off_diag_args = list(size = 1.5))

bayes_R2(mod_decorr) # 0.3392029

### Caterpillar plot ###

ests_mod_decorr <- ests_mod_decorr %>%
  mutate(parameters = factor(parameters, levels = c("Non-HH-International:Spatial extent", "HH-National:Spatial extent", "HH-Interational:Spatial extent", "Mean NDVI", "Dist. to pop. center", "Migrant number", "Household size", "Wealth", "Non-HH-International","HH-National","HH-International", "Spatial extent", "Autocorrelation", "Frequency", "Severity", "Intercept")))

ggplot(ests_mod_decorr, aes(x = parameters, y = Odds_Ratio)) +
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

## Investigating the really large OR for the intercept -- does this suggest unusual uncertainty? Support from ChatGPT on this one.

bayesplot::mcmc_hist(as_draws_df(mod_decorr), pars = "b_Intercept") # +/-2 to +/-4 range is pretty normal for a model estimate on this scale; not hugely clustered at +/- 2.5, so probably not straining against the prior (and no truncation). Peak at 3 means 95% probability at the intercept - this is true for non-household members, which is likely what's being picked up.

### Examine two-way interactions ###

means <- dat %>%
  summarise(across(c(severity_s, annual_frequency_s, autocorrelation_s, pop_center_s, wealth_index_s, hh_size_s, migrant_num_s, pop_center_s, NDVI_mean_s), ~ mean(.x, na.rm = TRUE)))

# Get range of avg_area_km_s
avg_area_km_s_seq <- seq(min(dat$avg_area_km_s, na.rm = TRUE), max(dat$avg_area_km_s, na.rm = TRUE), length.out = 100)

# Create new data grid
grid_base <- expand_grid(
  hh_by_loc = unique(na.omit(factor(dat$hh_by_loc))),   # all levels of source
  avg_area_km_s = avg_area_km_s_seq
)

new_data <- grid_base %>%
  bind_cols(means[rep(1, nrow(grid_base)), ])

preds <- new_data %>%
  add_epred_draws(mod_decorr, re_formula = NA)  # remove REs for marginal/fixed predictions

# Plot

ggplot(preds, aes(x = avg_area_km_s, y = .epred, color = hh_by_loc, fill = hh_by_loc)) +
  stat_summary(fun = mean, geom = "line", linewidth = 2) +
  stat_summary(fun.data = mean_cl_boot, geom = "ribbon", alpha = 0.2, color = NA) +
  labs(
    x = "Spatial extent",
    y = "Predicted probability of remittance",
    color = "HH-location level",
    fill = "HH-location level"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, face = "bold"),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 15, face = "bold"),
  ) +
  theme(legend.position = "right")

##### 2.3.1.3 When did migration occur? #####

mod_when <- brm(remit ~ 
                    severity_s*hh_time + annual_frequency_s*hh_time  + autocorrelation_s*hh_time  + avg_area_km_s*hh_time  + # Environmental predictors of interest
                    wealth_index_s + hh_size_s + migrant_num_s + pop_center_s + NDVI_mean_s + # Controls
                    (1 | date_s + house + census_tract + country), 
                  data = dat,
                  family = bernoulli,
                  control = list(adapt_delta = 0.99),
                  prior = c(prior(cauchy(0, 2), class = "sd"),
                            prior(normal(0, 1), class = "b")
                  ),
                  backend = "cmdstanr", threads = threading(2, static = TRUE), cores = 4,
                  chains = 4 # Reminder that I have a seed set for the entire session (see top)
)

save.image("patterning_remittances.RData")

# Convert to odds ratios

ests_mod_when <- data.frame(exp(cbind(Odds_Ratio = fixef(mod_when)[,1], Lower = fixef(mod_when, probs = c(.05, .95))[,3], Upper = fixef(mod_when, probs = c(.5, .95))[,4])))

ests_mod_when$parameters <- c("Intercept", "Severity", "Time-Interval", "Time-Late", "Frequency", "Autocorrelation", "Spatial extent", "Wealth", "Household size", "Migrant number", "Dist. to pop. center", "Mean NDVI", "Time-Interval:Severity", "Time-After:Severity", "Time-Interval:Frequency", "Time-After:Frequency", "Time-Interval:Autocorrelation", "Time-After:Autocorrelation", "Time-Interval:Spatial extent", "Time-After:Spatial extent")

### Posterior checks ###

# For variables moderately correlated, check for signs of ridges

posterior <- as.array(mod_when)

color_scheme_set("pink")
mcmc_pairs(posterior, pars = c("b_NDVI_mean_s", "b_avg_area_km_s", "b_annual_frequency_s", "b_autocorrelation_s", "b_severity_s"),
           off_diag_args = list(size = 1.5))

bayes_R2(mod_when) # 0.2670573

### Caterpillar plot ###

ests_mod_when <- ests_mod_when %>%
  mutate(parameters = factor(parameters, levels = c("Time-After:Spatial extent", "Time-Interval:Spatial extent", "Time-After:Autocorrelation", "Time-Interval:Autocorrelation", "Time-After:Frequency", "Time-Interval:Frequency", "Time-After:Severity", "Time-Interval:Severity", "Mean NDVI", "Dist. to pop. center", "Migrant number", "Household size", "Wealth", "Time-Late","Time-Interval", "Spatial extent", "Autocorrelation", "Frequency", "Severity", "Intercept"))) 

ggplot(ests_mod_when, aes(x = parameters, y = Odds_Ratio)) +
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

### Examine two-way interactions ###

## Severity

means <- dat %>%
  summarise(across(c(annual_frequency_s, autocorrelation_s, avg_area_km_s, pop_center_s, wealth_index_s, hh_size_s, migrant_num_s, pop_center_s, NDVI_mean_s), ~ mean(.x, na.rm = TRUE)))

# Get range of severity_s
severity_s_seq <- seq(min(dat$severity_s, na.rm = TRUE), max(dat$severity_s, na.rm = TRUE), length.out = 100)

# Create new data grid
grid_base <- expand_grid(
  hh_time = unique(na.omit(factor(dat$hh_time))),   # all levels of source
  severity_s = severity_s_seq
)

new_data <- grid_base %>%
  bind_cols(means[rep(1, nrow(grid_base)), ])

preds <- new_data %>%
  add_epred_draws(mod_when, re_formula = NA)  # remove REs for marginal/fixed predictions

# Plot

sev_plot <- ggplot(preds, aes(x = severity_s, y = .epred, color = hh_time, fill = hh_time)) +
  stat_summary(fun = mean, geom = "line", size = 2) +
  stat_summary(fun.data = mean_cl_boot, geom = "ribbon", alpha = 0.2, color = NA) +
  labs(
    x = "Severity",
    y = "Predicted probability of remittance",
    color = "Timing",
    fill = "Timing"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.title.x = element_text(size = 15, face = "bold"),
    legend.position = "none",
    plot.margin = margin(t = 40, r = 10, b = 0, l = 10)
  ) +
  coord_cartesian(xlim = c(-2, 5), ylim = c(0.2, 0.75), clip = "off") +
  annotate("text", x = -2.3, y = 0.85, label = "A", size = 6, fontface = "bold")

## Spatial extent

means <- dat %>%
  summarise(across(c(severity_s, annual_frequency_s, autocorrelation_s, pop_center_s, wealth_index_s, hh_size_s, migrant_num_s, pop_center_s, NDVI_mean_s), ~ mean(.x, na.rm = TRUE)))

# Get range of avg_area_km_s
avg_area_km_s_seq <- seq(min(dat$avg_area_km_s, na.rm = TRUE), max(dat$avg_area_km_s, na.rm = TRUE), length.out = 100)

# Create new data grid
grid_base <- expand_grid(
  hh_time = unique(na.omit(factor(dat$hh_time))),   # all levels of source
  avg_area_km_s = avg_area_km_s_seq
)

new_data <- grid_base %>%
  bind_cols(means[rep(1, nrow(grid_base)), ])

preds <- new_data %>%
  add_epred_draws(mod_when, re_formula = NA)  # remove REs for marginal/fixed predictions

# Plot

spa_plot <- ggplot(preds, aes(x = avg_area_km_s, y = .epred, color = hh_time, fill = hh_time)) +
  stat_summary(fun = mean, geom = "line", size = 2) +
  stat_summary(fun.data = mean_cl_boot, geom = "ribbon", alpha = 0.2, color = NA) +
  labs(
    x = "Spatial extent",
    y = "Predicted probability of remittance",
    color = "Timing",
    fill = "Timing"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, face = "bold"),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 15, face = "bold"),
    legend.position = "right",
    plot.margin = margin(t = 40, r = 10, b = 0, l = 10)
  ) +
  coord_cartesian(xlim = c(-2, 5), ylim = c(0.2, 0.75), clip = "off") +
  annotate("text", x = -2.3, y = 0.85, label = "B", size = 6, fontface = "bold")


## Frequency

means <- dat %>%
  summarise(across(c(severity_s, autocorrelation_s, avg_area_km_s, pop_center_s, wealth_index_s, hh_size_s, migrant_num_s, pop_center_s, NDVI_mean_s), ~ mean(.x, na.rm = TRUE)))

# Get range of annual_frequency_s
annual_frequency_s_seq <- seq(min(dat$annual_frequency_s, na.rm = TRUE), max(dat$annual_frequency_s, na.rm = TRUE), length.out = 100)

# Create new data grid
grid_base <- expand_grid(
  hh_time = unique(na.omit(factor(dat$hh_time))),   # all levels of source
  annual_frequency_s = annual_frequency_s_seq
)

new_data <- grid_base %>%
  bind_cols(means[rep(1, nrow(grid_base)), ])

preds <- new_data %>%
  add_epred_draws(mod_when, re_formula = NA)  # remove REs for marginal/fixed predictions

# Plot

freq_plot <- ggplot(preds, aes(x = annual_frequency_s, y = .epred, color = hh_time, fill = hh_time)) +
  stat_summary(fun = mean, geom = "line", size = 2) +
  stat_summary(fun.data = mean_cl_boot, geom = "ribbon", alpha = 0.2, color = NA) +
  labs(
    x = "Frequency",
    y = "Predicted probability of remittance",
    color = "Timing",
    fill = "Timing"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.title.x = element_text(size = 15, face = "bold"),
    legend.position = "none",
    plot.margin = margin(t = 40, r = 10, b = 0, l = 10)
  )  +
  coord_cartesian(xlim = c(-2, 5), ylim = c(0.2, 0.75), clip = "off") +
  annotate("text", x = -2.3, y = 0.85, label = "C", size = 6, fontface = "bold")

## Autocorrelation

means <- dat %>%
  summarise(across(c(severity_s, annual_frequency_s, avg_area_km_s, pop_center_s, wealth_index_s, hh_size_s, migrant_num_s, pop_center_s, NDVI_mean_s), ~ mean(.x, na.rm = TRUE)))

# Get range of autocorrelation_s
autocorrelation_s_seq <- seq(min(dat$autocorrelation_s, na.rm = TRUE), max(dat$autocorrelation_s, na.rm = TRUE), length.out = 100)

# Create new data grid
grid_base <- expand_grid(
  hh_time = unique(na.omit(factor(dat$hh_time))),   # all levels of source
  autocorrelation_s = autocorrelation_s_seq
)

new_data <- grid_base %>%
  bind_cols(means[rep(1, nrow(grid_base)), ])

preds <- new_data %>%
  add_epred_draws(mod_when, re_formula = NA)  # remove REs for marginal/fixed predictions

# Plot

auto_plot <- ggplot(preds, aes(x = autocorrelation_s, y = .epred, color = hh_time, fill = hh_time)) +
  stat_summary(fun = mean, geom = "line", size = 2) +
  stat_summary(fun.data = mean_cl_boot, geom = "ribbon", alpha = 0.2, color = NA) +
  labs(
    x = "Temporal autocorrelation",
    y = "Predicted probability of remittance",
    color = "Timing",
    fill = "Timing"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15, face = "bold"),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 15, face = "bold"),
    legend.position = "right",
    plot.margin = margin(t = 40, r = 10, b = 0, l = 10)
  ) +
  coord_cartesian(xlim = c(-2, 5), ylim = c(0.2, 0.75), clip = "off") +
  annotate("text", x = -2.3, y = 0.85, label = "D", size = 6, fontface = "bold")

## Save

#combined_plot <- plot_grid(sev_plot, spa_plot, freq_plot, auto_plot, 
                          # ncol = 2)

combined_plot <- (sev_plot + spa_plot) /
  (freq_plot + auto_plot) +
  plot_layout(guides = "collect")

ggsave("facet_plot.pdf", combined_plot, width = 10, height = 8)


##### Do climate variables predict when to send a migrant? #####

# Change contrast category

dat$hh_time <- relevel(as.factor(dat$hh_time), "early")

# Model

mod_time <- brm(hh_time ~ 
                     severity_s + annual_frequency_s + autocorrelation_s + avg_area_km_s + # Environmental predictors of interest
                     wealth_index_s + hh_size_s + migrant_num_s + pop_center_s + NDVI_mean_s + # Controls
                     (1 | house + date_s + census_tract + country), 
                   data = dat,
                   family = categorical(),
                   control = list(adapt_delta = 0.99),
                   backend = "cmdstanr", threads = threading(2, static = TRUE), cores = 4, 
                   chains = 4
)

save.image("patterning_remittances.RData")

# Convert to odds ratios

ests_mod_time <- data.frame(exp(cbind(Odds_Ratio = fixef(mod_time)[,1], Lower = fixef(mod_time, probs = c(.05, .95))[,3], Upper = fixef(mod_time, probs = c(.5, .95))[,4])))

ests_mod_time$parameters <- c("Interval-Intercept", "After-Intercept", "Interval-Severity", "Interval-Frequency", "Interval-Autocorrelation", "Interval-Spatial extent", "Interval-Wealth", "Interval-Household size", "Interval-Migrant number", "Interval-Dist. to pop. center", "Interval-Mean NDVI", "After-Severity", "After-Frequency", "After-Autocorrelation", "After-Spatial extent", "After-Wealth", "After-Household size", "After-Migrant number", "After-Dist. to pop. center", "After-Mean NDVI")

### Posterior checks ###

# For variables moderately correlated, check for signs of ridges

posterior <- as.array(mod_time)

color_scheme_set("pink")
mcmc_pairs(posterior, pars = c("b_muinterval_NDVI_mean_s", "b_muinterval_avg_area_km_s", "b_muinterval_annual_frequency_s", "b_muinterval_autocorrelation_s", "b_muinterval_severity_s", "b_mulate_NDVI_mean_s", "b_mulate_avg_area_km_s", "b_mulate_annual_frequency_s", "b_mulate_autocorrelation_s", "b_mulate_severity_s"),
           off_diag_args = list(size = 1.5))

# bayes_R2(mod_time) # Not defined for categorical models

### Caterpillar plot ###

ests_mod_time <- ests_mod_time %>%
  mutate(parameters = factor(parameters, levels = rev(c("Interval-Intercept", "After-Intercept", "Interval-Severity", "Interval-Frequency", "Interval-Autocorrelation", "Interval-Spatial extent", "Interval-Wealth", "Interval-Household size", "Interval-Migrant number", "Interval-Dist. to pop. center", "Interval-Mean NDVI", "After-Severity", "After-Frequency", "After-Autocorrelation", "After-Spatial extent", "After-Wealth", "After-Household size", "After-Migrant number", "After-Dist. to pop. center", "After-Mean NDVI")))) #%>%
  #filter(!(parameters %in% c("Migrant number")))

ggplot(ests_mod_time, aes(x = parameters, y = Odds_Ratio)) +
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


##### 2.3.1.4 Time window #####

# What if hydroclimate patterns are considered over 10 years instead of 5?

### Load data ###
burkina_10 <- read_csv("data/burkinafaso_SPEI-3_2000-2009_10percentile_1981-2009_means.csv")
kenya_10 <- read_csv("data/kenya_SPEI-3_2000-2009_10percentile_1981-2009_means.csv")
nigeria_10 <- read_csv("data/nigeria_SPEI-3_2000-2009_10percentile_1981-2009_means.csv")
senegal_10 <- read_csv("data/senegal_SPEI-3_2000-2009_10percentile_1981-2009_means.csv")
south_africa_10 <- read_csv("data/southafrica_SPEI-3_2000-2009_10percentile_1981-2009_means.csv")
uganda_10 <- read_csv("data/uganda_SPEI-3_2000-2009_10percentile_1981-2009_means.csv")

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

# scale doesn't play well with mutate anymore so go old school:

sub_10$autocorrelation_s <- as.numeric(scale(sub_10$autocorrelation))
sub_10$NDVI_mean_s <- as.numeric(scale(sub_10$NDVI_mean))
sub_10$SVI_severity_s <- as.numeric(scale(sub_10$SVI_severity))
sub_10$avg_area_km_s <- as.numeric(scale(sub_10$avg_area_km))
sub_10$severity_s <- as.numeric(scale(sub_10$severity_pos))
sub_10$annual_frequency_s <- as.numeric(scale(sub_10$annual_frequency))

sub_10 <-  sub_10 %>%
  select(house, country, annual_frequency_s, severity_s, autocorrelation_s, avg_area_km_s, NDVI_mean_s, SVI_severity_s)

# Going to put a prefix on columns before merge

columns_to_prefix <- c("annual_frequency_s", "severity_s", "autocorrelation_s", "avg_area_km_s", "NDVI_mean_s", "SVI_severity_s")

sub_10a <- sub_10 %>% 
  rename_with(~ paste0("yr10_", .), all_of(columns_to_prefix))

# Merge

dat_10 <- dat %>%
  select(-annual_frequency, -severity, -severity_pos, -autocorrelation, -avg_area, -avg_area_km, -NDVI_mean, -SVI_severity, -annual_frequency_s, -severity_s, -autocorrelation_s, -avg_area_km_s, -NDVI_mean_s, -SVI_severity_s)

dat_onerow_10 <- dat_onerow %>%
  select(-annual_frequency, -severity, -severity_pos, -autocorrelation, -avg_area, -avg_area_km, -NDVI_mean, -SVI_severity, -annual_frequency_s, -severity_s, -autocorrelation_s, -avg_area_km_s, -NDVI_mean_s, -SVI_severity_s)


dat_sub_10 <- inner_join(dat_10, sub_10a, by = c("house", "country"))

dat_onerow_sub_10 <- inner_join(dat_onerow_10, sub_10a, by = c("house", "country"))


### Check correlations ###

# A correlation about 0.7, but the weakly informative prior preventing ridges in the posterior suggesting fits are still fine.

cor.mat_10 <- cor(dat_onerow_sub_10[, c("yr10_severity_s", "yr10_annual_frequency_s", "yr10_autocorrelation_s", "yr10_avg_area_km_s", "wealth_index_s", "hh_size_s", "migrant_num_s", "pop_center_s", "yr10_NDVI_mean_s")], use = "complete.obs")
# Still some moderate correlations to look out for (posterior checks), up to 0.73

### Analysis ###

mod_10 <- brm(any_remit ~ 
              yr10_severity_s + yr10_annual_frequency_s + yr10_autocorrelation_s + yr10_avg_area_km_s + # Environmental predictors of interest
              wealth_index_s + hh_size_s + migrant_num_s + pop_center_s + yr10_NDVI_mean_s + # Controls
              (1 | date_s + census_tract + country), 
            data = dat_onerow_sub_10,
            family = bernoulli,
            control = list(adapt_delta = 0.99),
            prior = c(prior(cauchy(0, 2), class = "sd"),
                      prior(normal(0, 1), class = "b")
            ),
            backend = "cmdstanr", threads = threading(2, static = TRUE), cores = 4,
            chains = 4
)

save.image("patterning_remittances.RData")

posterior_10 <- as.array(mod_10)

color_scheme_set("pink")
mcmc_pairs(posterior_10, pars = c("b_yr10_NDVI_mean_s", "b_yr10_avg_area_km_s", "b_yr10_annual_frequency_s", "b_yr10_autocorrelation_s", "b_yr10_severity_s"),
           off_diag_args = list(size = 1.5))

ests_time0 <- data.frame(exp(cbind(Odds_Ratio = fixef(mod_10)[,1], Lower = fixef(mod_10, probs = c(.05, .95))[,3], Upper = fixef(mod_10, probs = c(.5, .95))[,4])))

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

bayes_R2(mod_10) #0.3366964


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

# scale doesn't play well with mutate anymore so go old school:

sub_15$autocorrelation_s <- as.numeric(scale(sub_15$autocorrelation))
sub_15$avg_area_km_s <- as.numeric(scale(sub_15$avg_area_km))
sub_15$severity_s <- as.numeric(scale(sub_15$severity_pos))
sub_15$annual_frequency_s <- as.numeric(scale(sub_15$annual_frequency))


sub_15 <-  sub_15 %>%
  select(house, country, annual_frequency_s, severity_s, autocorrelation_s, avg_area_km_s) # Don't need to pull in NDVI or SVI-severity because it's not changed by the shifted cutoff.

# Going to put a prefix on columns before merge

columns_to_prefix <- c("annual_frequency_s", "severity_s", "autocorrelation_s", "avg_area_km_s")

sub_15a <- sub_15 %>% 
  rename_with(~ paste0("sd_", .), all_of(columns_to_prefix))

# Merge

dat_15 <- dat %>%
  select(-annual_frequency, -severity, -severity_pos, -autocorrelation, -avg_area, -avg_area_km, -annual_frequency_s, -severity_s, -autocorrelation_s, -avg_area_km_s)

dat_onerow_15 <- dat_onerow %>%
  select(-annual_frequency, -severity, -severity_pos, -autocorrelation, -avg_area, -avg_area_km, -annual_frequency_s, -severity_s, -autocorrelation_s, -avg_area_km_s)

dat_sub_15 <- inner_join(dat_15, sub_15a, by = c("house", "country"))

dat_onerow_sub_15 <- inner_join(dat_15, sub_15a, by = c("house", "country"))

# High values of for spatial extent...

dat_onerow_sub_15_noext <- dat_onerow_sub_15[dat_onerow_sub_15$sd_avg_area_km_s <= 3,] # 371 observations. Only have one annual_frequency (medium low; -1.08251295545341), one autocorrelation (0.2), though a range of severities. 2/3 remit, all are from Nigeria

# High severity...

dat_onerow_sub_15_noext <- dat_onerow_sub_15_noext[dat_onerow_sub_15_noext$sd_severity_s <= 3,] # 9 observations. All identical on spatial extent, annual_frequency, severity, and autocorrelation; 2 remit; all uganda. 

### Check correlations ###

cor.mat_15 <- cor(dat_onerow_sub_15[, c("sd_severity_s",  "sd_annual_frequency_s", "sd_autocorrelation_s", "sd_avg_area_km_s", "wealth_index_s", "hh_size_s", "migrant_num_s", "pop_center_s",  "NDVI_mean_s")], use = "complete.obs")
# Still some moderate correlations to look out for (posterior checks), up to 0.52

### Analysis ###

mod_15 <- brm(any_remit ~ 
                sd_severity_s + sd_annual_frequency_s + sd_autocorrelation_s + sd_avg_area_km_s + # Environmental predictors of interest
                wealth_index_s + hh_size_s + migrant_num_s + pop_center_s + NDVI_mean_s + # Controls
                (1 | date_s + census_tract + country), 
              data = dat_onerow_sub_15,
              family = bernoulli,
              control = list(adapt_delta = 0.99),
              prior = c(prior(cauchy(0, 2), class = "sd"),
                        prior(normal(0, 1), class = "b")
              ),
              backend = "cmdstanr", threads = threading(2, static = TRUE), cores = 4,
              chains = 4
)

mod_15_noext <- brm(any_remit ~ 
                      sd_severity_s + sd_annual_frequency_s + sd_autocorrelation_s + sd_avg_area_km_s + # Environmental predictors of interest
                      wealth_index_s + hh_size_s + migrant_num_s + pop_center_s + NDVI_mean_s + # Controls
                      (1 | date_s + census_tract + country), 
                    data = dat_onerow_sub_15_noext,
                    family = bernoulli,
                    control = list(adapt_delta = 0.99),
                    prior = c(prior(cauchy(0, 2), class = "sd"),
                              prior(normal(0, 1), class = "b")
                    ),
                    backend = "cmdstanr", threads = threading(2, static = TRUE), cores = 4,
                    chains = 4
)

save.image("patterning_remittances.RData")

bayes_R2(mod_15) #0.3868731

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


### No extreme vals

bayes_R2(mod_15_noext) #0.3861453

posterior_15_noext <- as.array(mod_15_noext)

color_scheme_set("pink")
mcmc_pairs(posterior_15_noext, pars = c("b_NDVI_mean_s", "b_sd_avg_area_km_s", "b_sd_annual_frequency_s", "b_sd_autocorrelation_s", "b_sd_severity_s"),
           off_diag_args = list(size = 1.5))

ests_mod15_noext <- data.frame(exp(cbind(Odds_Ratio = fixef(mod_15_noext)[,1], Lower = fixef(mod_15_noext, probs = c(.05, .95))[,3], Upper = fixef(mod_15_noext, probs = c(.5, .95))[,4])))

ests_mod15_noext$parameters <- c("Intercept", "Severity", "Frequency", "Autocorrelation", "Spatial extent", "Wealth", "Household size", "Migrant number", "Dist. to pop. center", "Mean NDVI")


ests_mod15_noext <- ests_mod15_noext %>%
  mutate(parameters = factor(parameters, levels = c("Mean NDVI", "Dist. to pop. center", "Migrant number", "Household size", "Wealth", "Spatial extent", "Autocorrelation", "Frequency", "Severity", "Intercept"))) %>%
  filter(!(parameters %in% c("Migrant number")))

ggplot(ests_mod15_noext, aes(x = parameters, y = Odds_Ratio)) +
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

# scale doesn't play well with mutate anymore so go old school:

sub_12$autocorrelation_s <- as.numeric(scale(sub_12$autocorrelation))
sub_12$avg_area_km_s <- as.numeric(scale(sub_12$avg_area_km))
sub_12$severity_s <- as.numeric(scale(sub_12$severity_pos))
sub_12$annual_frequency_s <- as.numeric(scale(sub_12$annual_frequency))

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
  select(-annual_frequency, -severity, -severity_pos, -autocorrelation, -avg_area, -avg_area_km, -annual_frequency_s, -severity_s, -autocorrelation_s, -avg_area_km_s)

dat_onerow_12 <- dat_onerow %>%
  select(-annual_frequency, -severity, -severity_pos, -autocorrelation, -avg_area, -avg_area_km, -annual_frequency_s, -severity_s, -autocorrelation_s, -avg_area_km_s)


dat_sub_12 <- inner_join(dat_12, sub_12a, by = c("house", "country"))

dat_onerow_sub_12 <- inner_join(dat_onerow_12, sub_12a, by = c("house", "country"))


### Check correlations ###

# A correlation about 0.7, but the weakly informative prior preventing ridges in the posterior suggesting fits are still fine.

cor.mat_12 <- cor(dat_onerow_sub_12[, c("spei12_severity_s", "spei12_annual_frequency_s", "spei12_autocorrelation_s", "spei12_avg_area_km_s", "hh_size_s", "migrant_num_s", "wealth_index_s", "pop_center_s", "NDVI_mean_s")], use = "complete.obs")
# Moderate correlations to look out for (posterior checks), up to 0.73

### Analysis ###

mod_12 <- brm(any_remit ~ 
                spei12_severity_s + spei12_annual_frequency_s + spei12_autocorrelation_s + spei12_avg_area_km_s + # Environmental predictors of interest
                wealth_index_s + hh_size_s + migrant_num_s + pop_center_s + NDVI_mean_s + # Controls
                (1 | date_s + census_tract + country), 
              data = dat_onerow_sub_12,
              family = bernoulli,
              control = list(adapt_delta = 0.99),
              prior = c(prior(cauchy(0, 2), class = "sd"),
                        prior(normal(0, 1), class = "b")
              ),
              backend = "cmdstanr", threads = threading(2, static = TRUE), cores = 4,
              chains = 4
)

save.image("patterning_remittances.RData")

bayes_R2(mod_12) #0.337013

posterior_12 <- as.array(mod_12)

color_scheme_set("pink")
mcmc_pairs(posterior_12, pars = c("b_NDVI_mean_s", "b_spei12_avg_area_km_s", "b_spei12_annual_frequency_s", "b_spei12_autocorrelation_s", "b_spei12_severity_s"),
           off_diag_args = list(size = 1.5))

ests_mod12 <- data.frame(exp(cbind(Odds_Ratio = fixef(mod_12)[,1], Lower = fixef(mod_12, probs = c(.05, .95))[,3], Upper = fixef(mod_12, probs = c(.5, .95))[,4])))

ests_mod12$parameters <- c("Intercept", "Severity", "Frequency", "Autocorrelation", "Spatial extent", "Wealth", "Household size", "Migrant number", "Dist. to pop. center", "Mean NDVI")


ests_mod12 <- ests_mod12 %>%
  mutate(parameters = factor(parameters, levels = c("Mean NDVI", "Dist. to pop. center", "Migrant number", "Household size", "Wealth", "Spatial extent", "Autocorrelation", "Frequency", "Severity", "Intercept"))) %>%
  filter(!(parameters %in% c("Migrant number")))

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

svi_sev <- brm(any_remit ~ 
              SVI_severity_s +
              autocorrelation_s + annual_frequency_s + severity_s + avg_area_km_s + # Environmental predictors of interest
              wealth_index_s + hh_size_s + migrant_num_s + pop_center_s + NDVI_mean_s + # Controls
              (1 | date_s + census_tract + country), 
            data = dat_onerow,
            family = bernoulli,
            control = list(adapt_delta = 0.99),
            prior = c(prior(cauchy(0, 2), class = "sd"),
            prior(normal(0, 1), class = "b")
            ),
            backend = "cmdstanr", threads = threading(2, static = TRUE), cores = 4,
            chains = 4
)

save.image("patterning_remittances.RData")

bayes_R2(svi_sev) #0.3369297

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


##### Two-way interaction plot #####

mod_ixn <- brm(any_remit ~ 
              severity_s * avg_area_km_s + annual_frequency_s + autocorrelation_s +  # Environmental predictors of interest
              wealth_index_s + hh_size_s + migrant_num_s + pop_center_s + NDVI_mean_s + # Controls
              (1 | date_s + census_tract + country), 
            data = dat_onerow,
            family = bernoulli,
            control = list(adapt_delta = 0.99),
            prior = c(prior(cauchy(0, 2), class = "sd"),
                      prior(normal(0, 1), class = "b")
            ),
            backend = "cmdstanr", threads = threading(2, static = TRUE), cores = 4,
            chains = 4
)


# Convert to odds ratios

ests_mod_ixn <- data.frame(exp(cbind(Odds_Ratio = fixef(mod_ixn)[,1], Lower = fixef(mod_ixn, probs = c(.05, .95))[,3], Upper = fixef(mod_ixn, probs = c(.5, .95))[,4])))

ests_mod_ixn$parameters <- c("Intercept", "Severity", "Spatial extent", "Frequency", "Autocorrelation", "Wealth", "Household size", "Migrant number", "Dist. to pop. center", "Mean NDVI", "Severity * Spatial extent")

### Posterior checks ###

# For variables moderately correlated, check for signs of ridges

posterior <- as.array(mod_ixn)

color_scheme_set("pink")
mcmc_pairs(posterior, pars = c("b_annual_frequency_s", "b_autocorrelation_s", "b_severity_s", "b_avg_area_km_s", "b_severity_s:avg_area_km_s"), # main effects look fine from first pairs plot; focus here on interaction terms
           off_diag_args = list(size = 1.5))
# The only clear positive trends are between interaction terms, so not surprising.

bayes_R2(mod_ixn) #0.336

ests_mod_ixn <- ests_mod_ixn %>%
  mutate(parameters = factor(parameters, levels = c("Mean NDVI", "Dist. to pop. center", "Migrant number", "Household size", "Wealth", "Frequency * Severity * Spatial extent", "Severity * Spatial extent", "Frequency * Spatial extent", "Frequency * Severity", "Spatial extent", "Autocorrelation", "Frequency", "Severity", "Intercept"))) %>%
  filter(!(parameters %in% c("Migrant number")))

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

### Examine two-way interactions ###

means <- dat_onerow_noext %>%
  summarise(across(c(annual_frequency_s, autocorrelation_s, pop_center_s, wealth_index_s, hh_size_s, migrant_num_s, pop_center_s, NDVI_mean_s), ~ mean(.x, na.rm = TRUE)))

# Get range of avg_area_km_s
avg_area_km_s_seq <- seq(min(dat_onerow$avg_area_km_s, na.rm = TRUE), max(dat_onerow$avg_area_km_s, na.rm = TRUE), length.out = 100)

# Get range of avg_area_km_s
severity_s_seq <- seq(min(dat_onerow$severity_s, na.rm = TRUE), max(dat_onerow$severity_s, na.rm = TRUE), length.out = 100)

# Create new data grid
grid_base <- expand_grid(
  avg_area_km_s = avg_area_km_s_seq,
  severity_s = severity_s_seq
)

new_data <- grid_base %>%
  bind_cols(means[rep(1, nrow(grid_base)), ])

preds <- new_data %>%
  add_epred_draws(mod_ixn, re_formula = NA)  # remove REs for marginal/fixed predictions

# Plot

preds_summary <- preds %>%
  group_by(avg_area_km_s, severity_s) %>%
  summarise(
    mean_epred = mean(.epred),
    .lower = quantile(.epred, 0.05),
    .upper = quantile(.epred, 0.95),
    .groups = "drop"
  )

ggplot(preds_summary, aes(x = avg_area_km_s, y = severity_s, fill = mean_epred)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma", name = "Predicted\nprobability") +
  labs(
    x = "Spatial extent", 
    y = "Severity"
    #title = "Predicted probability of remittance by drought extent and severity"
  ) +
  theme_minimal()