library(tidyverse) # Various functions useful for data processing

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set working directory to location of this file (script file)


################# SPEI 3 5 year 10th percentile #################

##### Burkina #####

soc_bur <- read_csv("burkina_faso.csv")
env_spei3_5yr_10th_bur <- read_csv("burkinafaso_SPEI-3_2005-2009_10percentile_1981-2009_means.csv")

# Reduce columns to avoid duplicates

soc_bur <- soc_bur %>%
  select(-district, -census_tract, -country, -lat, -long)

env_spei3_5yr_10th_bur <- env_spei3_5yr_10th_bur %>%
  select(house, district, census_tract, country, lat, long, lat_SPEI, long_SPEI, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity, pop_cent_lat, pop_cent_long)


# soc_bur is one row per migrant, env_spei... is one row per house. Left merge!

df_spei3_5yr_10th_bur <- left_join(soc_bur, env_spei3_5yr_10th_bur, by = "house") # First time doing this I left lat, lon, and census tract in from both datasets to make sure they match, and they do.


##### Kenya #####

soc_ken <- read_csv("kenya.csv")
env_spei3_5yr_10th_ken <- read_csv("kenya_SPEI-3_2005-2009_10percentile_1981-2009_means.csv")

# Reduce columns to avoid duplicates

soc_ken <- soc_ken %>%
  select(-district, -census_tract, -country, -lat, -long)

env_spei3_5yr_10th_ken <- env_spei3_5yr_10th_ken %>%
  select(house, district, census_tract, country, lat, long, lat_SPEI, long_SPEI, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity, pop_cent_lat, pop_cent_long)


# soc_ken is one row per migrant, env_spei... is one row per house. Left merge!

df_spei3_5yr_10th_ken <- left_join(soc_ken, env_spei3_5yr_10th_ken, by = "house") # First time doing this I left lat, lon, and census tract in from both datasets to make sure they match, and they do.


##### Nigeria #####

soc_ng <- read_csv("nigeria.csv")
env_spei3_5yr_10th_ng <- read_csv("nigeria_SPEI-3_2005-2009_10percentile_1981-2009_means.csv")

# Reduce columns to avoid duplicates

soc_ng <- soc_ng %>%
  select(-district, -census_tract, -country, -lat, -long)

env_spei3_5yr_10th_ng <- env_spei3_5yr_10th_ng %>%
  select(house, district, census_tract, country, lat, long, lat_SPEI, long_SPEI, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity, pop_cent_lat, pop_cent_long)


# soc_ng is one row per migrant, env_spei... is one row per house. Left merge!

df_spei3_5yr_10th_ng <- left_join(soc_ng, env_spei3_5yr_10th_ng, by = "house") # First time doing this I left lat, lon, and census tract in from both datasets to make sure they match, and they do.
df_spei3_5yr_10th_ng$date <- NA


##### Senegal #####

soc_sen <- read_csv("senegal.csv")
env_spei3_5yr_10th_sen <- read_csv("senegal_SPEI-3_2005-2009_10percentile_1981-2009_means.csv")

# Reduce columns to avoid duplicates

soc_sen <- soc_sen %>%
  select(-district, -census_tract, -country, -lat, -long)

env_spei3_5yr_10th_sen <- env_spei3_5yr_10th_sen %>%
  select(house, department, census_tract, country, lat, long, lat_SPEI, long_SPEI, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity, pop_cent_lat, pop_cent_long) %>%
  rename(district = "department")


# soc_sen is one row per migrant, env_spei... is one row per house. Left merge!

df_spei3_5yr_10th_sen <- left_join(soc_sen, env_spei3_5yr_10th_sen, by = "house") # First time doing this I left lat, lon, and census tract in from both datasets to make sure they match, and they do.
df_spei3_5yr_10th_sen$date <- NA


##### South Africa #####

soc_za <- read_csv("south_africa.csv")
env_spei3_5yr_10th_za <- read_csv("southafrica_SPEI-3_2005-2009_10percentile_1981-2009_means.csv")

# Reduce columns to avoid duplicates

soc_za <- soc_za %>%
  select(-district, -census_tract, -country, -lat, -long)

env_spei3_5yr_10th_za <- env_spei3_5yr_10th_za %>%
  select(house, district, municipality, country, lat, long, lat_SPEI, long_SPEI, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity, pop_cent_lat, pop_cent_long) %>%
  rename(census_tract = "municipality")


# soc_za is one row per migrant, env_spei... is one row per house. Left merge!

df_spei3_5yr_10th_za <- left_join(soc_za, env_spei3_5yr_10th_za, by = "house") # First time doing this I left lat, lon, and census tract in from both datasets to make sure they match, and they do.
df_spei3_5yr_10th_za$date <- NA


##### Uganda #####

soc_uga <- read_csv("uganda.csv")
env_spei3_5yr_10th_uga <- read_csv("uganda_SPEI-3_2005-2009_10percentile_1981-2009_means.csv")

# Reduce columns to avoid duplicates

soc_uga <- soc_uga %>%
  select(-district, -census_tract, -country, -lat, -long)

env_spei3_5yr_10th_uga <- env_spei3_5yr_10th_uga %>%
  select(house, district, census_tract, country, lat, long, lat_SPEI, long_SPEI, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity, pop_cent_lat, pop_cent_long)


# soc_uga is one row per migrant, env_spei... is one row per house. Left merge!

df_spei3_5yr_10th_uga <- left_join(soc_uga, env_spei3_5yr_10th_uga, by = "house") # First time doing this I left lat, lon, and census tract in from both datasets to make sure they match, and they do.


##### Merge #####

df_spei3_5yr_10th <- bind_rows(df_spei3_5yr_10th_bur, df_spei3_5yr_10th_ken, df_spei3_5yr_10th_ng, df_spei3_5yr_10th_sen, df_spei3_5yr_10th_za, df_spei3_5yr_10th_uga)

write_csv(df_spei3_5yr_10th, "spei3_yr5_perc.csv")


################# SPEI 3 5 year 10th percentile #################

##### Burkina #####

soc_bur <- read_csv("burkina_faso.csv")
env_spei3_10yr_10th_bur <- read_csv("burkinafaso_SPEI-3_2000-2009_10percentile_1981-2009_means.csv")

# Reduce columns to avoid duplicates

soc_bur <- soc_bur %>%
  select(-district, -census_tract, -country, -lat, -long)

env_spei3_10yr_10th_bur <- env_spei3_10yr_10th_bur %>%
  select(house, district, census_tract, country, lat, long, lat_SPEI, long_SPEI, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity, pop_cent_lat, pop_cent_long)


# soc_bur is one row per migrant, env_spei... is one row per house. Left merge!

df_spei3_10yr_10th_bur <- left_join(soc_bur, env_spei3_10yr_10th_bur, by = "house") # First time doing this I left lat, lon, and census tract in from both datasets to make sure they match, and they do.


##### Kenya #####

soc_ken <- read_csv("kenya.csv")
env_spei3_10yr_10th_ken <- read_csv("kenya_SPEI-3_2005-2009_10percentile_1981-2009_means.csv")

# Reduce columns to avoid duplicates

soc_ken <- soc_ken %>%
  select(-district, -census_tract, -country, -lat, -long)

env_spei3_10yr_10th_ken <- env_spei3_10yr_10th_ken %>%
  select(house, district, census_tract, country, lat, long, lat_SPEI, long_SPEI, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity, pop_cent_lat, pop_cent_long)


# soc_ken is one row per migrant, env_spei... is one row per house. Left merge!

df_spei3_10yr_10th_ken <- left_join(soc_ken, env_spei3_10yr_10th_ken, by = "house") # First time doing this I left lat, lon, and census tract in from both datasets to make sure they match, and they do.


##### Nigeria #####

soc_ng <- read_csv("nigeria.csv")
env_spei3_10yr_10th_ng <- read_csv("nigeria_SPEI-3_2005-2009_10percentile_1981-2009_means.csv")

# Reduce columns to avoid duplicates

soc_ng <- soc_ng %>%
  select(-district, -census_tract, -country, -lat, -long)

env_spei3_10yr_10th_ng <- env_spei3_10yr_10th_ng %>%
  select(house, district, census_tract, country, lat, long, lat_SPEI, long_SPEI, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity, pop_cent_lat, pop_cent_long)


# soc_ng is one row per migrant, env_spei... is one row per house. Left merge!

df_spei3_10yr_10th_ng <- left_join(soc_ng, env_spei3_10yr_10th_ng, by = "house") # First time doing this I left lat, lon, and census tract in from both datasets to make sure they match, and they do.
df_spei3_10yr_10th_ng$date <- NA


##### Senegal #####

soc_sen <- read_csv("senegal.csv")
env_spei3_10yr_10th_sen <- read_csv("senegal_SPEI-3_2005-2009_10percentile_1981-2009_means.csv")

# Reduce columns to avoid duplicates

soc_sen <- soc_sen %>%
  select(-district, -census_tract, -country, -lat, -long)

env_spei3_10yr_10th_sen <- env_spei3_10yr_10th_sen %>%
  select(house, department, census_tract, country, lat, long, lat_SPEI, long_SPEI, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity, pop_cent_lat, pop_cent_long) %>%
  rename(district = "department")


# soc_sen is one row per migrant, env_spei... is one row per house. Left merge!

df_spei3_10yr_10th_sen <- left_join(soc_sen, env_spei3_10yr_10th_sen, by = "house") # First time doing this I left lat, lon, and census tract in from both datasets to make sure they match, and they do.
df_spei3_10yr_10th_sen$date <- NA


##### South Africa #####

soc_za <- read_csv("south_africa.csv")
env_spei3_10yr_10th_za <- read_csv("southafrica_SPEI-3_2005-2009_10percentile_1981-2009_means.csv")

# Reduce columns to avoid duplicates

soc_za <- soc_za %>%
  select(-district, -census_tract, -country, -lat, -long)

env_spei3_10yr_10th_za <- env_spei3_10yr_10th_za %>%
  select(house, district, municipality, country, lat, long, lat_SPEI, long_SPEI, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity, pop_cent_lat, pop_cent_long) %>%
  rename(census_tract = "municipality")


# soc_za is one row per migrant, env_spei... is one row per house. Left merge!

df_spei3_10yr_10th_za <- left_join(soc_za, env_spei3_10yr_10th_za, by = "house") # First time doing this I left lat, lon, and census tract in from both datasets to make sure they match, and they do.
df_spei3_10yr_10th_za$date <- NA


##### Uganda #####

soc_uga <- read_csv("uganda.csv")
env_spei3_10yr_10th_uga <- read_csv("uganda_SPEI-3_2005-2009_10percentile_1981-2009_means.csv")

# Reduce columns to avoid duplicates

soc_uga <- soc_uga %>%
  select(-district, -census_tract, -country, -lat, -long)

env_spei3_10yr_10th_uga <- env_spei3_10yr_10th_uga %>%
  select(house, district, census_tract, country, lat, long, lat_SPEI, long_SPEI, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity, pop_cent_lat, pop_cent_long)


# soc_uga is one row per migrant, env_spei... is one row per house. Left merge!

df_spei3_10yr_10th_uga <- left_join(soc_uga, env_spei3_10yr_10th_uga, by = "house") # First time doing this I left lat, lon, and census tract in from both datasets to make sure they match, and they do.


##### Merge #####

df_spei3_10yr_10th <- bind_rows(df_spei3_10yr_10th_bur, df_spei3_10yr_10th_ken, df_spei3_10yr_10th_ng, df_spei3_10yr_10th_sen, df_spei3_10yr_10th_za, df_spei3_10yr_10th_uga)

write_csv(df_spei3_10yr_10th, "spei3_yr10_perc.csv")

################# SPEI 12 #################

##### Burkina #####

### Main models: SPEI12, 5 year, 10th percentile

env_spei12_5yr_10th_bur <- read_csv("env_spei3_5yr_10th_burkinafaso_SPEI-12_2005-2009_10percentile_1981-2009_means.csv")

# Reduce columns to avoid duplicates

env_spei12_5yr_10th_bur <- env_spei12_5yr_10th_bur %>%
  select(house, district, census_tract, country, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity, pop_cent_lat, pop_cent_long)


# soc_bur is one row per migrant, env_spei... is one row per house. Left merge!

df_bur <- left_join(soc_bur, env_spei12_5yr_10th_bur, by = "house") # First time doing this I left lat, lon, and census tract in from both datasets to make sure they match, and they do.


##### Kenya #####

### Main models: SPEI12, 5 year, 10th percentile

soc_ken <- read_csv("env_spei3_5yr_10th_kenya.csv")
env_spei12_5yr_10th_ken <- read_csv("env_spei3_5yr_10th_kenya_SPEI-12_2005-2009_10percentile_1981-2009_means.csv")

# Reduce columns to avoid duplicates

soc_ken <- soc_ken %>%
  select(-district, -census_tract, -country, -lat, -long)

env_spei12_5yr_10th_ken <- env_spei12_5yr_10th_ken %>%
  select(house, district, census_tract, country, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity, pop_cent_lat, pop_cent_long)


# soc_ken is one row per migrant, env_spei... is one row per house. Left merge!

df_ken <- left_join(soc_ken, env_spei12_5yr_10th_ken, by = "house") # First time doing this I left lat, lon, and census tract in from both datasets to make sure they match, and they do.


##### Nigeria #####

### Main models: SPEI12, 5 year, 10th percentile

soc_ng <- read_csv("env_spei3_5yr_10th_nigeria.csv")
env_spei12_5yr_10th_ng <- read_csv("env_spei3_5yr_10th_nigeria_SPEI-12_2005-2009_10percentile_1981-2009_means.csv")

# Reduce columns to avoid duplicates

soc_ng <- soc_ng %>%
  select(-loc_meso, -loc_micro, -country, -lat, -long)

env_spei12_5yr_10th_ng <- env_spei12_5yr_10th_ng %>%
  select(house, district, census_tract, country, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity, pop_cent_lat, pop_cent_long)


# soc_ng is one row per migrant, env_spei... is one row per house. Left merge!

df_ng <- left_join(soc_ng, env_spei12_5yr_10th_ng, by = "house") # First time doing this I left lat, lon, and census tract in from both datasets to make sure they match, and they do.
df_ng$date <- NA


##### Senegal #####

### Main models: SPEI12, 5 year, 10th percentile

soc_sen <- read_csv("env_spei3_5yr_10th_senegal.csv")
env_spei12_5yr_10th_sen <- read_csv("env_spei3_5yr_10th_senegal_SPEI-12_2005-2009_10percentile_1981-2009_means.csv")

# Reduce columns to avoid duplicates

soc_sen <- soc_sen %>%
  select(-department, -census_tract, -country, -lat, -long)

env_spei12_5yr_10th_sen <- env_spei12_5yr_10th_sen %>%
  select(house, department, census_tract, country, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity, pop_cent_lat, pop_cent_long)


# soc_sen is one row per migrant, env_spei... is one row per house. Left merge!

df_sen <- left_join(soc_sen, env_spei12_5yr_10th_sen, by = "house") # First time doing this I left lat, lon, and census tract in from both datasets to make sure they match, and they do.
df_sen$date <- NA


##### South Africa #####

### Main models: SPEI12, 5 year, 10th percentile

soc_za <- read_csv("env_spei3_5yr_10th_south_africa.csv")
env_spei12_5yr_10th_za <- read_csv("env_spei3_5yr_10th_southafrica_SPEI-12_2005-2009_10percentile_1981-2009_means.csv")

# Reduce columns to avoid duplicates

soc_za <- soc_za %>%
  select(-district, -municipality, -country, -lat, -long)

env_spei12_5yr_10th_za <- env_spei12_5yr_10th_za %>%
  select(house, district, municipality, country, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity, pop_cent_lat, pop_cent_long)


# soc_za is one row per migrant, env_spei... is one row per house. Left merge!

df_za <- left_join(soc_za, env_spei12_5yr_10th_za, by = "house") # First time doing this I left lat, lon, and census tract in from both datasets to make sure they match, and they do.
df_za$date <- NA


##### Uganda #####

### Main models: SPEI12, 5 year, 10th percentile

soc_uga <- read_csv("env_spei3_5yr_10th_uganda.csv")
env_spei12_5yr_10th_uga <- read_csv("env_spei3_5yr_10th_uganda_SPEI-12_2005-2009_10percentile_1981-2009_means.csv")

# Reduce columns to avoid duplicates

soc_uga <- soc_uga %>%
  select(-district, -census_tract, -country, -lat, -long)

env_spei12_5yr_10th_uga <- env_spei12_5yr_10th_uga %>%
  select(house, district, census_tract, country, annual_frequency, severity, autocorrelation, avg_area, NDVI_mean, SVI_severity, pop_cent_lat, pop_cent_long)


# soc_uga is one row per migrant, env_spei... is one row per house. Left merge!

df_uga <- left_join(soc_uga, env_spei12_5yr_10th_uga, by = "house") # First time doing this I left lat, lon, and census tract in from both datasets to make sure they match, and they do.


##### Merge #####

# Rename everything to census_tract and district
df_sen <- df_sen %>%
  rename("district" = department)
df_za <- df_za %>%
  rename("census_tract" = municipality)
df_uga <- df_uga %>%
  rename("census_tract" = municipality)

# bind_rows
df <- bind_rows(df_bur, df_ken, df_ng, df_sen, df_za, df_uga)

write_csv(df, "spei12_yr5_perc.csv")

