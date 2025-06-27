
# When practices for construction DHS Wealth Index are referenced throughout, this is the citation:
# Rutstein, S. O. (2015). Steps to constructing the new DHS Wealth Index. Rockville, MD: ICF International, 6.


library(foreign) # Bring in .dta data
library(tidyverse) # Various functions useful for data processing
library(data.table) # More handy for going long to wide on a single factor
library(mice) # Imputing missing data

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set working directory to location of this file (script file)

##### Download data #####
# Download from https://microdata.worldbank.org/index.php/catalog/95


##### Import data #####

u0 <- as_tibble(read.dta("Uganda Sections 2 and 3 Household_21_03_2011.dta"))
u1 <- as_tibble(read.dta("Uganda Sections 1 and 4 Household_Members_21_03_2011.dta"))
u5 <- as_tibble(read.dta("Uganda Section 5 Household Member Migrants_21_03_2011.dta"))
u6 <- as_tibble(read.dta("Uganda Section 6 Non Household Member Migrants_21_03_2011.dta"))

##### Subset #####

u0a <- u0 %>% 
  select(qnaireno, earea, parish, district, intdate, #qnaireno = HH ID, earea = enumeration area, intdate = interview date
        q223, q224, q225, q226, q227, q31a1i, q31a3i, q31b2m, q31b3m, q31b4m, q31b9m, q31b10m) %>% 
  # q223 = walls, q224 = cooking room, q225 = total number rooms, q226 = electric, q227 = water, q31a1i = own ag land, q31a3i = own house, q31b2m = radio, q31b3m = tv, q31b4m = fridge, q31b9m = mobile phone, q31b10m = landline
  
  # Considered owning tractorharvestor, motorcycle, car truck, air cond, computer, ox cart but variability was quite low
  # Things in DHS that are not present here: no cooking fuel type, toilet, trash disposal, floor material, roof material, livestock, separate bedrooms.

  mutate_at(vars(earea, parish, district), ~ str_to_lower(.)) %>% # Already unique by household; didn't need to apply unique requirement.
  rename(
    "date" = `intdate`,
    "wall" = `q223`,
    "rm_cook" = `q224`,
    "rm_count" = `q225`,
    "electric" = `q226`,
    "water" = `q227`,
    "own_land" = `q31a1i`,
    "own_house" = `q31a3i`,
    "radio" = `q31b2m`,
    "tv" = `q31b3m`,
    "fridge" = `q31b4m`,
    "tele_mobile" = `q31b9m`,
    "tele_trad" = `q31b10m`)

u1a <- u1 %>%
  select(qnaireno, per1id) # List of everyone who *currently* lives in the house. Numqest is unique across the dataset so don't need census for merging

u5a <- u5 %>%
  select(qnaireno, q58, q59yr, q59mn, q520_1, q523_1) %>% # 58 = where, 59 = how long
  rename(
    "hh_loc" = `q58`,
    "hh_years" = `q59yr`,
    "hh_month" = `q59mn`,
   "hh_money" = `q520_1`,
   "hh_goods" = `q523_1`
  ) %>%
  mutate(migrant_hh = "hh")

u6a <- u6 %>%
  select(qnaireno, q62, q66, q6_11) %>%
  rename(
    "nonhh_loc" = `q62`,
    "non_hh_money" = `q66`,
    "non_hh_goods" = `q6_11`
  ) %>%
  mutate(migrant_hh = "non-hh")


##### Process household roster into count of individuals #####

# Researchers provide hh size in u0 but I've been calculating for other countries so will do the same here. Good news! I get the same numbers the researchers provide in u0.

u1b <- u1a %>%
  group_by(qnaireno) %>%
  count(qnaireno) %>%
  rename(hh_size = n)


##### Process migrant roster into a count of household migrants #####

u5b <- u5a %>%
  group_by(qnaireno) %>%
  count(qnaireno) %>%
  rename(migrant_num = n)


##### Combine just household-level data for now ####

ug1a <- full_join(u0a, u1b, by = c("qnaireno")) # Combine basic household data, keeping all rows.
ug1b <- full_join(ug1a, u5b, by = c("qnaireno"))


### Pause! Impute NAs before processing further ###

# Tidy up a smidge to make number codes factors, remove unused levels, etc.

ug1b$migrant_num[is.na(ug1b$migrant_num)] <- 0 # *everyone* who is NA here is a zero; see above calculation of migrant number (only for households with one or more), then merging keeping all data

ug1b$wall[ug1b$wall == "Other (specify)"] <- NA # so few people have other for walls, recoding to "NA" and then imputing (11 out of 1872)

ug1b$water[ug1b$water == "Other"] <- NA # Only 4% have "other" so imputing (82 out of 1872)

# Date issues

ug1b$date[ug1b$date == "2000-01-19"] <- "2010-01-19"
ug1b$date[ug1b$date == "2001-03-20"] <- NA # Too many likely errors in data entry on this date; I'll impute at analysis stage
ug1b$date[ug1b$date == "2018-03-16"] <- NA # Too many likely errors in data entry on this date; I'll impute at analysis stage
ug1b$date[ug1b$date == "2020-02-16"] <- "2010-02-16"
ug1b$date[ug1b$date == "2020-02-19"] <- "2010-02-19"
ug1b$date[ug1b$date == "2201-02-13"] <- "2010-02-13" 


# For these columns, if they're missing these pairs, they seem to be missing most for wealth, probably from data collection errors - remove.

ug1b <- ug1b[!(is.na(ug1b$tele_trad) & is.na(ug1b$tv)),]

ug1b <- droplevels(ug1b)


# Unlike other countries, amazing data completeness and not having to remove folks with missing data - thanks Senegal!


# Tell R which variables (columns) to use in imputation for which variables (rows)

predMat_n <- matrix(rep(0, ncol(ug1b)^2), ncol = ncol(ug1b), nrow = ncol(ug1b))
rownames(predMat_n) <- colnames(ug1b)
colnames(predMat_n) <- colnames(ug1b)
predMat_n <- data.frame(predMat_n)

predMat_n[colnames(ug1b) %in% c("wall", "water", "own_land", "own_house", "tele_trad"), colnames(ug1b) %in% c("rm_cook", "rm_count", "electric", "radio", "tv", "fridge", "tele_mobile")] <- 1 # Variables in front of the comma need to be imputed. Variables after the comma are being used for imputation. "Each row [in the predictor matrix] corresponds to a variable block, i.e., a set of variables to be imputed. A value of 1 means that the column variable is used as a predictor for the target block (in the rows)."

# Tell mice to use predictive mean matching where it's imputing; all other slots in this vector are empty.
these_n <- rep("", length(ug1b))
these_n[which(colnames(ug1b) %in% row.names(predMat_n)[rowSums(predMat_n) > 0])] <-"pmm" 

# Run imputation (and set seed for replicability; if you don't set a seed, R picks a random one each time and the imputed values move around)

ug1c <- complete (mice (ug1b, method = these_n, predictorMatrix = as.matrix(predMat_n), seed = 17000731, print = FALSE)) # Seed chosen randomly on June 14 2024. PMM means predictive mean matching; DHS protocol uses mean assignment for the DHS wealth index. Complete returns the data set (saved as dat1) with missing values populated with predicted values.

ug1c <- droplevels(ug1c) # Dropping levels to improve behavior of factors

### Recode wealth data ###

ug1c$ppl_room <- ifelse(ug1c$rm_count == 0, 0, ug1c$hh_size / ug1c$rm_count) # number of people per room. Number of people per sleeping room is preferred; this is second choice when sleeping room data are not available. In Ugandan data, 37 households have 0 rooms and no explanation is given -- this might be real data, so keeping and making this 0 for ppl_room to avoid infinite numbers.

ug1c$own_land <- case_when(
  ug1c$own_land == "Yes" ~ 1,
  ug1c$own_land == "No" ~ 0
)

ug1c$own_house <- case_when(
  ug1c$own_house == "Yes" ~ 1,
  ug1c$own_house == "No" ~ 0
)

ug1c$wall <- case_when(
  ug1c$wall == "Bricks / Stones" ~ "non_natural",
  ug1c$wall == "Straw" ~ "natural",
  ug1c$wall == "Wood" ~ "natural",
  ug1c$wall == "Mud" ~ "natural",
  ug1c$wall == "Pre-fabricated" ~ "non_natural",
  ug1c$wall == "eternittin" ~ "non_natural"
) #natural materials are one category according to DHS protocol

ug1c$wall <- as_factor(ug1c$wall)

setDT(ug1c)[, c(paste0("wall_",levels(ug1c$wall)), "wall") := 
               c(lapply(levels(wall), function(x) as.integer(x == wall)), .(NULL))] # walls are binary for each case

ug1c$water <- case_when(
  ug1c$water == "Private faucet or tap" ~ "private",
  ug1c$water == "Public pump" ~ "public",
  ug1c$water == "Rain water" ~ "private",
  ug1c$water == "Well" ~ "public", # Unclear whether private or public, but it's usually public
  ug1c$water == "Water truck" ~ "public", # I thought about this one for a while: depends on the research question. Yes, paying for water. Yes, might be cleaner than others. Still suggests less access to infrastructure though. I made the same decision for vending for other countries too.
  ug1c$water == "River or steam" ~ "surface"
)

ug1c$water <- as_factor(ug1c$water)

setDT(ug1c)[, c(paste0("water_",levels(ug1c$water)), "water") := 
               c(lapply(levels(water), function(x) as.integer(x == water)), .(NULL))]


### Following ones are simple presence-absence, requiring less manipulation than the above

ug1c$tv <- case_when(
  ug1c$tv == "Yes" ~ 1,
  ug1c$tv == "No" ~ 0
)

ug1c$fridge <- case_when(
  ug1c$fridge == "Yes" ~ 1,
  ug1c$fridge == "No" ~ 0
)

ug1c$tele <- case_when(
      ug1c$tele_mobile == "Yes" | ug1c$tele_trad == "Yes" ~ 1,
      .default = 0
    ) # according to standard DHS practice, landline and mobile both count

ug1c$rm_cook <- case_when(
  ug1c$rm_cook == "Yes" ~ 1,
  ug1c$rm_cook == "No" ~ 0
)

ug1c$electric <- case_when(
  ug1c$electric == "Yes" ~ 1,
  ug1c$electric == "No" ~ 0
)

ug1c$radio <- case_when(
  ug1c$radio == "Yes" ~ 1,
  ug1c$radio == "No" ~ 0
)


##### PCA for wealth #####

# This is standard practice for the DHS wealth index
# Because just two levels each for wall and water, just name one variable in PCA

pca_n <- ug1c %>%
  as_tibble() %>%
select(own_land, own_house, water_public, water_surface, wall_natural, ppl_room, rm_cook, tv, fridge, electric, tele, radio) %>%
  prcomp(scale = TRUE) # scale scales to 0-1 since not all variables are binary. using singular value decomposition because I have a non-symmetric matrix.


# DHS protocol is to extract first component loading for each household

ug1c$wealth_index <- pca_n$x[ ,1] # household-level score on first principal component. Explains 31.1% of the variance in wealth.


##### Now process individual-level data for money received #####

## Recode household remittance data ###

u5a <- droplevels(u5a) # Dropping levels to improve behavior of factors

u5a$hh_money <- case_when(
  u5a$hh_money > 0 ~ 1,
  u5a$hh_money == 0 ~ 0) # Zeroes are meaningful and there aren't any

u5a$hh_goods <- case_when(
  u5a$hh_goods == "Yes" ~ 1,
  u5a$hh_goods == "No" ~ 0)

u5a$hh_remit <- case_when(
  u5a$hh_goods == 1 | u5a$hh_money == 1 ~ 1,
  u5a$hh_goods == 0 & u5a$hh_money == 0 ~ 0 # True zeroes are only those that have zeros for both columns
)

u5a <- u5a %>%
  filter(!is.na(hh_remit) | !is.na(hh_loc) | !is.na(hh_years) | !is.na(hh_month), na.rm = TRUE) # remove rows where we have no location, no length of time in the place, or no remittance -- these appear to be errors.

### Received a remittance from non-household member ###

u6a$nonhh_remit <- case_when(
  u6a$non_hh_money == "Yes" | u6a$non_hh_goods == "Yes" ~ 1,
  u6a$non_hh_money == "No" & u6a$non_hh_goods == "No" ~ 0
)

u6b <- u6a %>%
  group_by(qnaireno) %>% # need to group first to use keep in mutate
  filter(!is.na(nonhh_remit) | !is.na(nonhh_loc), na.rm = TRUE) %>% # remove rows where there was no location for non-hh migrant or no remittance coming in -- these appear to be added in error
  select(-non_hh_goods, -non_hh_money)

u56 <- bind_rows(u5a, u6b) # Combine rows from households with HH members elsewhere and HHs with social-network members elsewhere (that are receiving remittances)


##### Migrant characteristics #####

# This is in preparation for exploratory models on whether households are investing in migration to get remittances vs not #
# A separate set of exploratory models will investigate whether households invested in migration during the time periods over which we're measuring shocks (thus the months for hh migrants; we don't have time for non-hh migrants) #

### Code migrant locations as internal vs external ###

u56 <- u56 %>%
  mutate(hh_loc = if_else(hh_loc == "Urban uganda" | hh_loc == "Rural uganda", "national", "international"), nonhh_loc = if_else(nonhh_loc == "Urban uganda" | nonhh_loc == "Rural uganda", "national", "international"), .keep = "unused")

### Code how many months a household migrant has been in current location ###

# Recode empty months that are paired with non-empty years and vice versa
u56$hh_month <- if_else(!is.na(u56$hh_years) & is.na(u56$hh_month), 0, u56$hh_month)
u56$hh_years <- if_else(!is.na(u56$hh_month) & is.na(u56$hh_years), 0, u56$hh_years)

u56 <- u56 %>%
  rowwise() %>%
  mutate(hh_months = (hh_years * 12) + hh_month, .keep = "all")

### Combine columns and remove unused ones ###

u56$remit <- case_when(
  u56$hh_remit == 1 & is.na(u56$nonhh_remit) ~ 1,
  u56$nonhh_remit == 1 & is.na(u56$hh_remit) ~ 1,
  u56$nonhh_remit == 0 & u56$hh_remit == 0 ~ 0
) # This is cleanest, to avoid overwriting any data... the below (unite) can be less clean but checked for potential overwriting before doing this with u56[!is.na(u56$hh_loc) & !is.na(u56$nonhh_loc),]

u56 <- u56 %>% unite("migrant_loc", hh_loc, nonhh_loc, na.rm = TRUE)

# Remove unused columns

u56 <- u56 %>%
  select(-hh_money, -hh_goods, -hh_remit, -nonhh_remit, -hh_years, -hh_month)


##### Make binary for whether any remittances received period, whether from hh vs non-hh migrants #####

u56 <- u56 %>%
  group_by(qnaireno) %>%
  mutate(any_remit = if_else(any(remit == 1), 1, 0))

##### Recombining -- there's definitely a row for every house, even if they had no migrants (and from those migrants, no remittances) #####

ug1d <- left_join(ug1c, u56, by = c("qnaireno"))

# NAs for remit are informative: these are households that didn't receive food or money, from same household or different household. NA is zero.


##### Merge with location data #####

ug1d$country <- "uganda" # Country name for later merge

ug1d <- ug1d %>%
  select(qnaireno, earea, district, country, date, wealth_index, hh_size, migrant_num, any_remit, migrant_hh, migrant_loc, hh_months, remit) %>% # Simplify and reduce participant identifiability
  rename(house = qnaireno, loc_meso = district, loc_micro = earea) %>% # For merging. Department is meso in Senegal - region is bigger
  mutate(across(everything(), ~ str_squish(.)))

loc <- read_csv("uganda locations_micro.csv") # Authors AP and HJ coded the lat/long of the provincial capital for each province; import those data to merge with data on household remittances

loc <- loc %>%
  distinct(location, parish, district, .keep_all = TRUE) %>%
  mutate(landmark = NULL, notes = NULL, parish = NULL, district = NULL)  %>% # Remove notes about how we got lat/long for each; option to keep or remove parish and district here for more accurate merging purposes (I found it unnecessary and removed)
  mutate(across(everything(), ~ str_squish(.)))

ug1e <- left_join(ug1d, loc, by = c("loc_micro" = "location")) 

ug1e <- ug1e %>%
  rename(district = loc_meso, census_tract = loc_micro) %>% # Renaming to match across countries
  mutate(parish = NULL)

##### Output #####

write_csv(ug1e, "uganda.csv")
