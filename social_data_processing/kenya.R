
# When practices for construction DHS Wealth Index are referenced throughout, this is the citation:
# Rutstein, S. O. (2015). Steps to constructing the new DHS Wealth Index. Rockville, MD: ICF International, 6.


library(foreign) # Bring in .dta data
library(tidyverse) # Various functions useful for data processing
library(data.table) # More handy for going long to wide on a single factor
library(mice) # Imputing missing data

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set working directory to location of this file (script file)

##### Download data #####
# Download from https://microdata.worldbank.org/index.php/catalog/94


##### Import data #####

k0 <- as_tibble(read.dta("Household.dta"))
k1 <- as_tibble(read.dta("section1.dta"))
k5 <- as_tibble(read.dta("section5.dta"))
k6 <- as_tibble(read.dta("section6.dta"))


##### Subset #####

k0a <- k0 %>%
  select(qno, district, cunit, hhmnum, date1, q2_3, q2_4, q2_5a, q2_6, q2_7, q3_1_1, q3_1_3, q3_1_5, q3_1_6, q3_1_7, q3_1_8, q3_1_9, q3_1_12, q3_1_13, q3_1_14, q3_1_15, q3_1_17, q3_1_18) %>% #Select household, district, census unit, walls (2_3), separate cooking room (2_4), total count of dining sleeping and living rooms in main house (2_5a), electricity (2_6), water source (2_7), ag land (3_1_1), own house (3_1_3), q3_1_5 (tuk tuk (vehicle)), q3_1_6 (radio), q3_1_7 (tv), q3_1_8 (fridge), q3_1_9 (air conditioner), q3_1_12 (computer), q3_1_13 (mobile), q3_1_14 (other phone), q3_1_15 (bicycle), q3_1_17 (car), q3_1_18 (motorcycle). 
  #Things in DHS that are not present here: no cooking fuel type, toilet, trash disposal, floor material, roof material, livestock, separate bedrooms. 

  mutate_at(vars(district, cunit), ~ str_to_lower(.))

k5a <- k5 %>%
  select(qno, q5_19, q5_23, q5_8_11, q5_9c) %>% # These questions were posed to households with one or more household members that are currently elsewhere, whether in Kenya or abroad. 5_19: money transfers in last 12 mos. 5_23: 0/1 send or bring food to HH in last 12 months. 5_8_11: local vs foreign current residence. 5_9c: total months in current location.
# In case needed in the future, note that for Kenya specifically, looks like there is locale -- at least coarse-grained in country (like Naivasha, Kisuyu) and rough international (Texas, Finland).
  mutate(migrant_hh = "hh")

k6a <- k6 %>%
  select(qno, q6_2, q6_6, q6_11) %>% # Where non-household migrant lives (6_2), send money x times (6_6), send goods/food (6_11).
  mutate(migrant_hh = "non-hh")

### Some processing I did for e.g. Burkina is not needed for Kenya because researchers already did it, including counting number of household members.

##### Process migrant roster into a count of household migrants #####

k5b <- k5a %>%
  group_by(qno) %>%
  count(qno) %>%
  rename(migrant_num = n)

##### Combine just household-level data for now ####

ken1a <- full_join(k0a, k5b, by = "qno") # Keep all rows using full_join.

ken1a <- ken1a %>%
  group_by(qno) %>% # Group by household
  rename(house = qno,
         wall = q2_3, rm_cook = q2_4, own_land = q3_1_1, own_house = q3_1_3, rm_count = q2_5a, electric = q2_6, water = q2_7, radio = q3_1_6, tv = q3_1_7, fridge = q3_1_8, auto = q3_1_17, moto = q3_1_18, tele_mobile = q3_1_13, tele_land = q3_1_14, tuk_tuk = q3_1_5, air_cond = q3_1_9, computer = q3_1_12, bicycle = q3_1_15, date = date1, hh_size = hhmnum)


### Pause! Impute NAs before processing further ###

# Data show signs of being missing at random

# Tidy up a smidge to make number codes factors, remove unused levels, etc.

ken1a$wall[ken1a$wall == "other (specify"] <- NA # so few people have other for walls, recoding to "NA" and then imputing (9 out of 1942)

ken1a$rm_cook[ken1a$rm_cook == 3] <- NA # this is a blank level in the WB database and only has three observations, so recoding to NA and imputing

ken1a$migrant_num[is.na(ken1a$migrant_num)] <- 0 # *everyone* who is NA here is a zero; see above calculation of migrant number (only for households with one or more), then merging keeping all data -- thus the 672 NAs

# Date issues
# One year was marked 1991 -- fix to be same year as other years in this tract
ken1a$date[(which(ken1a$date == "1991-12-13"))] <- "2009-12-13"

# One year was marked 2008 but with same day and month as other households in the census tract; fixing year
ken1a$date[(which(ken1a$date == "2008-12-10"))] <- "2009-12-10"

# One month was marked 02 but with same day and year as other households in the census tract; fixing month
ken1a$date[(which(ken1a$date == "2009-02-14"))] <- "2009-12-14"

# One month was marked 09 - looks to be 11, as day is 11 and many interviews took place on 11-11 but not 11-09
ken1a$date[(which(ken1a$date == "2009-09-11"))] <- "2009-11-11"


ken1a <- ken1a[!is.na(ken1a$tuk_tuk), ] # 13 houses are missing the owned items entirely, leaving few variables on which I could impute, even though evidence suggests they're missing at random (e.g., with respect to interview date and location). Ultimately I opted to remove them, rather than using four variables to impute 13 variables (for 13 households).

ken1a <- droplevels(ken1a)

# Tell R which variables (columns) to use in imputation for which variables (rows)

predMat_k <- matrix(rep(0, ncol(ken1a)^2), ncol = ncol(ken1a), nrow = ncol(ken1a))
rownames(predMat_k) <- colnames(ken1a)
colnames(predMat_k) <- colnames(ken1a)
predMat_k <- data.frame(predMat_k)

predMat_k[colnames(predMat_k) %in% c("hh_size", "wall", "rm_cook", "rm_count", "electric", "water", "moto"), colnames(predMat_k) %in% c("own_land", "own_house", "tuk_tuk", "radio", "tv", "fridge","air_cond", "computer", "tele_mobile", "tele_land", "bicycle", "auto")] <- 1 # Variables in front of the comma need to be imputed. Variables after the comma are being used for imputation. "Each row [in the predictor matrix] corresponds to a variable block, i.e., a set of variables to be imputed. A value of 1 means that the column variable is used as a predictor for the target block (in the rows)." 

# Tell mice to use predictive mean matching where it's imputing; all other slots in this vector are empty.
these_k <- rep("", length(ken1a))
these_k[which(colnames(ken1a) %in% row.names(predMat_k)[rowSums(predMat_k) > 0])] <-"pmm" 

# Run imputation (and set seed for replicability; if you don't set a seed, R picks a random one each time and the imputed values move around)

ken1b <- complete (mice (ken1a, method = these_k, predictorMatrix = as.matrix(predMat_k), seed = 17000731, print = FALSE)) # Seed chosen randomly on June 14 2024. PMM means predictive mean matching; DHS protocol uses mean assignment for the DHS wealth index. Complete returns the data set (saved as dat1) with missing values populated with predicted values.

ken1b <- droplevels(ken1b) # Dropping levels to improve behavior of factors

### Recode wealth data ###

ken1b$ppl_room <- ken1b$hh_size/ken1b$rm_count # number of people per room. Number of people per sleeping room is preferred; this is second choice when sleeping room data are not available. Produces 24 NAs as expected, as those data are missing in the original variables.

ken1b$own_land <- case_when(
  ken1b$own_land == "yes" ~ 1,
  ken1b$own_land == "no" ~ 0
)

ken1b$own_house <- case_when(
  ken1b$own_house == "yes" ~ 1,
  ken1b$own_house == "no" ~ 0
)

ken1b$wall <- case_when(
  ken1b$wall == "bricks/stones" ~ "non-natural",
  ken1b$wall == "pre-fabricated" ~ "non-natural",
  ken1b$wall == "wood/off cuts" ~ "natural",
  ken1b$wall == "mud" ~ "natural",
  ken1b$wall == "tin" ~ "non-natural", 
  ken1b$wall == "iron sheets" ~ "non-natural"
) #natural materials are one category according to DHS protocol

ken1b$wall <- as_factor(ken1b$wall)

setDT(ken1b)[, c(paste0("wall_",levels(ken1b$wall)), "wall") := 
               c(lapply(levels(wall), function(x) as.integer(x == wall)), .(NULL))] # walls are binary for each case

ken1b$water <- case_when(
  ken1b$water == "piped water to resident" ~ "piped",
  ken1b$water == "rain water" ~ "surface",
  ken1b$water == "river or stream" ~ "surface",
  ken1b$water == "well \\ stream" ~ "surface",
  ken1b$water == "boreholes" ~ "public", #based on reports of NGOs, these are very likely to be public wells
  ken1b$water == "public pump" ~ "public",
  ken1b$water == "water truck \\ vendors" ~ "public",
  ken1b$water == "spring" ~ "surface"
) #natural materials are one category

ken1b$water <- as_factor(ken1b$water)

setDT(ken1b)[, c(paste0("water_",levels(ken1b$water)), "water") := 
               c(lapply(levels(water), function(x) as.integer(x == water)), .(NULL))] #water is binary for each source, with surface water grouped

### Following ones are simple presence-absence, requiring less manipulation than the above

ken1b$tv <- case_when(
  ken1b$tv == "yes" ~ 1,
  ken1b$tv == "no" ~ 0
)

ken1b$fridge <- case_when(
  ken1b$fridge == "yes" ~ 1,
  ken1b$fridge == "no" ~ 0
)

ken1b$tele <- case_when(
      ken1b$tele_mobile == "yes" | ken1b$tele_land == "yes" ~ 1,
      .default = 0
    ) # according to standard DHS practice, landline and mobile both count

ken1b <- ken1b %>%
  mutate(vehicle = case_when(
  auto == "yes" ~ 1,
  tuk_tuk == "yes" ~ 1,
  moto == "yes" ~ 1,
  .default = 0
)
) #Because there were few yeses for each vehicle type, I combined them to capture more variation with future variables. This also matches the use cases of tuk tuks in coastal places, for moving people.

ken1b$air_cond <- case_when(
  ken1b$air_cond == "yes" ~ 1,
  ken1b$air_cond == "no" ~ 0
)

ken1b$computer <- case_when(
  ken1b$computer == "yes" ~ 1,
  ken1b$computer == "no" ~ 0
)

ken1b$bicycle <- case_when(
  ken1b$bicycle == "yes" ~ 1,
  ken1b$bicycle == "no" ~ 0
)

ken1b$rm_cook <- case_when(
  ken1b$rm_cook == "1" ~ 1,
  ken1b$rm_cook == "2" ~ 0
)

ken1b$electric <- case_when(
  ken1b$electric == "yes" ~ 1,
  ken1b$electric == "no" ~ 0
)

ken1b$radio <- case_when(
  ken1b$radio == "yes" ~ 1,
  ken1b$radio == "no" ~ 0
)

##### PCA for wealth #####

#This is standard practice for the DHS wealth index

pca_k <- ken1b %>%
  as_tibble() %>%
select(own_land, own_house, starts_with("water_"), starts_with("wall_"), ppl_room, rm_cook, tv, fridge, vehicle, electric, computer, tele) %>%
  prcomp(scale = TRUE) # scale scales to 0-1 since not all variables are binary. using singular value decomposition because I have a non-symmetric matrix
#Considered tractor, bus, lorry but few households had one.
#Items with low loading on PC1 that were removed, in order of removal: bicycle, radio, air_cond.
#Got to 30% loading with these removals but couldn't remove more because standard DHS.

# DHS protocol is to extract first component loading for each household

ken1b$wealth_index <- pca_k$x[ ,1] # household-level score on first principal component. Explains 31.5% of the variance in wealth.


##### Now process individual-level data for money received #####

### Recode household remittance data ###

k5a <- droplevels(k5a) # Dropping levels to improve behavior of factors

k5a <- k5a %>%
  rename(hh_money = q5_19, hh_goods = q5_23)

k5a$hh_goods[k5a$hh_goods == 6 | k5a$hh_goods == 0] <- NA # The levels are just 1 and 2 (see below); these are data entry errors

k5a$hh_money <- case_when( # Data are continuous (number of times money sent) but very sparse (few received money more than 3 times) and don't specify amounts (amount could be huge and only sent once); given these uncertainties, we code all to presence/absence.
  k5a$hh_money > 0 ~ 1,
  k5a$hh_money == 0 ~ 0) # There are no zeroes but there would be if there were any, according to documentation

k5a$hh_goods <- case_when(
  k5a$hh_goods == 1 ~ 1,
  k5a$hh_goods == 2 ~ 0
)

k5a$hh_remit <- case_when(
  k5a$hh_goods == 1 | k5a$hh_money == 1 ~ 1,
  k5a$hh_goods == 0 & k5a$hh_money == 0 ~ 0 # True zeroes are only those that have zeroes for both columns; none here
)

k5a <- k5a %>%
  filter(!is.na(hh_remit) | !is.na(q5_8_11) | !is.na(q5_9c), na.rm = TRUE) # remove rows where we have no location, no length of time in the place, or no remittance -- these appear to be errors.

### Received a remittance from non-household member ###

k6a <- droplevels(k6a) 

k6a$nonhh_remit <- case_when(
  !is.na(k6a$q6_6) | k6a$q6_11 == "yes" ~ 1, # zeroes are real in q6_6 (money) and there are none: all numeric values are transfers
  k6a$q6_6 == 0 & k6a$q6_11 == "no" ~ 0 # No zeroes for nonhh_remit because there are no q6_6 zeroes
)

k6b <- k6a %>%
  group_by(qno) %>% # need to group first to use keep in mutate
  filter(!is.na(nonhh_remit) | !is.na(q6_2), na.rm = TRUE) %>% # remove rows where there was no location for non-hh migrant or no remittance coming in -- these appear to be added in error
  select(-q6_6, -q6_11) # Remove unused columns

k56 <- bind_rows(k5a, k6b) # Combine rows from households with HH members elsewhere and HHs with social-network members elsewhere


k56 <- k56 %>%
  rename(house = qno) # hh variables are for current HH members who are elsewhere; nothh variables are for non-HH members who are elsewhere

##### Migrant characteristics #####

# This is in preparation for exploratory models on whether households are investing in migration to get remittances vs not #
# A separate set of exploratory models will investigate whether households invested in migration during the time periods over which we're measuring shocks (thus the months for hh migrants; we don't have time for non-hh migrants) #

### Code migrant locations as internal vs external ###

# First, recode numeric index to string

k56$q6_2 <- case_when(
  k56$q6_2 == 1 ~ "urban", # Could just recode straight to "national" here but following same pattern as Burkina and other countries
  k56$q6_2 == 2 ~ "rural",
  k56$q6_2 > 2 & k56$q6_2 < 20 ~ "international",
  k56$q6_2 > 20 ~ NA
)

# Now new variables

k56 <- k56 %>%
  mutate(hh_loc = if_else(q5_8_11 == "local", "national", "international"), nonhh_loc = if_else(q6_2 == "urban" | q6_2 == "rural", "national", "international"), .keep = "unused")


### Code how many months a household migrant has been in current location ###
# Not needed for Kenya; researchers already provided it (and I cross-checked for accuracy with years and months)

k56 <- k56 %>%
  rename(hh_months = q5_9c)

### Combine columns and remove unused ones ###

k56$remit <- case_when(
  k56$hh_remit == 1 & is.na(k56$nonhh_remit) ~ 1,
  k56$nonhh_remit == 1 & is.na(k56$hh_remit) ~ 1,
  k56$nonhh_remit == 0 & k56$hh_remit == 0 ~ 0
) # This is cleanest, to avoid overwriting any data... the below (unite) can be less clean but checked for potential overwriting before doing this with k56[!is.na(k56$hh_loc) & !is.na(k56$nonhh_loc),]

k56 <- k56 %>% unite("migrant_loc", hh_loc, nonhh_loc, na.rm = TRUE)

# Remove unused columns

k56 <- k56 %>%
  select(-hh_money, -hh_goods, -hh_remit, -nonhh_remit)


##### Make binary for whether any remittances received period, whether from hh vs non-hh migrants #####

k56 <- k56 %>%
  group_by(house) %>%
  mutate(any_remit = if_else(any(remit == 1), 1, 0))


##### Recombining -- there's definitely a row for every house, even if they had no migrants (and from those migrants, no remittances) #####

ken1c <- left_join(ken1b, k56, by = "house")

# NAs for remit are informative: these are households that didn't receive food or money, from same household or different household. NA is zero.


##### Merge with location data #####

ken1c$country <- "kenya" # Country name for later merge

ken1c <- ken1c %>%
  select(house, district, cunit, country, date, date, wealth_index, hh_size, migrant_num, any_remit, migrant_hh, migrant_loc, hh_months, remit) %>% # Simplify and reduce participant identifiability by reducing columns available
  rename(loc_meso = district, loc_micro = cunit) # For merging

loc <- read_csv("kenya locations_micro.csv") # Authors AP and HJ coded the lat/long of the provincial capital for each province; import those data to merge with data on household remittances

loc <- mutate(loc, landmark = NULL, notes = NULL) # Remove AP's notes about how she got lat/long for each

ken1d <- left_join(ken1c, loc, by = c("loc_micro" = "location")) # Merge province location with household data

ken1d <- ken1d %>%
  rename(district = loc_meso, census_tract = loc_micro)

##### Output #####

write_csv(ken1d, "kenya.csv")
