# When practices for construction DHS Wealth Index are referenced throughout, this is the citation:
# Rutstein, S. O. (2015). Steps to constructing the new DHS Wealth Index. Rockville, MD: ICF International, 6.


library(foreign) # Bring in .dta data
library(tidyverse) # Various functions useful for data processing
library(data.table) # More handy for going long to wide on a single factor
library(mice) # Imputing missing data

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set working directory to location of this file (script file)

##### Download data #####
# Download from https://microdata.worldbank.org/index.php/catalog/96


##### Import data #####

z0 <- as_tibble(read.dta("wbsasection 1-5 & 9.dta"))

z5 <- as_tibble(read.dta("wbsasection 6.dta")) # This looks weird I know, saving a file named 6 into an object named 5, but this matches the naming system other countries had, where household members are 5.
z6 <- as_tibble(read.dta("wbsasection 7.dta"))

## Match enumeration area to geo location I can find on Google Maps (as I can't find key with enumeration lat/long)
# Most do not include main place (see details on enumeration areas below), so match to province + municipality/district (first 3 digits) instead (although more fine-grained ones are available here: https://www.statssa.gov.za/?page_id=4503)

ea_codes <- read_csv("Enumeration area key.csv", show_col_types = FALSE) %>%
  select(MP_CODE, MN_NAME, DC_NAME) %>%
  rename(ea = MP_CODE, loc_micro = MN_NAME, loc_meso = DC_NAME) # For some weird reason, running these lines first and then running the whole thing (below) works, otherwise it throws an error. Switched from province to district municipality name on 9-9-2024 for comparability with other locations.

ea_codes <- read_csv("Enumeration area key.csv", show_col_types = FALSE) %>%
  select(MP_CODE, MN_NAME, DC_NAME) %>%
  rename(ea = MP_CODE, loc_micro = MN_NAME, loc_meso = DC_NAME) %>% 
  mutate(ea = as.numeric(str_sub(ea_codes$ea, 1, 3))) %>%
  mutate_at(vars(loc_micro, loc_meso), ~ str_to_lower(.)) %>%
  distinct(ea, .keep_all = T)


##### Subset #####

z0a <- z0 %>%
  select(id, ea, 
         Q2_4, Q2_5, Q3_2_3, Q3_2_8, Q3_2_9, Q3_2_10, Q3_2_13, Q3_2_14, Q3_2_15, Q3_2_16, Q3_2_17, Q3_2_19) %>% 
  #Select household id (unique to ea not dataset), enumeration area, separate cooking room (Q2_4), total number of separate rooms (seems to include kitchen but excludes bathrooms and storage; Q2_5), own house (Q3_2_3), fridge (Q3_2_8), air cond (Q3_2_9), tv (Q3_2_10), computer (Q3_2_13), radio (Q3_2_14), mobile phone (Q3_2_15), nonmobile phone (Q3_2_16), car or truck (Q3_2_17), bicycle (Q3_2_19)
  
  #Things in DHS that are not present here: no cooking fuel type, toilet, trash disposal, floor material, roof material, livestock, separate bedrooms.
  
  # I didn't do this with other countries but this is such an urban context that own_land was SO low here and natural walls SO rare and both did so poorly on PCA, I removed them. From the DHS Wealth Index creation guide: "select out those that do not apply to one or another area or are thought to indicate different levels of wealth."
  
  # Considered owning beds, tractorharvestor, moto but variability was quite low (and many NAs for moto). Also much to my frustration, had to eliminate water and electricity because so few na's were marked as such, it was unclear how many households didn't have these things. At best, if those really were the only real na's (which I doubt, there were HUNDREDs of blank rows), then few people are without these things.
  
  # UPDATE: Dropped variables that had low loadings on first PC: moto, water_public, water_piped.
  
  # Researchers did not include a date variable. All data colection took place between Nov 13 and Dec 23 2009. Interviews took place simultaneously across regions by multiple teams, so interviewer ID doesn't help
  
  # Yes, each row is a person, so household data is duplicated for each person in that house. See below for where we go unique.
  
  rename(
    "rm_cook" = `Q2_4`,
    "rm_count" = `Q2_5`,
    "own_house" = `Q3_2_3`,
    "fridge" = `Q3_2_8`,
    "air_cond" = `Q3_2_9`,
    "tv" = `Q3_2_10`,
    "computer" = `Q3_2_13`,
    "radio" = `Q3_2_14`,
    "tele_mobile" = `Q3_2_15`,
    "tele_trad" = `Q3_2_16`,
    "auto" = `Q3_2_17`,
    "bicycle" = `Q3_2_19`) %>%
  mutate(ea = as.numeric(str_sub(z0$ea, 1, 3)))

z1a <- z0 %>%
  select(id, ea, persnum) %>% # List of every who *currently lives* in the household, according to survey instrument, *but* on metadata note that it says that z5 data are for Former Household Members, suggesting interviewers are differentiating: migrants are not current household members.
  mutate(ea = as.numeric(str_sub(ea, 1, 3)))

z5a<- z5 %>%
  select(id, ea, Q6_18, Q6_45, starts_with("Q6_56"), ) %>% # Current location (18), money transfers (45), kinds of goods (56). *In SA, there is no data on length of time in current location.
  mutate(hh_goods = ifelse(if_any(starts_with("Q6_56"), ~!is.na(.x)), TRUE, NA), migrant_hh = "hh") %>% #ANY goods count
  rename("hh_money" = Q6_45, "hh_loc" = Q6_18) %>%
  mutate(ea = as.numeric(str_sub(ea, 1, 3))) %>%
  select(id, ea, hh_money, hh_goods, hh_loc, migrant_hh)

z6a <- z6 %>%
  select(id, ea, Q7_2, Q7_8, Q7_10) %>% # Q7_2 = current location, Q7_8 = times received money in last 12, Q7_10 = goods in last 12
  mutate(ea = as.numeric(str_sub(ea, 1, 3)), migrant_hh = "non-hh") %>%
  rename("nonhh_loc" = Q7_2, "nonhh_money" = `Q7_8`, "nonhh_goods" = `Q7_10`)

# HH IDs are unique to ea, not to the dataset.
# Make unique for the combo of id and ea.

z0b <- z0a %>%
  distinct(id, ea, .keep_all = T) %>% #Note that original file has a row per person, meaning household data is repeated for each person in the house. This moves to distinct rows by house.
  arrange(ea, id) %>%
  mutate(house = seq(1:nrow(.))) %>%
  relocate(house)

##### Process household roster into count of individuals #####

z1b <- z1a %>%
  group_by(ea, id) %>%
  count(ea, id) %>%
  rename(hh_size = n)

##### Process migrant roster into a count of household migrants #####

z5b <- z5a %>%
  group_by(ea, id) %>%
  count(ea, id) %>%
  rename(migrant_num = n)

##### Combine just household-level data for now ####

za1a <- full_join(z0b, z1b, by = c("ea", "id")) # Combine basic household data with household size, keeping all rows.
za1b <- full_join(za1a, z5b, by = c("ea", "id")) # Add migrant count


### Pause! Impute NAs before processing further ###

# Tidy up a smidge to make number codes factors, remove unused levels, etc.

za1b$migrant_num[is.na(za1b$migrant_num)] <- 0 # *everyone* who is NA here is a zero; see above calculation of migrant number (only for households with one or more), then merging keeping all data

za1b$air_cond[za1b$air_cond == 0] <- NA # there's one household with a 0; unclear whether that's "no" or just because 0 is close to 1 on the keyboard (and 1 means "yes")

za1b$computer[za1b$computer == 0] <- NA # there's one household with a 0; unclear whether that's "no" or just because 0 is close to 1 on the keyboard (and 1 means "yes")

za1b$bicycle[za1b$bicycle== 0] <- NA  # there's one household with a 0; unclear whether that's "no" or just because 0 is close to 1 on the keyboard (and 1 means "yes")

# Drop those with a LOT of NAs for imputed variables -- these are hard to impute.

# If someone was missing both TV and tele_mobile, they were likely missing a lot of other items.

za1b <- za1b[!(is.na(za1b$tele_mobile) & is.na(za1b$tv)),]

# If someone was missing both fridge and air_cond, they were likely missing a lot of other items.

za1b <- za1b[!(is.na(za1b$fridge) & is.na(za1b$air_cond)),]


### On to imputation

# Tell R which variables (columns) to use in imputation for which variables (rows)

predMat_n <- matrix(rep(0, ncol(za1b)^2), ncol = ncol(za1b), nrow = ncol(za1b))
rownames(predMat_n) <- colnames(za1b)
colnames(predMat_n) <- colnames(za1b)
predMat_n <- data.frame(predMat_n)

predMat_n[colnames(za1b) %in% c("air_cond", "computer", "bicycle", "auto"), colnames(za1b) %in% c("rm_cook", "rm_count", "own_house", "fridge", "tv", "radio", "tele_mobile")] <- 1 # Variables in front of the comma need to be imputed. Variables after the comma are being used for imputation. "Each row [in the predictor matrix] corresponds to a variable block, i.e., a set of variables to be imputed. A value of 1 means that the column variable is used as a predictor for the target block (in the rows)." 

# Tell mice to use predictive mean matching where it's imputing; all other slots in this vector are empty.
these_n <- rep("", length(za1b))
these_n[which(colnames(za1b) %in% row.names(predMat_n)[rowSums(predMat_n) > 0])] <-"pmm" 

# Run imputation (and set seed for replicability; if you don't set a seed, R picks a random one each time and the imputed values move around)

za1c <- complete(mice (za1b, method = these_n, predictorMatrix = as.matrix(predMat_n), seed = 17000731, print = FALSE)) # Seed chosen randomly on June 14 2024. PMM means predictive mean matching; DHS protocol uses mean assignment for the DHS wealth index. Complete returns the data set (saved as dat1) with missing values populated with predicted values.

za1c <- droplevels(za1c) # Dropping levels to improve behavior of factors


### Recode wealth data ###

za1c$ppl_room <- ifelse(za1c$rm_count == 0, 0, za1c$hh_size / za1c$rm_count) # number of people per room. Number of people per sleeping room is preferred; this is second choice when sleeping room data are not available. In South African data, 179 households have 0 rooms and no explanation is given -- this might be real data, so keeping and making this 0 for ppl_room to avoid infinite numbers. Produces 3 NAs as expected, as those data are missing from rm_count.

za1c$own_house <- case_when(
  za1c$own_house == "Yes" ~ 1,
  za1c$own_house == "No" ~ 0
)

# Water and electricity and own_land and wall are usually here, and reminder that they were NOT available/possible for South Africa.


### Following ones are simple presence-absence, requiring less manipulation than the above

za1c$tv <- case_when(
  za1c$tv == "Yes" ~ 1,
  za1c$tv == "No" ~ 0
)

za1c$fridge <- case_when(
  za1c$fridge == "Yes" ~ 1,
  za1c$fridge == "No" ~ 0
)

za1c$tele <- case_when(
      za1c$tele_mobile == "Yes" | za1c$tele_trad == "Yes" ~ 1,
      .default = 0
    ) # according to standard DHS practice, landline and mobile both count

za1c$auto <- case_when(
  za1c$auto == 1 ~ 1,
  za1c$auto == 2 ~ 0
)

za1c$air_cond <- case_when(
  za1c$air_cond == 1 ~ 1,
  za1c$air_cond == 2 ~ 0
)

za1c$computer <- case_when(
  za1c$computer == 1 ~ 1,
  za1c$computer == 2 ~ 0,
)

za1c$bicycle <- case_when(
  za1c$bicycle == 1 ~ 1,
  za1c$bicycle == 2 ~ 0
)

za1c$rm_cook <- case_when(
  za1c$rm_cook == "Yes" ~ 1,
  za1c$rm_cook == "No" ~ 0
)

za1c$radio <- case_when(
  za1c$radio == "Yes" ~ 1,
  za1c$radio == "No" ~ 0
)

# Remove variables no longer using before cutting NAs

za1c <- mutate(za1c, tele_trad = NULL, tele_mobile = NULL, rm_count = NULL)

##### PCA for wealth #####

#This is standard practice for the DHS wealth index

# Dropping remaining NAs that are missing variables needed for wealth variable.
za1d <- na.omit(za1c) # Tried to systematically omit as many as I could with multiple wealth variables missing up top, but still a few missing here (57)

# Remember again that own_land and wall and water and electric are missing from South Africa - don't forget when reusing this code!

pca_n <- za1d %>%
  as_tibble() %>%
select(own_house, ppl_room, rm_cook, tv, auto, fridge, computer, radio, air_cond, tele, bicycle) %>%
  prcomp(scale = TRUE) # scale scales to 0-1 since not all variables are binary. using singular value decomposition because I have a non-symmetric matrix.
# Got to 30% by dropping stuff not relevant to urban, but people did have cooking rooms (next lowest loading) and I'm already without four standard DHS variables (natural walls and owning land, because urban, and data issues for water and electric) so I kept cooking rooms in.

# DHS protocol is to extract first component loading for each household

za1d$wealth_index <- pca_n$x[ ,1] # household-level score on first principal component. Explains 31.5% of the variance in wealth.


##### Now process individual-level data for money received #####

## Recode household remittance data ###

z5a$hh_money <- case_when(
  z5a$hh_money > 0 ~ 1,
  z5a$hh_money == 0 ~ 0) # Unclear whether NAs are 0s or NA (same below) so keeping NA

z5a$hh_goods <- case_when(
  z5a$hh_goods == TRUE ~ 1)

z5a$hh_remit <- case_when(
  z5a$hh_goods == 1 | z5a$hh_money == 1 ~ 1,
  z5a$hh_goods == 0 & z5a$hh_money == 0 ~ 0 # True zeroes are only those that have zeroes for both columns
)

z5a <- z5a %>%
  filter(!is.na(hh_remit) | !is.na(hh_loc), na.rm = TRUE) # remove rows where we have no location, no length of time in the place, or no remittance -- these appear to be errors.

### Received a remittance from non-household member ###

z6a$nonhh_remit <- case_when(
  z6a$nonhh_money > 0 | z6a$nonhh_goods == "Yes" ~ 1,
  z6a$nonhh_money == 0 & z6a$nonhh_goods == "No" ~ 0 # Zeroes could exist for money but don't, so there will be no zeroes here
)

z6b <- z6a %>%
  group_by(ea, id) %>% # need to group first to use keep in mutate
  filter(!is.na(nonhh_remit) | !is.na(nonhh_loc), na.rm = TRUE) %>% # remove rows where there was no location for non-hh migrant or no remittance coming in -- these appear to be added in error
  select(-nonhh_money, -nonhh_goods) # Remove unused columns

z56 <- bind_rows(z5a, z6b) # Combine rows from households with HH members elsewhere and HHs with social-network members elsewhere

##### Migrant characteristics #####

# This is in preparation for exploratory models on whether households are investing in migration to get remittances vs not #
# A separate set of exploratory models will investigate whether households invested in migration during the time periods over which we're measuring shocks (thus the months for hh migrants; we don't have time for non-hh migrants) #

### Code migrant locations as internal vs external ###

z56 <- z56 %>%
  mutate(hh_loc = if_else(hh_loc == "Urban area within South Africa" | hh_loc == "Rural area within South Africa", "national", "international"), nonhh_loc = if_else(nonhh_loc == "Urban area within South Africa" | nonhh_loc == "Rural area within South Africa", "national", "international"), .keep = "unused")

### Code how many months a household migrant has been in current location ###

# *Not available for South Africa; create column of NAs to match other countries' data*

z56 <- z56 %>%
  mutate(hh_months = NA)

### Combine columns and remove unused ones ###

z56$remit <- case_when(
  z56$hh_remit == 1 & is.na(z56$nonhh_remit) ~ 1,
  z56$nonhh_remit == 1 & is.na(z56$hh_remit) ~ 1,
  z56$nonhh_remit == 0 & z56$hh_remit == 0 ~ 0
) # This is cleanest, to avoid overwriting any data... the below (unite) can be less clean but checked for potential overwriting before doing this with z56[!is.na(z56$hh_loc) & !is.na(z56$nonhh_loc),]


# There are six remaining NAs that don't have location data and it's harder to figure out if they're real without months at location. Checking original data files, it looks like id's 79 (one obervation) and 83 (two observations) are just NAs across all columns, so cut both.

z56 <- z56[!(z56$id %in% c(79, 83)),]

z56 <- z56 %>% unite("migrant_loc", hh_loc, nonhh_loc, na.rm = TRUE)

# Remove unused columns

z56 <- z56 %>%
  select(-hh_money, -hh_goods, -hh_remit, -nonhh_remit)


##### Make binary for whether any remittances received period, whether from hh vs non-hh migrants #####

z56 <- z56 %>%
  group_by(ea, id) %>%
  mutate(any_remit = if_else(any(remit == 1), 1, 0))


##### Recombining -- there's definitely a row for every house, even if they had no migrants (and from those migrants, no remittances) #####

za1e <- left_join(za1d, z56, by = c("id", "ea"))

# NAs for remit are informative: these are households that didn't receive food or money, from same household or different household. NA is zero.


##### Merge with location data #####

za1e$country <- "south_africa" # Country name for later merge

za1e <- za1e %>%
  select(house, ea, country, wealth_index, hh_size, migrant_num, any_remit, migrant_hh, migrant_loc, hh_months, remit) # Simplify and reduce participant identifiability by reducing columns available

loc <- read_csv("south africa locations_micro.csv") # Authors AP and HJ coded the lat/long of the provincial capital for each province; import those data to merge with data on household remittances

loc1 <- left_join(loc, ea_codes, by = c("location" = "loc_micro")) # This does make for duplicate location names BUT because we're merging with the data by ea (enumeration area), this sorts itself out.

za1f <- left_join(za1e, loc1, by = "ea")

za1f <- za1f %>%
  rename(district = loc_meso, census_tract = location) # District is province - just keeping similar names to match other countries. Loc meso is actually municipality - didn't have finer-grained detail on South Africa.

za1f <- mutate(za1f, landmark = NULL, notes = NULL, source = NULL, ea = NULL) # Remove notes about how she got lat/long for each

##### Output #####

write_csv(za1f, "south_africa.csv")
