
# When practices for construction DHS Wealth Index are referenced throughout, this is the citation:
# Rutstein, S. O. (2015). Steps to constructing the new DHS Wealth Index. Rockville, MD: ICF International, 6.


library(foreign) # Bring in .dta data
library(tidyverse) # Various functions useful for data processing
library(data.table) # More handy for going long to wide on a single factor
library(mice) # Imputing missing data
library(fuzzyjoin) # For fuzzy string matching between location and social data

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set working directory to location of this file (script file)

##### Download data #####
# Download from https://microdata.worldbank.org/index.php/catalog/534


##### Import data #####

s0 <- as_tibble(read.dta("base_menage_21avril2011.dta"))
s1 <- as_tibble(read.dta("base_individu_21_avril_2011.dta"))
s5 <- as_tibble(read.dta("base_anciens_membres_du_menage_final_21_avril11.dta"))
s6 <- as_tibble(read.dta("bases_non_anciens_membres_du_menage_21 avril2011.dta"))

##### Subset #####

s0a <- s0 %>% 
  select(numqest, q02, q03, q05, #numqest = house, q03 = department, q05 = village
         q23, q24, q25, q26, q27, q31_1, q31_3, q31bis_2, q31bis_3, q31bis_4, q31bis_8, q31bis_9, q31bis_10, q31bis_12, q31bis_13) %>% 
  # q23 = walls, q24 = cooking room, q25 = total number rooms, q26 = electric, q27 = water, q31_1 = own ag land, q31_3 = own house, q31bis_2 = radio, q31bis_3 = tv, q31bis_4 = fridge, q31bis_8 = computer, q31bis_9 = mobile phone, q31bis_10 = landline, q31bis_12 = ox-drawn cart, q31bis_13 = car/truck
  
  # Considered owning tractorharvestor, motorcycle, air cond but variability was quite low. Unlike other countries added ox-drawn cart because ownership of things like car/truck and motorcycle were low.
  # Low loading on PCA and removed: bicycle
  # Things in DHS that are not present here: no cooking fuel type, toilet, trash disposal, floor material, roof material, livestock, separate bedrooms.
  # Researchers did not include a date variable. All data colection took place in Oct and Nov 2009. Variables such as Batch were not usable as I could not confirm that they meant e.g. strata, and interviews took place simultaneously across regions by multiple teams, so even strata wouldn't help.
  
  mutate_at(vars(q02, q03, q05), ~ str_to_lower(.)) %>% # Already unique by household; didn't need to apply unique requirement.
  rename(
    "department" = `q02`,
    "district" = `q03`,
    "village" = `q05`,
    "wall" = `q23`,
    "rm_cook" = `q24`,
    "rm_count" = `q25`,
    "electric" = `q26`,
    "water" = `q27`,
    "own_land" = `q31_1`,
    "own_house" = `q31_3`,
    "radio" = `q31bis_2`,
    "tv" = `q31bis_3`,
    "fridge" = `q31bis_4`,
    "computer" = `q31bis_8`,
    "tele_mobile" = `q31bis_9`,
    "tele_trad" = `q31bis_10`,
    "cart" = `q31bis_12`,
    "auto" = `q31bis_13`)

s1a <- s1 %>%
  select(numqest, numpers1) # List of everyone who *currently* lives in the house. Numqest is unique across the dataset so don't need census for merging

s5a <- s5 %>%
  select(numqest, q519, q521, q57, q58_1, q58_2) %>%
  rename(
   "hh_money" = `q519`,
   "hh_goods" = `q521`,
   "hh_loc" =  `q57`,
   "hh_years" = `q58_1`,
   "hh_months_just" = `q58_2` # Other countries have length in months as output (hh_months) so clarifying here that this is JUST months without years*12 added yet
   ) %>%
  mutate(migrant_hh = "hh")

s6a <- s6 %>%
  select(numqest, q62, q66, q610) %>%
  rename(
    "non_hh_money" = `q66`,
    "non_hh_goods" = `q610`,
    "non_hh_loc" = `q62`
  ) %>%
  mutate(migrant_hh = "non-hh")



##### Process household roster into count of individuals #####

# Researchers provide hh size in s0 but I've been calculating for other countries so will do the same here. Good news! I get the same numbers the researchers provide in s0.

s1b <- s1a %>%
  group_by(numqest) %>%
  count(numqest) %>%
  rename(hh_size = n)


##### Process migrant roster into a count of household migrants #####

# Same story: researchers also provide this number in s0, but I follow my own SOP from other countries. I have slightly more individuals at lower counts than researcher-provided counts -- more in e.g., 1 and fewer coming in with over 10 migrants -- but stick with SOP for consistency.

s5b <- s5a %>%
  group_by(numqest) %>%
  count(numqest) %>%
  rename(migrant_num = n)


##### Combine just household-level data for now ####

se1a <- full_join(s0a, s1b, by = c("numqest")) # Combine basic household data, keeping all rows.
se1b <- full_join(se1a, s5b, by = c("numqest"))


### Pause! Impute NAs before processing further ###

# Tidy up a smidge to make number codes factors, remove unused levels, etc.

se1b$migrant_num[is.na(se1b$migrant_num)] <- 0 # *everyone* who is NA here is a zero; see above calculation of migrant number (only for households with one or more), then merging keeping all data

se1b$wall[se1b$wall == "autres"] <- NA # so few people have other for walls, recoding to "NA" and then imputing (16 out of 1953)

se1b$water[se1b$water == "autres"] <- NA # only 1 has "other" so imputing

se1b <- droplevels(se1b)

# Unlike other countries, amazing data completeness and not having to remove folks with missing data - thanks Senegal!


# Tell R which variables (columns) to use in imputation for which variables (rows)

predMat_n <- matrix(rep(0, ncol(se1b)^2), ncol = ncol(se1b), nrow = ncol(se1b))
rownames(predMat_n) <- colnames(se1b)
colnames(predMat_n) <- colnames(se1b)
predMat_n <- data.frame(predMat_n)

predMat_n[colnames(se1b) %in% c("wall", "water"), colnames(se1b) %in% c("rm_cook", "rm_count", "electric", "own_land", "own_house", "radio", "tv", "fridge", "computer", "tele_mobile", "tele_trad", "bicycle", "cart", "auto")] <- 1 # Variables in front of the comma need to be imputed. Variables after the comma are being used for imputation. "Each row [in the predictor matrix] corresponds to a variable block, i.e., a set of variables to be imputed. A value of 1 means that the column variable is used as a predictor for the target block (in the rows)." 

# Tell mice to use predictive mean matching where it's imputing; all other slots in this vector are empty.
these_n <- rep("", length(se1b))
these_n[which(colnames(se1b) %in% row.names(predMat_n)[rowSums(predMat_n) > 0])] <-"pmm" 

# Run imputation (and set seed for replicability; if you don't set a seed, R picks a random one each time and the imputed values move around)

se1c <- complete (mice (se1b, method = these_n, predictorMatrix = as.matrix(predMat_n), seed = 17000731, print = FALSE)) # Seed chosen randomly on June 14 2024. PMM means predictive mean matching; DHS protocol uses mean assignment for the DHS wealth index. Complete returns the data set (saved as dat1) with missing values populated with predicted values.

se1c <- droplevels(se1c) # Dropping levels to improve behavior of factors

### Recode wealth data ###

se1c$ppl_room <- se1c$hh_size/se1c$rm_count # number of people per room. Number of people per sleeping room is preferred; this is second choice when sleeping room data are not available. Produces 5 NAs as expected, as those data are missing in the original variables.

se1c$own_land <- case_when(
  se1c$own_land == "oui" ~ 1,
  se1c$own_land == "non" ~ 0
)

se1c$own_house <- case_when(
  se1c$own_house == "oui" ~ 1,
  se1c$own_house == "non" ~ 0
)

se1c$wall <- case_when(
  se1c$wall == "ciment" ~ "non_natural",
  se1c$wall == "paille/tige" ~ "natural", # straw/sticks
  se1c$wall == "bois" ~ "natural", # wood
  se1c$wall == "banco" ~ "natural" # mud
) #natural materials are one category according to DHS protocol

se1c$wall <- as_factor(se1c$wall)

setDT(se1c)[, c(paste0("wall_",levels(se1c$wall)), "wall") := 
               c(lapply(levels(wall), function(x) as.integer(x == wall)), .(NULL))] # walls are binary for each case

se1c$water <- case_when(
  se1c$water == "puits int\xe9rieur" ~ "private", # well
  se1c$water == "puits ext\xe9rieur" ~ "public",
  se1c$water == "robinet int\xe9rieur" ~ "private", # faucet
  se1c$water == "robinet ext\xe9rieur" ~ "public",
  se1c$water == "forage" ~ "public", # borehole
  se1c$water == "vendeur d'eau/citerne" ~ "public", # I thought about this one for a while: depends on the research question. Yes, paying for water. Yes, might be cleaner than others. Still suggests less access to infrastructure though. I made the same decision for vending for other countries too.
  se1c$water == "rivi\xe8re ou marigot" ~ "public" # Surface is supposed to be separate according to DHS but there's only 14 people; calling it public (and it's not that different from boreholes anyway, and consistent with what I did for other countries)
)

se1c$water <- as_factor(se1c$water)

setDT(se1c)[, c(paste0("water_",levels(se1c$water)), "water") := 
               c(lapply(levels(water), function(x) as.integer(x == water)), .(NULL))]


### Following ones are simple presence-absence, requiring less manipulation than the above

se1c$tv <- case_when(
  se1c$tv == "oui" ~ 1,
  se1c$tv == "non" ~ 0
)

se1c$fridge <- case_when(
  se1c$fridge == "oui" ~ 1,
  se1c$fridge == "non" ~ 0
)

se1c$tele <- case_when(
      se1c$tele_mobile == "oui" | se1c$tele_trad == "oui" ~ 1,
      .default = 0
    ) # according to standard DHS practice, landline and mobile both count

se1c$auto <- case_when(
  se1c$auto == "oui" ~ 1,
  se1c$auto == "non" ~ 0
)

se1c$cart <- case_when(
  se1c$cart == "oui" ~ 1,
  se1c$cart == "non" ~ 0
)

se1c$computer <- case_when(
  se1c$computer == "oui" ~ 1,
  se1c$computer == "non" ~ 0
)

se1c$rm_cook <- case_when(
  se1c$rm_cook == "oui" ~ 1,
  se1c$rm_cook == "non" ~ 0
)

se1c$electric <- case_when(
  se1c$electric == "oui" ~ 1,
  se1c$electric == "non" ~ 0
)

se1c$radio <- case_when(
  se1c$radio == "oui" ~ 1,
  se1c$radio == "non" ~ 0
)

##### PCA for wealth #####

# This is standard practice for the DHS wealth index
# Because just two levels each for wall and water, just name one variable in PCA

pca_n <- se1c %>%
  as_tibble() %>%
select(own_land, own_house, water_public, wall_natural, ppl_room, rm_cook, tv, auto, fridge, electric, computer, tele, radio, cart) %>%
  prcomp(scale = TRUE) # scale scales to 0-1 since not all variables are binary. using singular value decomposition because I have a non-symmetric matrix.


# DHS protocol is to extract first component loading for each household

se1c$wealth_index <- pca_n$x[ ,1] # household-level score on first principal component. Explains 31.5% of the variance in wealth.


##### Now process individual-level data for money received #####

### Recode household remittance data ###

s5a$hh_money <- case_when(
  s5a$hh_money > 0 ~ 1,
  s5a$hh_money == 0 ~ 0)

s5a$hh_goods <- case_when(
  s5a$hh_goods == "oui" ~ 1,
  s5a$hh_goods == "non" ~ 0)

s5a$hh_remit <- case_when(
  s5a$hh_goods == 1 | s5a$hh_money == 1 ~ 1,
  s5a$hh_goods == 0 & s5a$hh_money == 0 ~ 0 # True zeroes are only those that have zeroes for both columns
)

s5a <- s5a %>%
  filter(!is.na(hh_remit) | !is.na(hh_years) | !is.na(hh_months_just) | !is.na(hh_loc), na.rm = TRUE) # remove rows where we have no location, no length of time in the place, or no remittance -- these appear to be errors.

### Received a remittance from non-household member ###

s6a$nonhh_remit <- case_when(
  s6a$non_hh_money == "oui" | s6a$non_hh_goods == "oui" ~ 1,
  s6a$non_hh_money == "non" & s6a$non_hh_goods == "non" ~ 0
)

s6b <- s6a %>%
  group_by(numqest) %>% # need to group first to use keep in mutate
  filter(!is.na(nonhh_remit) | !is.na(non_hh_loc), na.rm = TRUE) %>% # not applicable in Senegal but for SOP: remove rows where there was no location for non-hh migrant or no remittance coming in -- these appear to be added in error
  select(-non_hh_money, -non_hh_goods) # Remove unused columns

s56 <- bind_rows(s5a, s6b) # Combine rows from households with HH members elsewhere and HHs with social-network members elsewhere

##### Migrant characteristics #####

# This is in preparation for exploratory models on whether households are investing in migration to get remittances vs not #
# A separate set of exploratory models will investigate whether households invested in migration during the time periods over which we're measuring shocks (thus the months for hh migrants; we don't have time for non-hh migrants) #

### Code migrant locations as internal vs external ###

s56 <- s56 %>%
  mutate(hh_loc = if_else(hh_loc == "milieu urbain du s\xe9n\xe9gal" | hh_loc == "milieu rural du s\xe9n\xe9gal", "national", "international"), nonhh_loc = if_else(non_hh_loc == "milieu urbain du s\xe9n\xe9gal" | non_hh_loc == "milieu rural du s\xe9n\xe9gal", "national", "international"), .keep = "unused")

### Code how many months a household migrant has been in current location ###

s56 <- s56 %>%
  rowwise() %>%
  mutate(hh_months = (hh_years * 12) + hh_months_just, .keep = "unused")

### Combine columns and remove unused ones ###

s56$remit <- case_when(
  s56$hh_remit == 1 & is.na(s56$nonhh_remit) ~ 1,
  s56$nonhh_remit == 1 & is.na(s56$hh_remit) ~ 1,
  s56$nonhh_remit == 0 & s56$hh_remit == 0 ~ 0
) # This is cleanest, to avoid overwriting any data... the below (unite) can be less clean but checked for potential overwriting before doing this with s56[!is.na(s56$hh_loc) & !is.na(s56$nonhh_loc),]

s56 <- s56 %>% unite("migrant_loc", hh_loc, nonhh_loc, na.rm = TRUE)

# Remove unused columns

s56 <- s56 %>%
  select(-hh_money, -hh_goods, -hh_remit, -nonhh_remit)


##### Make binary for whether any remittances received period, whether from hh vs non-hh migrants #####

s56 <- s56 %>%
  group_by(numqest) %>%
  mutate(any_remit = if_else(any(remit == 1), 1, 0))


##### Recombining -- there's definitely a row for every house, even if they had no migrants (and from those migrants, no remittances) #####

se1d <- left_join(se1c, s56, by = "numqest")

# NAs for remit are informative: these are households that didn't receive food or money, from same household or different household. NA is zero.


##### Merge with location data #####

se1d$country <- "senegal" # Country name for later merge

se1d <- se1d %>%
  select(numqest, department, district, village, country, wealth_index, hh_size, migrant_num, any_remit, migrant_hh, migrant_loc, hh_months, remit) %>% # Simplify and reduce participant identifiability by reducing columns available
  mutate(across(everything(), ~ str_squish(.))) # Remove white space before merge

loc <- read_csv("senegal locations_micro.csv") # Authors AP and HJ coded the lat/long of the provincial capital for each province; import those data to merge with data on household remittances. Lots of spelling errors for Senegal in particular

loc <- loc %>%
  mutate(commune = NULL, landmark = NULL, notes = NULL)  %>% # Remove notes about how she got lat/long for each
  mutate(across(everything(), ~ str_squish(.))) %>%
  distinct(village, district, department, .keep_all = TRUE)

se1e <- left_join(se1d, loc, by = c("department", "district", "village")) 

se1e <- se1e %>%
  rename(house = numqest, census_tract = village) %>% # Department is meso in Senegal - region is bigger
  mutate(district = NULL)

se1f <- se1e %>%
  rename(district = department) # To avoid misread, new dataframe and new function here: I deleted district above, then renamed department "distrct" for comparability to other countries -- it's the "meso" level!

##### Output #####

write_csv(se1f, "senegal.csv")
