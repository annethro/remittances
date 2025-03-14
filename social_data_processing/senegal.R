
# When practices for construction DHS Wealth Index are referenced throughout, this is the citation:
# Rutstein, S. O. (2015). Steps to constructing the new DHS Wealth Index. Rockville, MD: ICF International, 6.


library(foreign) # Bring in .dta data
library(tidyverse) # Various functions useful for data processing
library(data.table) # More handy for going long to wide on a single factor
library(mice) # Imputing missing data
library(fuzzyjoin) # For fuzzy string matching between location and social data

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set working directory to location of this file (script file)

##### Download data #####
# Download from https://microdata.worldbank.org/index.php/catalog/95


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
  select(numqest, q519, q521) %>%
  rename(
   "hh_money" = `q519`,
   "hh_goods" = `q521`
  )

s6a <- s6 %>%
  select(numqest, q66, q610) %>%
  rename(
    "non_hh_money" = `q66`,
    "non_hh_goods" = `q610`
  )



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

### For households that received remittances or food from one or more non-household member, just give them a 1 -- one row per household ###

s6b <- s6a %>%
  group_by(numqest) %>% # need to group first to use keep in mutate
  mutate(nonhh_remit = if_else(non_hh_money == "oui" | non_hh_goods == "oui", 1, 0), .keep = "none") %>%
  filter(nonhh_remit == 1, na.rm = TRUE) %>% # remove NAs
  distinct() # remove duplicate lines for households

s56 <- bind_rows(s5a, s6b) # Combine rows from households with HH members elsewhere and HHs with social-network members elsewhere


### Clean and recode ###

## Recode remittance data

s56$hh_money <- case_when(
  s56$hh_money > 0 ~ 1,
  is.na(s56$hh_money) | s56$hh_money == 0 ~ 0)

s56$hh_goods <- case_when(
  s56$hh_goods == "oui" ~ 1,
  is.na(s56$hh_goods) | s56$hh_goods == "non" ~ 0)


##### Make binary for whether any remittances received #####
# This is combining across source (same-HH, non-HH) and type (money or goods)

s56$cts <- rowSums(s56[ , 2:4], na.rm = T) # Since these are all binary, it's okay to sum

saggs <- aggregate(s56$cts, by = list(s56$numqest),function(x){ifelse(sum (x) > 0, 1, 0)}) # Record to binary

saggs <- data.frame(numqest = saggs$Group.1, remit = saggs$x) # Aggregate creates annoying dataframes (still useful function though!), so rename that messy dataframe

se1d <- left_join(se1c, saggs, by = "numqest")


##### Merge with location data #####

se1d$country <- "senegal" # Country name for later merge

se1d <- se1d %>%
  select(numqest, department, district, village, country, hh_size, migrant_num, wealth_index, remit) %>% # Simplify and reduce participant identifiability
  rename(house = numqest, loc_meso = department, loc_micro = village) %>% # For merging. Department is meso in Senegal - region is bigger
  mutate(across(everything(), ~ str_squish(.)))

#se1d$district[se1d$district == ""] <- "empty_string"

loc <- read_csv("senegal locations_micro.csv") # Authors AP and HJ coded the lat/long of the provincial capital for each province; import those data to merge with data on household remittances. Lots of spelling errors for Senegal in particular

loc <- loc %>%
  mutate(commune = NULL, landmark = NULL, notes = NULL)  %>% # Remove notes about how she got lat/long for each
  distinct(village, district, department, .keep_all = TRUE) %>%
#  mutate(across(c(village, district, department), ~replace_na(., "empty_string"))) %>%
  mutate(across(everything(), ~ str_squish(.)))

se1e <- left_join(se1d, loc, by = c("loc_micro" = "village")) 

#unique(se1e[duplicated(se1e$house), c("loc_micro","loc_meso")]) # When one investigates the duplicates here, this appears to be one of several errors that don't impede the accuracy of village to village matching: (1) treating department as region (kanel is a department in matam; dagan is in st louis; rufisque is in dakar; ) (2) RAs did not agree whether a village was in one department or a neighboring one (darou khoudoss in mbacke vs diourbel); 

#se1e <- stringdist_left_join(se1d, loc, by = c("loc_micro" = "village", "district" = "district", "loc_meso" = "department"), max_dist = 1) # Reminder that the by doesn't work without same-name variables -- need to adjust

se1e <- se1e %>%
  rename(department = loc_meso, census_tract = loc_micro) %>%
  mutate(district = NULL)

##### Output #####

write_csv(se1e, "senegal.csv")
