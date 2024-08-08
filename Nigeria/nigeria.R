
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

n0 <- as_tibble(read.dta("consolidated.dta"))
n1 <- as_tibble(read.dta("individuals 5.dta")) 
n5 <- as_tibble(read.dta("migrants 2.dta"))
n6 <- as_tibble(read.dta("non hh member migrants 2.dta"))


##### Subset #####

n0a <- n0 %>%
  select(hhno, state, censusunit, villagetown, constructionmaterial, separatekitchen, noofseparaterooms, electricity, drinkingwatersource, agricland, house, tv, fridge, airconditioner, computer, mobilephone, nonmobilephone, bicycle, cartruck, motorcycle) %>% #Select household, district, census unit, walls (construction material), separate cooking room (separatekitchen), total count of dining sleeping and living rooms in main house (noofseparaterooms), [some self explanatory ones], own house (house), [some self explanatory ones]. 
  #Things in DHS that are not present here: no cooking fuel type, toilet, trash disposal, floor material, roof material, livestock, separate bedrooms.
  # Considereed owning beds, radio, tractorharvestor but variability was quite low.
  # Dropped variables that had low loadings on first PC: moto, water_public, water_piped.
  # Researchers did not include a date variable. All data colection took place between Oct 5 and November 6, 2009. Variables such as Batch were not usable as I could not confirm that they meant e.g. strata, and interviews took place simultaneously across regions by multiple teams, so even strata wouldn't help.
  
  mutate_at(vars(state, villagetown, censusunit), ~ str_to_lower(.)) %>% # Already unique by household; didn't need to apply unique requirement.
  rename("own_house" = `house`)

n1a <- n1 %>%
  select(hhno, censusunit, personid) %>% # List of everyone who *currently* lives in the house.
  mutate_at(vars(censusunit), ~ str_to_lower(.))

## Census units are missing for 124 rows (65 homes) in n5. Add from n0.

n5a <- n5 %>%
  select(HHNo, CensusUnit, Town, Howmanytimes, Sendfoodgoods) %>%
  rename("hhno" = `HHNo`, "villagetown" = `Town`) %>%
  mutate_at(vars(villagetown, CensusUnit), ~ str_to_lower(.)) %>%
  arrange(villagetown, hhno)

# I checked using hhtype (shared across datasets) and there are unique matches in n0 for each row in misses.

n0a_census <- n0a %>%
  select("hhno", "villagetown", "censusunit") %>%
  distinct(pick(villagetown, hhno), .keep_all = TRUE)

n5b <- left_join(n5a, n0a_census, by = c("hhno", "villagetown"))

n5b$CensusUnit <- if_else(n5b$CensusUnit == "", n5b$censusunit, n5b$CensusUnit) # Replace missing values with n0 censusunit.

n5b <- n5b %>%
  mutate(censusunit = NULL, villagetown = NULL) %>%
  rename("censusunit" = `CensusUnit`) %>%
  relocate(hhno, censusunit)


n6a <- n6 %>%
  select(HHNo, Censusunit, Sendmoney, Foodgoods) %>% # Some houses (12) didn't receive money or goods in last 12, so need to exclude those.
  rename("hhno" = `HHNo`, "censusunit" = `Censusunit`) %>%
  mutate_at(vars(censusunit), ~ str_to_lower(.))


# HH IDs are unique to census unit, not to the dataset.
# Make unique for the combo of HHno and census unit.

n0b <- n0a %>%
  distinct(hhno, censusunit, .keep_all = T) %>%
  arrange(censusunit, hhno) %>%
  mutate(house = seq(1:nrow(.))) %>%
  relocate(house) # Five duplicates removed in this process.

##### Process household roster into count of individuals #####

# Researchers provide hh size but there are two NAs, so this improves accuracy

n1b <- n1a %>%
  group_by(censusunit, hhno) %>%
  count(censusunit, hhno) %>%
  rename(hh_size = n)


##### Process migrant roster into a count of household migrants #####

n5c <- n5b %>%
  group_by(censusunit, hhno) %>%
  count(censusunit, hhno) %>%
  rename(migrant_num = n)

##### Combine just household-level data for now ####

nga1a <- full_join(n0b, n1b, by = c("censusunit", "hhno")) # Combine basic household data, keeping all rows.
nga1b <- left_join(nga1a, n5c, by = c("censusunit", "hhno")) # Something weird here where getting duplicates, even though R claims no duplicated house/census pairs in either dataset. Using left join to avoid this, making sure all nga1a makes it in.

nga1b <- nga1b %>% relocate(house)

nga1b <- nga1b %>%
  group_by(house) %>%
  rename(wall = constructionmaterial, rm_cook = separatekitchen, own_land = agricland, rm_count = noofseparaterooms, electric = electricity, water = drinkingwatersource, auto = cartruck, moto = motorcycle, tele_mobile = mobilephone, tele_land = nonmobilephone, air_cond = airconditioner)


### Pause! Impute NAs before processing further ###

# Tidy up a smidge to make number codes factors, remove unused levels, etc.

nga1b$migrant_num[is.na(nga1b$migrant_num)] <- 0 # *everyone* who is NA here is a zero; see above calculation of migrant number (only for households with one or more), then merging keeping all data

nga1b$wall[nga1b$wall == "Other"] <- NA # so few people have other for walls, recoding to "NA" and then imputing (9 out of 1942)

nga1b$air_cond[nga1b$air_cond == 22] <- 2 # data entry error

# There are a number of people missing most or all of their wealth variables. Remove them.

nga1b <- nga1b[!(is.na(nga1b$wall) & is.na(nga1b$tv)),]
nga1b <- nga1b[!(is.na(nga1b$air_cond) & is.na(nga1b$tv)),]
nga1b <- nga1b[!(is.na(nga1b$wall) & is.na(nga1b$water)),]
nga1b <- nga1b[!(is.na(nga1b$moto) & is.na(nga1b$auto)),]

# For these columns, if they're missing these pairs, they seem to be missing most for wealth, probably from data collection errors - remove.

nga1b <- droplevels(nga1b)

# Tell R which variables (columns) to use in imputation for which variables (rows)

predMat_n <- matrix(rep(0, ncol(nga1b)^2), ncol = ncol(nga1b), nrow = ncol(nga1b))
rownames(predMat_n) <- colnames(nga1b)
colnames(predMat_n) <- colnames(nga1b)
predMat_n <- data.frame(predMat_n)

predMat_n[colnames(nga1b) %in% c("wall", "rm_cook", "rm_count", "water", "own_land", "own_house", "air_cond", "computer", "bicycle", "auto", "moto"), colnames(nga1b) %in% c("electric", "water", "tv", "fridge", "tele_mobile")] <- 1 # Variables in front of the comma need to be imputed. Variables after the comma are being used for imputation. "Each row [in the predictor matrix] corresponds to a variable block, i.e., a set of variables to be imputed. A value of 1 means that the column variable is used as a predictor for the target block (in the rows)." 
# Left out tele_land because not worth reducing estimation power for that since already quite rare.

# Tell mice to use predictive mean matching where it's imputing; all other slots in this vector are empty.
these_n <- rep("", length(nga1b))
these_n[which(colnames(nga1b) %in% row.names(predMat_n)[rowSums(predMat_n) > 0])] <-"pmm" 

# Run imputation (and set seed for replicability; if you don't set a seed, R picks a random one each time and the imputed values move around)

nga1c <- complete (mice (nga1b, method = these_n, predictorMatrix = as.matrix(predMat_n), seed = 17000731, print = FALSE)) # Seed chosen randomly on June 14 2024. PMM means predictive mean matching; DHS protocol uses mean assignment for the DHS wealth index. Complete returns the data set (saved as dat1) with missing values populated with predicted values.

nga1c <- droplevels(nga1c) # Dropping levels to improve behavior of factors

### Recode wealth data ###

nga1c$ppl_room <- nga1c$hh_size/nga1c$rm_count # number of people per room. Number of people per sleeping room is preferred; this is second choice when sleeping room data are not available. Produces 5 NAs as expected, as those data are missing in the original variables.

nga1c$own_land <- case_when(
  nga1c$own_land == "Yes" ~ 1,
  nga1c$own_land == "No" ~ 0
)

nga1c$own_house <- case_when(
  nga1c$own_house == "Yes" ~ 1,
  nga1c$own_house == "No" ~ 0
)

nga1c$wall <- case_when(
  nga1c$wall == "Bricks" ~ "non-natural",
  nga1c$wall == "Straw" ~ "natural",
  nga1c$wall == "Wood" ~ "natural",
  nga1c$wall == "Mud" ~ "natural",
  nga1c$wall == "Prefabricated" ~ "non-natural"
) #natural materials are one category according to DHS protocol

nga1c$wall <- as_factor(nga1c$wall)

setDT(nga1c)[, c(paste0("wall_",levels(nga1c$wall)), "wall") := 
               c(lapply(levels(wall), function(x) as.integer(x == wall)), .(NULL))] # walls are binary for each case

nga1c$water <- case_when(
  nga1c$water == "Private faucet or tap" ~ "piped",
  nga1c$water == "Rain water" ~ "surface",
  nga1c$water == "Well" ~ "public",
  nga1c$water == "Public pump" ~ "public",
  nga1c$water == "Water truck" ~ "public",
  nga1c$water == "River or stream" ~ "surface",
  nga1c$water == "Other" ~ "other" #No idea what other is here (looking at online data didn't help narrow itd down) and with 96 observations, I decided it was too much to impute.
) #natural materials are one category

nga1c$water <- as_factor(nga1c$water)

setDT(nga1c)[, c(paste0("water_",levels(nga1c$water)), "water") := 
               c(lapply(levels(water), function(x) as.integer(x == water)), .(NULL))] #water is binary for each source, with surface water grouped

### Following ones are simple presence-absence, requiring less manipulation than the above

nga1c$tv <- case_when(
  nga1c$tv == "Yes" ~ 1,
  nga1c$tv == "No" ~ 0
)

nga1c$fridge <- case_when(
  nga1c$fridge == "Yes" ~ 1,
  nga1c$fridge == "No" ~ 0
)

nga1c$tele <- case_when(
      nga1c$tele_mobile == "Yes" | nga1c$tele_land == "Yes" ~ 1,
      .default = 0
    ) # according to standard DHS practice, landline and mobile both count

nga1c$auto <- case_when(
  nga1c$auto == "Yes" ~ 1,
  nga1c$auto == "No" ~ 0
)

nga1c$moto <- case_when(
  nga1c$moto == "Yes" ~ 1,
  nga1c$moto == "No" ~ 0
)

nga1c$air_cond <- case_when(
  nga1c$air_cond == 1 ~ 1,
  nga1c$air_cond == 2 ~ 0
)

nga1c$computer <- case_when(
  nga1c$computer == "Yes" ~ 1,
  nga1c$computer == "No" ~ 0
)

nga1c$bicycle <- case_when(
  nga1c$bicycle == "Yes" ~ 1,
  nga1c$bicycle == "No" ~ 0
)

nga1c$rm_cook <- case_when(
  nga1c$rm_cook == "Yes" ~ 1,
  nga1c$rm_cook == "No" ~ 0
)

nga1c$electric <- case_when(
  nga1c$electric == "Yes" ~ 1,
  nga1c$electric == "No" ~ 0
)

##### PCA for wealth #####

#This is standard practice for the DHS wealth index

# Dropping remaining NAs that are missing variables needed for wealth variable.
nga1d <- na.omit(nga1c) # Tried to systematically omit as many as I could with multiple wealth variables missing up top, but still a few missing here.

pca_n <- nga1d %>%
  as_tibble() %>%
select(own_land, own_house, water_piped, water_surface, starts_with("wall_"), ppl_room, rm_cook, tv, auto, fridge, electric, computer, air_cond, tele, bicycle) %>%
  prcomp(scale = TRUE) # scale scales to 0-1 since not all variables are binary. using singular value decomposition because I have a non-symmetric matrix.
# Couldn't get PC1 above 30% - couldn't drop the lowest loadings, like surface water or people per room, as that's standard DHS.

# DHS protocol is to extract first component loading for each household

nga1d$wealth_index <- pca_n$x[ ,1] # household-level score on first principal component. Explains 31.5% of the variance in wealth.


##### Now process individual-level data for money received #####

### For households that received remittances or food from one or more non-household member, just give them a 1 -- one row per household ###

n6b <- n6a %>%
  group_by(censusunit, hhno) %>% # need to group first to use keep in mutate
  mutate(nonhh_remit = if_else(Sendmoney == "Yes" | Foodgoods == "Yes", 1, 0), .keep = "none") %>%
  filter(nonhh_remit == 1, na.rm = TRUE) %>% # remove NAs
  distinct() # remove duplicate lines for households

n56 <- bind_rows(n5b, n6b) # Combine rows from households with HH members elsewhere and HHs with social-network members elsewhere


n56 <- n56 %>%
  group_by(censusunit, hhno) %>% #Group by household
  rename(curr_money = Howmanytimes, curr_goods = Sendfoodgoods) # curr variables are for current HH members who are elsewhere; nothh variables are for non-HH members who are elsewhere


### Clean and recode ###

## Recode remittance data

n56$curr_money <- case_when(
  n56$curr_money > 0 ~ 1,
  n56$curr_money == 0 ~ 0)

n56$curr_goods <- case_when(
  n56$curr_goods == "Yes" ~ 1,
  n56$curr_goods == "No" ~ 0
)


##### Make binary for whether any remittances received #####
# This is combining across source (same-HH, non-HH) and type (money or goods)

n56$cts <- rowSums(n56[ , 3:5], na.rm = T) # Since these are all binary, it's okay to sum

naggs <- aggregate(n56$cts, by = list(n56$censusunit, n56$hhno),function(x){ifelse(sum (x) > 0, 1, 0)}) # Record to binary

naggs <- data.frame(censusunit = naggs$Group.1, hhno = naggs$Group.2, remit = naggs$x) # Aggregate creates annoying dataframes (still useful function though!), so rename that messy dataframe

nga1e <- left_join(nga1d, naggs, by = c("hhno", "censusunit"))


##### Merge with location data #####

nga1e$country <- "nigeria" # Country name for later merge

nga1e <- nga1e %>%
  select(house, state, censusunit, country, hh_size, migrant_num, wealth_index, remit) %>% # Simplify and reduce participant identifiability
  rename(loc_meso = state, loc_micro = censusunit) # For merging. There are 36 states in Nigeria, so though they are administrative level 1, I'd opted to use state as meso-level geographic designation for comparability to other countries in the dataset.

loc <- read_csv("nigeria locations_micro.csv") # Authors AP and HJ coded the lat/long of the provincial capital for each province; import those data to merge with data on household remittances

loc <- mutate(loc, landmark = NULL, notes = NULL) # Remove AP's notes about how she got lat/long for each

nga1f <- left_join(nga1e, loc, by = c("loc_micro" = "location")) # Merge province location with household data

nga1f <- nga1f %>%
  rename(district = loc_meso, census_tract = loc_micro)

##### Output #####

write_csv(nga1f, "nigeria.csv")
