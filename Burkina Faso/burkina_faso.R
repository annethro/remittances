
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

b0 <- as_tibble(read.dta("f0w.dta")) 
b1 <- as_tibble(read.dta("f1w.dta")) 
b2 <- as_tibble(read.dta("f2w.dta")) 
b3 <- as_tibble(read.dta("f3w.dta")) 
b5 <- as_tibble(read.dta("f5iw.dta"))
b6 <- as_tibble(read.dta("f6iw.dta"))


##### Subset #####

b0a <- b0 %>%
  select(men, prov, vill, jint, mint, aint) %>% #Select household, province, and village.
  mutate_at(vars(prov, vill), ~ str_to_lower(.)) %>% #Make all names lowercase for ease of merging.
  distinct(men, .keep_all = T) #One row per household in case duplicates. There are none for Burkina.

b1a <- b1 %>%
  select(men, id) #List of everyone who *currently* lives in the house. (Based on wording in English, probably can't guarantee that people got this currently bit -- unclear if migrants are counted.)

b2a <- b2 %>%
  select(men, s21, s23, s24, s25, s26, s27) #These are details about the household itself, including ownership status (21), wall material (23), separate room for cooking (24), separate rooms overall (25), presence of electricity (26), water availability (27).

b3a <- b3 %>%
  select(men, s311, s316, s317, s318, s3113, s3114, s3117, s3118) #These are details about the household itself, including owns agricultural land (311), radio (s316), TV (s317), fridge (s318), car or truck (s3117), motorcycle (s3118), phones (s3113, s3114) #Things in DHS that are not present here: no cooking fuel type, toilet, trash disposal, flood material, roof material, livestock, separate bedrooms.
  
b5a <- b5 %>%
  select(men, s56, s57, s519, s521) # These questions were posed to households with one or more household members that are currently elsewhere, whether in Burkina or abroad. 56: why live outside HH? 57: where live. 519: 0/1 send money to HH last 12 months. 522: 0/1 send or bring food to HH.

b6a <- b6 %>%
  select(men, s62, s66, s610) # These are the households that have social network connections from outside their household that are currently elsewhere, whether in Burkina or abroad. 62: where live? 66: pres/abs of money, 610: 0/1 receive goods.

##### Process household roster into count of individuals #####

b1b <- b1a %>%
  group_by(men) %>%
  count(men) %>%
  rename(hh_size = n)

##### Process migrant roster into a count of household migrants #####

b5b <- b5a %>%
  group_by(men) %>%
  count(men) %>%
  rename(migrant_num = n)

##### Combine just household-level data for now ####

bur1a <- full_join(b0a, b1b, by = "men") # Combine basic household data, keeping all rows.

bur1b <- full_join(b2a, bur1a, by = "men")

bur1c <- full_join(b3a, bur1b, by = "men")

bur1d <- full_join(b5b, bur1c, by = "men")

bur1d <- bur1d %>%
  group_by(men) %>% # Group by household
  rename(house = men, 
         own_house = s21, own_land = s311, wall = s23, rm_cook = s24, rm_count = s25, electric = s26, water = s27, radio = s316, tv = s317, fridge = s318, auto = s3117, moto = s3118, tele_mobile = s3113, tele_land = s3114, day = jint, month = mint, year = aint)


### Pause! Impute NAs before processing further ###

# Data show signs of being missing at random

# Further, one "other" category is so low (9 cases out of 2102 are "other" for walls) that I use participants' other responses to impute their most likely wall composition -- that's actually more informative for computing wealth below with a PCA.

bur1d$wall[bur1d$wall == "Other"] <- NA

bur1d$hh_size[bur1d$hh_size == 44] <- NA # remove not-credible extreme outlier: 1 room with 44 people in it

bur1d$migrant_num[is.na(bur1d$migrant_num)] <- 0 # *everyone* who is NA here is a zero; see above calculation of migrant number (only for households with one or more), then merging keeping all data -- thus the NAs

bur1d <- droplevels(bur1d)

# Tell R which variables (columns) to use in imputation for which variables (rows)

# Run imputation (and set seed for replicability; if you don't set a seed, R picks a random one each time and the imputed values move around)

predMat_b <- matrix(rep(0, ncol(bur1d)^2), ncol = ncol(bur1d), nrow = ncol(bur1d))
rownames(predMat_b) <- colnames(bur1d)
colnames(predMat_b) <- colnames(bur1d)
predMat_b <- data.frame(predMat_b)

predMat_b[colnames(bur1d) %in% c("own_land", "rm_cook", "water", "rm_count", "day", "wall", "hh_size"), colnames(bur1d) %in% c("own_house", "radio", "tv", "fridge", "tele_mobile", "tele_land", "auto", "moto", "electric")] <- 1 # Variables in front of the comma need to be imputed. Variables after the comma are being used for imputation. "Each row [in the predictor matrix] corresponds to a variable block, i.e., a set of variables to be imputed. A value of 1 means that the column variable is used as a predictor for the target block (in the rows)." 

#Note: day is the weirdest one, but assuming there's spatial autocorrelation to wealth, not terrible idea...

# Tell mice to use predictive mean matching where it's imputing; all other slots in this vector are empty.
these_b <- rep("", length(bur1d))
these_b[which(colnames(bur1d) %in% row.names(predMat_b)[rowSums(predMat_b) > 0])] <-"pmm"

# Run imputation (and set seed for replicability; if you don't set a seed, R picks a random one each time and the imputed values move around)

bur1e <- complete(mice (bur1d, method = these_b, predictorMatrix = as.matrix(predMat_b), seed = 17000731, print = FALSE)) # Seed chosen randomly on June 14 2024. PMM means predictive mean matching; DHS protocol uses mean assignment for the DHS wealth index. Complete returns the data set (saved as dat1) with missing values populated with predicted values.

bur1e <- droplevels(bur1e) # Dropping levels to improve behavior of factors

### Recode wealth data ###

bur1e$ppl_room <- bur1e$hh_size/bur1e$rm_count # number of people per room. Number of people per sleeping room is preferred; this is second choice when sleeping room data are not available. Produces 19 NAs as expected


bur1e$own_land <- case_when(
  bur1e$own_land == "yes" ~ 1,
  bur1e$own_land == "No" ~ 0
)

bur1e$own_house <- case_when(
  bur1e$own_house == "Owned" ~ 1,
  .default = 0
)

bur1e$wall <- case_when(
  bur1e$wall == "Bricks/Stones" ~ "cement",
  bur1e$wall == "Flaw" ~ "natural", #this is straw, cross-referencing the French
  bur1e$wall == "Wood" ~ "natural",
  bur1e$wall == "Mud" ~ "natural",
  bur1e$wall == "Other" ~ "other"
) #natural materials are one category according to DHS protocol. Because of very low numbers in "other" (.4% of the sample), I've treated these as missing (above) and randomly assigned them to one of the other categories based on responses to other questions (predictive mean matching).

bur1e$wall <- as_factor(bur1e$wall)
  
setDT(bur1e)[, c(paste0("wall_",levels(bur1e$wall)), "wall") := 
               c(lapply(levels(wall), function(x) as.integer(x == wall)), .(NULL))] # walls are binary for each case

# Like wall, each category of water is suppose to be an indicator, but this isn't sensible for some of the smallest categories (capturing like 1% of the variance), especially since principal components analysis is coming further down in the code file, & Rutstein (2015) encourages researchers to use common sense. The only specific suggestion is to keep surface water separate.
# Here, based on descriptions from World Bank, UN, and USAID, it seems the main difference is between public sources, private sources, and the least sanitary and most precarious given variation in water availability: surface water. HOWEVER, here surface water is only 2% (49 cases out of 2102), so again using my best judgment and treating that as "public" in the Burkina case.

bur1e$water <- case_when(
  bur1e$water == "Well in the coumpound" ~ "private",
  bur1e$water == "Internal faucet/tap" ~ "private",
  bur1e$water == "External faucet/tape" ~ "private", #intuition: like Bolivia, sometimes the pipe comes right outside the door, and this is different than a shared public well
  bur1e$water == "Water truck" ~ "public",
  bur1e$water == "Public pump" ~ "public",
  bur1e$water == "Well outside the coumpound" ~ "public", #based on reports of NGOs, these are very likely to be public wells
  bur1e$water == "River or stream" ~ "public"
) #natural materials are one category

bur1e$water <- as_factor(bur1e$water)

setDT(bur1e)[, c(paste0("water_",levels(bur1e$water)), "water") := 
               c(lapply(levels(water), function(x) as.integer(x == water)), .(NULL))] #water is binary for each source, with surface water grouped (in this case, it's already grouped that way in the raw data)

### Following ones are simple presence-absence, requiring less manipulation than the above

bur1e$radio <- case_when(
  bur1e$radio == "yes" ~ 1,
  bur1e$radio == "No" ~ 0
)

bur1e$tv <- case_when(
  bur1e$tv == "yes" ~ 1,
  bur1e$tv == "No" ~ 0
)

bur1e$fridge <- case_when(
  bur1e$fridge == "yes" ~ 1,
  bur1e$fridge == "No" ~ 0
)

bur1e$tele <- case_when(
      bur1e$tele_mobile == "yes" | bur1e$tele_land == "yes" ~ 1,
      .default = 0
    ) # accprding to standard DHS practice, landline and mobile both count

bur1e$auto <- case_when(
  bur1e$auto == "yes" ~ 1,
  bur1e$auto == "No" ~ 0
)

bur1e$moto <- case_when(
  bur1e$moto == "yes" ~ 1,
  bur1e$moto == "No" ~ 0
)

bur1e$rm_cook <- case_when(
  bur1e$rm_cook == "yes" ~ 1,
  bur1e$rm_cook == "no" ~ 0
)

bur1e$electric <- case_when(
  bur1e$electric == "yes" ~ 1,
  bur1e$electric == "no" ~ 0
)



##### PCA for wealth #####

#This is standard practice for the DHS wealth index

pca1 <- bur1e %>%
  as_tibble() %>%
select(own_land, own_house, starts_with("water_"), starts_with("wall_"), ppl_room, radio, tv, fridge, tele, auto, moto, rm_cook, electric) %>%
  prcomp(scale = TRUE) # scale scales to 0-1 since not all variables are binary. using singular value decomposition because I have a non-symmetric matrix



# DHS protocol is to extract first component loading for each household

bur1e$wealth_index <- pca1$x[ ,1] # household-level score on first principal component. Explains 32% of the variance in wealth.


### Get usable interview date ###

bur1e <- bur1e %>%
  unite(col = "long_date", c(day, month, year), sep = "-", na.rm = TRUE, remove = FALSE)

bur1e$date <- dmy(bur1e$long_date) #"failed to parse" is as expected - they're missing.


##### Now process individual-level data for money received #####

b56 <- bind_rows(b5a, b6a) # Combine rows from households with HH members elsewhere and HHs with social-network members elsewhere


b56 <- b56 %>%
  group_by(men) %>% #Group by household
  rename(house = men,  
         curr_why = s56, curr_where = s57, curr_money = s519, curr_goods = s521, nothh_where = s62, nothh_money = s66, nothh_goods = s610) # curr variables are for current HH members who are elsewhere; nothh variables are for non-HH members who are elsewhere


### Clean and recode ###

b56 <- droplevels(b56) # Dropping levels to improve behavior of factors

## Recode remittance data

b56$curr_money <- case_when( # Data are continuous (number of times money sent) but very sparse (few received money more than 3 times) and don't specify amounts (amount could be huge and only sent once); given these uncertainties, we code all to presence/absence.
  b56$curr_money > 0 ~ 1,
  b56$curr_money == 0 ~ 0)

b56$curr_goods <- case_when(
  b56$curr_goods == 1 ~ 1,
  b56$curr_goods == 2 ~ 0,
  b56$curr_goods == 0 ~ NA
)

b56$nothh_money <- case_when(
  b56$nothh_money == "yes" ~ 1,
  b56$nothh_money == "no" ~ 0
)

b56$nothh_goods <- case_when(
  b56$nothh_goods == "yes" ~ 1,
  b56$nothh_goods == "no" ~ 0
)

##### Make binary for whether any remittances received #####
# This is combining across source (same-HH, non-HH) and type (money or goods)

b56 <- b56 %>%
  select(house, curr_money, curr_goods, nothh_money, nothh_goods)

b56$cts <- rowSums(b56[ , 2:5], na.rm = T) # Since these are all binary, it's okay to sum

baggs <- aggregate(b56$cts, by = list(b56$house),function(x){ifelse(sum (x) > 0, 1, 0)}) # Record to binary

baggs <- data.frame(house = baggs$Group.1, remit = baggs$x) # Aggregate creates annoying dataframes (still useful function though!), so rename that messy dataframe

bur1f <- left_join(bur1e, baggs, by = "house")


##### Merge with location data #####

bur1f$country <- "burkina_faso" # Country name for later merge

bur1f <- bur1f %>%
  select(house, prov, vill, country, date, hh_size, migrant_num, wealth_index, remit) %>% # Simplify and reduce participant identifiability
  rename(loc_meso = prov, loc_micro = vill) # For merging

loc <- read_csv("burkina locations_micro.csv") # Authors AP and HJ coded the lat/long of the provincial capital for each province; import those data to merge with data on household remittances

loc <- mutate(loc, landmark = NULL, notes = NULL) # Remove AP's notes about how she got lat/long for each

bur1g <- left_join(bur1f, loc, by = c("loc_micro" = "location")) # Merge province location with household data

bur1g <- bur1g %>%
  rename(district = loc_meso, census_tract = loc_micro)

##### Output #####

write_csv(bur1g, "burkina_faso.csv")
