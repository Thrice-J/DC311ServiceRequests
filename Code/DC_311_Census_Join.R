library(tidyverse)
library(janitor)
library(lubridate)

#Reading in local file (can be found on github)
DC_gent_tracts <- read_csv("Census_Data_2012_2019_Eligible_Gentrified.csv")

#Separting GEOID to get CENSUSTRACT variable for join
DC_gent_tracts <- DC_gent_tracts %>% 
  separate(GEOID, into = c('STATECOUNTY', 'CENSUSTRACT'), sep = 5) %>%
  dplyr::select(2:26)

#Reading in local file (can be found on shared google drive, too big for github)
DC_311_Data_with_Tracts <- read_csv("DC_311_2010to2019_with_Tracts.csv")

#178 unique tracts
#unique(DC_gent_tracts$CENSUSTRACT)

#179 unique tracts
#unique(DC_311_Data_with_Tracts$CENSUSTRACT)


#DC_gent_tracts %>% tabyl(CENSUSTRACT)          # -does not have tract 006202 
#DC_311_Data_with_Tracts %>% tabyl(CENSUSTRACT)


DC_311_Census_Join <- DC_311_Data_with_Tracts %>% #joining
  full_join(DC_gent_tracts, by = "CENSUSTRACT")


DC_311_Census_Join <- DC_311_Census_Join %>% #tidying
  dplyr::select(YEAR, CENSUSTRACT, eligible, gentrified, 2:14, 16:38)

DC_311_Census_Join <- DC_311_Census_Join %>% #changing chr to period
  mutate(RESOLUTIONTIME = period(RESOLUTIONTIME))

DC_311_Census_Join <- DC_311_Census_Join %>% #adding resolutiondays variable
  mutate(RESOLUTIONDAYS = period_to_seconds(RESOLUTIONTIME)/86400) 

DC_311_Census_Join <- DC_311_Census_Join %>% #tidying
  dplyr::select(1:9, 41, 10:40) 


#DC_311_Census_Join %>% tabyl(eligible, CENSUSTRACT)
  #Only NAs coming from tract 006202 and original tract NAs (probably outside DC)

#verifying tracks with addresses
#view(DC_311_Census_Join %>% slice(20, 1500, 2600, 8000, 200000, 400001, 600003, 800004, 1000000, 1500002, 2000003, 2300001))

#Writing to local and stored on shared google drive (too big for github)
write_csv(DC_311_Census_Join, "DC_311_Gentrified_Tracts_2010to2019.csv")

#Filtering for just 2012 and 2019
DC_311_Census_2012_2019 <- DC_311_Census_Join %>% filter(YEAR == 2012 | YEAR == 2019)

#Checking how many eligible and gentrified observations we have
#DC_311_Census_2012_2019 %>% tabyl(eligible, gentrified)

#Checking NA's
#map_df(DC_311_Census_2012_2019, ~ sum(is.na(.)))

#Writing to local and stored on github
write_csv(DC_311_Census_2012_2019, "DC_311_Gentrified_Tracts_2012_2019_only.csv")


