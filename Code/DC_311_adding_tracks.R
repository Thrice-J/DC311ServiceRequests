library(sf)
library(tidyverse)
library(janitor)


DC_311_Data <- read_csv("DC_311_2009to2022.csv")

#selecting years 2012-2019 and filtering NA's out of coordinates
DC_311_Data <- DC_311_Data %>% filter(YEAR <= 2019 & YEAR >= 2010) %>% filter(!is.na(LATITUDE))

#creating coordinates only dataframe
DC_311_Data_Coords <- DC_311_Data %>% select(LONGITUDE, LATITUDE)

#getting census tracts
dc <- tidycensus::get_acs(state = "DC", geography = "tract",  variables = "B01001_001", year = 2019, geometry = TRUE)

#getting tracks from coordinates
my_points <- data.frame(DC_311_Data_Coords) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = st_crs(dc))
my_points$tract <- st_join(my_points, dc)

#my_points

#my_points$tract$GEOID

#binding tracks to datasets
DC_311_Data_Tracts <- cbind(DC_311_Data, my_points$tract$GEOID)

#separating GEOID to get 6 digit census tract
DC_311_Data_Tracts <- DC_311_Data_Tracts %>% 
  separate(`my_points$tract$GEOID`, into = c('STATECOUNTYID', 'CENSUSTRACT'), sep = 5) 

#tidying
DC_311_Data_Tracts <- DC_311_Data_Tracts %>% 
  select(1:14, 18, 16)

#tabyl(DC_311_Data_Tracts$CENSUSTRACT)

#verifying 179 unique tracts + NAs
unique(DC_311_Data_Tracts$CENSUSTRACT)

write_csv(DC_311_Data_Tracts, "DC_311_2010to2019_with_Tracks.csv")

