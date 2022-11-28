library(tidyverse)
library(janitor)
library(lubridate)

DC_gent_tracks <- read_csv("Census_Data_2012_2019_Eligible_Gentrified.csv")

DC_gent_tracks <- DC_gent_tracks %>% separate(GEOID, into = c('state_county', 'census_tract'), sep = 5) %>%
  select(2:26)

DC_gent_tracks %>% tabyl(census_tract)

DC_311_tracks <- read_csv("top_calls_2012_2019_with_tracts.csv")

DC_311_tracks %>% tabyl(census_tract)

DC_311_Census_Join <- DC_311_tracks %>% full_join(DC_gent_tracks, by = "census_tract")

DC_311_Census_Join %>% tabyl(gentrified)

DC_311_Census_Join <- DC_311_Census_Join %>% 
  select(YEAR, census_tract, 2:14, 16:27, 34:57)

DC_311_Census_Join <- DC_311_Census_Join %>% 
  mutate(RESOLUTIONTIME = period(RESOLUTIONTIME))

DC_311_Census_Join <- DC_311_Census_Join %>% 
  mutate(RESOLUTIONDAYS = period_to_seconds(RESOLUTIONTIME)/86400) %>%
  select(1:2, 50:51, 3:7, 52, 8:49)

#glimpse(DC_311_Census_Join)

#view(DC_311_Census_Join %>% slice(1:20))

write_csv(DC_311_Census_Join, "DC_311_Gentrified_Tracks_2012_2019.csv")


