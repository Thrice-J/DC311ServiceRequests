library(tidycensus)
library(tidyverse)

#10 <- load_variables(2010, "acs1/profile", cache = TRUE)
#View(v10)

#Setting Scientific Notation
options(scipen=999)

#Creating vector of census variables
census_vars <- c(
  Population = "B01001_001",
  MedianAge = "B01002_001",
  MedianRent = "B25064_001",
  MHI = "B19013_001",
  MedianValue = "B25077_001",
  PercEduAttain = "DP02_0067P",
  PercWhite = "DP05_0059P",
  PercBlack = "DP05_0060P",
  PercPoverty = "DP03_0128P"
)

#Creating a year range
years <- 2010:2020
names(years) <- years

#Pulling census data
census_data <- map_dfr(years, ~{
  get_acs(
    geography = "tract",
    variables = census_vars,
    state = "DC",
    year = .x
  )
}, .id = "year")

#Tidying
census_data <- census_data %>% 
  mutate(YEAR = as.double(year),
         GEOID = as.double(GEOID)) %>%
  select(YEAR, GEOID, variable, estimate)

census_data <- census_data %>% 
  pivot_wider(id_cols = c(YEAR, GEOID),
            names_from = variable,
            values_from = estimate)

#Checking NA's
map_df(census_data, ~ sum(is.na(.)))

#view(census_data)

#writing to local drive (stored on github)
write_csv(census_data, "DC_Census_2010to2020.csv")

#Variable Codes of Interest:
#B01001_001 - Total - Population
#B01002_001 - Median Age
#B25064_001 - Median Gross Rent
#B19013_001 - Median Household Income in the past 12 months (in inflation-adjusted dollars)
#B25077_001 - Median value (dollars)
#DP02_0067P - Percent - EDUCATIONAL ATTAINMENT - Percent bachelor's degree or higher
#DP05_0059P - Percent - White
#DP05_0060P - Percent - Black or African American
#DP03_0128P - Percent - PERCENTAGE OF FAMILIES AND PEOPLE WHOSE INCOME IN THE PAST 12 MONTHS IS BELOW THE POVERTY LEVEL



#Other variable codes of note
#S0101_C01_001 - Total - population
#S0101_C01_030 - Median age (years)
#S1702_C01_035 - Poverty status for familie
#B02001_002 - Total - White alone
#B02001_003 - Total - Black or African American alone
#B25063_001 - Total - Gross Rent
#B25010_001 - Average Household Size
#B05001_006 - Total - Not a U.S. citizen
#B17001_002 - Total - Income in the past 12 months below poverty level
#B25056_001 - Total - CONTRACT RENT
#B14001_001 - Total - SCHOOL ENROLLMENT BY LEVEL OF SCHOOL FOR THE POPULATION 3 YEARS AND OVER
#DP02_0052P - Percent - SCHOOL ENROLLMENT - Population 3 years and over enrolled in school
#DP02_0061P - Percent - EDUCATIONAL ATTAINMENT - High school graduate (includes equivalency)
#DP02_0064P - Percent - EDUCATIONAL ATTAINMENT - Bachelor's degree
#DP02_0065P - Percent - EDUCATIONAL ATTAINMENT - Graduate or professional degree
#DP02_0095P - Percent - U.S. CITIZENSHIP STATUS - Not a U.S. citizen
#DP03_0035P - Percent - INDUSTRY - Manufacturing
#DP03_0041P - Percent - INDUSTRY - Professional, scientific, and management, and administrative and waste management services
#DP03_0099P - Percent - No health insurance coverage
#DP03_0097P - Percent - With private health insurance
#DP04_0090P - Percent - Housing units with a mortgage
#DP05_0066P - Percent - Hispanic or Latino (of any race)
#S2201_C01_001 - Total - Households - Food Stamps
#S2301_C04_001 - Unemployment rate - Population 16 years and over
#S2301_C04_019 - Unemployment rate - Population 20 to 64 years
#S2503_C03_001 - Renter-occupied housing units - Occupied housing units
#S2503_C02_001 - Owner-occupied housing units - Occupied housing units

