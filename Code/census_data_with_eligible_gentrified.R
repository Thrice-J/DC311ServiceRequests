library(tidyverse)
census_data <- read_csv("DC_Census_2010to2020.csv")

census_data_2012 <- census_data %>%
  filter(YEAR == 2012) %>%
  select(-YEAR)


census_data_2019 <- census_data %>%
  filter(YEAR == 2019) %>%
  select(-YEAR)


names(census_data_2012)[2:ncol(census_data_2012)] <- paste0(names(census_data_2012)[2:ncol(census_data_2012)], "2012")
names(census_data_2019)[2:ncol(census_data_2019)] <- paste0(names(census_data_2019)[2:ncol(census_data_2019)], "2019")
census_data_2012_2019 <- inner_join(census_data_2012, census_data_2019)


#179 total tracts in DC, but 178 with population >= 500 
dim(census_data_2012_2019)

census_data_2012_2019 <- census_data_2012_2019 %>%
  filter(Population2012 >= 500)

dim(census_data_2012_2019)


# 40th percentile for MHI, home value, rent, education in 2012

mhi40th <- quantile(census_data_2012_2019$MHI2012, .4, na.rm = TRUE)

rent40th <- quantile(census_data_2012_2019$MedianRent2012, .4, na.rm = TRUE)

homevalue40th <- quantile(census_data_2012_2019$MedianValue2012, .4, na.rm = TRUE)

education40th <- quantile(census_data_2012_2019$PercEduAttain2012, .4, na.rm = TRUE)


############ IDENTIFYING TRACTS AS ELIGIBLE FOR GENTRIFICATION ###############


# Eligibility based on MHI, home value, and education (54 eligible):

census_data_2012_2019 <- census_data_2012_2019 %>%
  mutate(eligible = case_when(((MHI2012 <= mhi40th) & (MedianValue2012 <= homevalue40th) & (PercEduAttain2012 <= education40th)) ~ 1, 
                                   TRUE ~ 0))

sum(census_data_2012_2019$eligible)


############# CALCULATING RELATIVE GROWTH ##################

# adjusting for inflation, assuming dollar in 2012 is worth 10.19% less than a dollar in 2019


census_data_2012_2019 <- census_data_2012_2019 %>%
  mutate(MHI_rel_growth = ((MHI2019*.8981)/MHI2012) -1) %>%
  mutate(MedianValue_rel_growth = ((MedianValue2019 *.8981)/MedianValue2012) -1) %>%
  mutate(MedianRent_rel_growth = ((MedianRent2019 *.8981)/MedianRent2012) -1) %>%
  mutate(PercEduAttain_rel_growth = (PercEduAttain2019/PercEduAttain2012) -1)


#### Calculating median relative growth for each indicator #####


mhi_rel_50th <-quantile(census_data_2012_2019$MHI_rel_growth, .5, na.rm = TRUE)
rent_rel_50th <- quantile(census_data_2012_2019$MedianRent_rel_growth, .5, na.rm = TRUE)
homevalue_rel_50th <- quantile(census_data_2012_2019$MedianValue_rel_growth, .5,  na.rm = TRUE)
education_rel_50th<- quantile(census_data_2012_2019$PercEduAttain_rel_growth, .5,  na.rm = TRUE)


########### Classifying tracts as gentrified, using 40th percentile for eligible and 50th percentile for gentrified #######

# results in 15 gentrified, out of 54 eligible

census_data_2012_2019 <- census_data_2012_2019 %>%
  mutate(gentrified = eligible & MHI_rel_growth >= mhi_rel_50th & MedianValue_rel_growth >= homevalue_rel_50th & PercEduAttain_rel_growth >= education_rel_50th) 


sum(census_data_2012_2019$gentrified, na.rm = TRUE)


census_data_2012_2019.1 <- census_data_2012_2019 %>%
  mutate(gentrified = case_when((gentrified == TRUE) ~ 1, 
                              TRUE ~ 0))

# check numbers

sum(census_data_2012_2019.1$eligible)
sum(census_data_2012_2019.1$gentrified)

#view(census_data_2012_2019.1)

#reorder columns

census_data_2012_2019.1 <- census_data_2012_2019.1 %>%
  select(-eligible, everything())


census_data_2012_2019.1 <- census_data_2012_2019.1 %>%
  select(-gentrified, everything())

#view(census_data_2012_2019.1)


write_csv(census_data_2012_2019.1, "Census_Data_2012_2019_Eligible_Gentrified.csv")


