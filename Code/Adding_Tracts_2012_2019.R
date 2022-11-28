
library(censusxy)
library(lubridate)
library(stringr)
library(tidygeocoder)
library(tidyverse)

calls_2012 <- read_csv("311_City_Service_Requests_in_2012.csv")

calls_2013 <- read_csv("311_City_Service_Requests_in_2013.csv")
calls_2014 <- read_csv("311_City_Service_Requests_in_2014.csv")

calls_2019 <- read_csv("311_City_Service_Requests_in_2019.csv")


#Mutating to change and add variables and standardizing some categorical variables
calls_2012 <- calls_2012 %>%
  mutate(ADDDATE = as_datetime(ADDDATE), #changing to dttm
         RESOLUTIONDATE = as_datetime(RESOLUTIONDATE), #changing to dttm
         SERVICEDUEDATE = as_datetime(SERVICEDUEDATE), #changing to dttm
         SERVICEORDERDATE = as_datetime(SERVICEORDERDATE), #changing to dttm
         YEAR = 2012, #adding YEAR variable
         RESOLUTIONTIME = seconds_to_period(RESOLUTIONDATE - ADDDATE), #adding variable
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "CLOSED",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Closed - Incomplete Information",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "CLOSED - INCOMPLETE INFORMATION",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Closed - Incomplete Information (Duplica",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Closed (Duplicate)",
                                      "Duplicate (Closed)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Closed (Transferred)",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Dispute (Closed)",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "DUPLICATE (CLOSED)",
                                      "Duplicate (Closed)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Duplicate (Closed) (Duplicate)",
                                      "Duplicate (Closed)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "DUPLICATE (OPENED)",
                                      "Duplicate (Open)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "IN-PROGRESS",
                                      "In-Progress"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "New",
                                      NA),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "OPEN",
                                      "Open"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Open (Duplicate)",
                                      "Duplicate (Open)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "VOIDED",
                                      "Voided"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Voided (Duplicate)",
                                      "Duplicate (Voided)"),
         PRIORITY = replace(PRIORITY, PRIORITY == "High", "Urgent"),
         PRIORITY = replace(PRIORITY, PRIORITY == "Medium", "Standard"),
         PRIORITY = replace(PRIORITY, PRIORITY == "Low", "Standard"),
         PRIORITY = replace(PRIORITY, PRIORITY == "STANDARD", "Standard"),
         PRIORITY = replace(PRIORITY, PRIORITY == "URGENT", "Urgent")) %>%
  select(YEAR, SERVICECODEDESCRIPTION, ORGANIZATIONACRONYM, ADDDATE, RESOLUTIONDATE,
         RESOLUTIONTIME, SERVICEORDERSTATUS, SERVICEORDERDATE, SERVICEDUEDATE, 
         PRIORITY, STREETADDRESS, ZIPCODE, LATITUDE, LONGITUDE, 
         MARADDRESSREPOSITORYID, WARD)

View(calls_2012)





calls_2019 <- calls_2019 %>%
  mutate(ADDDATE = as_datetime(ADDDATE), #changing to dttm
         RESOLUTIONDATE = as_datetime(RESOLUTIONDATE), #changing to dttm
         SERVICEDUEDATE = as_datetime(SERVICEDUEDATE), #changing to dttm
         SERVICEORDERDATE = as_datetime(SERVICEORDERDATE), #changing to dttm
         YEAR = 2019, #adding YEAR variable
         RESOLUTIONTIME = seconds_to_period(RESOLUTIONDATE - ADDDATE), #adding variable
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "CLOSED",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Closed - Incomplete Information",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "CLOSED - INCOMPLETE INFORMATION",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Closed - Incomplete Information (Duplica",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Closed (Duplicate)",
                                      "Duplicate (Closed)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Closed (Transferred)",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Dispute (Closed)",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "DUPLICATE (CLOSED)",
                                      "Duplicate (Closed)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Duplicate (Closed) (Duplicate)",
                                      "Duplicate (Closed)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "DUPLICATE (OPENED)",
                                      "Duplicate (Open)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "IN-PROGRESS",
                                      "In-Progress"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "New",
                                      NA),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "OPEN",
                                      "Open"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Open (Duplicate)",
                                      "Duplicate (Open)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "VOIDED",
                                      "Voided"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Voided (Duplicate)",
                                      "Duplicate (Voided)"),
         PRIORITY = replace(PRIORITY, PRIORITY == "High", "Urgent"),
         PRIORITY = replace(PRIORITY, PRIORITY == "Medium", "Standard"),
         PRIORITY = replace(PRIORITY, PRIORITY == "Low", "Standard"),
         PRIORITY = replace(PRIORITY, PRIORITY == "STANDARD", "Standard"),
         PRIORITY = replace(PRIORITY, PRIORITY == "URGENT", "Urgent")) %>%
  select(YEAR, SERVICECODEDESCRIPTION, ORGANIZATIONACRONYM, ADDDATE, RESOLUTIONDATE,
         RESOLUTIONTIME, SERVICEORDERSTATUS, SERVICEORDERDATE, SERVICEDUEDATE, 
         PRIORITY, STREETADDRESS, ZIPCODE, LATITUDE, LONGITUDE, 
         MARADDRESSREPOSITORYID, WARD)

calls_2013 <- calls_2013 %>%
  mutate(ADDDATE = as_datetime(ADDDATE), #changing to dttm
         RESOLUTIONDATE = as_datetime(RESOLUTIONDATE), #changing to dttm
         SERVICEDUEDATE = as_datetime(SERVICEDUEDATE), #changing to dttm
         SERVICEORDERDATE = as_datetime(SERVICEORDERDATE), #changing to dttm
         YEAR = 2013, #adding YEAR variable
         RESOLUTIONTIME = seconds_to_period(RESOLUTIONDATE - ADDDATE), #adding variable
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "CLOSED",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Closed - Incomplete Information",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "CLOSED - INCOMPLETE INFORMATION",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Closed - Incomplete Information (Duplica",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Closed (Duplicate)",
                                      "Duplicate (Closed)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Closed (Transferred)",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Dispute (Closed)",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "DUPLICATE (CLOSED)",
                                      "Duplicate (Closed)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Duplicate (Closed) (Duplicate)",
                                      "Duplicate (Closed)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "DUPLICATE (OPENED)",
                                      "Duplicate (Open)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "IN-PROGRESS",
                                      "In-Progress"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "New",
                                      NA),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "OPEN",
                                      "Open"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Open (Duplicate)",
                                      "Duplicate (Open)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "VOIDED",
                                      "Voided"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Voided (Duplicate)",
                                      "Duplicate (Voided)"),
         PRIORITY = replace(PRIORITY, PRIORITY == "High", "Urgent"),
         PRIORITY = replace(PRIORITY, PRIORITY == "Medium", "Standard"),
         PRIORITY = replace(PRIORITY, PRIORITY == "Low", "Standard"),
         PRIORITY = replace(PRIORITY, PRIORITY == "STANDARD", "Standard"),
         PRIORITY = replace(PRIORITY, PRIORITY == "URGENT", "Urgent")) %>%
  select(YEAR, SERVICECODEDESCRIPTION, ORGANIZATIONACRONYM, ADDDATE, RESOLUTIONDATE,
         RESOLUTIONTIME, SERVICEORDERSTATUS, SERVICEORDERDATE, SERVICEDUEDATE, 
         PRIORITY, STREETADDRESS, ZIPCODE, LATITUDE, LONGITUDE, 
         MARADDRESSREPOSITORYID, WARD)

View(calls_2013)




################## WHAT CALL TYPES ARE THERE?##############


top_calls_2012 <- calls_2012 %>%
  group_by(SERVICECODEDESCRIPTION) %>%
  count() %>%
  arrange(desc(n))

top_calls_2019 <- calls_2019 %>%
  group_by(SERVICECODEDESCRIPTION) %>%
  count() %>%
  arrange(desc(n))

top_calls_2013 <- calls_2013 %>%
  group_by(SERVICECODEDESCRIPTION) %>%
  count() %>%
  arrange(desc(n))

names(top_calls_2012)[2] <- paste0(names(top_calls_2012)[2], "2012")
names(top_calls_2019)[2] <- paste0(names(top_calls_2019)[2], "2019")
names(top_calls_2013)[2] <- paste0(names(top_calls_2013)[2], "2013")

top_calls_2012_2019 <- inner_join(top_calls_2012, top_calls_2019)   
top_calls_2013_2019 <- inner_join(top_calls_2013, top_calls_2019)   


View(top_calls_2012_2019)
View(top_calls_2013_2019)


######### fixing addresses calls_2012 and calls_2019 #############

# convert street addresses with blocks to 00
calls_2012$FIXEDSTREETADDRESS <- gsub(" - \\d+ BLOCK OF", "", calls_2012$STREETADDRESS)
#also do this to take care of blocks with no dash
calls_2012$FIXEDSTREETADDRESS <- gsub(" \\d+ BLOCK OF", '', calls_2012$FIXEDSTREETADDRESS)

# convert street addresses with blocks to 00
calls_2019$FIXEDSTREETADDRESS <- gsub(" - \\d+ BLOCK OF", "", calls_2019$STREETADDRESS)
#also do this to take care of blocks with no dash
calls_2019$FIXEDSTREETADDRESS <- gsub(" \\d+ BLOCK OF", '', calls_2019$FIXEDSTREETADDRESS)


############### getting tracts for bulk_collection_2012#######################
bulk_collection_2012 <- calls_2012 %>%
  filter(SERVICECODEDESCRIPTION == "Bulk Collection")

nslices = nrow(bulk_collection_2012)/500
bulk_collection_2012$SLICE = rep(1:nslices, length.out = nrow(bulk_collection_2012))
table(bulk_collection_2012$SLICE)

split_bulk_2012 <- split(bulk_collection_2012, bulk_collection_2012$SLICE) #turning df into list of smaller df
split_builk_2012_with_tracts <- lapply(split_bulk_2012, function(x){     
  print(unique(x$SLICE))
  x %>%
    mutate(CITY = "WASHINGTON", STATE = "DC") %>%
    geocode(street = FIXEDSTREETADDRESS, city = CITY, state = STATE, method = "census", full_results = TRUE, api_options = list(census_return_type = 'geographies'))
}) #applying the function to each of the samller dfs in the list

#then run this to put them all back together:
bulk_collection_2012_with_tracts <- do.call(rbind, split_builk_2012_with_tracts)

save(bulk_collection_2012_with_tracts, file = "bulk_collection_with_tracts.RData")

write_csv(bulk_collection_2012_with_tracts, "bulk_collection_2012_with_tracts.csv")

######### getting tracts for bulk_collection_2019 #############

bulk_collection_2019 <- calls_2019 %>%
  filter(SERVICECODEDESCRIPTION == "Bulk Collection")


nslices = nrow(bulk_collection_2019)/500
bulk_collection_2019$SLICE = rep(1:nslices, length.out = nrow(bulk_collection_2019))
table(bulk_collection_2019$SLICE)

bulk_slice_101 <- bulk_collection_2019 %>%
  filter(SLICE == 101)
View(bulk_slice_101)


bulk_collection_2019_wo <- bulk_collection_2019 %>%
  filter(!SLICE == 101)


split_bulk_2019_wo <- split(bulk_collection_2019_wo, bulk_collection_2019$SLICE) #turning df into list of smaller df
split_builk_2019_with_tracts <- lapply(split_bulk_2019_wo, function(x){     
  print(unique(x$SLICE))
  x %>%
    mutate(CITY = "WASHINGTON", STATE = "DC") %>%
    geocode(street = FIXEDSTREETADDRESS, city = CITY, state = STATE, method = "census", full_results = TRUE, api_options = list(census_return_type = 'geographies'))
}) #applying the function to each of the samller dfs in the list

#then run this to put them all back together:
bulk_collection_2019_with_tracts_minus_slice_101 <- do.call(rbind, split_builk_2019_with_tracts)

save(bulk_collection_2019_with_tracts_minus_slice_101, file = "bulk_collection_2019_with_tracts_minus_slice_101.RData")

write_csv(bulk_collection_2019_with_tracts_minus_slice_101, "bulk_collection_2019_with_tracts_minus_slice_101.csv")

