#GOV670 311 Data Cleaning

library(tidyverse)
library(lubridate)

#All yearly DC 311 Data.csv can be found on https://opendata.dc.gov/

#Reading in local file (stored on shared google drive)
complaints.2022 <- read_csv("DC_311_2022.csv")

#Mutating to change and add variables and standardizing some categorical variables
complaints.2022 <- complaints.2022 %>%
    mutate(ADDDATE = as_datetime(ADDDATE), #changing to dttm
         RESOLUTIONDATE = as_datetime(RESOLUTIONDATE), #changing to dttm
         SERVICEDUEDATE = as_datetime(SERVICEDUEDATE), #changing to dttm
         SERVICEORDERDATE = as_datetime(SERVICEORDERDATE), #changing to dttm
         WARD = as.double(WARD), #changing to dbl for row bind
         YEAR = 2022, #adding YEAR variable
         RESOLUTIONTIME = seconds_to_period(RESOLUTIONDATE - ADDDATE), #adding variable
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Canceled (Duplicate)",
                                      "Duplicate (Canceled)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Closed (Duplicate)",
                                      "Duplicate (Closed)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Closed (Transferred)",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "In Progress",
                                      "In-Progress"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Open (Duplicate)",
                                      "Duplicate (Open)"),
         PRIORITY = replace(PRIORITY, PRIORITY == "High", "Urgent"),
         PRIORITY = replace(PRIORITY, PRIORITY == "Medium", "Standard"),
         PRIORITY = replace(PRIORITY, PRIORITY == "Low", "Standard")) %>%
  select(YEAR, SERVICECODEDESCRIPTION, ORGANIZATIONACRONYM, ADDDATE, RESOLUTIONDATE,
         RESOLUTIONTIME, SERVICEORDERSTATUS, SERVICEORDERDATE, SERVICEDUEDATE, 
         PRIORITY, STREETADDRESS, ZIPCODE, LATITUDE, LONGITUDE, 
         MARADDRESSREPOSITORYID, WARD) 

#Reading in local file (stored on shared google drive)
complaints.2021 <- read_csv("DC_311_2021.csv")

#Mutating to change and add variables and standardizing some categorical variables
complaints.2021 <- complaints.2021 %>%
  mutate(ADDDATE = as_datetime(ADDDATE), #changing to dttm
         RESOLUTIONDATE = as_datetime(RESOLUTIONDATE), #changing to dttm
         SERVICEDUEDATE = as_datetime(SERVICEDUEDATE), #changing to dttm
         SERVICEORDERDATE = as_datetime(SERVICEORDERDATE), #changing to dttm
         YEAR = 2021, #adding YEAR variable
         RESOLUTIONTIME = seconds_to_period(RESOLUTIONDATE - ADDDATE), #adding variable
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Canceled (Duplicate)",
                                      "Duplicate (Canceled)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Closed (Duplicate)",
                                      "Duplicate (Closed)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Closed (Transferred)",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "In Progress",
                                      "In-Progress"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "New",
                                      NA),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Open (Duplicate)",
                                      "Duplicate (Open)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Voided (Duplicate)",
                                      "Duplicate (Voided)"),
         PRIORITY = replace(PRIORITY, PRIORITY == "High", "Urgent"),
         PRIORITY = replace(PRIORITY, PRIORITY == "Medium", "Standard"),
         PRIORITY = replace(PRIORITY, PRIORITY == "Low", "Standard")) %>%
  select(YEAR, SERVICECODEDESCRIPTION, ORGANIZATIONACRONYM, ADDDATE, RESOLUTIONDATE,
         RESOLUTIONTIME, SERVICEORDERSTATUS, SERVICEORDERDATE, SERVICEDUEDATE, 
         PRIORITY, STREETADDRESS, ZIPCODE, LATITUDE, LONGITUDE, 
         MARADDRESSREPOSITORYID, WARD) 
         

#Reading in local file (stored on shared google drive)
complaints.2020 <- read_csv("DC_311_2020.csv")

#Mutating to change and add variables and standardizing some categorical variables
complaints.2020 <- complaints.2020 %>%
  mutate(ADDDATE = as_datetime(ADDDATE), #changing to dttm
         RESOLUTIONDATE = as_datetime(RESOLUTIONDATE), #changing to dttm
         SERVICEDUEDATE = as_datetime(SERVICEDUEDATE), #changing to dttm
         SERVICEORDERDATE = as_datetime(SERVICEORDERDATE), #changing to dttm
         YEAR = 2020, #adding YEAR variable
         RESOLUTIONTIME = seconds_to_period(RESOLUTIONDATE - ADDDATE), #adding variable
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Closed (Duplicate)",
                                      "Duplicate (Closed)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Closed (Transferred)",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "In Progress",
                                      "In-Progress"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "In-Progress (Duplicate)",
                                      "Duplicate (Open)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "In Progress (Duplicate)",
                                      "Duplicate (Open)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "New",
                                      NA),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Open (Duplicate)",
                                      "Duplicate (Open)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "Voided (Duplicate)",
                                      "Duplicate (Voided)"),
         PRIORITY = replace(PRIORITY, PRIORITY == "High", "Urgent"),
         PRIORITY = replace(PRIORITY, PRIORITY == "Medium", "Standard")) %>%
  select(YEAR, SERVICECODEDESCRIPTION, ORGANIZATIONACRONYM, ADDDATE, RESOLUTIONDATE,
         RESOLUTIONTIME, SERVICEORDERSTATUS, SERVICEORDERDATE, SERVICEDUEDATE, 
         PRIORITY, STREETADDRESS, ZIPCODE, LATITUDE, LONGITUDE, 
         MARADDRESSREPOSITORYID, WARD) 

#Reading in local file (stored on shared google drive)
complaints.2019 <- read_csv("DC_311_2019.csv")

#Mutating to change and add variables and standardizing some categorical variables
complaints.2019 <- complaints.2019 %>%
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


#Reading in local file (stored on shared google drive)
complaints.2018 <- read_csv("DC_311_2018.csv")

#Mutating to change and add variables and standardizing some categorical variables
complaints.2018 <- complaints.2018 %>%
  mutate(ADDDATE = as_datetime(ADDDATE), #changing to dttm
         RESOLUTIONDATE = as_datetime(RESOLUTIONDATE), #changing to dttm
         SERVICEDUEDATE = as_datetime(SERVICEDUEDATE), #changing to dttm
         SERVICEORDERDATE = as_datetime(SERVICEORDERDATE), #changing to dttm
         YEAR = 2018, #adding YEAR variable
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
                                      SERVICEORDERSTATUS == "DISPUTE",
                                      NA),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "DISPUTE (CLOSED)",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "DISPUTE (OPEN)",
                                      "Open"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "DUPLICATE (CLOSED)",
                                      "Duplicate (Closed)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "DUPLICATE (OPENED)",
                                      "Duplicate (Open)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "IN-PROGRESS",
                                      "In-Progress"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "IN PROGRESS",
                                      "In-Progress"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "OPEN",
                                      "Open"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "VOIDED",
                                      "Voided"),
         PRIORITY = replace(PRIORITY, PRIORITY == "EMERGENCY", "Emergency"),
         PRIORITY = replace(PRIORITY, PRIORITY == "STANDARD", "Standard"),
         PRIORITY = replace(PRIORITY, PRIORITY == "URGENT", "Urgent")) %>%
  select(YEAR, SERVICECODEDESCRIPTION, ORGANIZATIONACRONYM, ADDDATE, RESOLUTIONDATE,
         RESOLUTIONTIME, SERVICEORDERSTATUS, SERVICEORDERDATE, SERVICEDUEDATE, 
         PRIORITY, STREETADDRESS, ZIPCODE, LATITUDE, LONGITUDE, 
         MARADDRESSREPOSITORYID, WARD)

#Reading in local file (stored on shared google drive)
complaints.2017 <- read_csv("DC_311_2017.csv")

#Mutating to change and add variables and standardizing some categorical variables
complaints.2017 <- complaints.2017 %>%
  mutate(ADDDATE = as_datetime(ADDDATE), #changing to dttm
         RESOLUTIONDATE = as_datetime(RESOLUTIONDATE), #changing to dttm
         SERVICEDUEDATE = as_datetime(SERVICEDUEDATE), #changing to dttm
         SERVICEORDERDATE = as_datetime(SERVICEORDERDATE), #changing to dttm
         YEAR = 2017, #adding YEAR variable
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
                                      SERVICEORDERSTATUS == "DISPUTE",
                                      NA),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "DUPLICATE (CLOSED)",
                                      "Duplicate (Closed)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "DUPLICATE (OPENED)",
                                      "Duplicate (Open)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "IN-PROGRESS",
                                      "In-Progress"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "IN PROGRESS",
                                      "In-Progress"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "OPEN",
                                      "Open"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "VOIDED",
                                      "Voided"),
         PRIORITY = replace(PRIORITY, PRIORITY == "EMERGENCY", "Emergency"),
         PRIORITY = replace(PRIORITY, PRIORITY == "ESCALATED", "Urgent"),
         PRIORITY = replace(PRIORITY, PRIORITY == "STANDARD", "Standard"),
         PRIORITY = replace(PRIORITY, PRIORITY == "URGENT", "Urgent")) %>%
  select(YEAR, SERVICECODEDESCRIPTION, ORGANIZATIONACRONYM, ADDDATE, RESOLUTIONDATE,
         RESOLUTIONTIME, SERVICEORDERSTATUS, SERVICEORDERDATE, SERVICEDUEDATE, 
         PRIORITY, STREETADDRESS, ZIPCODE, LATITUDE, LONGITUDE, 
         MARADDRESSREPOSITORYID, WARD) 

#Reading in local file (stored on shared google drive)
complaints.2016 <- read_csv("DC_311_2016.csv")

#Mutating to change and add variables and standardizing some categorical variables
complaints.2016 <- complaints.2016 %>%
  mutate(ADDDATE = as_datetime(ADDDATE), #changing to dttm
         RESOLUTIONDATE = as_datetime(RESOLUTIONDATE), #changing to dttm
         SERVICEDUEDATE = as_datetime(SERVICEDUEDATE), #changing to dttm
         SERVICEORDERDATE = as_datetime(SERVICEORDERDATE), #changing to dttm
         WARD = parse_number(WARD), #fixing WARD issues
         WARD = as.double(WARD), #changing to double for row bind
         YEAR = 2016, #adding YEAR variable
         RESOLUTIONTIME = seconds_to_period(RESOLUTIONDATE - ADDDATE), #adding variable
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "CLOSE",
                                      "Closed"),
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
                                      SERVICEORDERSTATUS == "DUPLICATE (CLOSED)",
                                      "Duplicate (Closed)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "DUPLICATE (OPENED)",
                                      "Duplicate (Open)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "IN-PROGRESS",
                                      "In-Progress"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "In Progress",
                                      "In-Progress"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "OPEN",
                                      "Open"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "VOIDED",
                                      "Voided"),
         PRIORITY = replace(PRIORITY, PRIORITY == "EMERGENCY", "Emergency"),
         PRIORITY = replace(PRIORITY, PRIORITY == "STANDARD", "Standard"),
         PRIORITY = replace(PRIORITY, PRIORITY == "URGENT", "Urgent")) %>%
  select(YEAR, SERVICECODEDESCRIPTION, ORGANIZATIONACRONYM, ADDDATE, RESOLUTIONDATE,
         RESOLUTIONTIME, SERVICEORDERSTATUS, SERVICEORDERDATE, SERVICEDUEDATE, 
         PRIORITY, STREETADDRESS, ZIPCODE, LATITUDE, LONGITUDE, 
         MARADDRESSREPOSITORYID, WARD)  

#Reading in local file (stored on shared google drive)
complaints.2015 <- read_csv("DC_311_2015.csv")

#Mutating to change and add variables and standardizing some categorical variables
complaints.2015 <- complaints.2015 %>%
  mutate(ADDDATE = as_datetime(ADDDATE), #changing to dttm
         RESOLUTIONDATE = as_datetime(RESOLUTIONDATE), #changing to dttm
         SERVICEDUEDATE = as_datetime(SERVICEDUEDATE), #changing to dttm
         SERVICEORDERDATE = as_datetime(SERVICEORDERDATE), #changing to dttm
         YEAR = 2015, #adding YEAR variable
         RESOLUTIONTIME = seconds_to_period(RESOLUTIONDATE - ADDDATE), #adding variable
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "CLOSED",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "CLOSED - INCOMPLETE INFORMATION",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "DUPCLOSD",
                                      "Duplicate (Closed)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "DUPLICATE (CLOSED)",
                                      "Duplicate (Closed)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "DUPLICATE (OPENED)",
                                      "Duplicate (Open)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "IN-PROGRESS",
                                      "In-Progress"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "IN PROGRESS",
                                      "In-Progress"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "LOCKED",
                                      NA),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "NONWORK",
                                      NA),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "OPEN",
                                      "Open"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "RESOLUTN",
                                      NA),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "VOIDED",
                                      "Voided"),
         PRIORITY = replace(PRIORITY, PRIORITY == "EMERGENCY", "Emergency"),
         PRIORITY = replace(PRIORITY, PRIORITY == "EMERGNCY", "Emergency"),
         PRIORITY = replace(PRIORITY, PRIORITY == "PRIORITY", "Urgent"),
         PRIORITY = replace(PRIORITY, PRIORITY == "STANDARD", "Standard"),
         PRIORITY = replace(PRIORITY, PRIORITY == "URGENT", "Urgent")) %>%
  select(YEAR, SERVICECODEDESCRIPTION, ORGANIZATIONACRONYM, ADDDATE, RESOLUTIONDATE,
         RESOLUTIONTIME, SERVICEORDERSTATUS, SERVICEORDERDATE, SERVICEDUEDATE, 
         PRIORITY, STREETADDRESS, ZIPCODE, LATITUDE, LONGITUDE, 
         MARADDRESSREPOSITORYID, WARD)

#Reading in local file (stored on shared google drive)
complaints.2014 <- read_csv("DC_311_2014.csv")

#Mutating to change and add variables and standardizing some categorical variables
complaints.2014 <- complaints.2014 %>%
  mutate(ADDDATE = as_datetime(ADDDATE), #changing to dttm
         RESOLUTIONDATE = as_datetime(RESOLUTIONDATE), #changing to dttm
         SERVICEDUEDATE = as_datetime(SERVICEDUEDATE), #changing to dttm
         SERVICEORDERDATE = as_datetime(SERVICEORDERDATE), #changing to dttm
         WARD = as.double(WARD), #fixing ward
         YEAR = 2014, #adding YEAR variable
         RESOLUTIONTIME = seconds_to_period(RESOLUTIONDATE - ADDDATE), #adding variable
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "CLOSED",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "OVERDUE CLOSED",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "DUPLICATE (CLOSED)",
                                      "Duplicate (Closed)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "IN-PROGRESS",
                                      "In-Progress"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "LOCKED",
                                      NA),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "NONWORK",
                                      NA),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "OPEN",
                                      "Open"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "OVERDUE OPEN",
                                      "Open"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "RESOLUTN",
                                      NA),
         PRIORITY = replace(PRIORITY, PRIORITY == "EMERGENCY", "Emergency"),
         PRIORITY = replace(PRIORITY, PRIORITY == "EMERGNCY", "Emergency"),
         PRIORITY = replace(PRIORITY, PRIORITY == "PRIOR003", "Urgent"),
         PRIORITY = replace(PRIORITY, PRIORITY == "PRIORITY", "Urgent"),
         PRIORITY = replace(PRIORITY, PRIORITY == "STANDARD", "Standard"),
         PRIORITY = replace(PRIORITY, PRIORITY == "URGENT", "Urgent")) %>%
  select(YEAR, SERVICECODEDESCRIPTION, ORGANIZATIONACRONYM, ADDDATE, RESOLUTIONDATE,
         RESOLUTIONTIME, SERVICEORDERSTATUS, SERVICEORDERDATE, SERVICEDUEDATE, 
         PRIORITY, STREETADDRESS, ZIPCODE, LATITUDE, LONGITUDE, 
         MARADDRESSREPOSITORYID, WARD)

#Reading in local file (stored on shared google drive)
complaints.2013 <- read_csv("DC_311_2013.csv")

#Mutating to change and add variables and standardizing some categorical variables
complaints.2013 <- complaints.2013 %>%
  mutate(ADDDATE = as_datetime(ADDDATE), #changing to dttm
         RESOLUTIONDATE = as_datetime(RESOLUTIONDATE), #changing to dttm
         SERVICEDUEDATE = as_datetime(SERVICEDUEDATE), #changing to dttm
         SERVICEORDERDATE = as_datetime(SERVICEORDERDATE), #changing to dttm
         WARD = as.double(WARD), #fixing ward
         YEAR = 2013, #adding YEAR variable
         RESOLUTIONTIME = seconds_to_period(RESOLUTIONDATE - ADDDATE), #adding variable
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "CLOSED",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "OVERDUE CLOSED",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "DUPLICATE (CLOSED)",
                                      "Duplicate (Closed)"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "IN-PROGRESS",
                                      "In-Progress"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "LOCKED",
                                      NA),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "OPEN",
                                      "Open"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "OVERDUE OPEN",
                                      "Open"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "RESOLUTN",
                                      NA),
         PRIORITY = replace(PRIORITY, PRIORITY == "EMERGENCY", "Emergency"),
         PRIORITY = replace(PRIORITY, PRIORITY == "EMERGNCY", "Emergency"),
         PRIORITY = replace(PRIORITY, PRIORITY == "PRIORITY", "Urgent"),
         PRIORITY = replace(PRIORITY, PRIORITY == "STANDARD", "Standard"),
         PRIORITY = replace(PRIORITY, PRIORITY == "URGENT", "Urgent")) %>%
  select(YEAR, SERVICECODEDESCRIPTION, ORGANIZATIONACRONYM, ADDDATE, RESOLUTIONDATE,
         RESOLUTIONTIME, SERVICEORDERSTATUS, SERVICEORDERDATE, SERVICEDUEDATE, 
         PRIORITY, STREETADDRESS, ZIPCODE, LATITUDE, LONGITUDE, 
         MARADDRESSREPOSITORYID, WARD)


#Reading in local file (stored on shared google drive)
complaints.2012 <- read_csv("DC_311_2012.csv")

#Mutating to change and add variables and standardizing some categorical variables
complaints.2012 <- complaints.2012 %>%
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
                                      SERVICEORDERSTATUS == "OVERDUE CLOSED",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "LOCKED",
                                      NA),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "OPEN",
                                      "Open"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "OVERDUE OPEN",
                                      "Open"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "RESOLUTN",
                                      NA),
         PRIORITY = replace(PRIORITY, PRIORITY == "EMERGENCY", "Emergency"),
         PRIORITY = replace(PRIORITY, PRIORITY == "EMERGNCY", "Emergency"),
         PRIORITY = replace(PRIORITY, PRIORITY == "PRIOR003", "Urgent"),
         PRIORITY = replace(PRIORITY, PRIORITY == "PRIORITY", "Urgent"),
         PRIORITY = replace(PRIORITY, PRIORITY == "STANDARD", "Standard"),
         PRIORITY = replace(PRIORITY, PRIORITY == "URGENT", "Urgent")) %>%
  select(YEAR, SERVICECODEDESCRIPTION, ORGANIZATIONACRONYM, ADDDATE, RESOLUTIONDATE,
         RESOLUTIONTIME, SERVICEORDERSTATUS, SERVICEORDERDATE, SERVICEDUEDATE, 
         PRIORITY, STREETADDRESS, ZIPCODE, LATITUDE, LONGITUDE, 
         MARADDRESSREPOSITORYID, WARD)

#Reading in local file (stored on shared google drive)
complaints.2011 <- read_csv("DC_311_2011.csv")

#Mutating to change and add variables and standardizing some categorical variables
complaints.2011 <- complaints.2011 %>%
  mutate(ADDDATE = as_datetime(ADDDATE), #changing to dttm
         RESOLUTIONDATE = as_datetime(RESOLUTIONDATE), #changing to dttm
         SERVICEDUEDATE = as_datetime(SERVICEDUEDATE), #changing to dttm
         SERVICEORDERDATE = as_datetime(SERVICEORDERDATE), #changing to dttm
         YEAR = 2011, #adding YEAR variable
         RESOLUTIONTIME = seconds_to_period(RESOLUTIONDATE - ADDDATE), #adding variable
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "CLOSED",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "LOCKED",
                                      NA),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "RESOLUTN",
                                      NA),
         PRIORITY = replace(PRIORITY, PRIORITY == "EMERGNCY", "Emergency"),
         PRIORITY = replace(PRIORITY, PRIORITY == "PRIOR002", "Urgent"),
         PRIORITY = replace(PRIORITY, PRIORITY == "PRIORITY", "Urgent"),
         PRIORITY = replace(PRIORITY, PRIORITY == "STANDARD", "Standard"),
         PRIORITY = replace(PRIORITY, PRIORITY == "URGENT", "Urgent")) %>%
  select(YEAR, SERVICECODEDESCRIPTION, ORGANIZATIONACRONYM, ADDDATE, RESOLUTIONDATE,
         RESOLUTIONTIME, SERVICEORDERSTATUS, SERVICEORDERDATE, SERVICEDUEDATE, 
         PRIORITY, STREETADDRESS, ZIPCODE, LATITUDE, LONGITUDE, 
         MARADDRESSREPOSITORYID, WARD)


#Reading in local file (stored on shared google drive)
complaints.2010 <- read_csv("DC_311_2010.csv")

#Mutating to change and add variables and standardizing some categorical variables
complaints.2010 <- complaints.2010 %>%
  mutate(ADDDATE = as_datetime(ADDDATE), #changing to dttm
         RESOLUTIONDATE = as_datetime(RESOLUTIONDATE), #changing to dttm
         SERVICEDUEDATE = as_datetime(SERVICEDUEDATE), #changing to dttm
         SERVICEORDERDATE = as_datetime(SERVICEORDERDATE), #changing to dttm
         YEAR = 2010, #adding YEAR variable
         RESOLUTIONTIME = seconds_to_period(RESOLUTIONDATE - ADDDATE), #adding variable
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "CLOSED",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "LOCKED",
                                      NA),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "RESOLUTN",
                                      NA),
         PRIORITY = replace(PRIORITY, PRIORITY == "EMERGNCY", "Emergency"),
         PRIORITY = replace(PRIORITY, PRIORITY == "PRIOR001", "Urgent"),
         PRIORITY = replace(PRIORITY, PRIORITY == "PRIOR002", "Urgent"),
         PRIORITY = replace(PRIORITY, PRIORITY == "PRIORITY", "Urgent"),
         PRIORITY = replace(PRIORITY, PRIORITY == "STANDARD", "Standard"),
         PRIORITY = replace(PRIORITY, PRIORITY == "URGENT", "Urgent")) %>%
  select(YEAR, SERVICECODEDESCRIPTION, ORGANIZATIONACRONYM, ADDDATE, RESOLUTIONDATE,
         RESOLUTIONTIME, SERVICEORDERSTATUS, SERVICEORDERDATE, SERVICEDUEDATE, 
         PRIORITY, STREETADDRESS, ZIPCODE, LATITUDE, LONGITUDE, 
         MARADDRESSREPOSITORYID, WARD)


#Reading in local file (stored on shared google drive)
complaints.2009 <- read_csv("DC_311_2009.csv")

#Mutating to change and add variables and standardizing some categorical variables
complaints.2009 <- complaints.2009 %>%
  mutate(ADDDATE = as_datetime(ADDDATE), #changing to dttm
         RESOLUTIONDATE = as_datetime(RESOLUTIONDATE), #changing to dttm
         SERVICEDUEDATE = as_datetime(SERVICEDUEDATE), #changing to dttm
         SERVICEORDERDATE = as_datetime(SERVICEORDERDATE), #changing to dttm
         YEAR = 2009, #adding YEAR variable
         RESOLUTIONTIME = seconds_to_period(RESOLUTIONDATE - ADDDATE), #adding variable
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "CLOSED",
                                      "Closed"),
         SERVICEORDERSTATUS = replace(SERVICEORDERSTATUS, 
                                      SERVICEORDERSTATUS == "RESOLUTN",
                                      NA),
         PRIORITY = replace(PRIORITY, PRIORITY == "EMERGNCY", "Emergency"),
         PRIORITY = replace(PRIORITY, PRIORITY == "PRIORITY", "Urgent"),
         PRIORITY = replace(PRIORITY, PRIORITY == "STANDARD", "Standard"),
         PRIORITY = replace(PRIORITY, PRIORITY == "URGENT", "Urgent")) %>%
  select(YEAR, SERVICECODEDESCRIPTION, ORGANIZATIONACRONYM, ADDDATE, RESOLUTIONDATE,
         RESOLUTIONTIME, SERVICEORDERSTATUS, SERVICEORDERDATE, SERVICEDUEDATE, 
         PRIORITY, STREETADDRESS, ZIPCODE, LATITUDE, LONGITUDE, 
         MARADDRESSREPOSITORYID, WARD)


#Combining all data
complaints.2009to2022 <- bind_rows(complaints.2022, complaints.2021, 
                                   complaints.2020, complaints.2019, 
                                   complaints.2018, complaints.2017,
                                   complaints.2016, complaints.2015,
                                   complaints.2014, complaints.2013,
                                   complaints.2012, complaints.2011,
                                   complaints.2010, complaints.2009)


#Cleaning Orangizations and Zipcodes
complaints.2009to2022 <- complaints.2009to2022 %>%
  mutate(ORGANIZATIONACRONYM = replace(ORGANIZATIONACRONYM, 
                                       ORGANIZATIONACRONYM == "OUC - SNOW",
                                       "Snow"),
         ORGANIZATIONACRONYM = replace(ORGANIZATIONACRONYM, 
                                       ORGANIZATIONACRONYM == "311 Customer Service Center",
                                       "Call Center"),
         ORGANIZATIONACRONYM = replace(ORGANIZATIONACRONYM, 
                                       ORGANIZATIONACRONYM == "311 Emergencies",
                                       "Call Center"),
         ORGANIZATIONACRONYM = replace(ORGANIZATIONACRONYM, 
                                       ORGANIZATIONACRONYM == "Department of Disability Services",
                                       "DDS"),
         ORGANIZATIONACRONYM = replace(ORGANIZATIONACRONYM, 
                                       ORGANIZATIONACRONYM == "Training Group",
                                       NA),
         ZIPCODE = replace(ZIPCODE, ZIPCODE < 20001, NA),
         ZIPCODE = replace(ZIPCODE, ZIPCODE > 20600, NA))


#Arranging by date and adding ID column for sample taking and antijoin
complaints.2009to2022 <- complaints.2009to2022 %>%
  arrange(ADDDATE) %>%
  rowid_to_column("ID")

set.seed(1234) 

#Taking a sample of data from 2015, 2018, and 2021 for exploratory work
complaints.sample <- complaints.2009to2022 %>%
  filter(YEAR %in% c(2015, 2018, 2021)) %>%
  slice_sample(n=100000)

#removing sample from original dataframe
complaints <- anti_join(complaints.2009to2022, complaints.sample, by="ID")

#removing ID column
complaints.sample <- complaints.sample %>%
  select(-ID)

#removing ID column
complaints <- complaints %>%
  select(-ID)

#writing to local drive (can be found on github)
write_csv(complaints.sample, "DC_311_Sample.csv")

#writing to local drive (can be found on shared google drive, too big for github)
write_csv(complaints, "DC_311_2009to2022.csv")
