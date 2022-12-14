library(tidyverse)

# File stored on shared Google Drive (too big for GitHub)

calls_2010_2019 <- read_csv("DC_311_Gentrified_Tracts_2010to2019.csv")

#File stored in Data folder on GitHub

calls_2012_2019 <- read_csv("DC_311_Gentrified_Tracts_2012_2019_only.csv")


calls_2010_2019 <- calls_2010_2019 %>%
  filter(!is.na(CENSUSTRACT)) %>%
  filter(!is.na(RESOLUTIONDAYS)) %>%
  filter(!CENSUSTRACT== "006202")


calls_2012_2019 <- calls_2012_2019 %>%
  filter(!is.na(CENSUSTRACT)) %>%
  filter(!is.na(RESOLUTIONDAYS)) %>%
  filter(!CENSUSTRACT== "006202")

# identifying the top call types 

calls_2012 <- calls_2012_2019 %>%
  filter(YEAR == "2012")

calls_2019 <- calls_2012_2019 %>%
  filter(YEAR == "2019")

top_calls_2012 <- calls_2012 %>%
  group_by(SERVICECODEDESCRIPTION) %>%
  count() %>%
  arrange(desc(n))

top_calls_2019 <- calls_2019 %>%
  group_by(SERVICECODEDESCRIPTION) %>%
  count() %>%
  arrange(desc(n))


names(top_calls_2012)[2] <- paste0(names(top_calls_2012)[2], "2012")
names(top_calls_2019)[2] <- paste0(names(top_calls_2019)[2], "2019")

top_calls_2012_2019 <- inner_join(top_calls_2012, top_calls_2019)   

top_calls_2012_2019 <- top_calls_2012_2019 %>%
  mutate(total_requests = n2012 + n2019) %>%
  arrange(desc(total_requests))



# selecting the relevant call types from top calls

selected_calls <- c("Bulk Collection", "Parking Enforcement", "Trash Collection - Missed", "Pothole", "Sanitation Enforcement", "Illegal Dumping",
                    "Residential Parking Permit Violation", "Streetlight Repair Investigation", "Tree Inspection", "Abandoned Vehicle - On Public Property",
                    "Recycling Collection - Missed", "Alley Cleaning", "Graffiti Removal", "Street Cleaning", "Sidewalk Repair")

n_distinct(selected_calls)


# plotting initial selection of call types and frequencies

calls_2012_2019 %>% 
  filter(SERVICECODEDESCRIPTION %in% selected_calls) %>%
  count(SERVICECODEDESCRIPTION, sort =TRUE) %>%
  mutate(SERVICECODEDESCRIPTION = reorder(SERVICECODEDESCRIPTION, n)) %>%
  #slice(1:20) %>%
  ggplot(aes(x=n, y = fct_reorder(SERVICECODEDESCRIPTION,n), label=n)) + 
  geom_point(size = 3, color="salmon1") +
  geom_text(hjust= 1.2, vjust=0.3, size=2.5) +
  labs(x="Total number of Calls (2012 and 2019)", y="Service Request") +
  #ggtitle("Initial Selection of Call Types and Frequencies") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 10))) +
  theme(axis.title.x = element_text(margin = margin(t = 25, r = 0, b = 10, l = 0))) +
  theme_bw()


# trends in selected service requests


service <- c("Bulk Collection", "Parking Enforcement", "Trash Collection - Missed", "Pothole", "Sanitation Enforcement", "Illegal Dumping",
             "Residential Parking Permit Violation", "Streetlight Repair Investigation", "Tree Inspection", "Abandoned Vehicle - On Public Property",
             "Recycling Collection - Missed", "Alley Cleaning", "Graffiti Removal", "Street Cleaning", "Sidewalk Repair")


## Plotting frequency of selected service requests by year
calls_2010_2019%>%
  filter(YEAR == 2012:2019) %>%
  filter(SERVICECODEDESCRIPTION %in% service) %>%
  group_by(SERVICECODEDESCRIPTION, YEAR) %>%
  count() %>%
  ggplot(aes(x=YEAR, y=n, color = fct_reorder(SERVICECODEDESCRIPTION, -n))) +
  geom_point(size=3) +
  geom_line(size=0.7) +
  scale_x_continuous(breaks = seq(2012, 2020, 3)) +
  scale_y_continuous(breaks = seq(0,8000,1000)) +
  labs(x="Year", y = "Number of Calls", color = "Call Type") +
  ggtitle("Frequency of Selected Service Requests by Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  theme_bw()


# Plotting average resolution time by call type
calls_2010_2019%>%
  filter(YEAR == 2012:2019) %>%
  filter(SERVICECODEDESCRIPTION %in% service) %>%
  group_by(SERVICECODEDESCRIPTION, YEAR) %>%
  summarize(AVGTIME = (mean(RESOLUTIONDAYS))) %>%
  ggplot(aes(x= YEAR, y=AVGTIME, color = fct_reorder(SERVICECODEDESCRIPTION, -AVGTIME))) +
  geom_point(size=3) +
  geom_line(size=0.7) +
  scale_x_continuous(breaks = seq(2012,2020,3)) +
  scale_y_continuous(breaks = seq(0,1200,100)) +
  labs(x="Year", y = "Average Resolution Time (Days)", color = "Call Type") +
  ggtitle("Resolution Time of Service Requests by Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  theme_bw()



# adding gentrification status column
calls_2012_2019_4status <- calls_2012_2019 %>%
  mutate(ineligible = case_when(eligible == 0 ~ 1, TRUE ~ 0), .before = eligible) %>%
  mutate(ungentrified = case_when(eligible + gentrified == 1 ~ 1, TRUE ~ 0), .before = SERVICECODEDESCRIPTION) %>%
  mutate(gentrification_status = case_when(ineligible == 1 ~ "ineligible", 
                                           gentrified == 1 ~ "gentrified", 
                                           ungentrified == 1 ~ "eligible but not gentrified"), .after = ungentrified) %>%
  mutate(gentrification_status = relevel(factor(gentrification_status), ref = "ineligible"))



## frequency of calls per tract by gentrification status (eligible-not gentrified, gentrified, ineligible) in 2012 and 2019

# before filtering for resolution time  <= 31 and before removing sidewalk

calls_2012_2019_4status %>%
  group_by(CENSUSTRACT, gentrification_status, YEAR) %>%
  count() %>%
  ungroup() %>%
  group_by(gentrification_status, YEAR) %>%
  summarise(avg_calls_per_tract = mean(n)) %>%
  ggplot(aes(x = gentrification_status, y = avg_calls_per_tract, fill = factor(YEAR))) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Gentrification status", y = "Average number of calls per tract", fill = "Year", 
       title = "Change in Call Frequency by Gentrification Status (2012 vs. 2019)")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))



## resolution days by gentrification status (eligible-not gentrified, gentrified, ineligible) in 2012 and 2019

# before filtering for resolution time  <= 31 and before removing sidewalk


calls_2012_2019_4status %>%
  group_by(gentrification_status, YEAR) %>%
  summarise(avg_resolution_time = mean(RESOLUTIONDAYS)) %>%
  ggplot(aes(x = gentrification_status, y = avg_resolution_time, fill = factor(YEAR))) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Gentrification status", y = "Average resolution time (days)", fill = "Year", 
       title = "Change in Resolution Time by Gentrification Status")+
  theme_bw() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))


#change in frequency of call type by gentrification status

calls_2012_2019_4status%>%
  filter(SERVICECODEDESCRIPTION %in% service) %>%
  group_by(CENSUSTRACT, gentrification_status, SERVICECODEDESCRIPTION, YEAR) %>%
  count() %>%
  ungroup() %>%
  group_by(gentrification_status, SERVICECODEDESCRIPTION, YEAR) %>%
  summarise(n = mean(n)) %>%
  ggplot(aes(x=YEAR, y=n, color = fct_reorder(SERVICECODEDESCRIPTION, -n))) +
  geom_point(size=3) +
  geom_line(size=0.7) +
  scale_x_continuous(breaks = c(2012, 2019)) +
  #scale_y_continuous(breaks = seq(0,8000,1000)) +
  labs(x="Year", y = "Average Number of Calls per Tract", color = "Call Type") +
  ggtitle("Frequency of Selected Service Requests by Call Type and Gentrification Status") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~gentrification_status)

suspect_types <- c("Bulk Collection", "Parking Enforcement", "Sidewalk Repair")

calls_2012_2019_4status%>%
  filter(SERVICECODEDESCRIPTION == "Sidewalk Repair") %>%
  group_by(CENSUSTRACT, gentrification_status, SERVICECODEDESCRIPTION, YEAR) %>%
  count() %>%
  ungroup() %>%
  group_by(gentrification_status, SERVICECODEDESCRIPTION, YEAR) %>%
  summarise(n = mean(n)) %>%
  ggplot(aes(x=YEAR, y=n, color = fct_reorder(SERVICECODEDESCRIPTION, -n))) +
  geom_point(size=3) +
  geom_line(size=0.7) +
  scale_x_continuous(breaks = c(2012, 2019)) +
  #scale_y_continuous(breaks = seq(0,8000,1000)) +
  labs(x="Year", y = "Average Number of Calls per Tract", color = "Call Type") +
  ggtitle("Frequency of Sidewalk Repair Requests and Gentrification Status") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~gentrification_status)


calls_2012_2019_4status %>%
  filter(SERVICECODEDESCRIPTION  %in% suspect_types) %>%
  group_by(SERVICECODEDESCRIPTION, gentrification_status, YEAR) %>%
  summarise(avg_resolution_time = mean(RESOLUTIONDAYS)) %>%
  ggplot(aes(x = gentrification_status, y = avg_resolution_time, fill = factor(YEAR))) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Gentrification status", y = "Average resolution time (days)", fill = "Year", 
       title = "Change in Resolution Time by Gentrification Status, 2012-2019")+
  theme_bw() +
  facet_wrap(~SERVICECODEDESCRIPTION, scales = "free_y")

# there was a change in resolution time, but the change was similar in all the different groups

##  was there a sudden change resolution time for these call types, or was trend gradual? 

calls_2012_2019 %>%
  filter(SERVICECODEDESCRIPTION == "Sidewalk Repair") %>%
  group_by(YEAR) %>%
  summarise(mean = mean(RESOLUTIONDAYS))

calls_2012_2019_4status %>%
  filter(SERVICECODEDESCRIPTION == "Sidewalk Repair") %>%
  group_by(YEAR, gentrification_status) %>%
  summarise(mean = mean(RESOLUTIONDAYS))

calls_2012_2019 %>%
  filter(SERVICECODEDESCRIPTION == "Parking Enforcement") %>%
  group_by(YEAR) %>%
  summarise(mean = mean(RESOLUTIONDAYS))

calls_2012_2019_4status %>%
  filter(SERVICECODEDESCRIPTION == "Parking Enforcement") %>%
  group_by(YEAR, gentrification_status) %>%
  summarise(mean = mean(RESOLUTIONDAYS))

calls_2012_2019 %>%
  filter(SERVICECODEDESCRIPTION == "Sidewalk Repair") %>%
  group_by(YEAR) %>%
  summarise(mean = mean(RESOLUTIONDAYS))

calls_2012_2019_4status %>%
  filter(SERVICECODEDESCRIPTION == "Sidewalk Repair") %>%
  group_by(YEAR, gentrification_status) %>%
  summarise(mean = mean(RESOLUTIONDAYS))

#sharp, sudden decrease in parking enforcement from 2012-2013
calls_2010_2019 %>%
  filter(SERVICECODEDESCRIPTION == "Parking Enforcement") %>%
  group_by(YEAR) %>%
  summarise(mean = mean(RESOLUTIONDAYS))

calls_2010_2019 %>%
  filter(SERVICECODEDESCRIPTION == "Sidewalk Repair") %>%
  group_by(YEAR) %>%
  summarise(mean = mean(RESOLUTIONDAYS))

calls_2010_2019 %>%
  filter(SERVICECODEDESCRIPTION == "Bulk Collection") %>%
  group_by(YEAR) %>%
  summarise(mean = mean(RESOLUTIONDAYS))



### graphing parking enforcement, bulk collection, and sidewalk repair separately

service_parking <- c("Parking Enforcement")

calls_2010_2019%>%
  filter(YEAR == 2012:2019) %>%
  filter(SERVICECODEDESCRIPTION %in% service_parking) %>%
  group_by(YEAR) %>%
  summarize(AVGTIME = (mean(RESOLUTIONDAYS))) %>%
  ggplot(aes(x=YEAR, y=AVGTIME)) +
  geom_point(size=3) +
  geom_line(size=0.7) +
  scale_x_continuous(breaks = seq(2012,2019, 7)) +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  labs(x="Year", y = "Average Resolution Time (Days)") +
  ggtitle("Resolution Time of Parking Enforcement Requests by Year") +
  theme(plot.title = element_text(hjust = 0.5))  


service_bulk <- c("Bulk Collection")
calls_2010_2019%>%
  filter(YEAR == 2012:2019) %>%
  filter(SERVICECODEDESCRIPTION %in% service_bulk) %>%
  group_by(YEAR) %>%
  summarize(AVGTIME = (mean(RESOLUTIONDAYS))) %>%
  ggplot(aes(x=YEAR, y=AVGTIME)) +
  geom_point(size=3) +
  geom_line(size=0.7) +
  scale_x_continuous(breaks = seq(2012,2019, 7)) +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  labs(x="Year", y = "Average Resolution Time (Days)") +
  ggtitle("Resolution Time of Bulk Collection Requests by Year") +
  theme(plot.title = element_text(hjust = 0.5))  


service_sidewalk <- c("Sidewalk Repair")
calls_2010_2019%>%
  filter(YEAR == 2012:2019) %>%
  filter(SERVICECODEDESCRIPTION %in% service_sidewalk) %>%
  group_by(YEAR) %>%
  summarize(AVGTIME = (mean(RESOLUTIONDAYS))) %>%
  ggplot(aes(x=YEAR, y=AVGTIME)) +
  geom_point(size=3) +
  geom_line(size=0.7) +
  scale_x_continuous(breaks = seq(2012,2019, 7)) +
  scale_y_continuous(breaks = seq(0, 1500, 100)) +
  labs(x="Year", y = "Average Resolution Time (Days)") +
  ggtitle("Resolution Time of Sidewalk Repair Requests by Year") +
  theme(plot.title = element_text(hjust = 0.5)) 


#adding gentrification status column to 2010-2019 (year-by-year) dataframe

calls_2010_2019_4status <- calls_2010_2019 %>%
  mutate(ineligible = case_when(eligible == 0 ~ 1, TRUE ~ 0), .before = eligible) %>%
  mutate(ungentrified = case_when(eligible + gentrified == 1 ~ 1, TRUE ~ 0), .before = SERVICECODEDESCRIPTION) %>%
  mutate(gentrification_status = case_when(ineligible == 1 ~ "ineligible", gentrified == 1 ~ "gentrified", ungentrified == 1 ~ "eligible but not gentrified"), .after = ungentrified) %>%
  mutate(gentrification_status = relevel(factor(gentrification_status), ref = "ineligible"))



calls_2010_2019_4status%>%
  filter(YEAR == 2012:2019) %>%
  filter(SERVICECODEDESCRIPTION %in% service_sidewalk) %>%
  group_by(gentrification_status, YEAR) %>%
  summarize(AVGTIME = (mean(RESOLUTIONDAYS))) %>%
  ggplot(aes(x=YEAR, y=AVGTIME, color = gentrification_status)) +
  geom_point(size=3) +
  geom_line(size=0.7) +
  scale_x_continuous(breaks = seq(2012,2019, 7)) +
  scale_y_continuous(breaks = seq(0, 1500, 100)) +
  labs(x="Year", y = "Average Resolution Time (Days)", color = "Gentrification Status") +
  ggtitle("Resolution Time of Sidewalk Repair Requests") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  theme_bw()



calls_2010_2019_4status%>%
  filter(YEAR == 2012:2019) %>%
  filter(SERVICECODEDESCRIPTION %in% service_parking) %>%
  group_by(gentrification_status, YEAR) %>%
  summarize(AVGTIME = (mean(RESOLUTIONDAYS))) %>%
  ggplot(aes(x=YEAR, y=AVGTIME, color = gentrification_status)) +
  geom_point(size=3) +
  geom_line(size=0.7) +
  scale_x_continuous(breaks = seq(2012,2019, 7)) +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  labs(x="Year", y = "Average Resolution Time (Days)", color = "Gentrification Status") +
  ggtitle("Resolution Time of Parking Enforcement Requests") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  theme_bw()


calls_2010_2019_4status%>%
  filter(YEAR == 2012:2019) %>%
  filter(SERVICECODEDESCRIPTION %in% service_bulk) %>%
  group_by(gentrification_status, YEAR) %>%
  summarize(AVGTIME = (mean(RESOLUTIONDAYS))) %>%
  ggplot(aes(x=YEAR, y=AVGTIME, color = gentrification_status)) +
  geom_point(size=3) +
  geom_line(size=0.7) +
  scale_x_continuous(breaks = seq(2012,2019, 7)) +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  labs(x="Year", y = "Average Resolution Time (Days)", color = "Gentrification Status") +
  ggtitle("Resolution Time of Bulk Collection Requests") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  theme_bw()




### re-running changes in resolution time by gentrification status, with parking enforcement removed


newservice <- c("Bulk Collection", "Trash Collection - Missed", "Pothole", "Sanitation Enforcement", "Illegal Dumping",
                "Residential Parking Permit Violation", "Streetlight Repair Investigation", "Tree Inspection", "Abandoned Vehicle - On Public Property",
                "Recycling Collection - Missed", "Alley Cleaning", "Graffiti Removal", "Street Cleaning", "Sidewalk Repair")
n_distinct(newservice)

calls_2012_2019_4status %>%
  filter(SERVICECODEDESCRIPTION %in% newservice) %>%
  group_by(CENSUSTRACT, gentrification_status, YEAR) %>%
  count() %>%
  ungroup() %>%
  group_by(gentrification_status, YEAR) %>%
  summarise(avg_calls_per_tract = mean(n)) %>%
  ggplot(aes(x = gentrification_status, y = avg_calls_per_tract, fill = factor(YEAR))) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Gentrification status", y = "Average number of calls per tract", fill = "Year", 
       title = "Change in Call Frequency by Gentrification Status")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))


calls_2010_2019_4status%>%
  filter(YEAR == 2012 | YEAR == 2019) %>%
  #calls_2012_2019_4status %>%
  filter(SERVICECODEDESCRIPTION %in% newservice) %>%
  group_by(gentrification_status, YEAR) %>%
  summarise(avg_resolution_time = mean(RESOLUTIONDAYS)) %>%
  ggplot(aes(x = gentrification_status, y = avg_resolution_time, fill = factor(YEAR))) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Gentrification status", y = "Average resolution time (days)", fill = "Year", 
       title = "Change in Resolution Time by Gentrification Status")+
  theme_bw() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))


#calls_2012_2019_4status %>%
calls_2010_2019_4status%>%
  filter(YEAR %in% c(2012:2019)) %>%
  filter(SERVICECODEDESCRIPTION %in% newservice) %>%
  group_by(gentrification_status, YEAR) %>%
  summarize(AVGTIME = (mean(RESOLUTIONDAYS))) %>%
  ggplot(aes(x=YEAR, y=AVGTIME, color = gentrification_status)) +
  geom_point(size=3) +
  geom_line(size=0.7) +
  scale_x_continuous(breaks = seq(2012,2019, 2)) +
  scale_y_continuous(breaks = seq(0, 60, 2)) +
  labs(x="Year", y = "Average Resolution Time (Days)", color = "Gentrification Status") +
  ggtitle("Change in Resolution Time of by Gentrification Status") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  theme_bw()


calls_2010_2019_4status%>%
  filter(YEAR == 2012 | YEAR == 2019) %>%
  filter(SERVICECODEDESCRIPTION %in% newservice) %>%
  group_by(gentrification_status, YEAR) %>%
  summarize(AVGTIME = (mean(RESOLUTIONDAYS)))

calls_2010_2019_4status%>%
  filter(YEAR == 2012) %>%
  filter(SERVICECODEDESCRIPTION %in% newservice) %>%
  group_by(gentrification_status, YEAR) %>%
  summarize(AVGTIME = (mean(RESOLUTIONDAYS)))

calls_2010_2019_4status%>%
  filter(YEAR == 2019) %>%
  filter(SERVICECODEDESCRIPTION %in% newservice) %>%
  group_by(gentrification_status, YEAR) %>%
  summarize(AVGTIME = (mean(RESOLUTIONDAYS)))


# ######## Difference-in-difference without parking enforcement
# 
# calls_2012_2019_4status <- calls_2012_2019_4status %>%
#   mutate(Treatment = case_when(gentrified == 1 ~ 1,
#                                TRUE ~ 0), .after = gentrification_status)
# 
# calls_2012_2019_4status <- calls_2012_2019_4status %>%
#   mutate(Post = case_when(YEAR == 2019 ~ 1,
#                           TRUE ~ 0), .after = Treatment)
# 
# # checking to make sure numbers are right
# 
# treatment_tracts <- calls_2012_2019_4status %>%
#   filter(Treatment == 1)
# 
# n_distinct(treatment_tracts$CENSUSTRACT)
# 
# post_tracts <- calls_2012_2019_4status %>%
#   filter(Post == 1)
# 
# n_distinct(post_tracts$CENSUSTRACT)
# 
# 
# 
# calls_2012_2019_4status_forDID <- calls_2012_2019_4status %>%
#   filter(SERVICECODEDESCRIPTION %in% newservice) %>%
#   filter(gentrified == 1 | ungentrified == 1)
# 
# #making sure this has 0 rows
# calls_2012_2019_4status_forDID %>%
#   filter(ineligible == 1)
# 
# #making sure this is 15
# calls_2012_2019_4status_forDID %>%
#   filter(gentrified== 1) %>%
#   group_by(CENSUSTRACT) %>%
#   summarise()%>%
#   count()
# 
# 
# #making sure this is 39
# calls_2012_2019_4status_forDID %>%
#   filter(ungentrified == 1) %>%
#   group_by(CENSUSTRACT) %>%
#   summarise()%>%
#   count()
# 
# nrow(calls_2012_2019_4status_forDID)
# #View(calls_2012_2019_4status_forDID)
# 
# basic_reg = lm(RESOLUTIONDAYS ~ Treatment + Post + Treatment*Post,  data = calls_2012_2019_4status_forDID)
# basic_reg
# summary(basic_reg)
# broom :: tidy(basic_reg)
# 
# #confidence interval
# confint(basic_reg)
# 
# b <- coef(basic_reg)
# b
# 
# DIDbasicdf <- data.frame(year = rep(c(2012, 2019), each = 3), 
#                          group = rep(c("Gentrified", "Eligible-Not-Gentrified", "Counterfactual"), 2),
#                          resolutiondays = round(c( (b[1]+b[2]), (b[1]), (b[1]+b[2]), 
#                                                    (b[1] + b[2] + b[3] + b[4]), (b[1] + b[3]), (b[1]+b[2]+b[3])),2),
#                          colorgroup = c(1, 2, 1, 1, 2, 3))
# 
# 
# 
# DIDbasicdf$linetype = DIDbasicdf$group == "Counterfactual"
# ggplot(DIDbasicdf) +
#   geom_point(mapping = aes(x = year, y = resolutiondays, color = factor(colorgroup)), size = 5, pch = 1, show.legend = FALSE) +
#   geom_line(mapping = aes(x = year, y = resolutiondays, color = group, lty = linetype), show.legend = FALSE) +
#   geom_text(mapping = aes(x = year, y = resolutiondays, label = resolutiondays), size =2) +
#   geom_text(aes(x = 2020.1, y = resolutiondays, label = group, color = group), data = filter(DIDbasicdf, year == 2019), size = 2, show.legend = FALSE) +
#   scale_color_manual(values = c("light blue", "salmon", "gray", "gray", "salmon", "light blue"))+
#   scale_x_continuous(breaks = c(2012, 2019), limits = c(2012, 2021)) +
#   theme_minimal() +
#   labs(title = "Difference-in-difference (basic model)", y = "Average resolution time (in days)", x = "Year")
# 
# 
# ###### controlling for call type
# 
# control_reg = lm(RESOLUTIONDAYS ~ Treatment + Post + Treatment*Post + as.factor(SERVICECODEDESCRIPTION), data = calls_2012_2019_4status_forDID)
# control_reg
# summary(control_reg)
# broom::tidy(control_reg)
# 
# 
# #confidence interval
# confint(control_reg)
# 
# 
# 

####re-running the above with sidewalk repair removed and parking enforcement reinstated

#services_with_parking_no_sidewalk includes parking enforcement but does not include sidewalk repair
services_with_parking_no_sidewalk <- c("Bulk Collection", "Parking Enforcement", "Trash Collection - Missed", "Pothole", "Sanitation Enforcement", "Illegal Dumping",
                  "Residential Parking Permit Violation", "Streetlight Repair Investigation", "Tree Inspection", "Abandoned Vehicle - On Public Property",
                  "Recycling Collection - Missed", "Alley Cleaning", "Graffiti Removal", "Street Cleaning")

n_distinct(services_with_parking_no_sidewalk)


calls_2012_2019_4status %>%
  filter(SERVICECODEDESCRIPTION %in% services_with_parking_no_sidewalk) %>%
  filter(RESOLUTIONDAYS <= 31) %>%
  group_by(CENSUSTRACT, gentrification_status, YEAR) %>%
  count() %>%
  ungroup() %>%
  group_by(gentrification_status, YEAR) %>%
  summarise(avg_calls_per_tract = mean(n)) %>%
  ggplot(aes(x = gentrification_status, y = avg_calls_per_tract, fill = factor(YEAR))) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Gentrification status", y = "Average number of calls per tract", fill = "Year", 
       title = "Change in Call Frequency by Gentrification Status")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))



calls_2010_2019_4status%>%
  filter(YEAR == 2012 | YEAR == 2019) %>%
  filter(SERVICECODEDESCRIPTION %in% services_with_parking_no_sidewalk) %>%
  filter(RESOLUTIONDAYS <= 31) %>%
  group_by(gentrification_status, YEAR) %>%
  summarise(avg_resolution_time = mean(RESOLUTIONDAYS)) %>%
  ggplot(aes(x = gentrification_status, y = avg_resolution_time, fill = factor(YEAR))) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Gentrification status", y = "Average resolution time (days)", fill = "Year", 
       title = "Change in Resolution Time by Gentrification Status")+
  theme_bw() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))


calls_2010_2019_4status%>%
  filter(YEAR %in% c(2012:2019)) %>%
  filter(SERVICECODEDESCRIPTION %in% services_with_parking_no_sidewalk) %>%
  filter(RESOLUTIONDAYS <= 31) %>%
  group_by(gentrification_status, YEAR) %>%
  summarize(AVGTIME = (mean(RESOLUTIONDAYS))) %>%
  ggplot(aes(x=YEAR, y=AVGTIME, color = fct_reorder(gentrification_status, -AVGTIME))) +
  geom_point(size=3) +
  geom_line(size=0.7) +
  scale_x_continuous(breaks = seq(2012,2019, 2)) +
  scale_y_continuous(breaks = seq(0, 60, 2)) +
  labs(x="Year", y = "Average Resolution Time (Days)", color = "Gentrification Status") +
  ggtitle("Change in Resolution Time by Gentrification Status") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  theme_bw()



calls_2012_2019_4status %>%
  filter(SERVICECODEDESCRIPTION %in% services_with_parking_no_sidewalk) %>%
  filter(RESOLUTIONDAYS <= 31) %>%
  group_by(gentrification_status, YEAR) %>%
  summarize(avg_res_time = mean(RESOLUTIONDAYS)) %>%
  summarize(diff_res_time = )



### Difference-in-difference with filter <= 31 days, without sidewalk, but with parking enforcement

calls_2012_2019_4status <- calls_2012_2019_4status %>%
  filter(SERVICECODEDESCRIPTION %in% services_with_parking_no_sidewalk) %>%
  filter(RESOLUTIONDAYS <= 31) 

ncol(calls_2012_2019_4status)

calls_2012_2019_4status <- calls_2012_2019_4status %>%
  mutate(Treatment = case_when(gentrified == 1 ~ 1,
                               TRUE ~ 0), .after = gentrification_status)

calls_2012_2019_4status <- calls_2012_2019_4status %>%
  mutate(Post = case_when(YEAR == 2019 ~ 1,
                          TRUE ~ 0), .after = Treatment)

ncol(calls_2012_2019_4status)

# checking to make sure numbers are right

#should be 15
treatment_tracts <- calls_2012_2019_4status %>%
  filter(Treatment == 1)

n_distinct(treatment_tracts$CENSUSTRACT)

#should be 178 (removed 1 with low population from 179 at the beginning)
post_tracts <- calls_2012_2019_4status %>%
  filter(Post == 1)

n_distinct(post_tracts$CENSUSTRACT)


calls_2012_2019_4status_forDID <- calls_2012_2019_4status %>%
  filter(SERVICECODEDESCRIPTION %in% services_with_parking_no_sidewalk) %>%
  filter(gentrified == 1 | ungentrified == 1)

#making sure this has 0 rows
calls_2012_2019_4status_forDID %>%
  filter(ineligible == 1)

#making sure this is 15
calls_2012_2019_4status_forDID %>%
  filter(gentrified== 1) %>%
  group_by(CENSUSTRACT) %>%
  summarise()%>%
  count()

#making sure this is 39
calls_2012_2019_4status_forDID %>%
  filter(ungentrified == 1) %>%
  group_by(CENSUSTRACT) %>%
  summarise()%>%
  count()

basic_reg = lm(RESOLUTIONDAYS ~ Treatment + Post + Treatment*Post,  data = calls_2012_2019_4status_forDID)
basic_reg
summary(basic_reg)
broom :: tidy(basic_reg)

#confidence interval
confint(basic_reg)

b <- coef(basic_reg)
b0 <- b[1]
b1 <- b[2]
b2 <- b[3]
b3 <- b[4]

b0 + b1
b0 + b2
b0 + b1 + b2
b0 + b1 + b2 + b3

DIDbasicdf <- data.frame(year = rep(c(2012, 2019), each = 3), 
                         group = rep(c("Gentrified", "Eligible-Not-Gentrified", "Counterfactual"), 2),
                         resolutiondays = round(c( (b[1]+b[2]), (b[1]), (b[1]+b[2]), 
                                                   (b[1] + b[2] + b[3] + b[4]), (b[1] + b[3]), (b[1]+b[2]+b[3])),2),
                         colorgroup = c(1, 2, 1, 1, 2, 3))


DIDbasicdf$linetype = DIDbasicdf$group == "Counterfactual"
ggplot(DIDbasicdf) +
  geom_point(mapping = aes(x = year, y = resolutiondays, color = factor(colorgroup)), size = 5, pch = 1, show.legend = FALSE) +
  geom_line(mapping = aes(x = year, y = resolutiondays, color = group, lty = linetype), show.legend = FALSE) +
  geom_text(mapping = aes(x = year, y = resolutiondays, label = resolutiondays), size =2) +
  geom_text(aes(x = 2020.1, y = resolutiondays, label = group, color = group), data = filter(DIDbasicdf, year == 2019), size = 1.8, show.legend = FALSE) +
  scale_color_manual(values = c("light blue", "salmon", "gray", "gray", "salmon", "light blue"))+
  scale_x_continuous(breaks = c(2012, 2019), limits = c(2012, 2021)) +
  theme_minimal() +
  labs(title = "Difference-in-difference (basic model)", y = "Average resolution time (in days)", x = "Year")


# controlling for call type

control_reg = lm(RESOLUTIONDAYS ~ Treatment + Post + Treatment*Post + as.factor(SERVICECODEDESCRIPTION), data = calls_2012_2019_4status_forDID)
control_reg
summary(control_reg)
broom::tidy(control_reg)


#confidence interval
confint(control_reg)


B <- coef(control_reg)
B
B0 <- B[1]
B1 <- B[2]
B2 <- B[3]
B3 <- B[17]

B0
B1
B2
B3

B0
B0 + B1
B0 + B2
B0 + B1 + B2
B0 + B1 + B2 + B3



DID_control_df <- data.frame(year = rep(c(2012, 2019), each = 3), 
                             group = rep(c("Gentrified", "Eligible-Not-Gentrified", "Counterfactual"), 2),
                             resolutiondays = round(c( (B[1]+B[2]), (B[1]), (B[1]+B[2]), 
                                                       (B[1] + B[2] + B[3] + B[17]), (B[1] + B[3]), (B[1]+B[2]+B[3])),2),
                             colorgroup = c(1, 2, 1, 1, 2, 3))


DID_control_df$linetype = DID_control_df$group == "Counterfactual"
ggplot(DID_control_df) +
  geom_point(mapping = aes(x = year, y = resolutiondays, color = factor(colorgroup)), size = 5, pch = 1, show.legend = FALSE) +
  geom_line(mapping = aes(x = year, y = resolutiondays, color = group, lty = linetype), show.legend = FALSE) +
  geom_text(mapping = aes(x = year, y = resolutiondays, label = resolutiondays), size =2) +
  geom_text(aes(x = 2020.1, y = resolutiondays, label = group, color = group), data = filter(DID_control_df, year == 2019), size = 1.8, show.legend = FALSE) +
  scale_color_manual(values = c("light blue", "salmon", "gray", "gray", "salmon", "light blue"))+
  scale_x_continuous(breaks = c(2012, 2019), limits = c(2012, 2021)) +
  theme_minimal() +
  labs(title = "Difference-in-difference (controlling for call type)", y = "Average resolution time (in days)", x = "Year")
