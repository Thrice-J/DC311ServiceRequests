library(tidyverse)
library(janitor)
library(tidycensus)
library(tmap)
library(tmaptools)

#Read in data set from local machine - can be found on github
DC_gent_map <- read_csv("Census_Data_2012_2019_Eligible_Gentrified.csv")

#Changing GeoID to Character
DC_gent_map <- DC_gent_map %>% mutate(GEOID = as.character(GEOID))

#Getting geometry for map
DC_geometry <- get_acs(state = "DC", geography = "tract",  variables = "B01001_001", year = 2019, geometry = TRUE)

#Joining with data by GEOID
DC_geometry <- DC_geometry %>%
  full_join(DC_gent_map, by = "GEOID")

#Selecting Relevant Variables
DC_geometry <- DC_geometry %>% 
  select(GEOID, eligible, gentrified, NAME, geometry)

#Creating Status for Maps
Map1 <- DC_geometry %>% mutate(Status = case_when(gentrified == 1 ~ "Gentrified",
                                                         eligible == 1 ~ "Eligible-Not-Gentrified",
                                                         T ~ as.character(NA)))

Map2 <- DC_geometry %>% mutate(Status = case_when(eligible == 1 ~ "Eligible-Not-Gentrified",
                                                  T ~ as.character(NA)))

tmap_mode('plot')

tm_shape(Map1) + 
  tm_polygons(col = "Status", 
              textNA="Ineligible") +
  tm_layout(frame = F,
            legend.outside = F,
            legend.text.size = 1.5) 

tm_shape(Map2) + 
  tm_polygons(col = "Status", 
              textNA="Ineligible") +
  tm_layout(frame = F,
            legend.outside = F,
            legend.text.size = 1.5) 



#Checking out basemap options
#tmap_mode('view')
#tm_shape(DC_geometry) + 
#  tm_polygons(col = "Status",
#              style = "jenks",
#              n = 5,
#              palette = "Set2") +
#  tm_basemap("OpenStreetMap.Mapnik") +
#  tm_layout(main.title = "DC Census Tracts",
#            frame = F)


#################### CREATING HISTOGRAMS ##############################

#Read in data set from local machine - can be found on github
DC_311_Data_Tracts <- read_csv("DC_311_Gentrified_Tracts_2012_2019_only.csv")

#Selected Calls including Sidewalk
selected_calls1 <- c("Bulk Collection", "Trash Collection - Missed", "Parking Enforcement", "Pothole", "Sanitation Enforcement", "Illegal Dumping", "Residential Parking Permit Violation", "Streetlight Repair Investigation", "Tree Inspection", "Abandoned Vehicle - On Public Property", "Recycling Collection - Missed", "Alley Cleaning", "Graffiti Removal", "Street Cleaning", "Sidewalk Repair")

DC_311_Data_Tracts <- DC_311_Data_Tracts %>% #filtering on selected calls
  filter(SERVICECODEDESCRIPTION %in% selected_calls1)

DC_311_Data_Graph <- DC_311_Data_Tracts %>% 
  filter(eligible == 1) %>% #filtering only eligible tracts
  filter(!is.na(RESOLUTIONDAYS)) %>% #getting rid of NA's
  select(YEAR, eligible, gentrified, SERVICECODEDESCRIPTION, RESOLUTIONDAYS, ADDDATE, RESOLUTIONDATE)


#Creating variables for use with graph
c1 <- length(subset(DC_311_Data_Graph$RESOLUTIONDAYS,
                    DC_311_Data_Graph$YEAR==2012))
c2 <- length(subset(DC_311_Data_Graph$RESOLUTIONDAYS,
                    DC_311_Data_Graph$YEAR==2019))

#Create Graph Histograms
p1 <- hist(subset(DC_311_Data_Graph$RESOLUTIONDAYS,
                  DC_311_Data_Graph$YEAR==2012), breaks=30)
p1$counts = p1$counts/c1*100

p2 <- hist(subset(DC_311_Data_Graph$RESOLUTIONDAYS,
                  DC_311_Data_Graph$YEAR==2019), breaks=30)
p2$counts = p2$counts/c2*100

#Plot Data
par(mfrow=c(1,2)) 

#2012
plot(p1,col=scales::alpha('red',.5),border=F,xlab="Resolution Days",
     ylab="% of Calls",main="2012",ylim=c(0,20),
     yaxt="n",xaxt="n")
axis(side=2, at=c(0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20),
     labels=c("0", "", "5", "", "10", "", "15", "", "20")) 
axis(side=1, at=c(0, 250, 500, 750, 1000, 1250),
     labels=c("0", "250", "500", "750", "1000", ""))

#2019
plot(p2,col=scales::alpha('red',.5),border=F,xlab="Resolution Days",
     ylab="% of Calls",main="2019",ylim=c(0,20),
     yaxt="n",xaxt="n")
axis(side=2, at=c(0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20),
     labels=c("0", "", "5", "", "10", "", "15", "", "20")) 
axis(side=1, at=c(0, 250, 500, 750, 1000, 1250),
     labels=c("0", "250", "500", "750", "1000", ""))



#filtering resolution days to 31 and under
DC_311_Data_Graph2 <- DC_311_Data_Graph %>% filter(RESOLUTIONDAYS <= 31)


#Creating variables for use with graph
c1 <- length(subset(DC_311_Data_Graph2$RESOLUTIONDAYS,
                    DC_311_Data_Graph2$YEAR==2012))
c2 <- length(subset(DC_311_Data_Graph2$RESOLUTIONDAYS,
                    DC_311_Data_Graph2$YEAR==2019))

#Create Graph Histograms
p1 <- hist(subset(DC_311_Data_Graph2$RESOLUTIONDAYS,
                  DC_311_Data_Graph2$YEAR==2012), breaks=30)
p1$counts = p1$counts/c1*100

p2 <- hist(subset(DC_311_Data_Graph2$RESOLUTIONDAYS,
                  DC_311_Data_Graph2$YEAR==2019), breaks=30)
p2$counts = p2$counts/c2*100

#Plot Data
par(mfrow=c(1,2)) 

#2012
plot(p1,col=scales::alpha('red',.5),border=F,xlab="Resolution Days",
     ylab="% of Calls",main="2012",ylim=c(0,20),
     yaxt="n",xaxt="n")
axis(side=2, at=c(0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20),
     labels=c("0", "", "5", "", "10", "", "15", "", "20")) 
axis(side=1, at=c(0, 5, 10, 15, 20, 25, 30),
     labels=c("0", "", "10", "", "20", "", "30"))

#2019
plot(p2,col=scales::alpha('red',.5),border=F,xlab="Resolution Days",
     ylab="% of Calls",main="2019",ylim=c(0,20),
     yaxt="n",xaxt="n")
axis(side=2, at=c(0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20),
     labels=c("0", "", "5", "", "10", "", "15", "", "20")) 
axis(side=1, at=c(0, 5, 10, 15, 20, 25, 30),
     labels=c("0", "", "10", "", "20", "", "30"))


#Attempt using ggplot
#DC_311_Data_Graph %>% 
#  filter(YEAR == 2019) %>%
#  ggplot(aes(x=(RESOLUTIONDAYS))) +
#  geom_histogram(bins = 30) +
#  theme_bw()


#DC_311_Data_Graph %>% 
#  filter(RESOLUTIONDAYS <= 31) %>%
#  ggplot(aes(x=RESOLUTIONDAYS)) +
#  geom_histogram(bins = 30) +
#  facet_wrap(vars(YEAR)) +
#  theme_bw()



