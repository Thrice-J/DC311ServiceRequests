library(tidyverse)
library(MatchIt)
library(lmtest)
library(sandwich)
library(optmatch)
library(quickmatch)
library(Matching)
library(tidycensus)
library(tmap)
library(tmaptools)
library(janitor)
library(lme4)

#Reading In From local machine (dataset can be found on GitHub)
DC_gent_tracts <- read_csv("Census_Data_2012_2019_Eligible_Gentrified.csv")

#Separting GEOID to get CENSUSTRACT variable for join
DC_gent_tracts <- DC_gent_tracts %>% separate(GEOID, into = c('STATECOUNTY', 'CENSUSTRACT'), sep = 5) %>%
  select(2:26)

#Checking for NA's
#map_df(DC_gent_tracts, ~ sum(is.na(.)))

#Filtering out NAs in 2012 data and only including eligible tracts and selecting variables needed
DC_gent_tracts <- DC_gent_tracts %>% 
  filter(!is.na(MHI2012) & !is.na(MedianRent2012) & !is.na(MedianValue2012) & eligible == 1) %>%
  select(1, 24, 25, 2:9)

#Scaling large variables for analysis
DC_gent_tracts <- DC_gent_tracts %>%
  mutate(Pop2012Scaled = Population2012/100,
         MHI2012Scaled = MHI2012/1000,
         MedRent2012Scaled = MedianRent2012/100,
         MedValue2012Scaled = MedianValue2012/10000)

#Checking original balance
m.out0 <- matchit(gentrified ~ MHI2012 + MedianValue2012 + PercEduAttain2012, data = DC_gent_tracts, method = NULL, distance ="glm")
summary(m.out0)

#Balanced Achieved!!! - 10 matched Treated and 10 Matched Control Tracts
m.out.balanced <- matchit(gentrified ~ MHI2012 + MedianValue2012 + PercEduAttain2012, data = DC_gent_tracts, method = "genetic", ratio = 1, replace = F, caliper = 0.2, pop.size=1000)
m.out.balanced
summary(m.out.balanced, un = F)

#Plot options with matchit
plot(summary(m.out.balanced), abs = FALSE)
plot(m.out.balanced, type = "jitter", interactive = FALSE)
plot(m.out.balanced, type = "hist", interactive = FALSE)
plot(m.out.balanced, type = "density", interactive = FALSE,
     which.xs = ~MHI2012 + MedianValue2012 + PercEduAttain2012)
plot(m.out.balanced, type = "qq", which.xs = ~MHI2012 + MedianValue2012 + PercEduAttain2012)
plot(m.out.balanced, type = "ecdf", which.xs = ~MHI2012 + MedianValue2012 + PercEduAttain2012)

#get_matches(m.out.balanced)

matched_data <- match.data(m.out.balanced)

#Trying NN Method with caliper - 10 matched - distance great but unbalanced
m.out1 <- matchit(gentrified ~ MHI2012 + MedianValue2012 + PercEduAttain2012, data = DC_gent_tracts, method = "nearest", ratio = 1, replace = F, distance = "glm", caliper = 0.2)
m.out1
summary(m.out1, un = F)
plot(summary(m.out1), abs = FALSE)


#Trying Method Full - 10 Treated and 18 control matched - Looks good but better balance achieved above
m.out2 <- matchit(gentrified ~ MHI2012 + MedianValue2012 + PercEduAttain2012, data = DC_gent_tracts, method = "full", distance = "glm", link = "probit", replace = F, ratio = 1, caliper = 0.1)
m.out2
summary(m.out2, un = F)
plot(summary(m.out2), var.order = "matched", abs = FALSE)

#Trying Method Optimal - Not Great - Doesn't work with caliper
m.out3 <- matchit(gentrified ~ MHI2012 + MedianValue2012 + PercEduAttain2012, data = DC_gent_tracts, method = "optimal", replace = F)
m.out3
summary(m.out3, un = F)
plot(summary(m.out3), var.order = "matched", abs = FALSE)

#Trying Method Quick - 10 Treated and 22 control matched - Looks good but better balance achieved above
m.out4 <- matchit(gentrified ~ MHI2012 + MedianValue2012 + PercEduAttain2012, data = DC_gent_tracts, method = "quick", replace = F, caliper = 0.2)
m.out4
summary(m.out4, un = F)
plot(summary(m.out4), var.order = "matched", abs = FALSE)

#adding squared and interaction - Only 6 matched - not great!
m.out5 <- matchit(gentrified ~ MHI2012Scaled + I(MHI2012Scaled^2) +  MedValue2012Scaled + I(MedValue2012Scaled^2) + PercEduAttain2012 + I(PercEduAttain2012^2) + I(PercEduAttain2012*MHI2012Scaled) + I(MHI2012Scaled*MedValue2012Scaled) + I(MedValue2012Scaled*PercEduAttain2012), data = DC_gent_tracts, method = "genetic", ratio = 1, replace = F, caliper = 0.2, pop.size=1000)
m.out5
summary(m.out5, un = F)
plot(summary(m.out5), var.order = "matched", abs = FALSE)

#adding other census tract variables - pretty balanced but only 9 matched
m.out6 <- matchit(gentrified ~ MHI2012 +  MedianValue2012 + PercEduAttain2012 + MedianRent2012 + PercWhite2012 + MedianAge2012 + Population2012, data = DC_gent_tracts,  method = "genetic", ratio = 1, replace = F, caliper = 0.2, pop.size=1000)
m.out6
summary(m.out6, un = F)
plot(summary(m.out6), var.order = "matched", abs = FALSE)


#Subclass - Matching not great
m.out7 <- matchit(gentrified ~ MHI2012Scaled + MedValue2012Scaled + PercEduAttain2012, data = DC_gent_tracts, method = "subclass", subclass = 15, replace = F)
summary(m.out7, un = FALSE)
s <- summary(m.out7, subclass = TRUE)
plot(s, var.order = "matched", abs = FALSE)


##########################Matched Data Map######################################

#Getting matched data from above
matched_data <- match.data(m.out.balanced)

matched_data <- matched_data %>%
  mutate(matched = eligible, #Creating Matched Data Column
         GEOID = paste0("11001", CENSUSTRACT)) %>% #Creating GEOID key for join
  dplyr::select(GEOID, matched) #selecting GEOID and matched column only

#Read in data set from local machine - can be found on github
DC_gent_map <- read_csv("Census_Data_2012_2019_Eligible_Gentrified.csv")

#Changing GeoID to Character
DC_gent_map <- DC_gent_map %>% mutate(GEOID = as.character(GEOID))

#Joining
DC_gent_map <- DC_gent_map %>% full_join(matched_data, by = "GEOID")

#Getting geometry for map
DC_geometry <- get_acs(state = "DC", geography = "tract",  variables = "B01001_001", year = 2019, geometry = TRUE)

#Joining with data by GEOID
DC_geometry <- DC_geometry %>%
  full_join(DC_gent_map, by = "GEOID")

#Selecting Relevant Variables
DC_geometry <- DC_geometry %>% 
  dplyr::select(GEOID, eligible, gentrified, matched, NAME, geometry)

#Creating Status for Maps
Map3 <- DC_geometry %>% mutate("Matched Tracts" = case_when((gentrified == 1 & matched == 1) ~ "Gentrified",
                                                  (eligible == 1 & matched == 1) ~ "Eligible-Not-Gentrified",
                                                  T ~ as.character(NA)))

#Graph Code
tmap_mode('plot')

tm_shape(Map3) + 
  tm_polygons(col = "Matched Tracts", 
              textNA="Unmatched/Ineligble") +
  tm_layout(frame = T,
            legend.outside = F,
            legend.text.size = 1.5) 



##################Matching DiD#######################################

#Read in data set from local machine - can be found on github
DiD_Data <- read_csv("DC_311_Gentrified_Tracts_2012_2019_only.csv")

#retreiving Matched Data
matched_data <- match.data(m.out.balanced) 

matched_data <- matched_data %>%
  mutate(matched = eligible) %>% #creating matched column
  dplyr::select(CENSUSTRACT, matched) #selecting new column and tract for join

#Joining
DiD_Data <- DiD_Data %>% full_join(matched_data, by = "CENSUSTRACT")

DiD_Data <- DiD_Data %>%
  filter(!is.na(RESOLUTIONDAYS), #filtering out NA's
         matched == 1, #Only included matched data
         RESOLUTIONDAYS <= 31) #filtering for less than 31 days

selected_calls <- c("Bulk Collection", "Trash Collection - Missed", "Parking Enforcement", "Pothole", "Sanitation Enforcement", "Illegal Dumping", "Residential Parking Permit Violation", "Streetlight Repair Investigation", "Tree Inspection", "Abandoned Vehicle - On Public Property", "Recycling Collection - Missed", "Alley Cleaning", "Graffiti Removal", "Street Cleaning")

DiD_Data <- DiD_Data %>% #filtering on selected calls
  filter(SERVICECODEDESCRIPTION %in% selected_calls1) %>%
  dplyr::select(YEAR, eligible, gentrified, matched, SERVICECODEDESCRIPTION, RESOLUTIONDAYS, ADDDATE, RESOLUTIONDATE)


#Looking at matched data
#view(DiD_Data %>% group_by(YEAR, SERVICECODEDESCRIPTION) %>%
#  summarise(Mean = mean(RESOLUTIONDAYS),
#            n = n()))

#Creating variables for DiD
DiD_Data <- DiD_Data %>%
  mutate(Treatment = gentrified,
         Post = case_when(YEAR == 2012 ~ 0,
                          YEAR == 2019 ~ 1))

#double checking for NA's
#map_df(DiD_Data, ~ sum(is.na(.)))

#Running DiD regression
reg = lm(RESOLUTIONDAYS ~ Treatment + Post + Treatment*Post,  DiD_Data)
summary(reg)


#confidence interval
confint(reg)

#Getting Regression Coefficients
b <- coef(reg)
#b

#Creating DiD Coefficients Data Frame
DIDbasicdf <- data.frame(year = rep(c(2012, 2019), each = 3), 
                         group = rep(c("Gentrified", "Eligible-Not-Gentrified", "Counterfactual"), 2),
                         resolutiondays = round(c( (b[1]+b[2]), (b[1]), (b[1]+b[2]), 
                                                   (b[1] + b[2] + b[3] + b[4]), (b[1] + b[3]), (b[1]+b[2]+b[3])),2),
                         colorgroup = c(1, 2, 1, 1, 2, 3))


#Creating DiD graph
DIDbasicdf$linetype = DIDbasicdf$group == "Counterfactual"
ggplot(DIDbasicdf) +
  geom_point(mapping = aes(x = year, y = resolutiondays, color = factor(colorgroup)), size = 5, pch = 1, show.legend = FALSE) +
  geom_line(mapping = aes(x = year, y = resolutiondays, color = group, lty = linetype), show.legend = FALSE) +
  geom_text(mapping = aes(x = year, y = resolutiondays, label = resolutiondays), size =2) +
  geom_text(aes(x = 2020.1, y = resolutiondays, label = group, color = group), data = filter(DIDbasicdf, year == 2019), size = 2, show.legend = FALSE) +
  scale_color_manual(values = c("light blue", "salmon", "gray", "gray", "salmon", "light blue"))+
  scale_x_continuous(breaks = c(2012, 2019), limits = c(2012, 2021)) +
  theme_minimal() +
  labs(title = "Difference-in-difference (matching)", y = "Average resolution time (in days)", x = "Year")


#Controlling for Service Code Type
reg2 = lm(RESOLUTIONDAYS ~ Treatment + Post + Treatment*Post + factor(SERVICECODEDESCRIPTION),  DiD_Data)
summary(reg2)

#Looking at multilevel model
reg3 = lmer(RESOLUTIONDAYS ~ Treatment + Post + Treatment*Post + (1|SERVICECODEDESCRIPTION), data = DiD_Data)
summary(reg3)
