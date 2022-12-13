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

#Reading In From local machine (dataset can be found on GitHub)
DC_gent_tracts <- read_csv("../Data/Census_Data_2012_2019_Eligible_Gentrified.csv")

#Separting GEOID to get CENSUSTRACT variable for join
DC_gent_tracts <- DC_gent_tracts %>% separate(GEOID, into = c('STATECOUNTY', 'CENSUSTRACT'), sep = 5) %>%
  dplyr::select(2:26)

#Checking for NA's
#map_df(DC_gent_tracts, ~ sum(is.na(.)))

#Filtering out NAs in 2012 data and only including eligible tracts and selecting variables needed
DC_gent_tracts <- DC_gent_tracts %>% 
  filter(!is.na(MHI2012) & !is.na(MedianRent2012) & !is.na(MedianValue2012) & eligible == 1) %>%
  dplyr::select(1, 24, 25, 2:9)

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

#Getting matched census tracts
matched_data <- match.data(m.out.balanced)

matched_data <- matched_data %>%
  mutate(matched = eligible, #Creating Matched Data Column
         GEOID = paste0("11001", CENSUSTRACT)) %>% #Creating GEOID key for join
  dplyr::select(GEOID, matched) #selecting GEOID and matched column only

#Read in data set from local machine - can be found on github
DC_gent_data <- read_csv("../Data/Census_Data_2012_2019_Eligible_Gentrified.csv")

#Changing GeoID to Character
DC_gent_data <- DC_gent_data %>% mutate(GEOID = as.character(GEOID))

#Joining
DC_matched_data <- DC_gent_data %>% full_join(matched_data, by = "GEOID")

#Tidying matched variable
DC_matched_data <- DC_matched_data %>% mutate(matched = case_when(matched == 1 ~ 1,
                                            T ~ 0))

#Ordering variables
DC_matched_data <- DC_matched_data %>% dplyr::select(1, 24:26, 2:23)

#Writing so matched data set is now on github
write_csv(DC_matched_data, "../Data/Census_Data_2012_2019_Matched.csv")

####Other Matching Attempts#####

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

#Read in data set from local machine - can be found on github
DC_matched_tracts <- read_csv("../Data/Census_Data_2012_2019_Matched.csv")

#Changing GeoID to Character
DC_matched_tracts <- DC_matched_tracts %>% mutate(GEOID = as.character(GEOID))

#Getting geometry for map
DC_geometry <- get_acs(state = "DC", geography = "tract",  variables = "B01001_001", year = 2019, geometry = TRUE)

#Joining with data by GEOID
DC_geometry <- DC_geometry %>%
  full_join(DC_matched_tracts, by = "GEOID")

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
            legend.width = 1)



##########################Matching DiD#######################################

#Read in data set from local machine - can be found on github
DiD_Data <- read_csv("../Data/DC_311_Gentrified_Tracts_2012_2019_only.csv")

#Read in data set from local machine - can be found on github
DC_matched <- read_csv("../Data/Census_Data_2012_2019_Matched.csv")

DC_matched <- DC_matched %>% separate(GEOID, into = c('STATECOUNTY', 'CENSUSTRACT'), sep = 5) %>%
  dplyr::select(CENSUSTRACT, matched) #selecting new column and tract for join

#Joining
DiD_Data_Match <- DiD_Data %>% full_join(DC_matched, by = "CENSUSTRACT") %>%
  dplyr::select(1:4, 42, 5:41) #arranging columns


DiD_Data_Match <- DiD_Data_Match %>%
  filter(matched == 1, #Only included matched data
         !is.na(RESOLUTIONDAYS), #filtering out NA's
         RESOLUTIONDAYS <= 31) #filtering for less than 31 days

selected_calls <- c("Bulk Collection", "Trash Collection - Missed", "Parking Enforcement", "Pothole", "Sanitation Enforcement", "Illegal Dumping", "Residential Parking Permit Violation", "Streetlight Repair Investigation", "Tree Inspection", "Abandoned Vehicle - On Public Property", "Recycling Collection - Missed", "Alley Cleaning", "Graffiti Removal", "Street Cleaning")

DiD_Data_Match <- DiD_Data_Match %>% #filtering on selected calls
  filter(SERVICECODEDESCRIPTION %in% selected_calls) %>%
  dplyr::select(YEAR, eligible, gentrified, matched, SERVICECODEDESCRIPTION, ORGANIZATIONACRONYM, RESOLUTIONDAYS, ADDDATE, RESOLUTIONDATE)


#Looking at matched data
#view(DiD_Data %>% group_by(YEAR, SERVICECODEDESCRIPTION) %>%
#  summarise(Mean = mean(RESOLUTIONDAYS),
#            n = n()))

#Creating variables for DiD
DiD_Data_Match <- DiD_Data_Match %>%
  mutate(Treatment = gentrified,
         Post = case_when(YEAR == 2012 ~ 0,
                          YEAR == 2019 ~ 1))

#double checking for NA's
#map_df(DiD_Data, ~ sum(is.na(.)))

#Running DiD regression
reg = lm(RESOLUTIONDAYS ~ Treatment + Post + Treatment*Post,  DiD_Data_Match)
summary(reg)


#confidence interval
confint(reg)

#Getting Regression Coefficients
b <- coef(reg)
#b

#Creating DiD Coefficients Data Frame
DIDmatchingdf <- data.frame(year = rep(c(2012, 2019), each = 3), 
                         group = rep(c("Gentrified", "Eligible-Not-Gentrified", "Counterfactual"), 2),
                         resolutiondays = round(c( (b[1]+b[2]), (b[1]), (b[1]+b[2]), 
                                                   (b[1] + b[2] + b[3] + b[4]), (b[1] + b[3]), (b[1]+b[2]+b[3])),2),
                         colorgroup = c(1, 2, 1, 1, 2, 3))


#Creating DiD graph
DIDmatchingdf$linetype = DIDmatchingdf$group == "Counterfactual"
ggplot(DIDmatchingdf) +
  geom_point(mapping = aes(x = year, y = resolutiondays, color = factor(colorgroup)), size = 5, pch = 1, show.legend = FALSE) +
  geom_line(mapping = aes(x = year, y = resolutiondays, color = group, lty = linetype), show.legend = FALSE) +
  geom_text(mapping = aes(x = year, y = resolutiondays, label = resolutiondays), size =2) +
  geom_text(aes(x = 2020.1, y = resolutiondays, label = group, color = group), data = filter(DIDmatchingdf, year == 2019), size = 1.8, show.legend = FALSE) +
  scale_color_manual(values = c("light blue", "salmon", "gray", "gray", "salmon", "light blue"))+
  scale_x_continuous(breaks = c(2012, 2019), limits = c(2012, 2021)) +
  theme_minimal() +
  labs(title = "Difference-in-difference (matching)", y = "Average resolution time (in days)", x = "Year")



##################Matching DiD -  Controlling for Call Type#####################


#Matching and Controlling for Call Type
reg2 = lm(RESOLUTIONDAYS ~ Treatment + Post + Treatment*Post + factor(SERVICECODEDESCRIPTION), 
          DiD_Data_Match)
summary(reg2)

#confidence interval
confint(reg2)

#Getting Regression Coefficients
b1 <- coef(reg2)
#b

#Creating DiD Coefficients Data Frame
DIDmatchingdf2 <- data.frame(year = rep(c(2012, 2019), each = 3), 
                            group = rep(c("Gentrified", "Eligible-Not-Gentrified", "Counterfactual"), 2),
                            resolutiondays = round(c( (b1[1]+b1[2]), (b1[1]), (b1[1]+b1[2]), 
                                                      (b1[1] + b1[2] + b1[3] + b1[17]), (b1[1] + b1[3]),
                                                      (b1[1]+b1[2]+b1[3])),2),
                            colorgroup = c(1, 2, 1, 1, 2, 3))


#Creating DiD graph
DIDmatchingdf2$linetype = DIDmatchingdf2$group == "Counterfactual"
ggplot(DIDmatchingdf2) +
  geom_point(mapping = aes(x = year, y = resolutiondays, color = factor(colorgroup)), size = 5, pch = 1, show.legend = FALSE) +
  geom_line(mapping = aes(x = year, y = resolutiondays, color = group, lty = linetype), show.legend = FALSE) +
  geom_text(mapping = aes(x = year, y = resolutiondays, label = resolutiondays), size =2) +
  geom_text(aes(x = 2020.1, y = resolutiondays, label = group, color = group), data = filter(DIDmatchingdf2, year == 2019), size = 1.8, show.legend = FALSE) +
  scale_color_manual(values = c("light blue", "salmon", "gray", "gray", "salmon", "light blue"))+
  scale_x_continuous(breaks = c(2012, 2019), limits = c(2012, 2021)) +
  theme_minimal() +
  labs(title = "Difference-in-difference (matching - controlling for call type)", y = "Average resolution time (in days)", x = "Year")


##################Matching DiD -  Individual Call Types#####################

#Looking at Bulk Collection with Matched Tracks only
DiD_Data_Bulkmatch <- DiD_Data_Match %>%
  filter(SERVICECODEDESCRIPTION == "Bulk Collection")

#Running DiD regression
reg_bulkmatch = lm(RESOLUTIONDAYS ~ Treatment + Post + Treatment*Post,  DiD_Data_Bulkmatch)
summary(reg_bulkmatch)


#confidence interval
confint(reg_bulkmatch)

#Getting Regression Coefficients
bbm <- coef(reg_bulkmatch)
#b

#Creating DiD Coefficients Data Frame
DIDbulkmatchdf <- data.frame(year = rep(c(2012, 2019), each = 3), 
                            group = rep(c("Gentrified", "Eligible-Not-Gentrified", "Counterfactual"), 2),
                            resolutiondays = round(c( (bbm[1]+bbm[2]), (bbm[1]), (bbm[1]+bbm[2]), 
                                                      (bbm[1] + bbm[2] + bbm[3] + bbm[4]), (bbm[1] + bbm[3]),
                                                      (bbm[1]+bbm[2]+bbm[3])),2),
                            colorgroup = c(1, 2, 1, 1, 2, 3))


#Creating DiD graph
DIDbulkmatchdf$linetype = DIDbulkmatchdf$group == "Counterfactual"
ggplot(DIDbulkmatchdf) +
  geom_point(mapping = aes(x = year, y = resolutiondays, color = factor(colorgroup)), size = 5, pch = 1, show.legend = FALSE) +
  geom_line(mapping = aes(x = year, y = resolutiondays, color = group, lty = linetype), show.legend = FALSE) +
  geom_text(mapping = aes(x = year, y = resolutiondays, label = resolutiondays), size =2) +
  geom_text(aes(x = 2020.1, y = resolutiondays, label = group, color = group), data = filter(DIDbulkmatchdf, year == 2019), size = 1.8, show.legend = FALSE) +
  scale_color_manual(values = c("light blue", "salmon", "gray", "gray", "salmon", "light blue"))+
  scale_x_continuous(breaks = c(2012, 2019), limits = c(2012, 2021)) +
  theme_minimal() +
  labs(title = "Difference-in-difference (matching - bulk collection)", y = "Average resolution time (in days)", x = "Year")



#Looking at Parking Enforcement with Matched Tracks only
DiD_Data_Parkmatch <- DiD_Data_Match %>%
  filter(SERVICECODEDESCRIPTION == "Parking Enforcement")

#Running DiD regression
reg_parkmatch = lm(RESOLUTIONDAYS ~ Treatment + Post + Treatment*Post,  DiD_Data_Parkmatch)
summary(reg_parkmatch)


#confidence interval
confint(reg_parkmatch)

#Getting Regression Coefficients
bpm <- coef(reg_parkmatch)
#b

#Creating DiD Coefficients Data Frame
DIDparkmatchdf <- data.frame(year = rep(c(2012, 2019), each = 3), 
                             group = rep(c("Gentrified", "Eligible-Not-Gentrified", "Counterfactual"), 2),
                             resolutiondays = round(c( (bpm[1]+bpm[2]), (bpm[1]), (bpm[1]+bpm[2]), 
                                                       (bpm[1] + bpm[2] + bpm[3] + bpm[4]), (bpm[1] + bpm[3]),
                                                       (bpm[1]+bpm[2]+bpm[3])),2),
                             colorgroup = c(1, 2, 1, 1, 2, 3))


#Creating DiD graph
DIDparkmatchdf$linetype = DIDparkmatchdf$group == "Counterfactual"
ggplot(DIDparkmatchdf) +
  geom_point(mapping = aes(x = year, y = resolutiondays, color = factor(colorgroup)), size = 5, pch = 1, show.legend = FALSE) +
  geom_line(mapping = aes(x = year, y = resolutiondays, color = group, lty = linetype), show.legend = FALSE) +
  geom_text(mapping = aes(x = year, y = resolutiondays, label = resolutiondays), size =2) +
  geom_text(aes(x = 2020.1, y = resolutiondays, label = group, color = group), data = filter(DIDparkmatchdf, year == 2019), size = 1.8, show.legend = FALSE) +
  scale_color_manual(values = c("light blue", "salmon", "gray", "gray", "salmon", "light blue"))+
  scale_x_continuous(breaks = c(2012, 2019), limits = c(2012, 2021)) +
  theme_minimal() +
  labs(title = "Difference-in-difference (matching - parking enforcement)", y = "Average resolution time (in days)", x = "Year")




