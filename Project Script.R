#packages needed
library(dplyr)
library(tidyr)
install.packages("readxl")
library(readxl)

#Data claering
#setup
rm(list=ls()) 
setwd("desktop") 
#loading the kingston data
King_2022<-read_excel("Kingston_2022.xlsx")
#simply the kingston data
King_2022<-King_2022 %>% select('placeID','Lat','Long','Nights.active','Cat','Fox')
King_2022<-drop_na(King_2022)
#Create trapping rates for cats and foxes in Kingston.2022.
trapping_rate_cat<-King_2022$Cat/King_2022$Nights.active
trapping_rate_fox<-King_2022$Fox/King_2022$Nights.active
King_2022<-cbind(King_2022,trapping_rate_cat,trapping_rate_fox)
write.csv(King_2022,"/Users/allenlong/Desktop/Kingston_three_parks_2022.csv")

#loading the Barnes data
Barnes_2022<-read.csv("BC22_QGIS.csv")
#simply HH data
Barnes_2022<-Barnes_2022 %>% select('placeID','Lat','Long','Nights.active','Cat','Fox')
Barnes_2022<-drop_na(Barnes_2022)
#Create trapping rates for cats and foxes, in Barnes.
trapping_rate_cat<-Barnes_2022$Cat/Barnes_2022$Nights.active
trapping_rate_fox<-Barnes_2022$Fox/Barnes_2022$Nights.active
Barnes_2022<-Barnes_2022 %>% add_column(trapping_rate_cat,trapping_rate_fox)
write.csv(Barnes_2022,"/Users/allenlong/Desktop/Barnes_2022.csv")

#loading the Hampstead heath data
Ealing_2022<-read.csv("Ealing22_GIS.csv")
#simply Ealing data
Ealing_2022<-Ealing_2022 %>% select('Place_ID','Latitude','Longitude','Nights.active','Cat','Fox')
Ealing_2022<-drop_na(Ealing_2022)
#Create trapping rates for cats and foxes
trapping_rate_cat<-Ealing_2022$Cat/Ealing_2022$Nights.active
trapping_rate_fox<-Ealing_2022$Fox/Ealing_2022$Nights.active
Ealing_2022<-Ealing_2022 %>% add_column(trapping_rate_cat,trapping_rate_fox)
write.csv(Ealing_2022,"/Users/allenlong/Desktop/Ealing_2022.csv")

#loading the Bushy park data
BP_2022<-read_excel("Bushy.Park.alt.xlsx")
#simply the Bushy park data
BP_2022<-BP_2022 %>% select('placeID','Lat','Long','Nights.active','Cat','Fox')
BP_2022<-drop_na(BP_2022)
#Create trapping rates for cats and foxes
trapping_rate_cat<-BP_2022$Cat/BP_2022$Nights.active
trapping_rate_fox<-BP_2022$Fox/BP_2022$Nights.active
BP_2022<-cbind(BP_2022,trapping_rate_cat,trapping_rate_fox)
write.csv(BP_2022,"/Users/allenlong/Desktop/Bushy_Park_alt.2022.csv")



#modelling and analysis
rm(list=ls()) 
setwd("desktop") 
#modelling to see if proximity, canopy coverage, and size affacts the trapping rate?
#Proximity
Urban_Park<-read.csv("Urban_Park.csv")

##case one, assume each variable follows a normal distribution
#assume each points are independent? Try simple general linear model for each case.
install.packages("MASS")
library(MASS)
# (Fox) Assuming 'trapping_rate' is the response variable, distance and canopy.coverage ass continuous explanatory variables
fox_glm_model <- glm(trapping_rate_fox ~ distance + canopy.coverage, data = Urban_Park, family = gaussian)
# Summary of the GLM model
summary(fox_glm_model)
# (Cats) Assuming 'trapping_rate' is the response variable, distance and canopy.coverage ass continuous explanatory variables
cat_glm_model <- glm(trapping_rate_cat ~ distance + canopy.coverage, data = Urban_Park, family = gaussian)
# Summary of the GLM model
summary(cat_glm_model)

#recheck the data by running a generalised linear mixed model glmm.
##unfortunately the trapping rate of animal, the canopy coverage are not normally distributed by nature (as they are derived from count data)
#Check data distribution
# Histogram
par(mfrow = c(2, 2))
hist(Urban_Park$trapping_rate_fox, main = "Histogram of TR_fox", xlab = "Data Values")
hist(Urban_Park$trapping_rate_cat, main = "Histogram of TR_cat", xlab = "Data Values")
hist(Urban_Park$distance, main = "Histogram of distance", xlab = "Data Values")
hist(Urban_Park$canopy.coverage, main = "Histogram of canopy.coverage", xlab = "Data Values")
# Density Plot
par(mfrow = c(2, 2))
plot(density(Urban_Park$trapping_rate_fox), main = "data density of TR_fox", xlab = "Data Values")
plot(density(Urban_Park$trapping_rate_cat), main = "data density of TR_cat", xlab = "Data Values")
plot(density(Urban_Park$distance), main = "data density of distance", xlab = "Data Values")
plot(density(Urban_Park$canopy.coverage), main = "data density of canopy.coverage", xlab = "Data Values")
#Q-Q Plot
qqnorm(Urban_Park$trapping_rate_fox)
qqnorm(Urban_Park$trapping_rate_cat)
qqnorm(Urban_Park$distance)
qqnorm(Urban_Park$canopy.coverage)
qqline(Urban_Park$trapping_rate_fox)
qqline(Urban_Park$trapping_rate_cat)
qqline(Urban_Park$distance)
qqline(Urban_Park$canopy.coverage)
# Shapiro-Wilk Test
shapiro.test(Urban_Park$trapping_rate_fox)
shapiro.test(Urban_Park$trapping_rate_cat)
shapiro.test(Urban_Park$distance)
shapiro.test(Urban_Park$canopy.coverage)
#only canopy.coverage distributes normally, sort of, but not actually.

# Assuming 'trapping_rate' as response variable and 'distance' and 'canopy.coverage' as continuous explanatory variables.
# create 'camera_trap_id' as the grouping variable representing the camera traps (random effect)
# Combine 'Park' and 'placeID' into one categorical variable 'camera_trap_id' using the interaction operator
Urban_Park$camera_trap_id <- interaction(Urban_Park$Park, Urban_Park$placeID)
# 'Urban_Park' as data frame
install.packages("Matrix")
library(Matrix)
library(lme4)
# Fit the GLMM with the combined variable 'camera_trap_id' as a random effect (foxes)
fox_glmm_model <- lmer(trapping_rate_fox ~ distance + canopy.coverage + (1 | camera_trap_id), data = Urban_Park)
summary(fox_glmm_model)
# Fit the GLMM with the combined variable 'camera_trap_id' as a random effect (cats)
cat_glmm_model <- lmer(trapping_rate_cat ~ distance + canopy.coverage + (1 | camera_trap_id), data = Urban_Park)
summary(cat_glmm_model)

#GLMM, but this time with a logit
install.packages("glmmTMB")
library(glmmTMB)
install.packages('TMB', type = 'source')
install.packages('glmmTMB', type = 'source')
library(glmmTMB)
install.packages("betareg")
library(betareg)
Fox_Beta_glmm_model <- betareg(trapping_rate_fox ~ distance + canopy.coverage + (1 | camera_trap_id), data = Urban_Park, family = betareg(link = "logit"))
library(betareg)
# Convert camera_trap_id to a factor variable
Urban_Park$camera_trap_id <- as.factor(Urban_Park$camera_trap_id)
# Check for missing values in the variables used in the model
sum(is.na(Urban_Park$trapping_rate_fox))
sum(is.na(Urban_Park$distance))
sum(is.na(Urban_Park$canopy.coverage))
sum(is.na(Urban_Park$camera_trap_id))
# Fit the beta regression model
Fox_Beta_glmm_model <- betareg(trapping_rate_fox ~ distance + canopy.coverage + (1 | camera_trap_id),
                               data = Urban_Park, link = "logit")

# Check the number of unique levels in camera_trap_id
num_levels <- length(unique(Urban_Park$camera_trap_id))
num_levels

library(dplyr)
# Check for levels with no variability in trapping_rate_fox
no_variability_levels <- Urban_Park %>%
  group_by(camera_trap_id) %>%
  summarize(variability = length(unique(trapping_rate_fox))) %>%
  filter(variability == 1)

no_variability_levels


#The attempt to use Beta GLMM
library(Matrix)
library(lme4)
library(lme4)
Fox_Beta_glmm_model <- glmer(trapping_rate_fox ~ distance + canopy.coverage + (1 | camera_trap_id), data = Urban_Park, family = beta)
# "trapping_rate_fox" and "canopy.coverage" are the response and predictor variables, respectively
# "distance" is also a predictor variable
# camera_trap_id is the random grouping variable
Fox_Beta_glmm_model <- glmer(trapping_rate_fox ~ distance + canopy.coverage + (1 | camera_trap_id),
                             data = Urban_Park,
                             family = betabinomial(link = "logit"))


#Gamma GLMM for fox trapping rate and cat trapping rate.
library(lme4)
#Check if the "Urban_park" is suitable for Gamma?
any(Urban_Park$trapping_rate_fox <= 0)
Urban_Park$trapping_rate_fox_transformed <- Urban_Park$trapping_rate_fox + 0.001
#First attempt on fox
Fox_Gamma_glmm_model <- glmer(trapping_rate_fox_transformed ~ distance + canopy.coverage +park.size + (1 | camera_trap_id),
                              data = Urban_Park,
                              family = Gamma(link = "log"))
#the result suggested too big eigenvalue, so need to be fixed.
#Check Colinearity
cor(Urban_Park[, c("trapping_rate_fox", "distance", "canopy.coverage","park.size")])
Urban_Park$distance_centered <- scale(Urban_Park$distance)
Urban_Park$canopy.coverage_centered <- scale(Urban_Park$canopy.coverage)
Urban_Park$park.size_centered <- scale(Urban_Park$park.size)
#Fit the Gama GLMM
Fox_Gamma_glmm_model <- glmer(trapping_rate_fox_transformed ~ distance_centered + canopy.coverage_centered + park.size_centered + (1 | camera_trap_id),
                              data = Urban_Park,
                              family = Gamma(link = "log"))
summary(Fox_Gamma_glmm_model)


#Repeat the same process, but substitute fox as cat.
library(lme4)
#Check if the "TR_cat" is suitable for Gamma?
any(Urban_Park$trapping_rate_cat <= 0)
Urban_Park$trapping_rate_cat_transformed <- Urban_Park$trapping_rate_cat + 0.001

#First attempt on cat
Cat_Gamma_glmm_model <- glmer(trapping_rate_cat_transformed ~ distance + park.size + canopy.coverage + (1 | camera_trap_id),
                              data = Urban_Park, 
                              family = Gamma(link = "log"))
#the result suggested too big eigenvalue, so need to be fixed.
summary (cat_glmm_model)

# Center the distance and canopy.coverage variables
Urban_Park$distance_centered <- scale(Urban_Park$distance)
Urban_Park$canopy.coverage_centered <- scale(Urban_Park$canopy.coverage)
# Fit the Gamma GLMM with convergence control
ctrl <- glmerControl(optimizer = "bobyqa", niter = 1000)
Cat_Gamma_glmm_model <- glmer(trapping_rate_cat_transformed ~ distance_centered + canopy.coverage_centered + (1 | camera_trap_id),
                              data = Urban_Park,
                              family = Gamma(link = "log"),
                              control = ctrl)
# Check for collinearity
cor(Urban_Park[, c("trapping_rate_cat_transformed", "distance", "canopy.coverage","park.size")])
# Print the model summary
summary(Cat_Gamma_glmm_model)
#The modelling result for cat was not ideal, it is possibly caused by "0-inflated bias"


# Create a new data frame foe cat and exclude 0
# Select the columns into the new data frame
selected_columns <- Urban_Park[, c("placeID", "Park", "canopy.coverage", "Lat", "Long", "Nights.active", "Cat", "trapping_rate_cat", "distance", "camera_trap_id","park.size")]
# Create a new data frame with the selected columns
Dataframe_for_cats <- data.frame(selected_columns)
#remove observations with cat count=0
Dataframe_for_cats<- Dataframe_for_cats[Dataframe_for_cats$Cat != 0, ]
# Save the new data frame as a CSV file
write.csv(Dataframe_for_cats, file = "/Users/allenlong/Desktop/new_dataframe_for_cat_filtered.csv", row.names = FALSE)

#Now repeat previous steps
#Check if the "TR_Cat" is suitable for Gamma?
any(Dataframe_for_cats$trapping_rate_cat <= 0)
Dataframe_for_cats$trapping_rate_cat_transformed <- Dataframe_for_cats$trapping_rate_cat + 0.001

#second attempt on cats
Cat_Gamma_glmm_model <- glmer(trapping_rate_cat ~ distance + canopy.coverage + (1 | camera_trap_id),
                              data = Dataframe_for_cats,
                              family = Gamma(link = "log"))
#the result suggested too big eigenvalue, so need to be fixed.

#Check Colinearity
cor(Dataframe_for_cats[, c("trapping_rate_cat", "distance", "canopy.coverage","park.size")])
Dataframe_for_cats$distance_centered <- scale(Dataframe_for_cats$distance)
Dataframe_for_cats$canopy.coverage_centered <- scale(Dataframe_for_cats$canopy.coverage)

Dataframe_for_cats<-read.csv("new_dataframe_for_cat.csv")
#Fit the Gama GLMM
Cat_Gamma_glmm_model <- glmer(trapping_rate_cat ~ distance_centered + canopy.coverage_centered + (1 | camera_trap_id),
                              data = Dataframe_for_cats,
                              family = Gamma(link = "log"))
summary(Cat_Gamma_glmm_model)
#the result was still not ideal, so maybe it will be a good ideal to center distance and canopy.coverage
# Center the distance and canopy.coverage variables
Dataframe_for_cats$distance_centered <- scale(Dataframe_for_cats$distance)
Dataframe_for_cats$canopy.coverage_centered <- scale(Dataframe_for_cats$canopy.coverage)
Dataframe_for_cats$park.size_centered <- scale(Dataframe_for_cats$park.size)


# Fit the Gamma GLMM with convergence control
ctrl <- glmerControl(optimizer = "bobyqa", niter = 1000)
Cat_Gamma_glmm_model <- glmer(trapping_rate_cat ~ distance_centered + canopy.coverage_centered + park.size_centered + (1 | camera_trap_id),
                              data = Urban_Park,
                              family = Gamma(link = "log"))
# Print the model summary
summary(Cat_Gamma_glmm_model)
#somehow many negative values were generated from the centering step, so it will be a good idea to transform them into non-negative values?


# Check for non-positive values in the 'trapping_rate_cat' variable
any(Dataframe_for_cats$trapping_rate_cat <= 0)  
# Check if there are non-positive values

# If there are non-positive values, consider transforming or removing them from the data
# For example, you can add a small positive constant to the trapping_rate_cat:
Dataframe_for_cats$trapping_rate_cat_transformed <- Dataframe_for_cats$trapping_rate_cat + 0.001

# Fit the Gamma GLMM
Cat_Gamma_glmm_model <- glmer(trapping_rate_cat_transformed ~ distance_centered  + canopy.coverage_centered + park.size_centered + (1 | camera_trap_id),
                              data = Dataframe_for_cats,
                              family = Gamma(link = "log"))
summary(Cat_Gamma_glmm_model)
#the result has not changed much.


#Try the normal one?
#check the normality of the filtered data frame for cat
shapiro.test(Dataframe_for_cats$trapping_rate_cat)
shapiro.test(Dataframe_for_cats$distance)
shapiro.test(Dataframe_for_cats$canopy.coverage)

# Fit the GLMM with the combined variable 'camera_trap_id' as a random effect (cats)
Cat_glmm_model <- lmer(trapping_rate_cat ~ distance + canopy.coverage + (1 | camera_trap_id), data = Dataframe_for_cats)
summary(Cat_glmm_model)
#the results were okay but none of the varibales are normally distributed, so Gamma GLMM should grant the best fit.







#check if size of the park may influence the trapping rate?
Urban_Park<-read.csv("Urban_Park.csv")
# Fit the GLMM with the combined variable 'camera_trap_id' as a random effect (foxes)
Urban_Park$camera_trap_id <- interaction(Urban_Park$Park, Urban_Park$placeID)
fox_glmm_model <- lmer(trapping_rate_fox ~ distance + canopy.coverage + park.size + (1 | camera_trap_id), data = Urban_Park)
summary(fox_glmm_model)


#Gamma GLMM for fox trapping rate and cat trapping rate.
library(lme4)
#Check if the "Urban_park" is suitable for Gamma?
any(Urban_Park$trapping_rate_fox <= 0)
Urban_Park$trapping_rate_fox_transformed <- Urban_Park$trapping_rate_fox + 0.001
#Check Colinearity
cor(Urban_Park[, c("trapping_rate_fox_transformed", "distance", "canopy.coverage", "park.size")])
Urban_Park$distance_centered <- scale(Urban_Park$distance)
Urban_Park$canopy.coverage_centered <- scale(Urban_Park$canopy.coverage)
Urban_Park$park.size_centered <- scale(Urban_Park$park.size)
#Fit the Gama GLMM
Fox_Gamma_glmm_model <- glmer(trapping_rate_fox_transformed ~ distance_centered + canopy.coverage_centered + park.size_centered + (1 | camera_trap_id),
                              data = Urban_Park,
                              family = Gamma(link = "log"))
summary(Fox_Gamma_glmm_model)

# Fit the GLMM with the combined variable 'camera_trap_id' as a random effect (cats)
cat_glmm_model <- lmer(trapping_rate_cat ~ distance + canopy.coverage +park.size + (1 | camera_trap_id), data = Urban_Park)
summary(cat_glmm_model)


#Maybe it will be a good idea to try every park?
##Attempt to place a Gamma GLMM on individual parks to see if our datasets was biased.
#Regent park.
Regent_park<-read_excel("Regent_Park.xlsx")
library(lme4)
any(Regent_park$trapping_rate_fox <= 0)
Regent_park$trapping_rate_fox_transformed <- Regent_park$trapping_rate_fox + 0.001

#Check Colinearity
cor(Regent_park[, c("trapping_rate_fox_transformed", "distance", "canopy.coverage")])
Regent_park$distance_centered <- scale(Regent_park$distance)
Regent_park$canopy.coverage_centered <- scale(Regent_park$canopy.coverage)
#Fit the Gama GLMM
Fox_G_RP_glmm_model <- glmer(trapping_rate_fox_transformed ~ distance_centered + canopy.coverage_centered + (1 | placeID),
                              data = Regent_park,
                              family = Gamma(link = "log"))
summary(Fox_G_RP_glmm_model)

#Hampstead Heath
HH<-read_excel("Hampstead.H.xlsx")
library(lme4)
any(HH$trapping_rate_fox <= 0)
HH$trapping_rate_fox_transformed <- HH$trapping_rate_fox + 0.001
#Check Colinearity
cor(HH[, c("trapping_rate_fox_transformed", "distance", "canopy.coverage")])
HH$distance_centered <- scale(HH$distance)
HH$canopy.coverage_centered <- scale(HH$canopy.coverage)
#Fit the Gama GLMM
Fox_G_HH_glmm_model <- glmer(trapping_rate_fox_transformed ~ distance_centered + canopy.coverage_centered + (1 | placeID),
                             data = HH,
                             family = Gamma(link = "log"))
summary(Fox_G_HH_glmm_model)

#Barnes
Barnes<-read_excel("Barnes.xlsx")
library(lme4)
any(Barnes$trapping_rate_fox <= 0)
Barnes$trapping_rate_fox_transformed <- Barnes$trapping_rate_fox + 0.001
#Check Colinearity
cor(Barnes[, c("trapping_rate_fox_transformed", "distance", "canopy.coverage")])
Barnes$distance_centered <- scale(Barnes$distance)
Barnes$canopy.coverage_centered <- scale(Barnes$canopy.coverage)
#Fit the Gama GLMM
Fox_G_Barnes_glmm_model <- glmer(trapping_rate_fox_transformed ~ distance_centered + canopy.coverage_centered + (1 | placeID),
                             data = Barnes,
                             family = Gamma(link = "log"))
summary(Fox_G_Barnes_glmm_model)

#Kempton
Kempton<-read_excel("Kempton.xlsx")
library(lme4)
any(Kempton$trapping_rate_fox <= 0)
Kempton$trapping_rate_fox_transformed <- Kempton$trapping_rate_fox + 0.001
#Check Colinearity
cor(Kempton[, c("trapping_rate_fox_transformed", "distance", "canopy.coverage")])
Kempton$distance_centered <- scale(Kempton$distance)
Kempton$canopy.coverage_centered <- scale(Kempton$canopy.coverage)
#Fit the Gama GLMM
Fox_G_Kempton_glmm_model <- glmer(trapping_rate_fox_transformed ~ distance_centered + canopy.coverage_centered + (1 | placeID),
                                 data = Kempton,
                                 family = Gamma(link = "log"))
summary(Fox_G_Kempton_glmm_model)

#Bushy Park
Bushy_park<-read.csv("Bushy_Park_alt.2022.csv")
library(lme4)
any(Bushy_park$trapping_rate_fox <= 0)
Bushy_park$trapping_rate_fox_transformed <- Bushy_park$trapping_rate_fox + 0.001
#Check Colinearity
cor(Bushy_park[, c("trapping_rate_fox_transformed", "distance", "canopy.coverage")])
Bushy_park$distance_centered <- scale(Bushy_park$distance)
Bushy_park$canopy.coverage_centered <- scale(Bushy_park$canopy.coverage)
#Fit the Gama GLMM
Fox_G_Bushy_park_glmm_model <- glmer(trapping_rate_fox_transformed ~ distance_centered + canopy.coverage_centered + (1 | placeID),
                                  data = Bushy_park,
                                  family = Gamma(link = "log"))
summary(Fox_G_Bushy_park_glmm_model)

#GNW
GNW<-read_excel("GNW.xlsx")
library(lme4)
any(GNW$trapping_rate_fox <= 0)
GNW$trapping_rate_fox_transformed <- GNW$trapping_rate_fox + 0.001
#Check Colinearity
cor(GNW[, c("trapping_rate_fox_transformed", "distance", "canopy.coverage")])
GNW$distance_centered <- scale(GNW$distance)
GNW$canopy.coverage_centered <- scale(GNW$canopy.coverage)
#Fit the Gama GLMM
Fox_G_GNW_glmm_model <- glmer(trapping_rate_fox_transformed ~ distance_centered + canopy.coverage_centered + (1 | placeID),
                                     data = GNW,
                                     family = Gamma(link = "log"))
summary(Fox_G_GNW_glmm_model)

##none of the "individual glmm" has retuened to reasonably result, some of then cannot even generate a meaningful model!!!
##therefore, a generalised gamma glmm based on dataset "urban park" would be better 
##"Urban Park" is better as compare to individual parks, there are more observations wihtin the dataset.


#Code for figure that describes the rough distribution of the varibale, plotting them against trapping rate.
# Load the ggplot2 package
library(ggplot2)
Urban_Park<-read.csv("Urban_Park.csv")
# Load the cowplot package
library(cowplot)
# Create scatterplots for fox
plot1<-ggplot(Urban_Park, aes(x = distance, y = trapping_rate_fox)) +
  geom_point(colour="black",size=0.1) +
  labs(x = "Distance", y = "Trapping Rate of Fox") +
  ggtitle("How Edge Proximity affects Fox Trapping Rate") + 
  geom_abline(colour="red") +
  theme_minimal()
plot3<-ggplot(Urban_Park, aes(x = canopy.coverage, y = trapping_rate_fox)) +
  geom_point(colour="black",size=0.1) +
  geom_abline(colour="red") +
  labs(x = "Estimated Canopy Coverage", y = "Trapping Rate of Fox") +
  ggtitle("How Canopy Coverage affects Fox Trapping Rate") +
  theme_minimal()

#Create scatterplot for cats
plot2<-ggplot(Urban_Park, aes(x = distance, y = trapping_rate_cat)) +
  geom_point(colour="black",size=0.1) +
  geom_abline(colour="red") +
  labs(x = "Distance", y = "Trapping Rate of Cat")  +
  ggtitle("How Edge Proximity affects Cat Trapping Rate") +
  theme_minimal()
plot4<-ggplot(Urban_Park, aes(x = canopy.coverage, y = trapping_rate_cat)) +
  geom_point(colour="black",size=0.1) + 
  geom_abline(colour="red") +
  labs(x = "Estimated Canopy Coverage", y = "Trapping Rate of Cat") +
  ggtitle("How Canopy Coverage affects Cat Trapping Rate") +
  theme_minimal()

#Create scatterplot for park size in cat and fox
plot5<-ggplot(Urban_Park, aes(x = park.size, y = trapping_rate_fox)) +
  geom_point(colour="black",size=0.1) +
  geom_abline(colour="red") +
  labs(x = "park size", y = "Trapping Rate of Fox")  +
  ggtitle("How Park Size affects Fox Trapping Rate") +
  theme_minimal()
plot6<-ggplot(Urban_Park, aes(x = park.size, y = trapping_rate_cat)) +
  geom_point(colour="black",size=0.1) +
  geom_abline(colour="red") +
  labs(x = "park size", y = "Trapping Rate of Cat") +
  ggtitle("How Park Size affects Cat Trapping Rate") +
  theme_minimal()

# Arrange the plots in a grid
plot_grid(plot1, plot2, plot3, plot4,plot5, plot6,ncol = 2)

#Check model overdispersion for fox (if doubt see line 171-190)
install.packages("arm")
library(arm)
Fox_Gamma_glmm_model <- glmer(trapping_rate_fox_transformed ~ distance_centered + canopy.coverage_centered +  
                                park.size_centered + (1 | camera_trap_id), data = Urban_Park, family = Gamma(link = "log"))
summary(Fox_Gamma_glmm_model)

# Check for overdispersion
install.packages("DHARMa")
library(DHARMa)
Fox_simulated_residuals <- simulateResiduals(Fox_Gamma_glmm_model)
Fox_dharma_object <- createDHARMa(Fox_simulated_residuals)
# Assuming Fox_observed_response is your observed response variable
# and Fox_simulated_residuals is your simulated residuals
Fox_dharma_object <- createDHARMa(observedResponse = Fox_observed_response, simulatedResiduals = Fox_simulated_residuals)

###failure, as overdispersion is not shomething that often found in Gamma distribution.























