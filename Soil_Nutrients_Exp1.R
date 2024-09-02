#Soilnutrients data anlysis harvest day (NH4,NO3, PO4)
## load libraries
library(tidyverse)
library(car)
library(emmeans)
library(ggplot2)
setwd("//ad.uws.edu.au/dfshare/HomesBLK$/90958427/My Documents/Niraj Pot Experiment/Modified Data File")

xrf<- read.csv("Pot_ExP_Data_For_ph_ec_soil.csv")
head(xrf)
###Site Descriptions####
Site_Info<-read.csv('Pot_ExP_Data_For_ph_ec_soil.csv')
Site_Info
colnames(Site_Info)[1]<-'Site'
Site_Info$Site= sub(c('ABS00'),'',Site_Info$Site)
Site_Info$Site= sub(c('ABS0'), '',Site_Info$Site)

Site_Info



# Visualize the distribution of Soil_bray_P by Dose and Plant.type
ggplot(xrf, aes(x = Dose, y = Soil_bray_P.mg.L., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Soil Bray P by Dose and Plant Type", x = "Dose", y = "Soil Bray P (mg/L)")

# Fit the model after ensuring the 'Plant.type' variable is present
m1_brayP <- lm(Soil_bray_P.mg.L. ~ Dose * Plant.type , data = xrf)
summary(m1_brayP)
# Check residuals for linearity and normality
library(car)
residualPlot(m1_brayP)  # Plot residuals vs fitted values
qqPlot(m1_brayP)         # Q-Q plot to check normality of residuals
# Test significance of the model effects
library(car)
# Type II ANOVA test to check significance of effects
Anova(m1_brayP, type = "II")
# Perform multiple comparison test to see differences within Plant.type across Doses
library(emmeans)

# Get pairwise comparisons for Dose within each Plant.type
pairwise_comparisons <- emmeans(m1_brayP, ~ Dose | Plant.type)
summary(pairwise_comparisons)

# Generate compact letter display to visualize significant differences
multcomp::cld(pairwise_comparisons)

# Compare the effects of Dose between different Plant types
plant_type_comparisons <- emmeans(m1_brayP, pairwise ~ Plant.type | Dose)
summary(plant_type_comparisons)

# Visualize the significant differences with compact letter display
multcomp::cld(plant_type_comparisons)



#sOIL_Nutrient_AMMONIUM
ggplot(xrf, aes(x = Dose, y = Soil_NH4..mg.L., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type)


# Fit the model after ensuring the 'Plant.type' variable is present
m1_nh4 <- lm(log10(Soil_NH4..mg.L.) ~ Dose * Plant.type , data = xrf)
summary(m1_nh4)
#Need to find the calibration range for ammonium and ask with Jeff.
# check residuals to see if transformation needed
residualPlot(m1_nh4)
qqPlot(m1_nh4)
# test significance
Anova(m1_nh4)
# perform multiple comparison test
library(emmeans)
# pairs(emmeans(m1_brayP, ~ Dose | Plant.type))
multcomp::cld(emmeans(m1_nh4, ~ Dose | Plant.type))



#sOIL_Nutrient_Nitrate
ggplot(xrf, aes(x = Dose, y = Soil_NO3.mg.L., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type)


# Fit the model after ensuring the 'Plant.type' variable is present
m1_no3 <- lm(log10(Soil_NO3.mg.L.) ~ Dose * Plant.type , data = xrf)
summary(m1_no3)

# check residuals to see if transformation needed
residualPlot(m1_no3)
qqPlot(m1_no3)
# test significance
Anova(m1_no3)

# pairs(emmeans(m1_brayP, ~ Dose | Plant.type))
multcomp::cld(emmeans(m1_no3, ~ Dose | Plant.type))

#now what will be the test for treatment and dose test 
# check residuals to see if transformation needed
residualPlot(m1_no3)
qqPlot(m1_no3)
# test significance
Anova(m1_no3)

# pairs(emmeans(m1_brayP, ~ Dose | Plant.type))
multcomp::cld(emmeans(m1_no3, ~ Dose | Plant.type))

