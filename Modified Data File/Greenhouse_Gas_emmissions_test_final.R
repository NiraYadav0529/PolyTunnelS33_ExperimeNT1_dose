# GHG Flux Data Analysis
## Load libraries
library(tidyverse)
library(car)
library(emmeans)
library(ggplot2)
# New test for GHZs Gas
# Use double backslashes and make sure there's no leading backslash
setwd("C:\\Users\\90958427\\OneDrive - Western Sydney University\\Niraj Pot Experiment\\Modified Data File")

# Alternatively, you can use forward slashes
setwd("C:/Users/90958427/OneDrive - Western Sydney University/Niraj Pot Experiment/Modified Data File")
GHZ<- read.csv("Final_GHG_Calculation_with_Flux.csv")
head(GHZ)
GHZ_clean <- GHZ %>%
  filter(!is.na(Flux..Fm..umol.m2.min..2))

ggplot(GHZ_clean, aes(x = Treatment, y = Flux..Fm..umol.m2.min..2, colour = Treatment)) + 
  geom_boxplot() + 
  facet_wrap(~Test.Name) +
  labs(title = "Flux by Treatment and Test Name", x = "Treatment", y = "Flux (umol/m2/min)")


# Fit the model for CH4
m1_CH4 <- lm(BF.flux..umol.min.m2. ~ Treatment * Plant.type , data = GHZ[GHZ$Test.Name == "CH4", ])
summary(m1_CH4)

# Check residuals for linearity and normality
residualPlot(m1_CH4)  # Plot residuals vs fitted values
qqPlot(m1_CH4)         # Q-Q plot to check normality of residuals

# Type II ANOVA test to check significance of effects
Anova(m1_CH4, type = "II")

# Perform multiple comparison test to see differences within Plant.type across Treatments
pairwise_comparisons_CH4 <- emmeans(m1_CH4, ~ Treatment | Plant.type)
summary(pairwise_comparisons_CH4)
multcomp::cld(pairwise_comparisons_CH4)

# Compare the effects of Treatment between different Plant types for CH4
plant_type_comparisons_CH4 <- emmeans(m1_CH4, pairwise ~ Plant.type | Treatment)
summary(plant_type_comparisons_CH4)
multcomp::cld(plant_type_comparisons_CH4)

# Repeat the analysis for CO2
m1_CO2 <- lm(BF.flux..umol.min.m2. ~ Treatment * Plant.type , data = GHZ[GHZ$Test.Name == "CO2", ])
summary(m1_CO2)
residualPlot(m1_CO2)
qqPlot(m1_CO2)
Anova(m1_CO2, type = "II")
pairwise_comparisons_CO2 <- emmeans(m1_CO2, ~ Treatment | Plant.type)
summary(pairwise_comparisons_CO2)
multcomp::cld(pairwise_comparisons_CO2)
plant_type_comparisons_CO2 <- emmeans(m1_CO2, pairwise ~ Plant.type | Treatment)
summary(plant_type_comparisons_CO2)
multcomp::cld(plant_type_comparisons_CO2)

# Repeat the analysis for N2O
m1_N2O <- lm(BF.flux..umol.min.m2. ~ Treatment * Plant.type , data = GHZ[GHZ$Test.Name == "N2O", ])
summary(m1_N2O)
residualPlot(m1_N2O)
qqPlot(m1_N2O)
Anova(m1_N2O, type = "II")
pairwise_comparisons_N2O <- emmeans(m1_N2O, ~ Treatment | Plant.type)
summary(pairwise_comparisons_N2O)
multcomp::cld(pairwise_comparisons_N2O)
plant_type_comparisons_N2O <- emmeans(m1_N2O, pairwise ~ Plant.type | Treatment)
summary(plant_type_comparisons_N2O)
multcomp::cld(plant_type_comparisons_N2O)

# Now repeat the analysis for each sampling time point (1st day, 3rd day, 5th day, 7th day)

# Example for CH4 on the 1st Day post-fertilization
m1_CH4_1stDay <- lm(AF_1stday_flux..umol.min.m2. ~ Treatment * Plant.type , data = GHZ[GHZ$Test.Name == "CH4", ])
summary(m1_CH4_1stDay)
residualPlot(m1_CH4_1stDay)
qqPlot(m1_CH4_1stDay)
Anova(m1_CH4_1stDay, type = "II")
pairwise_comparisons_CH4_1stDay <- emmeans(m1_CH4_1stDay, ~ Treatment | Plant.type)
summary(pairwise_comparisons_CH4_1stDay)
multcomp::cld(pairwise_comparisons_CH4_1stDay)
plant_type_comparisons_CH4_1stDay <- emmeans(m1_CH4_1stDay, pairwise ~ Plant.type | Treatment)
summary(plant_type_comparisons_CH4_1stDay)
multcomp::cld(plant_type_comparisons_CH4_1stDay)

# Repeat for each time point and Test.Name combination as needed

library(tidyr)
library(dplyr)
# Check the column names in your dataset
colnames(GHZ)
# Reshaping the data into long format
GHZ_long <- GHZ %>%
  gather(key = "Time_Point", value = "Flux", 
         `BF.flux..umol.min.m2.`, 
         `AF_1stday_flux..umol.min.m2.`, 
         `AF_3rdday_flux..umol.min.m2.`,
         `AF_5thday_flux..umol.min.m2.`,
         `AF_7thday_flux..umol.min.m2.`) %>%
  mutate(Time_Point = factor(Time_Point, 
                             levels = c("BF.flux..umol.min.m2.", 
                                        "AF_1stday_flux..umol.min.m2.", 
                                        "AF_3rdday_flux..umol.min.m2.", 
                                        "AF_5thday_flux..umol.min.m2.", 
                                        "AF_7thday_flux..umol.min.m2."),
                             labels = c("Pre-Fertilization", 
                                        "Day 1", "Day 3", "Day 5", "Day 7")))
# Filter out non-finite values
GHZ_long_filtered <- GHZ_long %>%
  filter(is.finite(Flux))
# Create box plots for each test name without non-finite values
ggplot(GHZ_long_filtered, aes(x = Time_Point, y = Flux, fill = Time_Point)) +
  geom_boxplot() +
  facet_wrap(~ Test.Name, scales = "free") +
  labs(title = "Flux Comparison Over Time", x = "Time Point", y = "Flux (umol/m2/min)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom")
# Convert Treatment and Test.Name to factors if not already
GHZ_long_filtered$Treatment <- factor(GHZ_long_filtered$Treatment)
GHZ_long_filtered$Test.Name <- factor(GHZ_long_filtered$Test.Name)

# ANOVA to check if there are significant differences across the time points
anova_results <- aov(Flux ~ Time_Point * Treatment * Test.Name, data = GHZ_long_filtered)
summary(anova_results)
# Filter the dataset to include only N2O data
GHZ_N2O <- GHZ_long_filtered %>%
  filter(Test.Name == "N2O")
# Filter the dataset to include only N2O data
GHZ_N2O <- GHZ_long_filtered %>%
  filter(Test.Name == "N2O")
# Create box plots for N2O flux over different time points and treatments
ggplot(GHZ_N2O, aes(x = Time_Point, y = Flux, fill = Time_Point)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(title = "N2O Flux Comparison Over Time by Treatment", 
       x = "Time Point", 
       y = "Flux (umol/m2/min)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom")
# Perform ANOVA to test the significance of differences across time points and treatments
anova_results_N2O <- aov(Flux ~ Time_Point * Treatment, data = GHZ_N2O)
summary(anova_results_N2O)
# Perform ANOVA to test the significance of differences across time points and treatments
anova_results_N2O <- aov(Flux ~ Time_Point * Treatment, data = GHZ_N2O)
summary(anova_results_N2O)
# Plotting the line graph for N2O flux trends over time
ggplot(GHZ_N2O, aes(x = Time_Point, y = Flux, color = Treatment, group = Treatment)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "N2O Flux Trends Over Time by Treatment", 
       x = "Time Point", 
       y = "Flux (umol/m2/min)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Create box plots for N2O flux over different time points and treatments
ggplot(GHZ_N2O, aes(x = Time_Point, y = Flux, fill = Treatment)) +
  geom_boxplot() +
  labs(title = "N2O Flux Comparison Over Time by Treatment", 
       x = "Time Point", 
       y = "Flux (umol/m2/min)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom")


# Filter the dataset to include only CO2 data
GHZ_CO2 <- GHZ_long_filtered %>%
  filter(Test.Name == "CO2")

# Create box plots for CO2 flux over different time points and treatments
ggplot(GHZ_CO2, aes(x = Time_Point, y = Flux, fill = Time_Point)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(title = "CO2 Flux Comparison Over Time by Treatment", 
       x = "Time Point", 
       y = "Flux (umol/m2/min)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom")

# Perform ANOVA to test the significance of differences across time points and treatments for CO2
anova_results_CO2 <- aov(Flux ~ Time_Point * Treatment, data = GHZ_CO2)
summary(anova_results_CO2)

# Plotting the line graph for CO2 flux trends over time
ggplot(GHZ_CO2, aes(x = Time_Point, y = Flux, color = Treatment, group = Treatment)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "CO2 Flux Trends Over Time by Treatment", 
       x = "Time Point", 
       y = "Flux (umol/m2/min)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create box plots for CO2 flux over different time points and treatments
ggplot(GHZ_CO2, aes(x = Time_Point, y = Flux, fill = Treatment)) +
  geom_boxplot() +
  labs(title = "CO2 Flux Comparison Over Time by Treatment", 
       x = "Time Point", 
       y = "Flux (umol/m2/min)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom")
# Filter the dataset to include only CH4 data
GHZ_CH4 <- GHZ_long_filtered %>%
  filter(Test.Name == "CH4")

# Create box plots for CH4 flux over different time points and treatments
ggplot(GHZ_CH4, aes(x = Time_Point, y = Flux, fill = Time_Point)) +
  geom_boxplot() +
  facet_wrap(~ Treatment, scales = "free") +
  labs(title = "CH4 Flux Comparison Over Time by Treatment", 
       x = "Time Point", 
       y = "Flux (umol/m2/min)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom")

# Perform ANOVA to test the significance of differences across time points and treatments for CH4
anova_results_CH4 <- aov(Flux ~ Time_Point * Treatment, data = GHZ_CH4)
summary(anova_results_CH4)

# Plotting the line graph for CH4 flux trends over time
ggplot(GHZ_CH4, aes(x = Time_Point, y = Flux, color = Treatment, group = Treatment)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "CH4 Flux Trends Over Time by Treatment", 
       x = "Time Point", 
       y = "Flux (umol/m2/min)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create box plots for CH4 flux over different time points and treatments
ggplot(GHZ_CH4, aes(x = Time_Point, y = Flux, fill = Treatment)) +
  geom_boxplot() +
  labs(title = "CH4 Flux Comparison Over Time by Treatment", 
       x = "Time Point", 
       y = "Flux (umol/m2/min)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom")
# Perform ANOVA for N2O
anova_results_N2O <- aov(Flux ~ Time_Point * Treatment, data = GHZ_N2O)

# Perform post-hoc test for pairwise comparisons across treatments for each time point
pairwise_comparisons_N2O <- emmeans(anova_results_N2O, pairwise ~ Treatment | Time_Point)

# Display results
summary(pairwise_comparisons_N2O)

# Display compact letter display for significant differences
multcomp::cld(pairwise_comparisons_N2O)
# Perform ANOVA for CO2
anova_results_CO2 <- aov(Flux ~ Time_Point * Treatment, data = GHZ_CO2)

# Perform post-hoc test for pairwise comparisons across treatments for each time point
pairwise_comparisons_CO2 <- emmeans(anova_results_CO2, pairwise ~ Treatment | Time_Point)

# Display results
summary(pairwise_comparisons_CO2)

# Display compact letter display for significant differences
multcomp::cld(pairwise_comparisons_CO2)
# Perform ANOVA for CH4
anova_results_CH4 <- aov(Flux ~ Time_Point * Treatment, data = GHZ_CH4)

# Perform post-hoc test for pairwise comparisons across treatments for each time point
pairwise_comparisons_CH4 <- emmeans(anova_results_CH4, pairwise ~ Treatment | Time_Point)

# Display results
summary(pairwise_comparisons_CH4)

# Display compact letter display for significant differences
multcomp::cld(pairwise_comparisons_CH4)

