
library(tidyverse)
library(ggplot2)

# Set the working directory
setwd("C:/Users/90958427/OneDrive - Western Sydney University/Niraj Pot Experiment/Modified Data File")

# Read the CSV file
XRF_data <- read.csv("xrf_Nutrients_analysis_exp1_final.csv")

# View the first few rows to verify data was read correctly
head(XRF_data)
# Reshape the dataset into long format
XRF_long <- XRF_data %>%
  gather(key = "Nutrient", value = "Concentration", 
         Na._.ppm., Mg_..ppm., Al_.ppm., P_.ppm., K_.ppm., Ca_.ppm., 
         Fe_.ppm., Cl_.ppm., Ag_.cps., Cu_.ppm., Mn_.ppm., S_.ppm., Zn_.ppm.)

# View the reshaped data
head(XRF_long)
# Create a faceted boxplot for all nutrients
ggplot(XRF_long, aes(x = Treatment, y = Concentration, fill = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~ Nutrient, scales = "free_y") +
  labs(title = "Nutrient Concentrations by Treatment, Dose, and Plant Type", 
       x = "Treatment", 
       y = "Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom")
# Create a faceted boxplot for all nutrients, colored by Plant.type
ggplot(XRF_long, aes(x = Treatment, y = Concentration, fill = Plant.type)) + 
  geom_boxplot() + 
  facet_wrap(~ Nutrient, scales = "free_y") +
  labs(title = "Nutrient Concentrations by Treatment, Dose, and Plant Type", 
       x = "Treatment", 
       y = "Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom")
# Reshape the root nutrient dataset into long format
XRF_root_long <- XRF_data %>%
  gather(key = "Nutrient", value = "Concentration", 
         Na._.ppm..1, Mg_..ppm..1, Al_.ppm..1, P_.ppm..1, K_.ppm..1, 
         Ca_.ppm..1, Fe_.ppm..1, Cl_.ppm..1, Cu_.ppm..1, Mn_.ppm..1, 
         S_.ppm..1, Zn_.ppm..1)

# View the reshaped data
head(XRF_root_long)
# Create a faceted boxplot for all root nutrients
ggplot(XRF_root_long, aes(x = Treatment, y = Concentration, fill = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~ Nutrient, scales = "free_y") +
  labs(title = "Root Nutrient Concentrations by Treatment, Dose, and Plant Type", 
       x = "Treatment", 
       y = "Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom")
# Create a faceted boxplot for all root nutrients, colored by Plant.type
ggplot(XRF_root_long, aes(x = Treatment, y = Concentration, fill = Plant.type)) + 
  geom_boxplot() + 
  facet_wrap(~ Nutrient, scales = "free_y") +
  labs(title = "Root Nutrient Concentrations by Treatment, Dose, and Plant Type", 
       x = "Treatment", 
       y = "Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom")
# Load necessary libraries
library(tidyverse)
library(car)
library(emmeans)
library(ggplot2)

# Setting the working directory (adjust this to your correct path)
setwd("C:/Users/90958427/OneDrive - Western Sydney University/Niraj Pot Experiment/Modified Data File")

# Reading the CSV file
XRF_data <- read.csv("xrf_Nutrients_analysis_exp1_final.csv") 

# List of Leaf Foliage nutrients
leaf_nutrients <- c("Na._.ppm.", "Mg_..ppm.", "Al_.ppm.", "P_.ppm.", "K_.ppm.", 
                    "Ca_.ppm.", "Fe_.ppm.", "Cl_.ppm.", "Cu_.ppm.", "Mn_.ppm.", 
                    "S_.ppm.", "Zn_.ppm.")

# Perform statistical analysis for each nutrient
for (nutrient in leaf_nutrients) {
  # Fit the linear model
  model <- lm(as.formula(paste(nutrient, "~ Plant.type * Treatment * Dose")), data = XRF_data)
  
  # Summary of the model
  print(paste("Summary for", nutrient))
  print(summary(model))
  
  # ANOVA to test the significance of effects
  print(paste("ANOVA for", nutrient))
  print(Anova(model, type = "II"))
}
# List of Root nutrients
root_nutrients <- c("Na._.ppm..1", "Mg_..ppm..1", "Al_.ppm..1", "P_.ppm..1", "K_.ppm..1", 
                    "Ca_.ppm..1", "Fe_.ppm..1", "Cl_.ppm..1", "Cu_.ppm..1", "Mn_.ppm..1", 
                    "S_.ppm..1", "Zn_.ppm..1")

# Perform statistical analysis for each nutrient
for (nutrient in root_nutrients) {
  # Fit the linear model
  model <- lm(as.formula(paste(nutrient, "~ Plant.type * Treatment * Dose")), data = XRF_data)
  
  # Summary of the model
  print(paste("Summary for", nutrient))
  print(summary(model))
  
  # ANOVA to test the significance of effects
  print(paste("ANOVA for", nutrient))
  print(Anova(model, type = "II"))
}

