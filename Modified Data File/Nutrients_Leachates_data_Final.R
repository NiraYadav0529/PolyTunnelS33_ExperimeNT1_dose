# Soil Leachates Data Analysis
library(tidyverse)
library(car)
library(emmeans)
library(ggplot2)

# Setting the working directory
setwd("C:/Users/90958427/OneDrive - Western Sydney University/Niraj Pot Experiment/Modified Data File")

# Read the CSV file
Leachates_data <- read.csv("Nutrients_Lechatesdata_exp1_final.csv")

# Check the first few rows to understand the structure
head(Leachates_data)
colnames(Leachates_data)

# Clean the data by removing rows with NA values in the relevant columns
Leachates_data_clean <- Leachates_data %>%
  filter(!is.na(NH4_Lechates_Before_2nd_fertilization..mg.N.L.) &
           !is.na(NO3_Lechates_Before_2nd_fertilization..mg.N.L.) &
           !is.na(PO4_Lechates_Before_2nd_fertilization..mg.N.L.))

# Ensure Dose and Plant.type have at least two levels
if(length(unique(Leachates_data_clean$Dose)) < 2 || length(unique(Leachates_data_clean$Plant.type)) < 2) {
  stop("Either 'Dose' or 'Plant.type' has fewer than two levels. Cannot fit the model.")
}

### NH4 Analysis ###

# Fit the model for NH4 Before 2nd Fertilization
m1_NH4_2nd <- lm(NH4_Lechates_Before_2nd_fertilization..mg.N.L. ~ Dose * Plant.type, data = Leachates_data_clean)

# Summary of the model
summary(m1_NH4_2nd)

# Residual and Q-Q plots to check assumptions
residualPlot(m1_NH4_2nd)
qqPlot(m1_NH4_2nd)

# ANOVA to check significance
Anova(m1_NH4_2nd, type = "II")

# Visualize NH4 in Leachates before the 2nd fertilization
ggplot(Leachates_data_clean, aes(x = Dose, y = NH4_Lechates_Before_2nd_fertilization..mg.N.L., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "NH4 in Leachates Before 2nd Fertilization by Dose and Plant Type", x = "Dose", y = "NH4 (mg N/L)")

### NO3 Analysis ###

# Fit the model for NO3 Before 2nd Fertilization
m1_NO3_2nd <- lm(NO3_Lechates_Before_2nd_fertilization..mg.N.L. ~ Dose * Plant.type, data = Leachates_data_clean)

# Summary of the model
summary(m1_NO3_2nd)

# Residual and Q-Q plots to check assumptions
residualPlot(m1_NO3_2nd)
qqPlot(m1_NO3_2nd)

# ANOVA to check significance
Anova(m1_NO3_2nd, type = "II")

# Visualize NO3 in Leachates before the 2nd fertilization
ggplot(Leachates_data_clean, aes(x = Dose, y = NO3_Lechates_Before_2nd_fertilization..mg.N.L., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "NO3 in Leachates Before 2nd Fertilization by Dose and Plant Type", x = "Dose", y = "NO3 (mg N/L)")

### PO4 Analysis ###

# Fit the model for PO4 Before 2nd Fertilization
m1_PO4_2nd <- lm(PO4_Lechates_Before_2nd_fertilization..mg.N.L. ~ Dose * Plant.type, data = Leachates_data_clean)

# Summary of the model
summary(m1_PO4_2nd)

# Residual and Q-Q plots to check assumptions
residualPlot(m1_PO4_2nd)
qqPlot(m1_PO4_2nd)

# ANOVA to check significance
Anova(m1_PO4_2nd, type = "II")

# Visualize PO4 in Leachates before the 2nd fertilization
ggplot(Leachates_data_clean, aes(x = Dose, y = PO4_Lechates_Before_2nd_fertilization..mg.N.L., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "PO4 in Leachates Before 2nd Fertilization by Dose and Plant Type", x = "Dose", y = "PO4 (mg N/L)")

### Summary ###

# Each of these sections compares the concentration of NH4, NO3, and PO4 in leachates before the second fertilization across different doses and plant types.
# The residual and Q-Q plots check the assumptions of the models, and the ANOVA tests the significance of the effects.


# Function to perform pairwise comparison and visualize
perform_analysis <- function(response_var, title, y_label) {
  # Fit the model
  model <- lm(as.formula(paste(response_var, "~ Dose * Plant.type")), data = Leachates_data_clean)
  
  # Perform pairwise comparison using emmeans
  pairwise_comparisons <- emmeans(model, pairwise ~ Dose | Plant.type)
  
  # Display the pairwise comparison results
  print(summary(pairwise_comparisons))
  
  # Visualize the pairwise comparisons
  plot <- ggplot(Leachates_data_clean, aes_string(x = "Dose", y = response_var, fill = "Dose")) +
    geom_boxplot() +
    facet_wrap(~ Plant.type) +
    labs(title = title, x = "Dose", y = y_label) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
  
  return(plot)
}

# Visualize NH4, NO3, and PO4 together in one grid
p1 <- perform_analysis("NH4_Lechates_Before_2nd_fertilization..mg.N.L.", 
                       "NH4 in Leachates Before 2nd Fertilization by Dose and Plant Type", 
                       "NH4 (mg N/L)")

p2 <- perform_analysis("NO3_Lechates_Before_2nd_fertilization..mg.N.L.", 
                       "NO3 in Leachates Before 2nd Fertilization by Dose and Plant Type", 
                       "NO3 (mg N/L)")

p3 <- perform_analysis("PO4_Lechates_Before_2nd_fertilization..mg.N.L.", 
                       "PO4 in Leachates Before 2nd Fertilization by Dose and Plant Type", 
                       "PO4 (mg N/L)")

# Arrange the plots in one grid
install.packages("gridExtra")
library(gridExtra)

grid.arrange(p1, p2, p3, nrow = 3)
