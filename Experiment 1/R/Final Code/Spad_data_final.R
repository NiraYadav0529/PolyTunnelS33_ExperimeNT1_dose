cholorophyll_spaddata_analysis_exp1_final

# Leaf Cholorophyll measurement data
library(tidyverse)
library(car)
library(emmeans)
library(ggplot2)

# Setting the working directory
setwd("C:/Users/90958427/OneDrive - Western Sydney University/Niraj Pot Experiment/Modified Data File")

# Read the CSV file
SPAD_data <- read.csv("cholorophyll_spaddata_analysis_exp1_final.csv")

# Check the first few rows to understand the structure
head(SPAD_data)
colnames(SPAD_data)
# Visualize the distribution of SPAD Data by Dose and Plant.type
ggplot(SPAD_data, aes(x = Dose, y = SPAD.Data..nmol.ch.mg.fresh.weight., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "SPAD Data by Dose and Plant Type", x = "Dose", y = "SPAD Data (nmol ch/mg fresh weight)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Fit the model to understand the effect of Dose and Plant.type on SPAD Data
m1_spad <- lm(SPAD.Data..nmol.ch.mg.fresh.weight. ~ Dose * Plant.type, data = SPAD_data)

# Summarize the model
summary(m1_spad)
# Check residuals for linearity and normality
residualPlot(m1_spad)  # Plot residuals vs fitted values
qqPlot(m1_spad)        # Q-Q plot to check normality of residuals
# Type II ANOVA test to check significance of effects
Anova(m1_spad, type = "II")
# Perform multiple comparison tests to see differences within Plant.type across Doses
pairwise_comparisons <- emmeans(m1_spad, ~ Dose | Plant.type)
summary(pairwise_comparisons)

# Generate compact letter display to visualize significant differences
multcomp::cld(pairwise_comparisons)
# Visualize the significant differences with compact letter display
ggplot(pairwise_comparisons, aes(x = Dose, y = emmean, colour = Plant.type)) +
  geom_point() +
  facet_wrap(~Plant.type) +
  labs(title = "Pairwise Comparisons of SPAD Data by Dose and Plant Type",
       x = "Dose",
       y = "Estimated Marginal Means of SPAD Data") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Convert the pairwise comparisons to a data frame
pairwise_comparisons_df <- as.data.frame(pairwise_comparisons)

# Visualize the significant differences with compact letter display
ggplot(pairwise_comparisons_df, aes(x = Dose, y = emmean, colour = Plant.type)) +
  geom_point(size = 3) +
  facet_wrap(~ Plant.type) +
  labs(title = "Pairwise Comparisons of SPAD Data by Dose and Plant Type",
       x = "Dose",
       y = "Estimated Marginal Means of SPAD Data") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

