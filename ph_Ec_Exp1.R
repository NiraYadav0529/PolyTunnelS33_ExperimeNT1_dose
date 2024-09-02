# Soil pH/ec Data Analysis
# Load necessary libraries
library(tidyverse)
library(car)
library(emmeans)
library(ggplot2)

# Setting the working directory

setwd("C:/Users/90958427/OneDrive - Western Sydney University/Niraj Pot Experiment/Modified Data File")
list.files()

# Reading the CSV file
ph_ec_data <- read.csv("pH_ec_data_exp1_final.csv") 

# View the first few rows of the dataset
head(ph_ec_data)

# Visualize the distribution of pH by Dose and Plant Type across different time points
ggplot(ph_ec_data, aes(x = Dose, y = pH_beforefertilization, colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "pH Before Fertilization by Dose and Plant Type", x = "Dose", y = "pH")

ggplot(ph_ec_data, aes(x = Dose, y = pH_afterFertilization_7thday, colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "pH 7th Day After Fertilization by Dose and Plant Type", x = "Dose", y = "pH")

ggplot(ph_ec_data, aes(x = Dose, y = pH_Afterfertilization_30days, colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "pH 30 Days After Fertilization by Dose and Plant Type", x = "Dose", y = "pH")

ggplot(ph_ec_data, aes(x = Dose, y = pH.Harvest.day_after2ndfertilization_60thday, colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "pH at Harvest Day (60 Days After 2nd Fertilization) by Dose and Plant Type", x = "Dose", y = "pH")

# Visualize the distribution of EC by Dose and Plant Type across different time points
ggplot(ph_ec_data, aes(x = Dose, y = EC_beforefertilization, colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "EC Before Fertilization by Dose and Plant Type", x = "Dose", y = "EC")

ggplot(ph_ec_data, aes(x = Dose, y = EC_Afterfertilization_30days, colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "EC 30 Days After Fertilization by Dose and Plant Type", x = "Dose", y = "EC")

ggplot(ph_ec_data, aes(x = Dose, y = EC.harvest.day, colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "EC at Harvest Day by Dose and Plant Type", x = "Dose", y = "EC")

# Fit the models for pH at different time points
m1_pH_BF <- lm(pH_beforefertilization ~ Dose * Plant.type, data = ph_ec_data)
m1_pH_7d <- lm(pH_afterFertilization_7thday ~ Dose * Plant.type, data = ph_ec_data)
m1_pH_30d <- lm(pH_Afterfertilization_30days ~ Dose * Plant.type, data = ph_ec_data)
m1_pH_Harvest <- lm(pH.Harvest.day_after2ndfertilization_60thday ~ Dose * Plant.type, data = ph_ec_data)

# Summarize the models for pH
summary(m1_pH_BF)
summary(m1_pH_7d)
summary(m1_pH_30d)
summary(m1_pH_Harvest)

# Fit the models for EC at different time points
m1_EC_BF <- lm(EC_beforefertilization ~ Dose * Plant.type, data = ph_ec_data)
m1_EC_30d <- lm(EC_Afterfertilization_30days ~ Dose * Plant.type, data = ph_ec_data)
m1_EC_Harvest <- lm(EC.harvest.day ~ Dose * Plant.type, data = ph_ec_data)

# Summarize the models for EC
summary(m1_EC_BF)
summary(m1_EC_30d)
summary(m1_EC_Harvest)

# Check residuals for linearity and normality for pH models
residualPlot(m1_pH_BF)
qqPlot(m1_pH_BF)

residualPlot(m1_pH_7d)
qqPlot(m1_pH_7d)

residualPlot(m1_pH_30d)
qqPlot(m1_pH_30d)

residualPlot(m1_pH_Harvest)
qqPlot(m1_pH_Harvest)

# Check residuals for linearity and normality for EC models
residualPlot(m1_EC_BF)
qqPlot(m1_EC_BF)

residualPlot(m1_EC_30d)
qqPlot(m1_EC_30d)

residualPlot(m1_EC_Harvest)
qqPlot(m1_EC_Harvest)

# Perform ANOVA on the pH models
Anova(m1_pH_BF)
Anova(m1_pH_7d)
Anova(m1_pH_30d)
Anova(m1_pH_Harvest)

# Perform ANOVA on the EC models
Anova(m1_EC_BF)
Anova(m1_EC_30d)
Anova(m1_EC_Harvest)

# Perform multiple comparison tests for pH
pairwise_comparisons_pH_BF <- emmeans(m1_pH_BF, ~ Dose | Plant.type)
pairwise_comparisons_pH_7d <- emmeans(m1_pH_7d, ~ Dose | Plant.type)
pairwise_comparisons_pH_30d <- emmeans(m1_pH_30d, ~ Dose | Plant.type)
pairwise_comparisons_pH_Harvest <- emmeans(m1_pH_Harvest, ~ Dose | Plant.type)

# Display pairwise comparisons for pH
summary(pairwise_comparisons_pH_BF)
summary(pairwise_comparisons_pH_7d)
summary(pairwise_comparisons_pH_30d)
summary(pairwise_comparisons_pH_Harvest)

# Perform multiple comparison tests for EC
pairwise_comparisons_EC_BF <- emmeans(m1_EC_BF, ~ Dose | Plant.type)
pairwise_comparisons_EC_30d <- emmeans(m1_EC_30d, ~ Dose | Plant.type)
pairwise_comparisons_EC_Harvest <- emmeans(m1_EC_Harvest, ~ Dose | Plant.type)

# Display pairwise comparisons for EC
summary(pairwise_comparisons_EC_BF)
summary(pairwise_comparisons_EC_30d)
summary(pairwise_comparisons_EC_Harvest)

# Generate compact letter display for pH comparisons
multcomp::cld(pairwise_comparisons_pH_BF)
multcomp::cld(pairwise_comparisons_pH_7d)
multcomp::cld(pairwise_comparisons_pH_30d)
multcomp::cld(pairwise_comparisons_pH_Harvest)

# Generate compact letter display for EC comparisons
multcomp::cld(pairwise_comparisons_EC_BF)
multcomp::cld(pairwise_comparisons_EC_30d)
multcomp::cld(pairwise_comparisons_EC_Harvest)

# Ensure the necessary libraries are loaded
library(tidyverse)

# Combine pH data into long format
ph_long <- ph_ec_data %>%
  select(Dose, Plant.type, 
         pH_beforefertilization, 
         pH_afterFertilization_7thday, 
         pH_Afterfertilization_30days, 
         pH.Harvest.day_after2ndfertilization_60thday) %>%
  pivot_longer(cols = starts_with("pH"),
               names_to = "Time_Point",
               values_to = "pH") %>%
  mutate(Time_Point = case_when(
    Time_Point == "pH_beforefertilization" ~ "Before Fertilization",
    Time_Point == "pH_afterFertilization_7thday" ~ "7th Day",
    Time_Point == "pH_Afterfertilization_30days" ~ "30th Day",
    Time_Point == "pH.Harvest.day_after2ndfertilization_60thday" ~ "Harvest Day"
  ))

# Check the first few rows to ensure it was reshaped correctly
head(ph_long)
# Plot pH across time points
p1 <- ggplot(ph_long, aes(x = Time_Point, y = pH, fill = Dose)) +
  geom_boxplot() +
  facet_wrap(~ Plant.type) +
  labs(title = "pH Comparison Across Time Points",
       x = "Time Point", y = "pH") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

# Display the plot
print(p1)

# Combine EC data into long format
ec_long <- ph_ec_data %>%
  select(Dose, Plant.type, 
         EC_beforefertilization, 
         EC_Afterfertilization_30days, 
         EC.harvest.day) %>%
  pivot_longer(cols = starts_with("EC"),
               names_to = "Time_Point",
               values_to = "EC") %>%
  mutate(Time_Point = case_when(
    Time_Point == "EC_beforefertilization" ~ "Before Fertilization",
    Time_Point == "EC_Afterfertilization_30days" ~ "30th Day",
    Time_Point == "EC.harvest.day" ~ "Harvest Day"
  ))
# ph ddat analyzed 
# Check the first few rows to ensure it was reshaped correctly
head(ec_long)
# Plot EC across time points
p2 <- ggplot(ec_long, aes(x = Time_Point, y = EC, fill = Dose)) +
  geom_boxplot() +
  facet_wrap(~ Plant.type) +
  labs(title = "EC Comparison Across Time Points",
       x = "Time Point", y = "EC (dS/m)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

# Display the plot
print(p2)

# Prepare the data for pH
ph_long <- ph_ec_data %>%
  select(Dose, Plant.type, Treatment, 
         pH_beforefertilization, 
         pH_afterFertilization_7thday, 
         pH_Afterfertilization_30days, 
         pH.Harvest.day_after2ndfertilization_60thday) %>%
  pivot_longer(cols = starts_with("pH"),
               names_to = "Time_Point",
               values_to = "pH") %>%
  mutate(Time_Point = case_when(
    Time_Point == "pH_beforefertilization" ~ "Before Fertilization",
    Time_Point == "pH_afterFertilization_7thday" ~ "7th Day",
    Time_Point == "pH_Afterfertilization_30days" ~ "30th Day",
    Time_Point == "pH.Harvest.day_after2ndfertilization_60thday" ~ "Harvest Day"
  ))

# Prepare the data for EC
ec_long <- ph_ec_data %>%
  select(Dose, Plant.type, Treatment, 
         EC_beforefertilization, 
         EC_Afterfertilization_30days, 
         EC.harvest.day) %>%
  pivot_longer(cols = starts_with("EC"),
               names_to = "Time_Point",
               values_to = "EC") %>%
  mutate(Time_Point = case_when(
    Time_Point == "EC_beforefertilization" ~ "Before Fertilization",
    Time_Point == "EC_Afterfertilization_30days" ~ "30th Day",
    Time_Point == "EC.harvest.day" ~ "Harvest Day"
  ))
# Box plot for pH across different time points
p_ph <- ggplot(ph_long, aes(x = Dose, y = pH, fill = Time_Point)) +
  geom_boxplot() +
  facet_wrap(~ Plant.type) +
  labs(title = "pH Comparison Across Time Points by Dose and Plant Type",
       x = "Dose", y = "pH") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

# Box plot for EC across different time points
p_ec <- ggplot(ec_long, aes(x = Dose, y = EC, fill = Time_Point)) +
  geom_boxplot() +
  facet_wrap(~ Plant.type) +
  labs(title = "EC Comparison Across Time Points by Dose and Plant Type",
       x = "Dose", y = "EC") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

# Plot both together in one layout
grid.arrange(p_ph, p_ec, nrow = 2)
