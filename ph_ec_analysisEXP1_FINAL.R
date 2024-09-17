# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(emmeans)
library(gridExtra)
library(car)  # For ANOVA
# Reading the CSV file
ph_ec_data <- read.csv("pH_ec_data_exp1_final.csv")
head(ph_ec_data)
# Check column names in the dataset
colnames(ph_ec_data)
# Ensure Time_Point is treated as a factor in both datasets (pH and EC data)
ph_long <- ph_ec_data %>%
  pivot_longer(cols = c(pH_beforefertilization, pH_afterFertilization_7thday, 
                        pH_Afterfertilization_30days, pH.Harvest.day_after2ndfertilization_60thday),
               names_to = "Time_Point", values_to = "pH") %>%
  mutate(Time_Point = factor(case_when(
    Time_Point == "pH_beforefertilization" ~ "0 Day",
    Time_Point == "pH_afterFertilization_7thday" ~ "07th Day",
    Time_Point == "pH_Afterfertilization_30days" ~ "30th Day",
    Time_Point == "pH.Harvest.day_after2ndfertilization_60thday" ~ "90th Day"
  ))) %>%
  mutate(pH = as.numeric(pH))  # Ensure pH is numeric

ec_long <- ph_ec_data %>%
  pivot_longer(cols = c(EC_beforefertilization, EC_Afterfertilization_30days, EC.harvest.day),
               names_to = "Time_Point", values_to = "EC") %>%
  mutate(Time_Point = factor(case_when(
    Time_Point == "EC_beforefertilization" ~ "0 Day",
    Time_Point == "EC_Afterfertilization_30days" ~ "30th Day",
    Time_Point == "EC.harvest.day" ~ "90th Day"
  ))) %>%
  mutate(EC = as.numeric(EC))  # Ensure EC is numeric

# Summarize the pH data to calculate means and standard errors for pH at different time points
pH_summary <- ph_long %>%
  group_by(Dose, Plant.type, Fertilizer_Type, Time_Point) %>%
  summarise(
    Mean_pH = mean(pH, na.rm = TRUE),
    SE_pH = sd(pH, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()

# Summarize the EC data to calculate means and standard errors for EC at different time points
EC_summary <- ec_long %>%
  group_by(Dose, Plant.type, Fertilizer_Type, Time_Point) %>%
  summarise(
    Mean_EC = mean(EC, na.rm = TRUE),
    SE_EC = sd(EC, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()

# Custom colors for colorblind accessibility
colorblind_palette <- c("None" = "black", "MF" = "#0072B2", "UF" = "#D55E00")  # Black, Blue, Orange

# Shapes for different doses
shapes <- c("0" = 19, "100" = 21, "200" = 23)  # Adjust based on dose levels

# Line types for fertilizer types
line_types <- c("None" = "solid", "MF" = "dashed", "UF" = "dotted")

# Create the pH visualization
pH_plot <- ggplot(pH_summary, aes(x = Time_Point, y = Mean_pH, color = Fertilizer_Type, 
                                  group = interaction(Dose, Fertilizer_Type))) +
  geom_line(aes(linetype = Fertilizer_Type), position = position_dodge(width = 0.2), size = 1) + 
  geom_point(aes(shape = Dose), position = position_dodge(width = 0.2), size = 3) +
  geom_errorbar(aes(ymin = Mean_pH - SE_pH, ymax = Mean_pH + SE_pH), 
                width = 0.2, position = position_dodge(0.2)) +
  scale_color_manual(values = colorblind_palette) +  # Colorblind-friendly colors
  scale_shape_manual(values = shapes) +  # Shapes for doses
  scale_linetype_manual(values = line_types) +  # Line types for fertilizer types
  labs(title = "pH Changes Across Doses and Fertilizer Types by Time Points", 
       x = "Days after Fertilization", y = "pH (+/- SE)", color = 'Fertilizer Type', 
       shape = 'Dose (N kg/ha)', linetype = 'Fertilizer Type') +
  facet_wrap(~ Plant.type) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create the EC visualization
EC_plot <- ggplot(EC_summary, aes(x = Time_Point, y = Mean_EC, color = Fertilizer_Type, 
                                  group = interaction(Dose, Fertilizer_Type))) +
  geom_line(aes(linetype = Fertilizer_Type), position = position_dodge(width = 0.2), size = 1) + 
  geom_point(aes(shape = Dose), position = position_dodge(width = 0.2), size = 3) +
  geom_errorbar(aes(ymin = Mean_EC - SE_EC, ymax = Mean_EC + SE_EC), 
                width = 0.2, position = position_dodge(0.2)) +
  scale_color_manual(values = colorblind_palette) +  # Colorblind-friendly colors
  scale_shape_manual(values = shapes) +  # Shapes for doses
  scale_linetype_manual(values = line_types) +  # Line types for fertilizer types
  labs(title = "EC Changes Across Doses and Fertilizer Types by Time Points", 
       x = "Days after Fertilization", y = "EC (+/- SE)", color = 'Fertilizer Type', 
       shape = 'Dose (N kg/ha)', linetype = 'Fertilizer Type') +
  facet_wrap(~ Plant.type) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plots
print(pH_plot)
print(EC_plot)



# ANOVA to test interaction between dose, fertilizer type, plant type, and time points for pH
interaction_model_pH <- lm(pH ~ Dose * Fertilizer_Type * Plant.type * Time_Point, data = ph_long)

# Summary of the interaction model for pH
summary(interaction_model_pH)

# Perform ANOVA on the interaction model for pH
anova_results_pH <- Anova(interaction_model_pH, type = "II")
print(anova_results_pH)

# ANOVA to test interaction between dose, fertilizer type, plant type, and time points for EC
interaction_model_EC <- lm(EC ~ Dose * Fertilizer_Type * Plant.type * Time_Point, data = ec_long)

# Summary of the interaction model for EC
summary(interaction_model_EC)

# Perform ANOVA on the interaction model for EC
anova_results_EC <- Anova(interaction_model_EC, type = "II")
print(anova_results_EC)


