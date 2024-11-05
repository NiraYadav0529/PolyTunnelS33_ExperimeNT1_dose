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









# Load necessary libraries
library(dplyr)
library(ggplot2)
library(emmeans)
library(multcompView)

# Reading the pH and EC data (assuming CSV is already loaded as 'ph_ec_data')
colnames(ph_ec_data)

# Ensure necessary columns are treated as factors for pH and EC data
ph_ec_data <- ph_ec_data %>%
  mutate(
    Dose = factor(Dose..N.kg.ha., levels = c("0", "100", "200")),  # Convert Dose column to factor
    Fertilizer_Type = factor(Fertilizer.type, levels = c('None', 'MF', 'UF')),  # Convert Fertilizer.type to Fertilizer_Type
    Plant.type = case_match(Plant.type, 'L' ~ 'Lucerne', 'P' ~ 'Phalaris'),  # Rename levels for labeling facets
    Plant.type = factor(Plant.type)  # Ensure Plant.type is a factor
  )

# Check structure of the dataset
ph_ec_data %>% 
  group_by(Plant.type, Fertilizer_Type, Dose) %>% 
  summarise(n())

# Restructure the pH data for analysis, focusing on the 90th Day (Harvest Day)
ph_long_90 <- ph_ec_data %>%
  pivot_longer(cols = c(pH.Harvest.day_after2ndfertilization_60thday), 
               names_to = "Time_Point", values_to = "pH") %>%
  mutate(Time_Point = factor("90th Day")) %>%
  mutate(pH = as.numeric(pH))  # Ensure pH is numeric

# Restructure the EC data for analysis, focusing on the 90th Day (Harvest Day)
ec_long_90 <- ph_ec_data %>%
  pivot_longer(cols = c(EC.harvest.day), 
               names_to = "Time_Point", values_to = "EC") %>%
  mutate(Time_Point = factor("90th Day")) %>%
  mutate(EC = as.numeric(EC))  # Ensure EC is numeric

# Summarize the pH data for 90th Day
pH_summary_90 <- ph_long_90 %>%
  group_by(Dose, Plant.type, Fertilizer_Type, Time_Point) %>%
  summarise(
    Mean_pH = mean(pH, na.rm = TRUE),
    SE_pH = sd(pH, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()

# Summarize the EC data for 90th Day
EC_summary_90 <- ec_long_90 %>%
  group_by(Dose, Plant.type, Fertilizer_Type, Time_Point) %>%
  summarise(
    Mean_EC = mean(EC, na.rm = TRUE),
    SE_EC = sd(EC, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()

### pH Data Analysis (90th Day) ###
# Model for pH data
m1_ph_90 <- lm(pH ~ Dose * Fertilizer_Type * Plant.type, data = ph_long_90)
summary(m1_ph_90)

# Check residuals for pH data
residualPlot(m1_ph_90)
qqPlot(m1_ph_90)

# Test significance of the model effects for pH data
Anova(m1_ph_90, type = "II")

# Perform multiple comparison test (Tukey HSD) for pH data
pairwise_comparisons_ph_90 <- emmeans(m1_ph_90, ~ Dose + Fertilizer_Type | Plant.type)
letters_ph_90 <- cld(pairwise_comparisons_ph_90, Letters = letters) %>%
  mutate(.group = stringr::str_trim(.group)) %>%
  as.data.frame()

# Merge the letters for pH data with pH_summary_90 using Dose and Plant.type
pH_summary_90 <- pH_summary_90 %>%
  left_join(letters_ph_90, by = c("Dose", "Fertilizer_Type", "Plant.type"))

### EC Data Analysis (90th Day) ###
# Model for EC data
m1_ec_90 <- lm(EC ~ Dose * Fertilizer_Type * Plant.type, data = ec_long_90)
summary(m1_ec_90)

# Check residuals for EC data
residualPlot(m1_ec_90)
qqPlot(m1_ec_90)

# Test significance of the model effects for EC data
Anova(m1_ec_90, type = "II")

# Perform multiple comparison test (Tukey HSD) for EC data
pairwise_comparisons_ec_90 <- emmeans(m1_ec_90, ~ Dose + Fertilizer_Type | Plant.type)
letters_ec_90 <- cld(pairwise_comparisons_ec_90, Letters = letters) %>%
  mutate(.group = stringr::str_trim(.group)) %>%
  as.data.frame()

# Merge the letters for EC data with EC_summary_90 using Dose and Plant.type
EC_summary_90 <- EC_summary_90 %>%
  left_join(letters_ec_90, by = c("Dose", "Fertilizer_Type", "Plant.type"))

# Custom colorblind-friendly palette
colorblind_palette <- c("None" = "black", "MF" = "#0072B2", "UF" = "#D55E00")

# Shapes for different doses
shapes <- c("0" = 19, "100" = 21, "200" = 23)

# Line types for fertilizer types
line_types <- c("None" = "solid", "MF" = "dashed", "UF" = "dotted")

### pH Plot for 90th Day ###
pH_plot_90 <- ggplot(pH_summary_90, aes(x = Dose, y = Mean_pH, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), 
           colour = "black") +
  geom_errorbar(aes(ymin = Mean_pH - SE_pH, ymax = Mean_pH + SE_pH), 
                width = 0.2, position = position_dodge2(0.9)) +
  labs(title = "pH 90th Day (Harvest Day)", 
       x = "Dose (N kg/ha)", 
       y = "pH (+/- SE)", 
       fill = 'Fertilizer') +
  facet_wrap(~ Plant.type) +
  scale_fill_manual(values = colorblind_palette) +
  theme_minimal() +
  
  # Adjusting the theme for bold text and making it black
  theme(
    axis.title.x = element_text(face = "bold", color = "black", size = 14),
    axis.title.y = element_text(face = "bold", color = "black", size = 14),
    axis.text.x = element_text(face = "bold", color = "black", size = 12),
    axis.text.y = element_text(face = "bold", color = "black", size = 12),
    plot.title = element_text(face = "bold", color = "black", size = 16, hjust = 0.5),
    legend.title = element_text(face = "bold", color = "black", size = 12),
    legend.text = element_text(face = "bold", color = "black", size = 12),
    strip.text = element_text(face = "bold", color = "black", size = 14)
  ) +
  
  # Add compact letter display (CLD) text on top of bars
  geom_text(aes(y = Mean_pH + SE_pH + 0.1, label = .group), 
            position = position_dodge2(0.9, preserve = 'single'))

### EC Plot for 90th Day ###
EC_plot_90 <- ggplot(EC_summary_90, aes(x = Dose, y = Mean_EC, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), 
           colour = "black") +
  geom_errorbar(aes(ymin = Mean_EC - SE_EC, ymax = Mean_EC + SE_EC), 
                width = 0.2, position = position_dodge2(0.9)) +
  labs(title = "EC 90th Day (Harvest Day)", 
       x = "Dose (N kg/ha)", 
       y = "EC (+/- SE)", 
       fill = 'Fertilizer') +
  facet_wrap(~ Plant.type) +
  scale_fill_manual(values = colorblind_palette) +
  theme_minimal() +
  
  # Adjusting the theme for bold text and making it black
  theme(
    axis.title.x = element_text(face = "bold", color = "black", size = 14),
    axis.title.y = element_text(face = "bold", color = "black", size = 14),
    axis.text.x = element_text(face = "bold", color = "black", size = 12),
    axis.text.y = element_text(face = "bold", color = "black", size = 12),
    plot.title = element_text(face = "bold", color = "black", size = 16, hjust = 0.5),
    legend.title = element_text(face = "bold", color = "black", size = 12),
    legend.text = element_text(face = "bold", color = "black", size = 12),
    strip.text = element_text(face = "bold", color = "black", size = 14)
  ) +
  
  # Add compact letter display (CLD) text on top of bars
  geom_text(aes(y = Mean_EC + SE_EC + 0.1, label = .group), 
            position = position_dodge2(0.9, preserve = 'single'))

# Print the plots
print(pH_plot_90)
print(EC_plot_90)




# Custom black, white, and grey color palette for Fertilizer Type
black_white_grey_palette <- c("None" = "grey", "MF" = "white", "UF" = "black")

# Set the dodge position (so both bars and error bars are aligned the same)
dodge_position <- position_dodge(width = 0.9)  # Adjust the dodge width

### pH Plot for 90th Day with Dose 0 Width Fixed ###
pH_plot_90_fixed <- ggplot(pH_summary_90, aes(x = Dose, y = Mean_pH, fill = Fertilizer_Type)) +
  # Adjusting bar width for all doses, especially for Dose 0
  geom_bar(stat = "identity", position = dodge_position, colour = "black", 
           aes(width = ifelse(Dose == 0, 0.45, 0.7))) +  # Adjusting Dose 0 to half the width
  geom_errorbar(aes(ymin = Mean_pH - SE_pH, ymax = Mean_pH + SE_pH), 
                width = 0.2, position = dodge_position, color = "black") +  # Error bars properly aligned
  labs(title = "pH Harvest Day", 
       x = "Dose (N kg/ha)", 
       y = "pH (+/- SE)", 
       fill = 'Fertilizer') +
  facet_wrap(~ Plant.type) +
  scale_fill_manual(values = black_white_grey_palette) +  # Use black, white, grey for bar fill
  theme_minimal() +
  
  # Adjusting the theme for bold text and making it black
  theme(
    axis.title.x = element_text(face = "bold", color = "black", size = 14),
    axis.title.y = element_text(face = "bold", color = "black", size = 14),
    axis.text.x = element_text(face = "bold", color = "black", size = 12),
    axis.text.y = element_text(face = "bold", color = "black", size = 12),
    plot.title = element_text(face = "bold", color = "black", size = 16, hjust = 0.5),
    legend.title = element_text(face = "bold", color = "black", size = 12),
    legend.text = element_text(face = "bold", color = "black", size = 12),
    strip.text = element_text(face = "bold", color = "black", size = 14)
  ) +
  
  # Add compact letter display (CLD) text on top of bars
  geom_text(aes(y = Mean_pH + SE_pH + 0.1, label = .group), 
            position = dodge_position)  # Adjust text position above bars

### EC Plot for 90th Day with Dose 0 Width Fixed ###
EC_plot_90_fixed <- ggplot(EC_summary_90, aes(x = Dose, y = Mean_EC, fill = Fertilizer_Type)) +
  # Adjusting bar width for all doses, especially for Dose 0
  geom_bar(stat = "identity", position = dodge_position, colour = "black", 
           aes(width = ifelse(Dose == 0, 0.45, 0.7))) +  # Adjusting Dose 0 to half the width
  geom_errorbar(aes(ymin = Mean_EC - SE_EC, ymax = Mean_EC + SE_EC), 
                width = 0.2, position = dodge_position, color = "black") +  # Error bars properly aligned
  labs(title = "EC Harvest Day", 
       x = "Dose (N kg/ha)", 
       y = "EC (+/- SE)", 
       fill = 'Fertilizer') +
  facet_wrap(~ Plant.type) +
  scale_fill_manual(values = black_white_grey_palette) +  # Use black, white, grey for bar fill
  theme_minimal() +
  
  # Adjusting the theme for bold text and making it black
  theme(
    axis.title.x = element_text(face = "bold", color = "black", size = 14),
    axis.title.y = element_text(face = "bold", color = "black", size = 14),
    axis.text.x = element_text(face = "bold", color = "black", size = 12),
    axis.text.y = element_text(face = "bold", color = "black", size = 12),
    plot.title = element_text(face = "bold", color = "black", size = 16, hjust = 0.5),
    legend.title = element_text(face = "bold", color = "black", size = 12),
    legend.text = element_text(face = "bold", color = "black", size = 12),
    strip.text = element_text(face = "bold", color = "black", size = 14)
  ) +
  
  # Add compact letter display (CLD) text on top of bars
  geom_text(aes(y = Mean_EC + SE_EC + 0.1, label = .group), 
            position = dodge_position)  # Adjust text position above bars

# Print the updated plots
print(pH_plot_90_fixed)
print(EC_plot_90_fixed)

