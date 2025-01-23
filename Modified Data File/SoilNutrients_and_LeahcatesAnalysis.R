setwd("C:\\Users\\90958427\\OneDrive - Western Sydney University\\PolyTunnelS33_ExperimeNT1_dose\\Modified Data File")
list.files()
FinalData_EXP1 <-read.csv("Final_Data_for analysis_Experiment1.csv")
colnames(FinalData_EXP1)

# Ensure dplyr is loaded explicitly
library(dplyr)

# Clean column names (using janitor to standardize names)
FinalData_EXP1 <- FinalData_EXP1 %>%
  janitor::clean_names()  # Clean column names to lowercase with underscores

# Check updated column names
colnames(FinalData_EXP1)

# Subset and rename relevant columns, ensuring proper factor levels
data <- FinalData_EXP1 %>%
  mutate(
    dose = factor(dose_n_kg_ha, levels = c("0", "100", "200")),  # Convert dose to factor with specified levels
    fertilizer = factor(fertilizer_type, levels = c("None", "MF", "UF")),  # Convert fertilizer to factor
    plant_species = factor(plant_type)  # Ensure plant_type is a factor
  ) %>%
  dplyr::select(  # Explicitly call dplyr::select to avoid conflicts
    fertilizer,
    dose,
    plant_species,
    plant_height = plant_height_harvestday_cm,
    stem_count = stem_count_harvest_day_no,
    fresh_biomass = fresh_biomass_harvestday_gm,
    dry_biomass = dry_biomass_bf_gm
  )

# Inspect the cleaned and subsetted data
str(data)


# Summary statistics for plant health metrics
summary_stats <- data %>%
  group_by(fertilizer, dose, plant_species) %>%
  summarise(across(c(plant_height, stem_count, fresh_biomass, dry_biomass), 
                   list(mean = mean, se = ~ sd(.) / sqrt(n())), .names = "{col}_{fn}"))

print(summary_stats)

# ANOVA for each plant health metric
anova_results <- list()

for (metric in c("plant_height", "stem_count", "fresh_biomass", "dry_biomass")) {
  model <- aov(as.formula(paste(metric, "~ fertilizer * dose * plant_species")), data = data)
  anova_results[[metric]] <- summary(model)
  cat("\n--- ANOVA for", metric, "---\n")
  print(anova_results[[metric]])
  
  # Residual diagnostics: QQ plot and residual plot
  cat("\n--- Diagnostic Plots for", metric, "---\n")
  qqnorm(residuals(model))
  qqline(residuals(model))
  plot(fitted(model), residuals(model), main = paste("Residuals vs Fitted:", metric))
}

# Pairwise comparisons
pairwise_results <- list()

for (metric in c("plant_height", "stem_count", "fresh_biomass", "dry_biomass")) {
  model <- aov(as.formula(paste(metric, "~ fertilizer * dose * plant_species")), data = data)
  emmeans_model <- emmeans(model, ~ fertilizer * dose * plant_species)
  pairwise_results[[metric]] <- pairs(emmeans_model)
  cat("\n--- Pairwise Comparisons for", metric, "---\n")
  print(pairwise_results[[metric]])
}

# Dose-response relationship: Dose vs PlantHeight and FreshBiomass
dose_response <- data %>%
  pivot_longer(cols = c(plant_height, fresh_biomass), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = dose, y = value, color = fertilizer)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ metric, scales = "free_y") +
  labs(title = "Dose-Response Relationship", x = "Dose (N kg/ha)", y = "Value") +
  theme_minimal()

print(dose_response)

# Save dose-response plot
ggsave("Dose_Response_Relationship.png", dose_response)




# Load libraries
library(dplyr)
library(ggplot2)
library(car)
library(emmeans)
library(gridExtra)


# Load the dataset (update the file path as necessary)
SoilNutrients_data <- read.csv("Final_Data_for analysis_Experiment1.csv")

colnames(SoilNutrients_data)
# View the structure and a summary of the data
str(SoilNutrients_data)
summary(SoilNutrients_data)


# Rename and preprocess the dataset
SoilNutrients_data <- SoilNutrients_data %>%
  rename(
    fertilizer_type = Fertilizer_Type,
    dose_original = Dose..N.kg.ha.,  # Avoid overwriting an existing column named 'dose'
    plant_species_original = Plant.type,  # Avoid overwriting an existing column named 'plant_species'
    soil_nh4 = Soil_NH4.g.kg..dw_HarvestDay,
    soil_no3 = Soil_NO3.g.kg..dw_HarvestDay,
    soil_p = Soil_P.g.kg..dw_HarvestDay
  ) %>%
  mutate(
    dose = factor(dose_original, levels = c("0", "100", "200")),       # Create a new dose column as a factor
    fertilizer = factor(fertilizer_type, levels = c("None", "MF", "UF")), # Convert fertilizer_type to factor
    plant_species = factor(plant_species_original)                    # Create a new plant_species column as a factor
  )
# Check the structure of the dataset
str(SoilNutrients_data)

# View the first few rows
head(SoilNutrients_data)
# Compute summary statistics
summary_stats <- SoilNutrients_data %>%
  group_by(fertilizer, dose, plant_species) %>%
  summarise(
    mean_nh4 = mean(soil_nh4, na.rm = TRUE),
    se_nh4 = sd(soil_nh4, na.rm = TRUE) / sqrt(n()),
    mean_no3 = mean(soil_no3, na.rm = TRUE),
    se_no3 = sd(soil_no3, na.rm = TRUE) / sqrt(n()),
    mean_p = mean(soil_p, na.rm = TRUE),
    se_p = sd(soil_p, na.rm = TRUE) / sqrt(n())
  )
print(summary_stats)
# ANOVA for NH4
anova_nh4 <- aov(soil_nh4 ~ fertilizer * dose * plant_species, data = SoilNutrients_data)
summary(anova_nh4)
# ANOVA for NO3
anova_no3 <- aov(soil_no3 ~ fertilizer * dose * plant_species, data = SoilNutrients_data)
summary(anova_no3)
# ANOVA for P
anova_p <- aov(soil_p ~ fertilizer * dose * plant_species, data = SoilNutrients_data)
summary(anova_p)
# Residual diagnostics for NH4
par(mfrow = c(1, 2))
plot(anova_nh4$fitted.values, residuals(anova_nh4), main = "Residuals vs Fitted (NH4)")
qqnorm(residuals(anova_nh4), main = "QQ Plot (NH4)")
qqline(residuals(anova_nh4))

# Residual diagnostics for NO3
par(mfrow = c(1, 2))
plot(anova_no3$fitted.values, residuals(anova_no3), main = "Residuals vs Fitted (NO3)")
qqnorm(residuals(anova_no3), main = "QQ Plot (NO3)")
qqline(residuals(anova_no3))

# Residual diagnostics for NO3
par(mfrow = c(1, 2))
plot(anova_p$fitted.values, residuals(anova_p), main = "Residuals vs Fitted (PO4)")
qqnorm(residuals(anova_p), main = "QQ Plot (PO4)")
qqline(residuals(anova_p))


# Violin plot with boxplot overlay for NH4
plot_nh4 <- ggplot(SoilNutrients_data, aes(x = interaction(fertilizer, dose), 
                                           y = soil_nh4, fill = plant_species)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.2, position = position_dodge(0.9)) +
  theme_minimal() +
  labs(title = "Soil NH4 Levels", x = "Fertilizer and Dose", y = "NH4 (g/kg DW)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Bar plot for NH4 means with standard error
plot_nh4_bar <- ggplot(summary_stats, aes(x = interaction(fertilizer, dose), 
                                          y = mean_nh4, fill = plant_species)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_nh4 - se_nh4, ymax = mean_nh4 + se_nh4), 
                width = 0.2, position = position_dodge(0.9)) +
  theme_minimal() +
  labs(title = "Mean Soil NH4 Levels with SE", x = "Fertilizer and Dose", y = "NH4 (g/kg DW)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


plot_no3 <- ggplot(SoilNutrients_data, aes(x = interaction(fertilizer, dose), 
                                           y = soil_no3, fill = plant_species)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.2, position = position_dodge(0.9)) +
  theme_minimal() +
  labs(title = "Soil NO3 Levels", x = "Fertilizer and Dose", y = "NO3 (g/kg DW)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_no3_bar <- ggplot(summary_stats, aes(x = interaction(fertilizer, dose), 
                                          y = mean_no3, fill = plant_species)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_no3 - se_no3, ymax = mean_no3 + se_no3), 
                width = 0.2, position = position_dodge(0.9)) +
  theme_minimal() +
  labs(title = "Mean Soil NO3 Levels with SE", x = "Fertilizer and Dose", y = "NO3 (g/kg DW)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_p <- ggplot(SoilNutrients_data, aes(x = interaction(fertilizer, dose), 
                                         y = soil_p, fill = plant_species)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.2, position = position_dodge(0.9)) +
  theme_minimal() +
  labs(title = "Soil P Levels", x = "Fertilizer and Dose", y = "P (g/kg DW)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_p_bar <- ggplot(summary_stats, aes(x = interaction(fertilizer, dose), 
                                        y = mean_p, fill = plant_species)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_p - se_p, ymax = mean_p + se_p), 
                width = 0.2, position = position_dodge(0.9)) +
  theme_minimal() +
  labs(title = "Mean Soil P Levels with SE", x = "Fertilizer and Dose", y = "P (g/kg DW)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine violin plots
grid.arrange(plot_nh4, plot_no3, plot_p, nrow = 3)

# Combine bar plots
grid.arrange(plot_nh4_bar, plot_no3_bar, plot_p_bar, nrow = 3)






# Compute summary statistics
summary_stats <- SoilNutrients_data %>%
  group_by(fertilizer, dose, plant_species) %>%
  summarise(
    mean_nh4 = mean(soil_nh4, na.rm = TRUE),
    se_nh4 = sd(soil_nh4, na.rm = TRUE) / sqrt(n()),
    mean_no3 = mean(soil_no3, na.rm = TRUE),
    se_no3 = sd(soil_no3, na.rm = TRUE) / sqrt(n()),
    mean_p = mean(soil_p, na.rm = TRUE),
    se_p = sd(soil_p, na.rm = TRUE) / sqrt(n())
  )
# Combined plot for NH4
plot_nh4 <- ggplot(SoilNutrients_data, aes(x = interaction(fertilizer, dose), y = soil_nh4, fill = plant_species)) +
  # Violin plot
  geom_violin(trim = FALSE, alpha = 0.5) +
  # Boxplot
  geom_boxplot(width = 0.2, position = position_dodge(0.9)) +
  # Bar plot
  geom_bar(data = summary_stats, aes(x = interaction(fertilizer, dose), y = mean_nh4, fill = plant_species), 
           stat = "identity", position = position_dodge(0.9), color = "black", alpha = 0.4) +
  # Error bars for bar plot
  geom_errorbar(data = summary_stats, aes(x = interaction(fertilizer, dose), ymin = mean_nh4 - se_nh4, ymax = mean_nh4 + se_nh4, fill = plant_species), 
                width = 0.2, position = position_dodge(0.9), inherit.aes = FALSE) +
  # Facet by plant species
  facet_wrap(~plant_species, ncol = 2) +
  # Theme and labels
  theme_minimal() +
  labs(title = "Soil NH4 Levels", x = "Fertilizer and Dose", y = "NH4 (g/kg DW)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_no3 <- ggplot(SoilNutrients_data, aes(x = interaction(fertilizer, dose), y = soil_no3, fill = plant_species)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.2, position = position_dodge(0.9)) +
  geom_bar(data = summary_stats, aes(x = interaction(fertilizer, dose), y = mean_no3, fill = plant_species), 
           stat = "identity", position = position_dodge(0.9), color = "black", alpha = 0.4) +
  geom_errorbar(data = summary_stats, aes(x = interaction(fertilizer, dose), ymin = mean_no3 - se_no3, ymax = mean_no3 + se_no3, fill = plant_species), 
                width = 0.2, position = position_dodge(0.9), inherit.aes = FALSE) +
  facet_wrap(~plant_species, ncol = 2) +
  theme_minimal() +
  labs(title = "Soil NO3 Levels", x = "Fertilizer and Dose", y = "NO3 (g/kg DW)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_p <- ggplot(SoilNutrients_data, aes(x = interaction(fertilizer, dose), y = soil_p, fill = plant_species)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.2, position = position_dodge(0.9)) +
  geom_bar(data = summary_stats, aes(x = interaction(fertilizer, dose), y = mean_p, fill = plant_species), 
           stat = "identity", position = position_dodge(0.9), color = "black", alpha = 0.4) +
  geom_errorbar(data = summary_stats, aes(x = interaction(fertilizer, dose), ymin = mean_p - se_p, ymax = mean_p + se_p, fill = plant_species), 
                width = 0.2, position = position_dodge(0.9), inherit.aes = FALSE) +
  facet_wrap(~plant_species, ncol = 2) +
  theme_minimal() +
  labs(title = "Soil P Levels", x = "Fertilizer and Dose", y = "P (g/kg DW)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
library(gridExtra)

# Arrange all plots in a grid
grid.arrange(plot_nh4, plot_no3, plot_p, nrow = 3)







FinalData_EXP1 <-read.csv("Final_Data_for analysis_Experiment1.csv")
colnames(FinalData_EXP1)
# Load required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(car)

# Load the dataset
FinalData_EXP1 <- read.csv("Final_Data_for analysis_Experiment1.csv")

# Rename and preprocess columns for easier handling
Leachates_data <- FinalData_EXP1 %>%
  rename(
    fertilizer_type = Fertilizer_Type,
    dose = Dose..N.kg.ha.,
    plant_species = Plant.type,
    nh4_leachate = NH4_Lechates_HarvestDay..mg.N.L.,
    no3_leachate = NO3_Lechates_HarvestDay...mg.N.L.,
    po4_leachate = PO4_Lechates_HarvestDay.mg.N.L.
  ) %>%
  mutate(
    dose = factor(dose, levels = c("0", "100", "200")),       # Convert dose to factor
    fertilizer = factor(fertilizer_type, levels = c("None", "MF", "UF")), # Convert fertilizer to factor
    plant_species = factor(plant_species)                    # Convert plant_species to factor
  )

# Check structure
str(Leachates_data)
# Compute summary statistics
summary_stats_leachates <- Leachates_data %>%
  group_by(fertilizer, dose, plant_species) %>%
  summarise(
    mean_nh4 = mean(nh4_leachate, na.rm = TRUE),
    se_nh4 = sd(nh4_leachate, na.rm = TRUE) / sqrt(n()),
    mean_no3 = mean(no3_leachate, na.rm = TRUE),
    se_no3 = sd(no3_leachate, na.rm = TRUE) / sqrt(n()),
    mean_po4 = mean(po4_leachate, na.rm = TRUE),
    se_po4 = sd(po4_leachate, na.rm = TRUE) / sqrt(n())
  )
print(summary_stats_leachates)
# ANOVA for NH4 leachate
anova_nh4_leachate <- aov(nh4_leachate ~ fertilizer * dose * plant_species, data = Leachates_data)
summary(anova_nh4_leachate)
# ANOVA for NO3 leachate
anova_no3_leachate <- aov(no3_leachate ~ fertilizer * dose * plant_species, data = Leachates_data)
summary(anova_no3_leachate)
# ANOVA for PO4 leachate
anova_po4_leachate <- aov(po4_leachate ~ fertilizer * dose * plant_species, data = Leachates_data)
summary(anova_po4_leachate)
par(mfrow = c(1, 2))
plot(anova_nh4_leachate$fitted.values, residuals(anova_nh4_leachate), main = "Residuals vs Fitted (NH4 Leachate)")
qqnorm(residuals(anova_nh4_leachate), main = "QQ Plot (NH4 Leachate)")
qqline(residuals(anova_nh4_leachate))
# NO3 Leachate Residuals
plot(anova_no3_leachate$fitted.values, residuals(anova_no3_leachate), main = "Residuals vs Fitted (NO3 Leachate)")
qqnorm(residuals(anova_no3_leachate), main = "QQ Plot (NO3 Leachate)")
qqline(residuals(anova_no3_leachate))

# PO4 Leachate Residuals
plot(anova_po4_leachate$fitted.values, residuals(anova_po4_leachate), main = "Residuals vs Fitted (PO4 Leachate)")
qqnorm(residuals(anova_po4_leachate), main = "QQ Plot (PO4 Leachate)")
qqline(residuals(anova_po4_leachate))


plot_nh4_leachate <- ggplot(Leachates_data, aes(x = interaction(fertilizer, dose), y = nh4_leachate, fill = plant_species)) +
  # Violin plot
  geom_violin(trim = FALSE, alpha = 0.5) +
  # Boxplot
  geom_boxplot(width = 0.2, position = position_dodge(0.9)) +
  # Bar plot
  geom_bar(data = summary_stats_leachates, aes(x = interaction(fertilizer, dose), y = mean_nh4, fill = plant_species), 
           stat = "identity", position = position_dodge(0.9), color = "black", alpha = 0.4) +
  # Error bars for bar plot
  geom_errorbar(data = summary_stats_leachates, aes(x = interaction(fertilizer, dose), ymin = mean_nh4 - se_nh4, ymax = mean_nh4 + se_nh4, fill = plant_species), 
                width = 0.2, position = position_dodge(0.9), inherit.aes = FALSE) +
  facet_wrap(~plant_species, ncol = 2) +
  theme_minimal() +
  labs(title = "NH4 Leachates", x = "Fertilizer and Dose", y = "NH4 (mg N/L)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_no3_leachate <- ggplot(Leachates_data, aes(x = interaction(fertilizer, dose), y = no3_leachate, fill = plant_species)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.2, position = position_dodge(0.9)) +
  geom_bar(data = summary_stats_leachates, aes(x = interaction(fertilizer, dose), y = mean_no3, fill = plant_species), 
           stat = "identity", position = position_dodge(0.9), color = "black", alpha = 0.4) +
  geom_errorbar(data = summary_stats_leachates, aes(x = interaction(fertilizer, dose), ymin = mean_no3 - se_no3, ymax = mean_no3 + se_no3, fill = plant_species), 
                width = 0.2, position = position_dodge(0.9), inherit.aes = FALSE) +
  facet_wrap(~plant_species, ncol = 2) +
  theme_minimal() +
  labs(title = "NO3 Leachates", x = "Fertilizer and Dose", y = "NO3 (mg N/L)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_po4_leachate <- ggplot(Leachates_data, aes(x = interaction(fertilizer, dose), y = po4_leachate, fill = plant_species)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.2, position = position_dodge(0.9)) +
  geom_bar(data = summary_stats_leachates, aes(x = interaction(fertilizer, dose), y = mean_po4, fill = plant_species), 
           stat = "identity", position = position_dodge(0.9), color = "black", alpha = 0.4) +
  geom_errorbar(data = summary_stats_leachates, aes(x = interaction(fertilizer, dose), ymin = mean_po4 - se_po4, ymax = mean_po4 + se_po4, fill = plant_species), 
                width = 0.2, position = position_dodge(0.9), inherit.aes = FALSE) +
  facet_wrap(~plant_species, ncol = 2) +
  theme_minimal() +
  labs(title = "PO4 Leachates", x = "Fertilizer and Dose", y = "PO4 (mg N/L)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Arrange plots in a grid
grid.arrange(plot_nh4_leachate, plot_no3_leachate, plot_po4_leachate, nrow = 3)



# Load libraries
library(emmeans)
library(multcompView)

# Pairwise comparisons and compact letter display for NH4
emmeans_nh4 <- emmeans(anova_nh4, ~ fertilizer * dose * plant_species)

# Generate compact letter display
cld_nh4 <- cld(emmeans_nh4, Letters = letters, adjust = "tukey")  # Tukey adjustment for pairwise comparisons
# Inspect the structure of cld_nh4
str(cld_nh4)
head(cld_nh4)
# Convert to data frame and select relevant columns
letters_nh4 <- as.data.frame(cld_nh4)[, c("fertilizer", "dose", "plant_species", ".group")]

plot_nh4 <- ggplot(SoilNutrients_data, aes(x = interaction(fertilizer, dose), y = soil_nh4, fill = plant_species)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.2, position = position_dodge(0.9)) +
  geom_bar(data = summary_stats, aes(x = interaction(fertilizer, dose), y = mean_nh4, fill = plant_species),
           stat = "identity", position = position_dodge(0.9), color = "black", alpha = 0.4) +
  geom_errorbar(data = summary_stats, aes(x = interaction(fertilizer, dose), ymin = mean_nh4 - se_nh4, ymax = mean_nh4 + se_nh4, fill = plant_species),
                width = 0.2, position = position_dodge(0.9), inherit.aes = FALSE) +
  geom_text(data = letters_nh4, aes(x = interaction(fertilizer, dose, plant_species), y = max(SoilNutrients_data$soil_nh4, na.rm = TRUE) + 0.5, label = .group),
            inherit.aes = FALSE, position = position_dodge(0.9)) +
  facet_wrap(~plant_species, ncol = 2) +
  theme_minimal() +
  labs(title = "Soil NH4 Levels with Significance", x = "Fertilizer and Dose", y = "NH4 (g/kg DW)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Load dplyr and use dplyr::select
library(dplyr)

letters_nh4 <- cld_nh4 %>%
  as.data.frame() %>%
  dplyr::select(fertilizer, dose, plant_species, .group)
# Perform emmeans for NO3
emmeans_no3 <- emmeans(anova_no3, ~ fertilizer * dose * plant_species)

# Inspect the emmeans object
print(emmeans_no3)
# Generate compact letter display for NO3
library(multcompView)
cld_no3 <- cld(emmeans_no3, Letters = letters, adjust = "tukey")
# Convert to a data frame and extract relevant columns
letters_no3 <- as.data.frame(cld_no3)[, c("fertilizer", "dose", "plant_species", ".group")]

# Check the letters_no3 data frame
print(letters_no3)
plot_no3 <- ggplot(SoilNutrients_data, aes(x = interaction(fertilizer, dose), y = soil_no3, fill = plant_species)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.2, position = position_dodge(0.9)) +
  geom_bar(data = summary_stats, aes(x = interaction(fertilizer, dose), y = mean_no3, fill = plant_species),
           stat = "identity", position = position_dodge(0.9), color = "black", alpha = 0.4) +
  geom_errorbar(data = summary_stats, aes(x = interaction(fertilizer, dose), ymin = mean_no3 - se_no3, ymax = mean_no3 + se_no3, fill = plant_species),
                width = 0.2, position = position_dodge(0.9), inherit.aes = FALSE) +
  geom_text(data = letters_no3, aes(x = interaction(fertilizer, dose, plant_species), y = max(SoilNutrients_data$soil_no3, na.rm = TRUE) + 0.5, label = .group),
            inherit.aes = FALSE, position = position_dodge(0.9)) +
  facet_wrap(~plant_species, ncol = 2) +
  theme_minimal() +
  labs(title = "Soil NO3 Levels with Significance", x = "Fertilizer and Dose", y = "NO3 (g/kg DW)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Check the ANOVA model for NO3
summary(anova_no3)

# Check the emmeans object for NO3
emmeans_no3 <- emmeans(anova_no3, ~ fertilizer * dose * plant_species)
print(emmeans_no3)

# Check the structure of the dataset to confirm the column name for PO4
str(SoilNutrients_data)

# Create the ANOVA model for PO4 (Soil Nutrients)
anova_po4 <- aov(soil_p ~ fertilizer * dose * plant_species, data = SoilNutrients_data)

# Check the ANOVA model summary
summary(anova_po4)

# Perform emmeans for PO4 (Soil Nutrients)
emmeans_po4 <- emmeans(anova_po4, ~ fertilizer * dose * plant_species)

# Check the emmeans object
print(emmeans_po4)

# Generate compact letter display for PO4
cld_po4 <- cld(emmeans_po4, Letters = letters, adjust = "tukey")

# Check the compact letter display
print(cld_po4)

# Extract group labels and significance letters
letters_po4 <- as.data.frame(cld_po4)[, c("fertilizer", "dose", "plant_species", ".group")]

# Check the letters for PO4
print(letters_po4)
# Annotate PO4 plot with significance letters
plot_po4 <- ggplot(SoilNutrients_data, aes(x = interaction(fertilizer, dose), y = soil_p, fill = plant_species)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.2, position = position_dodge(0.9)) +
  geom_bar(data = summary_stats, aes(x = interaction(fertilizer, dose), y = mean_p, fill = plant_species),
           stat = "identity", position = position_dodge(0.9), color = "black", alpha = 0.4) +
  geom_errorbar(data = summary_stats, aes(x = interaction(fertilizer, dose), ymin = mean_p - se_p, ymax = mean_p + se_p, fill = plant_species),
                width = 0.2, position = position_dodge(0.9), inherit.aes = FALSE) +
  geom_text(data = letters_po4, aes(x = interaction(fertilizer, dose, plant_species), y = max(SoilNutrients_data$soil_p, na.rm = TRUE) + 0.5, label = .group),
            inherit.aes = FALSE, position = position_dodge(0.9)) +
  facet_wrap(~plant_species, ncol = 2) +
  theme_minimal() +
  labs(title = "Soil PO4 Levels with Significance", x = "Fertilizer and Dose", y = "PO4 (g/kg DW)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
anova_po4_leachate <- aov(po4_leachate ~ fertilizer * dose * plant_species, data = Leachates_data)
summary(anova_po4_leachate)
emmeans_po4_leachate <- emmeans(anova_po4_leachate, ~ fertilizer * dose * plant_species)
print(emmeans_po4_leachate)
cld_po4_leachate <- cld(emmeans_po4_leachate, Letters = letters, adjust = "tukey")
letters_po4_leachate <- as.data.frame(cld_po4_leachate)[, c("fertilizer", "dose", "plant_species", ".group")]
print(letters_po4_leachate)
plot_po4_leachate <- ggplot(Leachates_data, aes(x = interaction(fertilizer, dose), y = po4_leachate, fill = plant_species)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.2, position = position_dodge(0.9)) +
  geom_bar(data = summary_stats_leachates, aes(x = interaction(fertilizer, dose), y = mean_po4, fill = plant_species),
           stat = "identity", position = position_dodge(0.9), color = "black", alpha = 0.4) +
  geom_errorbar(data = summary_stats_leachates, aes(x = interaction(fertilizer, dose), ymin = mean_po4 - se_po4, ymax = mean_po4 + se_po4, fill = plant_species),
                width = 0.2, position = position_dodge(0.9), inherit.aes = FALSE) +
  geom_text(data = letters_po4_leachate, aes(x = interaction(fertilizer, dose, plant_species), y = max(Leachates_data$po4_leachate, na.rm = TRUE) + 0.5, label = .group),
            inherit.aes = FALSE, position = position_dodge(0.9)) +
  facet_wrap(~plant_species, ncol = 2) +
  theme_minimal() +
  labs(title = "PO4 Leachates with Significance", x = "Fertilizer and Dose", y = "PO4 (mg N/L)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(plot_nh4, plot_no3, plot_po4, nrow = 3)
grid.arrange(plot_nh4_leachate, plot_no3_leachate, plot_po4_leachate, nrow = 3)






colnames(FinalData_EXP1)


#Objective A: Soil Nutrients vs. Plant Biomass
#Investigate how soil nutrient concentrations (Soil_NO3.g.kg..dw_HarvestDay, Soil_P.g.kg..dw_HarvestDay, Soil_NH4.g.kg..dw_HarvestDay) correlate with plant biomass (Shoot.Biomass_harvestday.dry, Root.Biomass_harvestday.gm).

# Load required libraries
library(dplyr)
library(ggplot2)
library(corrplot)

# Load the dataset
FinalData_EXP1 <- read.csv("Final_Data_for analysis_Experiment1.csv")

# Rename columns for easier access
data <- FinalData_EXP1 %>%
  rename(
    soil_no3 = Soil_NO3.g.kg..dw_HarvestDay,
    soil_nh4 = Soil_NH4.g.kg..dw_HarvestDay,
    soil_p = Soil_P.g.kg..dw_HarvestDay,
    shoot_biomass = Shoot.Biomass_harvestday.dry,
    root_biomass = Root.Biomass_harvestday.dry,
    nh4_leachate = NH4_BF_lechates..mg.N.L.,
    no3_leachate = NO3_BF_lechates.mg.N.L.
  )
# Scatterplot: Soil NO3 vs Shoot Biomass
plot_soil_no3_shoot <- ggplot(data, aes(x = soil_no3, y = shoot_biomass)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal() +
  labs(title = "Soil NO3 vs Shoot Biomass", x = "Soil NO3 (g/kg DW)", y = "Shoot Biomass (g)")

# Scatterplot: Soil NO3 vs Root Biomass
plot_soil_no3_root <- ggplot(data, aes(x = soil_no3, y = root_biomass)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal() +
  labs(title = "Soil NO3 vs Root Biomass", x = "Soil NO3 (g/kg DW)", y = "Root Biomass (g)")
# Soil NH4 vs Shoot Biomass
plot_soil_nh4_shoot <- ggplot(data, aes(x = soil_nh4, y = shoot_biomass)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal() +
  labs(title = "Soil NH4 vs Shoot Biomass", x = "Soil NH4 (g/kg DW)", y = "Shoot Biomass (g)")

# Soil P vs Shoot Biomass
plot_soil_p_shoot <- ggplot(data, aes(x = soil_p, y = shoot_biomass)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal() +
  labs(title = "Soil P vs Shoot Biomass", x = "Soil P (g/kg DW)", y = "Shoot Biomass (g)")
# Load the dplyr library
library(dplyr)
# Subset relevant columns using dplyr::select
cor_data <- data %>%
  dplyr::select(soil_no3, soil_nh4, soil_p, shoot_biomass, root_biomass)

# Check column names
colnames(data)

# Adjust to match actual column names
cor_data <- data %>%
  dplyr::select(soil_no3, soil_nh4, soil_p,
                shoot_biomass, root_biomass)
# Subset relevant columns using base R
cor_data <- data[, c("soil_no3", "soil_nh4", "soil_p", "shoot_biomass", "root_biomass")]

# Compute correlation matrix
cor_matrix <- cor(cor_data, use = "complete.obs")

# Visualize the correlation matrix
library(corrplot)
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)



# NH4 Leachate vs Soil NH4
plot_nh4_leachate_soil <- ggplot(data, aes(x = nh4_leachate, y = soil_nh4)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_minimal() +
  labs(title = "NH4 Leachate vs Soil NH4", x = "NH4 Leachate (mg N/L)", y = "Soil NH4 (g/kg DW)")

# NO3 Leachate vs Soil NO3
plot_no3_leachate_soil <- ggplot(data, aes(x = no3_leachate, y = soil_no3)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_minimal() +
  labs(title = "NO3 Leachate vs Soil NO3", x = "NO3 Leachate (mg N/L)", y = "Soil NO3 (g/kg DW)")


cor_leachate_soil <- data %>%
  dplyr::select(nh4_leachate, no3_leachate, soil_nh4, soil_no3)
str(cor_leachate_soil)  # Check the structure
cor_leachate_soil <- cor_leachate_soil %>%
  mutate(across(everything(), as.numeric))


# Linear regression: NH4 Leachate vs Soil NH4
lm_nh4 <- lm(soil_nh4 ~ nh4_leachate, data = cor_leachate_soil)

# Summary of regression model
summary(lm_nh4)

# Visualize regression with scatterplot
plot_nh4_leachate_soil <- ggplot(cor_leachate_soil, aes(x = nh4_leachate, y = soil_nh4)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_minimal() +
  labs(title = "Regression: NH4 Leachate vs Soil NH4",
       x = "NH4 Leachate (mg N/L)", y = "Soil NH4 (g/kg DW)")

# Linear regression: NO3 Leachate vs Soil NO3
lm_no3 <- lm(soil_no3 ~ no3_leachate, data = cor_leachate_soil)

# Summary of regression model
summary(lm_no3)

# Visualize regression with scatterplot
plot_no3_leachate_soil <- ggplot(cor_leachate_soil, aes(x = no3_leachate, y = soil_no3)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_minimal() +
  labs(title = "Regression: NO3 Leachate vs Soil NO3",
       x = "NO3 Leachate (mg N/L)", y = "Soil NO3 (g/kg DW)")
library(gridExtra)

# Combine plots for NH4 and NO3 regression
grid.arrange(plot_nh4_leachate_soil, plot_no3_leachate_soil, nrow = 1)




# Load required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(corrplot)

# Load the dataset
FinalData_EXP1 <- read.csv("Final_Data_for analysis_Experiment1.csv")
# Rename columns
data <- FinalData_EXP1 %>%
  rename(
    soil_no3 = Soil_NO3.g.kg..dw_HarvestDay,
    soil_nh4 = Soil_NH4.g.kg..dw_HarvestDay,
    nh4_leachate = NH4_BF_lechates..mg.N.L.,
    no3_leachate = NO3_BF_lechates.mg.N.L.
  )
# Subset relevant columns
leachate_soil_data <- data %>%
  dplyr::select(nh4_leachate, no3_leachate, soil_nh4, soil_no3)

# Inspect the structure and summary of the subset
str(leachate_soil_data)
summary(leachate_soil_data)
# Remove rows with NA values
leachate_soil_data <- leachate_soil_data %>%
  filter(complete.cases(.))


# Compute correlation matrix
cor_matrix <- cor(leachate_soil_data, use = "complete.obs")

# Visualize correlation matrix
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)


# Linear regression for NH4 Leachate vs Soil NH4
lm_nh4 <- lm(soil_nh4 ~ nh4_leachate, data = leachate_soil_data)
summary(lm_nh4)

# Scatterplot with regression line
plot_nh4_leachate_soil <- ggplot(leachate_soil_data, aes(x = nh4_leachate, y = soil_nh4)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_minimal() +
  labs(title = "NH4 Leachate vs Soil NH4",
       x = "NH4 Leachate (mg N/L)", y = "Soil NH4 (g/kg DW)")


# Linear regression for NO3 Leachate vs Soil NO3
lm_no3 <- lm(soil_no3 ~ no3_leachate, data = leachate_soil_data)
summary(lm_no3)

# Scatterplot with regression line
plot_no3_leachate_soil <- ggplot(leachate_soil_data, aes(x = no3_leachate, y = soil_no3)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_minimal() +
  labs(title = "NO3 Leachate vs Soil NO3",
       x = "NO3 Leachate (mg N/L)", y = "Soil NO3 (g/kg DW)")


# Combine scatterplots for NH4 and NO3 regression
grid.arrange(plot_nh4_leachate_soil, plot_no3_leachate_soil, nrow = 1)



#Soil-Plant Relationships
colnames(FinalData_EXP1)


# Load necessary libraries
library(ggplot2)
library(dplyr)

# Ensure dplyr::select() is explicitly used
soil_biomass_data <- FinalData_EXP1 %>%
  dplyr::select(
    Soil_NO3 = Soil_NO3.g.kg..dw_HarvestDay, 
    Soil_P = Soil_P.g.kg..dw_HarvestDay, 
    Shoot_Biomass = Shoot.Biomass_harvestday.dry, 
    Root_Biomass = Root.Biomass_harvestday.dry
  )
# Plot: Soil_NO3 vs. Shoot Biomass
ggplot(soil_biomass_data, aes(x = Soil_NO3, y = Shoot_Biomass)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Soil NO3 vs. Shoot Biomass", x = "Soil NO3 (g/kg dw)", y = "Shoot Biomass (dry, g)") +
  theme_minimal()

# Plot: Soil_P vs. Root Biomass
ggplot(soil_biomass_data, aes(x = Soil_P, y = Root_Biomass)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "green") +
  labs(title = "Soil P vs. Root Biomass", x = "Soil P (g/kg dw)", y = "Root Biomass (dry, g)") +
  theme_minimal()

# Linear regression models
lm_soil_no3_shoot <- lm(Shoot_Biomass ~ Soil_NO3, data = soil_biomass_data)
lm_soil_p_root <- lm(Root_Biomass ~ Soil_P, data = soil_biomass_data)

# Summarize regression results
summary(lm_soil_no3_shoot)
summary(lm_soil_p_root)
# Ensure dplyr::select() is used
leachates_soil_data <- FinalData_EXP1 %>%
  dplyr::select(
    NH4_Leachate = NH4_BF_lechates..mg.N.L., 
    NO3_Leachate = NO3_BF_lechates.mg.N.L., 
    Soil_NH4 = Soil_NH4.g.kg..dw_HarvestDay, 
    Soil_NO3 = Soil_NO3.g.kg..dw_HarvestDay
  )

# Plot: NH4 Leachate vs. Soil NH4
ggplot(leachates_soil_data, aes(x = NH4_Leachate, y = Soil_NH4)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "purple") +
  labs(title = "NH4 Leachate vs. Soil NH4", x = "NH4 Leachate (mg N/L)", y = "Soil NH4 (g/kg dw)") +
  theme_minimal()

# Plot: NO3 Leachate vs. Soil NO3
ggplot(leachates_soil_data, aes(x = NO3_Leachate, y = Soil_NO3)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  labs(title = "NO3 Leachate vs. Soil NO3", x = "NO3 Leachate (mg N/L)", y = "Soil NO3 (g/kg dw)") +
  theme_minimal()

# Linear regression models
lm_nh4 <- lm(Soil_NH4 ~ NH4_Leachate, data = leachates_soil_data)
lm_no3 <- lm(Soil_NO3 ~ NO3_Leachate, data = leachates_soil_data)

# Summarize regression results
summary(lm_nh4)
summary(lm_no3)

#code to address the two questions about Nutrient Uptake by Shoots and Roots using the provided dataset

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Select relevant columns for analysis
shoot_data <- FinalData_EXP1 %>%
  dplyr::select(
    Fertilizer_Type,
    `Dose..N.kg.ha.`,
    Shoot_P = `Shoot_P_.ppm.`,
    Shoot_K = `Shoot_K_.ppm.`,
    Shoot_Ca = `Shoot_Ca_.ppm.`,
    Fresh_Biomass = `Fresh.Biomass_harvestday..gm.`,
    Chlorophyll = `Cholorophyll_Data.nmol.ch.mg.fresh.weight._HarvestDay`
  )

# Convert dose to factor for comparison
shoot_data <- shoot_data %>%
  mutate(Dose = factor(`Dose..N.kg.ha.`))

# (A1) Compare nutrient concentrations across dose and fertilizer types
# Boxplot: Shoot_P_.ppm. across Dose and Fertilizer_Type
ggplot(shoot_data, aes(x = Dose, y = Shoot_P, fill = Fertilizer_Type)) +
  geom_boxplot() +
  labs(title = "Shoot P Concentrations Across Doses and Fertilizer Types", 
       x = "Dose (kg/ha)", y = "Shoot P (ppm)", fill = "Fertilizer Type") +
  theme_minimal()

# Boxplot: Shoot_K_.ppm. across Dose and Fertilizer_Type
ggplot(shoot_data, aes(x = Dose, y = Shoot_K, fill = Fertilizer_Type)) +
  geom_boxplot() +
  labs(title = "Shoot K Concentrations Across Doses and Fertilizer Types", 
       x = "Dose (kg/ha)", y = "Shoot K (ppm)", fill = "Fertilizer Type") +
  theme_minimal()

# Boxplot: Shoot_Ca_.ppm. across Dose and Fertilizer_Type
ggplot(shoot_data, aes(x = Dose, y = Shoot_Ca, fill = Fertilizer_Type)) +
  geom_boxplot() +
  labs(title = "Shoot Ca Concentrations Across Doses and Fertilizer Types", 
       x = "Dose (kg/ha)", y = "Shoot Ca (ppm)", fill = "Fertilizer Type") +
  theme_minimal()

# (A2) Correlation between shoot nutrient concentrations and plant health metrics
# Correlation: Shoot_P vs. Fresh Biomass
cor_shoot_p_biomass <- cor(shoot_data$Shoot_P, shoot_data$Fresh_Biomass, use = "complete.obs")
cat("Correlation between Shoot P and Fresh Biomass:", cor_shoot_p_biomass, "\n")

# Correlation: Shoot_K vs. Chlorophyll
cor_shoot_k_chlorophyll <- cor(shoot_data$Shoot_K, shoot_data$Chlorophyll, use = "complete.obs")
cat("Correlation between Shoot K and Chlorophyll:", cor_shoot_k_chlorophyll, "\n")

# Scatterplots for correlation visualization
# Scatterplot: Shoot_P vs. Fresh Biomass
ggplot(shoot_data, aes(x = Shoot_P, y = Fresh_Biomass)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Shoot P vs. Fresh Biomass", x = "Shoot P (ppm)", y = "Fresh Biomass (gm)") +
  theme_minimal()

# Scatterplot: Shoot_K vs. Chlorophyll
ggplot(shoot_data, aes(x = Shoot_K, y = Chlorophyll)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "green") +
  labs(title = "Shoot K vs. Chlorophyll", x = "Shoot K (ppm)", y = "Chlorophyll (nmol/mg FW)") +
  theme_minimal()

# Select relevant columns for analysis
root_data <- FinalData_EXP1 %>%
  dplyr::select(
    Fertilizer_Type,
    `Dose..N.kg.ha.`,
    Roots_P = `Roots_P_.ppm.`,
    Roots_K = `Roots_K_.ppm.`,
    RS_Ratio = `R.S.ratio`
  )

# Convert dose to factor for comparison
root_data <- root_data %>%
  mutate(Dose = factor(`Dose..N.kg.ha.`))

# (B1) Compare root nutrient concentrations across dose and fertilizer types
# Boxplot: Roots_P_.ppm. across Dose and Fertilizer_Type
ggplot(root_data, aes(x = Dose, y = Roots_P, fill = Fertilizer_Type)) +
  geom_boxplot() +
  labs(title = "Root P Concentrations Across Doses and Fertilizer Types", 
       x = "Dose (kg/ha)", y = "Root P (ppm)", fill = "Fertilizer Type") +
  theme_minimal()

# Boxplot: Roots_K_.ppm. across Dose and Fertilizer_Type
ggplot(root_data, aes(x = Dose, y = Roots_K, fill = Fertilizer_Type)) +
  geom_boxplot() +
  labs(title = "Root K Concentrations Across Doses and Fertilizer Types", 
       x = "Dose (kg/ha)", y = "Root K (ppm)", fill = "Fertilizer Type") +
  theme_minimal()

# (B2) Investigate R:S ratio across treatments
# Boxplot: R:S Ratio across Dose and Fertilizer_Type
ggplot(root_data, aes(x = Dose, y = RS_Ratio, fill = Fertilizer_Type)) +
  geom_boxplot() +
  labs(title = "R:S Ratio Across Doses and Fertilizer Types", 
       x = "Dose (kg/ha)", y = "Root-to-Shoot Ratio", fill = "Fertilizer Type") +
  theme_minimal()

# Summary statistics for R:S Ratio
rs_summary <- root_data %>%
  group_by(Fertilizer_Type, Dose) %>%
  summarise(
    Mean_RS = mean(RS_Ratio, na.rm = TRUE),
    SD_RS = sd(RS_Ratio, na.rm = TRUE)
  )
print(rs_summary)


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(car)         # For ANOVA
library(emmeans)     # For pairwise comparisons

# Select relevant data for shoot analysis
shoot_data <- FinalData_EXP1 %>%
  dplyr::select(
    Fertilizer_Type,
    Dose = `Dose..N.kg.ha.`,
    Shoot_P = `Shoot_P_.ppm.`,
    Shoot_K = `Shoot_K_.ppm.`,
    Shoot_Ca = `Shoot_Ca_.ppm.`,
    Fresh_Biomass = `Fresh.Biomass_harvestday..gm.`,
    Chlorophyll = `Cholorophyll_Data.nmol.ch.mg.fresh.weight._HarvestDay`
  ) %>%
  mutate(Dose = factor(Dose))

# Compute mean and standard error for each nutrient
shoot_summary <- shoot_data %>%
  group_by(Fertilizer_Type, Dose) %>%
  summarise(
    Mean_Shoot_P = mean(Shoot_P, na.rm = TRUE),
    SE_Shoot_P = sd(Shoot_P, na.rm = TRUE) / sqrt(n()),
    Mean_Shoot_K = mean(Shoot_K, na.rm = TRUE),
    SE_Shoot_K = sd(Shoot_K, na.rm = TRUE) / sqrt(n()),
    Mean_Shoot_Ca = mean(Shoot_Ca, na.rm = TRUE),
    SE_Shoot_Ca = sd(Shoot_Ca, na.rm = TRUE) / sqrt(n())
  )
print(shoot_summary)

# Linear models for nutrient concentrations
lm_shoot_p <- lm(Shoot_P ~ Dose * Fertilizer_Type, data = shoot_data)
lm_shoot_k <- lm(Shoot_K ~ Dose * Fertilizer_Type, data = shoot_data)
lm_shoot_ca <- lm(Shoot_Ca ~ Dose * Fertilizer_Type, data = shoot_data)

# QQ Plots for residuals
qqnorm(resid(lm_shoot_p), main = "QQ Plot: Residuals of Shoot P")
qqline(resid(lm_shoot_p), col = "red")

qqnorm(resid(lm_shoot_k), main = "QQ Plot: Residuals of Shoot K")
qqline(resid(lm_shoot_k), col = "red")

qqnorm(resid(lm_shoot_ca), main = "QQ Plot: Residuals of Shoot Ca")
qqline(resid(lm_shoot_ca), col = "red")

# ANOVA and pairwise comparisons
anova_shoot_p <- Anova(lm_shoot_p, type = "III")
anova_shoot_k <- Anova(lm_shoot_k, type = "III")
anova_shoot_ca <- Anova(lm_shoot_ca, type = "III")

# Tukey's HSD for pairwise comparisons
pairwise_shoot_p <- emmeans(lm_shoot_p, pairwise ~ Dose | Fertilizer_Type)
pairwise_shoot_k <- emmeans(lm_shoot_k, pairwise ~ Dose | Fertilizer_Type)
pairwise_shoot_ca <- emmeans(lm_shoot_ca, pairwise ~ Dose | Fertilizer_Type)

# Visualize Shoot P with pairwise comparison
ggplot(shoot_data, aes(x = Dose, y = Shoot_P, fill = Fertilizer_Type)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 20, size = 3, color = "black") +
  labs(title = "Shoot P Concentrations Across Treatments", x = "Dose (kg/ha)", y = "Shoot P (ppm)", fill = "Fertilizer Type") +
  theme_minimal()

# Add pairwise comparison letters to the plot (optional if needed)
# Select relevant data for root analysis
root_data <- FinalData_EXP1 %>%
  dplyr::select(
    Fertilizer_Type,
    Dose = `Dose..N.kg.ha.`,
    Roots_P = `Roots_P_.ppm.`,
    Roots_K = `Roots_K_.ppm.`,
    RS_Ratio = `R.S.ratio`
  ) %>%
  mutate(Dose = factor(Dose))

# Compute mean and standard error for each nutrient
root_summary <- root_data %>%
  group_by(Fertilizer_Type, Dose) %>%
  summarise(
    Mean_Roots_P = mean(Roots_P, na.rm = TRUE),
    SE_Roots_P = sd(Roots_P, na.rm = TRUE) / sqrt(n()),
    Mean_Roots_K = mean(Roots_K, na.rm = TRUE),
    SE_Roots_K = sd(Roots_K, na.rm = TRUE) / sqrt(n()),
    Mean_RS_Ratio = mean(RS_Ratio, na.rm = TRUE),
    SE_RS_Ratio = sd(RS_Ratio, na.rm = TRUE) / sqrt(n())
  )
print(root_summary)

# Linear models for root nutrient concentrations
lm_roots_p <- lm(Roots_P ~ Dose * Fertilizer_Type, data = root_data)
lm_roots_k <- lm(Roots_K ~ Dose * Fertilizer_Type, data = root_data)
lm_rs_ratio <- lm(RS_Ratio ~ Dose * Fertilizer_Type, data = root_data)

# QQ Plots for residuals
qqnorm(resid(lm_roots_p), main = "QQ Plot: Residuals of Roots P")
qqline(resid(lm_roots_p), col = "red")

qqnorm(resid(lm_roots_k), main = "QQ Plot: Residuals of Roots K")
qqline(resid(lm_roots_k), col = "red")

qqnorm(resid(lm_rs_ratio), main = "QQ Plot: Residuals of R:S Ratio")
qqline(resid(lm_rs_ratio), col = "red")

# ANOVA and pairwise comparisons
anova_roots_p <- Anova(lm_roots_p, type = "III")
anova_roots_k <- Anova(lm_roots_k, type = "III")
anova_rs_ratio <- Anova(lm_rs_ratio, type = "III")

# Tukey's HSD for pairwise comparisons
pairwise_roots_p <- emmeans(lm_roots_p, pairwise ~ Dose | Fertilizer_Type)
pairwise_roots_k <- emmeans(lm_roots_k, pairwise ~ Dose | Fertilizer_Type)
pairwise_rs_ratio <- emmeans(lm_rs_ratio, pairwise ~ Dose | Fertilizer_Type)

# Visualize Roots P with pairwise comparison
ggplot(root_data, aes(x = Dose, y = Roots_P, fill = Fertilizer_Type)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 20, size = 3, color = "black") +
  labs(title = "Root P Concentrations Across Treatments", x = "Dose (kg/ha)", y = "Root P (ppm)", fill = "Fertilizer Type") +
  theme_minimal()

# Add pairwise comparison letters to the plot (optional if needed)

# Install additional library if not already installed
if (!require(multcompView)) install.packages("multcompView")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(car)
library(emmeans)
library(multcompView) # For compact letter display

# Generate emmeans object
pairwise_shoot_p <- emmeans(lm_shoot_p, ~ Dose | Fertilizer_Type)

# Generate pairwise comparisons
pairwise_contrasts <- contrast(pairwise_shoot_p, method = "pairwise")

# Check the p_values object
p_values <- summary(pairwise_contrasts)$p.value
names(p_values) <- as.character(summary(pairwise_contrasts)$contrast)

# Print p_values
print(p_values)

# Check for NA values
any(is.na(p_values))  # TRUE if there are NA values
# Remove NA values from p_values
p_values <- p_values[!is.na(p_values)]
# Generate compact letter display
letters_shoot_p <- multcompView::multcompLetters(p_values)$Letters
# Print the letters
print(letters_shoot_p)

# Add letters to emmeans dataframe
emmeans_shoot_p <- as.data.frame(pairwise_shoot_p)
emmeans_shoot_p$Letters <- letters_shoot_p[emmeans_shoot_p$Dose]
# Linear model for Shoot_P
lm_shoot_p <- lm(Shoot_P ~ Dose * Fertilizer_Type, data = shoot_data)

# Generate emmeans
pairwise_shoot_p <- emmeans(lm_shoot_p, ~ Dose | Fertilizer_Type)

# Generate pairwise contrasts
pairwise_contrasts <- contrast(pairwise_shoot_p, method = "pairwise")

# Extract p-values and assign proper names
p_values <- summary(pairwise_contrasts)$p.value
names(p_values) <- as.character(summary(pairwise_contrasts)$contrast)

# Remove NA values if any
p_values <- p_values[!is.na(p_values)]

# Generate compact letter display
letters_shoot_p <- multcompView::multcompLetters(p_values)$Letters

# Add letters to emmeans dataframe
emmeans_shoot_p <- as.data.frame(pairwise_shoot_p)
emmeans_shoot_p$Letters <- letters_shoot_p[emmeans_shoot_p$Dose]

# Create boxplot with pairwise comparison letters
ggplot(shoot_data, aes(x = Dose, y = Shoot_P, fill = Fertilizer_Type)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 20, size = 3, color = "black") +
  labs(title = "Shoot P Concentrations Across Treatments", x = "Dose (kg/ha)", y = "Shoot P (ppm)", fill = "Fertilizer Type") +
  theme_minimal() +
  geom_text(data = emmeans_shoot_p, 
            aes(x = Dose, y = emmean + SE + 0.1, label = Letters), 
            position = position_dodge(width = 0.75), vjust = 0)

# Load required libraries
library(ggplot2)
library(dplyr)
library(car)       # For ANOVA
library(emmeans)   # For pairwise comparisons
library(multcompView)  # For compact letter display

# Select data for shoots
shoot_data <- FinalData_EXP1 %>%
  dplyr::select(
    Fertilizer_Type,
    Dose = `Dose..N.kg.ha.`,
    Shoot_Na = `Shoot_Na._.ppm.`,
    Shoot_P = `Shoot_P_.ppm.`,
    Shoot_K = `Shoot_K_.ppm.`,
    Shoot_Ca = `Shoot_Ca_.ppm.`
  ) %>%
  mutate(Dose = factor(Dose))  # Convert Dose to factor for comparisons

# Linear models for Shoot_P
lm_shoot_p <- lm(Shoot_P ~ Dose * Fertilizer_Type, data = shoot_data)
lm_shoot_na <- lm(Shoot_Na ~ Dose * Fertilizer_Type, data = shoot_data)

# ANOVA results
anova_shoot_p <- Anova(lm_shoot_p, type = "II")
anova_shoot_na <- Anova(lm_shoot_na, type = "II")

# Print ANOVA results
print(anova_shoot_p)
print(anova_shoot_na)

# Tukey's HSD for Shoot_P
pairwise_shoot_p <- emmeans(lm_shoot_p, ~ Dose | Fertilizer_Type)
pairwise_contrasts_shoot_p <- contrast(pairwise_shoot_p, method = "pairwise")
p_values_shoot_p <- summary(pairwise_contrasts_shoot_p)$p.value
names(p_values_shoot_p) <- as.character(summary(pairwise_contrasts_shoot_p)$contrast)

# Generate compact letter display for Shoot_P
letters_shoot_p <- multcompView::multcompLetters(p_values_shoot_p)$Letters

# Linear model for Shoot_P
lm_shoot_p <- lm(Shoot_P ~ Dose * Fertilizer_Type, data = shoot_data)

# Generate emmeans
pairwise_shoot_p <- emmeans(lm_shoot_p, ~ Dose | Fertilizer_Type)

# Generate pairwise contrasts
pairwise_contrasts <- contrast(pairwise_shoot_p, method = "pairwise")

# Extract p-values and assign proper names
p_values <- summary(pairwise_contrasts)$p.value
names(p_values) <- as.character(summary(pairwise_contrasts)$contrast)

# Remove NA values
p_values <- p_values[!is.na(p_values)]

# Generate compact letter display
letters_shoot_p <- multcompView::multcompLetters(p_values)$Letters

# Add letters to emmeans dataframe
emmeans_shoot_p <- as.data.frame(pairwise_shoot_p)
emmeans_shoot_p$Letters <- letters_shoot_p[emmeans_shoot_p$Dose]

# Create boxplot with pairwise comparison letters
ggplot(shoot_data, aes(x = Dose, y = Shoot_P, fill = Fertilizer_Type)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 20, size = 3, color = "black") +
  labs(title = "Shoot P Concentrations Across Treatments", x = "Dose (kg/ha)", y = "Shoot P (ppm)", fill = "Fertilizer Type") +
  theme_minimal() +
  geom_text(data = emmeans_shoot_p, 
            aes(x = Dose, y = emmean + 0.1, label = Letters), 
            position = position_dodge(width = 0.75), vjust = 0)
# Inspect pairwise contrasts summary
summary(pairwise_contrasts)

# Check p-values for NA
p_values <- summary(pairwise_contrasts)$p.value
print(p_values)


# Plot proportionality of Shoot_P to Dose
ggplot(shoot_data, aes(x = as.numeric(Dose), y = Shoot_P, color = Fertilizer_Type)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Proportionality of Shoot P to Fertilizer Dose", x = "Dose (kg/ha)", y = "Shoot P (ppm)") +
  theme_minimal()

# Select data for roots
root_data <- FinalData_EXP1 %>%
  dplyr::select(
    Fertilizer_Type,
    Dose = `Dose..N.kg.ha.`,
    Roots_Na = `Roots_Na._.ppm.`,
    Roots_P = `Roots_P_.ppm.`,
    Roots_K = `Roots_K_.ppm.`
  ) %>%
  mutate(Dose = factor(Dose))  # Convert Dose to factor for comparisons

# Linear models for Roots_P
lm_roots_p <- lm(Roots_P ~ Dose * Fertilizer_Type, data = root_data)

# ANOVA results
anova_roots_p <- Anova(lm_roots_p, type = "II")
print(anova_roots_p)

# Tukey's HSD for Roots_P
pairwise_roots_p <- emmeans(lm_roots_p, ~ Dose | Fertilizer_Type)
pairwise_contrasts_roots_p <- contrast(pairwise_roots_p, method = "pairwise")
p_values_roots_p <- summary(pairwise_contrasts_roots_p)$p.value
names(p_values_roots_p) <- as.character(summary(pairwise_contrasts_roots_p)$contrast)

# Generate compact letter display for Roots_P
letters_roots_p <- multcompView::multcompLetters(p_values_roots_p)$Letters

# Print p-values to inspect
print(p_values_roots_p)

# Check for NA values
any(is.na(p_values_roots_p))  # Returns TRUE if NA values are present
# Remove NA values from p-values
p_values_roots_p_clean <- p_values_roots_p[!is.na(p_values_roots_p)]
# Assign names if not already present
names(p_values_roots_p_clean) <- names(p_values_roots_p)[!is.na(p_values_roots_p)]
# Generate compact letter display
letters_roots_p <- multcompView::multcompLetters(p_values_roots_p_clean)$Letters
print(letters_roots_p)
# Linear model for Roots_P
lm_roots_p <- lm(Roots_P ~ Dose * Fertilizer_Type, data = root_data)

# Generate emmeans
pairwise_roots_p <- emmeans(lm_roots_p, ~ Dose | Fertilizer_Type)

# Generate pairwise contrasts
pairwise_contrasts_roots_p <- contrast(pairwise_roots_p, method = "pairwise")

# Extract p-values and assign names
p_values_roots_p <- summary(pairwise_contrasts_roots_p)$p.value
names(p_values_roots_p) <- as.character(summary(pairwise_contrasts_roots_p)$contrast)

# Remove NA values
p_values_roots_p_clean <- p_values_roots_p[!is.na(p_values_roots_p)]
names(p_values_roots_p_clean) <- names(p_values_roots_p)[!is.na(p_values_roots_p)]

# Generate compact letter display
letters_roots_p <- multcompView::multcompLetters(p_values_roots_p_clean)$Letters

# Add letters to emmeans dataframe
emmeans_roots_p <- as.data.frame(pairwise_roots_p)
emmeans_roots_p$Letters <- letters_roots_p[emmeans_roots_p$Dose]

# Create boxplot with pairwise comparison letters
ggplot(root_data, aes(x = Dose, y = Roots_P, fill = Fertilizer_Type)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 20, size = 3, color = "black") +
  labs(title = "Roots P Concentrations Across Treatments", x = "Dose (kg/ha)", y = "Roots P (ppm)", fill = "Fertilizer Type") +
  theme_minimal() +
  geom_text(data = emmeans_roots_p, 
            aes(x = Dose, y = emmean + 0.1, label = Letters), 
            position = position_dodge(width = 0.75), vjust = 0)


# Combine shoot and root data for P comparison
shoot_root_data <- FinalData_EXP1 %>%
  dplyr::select(
    Fertilizer_Type,
    Dose = `Dose..N.kg.ha.`,
    Shoot_P = `Shoot_P_.ppm.`,
    Roots_P = `Roots_P_.ppm.`
  ) %>%
  mutate(Dose = factor(Dose))
# Plot Shoot_P vs Roots_P
ggplot(shoot_root_data, aes(x = Roots_P, y = Shoot_P, color = Fertilizer_Type)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Comparison of Shoot P vs Roots P", x = "Roots P (ppm)", y = "Shoot P (ppm)") +
  theme_minimal()



colnames(FinalData_EXP1)
