FinalData_EXP1 <- read.csv("Final_Data_for analysis_Experiment1.csv")
colnames(FinalData_EXP1)

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
colnames(FinalData_EXP1)

# Assuming your dataset is named `FinalData_EXP1`
data <- FinalData_EXP1

ph_long <- FinalData_EXP1 %>%
  dplyr::select(Plant.type, Fertilizer_Type, Dose..N.kg.ha., all_of(ph_columns)) %>%
  pivot_longer(cols = all_of(ph_columns), names_to = "Timepoint", values_to = "pH") %>%
  mutate(Timepoint = factor(Timepoint, 
                            levels = ph_columns, 
                            labels = c("0", "7", "30", "60")))

head(FinalData_EXP1)

# Define the pH and EC columns to analyze
ph_columns <- c("pH_beforefertilization", 
                "pH_afterFertilization_7thday", 
                "pH_Afterfertilization_30days", 
                "pH.Harvest.day_after2ndfertilization_60thday")

ec_columns <- c("EC_beforefertilization", 
                "EC_Afterfertilization_30days", 
                "EC.harvest.day")

class(data)


# Reshape data for pH
ph_long <- data %>%
  dplyr::select(Plant.type, Fertilizer_Type, `Dose..N.kg.ha.`, all_of(ph_columns)) %>%
  pivot_longer(cols = all_of(ph_columns), names_to = "Timepoint", values_to = "pH") %>%
  mutate(Timepoint = factor(Timepoint, 
                            levels = ph_columns, 
                            labels = c("0", "7", "30", "60")))

# Calculate mean and standard error for pH
ph_summary <- ph_long %>%
  group_by(Plant.type, Fertilizer_Type, `Dose..N.kg.ha.`, Timepoint) %>%
  summarise(
    Mean_pH = mean(pH, na.rm = TRUE),
    SE_pH = sd(pH, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
# Plot pH changes over time
ph_plot <- ggplot(ph_summary, aes(x = Timepoint, y = Mean_pH, 
                                  group = `Dose..N.kg.ha.`, color = factor(`Dose..N.kg.ha.`))) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = Mean_pH - SE_pH, ymax = Mean_pH + SE_pH), width = 0.2) +
  facet_grid(Plant.type ~ Fertilizer_Type) +
  labs(title = "pH Trends Over Time", x = "Timepoint after Fertilization (Days)", y = "pH", color = "Dose (N.kg/ha)") +
  theme_minimal()

# Print the plot
print(ph_plot)


# Reshape data for EC
ec_long <- data %>%
  dplyr::select(Plant.type, Fertilizer_Type, `Dose..N.kg.ha.`, all_of(ec_columns)) %>%
  pivot_longer(cols = all_of(ec_columns), names_to = "Timepoint", values_to = "EC") %>%
  mutate(Timepoint = factor(Timepoint, 
                            levels = ec_columns, 
                            labels = c("0", "30", "60")))

# Calculate mean and standard error for EC
ec_summary <- ec_long %>%
  group_by(Plant.type, Fertilizer_Type, `Dose..N.kg.ha.`, Timepoint) %>%
  summarise(
    Mean_EC = mean(EC, na.rm = TRUE),
    SE_EC = sd(EC, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
# Plot EC changes over time
ec_plot <- ggplot(ec_summary, aes(x = Timepoint, y = Mean_EC, 
                                  group = `Dose..N.kg.ha.`, color = factor(`Dose..N.kg.ha.`))) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = Mean_EC - SE_EC, ymax = Mean_EC + SE_EC), width = 0.2) +
  facet_grid(Plant.type ~ Fertilizer_Type) +
  labs(title = "EC Trends Over Time", x = "Timepoint after Fertilization (Days)", y = "EC", color = "Dose (N.kg/ha)") +
  theme_minimal()

# Print the plot
print(ec_plot)




# Load necessary libraries
library(ggplot2)
library(dplyr)

# Inspect the dataset structure
colnames(FinalData_EXP1)

# Filter relevant columns for the analysis
data_subset <- FinalData_EXP1 %>%
  dplyr::select(`Ionic.strength..mol.L.`, `EC.harvest.day`) %>%
  filter(!is.na(`Ionic.strength..mol.L.`), !is.na(`EC.harvest.day`))  # Remove rows with missing values

# Perform linear regression
lm_model <- lm(`EC.harvest.day` ~ `Ionic.strength..mol.L.`, data = data_subset)

# Summary of the linear regression model
summary(lm_model)

# Extract residuals and fitted values for diagnostic plots
residuals <- resid(lm_model)
fitted_values <- fitted(lm_model)

# Plot the regression line
ggplot(data_subset, aes(x = `Ionic.strength..mol.L.`, y = `EC.harvest.day`)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red", formula = y ~ x) +
  labs(title = "Linear Regression: Ionic Strength vs. EC Harvest Day",
       x = "Ionic Strength (mol/L)",
       y = "EC at Harvest Day") +
  theme_minimal()

# Diagnostic plots for linear regression
# 1. Residual vs. Fitted
ggplot(data = data.frame(Fitted = fitted_values, Residuals = residuals), aes(x = Fitted, y = Residuals)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

# 2. QQ plot of residuals
qqnorm(residuals, main = "QQ Plot of Residuals")
qqline(residuals, col = "red", lwd = 2)

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Inspect dataset structure
colnames(FinalData_EXP1)

# Select relevant columns and filter for non-NA values
data_subset <- FinalData_EXP1 %>%
  dplyr::select(`Ionic.strength..mol.L.`, `EC.harvest.day`, Fertilizer_Type, `Dose..N.kg.ha.`) %>%
  filter(!is.na(`Ionic.strength..mol.L.`), !is.na(`EC.harvest.day`)) %>%
  mutate(Dose = factor(`Dose..N.kg.ha.`))  # Convert Dose to a factor for visualization
# Fit the linear model with interactions
lm_model <- lm(`EC.harvest.day` ~ `Ionic.strength..mol.L.` * Fertilizer_Type * Dose, data = data_subset)

# Summary of the linear model
summary(lm_model)

# Scatter plot with regression lines for each Fertilizer Type and Dose
ggplot(data_subset, aes(x = `Ionic.strength..mol.L.`, y = `EC.harvest.day`, color = Dose)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, aes(group = Dose), formula = y ~ x) +
  facet_wrap(~ Fertilizer_Type, scales = "free") +
  labs(title = "Ionic Strength vs EC at Harvest Day",
       x = "Ionic Strength (mol/L)",
       y = "EC at Harvest Day",
       color = "Dose (N.kg/ha)") +
  theme_minimal()
# Extract residuals and fitted values
residuals <- resid(lm_model)
fitted_values <- fitted(lm_model)

# Residuals vs Fitted values
ggplot(data.frame(Fitted = fitted_values, Residuals = residuals), aes(x = Fitted, y = Residuals)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

# QQ plot for residuals
qqnorm(residuals, main = "QQ Plot of Residuals")
qqline(residuals, col = "red", lwd = 2)

