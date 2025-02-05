# Set working directory and load data
setwd("C:/Users/90958427/OneDrive - Western Sydney University/PolyTunnelS33_ExperimeNT1_dose/Experiment_2/Modified_Data_File")
list.files()
# Load necessary libraries
library(dplyr)
library(lme4)
library(car)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(tidyverse)
library(ggplot2)
library(multcomp)
library(multcompView)



# Convert variables to factors
Lucerne_exp2 <- Lucerne_exp2 %>%
  mutate(
    Dose = factor(Dose...N.kg.ha., levels = c("0", "10", "20", "30", "40", "50", "60")),
    Fertilizer_Type = factor(Fertilizer.type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split"))
  )
# Print column names to check for exact matches
print(names(Lucerne_exp2))

# Select columns using base R (no select function)
pH_data <- Lucerne_exp2[, c(
  "Combined.pot.id", "Dose", "Fertilizer_Type", "Application_Method",
  "pH_BF", "pH_2.AF_1st_Split.", "pH_3_AF_2nd_Split", "pH_4_AF_2nd_Split",
  "pH_5_AF_3nd_Split", "pH_6_AF_3nd_Split", "pH_7_AF_3nd_Split", "pH_8_atHarvestDay"
)]

# Dynamically select columns matching "pH_" pattern
pH_data <- Lucerne_exp2[, c(
  "Combined.pot.id", "Dose", "Fertilizer_Type", "Application_Method",
  grep("^pH_", names(Lucerne_exp2), value = TRUE)  # Select all columns starting with "pH_"
)]
# Reload dplyr to ensure no conflicts
detach("package:dplyr", unload = TRUE)
library(dplyr)

# Reshape data to long format
pH_long <- pH_data %>%
  pivot_longer(
    cols = starts_with("pH_"),
    names_to = "Time_Point",
    values_to = "pH_Level"
  ) %>%
  mutate(
    Time_Point = factor(Time_Point),
    Combined.pot.id = as.character(Combined.pot.id)
  ) %>%
  drop_na(pH_Level)  # Remove missing pH values

# Split data into "Split" and "One-time" application methods
pH_split <- pH_long %>% filter(Application_Method == "Split")
pH_one_time <- pH_long %>% filter(Application_Method == "One-time")

# --- Visualization: pH Trends for Split Method ---
ggplot(pH_split, aes(x = Time_Point, y = pH_Level, color = Dose)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "pH Levels Across Time Points (Split Application Method)",
    y = "pH Level",
    x = "Time Point",
    color = "Dose (N kg/ha)"
  ) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# --- Visualization: pH Trends for One-time Method with Fertilizer_Type ---
ggplot(pH_one_time, aes(x = Time_Point, y = pH_Level, color = Dose)) +
  geom_boxplot() +
  facet_wrap(~ Fertilizer_Type) +  # Different fertilizer types in One-time method
  theme_minimal() +
  labs(
    title = "pH Levels Across Time Points (One-time Application Method)",
    y = "pH Level",
    x = "Time Point",
    color = "Dose (N kg/ha)"
  ) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# --- Fit a Linear Mixed Model ---
m1_pH <- lmer(pH_Level ~ Time_Point * Dose * Fertilizer_Type + (1 | Combined.pot.id), data = pH_long)

# Model summary
summary(m1_pH)

# ANOVA to test significance of fixed effects
Anova(m1_pH)


# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(car)
library(ggeffects)

# Convert variables to factors
Lucerne_exp2 <- Lucerne_exp2 %>%
  mutate(
    Dose = factor(Dose...N.kg.ha., levels = c("0", "10", "20", "30", "40", "50", "60")),
    Fertilizer_Type = factor(Fertilizer.type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split"))
  )

# Select pH-related columns dynamically
pH_columns <- grep("^pH_", names(Lucerne_exp2), value = TRUE)

# Select relevant columns including identifiers
pH_data <- Lucerne_exp2 %>%
  select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method, all_of(pH_columns))

# Reshape data to long format
pH_long <- pH_data %>%
  pivot_longer(
    cols = starts_with("pH_"),
    names_to = "Date",
    values_to = "pH_Level"
  ) %>%
  mutate(
    Date = factor(Date),
    Combined.pot.id = as.character(Combined.pot.id)
  ) %>%
  drop_na(pH_Level)  # Remove missing values

# Separate data by Application Method
pH_one_time <- pH_long %>% filter(Application_Method == "One-time")
pH_split <- pH_long %>% filter(Application_Method == "Split")

# --- Fit a Linear Mixed Effects Model (Repeated Measures) ---
m1_pH <- lmer(pH_Level ~ Dose * Date + (1|Combined.pot.id), data = pH_long)

# Model diagnostics
plot(m1_pH)
qqPlot(resid(m1_pH))  # QQ plot of residuals

# ANOVA to test significance of fixed effects
Anova(m1_pH, test="F")

# Summary of the model
summary(m1_pH)

# --- Prediction Plot Using ggeffects ---
predict_plot <- predict_response(m1_pH, c("Date", "Dose")) %>% plot()
print(predict_plot)

# Load necessary libraries
library(ggplot2)
library(emmeans)
library(dplyr)

# Manually order Date (Time Points) so pH_BF is the first level
pH_levels_order <- c("pH_BF", "pH_2.AF_1st_Split.", "pH_3_AF_2nd_Split", 
                     "pH_4_AF_2nd_Split", "pH_5_AF_3nd_Split", "pH_6_AF_3nd_Split", 
                     "pH_7_AF_3nd_Split", "pH_8_atHarvestDay")

# Ensure Date is a factor with the specified order
pH_long$Date <- factor(pH_long$Date, levels = pH_levels_order, ordered = TRUE)

# --- 1. Plot Estimated Marginal Means for pH by Dose ---
emmip_plot_dose <- emmip(m1_pH, Dose ~ Date, CIs = TRUE) +  
  geom_point(aes(shape = Dose), size = 3) +  # Different shapes for Dose
  geom_line(aes(linetype = Dose), size = 1) +  # Different line styles for visibility
  theme_minimal() +
  labs(
    title = "Estimated Marginal Means for pH Levels by Dose",
    y = "Estimated pH",
    x = "Date (Time Point)"
  ) +
  scale_x_discrete(limits = pH_levels_order) +  # Ensure custom order on x-axis
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
    Date=fct_relevel(Date, 'pH_BF')
  )
print(emmip_plot_dose)


# Load necessary libraries
library(lme4)
library(car)
library(ggplot2)
library(emmeans)
library(dplyr)
library(ggeffects)
library(tidyr)

# Convert variables to factors
Lucerne_exp2 <- Lucerne_exp2 %>%
  mutate(
    Dose = factor(Dose...N.kg.ha., levels = c("0", "10", "20", "30", "40", "50", "60")),
    Fertilizer_Type = factor(Fertilizer.type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split"))
  )

# Select pH-related columns dynamically
pH_columns <- grep("^pH_", names(Lucerne_exp2), value = TRUE)

# Select relevant columns including identifiers
pH_data <- Lucerne_exp2 %>%
  select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method, all_of(pH_columns))

# Reshape data to long format
pH_long <- pH_data %>%
  pivot_longer(
    cols = starts_with("pH_"),
    names_to = "Date",
    values_to = "pH_Level"
  ) %>%
  mutate(
    Date = factor(Date),
    Combined.pot.id = as.character(Combined.pot.id)
  ) %>%
  drop_na(pH_Level)  # Remove missing values

# Ensure pH_BF is the first level in Date
pH_levels_order <- c("pH_BF", "pH_2.AF_1st_Split.", "pH_3_AF_2nd_Split", 
                     "pH_4_AF_2nd_Split", "pH_5_AF_3nd_Split", "pH_6_AF_3nd_Split", 
                     "pH_7_AF_3nd_Split", "pH_8_atHarvestDay")

pH_long$Date <- factor(pH_long$Date, levels = pH_levels_order, ordered = TRUE)

# Separate data by Application Method
pH_one_time <- pH_long %>% filter(Application_Method == "One-time")
pH_split <- pH_long %>% filter(Application_Method == "Split")

# Apply log10 transformation to pH_Level
pH_long <- pH_long %>%
  mutate(log_pH_Level = log10(pH_Level))

# --- Fit a Linear Mixed Effects Model (Repeated Measures) with log10 transformation ---
m1_pH <- lmer(log_pH_Level ~ Dose * Date + (1|Combined.pot.id), data = pH_long)

# Model diagnostics
plot(m1_pH)
qqPlot(resid(m1_pH))  # QQ plot of residuals

# ANOVA to test significance of fixed effects
Anova(m1_pH, test="F")

# Summary of the model
summary(m1_pH)

# --- Prediction Plot Using ggeffects ---
predict_plot <- predict_response(m1_pH, c("Date", "Dose")) %>% plot()
print(predict_plot)
# Boxplot for log10(pH) across time points (Date) and Dose
pH_boxplot <- ggplot(pH_long, aes(x = Date, y = log_pH_Level, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +  # Boxplot with transparency
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +  # Add jittered points
  theme_minimal() +
  labs(
    title = "Distribution of log10(pH) Levels Over Time",
    x = "Date (Time Point)",
    y = "log10(pH) Level",
    fill = "Dose",
    color = "Dose"
  ) +
  scale_x_discrete(limits = pH_levels_order) +  # Ensure correct time order
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )

print(pH_boxplot)



# --- 1. Plot Estimated Marginal Means for pH by Dose ---
emmip_plot_dose <- emmip(m1_pH, Dose ~ Date, CIs = TRUE) +  
  geom_point(aes(shape = Dose), size = 3) +  # Different shapes for Dose
  geom_line(aes(linetype = Dose), linewidth = 1) +  # Use linewidth instead of size
  theme_minimal() +
  labs(
    title = "Estimated Marginal Means for log10(pH) Levels by Dose",
    y = "Estimated log10(pH)",
    x = "Date (Time Point)"
  ) +
  scale_x_discrete(limits = pH_levels_order) +  # Ensure custom order on x-axis
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(emmip_plot_dose)



###For PH LUCERNE 
# Load necessary libraries
library(lme4)
library(car)
library(ggplot2)
library(emmeans)
library(dplyr)
library(ggeffects)
library(tidyr)
# Convert variables to factors
Lucerne_exp2 <- Lucerne_exp2 %>%
  mutate(
    Dose = factor(Dose...N.kg.ha., levels = c("0", "10", "20", "30", "40", "50", "60")),
    Fertilizer_Type = factor(Fertilizer.type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split"))
  )

# Select pH-related columns dynamically
pH_columns <- grep("^pH_", names(Lucerne_exp2), value = TRUE)

# Select relevant columns including identifiers
pH_data <- Lucerne_exp2 %>%
  select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method, all_of(pH_columns))

# Reshape data to long format
pH_long <- pH_data %>%
  pivot_longer(
    cols = starts_with("pH_"),
    names_to = "Date",
    values_to = "pH_Level"
  ) %>%
  mutate(
    Date = factor(Date),
    Combined.pot.id = as.character(Combined.pot.id)
  ) %>%
  drop_na(pH_Level)  # Remove missing values

# Ensure pH_BF is the first level in Date
pH_levels_order <- c("pH_BF", "pH_2.AF_1st_Split.", "pH_3_AF_2nd_Split", 
                     "pH_4_AF_2nd_Split", "pH_5_AF_3nd_Split", "pH_6_AF_3nd_Split", 
                     "pH_7_AF_3nd_Split", "pH_8_atHarvestDay")

pH_long$Date <- factor(pH_long$Date, levels = pH_levels_order, ordered = TRUE)

# Separate data by Application Method
pH_one_time <- pH_long %>% filter(Application_Method == "One-time")
pH_split <- pH_long %>% filter(Application_Method == "Split")

# Apply log10 transformation to pH_Level
pH_long <- pH_long %>%
  mutate(log_pH_Level = log10(pH_Level))
# Fit a Linear Mixed Effects Model (Repeated Measures) with log10 transformation
m1_pH <- lmer(log_pH_Level ~ Dose * Date + (1|Combined.pot.id), data = pH_long)

# Model diagnostics
plot(m1_pH)
qqPlot(resid(m1_pH))  # QQ plot of residuals

# ANOVA to test significance of fixed effects
Anova(m1_pH, test="F")

# Summary of the model
summary(m1_pH)

# Apply log10 transformation to pH_Level before filtering datasets
pH_long <- pH_long %>%
  mutate(
    pH_Level = as.numeric(pH_Level),  # Ensure pH_Level is numeric
    log_pH_Level = log10(pH_Level)  # Apply log transformation
  ) %>%
  drop_na(log_pH_Level)  # Remove missing transformed values

# Now, create subsets after transformation
pH_one_time <- pH_long %>% filter(Application_Method == "One-time")
pH_split <- pH_long %>% filter(Application_Method == "Split")

# --- Boxplot for One-time Application Method ---
ggplot(pH_one_time, aes(x = Date, y = log_pH_Level, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +  # Boxplot with transparency
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +  # Add jittered points
  theme_minimal() +
  labs(
    title = "Distribution of log10(pH) Levels Over Time (One-time Application)",
    x = "Date (Time Point)",
    y = "log10(pH) Level",
    fill = "Dose",
    color = "Dose"
  ) +
  scale_x_discrete(limits = pH_levels_order) +  # Ensure correct time order
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  ) +
  geom_hline(yintercept = mean(pH_one_time$log_pH_Level[pH_one_time$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black")  # Add reference line for Dose 0

# --- Boxplot for Split Application Method ---
ggplot(pH_split, aes(x = Date, y = log_pH_Level, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +  # Boxplot with transparency
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +  # Add jittered points
  theme_minimal() +
  labs(
    title = "Distribution of log10(pH) Levels Over Time (Split Application)",
    x = "Date (Time Point)",
    y = "log10(pH) Level",
    fill = "Dose",
    color = "Dose"
  ) +
  scale_x_discrete(limits = pH_levels_order) +  # Ensure correct time order
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  ) +
  geom_hline(yintercept = mean(pH_split$log_pH_Level[pH_split$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black")  # Add reference line for Dose 0

# Calculate mean log_pH_Level for Dose 0 in One-time application
dose_0_mean <- mean(pH_one_time$log_pH_Level[pH_one_time$Dose == "0"], na.rm = TRUE)

# Plot with reference line
ggplot(pH_one_time, aes(x = Date, y = log_pH_Level, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +  # Boxplot with transparency
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +  # Add jittered points
  geom_hline(yintercept = dose_0_mean, linetype = "dashed", color = "black", linewidth = 1) +  # Reference line for Dose 0
  theme_minimal() +
  labs(
    title = "Distribution of log10(pH) Levels Over Time (One-time Application)",
    x = "Date (Time Point)",
    y = "log10(pH) Level",
    fill = "Dose",
    color = "Dose"
  ) +
  scale_x_discrete(limits = pH_levels_order) +  # Ensure correct time order
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )
# Ensure Dose 0 is included in One-time dataset
pH_one_time <- pH_long %>% 
  filter(Application_Method == "One-time" & Dose %in% c("0", "30", "60"))

# Calculate mean log_pH_Level for Dose 0 in One-time application
dose_0_mean <- mean(pH_one_time$log_pH_Level[pH_one_time$Dose == "0"], na.rm = TRUE)

# Plot with Dose 0 included
ggplot(pH_one_time, aes(x = Date, y = log_pH_Level, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +  # Boxplot with transparency
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +  # Add jittered points
  geom_hline(yintercept = dose_0_mean, linetype = "dashed", color = "black", linewidth = 1) +  # Reference line for Dose 0
  theme_minimal() +
  labs(
    title = "Distribution of log10(pH) Levels Over Time (One-time Application with Dose 0)",
    x = "Date (Time Point)",
    y = "log10(pH) Level",
    fill = "Dose",
    color = "Dose"
  ) +
  scale_x_discrete(limits = pH_levels_order) +  # Ensure correct time order
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )
# Ensure pH_Long has log transformation before filtering
pH_long <- pH_long %>%
  mutate(
    pH_Level = as.numeric(pH_Level),  # Ensure pH_Level is numeric
    log_pH_Level = log10(pH_Level)  # Apply log transformation
  ) %>%
  drop_na(log_pH_Level)  # Remove missing transformed values

# Create dataset including Dose 0 (control) in both application methods
pH_selected <- pH_long %>%
  filter(Dose %in% c("0", "10", "20", "30", "40", "50", "60"))  # Ensure Dose 0 is included

# Ensure correct factor levels for Application Method
pH_selected <- pH_selected %>%
  mutate(Application_Method = factor(Application_Method, levels = c("One-time", "Split")))

# Check structure of dataset to confirm it exists
str(pH_selected)
head(pH_selected)
# Fit a Linear Mixed Effects Model (Repeated Measures) with log10 transformation
m1_pH_control <- lmer(log_pH_Level ~ Dose * Date * Application_Method + (1|Combined.pot.id), data = pH_selected)

# Model diagnostics
plot(m1_pH_control)
qqPlot(resid(m1_pH_control))  # QQ plot of residuals

# ANOVA to test significance of fixed effects
Anova(m1_pH_control, test="F")

# Summary of the model
summary(m1_pH_control)
ls()  # Lists all objects in the current environment

# Fit a Linear Mixed Effects Model (Repeated Measures) with log10 transformation
m1_pH_control <- lmer(log_pH_Level ~ Dose * Date * Application_Method + (1|Combined.pot.id), data = pH_selected)

# Model diagnostics
plot(m1_pH_control)
qqPlot(resid(m1_pH_control))  # QQ plot of residuals

# ANOVA to test significance of fixed effects
Anova(m1_pH_control, test="F")

# Summary of the model
summary(m1_pH_control)

# --- Estimated Marginal Means Analysis ---
emmip_plot_dose_control <- emmip(m1_pH_control, Dose ~ Date | Application_Method, CIs = TRUE) +  
  geom_point(aes(shape = Dose), size = 3) +  # Different shapes for Dose
  geom_line(aes(linetype = Dose), linewidth = 1) +  # Use linewidth instead of size
  theme_minimal() +
  labs(
    title = "Estimated Marginal Means for log10(pH) Levels by Dose (One-time vs Split)",
    y = "Estimated log10(pH)",
    x = "Date (Time Point)"
  ) +
  scale_x_discrete(limits = pH_levels_order) +  # Ensure custom order on x-axis
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(emmip_plot_dose_control)





#Now cholrophyll data set:

colnames(Lucerne_exp2)


# Load necessary libraries
library(dplyr)
library(lme4)
library(car)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(tidyverse)
library(ggplot2)
library(multcomp)
library(multcompView)
#For Lucerne Data

# Load the dataset
Lucerne_exp2 <- read.csv("Lucerne_FinalData_analysis.csv", check.names = TRUE)

# Check the structure of the dataset
str(Lucerne_exp2)

# View the first few rows
head(Lucerne_exp2)
colnames(Lucerne_exp2)

# Load necessary libraries
library(tidyverse)
library(lme4)
library(emmeans)

# Check column names to ensure there are no typos
print(names(Lucerne_exp2))

# Convert variables to factors (check column names to match exactly)
Lucerne_exp2 <- Lucerne_exp2 %>%
  mutate(
    Dose = factor(Dose...N.kg.ha., levels = c("0", "10", "20", "30", "40", "50", "60")),
    Fertilizer_Type = factor(Fertilizer.type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split"))
  )

# Reshape data and include Combined.pot.id
data_all <- Lucerne_exp2 %>%
  pivot_longer(
    cols = starts_with("Dry_Biomass"),  # Use starts_with to capture all relevant columns
    names_to = "Time_Point",
    values_to = "Dry_Biomass"
  ) %>%
  mutate(
    Time_Point = factor(Time_Point),
    Combined.pot.id = as.character(Combined.pot.id)
  ) %>%
  drop_na(Dry_Biomass)  # Remove rows with missing Dry_Biomass values

# Check the structure of the reshaped data
str(data_all)


# Visualization with adjustments: legend as Dose, separate columns for Application_Method
ggplot(data_all, aes(x = Time_Point, y = Dry_Biomass, color = Dose)) +
  geom_boxplot() +
  facet_grid(. ~ Application_Method) +  # Separate columns for Application_Method
  theme_minimal() +
  labs(
    title = "Dry Biomass Distribution by Time Point and Application Method",
    y = "Dry Biomass",
    x = "Time Point",
    color = "Dose (N kg/ha)"  # Legend for Dose
  ) +
  theme(
    legend.position = "right",  # Ensure legend is positioned on the right
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )




# Fit a linear mixed-effects model
m1_all <- lmer(Dry_Biomass ~ Time_Point * Dose * Fertilizer_Type * Application_Method +  (1 | Combined.pot.id), data = data_all


plot(m1_all)
Anova(m1)
install.packages("insight")  # Replace "insight" with the actual package name
library(insight)

# effects are significant to visually inspect predictions
library(ggeffects)
predict_response(m1, c('Date', 'Dose')) %>% plot()
# check in more detail using emmeans


# Post-hoc analysis (if required)
emmeans(m1_all, pairwise ~ Time_Point * Dose * Fertilizer_Type * Application_Method)

# Perform post-hoc analysis using emmeans
post_hoc <- emmeans(m1_all, 
                    pairwise ~ Time_Point * Dose * Fertilizer_Type * Application_Method,
                    adjust = "tukey")  # Tukey adjustment for multiple comparisons

# Display the summary of pairwise comparisons
summary(post_hoc)

# Extract the emmeans table
emmeans_table <- as.data.frame(post_hoc$emmeans)
print(emmeans_table)

# Extract the pairwise comparisons table
pairwise_comparisons <- as.data.frame(post_hoc$contrasts)
print(pairwise_comparisons)

# Save the results for review if needed
write.csv(emmeans_table, "emmeans_table.csv", row.names = FALSE)
write.csv(pairwise_comparisons, "pairwise_comparisons.csv", row.names = FALSE)

# Plotting emmeans results
# Visualize estimated marginal means for Time_Point * Dose * Fertilizer_Type * Application_Method
emmeans_plot <- emmip(m1_all, Time_Point ~ Dose | Fertilizer_Type * Application_Method,
                      CIs = TRUE) +  # Add confidence intervals
  theme_minimal() +
  labs(
    title = "Estimated Marginal Means by Time Point, Dose, Fertilizer Type, and Application Method",
    y = "Estimated Dry Biomass",
    x = "Time Point"
  )
print(emmeans_plot)


#For PhAlaris Data
Phalaris_exp2 <- read.csv("Phalaris_FinalData_analysis.csv")
colnames(Phalaris_exp2)

# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(emmeans)
library(multcompView)

# Read the Phalaris dataset
Phalaris_exp2 <- read.csv("Phalaris_FinalData_analysis.csv")

# Convert variables to factors
Phalaris_exp2 <- Phalaris_exp2 %>%
  mutate(
    Dose = factor(Dose_.N.kg.ha., levels = c("0", "10", "20", "30", "40", "50", "60")),
    Fertilizer_Type = factor(Fertilizer_type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split"))
  )

# Reshape data to long format for dry biomass columns
data_all <- Phalaris_exp2 %>%
  pivot_longer(
    cols = c(Dry_Biomass..gm._35days, 
             Dry_Biomass.gm._66days, 
             Dry_Biomass..gm._95days, 
             Dry_shoot_..Biomass.g._125daysHD, 
             Dry_root_biomass.g._125daysHD),
    names_to = "Time_Point",
    values_to = "Dry_Biomass"
  ) %>%
  mutate(
    Time_Point = factor(Time_Point),
    Combined.pot.id = as.character(Combined.pot.id)
  ) %>%
  drop_na(Dry_Biomass)  # Remove rows with missing Dry_Biomass values

# Check the structure of the reshaped data
str(data_all)

# Visualization: Boxplot with Dose as Legend
ggplot(data_all, aes(x = Time_Point, y = Dry_Biomass, color = Dose)) +
  geom_boxplot() +
  facet_grid(. ~ Application_Method) +  # Separate columns for Application_Method
  theme_minimal() +
  labs(
    title = "Dry Biomass Distribution by Time Point and Application Method (Phalaris)",
    y = "Dry Biomass",
    x = "Time Point",
    color = "Dose (N kg/ha)"
  ) +
  theme(
    legend.position = "right",  # Ensure legend is positioned on the right
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )

# Fit a linear mixed-effects model
m1_all <- lmer(Dry_Biomass ~ Time_Point * Dose * Fertilizer_Type * Application_Method + 
                 (1 | Combined.pot.id), data = data_all)


m1_all <- lmer(Dry_Biomass ~ Time_Point + Dose + Fertilizer_Type + Application_Method +
                 Time_Point:Dose + Time_Point:Fertilizer_Type + Time_Point:Application_Method +
                 (1 | Combined.pot.id), data = data_all)
table(data_all$Time_Point, data_all$Dose, data_all$Fertilizer_Type, data_all$Application_Method)


m1_all <- lmer(Dry_Biomass ~ Time_Point * Dose + Time_Point * Fertilizer_Type + 
                 Application_Method + (1 | Combined.pot.id), data = data_all)

data_all <- data_all %>%
  mutate(Dose_scaled = scale(as.numeric(as.character(Dose))))







# Post-hoc analysis
post_hoc <- emmeans(m1_all, 
                    pairwise ~ Time_Point * Dose * Fertilizer_Type * Application_Method,
                    adjust = "tukey")  # Tukey adjustment for multiple comparisons

# Display the summary of pairwise comparisons
summary(post_hoc)

# Extract emmeans table and pairwise comparisons
emmeans_table <- as.data.frame(post_hoc$emmeans)
pairwise_comparisons <- as.data.frame(post_hoc$contrasts)

# Save the results for review
write.csv(emmeans_table, "Phalaris_emmeans_table.csv", row.names = FALSE)
write.csv(pairwise_comparisons, "Phalaris_pairwise_comparisons.csv", row.names = FALSE)

# Visualization of Estimated Marginal Means (emmeans)
emmeans_plot <- emmip(m1_all, Time_Point ~ Dose | Fertilizer_Type * Application_Method,
                      CIs = TRUE) +  # Add confidence intervals
  theme_minimal() +
  labs(
    title = "Estimated Marginal Means by Time Point, Dose, Fertilizer Type, and Application Method (Phalaris)",
    y = "Estimated Dry Biomass",
    x = "Time Point"
  )
print(emmeans_plot)

# Add compact letter display to the plot
contrast_summary <- summary(pairs(emmeans(m1_all, ~ Time_Point * Dose * Fertilizer_Type * Application_Method)))
contrast_letters <- multcompLetters(contrast_summary$p.value, threshold = 0.05)$Letters
emmeans_table$group <- contrast_letters[rownames(emmeans_table)]

data_for_plot <- data_all %>%
  left_join(emmeans_table, by = c("Time_Point", "Dose", "Fertilizer_Type", "Application_Method"))

ggplot(data_for_plot, aes(x = Time_Point, y = Dry_Biomass, color = Dose)) +
  geom_boxplot() +
  geom_text(aes(label = group), position = position_dodge(width = 0.75), vjust = -0.5, size = 3) +
  facet_grid(. ~ Application_Method) +
  theme_minimal() +
  labs(
    title = "Dry Biomass with Pairwise Comparison Letters (Phalaris)",
    y = "Dry Biomass",
    x = "Time Point",
    color = "Dose (N kg/ha)"
  ) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )





# Convert variables to factors
Lucerne_exp2 <- Lucerne_exp2 %>%
  mutate(
    Dose = factor(Dose...N.kg.ha., levels = c("0", "10", "20", "30", "40", "50", "60")),
    Fertilizer_Type = factor(Fertilizer.type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split"))
  )
# Select EC columns dynamically
EC_columns <- grep("^EC_", names(Lucerne_exp2), value = TRUE)

# Select relevant columns and reshape data
EC_data <- Lucerne_exp2 %>%
  select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method, all_of(EC_columns)) %>%
  pivot_longer(
    cols = starts_with("EC_"),
    names_to = "Time_Point",
    values_to = "EC_Value"
  ) %>%
  mutate(Time_Point = factor(Time_Point)) %>%
  drop_na(EC_Value)

# Separate data by Application Method
EC_split <- EC_data %>% filter(Application_Method == "Split")
EC_one_time <- EC_data %>% filter(Application_Method == "One-time")

# --- Fit a Linear Mixed Effects Model ---
m1_EC <- lmer(EC_Value ~ Dose * Time_Point + (1 | Combined.pot.id), data = EC_data)

# Model diagnostics
plot(m1_EC)
qqPlot(resid(m1_EC))
Anova(m1_EC, test = "F")
summary(m1_EC)

# --- Prediction Plot for EC Using ggeffects ---
predict_plot_EC <- predict_response(m1_EC, c("Time_Point", "Dose")) %>% plot()
print(predict_plot_EC)

# --- Visualization: Boxplots for Each Application Method ---
ggplot(EC_split, aes(x = Time_Point, y = EC_Value, color = Dose)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "EC Levels Across Time Points (Split Application Method)",
    y = "EC Value",
    x = "Time Point",
    color = "Dose (N kg/ha)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(EC_one_time, aes(x = Time_Point, y = EC_Value, color = Dose)) +
  geom_boxplot() +
  facet_wrap(~ Fertilizer_Type) +
  theme_minimal() +
  labs(
    title = "EC Levels Across Time Points (One-time Application Method)",
    y = "EC Value",
    x = "Time Point",
    color = "Dose (N kg/ha)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Select EC columns dynamically
EC_columns <- grep("^EC_", names(Lucerne_exp2), value = TRUE)

# Select relevant columns and reshape data
EC_data <- Lucerne_exp2 %>%
  select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method, all_of(EC_columns)) %>%
  pivot_longer(
    cols = starts_with("EC_"),
    names_to = "Time_Point",
    values_to = "EC_Value"
  ) %>%
  mutate(Time_Point = factor(Time_Point)) %>%
  drop_na(EC_Value)

# Separate data by Application Method
EC_split <- EC_data %>% filter(Application_Method == "Split")
EC_one_time <- EC_data %>% filter(Application_Method == "One-time")

# --- Fit a Linear Mixed Effects Model ---
m1_EC <- lmer(EC_Value ~ Dose * Time_Point + (1 | Combined.pot.id), data = EC_data)

# Model diagnostics
plot(m1_EC)
qqPlot(resid(m1_EC))
Anova(m1_EC, test = "F")
summary(m1_EC)

# --- Prediction Plot for EC Using ggeffects ---
predict_plot_EC <- predict_response(m1_EC, c("Time_Point", "Dose")) %>% plot()
print(predict_plot_EC)

# --- Visualization: Boxplots for Each Application Method ---
ggplot(EC_split, aes(x = Time_Point, y = EC_Value, color = Dose)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "EC Levels Across Time Points (Split Application Method)",
    y = "EC Value",
    x = "Time Point",
    color = "Dose (N kg/ha)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(EC_one_time, aes(x = Time_Point, y = EC_Value, color = Dose)) +
  geom_boxplot() +
  facet_wrap(~ Fertilizer_Type) +
  theme_minimal() +
  labs(
    title = "EC Levels Across Time Points (One-time Application Method)",
    y = "EC Value",
    x = "Time Point",
    color = "Dose (N kg/ha)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

