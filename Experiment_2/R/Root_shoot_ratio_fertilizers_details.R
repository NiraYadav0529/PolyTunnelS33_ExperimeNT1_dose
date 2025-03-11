# Load necessary libraries
library(dplyr)        # Data manipulation
library(tidyr)        # Reshaping data
library(ggplot2)      # Visualization
library(lme4)         # Mixed-effects models
library(car)          # ANOVA and diagnostics
library(ggeffects)    # Predicted values
library(lavaan)       # SEM
library(semPlot)      # Path diagram for SEM
library(ggcorrplot)   # Correlation heatmap

# Load Phalaris dataset
Phalaris_exp2 <- read.csv("Phalaris_FinalData_analysis.csv", check.names = TRUE)
colnames(Phalaris_exp2)


### 1. Data Preprocessing
# Convert dose to numeric and set factors
Phalaris_exp2 <- Phalaris_exp2 %>%
  mutate(
    Dose = as.numeric(as.character(Dose_.N.kg.ha.)),
    Fertilizer_Type = factor(Fertilizer_type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split"))
  )

### 2. Create an Integrated Data Set
# Reshape pH data
pH_data <- Phalaris_exp2 %>%
  select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method, starts_with("pH_")) %>%
  pivot_longer(cols = starts_with("pH_"), names_to = "Time", values_to = "pH_Level") %>%
  drop_na(pH_Level)

# Reshape EC data
ec_data <- Phalaris_exp2 %>%
  select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method, starts_with("EC_")) %>%
  pivot_longer(cols = starts_with("EC_"), names_to = "Time", values_to = "EC_Level") %>%
  drop_na(EC_Level)

# Reshape Chlorophyll data
chlorophyll_data <- Phalaris_exp2 %>%
  select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method, starts_with("CholorophyllContent")) %>%
  pivot_longer(cols = starts_with("CholorophyllContent"), names_to = "Time", values_to = "Chlorophyll_Level") %>%
  drop_na(Chlorophyll_Level)

# Reshape Biomass data
biomass_data <- Phalaris_exp2 %>%
  select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method, starts_with("Dry_Biomass")) %>%
  pivot_longer(cols = starts_with("Dry_Biomass"), names_to = "Time", values_to = "Dry_Biomass") %>%
  drop_na(Dry_Biomass)

# Merge datasets by common identifiers
integrated_data <- pH_data %>%
  inner_join(ec_data, by = c("Combined.pot.id", "Dose", "Fertilizer_Type", "Application_Method", "Time")) %>%
  inner_join(chlorophyll_data, by = c("Combined.pot.id", "Dose", "Fertilizer_Type", "Application_Method", "Time")) %>%
  inner_join(biomass_data, by = c("Combined.pot.id", "Dose", "Fertilizer_Type", "Application_Method", "Time"))

# Log-transform key variables
integrated_data <- integrated_data %>%
  mutate(
    log_EC_Level = log10(EC_Level),
    log_Chlorophyll = log10(Chlorophyll_Level),
    log_Dry_Biomass = log10(Dry_Biomass)
  )

# Identify columns with zero variance
zero_var_cols <- sapply(pca_data, function(x) var(x, na.rm = TRUE) == 0)

# Remove these columns
pca_data_filtered <- pca_data[, !zero_var_cols]

# Now run PCA
pca_result <- prcomp(pca_data_filtered, scale. = TRUE)
summary(pca_result)
biplot(pca_result, cex = 0.7)




colnames(Phalaris_exp2)
# Calculate Root-to-Shoot Ratio
Phalaris_exp2 <- Phalaris_exp2 %>%
  mutate(Root_to_Shoot_Ratio = Dry_root_biomass.g._125daysHD / Dry_shoot_..Biomass.g._125daysHD)
colnames(Phalaris_exp2)
# Check the new column

##converting categorical variables to factors 
Phalaris_exp2 <- Phalaris_exp2 %>%
  mutate(
    Dose = factor(Dose_.N.kg.ha., levels = c("0", "20", "40", "60", "80", "100", "120")),
    Fertilizer_Type = factor(Fertilizer_type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split")),
    Treatment_Type = factor(Treatment_Type, levels = c("Control", "Other"))
  )
head(Phalaris_exp2$Root_to_Shoot_Ratio)

# Boxplot for Root-to-Shoot Ratio by Fertilizer Type
ggplot(Phalaris_exp2, aes(x = Fertilizer_Type, y = Root_to_Shoot_Ratio, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +
  theme_minimal() +
  labs(
    title = "Root-to-Shoot Ratio by Fertilizer Type",
    x = "Fertilizer Type",
    y = "Root-to-Shoot Ratio",
    fill = "Dose",
    color = "Dose"
  ) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot for Root-to-Shoot Ratio by Application Method
ggplot(Phalaris_exp2, aes(x = Application_Method, y = Root_to_Shoot_Ratio, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +
  theme_minimal() +
  labs(
    title = "Root-to-Shoot Ratio by Application Method",
    x = "Application Method",
    y = "Root-to-Shoot Ratio",
    fill = "Dose",
    color = "Dose"
  ) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))

# Fit Linear Mixed-Effects Model for Root-to-Shoot Ratio
m1_root_shoot <- lmer(Root_to_Shoot_Ratio ~ Dose * Fertilizer_Type * Application_Method + (1|Combined.pot.id), data = Phalaris_exp2)

# Model Diagnostics
plot(m1_root_shoot)  # Check residuals
qqPlot(resid(m1_root_shoot))  # Check normality of residuals

# ANOVA for Fixed Effects
Anova(m1_root_shoot, test = "F")

# Summary of the Model
summary(m1_root_shoot)

# Dose-Response Curve for Root-to-Shoot Ratio
ggplot(Phalaris_exp2, aes(x = as.numeric(as.character(Dose)), y = Root_to_Shoot_Ratio, color = Fertilizer_Type)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  facet_wrap(~Application_Method) +
  theme_minimal() +
  labs(
    title = "Dose-Response Curve for Root-to-Shoot Ratio",
    x = "Nitrogen Dose (kg/ha)",
    y = "Root-to-Shoot Ratio",
    color = "Fertilizer Type"
  ) +
  theme(legend.position = "right")

# Factorial ANOVA for Root-to-Shoot Ratio
anova_root_shoot <- aov(Root_to_Shoot_Ratio ~ Dose * Fertilizer_Type * Application_Method, data = Phalaris_exp2)

# Summary of ANOVA
summary(anova_root_shoot)

# Correlation Matrix
correlation_data <- Phalaris_exp2 %>%
  select(Root_to_Shoot_Ratio, pH_8_atHarvestDay, EC._atHarvestDay, Cholorphyll_content.HarvestDay., Dry_Biomass_shoot_4, Dry_Biomass_root_118days)

# Calculate Correlation Matrix
cor_matrix <- cor(correlation_data, use = "complete.obs")

# Visualize Correlation Matrix
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)

# Generate Predicted Values
predicted_root_shoot <- ggpredict(m1_root_shoot, terms = c("Dose", "Fertilizer_Type", "Application_Method"))

# Convert to Data Frame
predicted_root_shoot_df <- as.data.frame(predicted_root_shoot)

# Plot Predicted Root-to-Shoot Ratio
ggplot(predicted_root_shoot_df, aes(x = x, y = predicted, group = group, color = group)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  facet_wrap(~facet, ncol = 1) +
  theme_minimal() +
  labs(
    title = "Predicted Root-to-Shoot Ratio",
    x = "Nitrogen Dose (kg/ha)",
    y = "Predicted Root-to-Shoot Ratio",
    color = "Fertilizer Type"
  ) +
  theme(legend.position = "right")



###Fertilizer Details 

Fertilizers_exp2 <- read.csv("Fertilizers_Details_Exp2.csv", check.names = TRUE)
colnames(Fertilizers_exp2)
head(Fertilizers_exp2)

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
# Reshape data for plotting
fertilizer_data <- Fertilizers_exp2 %>%
  select(Dose, 
         `pH..August.2024.`, `EC..August.2024._.mS.cm.`, 
         `pH..Feb.2025.`, `EC..Feb.2025.`) %>%
  pivot_longer(cols = -Dose, names_to = "Parameter", values_to = "Value") %>%
  separate(Parameter, into = c("Measure", "Date"), sep = "_") %>%
  mutate(Date = ifelse(Date == "August", "August 2024", "February 2025"))

# Check the reshaped data
head(fertilizer_data)

# Reshape data for plotting
fertilizer_data <- Fertilizers_exp2 %>%
  select(Dose, 
         pH_August = "pH..August.2024.", EC_August = "EC..August.2024._.mS.cm.",
         pH_Feb = "pH..Feb.2025.", EC_Feb = "EC..Feb.2025.") %>%
  pivot_longer(cols = -Dose, names_to = "Parameter", values_to = "Value") %>%
  separate(Parameter, into = c("Measure", "Date"), sep = "_") %>%
  mutate(Date = ifelse(Date == "August", "August 2024", "February 2025"))

# Convert Dose into a factor for categorical plotting
fertilizer_data$Dose <- factor(fertilizer_data$Dose, levels = unique(fertilizer_data$Dose))

# Create bar plot for pH
p1 <- ggplot(fertilizer_data %>% filter(Measure == "pH"), aes(x = Dose, y = Value, fill = Date)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "pH Levels Across Fertilizer Doses", x = "Fertilizer Dose", y = "pH Value") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create bar plot for EC
p2 <- ggplot(fertilizer_data %>% filter(Measure == "EC"), aes(x = Dose, y = Value, fill = Date)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "EC Levels Across Fertilizer Doses", x = "Fertilizer Dose", y = "EC Value (mS/cm)") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display both plots side by side
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)


# Reshape data for plotting
fertilizer_data <- Fertilizers_exp2 %>%
  select(Dose, 
         pH_August = "pH..August.2024.", EC_August = "EC..August.2024._.mS.cm.",
         pH_Feb = "pH..Feb.2025.", EC_Feb = "EC..Feb.2025.") %>%
  pivot_longer(cols = -Dose, names_to = "Parameter", values_to = "Value") %>%
  separate(Parameter, into = c("Measure", "Date"), sep = "_") %>%
  mutate(Date = ifelse(Date == "August", "August 2024", "February 2025"))

# Convert Dose into a factor for categorical plotting
fertilizer_data$Dose <- factor(fertilizer_data$Dose, levels = unique(fertilizer_data$Dose))

# Create a vector for doses with missing February data
missing_february_doses <- c("10 UF", "30 UF", "50 UF", "80 UF", "120 UF", "120 MF")

# Create bar plot for pH (black and white color, bold axes and background)
p1 <- ggplot(fertilizer_data %>% filter(Measure == "pH"), aes(x = Dose, y = Value, fill = Date)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.8) +
  labs(title = "pH Levels Across Fertilizer Doses", x = "Fertilizer Dose", y = "pH Value") +
  scale_fill_manual(values = c("black", "white")) +  # Black and white colors for the bars
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    panel.grid = element_blank(),  # Remove background grid for contrast
    panel.background = element_rect(fill = "white", color = "black"),  # Bold background
    axis.line = element_line(size = 1.5, color = "black")  # Bold axis lines
  ) +
  # Add "NA" label next to the doses with missing February data (10 UF, 30 UF, 50 UF, 80 UF, 120 UF, 120 MF)
  geom_text(aes(label = ifelse(Dose %in% missing_february_doses, "NA", "")), 
            position = position_dodge(width = 0.8), vjust = -0.5, size = 4, color = "red")

# Create bar plot for EC (black and white color, bold axes and background)
p2 <- ggplot(fertilizer_data %>% filter(Measure == "EC"), aes(x = Dose, y = Value, fill = Date)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.8) +
  labs(title = "EC Levels Across Fertilizer Doses", x = "Fertilizer Dose", y = "EC Value (mS/cm)") +
  scale_fill_manual(values = c("black", "white")) +  # Black and white colors for the bars
  scale_y_continuous(breaks = seq(0, max(fertilizer_data$Value, na.rm = TRUE), by = 20)) +  # Y-axis increment by 20
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    panel.grid = element_blank(),  # Remove background grid for contrast
    panel.background = element_rect(fill = "white", color = "black"),  # Bold background
    axis.line = element_line(size = 1.5, color = "black")  # Bold axis lines
  ) +
  # Add "NA" label next to the doses with missing February data (10 UF, 30 UF, 50 UF, 80 UF, 120 UF, 120 MF)
  geom_text(aes(label = ifelse(Dose %in% missing_february_doses, "NA", "")), 
            position = position_dodge(width = 0.8), vjust = -0.5, size = 4, color = "red")

# Display pH plot
print(p1)

# Display EC plot
print(p2)



###For lucerne

# Calculate Root-to-Shoot Ratio
Lucerne_exp2 <- Lucerne_exp2 %>%
  mutate(Root_to_Shoot_Ratio = Dry_Biomass_root_118days / Dry_Biomass_shoot_4)
colnames(Lucerne_exp2)
# Check the new column

# Convert categorical variables to factors
Lucerne_exp2 <- Lucerne_exp2 %>%
  mutate(
    Dose = factor(Dose...N.kg.ha., levels = c("0", "10", "20", "30", "40", "50", "60")),
    Fertilizer_Type = factor(Fertilizer.type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split"))
  )
head(Lucerne_exp2$Root_to_Shoot_Ratio)
# Boxplot for Root-to-Shoot Ratio by Fertilizer Type
ggplot(Lucerne_exp2, aes(x = Fertilizer.type, y = Root_to_Shoot_Ratio, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +
  theme_minimal() +
  labs(
    title = "Root-to-Shoot Ratio by Fertilizer Type",
    x = "Fertilizer Type",
    y = "Root-to-Shoot Ratio",
    fill = "Dose",
    color = "Dose"
  ) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot for Root-to-Shoot Ratio by Application Method
ggplot(Lucerne_exp2, aes(x = Application_Method, y = Root_to_Shoot_Ratio, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +
  theme_minimal() +
  labs(
    title = "Root-to-Shoot Ratio by Application Method",
    x = "Application Method",
    y = "Root-to-Shoot Ratio",
    fill = "Dose",
    color = "Dose"
  ) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))

# Fit Linear Mixed-Effects Model for Root-to-Shoot Ratio
m1_root_shoot <- lmer(Root_to_Shoot_Ratio ~ Dose * Fertilizer_Type * Application_Method + (1|Combined.pot.id), data = Lucerne_exp2)

# Model Diagnostics
plot(m1_root_shoot)  # Check residuals
qqPlot(resid(m1_root_shoot))  # Check normality of residuals

# ANOVA for Fixed Effects
Anova(m1_root_shoot, test = "F")

# Summary of the Model
summary(m1_root_shoot)



# Dose-Response Curve for Root-to-Shoot Ratio
ggplot(Lucerne_exp2, aes(x = as.numeric(as.character(Dose)), y = Root_to_Shoot_Ratio, color = Fertilizer_Type)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  facet_wrap(~Application_Method) +
  theme_minimal() +
  labs(
    title = "Dose-Response Curve for Root-to-Shoot Ratio",
    x = "Nitrogen Dose (kg/ha)",
    y = "Root-to-Shoot Ratio",
    color = "Fertilizer Type"
  ) +
  theme(legend.position = "right")

# Factorial ANOVA for Root-to-Shoot Ratio
anova_root_shoot <- aov(Root_to_Shoot_Ratio ~ Dose * Fertilizer_Type * Application_Method, data = Lucerne_exp2)

# Summary of ANOVA
summary(anova_root_shoot)


# Correlation Matrix
correlation_data <- Lucerne_exp2 %>%
  select(Root_to_Shoot_Ratio, pH_8_atHarvestDay, EC._atHarvestDay, Cholorphyll_content.HarvestDay., Dry_Biomass_shoot_4, Dry_Biomass_root_118days)

# Calculate Correlation Matrix
cor_matrix <- cor(correlation_data, use = "complete.obs")

# Visualize Correlation Matrix
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)


# Generate Predicted Values
predicted_root_shoot <- ggpredict(m1_root_shoot, terms = c("Dose", "Fertilizer_Type", "Application_Method"))

# Convert to Data Frame
predicted_root_shoot_df <- as.data.frame(predicted_root_shoot)

# Plot Predicted Root-to-Shoot Ratio
ggplot(predicted_root_shoot_df, aes(x = x, y = predicted, group = group, color = group)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  facet_wrap(~facet, ncol = 1) +
  theme_minimal() +
  labs(
    title = "Predicted Root-to-Shoot Ratio",
    x = "Nitrogen Dose (kg/ha)",
    y = "Predicted Root-to-Shoot Ratio",
    color = "Fertilizer Type"
  ) +
  theme(legend.position = "right")




# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lme4)
library(car)
library(ggpubr)
library(corrplot)
library(ggeffects)

# Convert categorical variables to factors
Phalaris_exp2 <- Phalaris_exp2 %>%
  mutate(
    Dose = factor(Dose_.N.kg.ha., levels = c("0", "20", "40", "60", "80", "100", "120")),
    Fertilizer_Type = factor(Fertilizer_type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split")),
    Treatment_Type = factor(Treatment_Type, levels = c("Control", "Other"))
  )

# Boxplot: Root-to-Shoot Ratio by Fertilizer Type
ggplot(Phalaris_exp2, aes(x = Fertilizer_Type, y = Root.shoot.Ratio, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +
  theme_minimal() +
  labs(
    title = "Root-to-Shoot Ratio by Fertilizer Type",
    x = "Fertilizer Type",
    y = "Root-to-Shoot Ratio",
    fill = "Dose",
    color = "Dose"
  ) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))

# Fit Linear Mixed-Effects Model for Root-to-Shoot Ratio
m1_root_shoot_phalaris <- lmer(Root.shoot.Ratio ~ Dose * Fertilizer_Type * Application_Method + (1|Combined.pot.id), data = Phalaris_exp2)

# Model Diagnostics
plot(m1_root_shoot_phalaris)  # Check residuals
qqPlot(resid(m1_root_shoot_phalaris))  # Check normality of residuals

# ANOVA for Fixed Effects
Anova(m1_root_shoot_phalaris, test = "F")

# Summary of the Model
summary(m1_root_shoot_phalaris)

# Dose-Response Curve for Root-to-Shoot Ratio
ggplot(Phalaris_exp2, aes(x = as.numeric(as.character(Dose)), y = Root.shoot.Ratio, color = Fertilizer_Type)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  facet_wrap(~Application_Method) +
  theme_minimal() +
  labs(
    title = "Dose-Response Curve for Root-to-Shoot Ratio",
    x = "Nitrogen Dose (kg/ha)",
    y = "Root-to-Shoot Ratio",
    color = "Fertilizer Type"
  ) +
  theme(legend.position = "right")

# Correlation Matrix
correlation_data_phalaris <- Phalaris_exp2 %>%
  select(Root.shoot.Ratio, pH_8, EC_8, CholorophyllContent_125daysHD, Dry_shoot_..Biomass.g._125daysHD, Dry_root_biomass.g._125daysHD)

# Calculate Correlation Matrix
cor_matrix_phalaris <- cor(correlation_data_phalaris, use = "complete.obs")

# Visualize Correlation Matrix
corrplot(cor_matrix_phalaris, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)


library(ggplot2)
library(dplyr)
library(tidyr)

# Reshape data for plotting
fertilizer_data <- Fertilizers_exp2 %>%
  pivot_longer(cols = -Dose, names_to = "Parameter", values_to = "Value") %>%
  separate(Parameter, into = c("Measure", "Date", "Unit"), sep = "_", fill = "right") %>%
  mutate(Date = paste(Date, "2024"),  
         Date = gsub("Feb 2024", "February 2025", Date),
         Unit = ifelse(is.na(Unit), "", Unit))  

# Extract numeric part of Dose for proper sorting
fertilizer_data <- fertilizer_data %>%
  mutate(Dose_numeric = as.numeric(gsub("[^0-9]", "", Dose))) %>%  # Extract numbers from Dose
  arrange(Dose_numeric) %>%  # Arrange by numeric Dose values
  mutate(Dose = factor(Dose, levels = unique(Dose)))  # Set as ordered factor

# Histogram-style bar plot for pH
p1 <- ggplot(subset(fertilizer_data, Measure == "pH"), aes(x = Dose, y = Value, fill = Date)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +  
  geom_text(aes(label = ifelse(is.na(Value), "NA", round(Value, 2))), vjust = -0.5, color = "red") +  
  labs(title = "pH Levels Across Fertilizer Doses", x = "Fertilizer Dose", y = "pH Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("black", "white"))

# Histogram-style bar plot for EC
p2 <- ggplot(subset(fertilizer_data, Measure == "EC"), aes(x = Dose, y = Value, fill = Date)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +  
  geom_text(aes(label = ifelse(is.na(Value), "NA", round(Value, 3))), vjust = -0.5, color = "red") +  
  labs(title = "EC Levels Across Fertilizer Doses", x = "Fertilizer Dose", y = "EC Value (mS/cm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("black", "white"))

# Print plots
print(p1)
print(p2)

