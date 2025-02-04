library(tidyverse)
library(ggplot2)

# Read the CSV file
XRF_data <- read.csv("xrf_Nutrients_analysis_exp1_final.csv")

# View the first few rows to verify data was read correctly
head(XRF_data)
colnames(XRF_data)


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



# Load necessary libraries
library(dplyr)

# Subset the relevant columns from the dataset
data_subset <- XRF_data %>%
  select(Leaf.C.N..ratio., Root.C.N..ratio., P_.ppm., P_.ppm..1, P.g.kg..dw.soil, NH4.g.kg..dw.soil, NO3.g.kg..dw.soil)

# Check the data types of the selected columns
str(data_subset)
# Check the unique values in the column
unique(XRF_data$Leaf.C.N..ratio.)
# Convert columns to numeric while handling non-numeric values gracefully
data_subset$Leaf_CN_Ratio <- suppressWarnings(as.numeric(as.character(XRF_data$Leaf.C.N..ratio.)))
data_subset$Root_CN_Ratio <- suppressWarnings(as.numeric(as.character(XRF_data$Root.C.N..ratio.)))
data_subset$Leaf_P_ppm <- suppressWarnings(as.numeric(as.character(XRF_data$P_.ppm.)))
data_subset$Root_P_ppm <- suppressWarnings(as.numeric(as.character(XRF_data$P_.ppm..1)))
data_subset$Soil_P_gkg <- suppressWarnings(as.numeric(as.character(XRF_data$P.g.kg..dw.soil)))
data_subset$Soil_NH4_gkg <- suppressWarnings(as.numeric(as.character(XRF_data$NH4.g.kg..dw.soil)))
data_subset$Soil_NO3_gkg <- suppressWarnings(as.numeric(as.character(XRF_data$NO3.g.kg..dw.soil)))

# Check if the conversion resulted in any NA values
summary(data_subset)

# Remove rows with NA values if necessary
data_subset <- na.omit(data_subset)

# Perform correlation tests as before
cor_leaf_cn_soil <- cor.test(data_subset$Leaf_CN_Ratio, data_subset$Soil_P_gkg)




# Convert the relevant columns to numeric if they are not already
data_subset$Leaf_CN_Ratio <- as.numeric(as.character(data_subset$Leaf_CN_Ratio))
data_subset$Root_CN_Ratio <- as.numeric(as.character(data_subset$Root_CN_Ratio))
data_subset$Leaf_P_ppm <- as.numeric(as.character(data_subset$Leaf_P_ppm))
data_subset$Root_P_ppm <- as.numeric(as.character(data_subset$Root_P_ppm))
data_subset$Soil_P_gkg <- as.numeric(as.character(data_subset$Soil_P_gkg))
data_subset$Soil_NH4_gkg <- as.numeric(as.character(data_subset$Soil_NH4_gkg))
data_subset$Soil_NO3_gkg <- as.numeric(as.character(data_subset$Soil_NO3_gkg))

# After conversion, check for any NA values that might have been introduced
summary(data_subset)

# Perform correlation tests again
# Correlation between leaf C:N ratio and soil nutrients
cor_leaf_cn_soil <- cor.test(data_subset$Leaf_CN_Ratio, data_subset$Soil_P_gkg, use = "complete.obs")
cor_leaf_cn_nh4 <- cor.test(data_subset$Leaf_CN_Ratio, data_subset$Soil_NH4_gkg, use = "complete.obs")
cor_leaf_cn_no3 <- cor.test(data_subset$Leaf_CN_Ratio, data_subset$Soil_NO3_gkg, use = "complete.obs")

# Correlation between root C:N ratio and soil nutrients
cor_root_cn_soil <- cor.test(data_subset$Root_CN_Ratio, data_subset$Soil_P_gkg, use = "complete.obs")
cor_root_cn_nh4 <- cor.test(data_subset$Root_CN_Ratio, data_subset$Soil_NH4_gkg, use = "complete.obs")
cor_root_cn_no3 <- cor.test(data_subset$Root_CN_Ratio, data_subset$Soil_NO3_gkg, use = "complete.obs")

# Correlation between leaf P and soil nutrients
cor_leaf_p_soil <- cor.test(data_subset$Leaf_P_ppm, data_subset$Soil_P_gkg, use = "complete.obs")
cor_leaf_p_nh4 <- cor.test(data_subset$Leaf_P_ppm, data_subset$Soil_NH4_gkg, use = "complete.obs")
cor_leaf_p_no3 <- cor.test(data_subset$Leaf_P_ppm, data_subset$Soil_NO3_gkg, use = "complete.obs")

# Correlation between root P and soil nutrients
cor_root_p_soil <- cor.test(data_subset$Root_P_ppm, data_subset$Soil_P_gkg, use = "complete.obs")
cor_root_p_nh4 <- cor.test(data_subset$Root_P_ppm, data_subset$Soil_NH4_gkg, use = "complete.obs")
cor_root_p_no3 <- cor.test(data_subset$Root_P_ppm, data_subset$Soil_NO3_gkg, use = "complete.obs")

# Display the correlation results
list(
  Leaf_CN_vs_Soil_P = cor_leaf_cn_soil,
  Leaf_CN_vs_Soil_NH4 = cor_leaf_cn_nh4,
  Leaf_CN_vs_Soil_NO3 = cor_leaf_cn_no3,
  Root_CN_vs_Soil_P = cor_root_cn_soil,
  Root_CN_vs_Soil_NH4 = cor_root_cn_nh4,
  Root_CN_vs_Soil_NO3 = cor_root_cn_no3,
  Leaf_P_vs_Soil_P = cor_leaf_p_soil,
  Leaf_P_vs_Soil_NH4 = cor_leaf_p_nh4,
  Leaf_P_vs_Soil_NO3 = cor_leaf_p_no3,
  Root_P_vs_Soil_P = cor_root_p_soil,
  Root_P_vs_Soil_NH4 = cor_root_p_nh4,
  Root_P_vs_Soil_NO3 = cor_root_p_no3
)
# Install required packages if not already installed
install.packages("GGally")
install.packages("ggplot2")

# Load necessary libraries
library(GGally)
library(ggplot2)

# Check for non-numeric values in the columns
summary(XRF_data$Leaf.C.N..ratio.)
summary(XRF_data$P_.ppm.)

# Handle non-numeric values (replace non-numeric values with NA during conversion)
data_subset <- data.frame(
  Leaf_CN_Ratio = suppressWarnings(as.numeric(as.character(XRF_data$Leaf.C.N..ratio.))),
  Root_CN_Ratio = suppressWarnings(as.numeric(as.character(XRF_data$Root.C.N..ratio.))),
  Leaf_P_ppm = suppressWarnings(as.numeric(as.character(XRF_data$P_.ppm.))),
  Root_P_ppm = suppressWarnings(as.numeric(as.character(XRF_data$P_.ppm..1))),
  Soil_P_gkg = suppressWarnings(as.numeric(as.character(XRF_data$P.g.kg..dw.soil))),
  Soil_NH4_gkg = suppressWarnings(as.numeric(as.character(XRF_data$NH4.g.kg..dw.soil))),
  Soil_NO3_gkg = suppressWarnings(as.numeric(as.character(XRF_data$NO3.g.kg..dw.soil)))
)

# Check for any NAs in the data
summary(data_subset)

# Optionally, remove rows with NA values
data_subset_clean <- na.omit(data_subset)
# Create the pairwise plot using ggpairs after cleaning the data
ggpairs(data_subset_clean, 
        title = "Pairwise Correlation between Soil and Plant Nutrient Concentrations", 
        lower = list(continuous = "smooth"),
        diag = list(continuous = "densityDiag"))

# Using pairs for visualization
pairs(data_subset_clean,
      main = "Pairwise Correlation between Soil and Plant Nutrients",
      pch = 19, col = "blue")




# Install required packages if you don't have them
install.packages("corrplot")
# Install dplyr or tidyverse if not already installed
# install.packages("dplyr")
# OR
# install.packages("tidyverse")

# Load dplyr package
library(dplyr)

# Load necessary libraries
library(corrplot)

# Subset the relevant columns for the correlation analysis
data_subset <- XRF_data %>%
  select(Leaf.C.N..ratio., Root.C.N..ratio., P_.ppm., P_.ppm..1, P.g.kg..dw.soil, NH4.g.kg..dw.soil, NO3.g.kg..dw.soil)
# Identify non-numeric values in the problematic columns
non_numeric_leaf_cn <- data_subset$Leaf.C.N..ratio.[!grepl("^[0-9.]+$", data_subset$Leaf.C.N..ratio.)]
non_numeric_root_cn <- data_subset$Root.C.N..ratio.[!grepl("^[0-9.]+$", data_subset$Root.C.N..ratio.)]

# Print non-numeric values
print(non_numeric_leaf_cn)
print(non_numeric_root_cn)
# Remove rows with non-numeric values in Leaf.C.N..ratio. and Root.C.N..ratio.
data_subset <- data_subset[grepl("^[0-9.]+$", data_subset$Leaf.C.N..ratio.), ]
data_subset <- data_subset[grepl("^[0-9.]+$", data_subset$Root.C.N..ratio.), ]

# Now, proceed with numeric conversion
data_subset$Leaf.C.N..ratio. <- as.numeric(as.character(data_subset$Leaf.C.N..ratio.))
data_subset$Root.C.N..ratio. <- as.numeric(as.character(data_subset$Root.C.N..ratio.))

# Continue with conversion of other columns if necessary

# Convert columns to numeric (if necessary)
data_subset$Leaf.C.N..ratio. <- as.numeric(as.character(data_subset$Leaf.C.N..ratio.))
data_subset$Root.C.N..ratio. <- as.numeric(as.character(data_subset$Root.C.N..ratio.))
data_subset$P_.ppm. <- as.numeric(as.character(data_subset$P_.ppm.))
data_subset$P_.ppm..1 <- as.numeric(as.character(data_subset$P_.ppm..1))
data_subset$P.g.kg..dw.soil <- as.numeric(as.character(data_subset$P.g.kg..dw.soil))
data_subset$NH4.g.kg..dw.soil <- as.numeric(as.character(data_subset$NH4.g.kg..dw.soil))
data_subset$NO3.g.kg..dw.soil <- as.numeric(as.character(data_subset$NO3.g.kg..dw.soil))

# Remove rows with NA values
data_subset_clean <- na.omit(data_subset)

# Calculate correlation matrix
cor_matrix <- cor(data_subset_clean)

# Visualize the correlation matrix
library(corrplot)
corrplot(cor_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7, 
         col = colorRampPalette(c("blue", "white", "red"))(200),
         title = "Correlation Matrix of Soil and Plant Nutrients")











# Load necessary libraries
library(tidyverse)
library(car)
library(emmeans)
library(ggplot2)
library(multcomp)
library(dplyr)

# Load the dataset
Plant_traits <- read.csv("Biomass_height_stem_data_exp1.csv")

# Inspect column names to check for non-standard characters
colnames(Plant_traits)

# Subset the dataset to include relevant columns and convert variables to factors
nodule_data <- Plant_traits %>%
  select(`Dose..N.kg.ha.`, Plant.type, Fertilizer.type, `Nodules.Count.no..`) %>%
  mutate(
    Dose = factor(`Dose..N.kg.ha.`, levels = c("0", "100", "200")),   # Convert Dose to factor
    Fertilizer_Type = factor(Fertilizer.type),                        # Convert Fertilizer type to factor
    Plant.type = factor(Plant.type)                                   # Convert Plant type to factor
  )

# Calculate mean and standard errors for nodule count by Dose, Plant Type, and Fertilizer Type
nodule_summary <- nodule_data %>%
  group_by(Dose, Plant.type, Fertilizer_Type) %>%
  summarise(
    Mean_Nodules = mean(`Nodules.Count.no..`, na.rm = TRUE),
    SE_Nodules = sd(`Nodules.Count.no..`, na.rm = TRUE) / sqrt(n())
  )

# Perform ANOVA for nodule count
anova_nodules <- aov(`Nodules.Count.no..` ~ Dose * Fertilizer_Type * Plant.type, data = nodule_data)
summary(anova_nodules)

# Perform Tukey's HSD test for nodule count if ANOVA is significant
letters_nodules <- rep(NA, nrow(nodule_summary))

if (summary(anova_nodules)[[1]][["Pr(>F)"]][1] < 0.05) {
  tukey_nodules <- HSD.test(anova_nodules, "Dose", group = TRUE)
  for (i in 1:nrow(nodule_summary)) {
    group <- nodule_summary$Dose[i]
    letters_nodules[i] <- tukey_nodules$groups[as.character(group), "groups"]
  }
}

# Add Tukey letters to the nodule_summary
nodule_summary$letters_nodules <- letters_nodules

# Convert Dose to factor for proper ordering in plots
nodule_summary$Dose <- factor(nodule_summary$Dose, levels = c("0", "100", "200"))

# Visualize Nodule Count with Mean, Standard Errors, and Tukey Letters
ggplot(nodule_summary, aes(x = Dose, y = Mean_Nodules, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Nodules - SE_Nodules, ymax = Mean_Nodules + SE_Nodules), 
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Nodule Count by Dose and Fertilizer Type", x = "Dose (N kg/ha)", y = "Nodule Count") +
  facet_wrap(~ Plant.type) +
  scale_fill_manual(values = c("UF" = "red", "MF" = "blue", "C" = "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(y = Mean_Nodules + SE_Nodules + 0.5, label = letters_nodules), position = position_dodge(0.9), vjust = -0.5)






















# Load necessary libraries
library(ggplot2)
library(dplyr)
library(car)
library(emmeans)
library(multcomp)

# Read the dataset
XRF_data <- read.csv("xrf_Nutrients_analysis_exp1_final.csv")

# Ensure necessary columns are treated as factors
XRF_data <- XRF_data %>%
  mutate(
    Dose = factor(Dose, levels = c("0", "100", "200")),  # Adjust Dose column
    Fertilizer_Type = factor(Treatment, levels = c('None', 'MF', 'UF')),  # Adjust Fertilizer type
    Plant.type = factor(Plant.type)  # Ensure Plant.type is a factor
  )

# Step 1: Calculate mean and standard errors for Leaf and Root C:N ratios
cn_summary <- XRF_data %>%
  group_by(Dose, Plant.type, Fertilizer_Type) %>%
  summarize(
    Leaf_CN_mean = mean(Leaf.C.N..ratio., na.rm = TRUE),  # Use the actual column name
    Leaf_CN_se = sd(Leaf.C.N..ratio., na.rm = TRUE) / sqrt(n()),  # Standard error for Leaf C:N
    Root_CN_mean = mean(Root.C.N..ratio., na.rm = TRUE),  # Use the actual column name
    Root_CN_se = sd(Root.C.N..ratio., na.rm = TRUE) / sqrt(n())  # Standard error for Root C:N
  )

# Step 2: Visualize Leaf C:N ratio by Dose, Plant Type, and Fertilizer Type
ggplot(cn_summary, aes(x = Dose, y = Leaf_CN_mean, fill = Fertilizer_Type)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = Leaf_CN_mean - Leaf_CN_se, ymax = Leaf_CN_mean + Leaf_CN_se), 
                width = 0.2, position = position_dodge(0.9), color = "black") +
  facet_wrap(~ Plant.type) +
  labs(title = "Leaf C:N Ratio by Dose, Plant Type, and Fertilizer Type", x = "Dose", y = "Leaf C:N Ratio") +
  scale_fill_manual(values = c("grey20", "grey50", "grey80")) +
  theme_minimal(base_size = 15) +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = "grey"))

# Step 3: Visualize Root C:N ratio by Dose, Plant Type, and Fertilizer Type
ggplot(cn_summary, aes(x = Dose, y = Root_CN_mean, fill = Fertilizer_Type)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = Root_CN_mean - Root_CN_se, ymax = Root_CN_mean + Root_CN_se), 
                width = 0.2, position = position_dodge(0.9), color = "black") +
  facet_wrap(~ Plant.type) +
  labs(title = "Root C:N Ratio by Dose, Plant Type, and Fertilizer Type", x = "Dose", y = "Root C:N Ratio") +
  scale_fill_manual(values = c("grey20", "grey50", "grey80")) +
  theme_minimal(base_size = 15) +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = "grey"))

# Step 4: Fit the model for Leaf C:N ratio
m1_leaf_cn <- lm(Leaf.C.N..ratio. ~ Dose * Plant.type * Fertilizer_Type, data = XRF_data)
summary(m1_leaf_cn)

# Check residuals for linearity and normality
residualPlot(m1_leaf_cn)
qqPlot(m1_leaf_cn)

# Test significance of the model effects (ANOVA)
Anova(m1_leaf_cn, type = "II")

# Perform multiple comparison test (Tukey HSD) for Leaf C:N ratio
pairwise_comparisons_leaf_cn <- emmeans(m1_leaf_cn, ~ Dose | Plant.type | Fertilizer_Type)
summary(pairwise_comparisons_leaf_cn)

# Visualize significant differences using compact letter display for Leaf C:N ratio
cld_leaf_cn <- cld(pairwise_comparisons_leaf_cn)

# Step 5: Fit the model for Root C:N ratio
m1_root_cn <- lm(Root.C.N..ratio. ~ Dose * Plant.type * Fertilizer_Type, data = XRF_data)
summary(m1_root_cn)

# Check residuals for linearity and normality
residualPlot(m1_root_cn)
qqPlot(m1_root_cn)

# Test significance of the model effects (ANOVA)
Anova(m1_root_cn, type = "II")

# Perform multiple comparison test (Tukey HSD) for Root C:N ratio
pairwise_comparisons_root_cn <- emmeans(m1_root_cn, ~ Dose | Plant.type | Fertilizer_Type)
summary(pairwise_comparisons_root_cn)

# Step 6: Join the Tukey test results for Leaf C:N ratio with cn_summary and plot with compact letter display (CLD)
cld_leaf_cn <- as.data.frame(cld_leaf_cn)
merged_leaf_cn <- merge(cn_summary, cld_leaf_cn, by = c("Dose", "Plant.type", "Fertilizer_Type"))

ggplot(merged_leaf_cn, aes(x = Dose, y = Leaf_CN_mean, fill = Fertilizer_Type)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = Leaf_CN_mean - Leaf_CN_se, ymax = Leaf_CN_mean + Leaf_CN_se), 
                width = 0.2, position = position_dodge(0.9), color = "black") +
  facet_wrap(~ Plant.type) +
  geom_text(aes(x = Dose, y = Leaf_CN_mean + Leaf_CN_se + 0.05, label = .group), 
            position = position_dodge(0.9), vjust = 0, color = "black") +
  labs(title = "Leaf C:N Ratio by Dose, Plant Type, and Fertilizer Type", x = "Dose", y = "Leaf C:N Ratio") +
  scale_fill_manual(values = c("grey20", "grey50", "grey80")) +
  theme_minimal(base_size = 15) +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = "grey"))

# Step 7: Join the Tukey test results for Root C:N ratio with cn_summary and plot with compact letter display (CLD)
cld_root_cn <- as.data.frame(cld_root_cn)
merged_root_cn <- merge(cn_summary, cld_root_cn, by = c("Dose", "Plant.type", "Fertilizer_Type"))

ggplot(merged_root_cn, aes(x = Dose, y = Root_CN_mean, fill = Fertilizer_Type)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = Root_CN_mean - Root_CN_se, ymax = Root_CN_mean + Root_CN_se), 
                width = 0.2, position = position_dodge(0.9), color = "black") +
  facet_wrap(~ Plant.type) +
  geom_text(aes(x = Dose, y = Root_CN_mean + Root_CN_se + 0.05, label = .group), 
            position = position_dodge(0.9), vjust = 0, color = "black") +
  labs(title = "Root C:N Ratio by Dose, Plant Type, and Fertilizer Type", x = "Dose", y = "Root C:N Ratio") +
  scale_fill_manual(values = c("grey20", "grey50", "grey80")) +
  theme_minimal(base_size = 15) +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = "grey"))
