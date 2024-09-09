library(tidyverse)
library(ggplot2)
# Setting the working directory

setwd("C:\\Users\\90958427\\OneDrive - Western Sydney University\\PolyTunnelS33_ExperimeNT1_dose\\Modified Data File")
list.files()

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

