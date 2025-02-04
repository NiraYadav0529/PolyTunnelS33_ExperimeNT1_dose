Soil_Nutrients <- read.csv("Soil_Nutrients_Extraction_EXP1.csv")
head(Soil_Nutrients)
# Load necessary libraries
library(dplyr)        # For data manipulation
library(ggplot2)      # For visualizations
library(factoextra)   # For PCA visualization
library(corrplot)     # For correlation matrix
library(car)          # For ANOVA and diagnostics
library(multcomp)     # For Tukey HSD test

# Read the dataset
Soil_Nutrients <- read.csv("Soil_Nutrients_Extraction_EXP1.csv")

# Inspect the data structure to ensure columns are loaded properly
str(Soil_Nutrients)
head(Soil_Nutrients)

# Data preparation: Select relevant columns including 'Fertilizer.type'
data_analysis <- Soil_Nutrients %>%
  dplyr::select(Sample.ID, Plant.type, Fertilizer.type, Dose..N.kg.ha., 
                NH4.g.kg..dw.soil, NO3.g.kg..dw.soil, pH.Harvest.day, 
                EC.harvest.day,Al_.ppm._Leaf, Al_.ppm._root, SPAD.Data..nmol.ch.mg.fresh.weight.)

# Remove rows with missing values for all relevant variables, including grouping variables like 'Fertilizer.type'
pca_data <- data_analysis %>%
  dplyr::select(Fertilizer.type, NH4.g.kg..dw.soil, NO3.g.kg..dw.soil, pH.Harvest.day, 
                EC.harvest.day,Al_.ppm._Leaf, Al_.ppm._root, SPAD.Data..nmol.ch.mg.fresh.weight.) %>%
  na.omit()

# Extract the 'Fertilizer.type' variable to use for coloring in the PCA plot after handling NAs
fertilizer_type_cleaned <- pca_data$Fertilizer.type

# Remove 'Fertilizer.type' from the PCA data, as it is a categorical variable
pca_data <- pca_data %>%
  dplyr::select(-Fertilizer.type)

# Perform PCA using prcomp (center and scale the data)
pca_result <- prcomp(pca_data, scale = TRUE)

# Visualize the PCA biplot using 'fertilizer_type_cleaned' for coloring
fviz_pca_biplot(pca_result, geom.ind = "point", pointshape = 21, 
                fill.ind = fertilizer_type_cleaned, col.ind = "black", 
                addEllipses = TRUE, ellipse.type = "confidence", 
                label = "var", col.var = "blue", repel = TRUE) +
  ggtitle("PCA Biplot: Soil and Plant Health Metrics")

# Correlation Matrix
cor_matrix <- cor(pca_data, use = "complete.obs")
corrplot(cor_matrix, method = "circle")

# ANOVA
