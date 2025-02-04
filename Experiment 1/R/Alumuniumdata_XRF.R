# Load necessary libraries
library(tidyverse)
library(car)
library(emmeans)
library(ggplot2)
library(multcomp)

# Set working directory and load data (adjust the path to your specific environment)
setwd("C:\\Users\\90958427\\OneDrive - Western Sydney University\\PolyTunnelS33_ExperimeNT1_dose\\Modified Data File")
Soil_Nutrients <- read.csv("Soil_Nutrients_Extraction_EXP1.csv")

# Ensure that Dose, Plant.type, and Fertilizer.type are treated as factors for analysis
Soil_Nutrients$Dose <- factor(Soil_Nutrients$Dose..N.kg.ha.)
Soil_Nutrients$Plant.type <- factor(Soil_Nutrients$Plant.type)
Soil_Nutrients$Fertilizer.type <- factor(Soil_Nutrients$Fertilizer.type)

# Replace negative values in relevant columns with zero
Soil_Nutrients <- Soil_Nutrients %>%
  mutate(
    Al_.ppm._Leaf = ifelse(Al_.ppm._Leaf < 0, 0, Al_.ppm._Leaf),
    Al_.ppm._root = ifelse(Al_.ppm._root < 0, 0, Al_.ppm._root),
    NH4.g.kg..dw.soil = ifelse(NH4.g.kg..dw.soil < 0, 0, NH4.g.kg..dw.soil),
    NO3.g.kg..dw.soil = ifelse(NO3.g.kg..dw.soil < 0, 0, NO3.g.kg..dw.soil)
  )

# Summarize means and standard errors for each group (Dose, Plant type, Fertilizer type) for Aluminum data
aluminum_summary <- Soil_Nutrients %>%
  group_by(Dose, Plant.type, Fertilizer.type) %>%
  summarize(
    Al_Leaf_mean = mean(Al_.ppm._Leaf, na.rm = TRUE),
    Al_Leaf_se = sd(Al_.ppm._Leaf, na.rm = TRUE) / sqrt(n()),
    Al_Root_mean = mean(Al_.ppm._root, na.rm = TRUE),
    Al_Root_se = sd(Al_.ppm._root, na.rm = TRUE) / sqrt(n())
  )

### Aluminum in Leaf Analysis ###
# Fit the model for Aluminum in leaves data
m1_al_leaf <- lm(Al_.ppm._Leaf ~ Dose * Plant.type * Fertilizer.type, data = Soil_Nutrients)
summary(m1_al_leaf)

# Check residuals for linearity and normality
residualPlot(m1_al_leaf)
qqPlot(m1_al_leaf)

# Test significance of the model effects (ANOVA)
Anova(m1_al_leaf, type = "II")

# Perform Tukey test and get compact letter display
pairwise_comparisons_al_leaf <- emmeans(m1_al_leaf, ~ Dose | Plant.type | Fertilizer.type)
cld_al_leaf <- cld(pairwise_comparisons_al_leaf, Letters = letters, adjust = "tukey")

# Merge Tukey test results with the aluminum summary for leaf
cld_al_leaf <- as.data.frame(cld_al_leaf)
merged_al_leaf <- merge(aluminum_summary, cld_al_leaf, by = c("Dose", "Plant.type", "Fertilizer.type"))

# Plot Aluminum in leaves data with compact letter display
ggplot(merged_al_leaf, aes(x = Dose, y = Al_Leaf_mean, fill = Fertilizer.type)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = Al_Leaf_mean - Al_Leaf_se, ymax = Al_Leaf_mean + Al_Leaf_se), width = 0.2, position = position_dodge(0.9), color = "black") +
  facet_wrap(~ Plant.type) +
  geom_text(aes(x = Dose, y = Al_Leaf_mean + Al_Leaf_se + 0.05, label = .group), position = position_dodge(0.9), vjust = 0, color = "black") +
  labs(title = "Aluminum in Leaves by Dose, Plant Type, and Fertilizer Type", x = "Dose", y = "Aluminum in Leaves (ppm)") +
  scale_fill_manual(values = c("grey20", "grey50", "grey80")) +
  theme_minimal(base_size = 15) +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = "grey"))

### Aluminum in Root Analysis ###
# Fit the model for Aluminum in roots data
m1_al_root <- lm(Al_.ppm._root ~ Dose * Plant.type * Fertilizer.type, data = Soil_Nutrients)
summary(m1_al_root)

# Check residuals for linearity and normality
residualPlot(m1_al_root)
qqPlot(m1_al_root)

# Test significance of the model effects (ANOVA)
Anova(m1_al_root, type = "II")

# Perform Tukey test and get compact letter display
pairwise_comparisons_al_root <- emmeans(m1_al_root, ~ Dose | Plant.type | Fertilizer.type)
cld_al_root <- cld(pairwise_comparisons_al_root, Letters = letters, adjust = "tukey")

# Merge Tukey test results with the aluminum summary for root
cld_al_root <- as.data.frame(cld_al_root)
merged_al_root <- merge(aluminum_summary, cld_al_root, by = c("Dose", "Plant.type", "Fertilizer.type"))

# Plot Aluminum in roots data with compact letter display
ggplot(merged_al_root, aes(x = Dose, y = Al_Root_mean, fill = Fertilizer.type)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = Al_Root_mean - Al_Root_se, ymax = Al_Root_mean + Al_Root_se), width = 0.2, position = position_dodge(0.9), color = "black") +
  facet_wrap(~ Plant.type) +
  geom_text(aes(x = Dose, y = Al_Root_mean + Al_Root_se + 0.05, label = .group), position = position_dodge(0.9), vjust = 0, color = "black") +
  labs(title = "Aluminum in Roots by Dose, Plant Type, and Fertilizer Type", x = "Dose", y = "Aluminum in Roots (ppm)") +
  scale_fill_manual(values = c("grey20", "grey50", "grey80")) +
  theme_minimal(base_size = 15) +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = "grey"))
