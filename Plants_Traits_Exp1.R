# Load required libraries
library(tidyverse)
library(car)
library(emmeans)
library(ggplot2)

# Set working directory (adjust as needed)
setwd("C:\\Users\\90958427\\OneDrive - Western Sydney University\\PolyTunnelS33_ExperimeNT1_dose\\Modified Data File")
list.files()
Plant_traits <-read.csv("Biomass_height_stem_data_exp1.csv")
# Load the dataset
head(Plant_traits)
colnames(Plant_traits)







# Analysis for Plant Height Before Fertilization
ggplot(Plant_traits, aes(x = Dose, y = Plant_Height_BF..cm., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Plant Height Before Fertilization by Dose and Plant Type", x = "Dose", y = "Plant Height (cm)")

m1_height <- lm((Plant_Height_BF..cm.) ~ Dose * Plant.type , data = Plant_traits)
summary(m1_height)
residualPlot(m1_height)
qqPlot(m1_height)
Anova(m1_height)
multcomp::cld(emmeans(m1_height, ~ Dose | Plant.type))

# Analysis for Stem Count per Pot Before Fertilization
ggplot(Plant_traits, aes(x = Dose, y = Stem_countperPot_BF..no.., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Stem Count per Pot Before Fertilization by Dose and Plant Type", x = "Dose", y = "Stem Count per Pot")

m1_stem <- lm(log10(Stem_countperPot_BF..no..) ~ Dose * Plant.type , data = Plant_traits)
summary(m1_stem)
residualPlot(m1_stem)
qqPlot(m1_stem)
Anova(m1_stem)
multcomp::cld(emmeans(m1_stem, ~ Dose | Plant.type))

# Analysis for Fresh Biomass Before Fertilization
ggplot(Plant_traits, aes(x = Dose, y = Fresh_biomass_BF..gm., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Fresh Biomass Before Fertilization by Dose and Plant Type", x = "Dose", y = "Fresh Biomass (gm)")

m1_biomass <- lm(log10(Fresh_biomass_BF..gm.) ~ Dose * Plant.type , data = Plant_traits)
summary(m1_biomass)
residualPlot(m1_biomass)
qqPlot(m1_biomass)
Anova(m1_biomass)
multcomp::cld(emmeans(m1_biomass, ~ Dose | Plant.type))

# Continue similar analysis for remaining traits:
# Fresh Biomass After 30 Days
ggplot(Plant_traits, aes(x = Dose, y = Fresh_biomass_AF_30days.gm., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Fresh Biomass After 30 Days by Dose and Plant Type", x = "Dose", y = "Fresh Biomass After 30 Days (gm)")

m1_biomass_af30 <- lm((Fresh_biomass_AF_30days.gm.) ~ Dose * Plant.type , data = Plant_traits)
summary(m1_biomass_af30)
residualPlot(m1_biomass_af30)
qqPlot(m1_biomass_af30)
Anova(m1_biomass_af30)
multcomp::cld(emmeans(m1_biomass_af30, ~ Dose | Plant.type))

# Plant Height at Harvest Day
ggplot(Plant_traits, aes(x = Dose, y = Plant_Height_Harvestday.cm., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Plant Height at Harvest Day by Dose and Plant Type", x = "Dose", y = "Plant Height at Harvest Day (cm)")

m1_height_harvest <- lm((Plant_Height_Harvestday.cm.) ~ Dose * Plant.type , data = Plant_traits)
summary(m1_height_harvest)
residualPlot(m1_height_harvest)
qqPlot(m1_height_harvest)
Anova(m1_height_harvest)
multcomp::cld(emmeans(m1_height_harvest, ~ Dose | Plant.type))

# Stem Count at Harvest Day
ggplot(Plant_traits, aes(x = Dose, y = Stem_count_HarvestDay.no.., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Stem Count at Harvest Day by Dose and Plant Type", x = "Dose", y = "Stem Count at Harvest Day")

m1_stem_harvest <- lm((Stem_count_HarvestDay.no..) ~ Dose * Plant.type , data = Plant_traits)
summary(m1_stem_harvest)
residualPlot(m1_stem_harvest)
qqPlot(m1_stem_harvest)
Anova(m1_stem_harvest)
multcomp::cld(emmeans(m1_stem_harvest, ~ Dose | Plant.type))

# Root Length
ggplot(Plant_traits, aes(x = Dose, y = Root.Length..cm., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Root Length by Dose and Plant Type", x = "Dose", y = "Root Length (cm)")

m1_root_length <- lm((Root.Length..cm.) ~ Dose * Plant.type , data = Plant_traits)
summary(m1_root_length)
residualPlot(m1_root_length)
qqPlot(m1_root_length)
Anova(m1_root_length)
multcomp::cld(emmeans(m1_root_length, ~ Dose | Plant.type))

# Nodules Count
ggplot(Plant_traits, aes(x = Dose, y = Nodules.Count.no.., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Nodules Count by Dose and Plant Type", x = "Dose", y = "Nodules Count")

m1_nodules <- lm((Nodules.Count.no..) ~ Dose * Plant.type , data = Plant_traits)
summary(m1_nodules)
residualPlot(m1_nodules)
qqPlot(m1_nodules)
Anova(m1_nodules)
multcomp::cld(emmeans(m1_nodules, ~ Dose | Plant.type))

# Fresh Biomass at Harvest Day
ggplot(Plant_traits, aes(x = Dose, y = Fresh.Biomass_harvestday..gm., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Fresh Biomass at Harvest Day by Dose and Plant Type", x = "Dose", y = "Fresh Biomass at Harvest Day (gm)")

m1_fresh_biomass_harvest <- lm((Fresh.Biomass_harvestday..gm.) ~ Dose * Plant.type , data = Plant_traits)
summary(m1_fresh_biomass_harvest)
residualPlot(m1_fresh_biomass_harvest)
qqPlot(m1_fresh_biomass_harvest)
Anova(m1_fresh_biomass_harvest)
multcomp::cld(emmeans(m1_fresh_biomass_harvest, ~ Dose | Plant.type))

# Root Biomass at Harvest Day
ggplot(Plant_traits, aes(x = Dose, y = Root.Biomass_harvestday.gm., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Root Biomass at Harvest Day by Dose and Plant Type", x = "Dose", y = "Root Biomass at Harvest Day (gm)")

m1_root_biomass_harvest <- lm((Root.Biomass_harvestday.gm.) ~ Dose * Plant.type , data = Plant_traits)
summary(m1_root_biomass_harvest)
residualPlot(m1_root_biomass_harvest)
qqPlot(m1_root_biomass_harvest)
Anova(m1_root_biomass_harvest)
multcomp::cld(emmeans(m1_root_biomass_harvest, ~ Dose | Plant.type))
head(Plant_traits)
colnames(Plant_traits)

# Fresh Biomass at Harvest Day
ggplot(Plant_traits, aes(x = Dose, y = R.S.ratio, colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Root:Shoot Ratio", x = "Dose", y = "Harvest day")
m1_RSratio <- lm(`R.S.ratio` ~ Dose * Plant.type, data = Plant_traits)
summary(m1_RSratio)
residualPlot(m1_RSratio)
qqPlot(m1_RSratio)
Anova(m1_RSratio)
multcomp::cld(emmeans(m1_RSratio, ~ Dose | Plant.type))


colnames(Plant_traits)
# Load the necessary libraries (if not already loaded)
library(ggplot2)

# Assuming Plant_traits is your dataset
# Pearson Correlation between NH??? and Nodule Count
cor_nh4_nodules <- cor(Plant_traits$NH4.g.kg..dw.soil, Plant_traits$Nodules.Count.no.., method = "pearson", use = "complete.obs")
cat("Pearson correlation between NH??? and Nodule Count: ", cor_nh4_nodules, "\n")

# Pearson Correlation between NO??? and Nodule Count
cor_no3_nodules <- cor(Plant_traits$NO3.g.kg..dw.soil, Plant_traits$Nodules.Count.no.., method = "pearson", use = "complete.obs")
cat("Pearson correlation between NO??? and Nodule Count: ", cor_no3_nodules, "\n")

# If you want to use Spearman correlation (in case of non-normal data)
# Spearman Correlation between NH??? and Nodule Count
spearman_nh4_nodules <- cor(Plant_traits$NH4.g.kg..dw.soil, Plant_traits$Nodules.Count.no.., method = "spearman", use = "complete.obs")
cat("Spearman correlation between NH??? and Nodule Count: ", spearman_nh4_nodules, "\n")

# Spearman Correlation between NO??? and Nodule Count
spearman_no3_nodules <- cor(Plant_traits$NO3.g.kg..dw.soil, Plant_traits$Nodules.Count.no.., method = "spearman", use = "complete.obs")
cat("Spearman correlation between NO??? and Nodule Count: ", spearman_no3_nodules, "\n")

# Optional: Visualize the correlation using scatter plots with regression lines

# Scatter plot for NH??? and Nodule Count
ggplot(Plant_traits, aes(x = NH4.g.kg..dw.soil, y = Nodules.Count.no..)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(title = "NH??? Concentration vs Nodule Count", x = "NH??? (g/kg dw soil)", y = "Nodule Count") +
  theme_minimal()

# Scatter plot for NO??? and Nodule Count
ggplot(Plant_traits, aes(x = NO3.g.kg..dw.soil, y = Nodules.Count.no..)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "green") +
  labs(title = "NO??? Concentration vs Nodule Count", x = "NO??? (g/kg dw soil)", y = "Nodule Count") +
  theme_minimal()
# Load necessary libraries
library(ggcorrplot)
library(ggplot2)

# Create a subset of your data that includes NH???, NO???, and Nodule Count
cor_data <- Plant_traits[, c("NH4.g.kg..dw.soil", "NO3.g.kg..dw.soil", "Nodules.Count.no..")]

# Calculate the correlation matrix between NH???, NO???, and Nodule Count
cor_matrix <- cor(cor_data, method = "pearson", use = "complete.obs")

# Visualize the correlation matrix as a heatmap using ggcorrplot
ggcorrplot(cor_matrix, 
           method = "circle", 
           type = "lower", 
           lab = TRUE, 
           title = "Correlation Heatmap Between NH???, NO???, and Nodule Count",
           colors = c("red", "white", "blue")) +
  theme_minimal()






# Load necessary libraries
library(ggcorrplot)

# Assuming the dataset is named 'Plant_traits'
# Calculate Pearson or Spearman Correlation Matrix between soil nutrients and plant traits

# Subset of relevant columns
cor_data <- Plant_traits[, c("NH4.g.kg..dw.soil", "NO3.g.kg..dw.soil", "P.g.kg..dw.soil",
                             "Nodules.Count.no..", "Root.Length..cm.", "Root.Biomass_harvestday.gm.",
                             "Fresh.Biomass_harvestday..gm.")]

# Pearson Correlation
cor_matrix_pearson <- cor(cor_data, method = "pearson", use = "complete.obs")

# Spearman Correlation (if you want non-linear)
cor_matrix_spearman <- cor(cor_data, method = "spearman", use = "complete.obs")

# Visualize the correlation matrix using ggcorrplot (Pearson example)
ggcorrplot(cor_matrix_pearson, method = "circle", type = "lower", lab = TRUE, 
           title = "Pearson Correlation Between Soil Nutrients and Plant Traits")

# Alternatively, visualize Spearman Correlation
# ggcorrplot(cor_matrix_spearman, method = "circle", type = "lower", lab = TRUE, 
#            title = "Spearman Correlation Between Soil Nutrients and Plant Traits")

# Print the correlation matrix to check the exact correlation values
print("Pearson Correlation Matrix:")
print(cor_matrix_pearson)

# If needed, print Spearman matrix
# print("Spearman Correlation Matrix:")
# print(cor_matrix_spearman)






































# Load necessary libraries
library(ggplot2)
library(dplyr)
library(multcompView)
install.packages("agricolae")
library(agricolae)
library(car)
library(emmeans)

# Set the working directory (adjust path as needed)
setwd("C:\\Users\\90958427\\OneDrive - Western Sydney University\\PolyTunnelS33_ExperimeNT1_dose\\Modified Data File")

# Load the dataset
Plant_traits <- read.csv("Biomass_height_stem_data_exp1.csv")

# Subset the required columns for analysis
data_subset <- Plant_traits %>%
  select(Dose, Plant.type, Fresh.Biomass_harvestday..gm., Root.Biomass_harvestday.gm.)

# Calculate mean and standard errors for shoot and root biomass by Dose and Plant Type
summary_data <- data_subset %>%
  group_by(Dose, Plant.type) %>%
  summarise(
    Shoot_Biomass_Mean = mean(Fresh.Biomass_harvestday..gm., na.rm = TRUE),
    Shoot_Biomass_SE = sd(Fresh.Biomass_harvestday..gm., na.rm = TRUE) / sqrt(n()),
    Root_Biomass_Mean = mean(Root.Biomass_harvestday.gm., na.rm = TRUE),
    Root_Biomass_SE = sd(Root.Biomass_harvestday.gm., na.rm = TRUE) / sqrt(n())
  )

# Perform ANOVA for shoot biomass
anova_shoot <- aov(Fresh.Biomass_harvestday..gm. ~ Dose * Plant.type, data = data_subset)
anova_root <- aov(Root.Biomass_harvestday.gm. ~ Dose * Plant.type, data = data_subset)

# Perform Tukey's HSD test for shoot and root biomass, and assign letters if significant
letters_shoot <- rep(NA, nrow(summary_data))
letters_root <- rep(NA, nrow(summary_data))

# Check if the ANOVA results are significant for shoot biomass
if (summary(anova_shoot)[[1]][["Pr(>F)"]][1] < 0.05) {
  # Perform Tukey's HSD test for shoot biomass
  tukey_shoot <- HSD.test(anova_shoot, "Dose", group = TRUE)
  # Match Tukey letters to the correct Dose and Plant.type
  for (i in 1:nrow(summary_data)) {
    group <- summary_data$Dose[i]
    letters_shoot[i] <- tukey_shoot$groups[as.character(group), "groups"]
  }
}

# Check if the ANOVA results are significant for root biomass
if (summary(anova_root)[[1]][["Pr(>F)"]][1] < 0.05) {
  # Perform Tukey's HSD test for root biomass
  tukey_root <- HSD.test(anova_root, "Dose", group = TRUE)
  # Match Tukey letters to the correct Dose and Plant.type
  for (i in 1:nrow(summary_data)) {
    group <- summary_data$Dose[i]
    letters_root[i] <- tukey_root$groups[as.character(group), "groups"]
  }
}

# Add Tukey letters to summary_data
summary_data$letters_shoot <- letters_shoot
summary_data$letters_root <- letters_root

# Create bar plot for shoot and root biomass with error bars and Tukey's test results
ggplot(summary_data, aes(x = Dose, fill = Plant.type)) +
  geom_bar(aes(y = Shoot_Biomass_Mean), stat = "identity", position = position_dodge(width = 0.9), colour = "black") +
  geom_errorbar(aes(ymin = Shoot_Biomass_Mean - Shoot_Biomass_SE, ymax = Shoot_Biomass_Mean + Shoot_Biomass_SE), 
                width = 0.2, position = position_dodge(0.9)) +
  geom_bar(aes(y = Root_Biomass_Mean), stat = "identity", position = position_dodge(width = 0.9), colour = "black", alpha = 0.5) +
  geom_errorbar(aes(ymin = Root_Biomass_Mean - Root_Biomass_SE, ymax = Root_Biomass_Mean + Root_Biomass_SE), 
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Plant Biomass (Shoot and Root) by Dose and Plant Type", x = "Dose", y = "Biomass (gm)") +
  theme_minimal() +
  facet_wrap(~ Plant.type) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # Add Tukey's letters for shoot biomass
  geom_text(aes(y = Shoot_Biomass_Mean + Shoot_Biomass_SE + 0.5, label = letters_shoot), 
            position = position_dodge(0.9), vjust = -0.5) +
  # Add Tukey's letters for root biomass
  geom_text(aes(y = Root_Biomass_Mean + Root_Biomass_SE + 0.5, label = letters_root), 
            position = position_dodge(0.9), vjust = -0.5)
