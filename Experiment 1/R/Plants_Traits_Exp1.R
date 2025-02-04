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
library(agricolae)

# Subset the dataset for shoot biomass analysis
shoot_biomass <- Plant_traits %>%
  select(Dose, Plant.type, Fresh.Biomass_harvestday..gm.)

# Calculate mean and standard error for each group (Dose and Plant.type)
shoot_summary <- shoot_biomass %>%
  group_by(Dose, Plant.type) %>%
  summarise(
    Mean_Shoot = mean(Fresh.Biomass_harvestday..gm., na.rm = TRUE),
    SE_Shoot = sd(Fresh.Biomass_harvestday..gm., na.rm = TRUE) / sqrt(n())
  )

# ANOVA for Shoot Biomass
anova_shoot <- aov(Fresh.Biomass_harvestday..gm. ~ Dose * Plant.type, data = shoot_biomass)
summary(anova_shoot)

# Tukey's post-hoc test
tukey_shoot <- HSD.test(anova_shoot, "Dose", group = TRUE)

# Plot Shoot Biomass
ggplot(shoot_summary, aes(x = Dose, y = Mean_Shoot, fill = Plant.type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Mean_Shoot - SE_Shoot, ymax = Mean_Shoot + SE_Shoot), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Shoot Biomass by Dose and Plant Type", x = "Dose", y = "Shoot Biomass (gm)") +
  theme_minimal()


# Subset the dataset for root biomass analysis
root_biomass <- Plant_traits %>%
  select(Dose, Plant.type, Root.Biomass_harvestday.gm.)

# Calculate mean and standard error for each group (Dose and Plant.type)
root_summary <- root_biomass %>%
  group_by(Dose, Plant.type) %>%
  summarise(
    Mean_Root = mean(Root.Biomass_harvestday.gm., na.rm = TRUE),
    SE_Root = sd(Root.Biomass_harvestday.gm., na.rm = TRUE) / sqrt(n())
  )

# ANOVA for Root Biomass
anova_root <- aov(Root.Biomass_harvestday.gm. ~ Dose * Plant.type, data = root_biomass)
summary(anova_root)

# Tukey's post-hoc test
tukey_root <- HSD.test(anova_root, "Dose", group = TRUE)

# Plot Root Biomass
ggplot(root_summary, aes(x = Dose, y = Mean_Root, fill = Plant.type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Mean_Root - SE_Root, ymax = Mean_Root + SE_Root), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Root Biomass by Dose and Plant Type", x = "Dose", y = "Root Biomass (gm)") +
  theme_minimal()



# Subset the dataset for plant height analysis
plant_height <- Plant_traits %>%
  select(Dose, Plant.type, Plant_Height_Harvestday.cm.)

# Calculate mean and standard error for each group (Dose and Plant.type)
height_summary <- plant_height %>%
  group_by(Dose, Plant.type) %>%
  summarise(
    Mean_Height = mean(Plant_Height_Harvestday.cm., na.rm = TRUE),
    SE_Height = sd(Plant_Height_Harvestday.cm., na.rm = TRUE) / sqrt(n())
  )

# ANOVA for Plant Height
anova_height <- aov(Plant_Height_Harvestday.cm. ~ Dose * Plant.type, data = plant_height)
summary(anova_height)

# Tukey's post-hoc test
tukey_height <- HSD.test(anova_height, "Dose", group = TRUE)

# Plot Plant Height
ggplot(height_summary, aes(x = Dose, y = Mean_Height, fill = Plant.type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Mean_Height - SE_Height, ymax = Mean_Height + SE_Height), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Plant Height by Dose and Plant Type", x = "Dose", y = "Plant Height (cm)") +
  theme_minimal()


# Subset the dataset for root length analysis
root_length <- Plant_traits %>%
  select(Dose, Plant.type, Root.Length..cm.)

# Calculate mean and standard error for each group (Dose and Plant.type)
length_summary <- root_length %>%
  group_by(Dose, Plant.type) %>%
  summarise(
    Mean_Length = mean(Root.Length..cm., na.rm = TRUE),
    SE_Length = sd(Root.Length..cm., na.rm = TRUE) / sqrt(n())
  )

# ANOVA for Root Length
anova_length <- aov(Root.Length..cm. ~ Dose * Plant.type, data = root_length)
summary(anova_length)

# Tukey's post-hoc test
tukey_length <- HSD.test(anova_length, "Dose", group = TRUE)

# Plot Root Length
ggplot(length_summary, aes(x = Dose, y = Mean_Length, fill = Plant.type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Mean_Length - SE_Length, ymax = Mean_Length + SE_Length), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Root Length by Dose and Plant Type", x = "Dose", y = "Root Length (cm)") +
  theme_minimal()



# Subset the dataset for nodule count analysis
nodule_count <- Plant_traits %>%
  select(Dose, Plant.type, Nodules.Count.no..)

# Calculate mean and standard error for each group (Dose and Plant.type)
nodule_summary <- nodule_count %>%
  group_by(Dose, Plant.type) %>%
  summarise(
    Mean_Nodule = mean(Nodules.Count.no.., na.rm = TRUE),
    SE_Nodule = sd(Nodules.Count.no.., na.rm = TRUE) / sqrt(n())
  )

# ANOVA for Nodule Count
anova_nodule <- aov(Nodules.Count.no.. ~ Dose * Plant.type, data = nodule_count)
summary(anova_nodule)

# Tukey's post-hoc test
tukey_nodule <- HSD.test(anova_nodule, "Dose", group = TRUE)

# Plot Nodule Count
ggplot(nodule_summary, aes(x = Dose, y = Mean_Nodule, fill = Plant.type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Mean_Nodule - SE_Nodule, ymax = Mean_Nodule + SE_Nodule), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Nodule Count by Dose and Plant Type", x = "Dose", y = "Nodule Count") +
  theme_minimal()



# Load necessary libraries
library(ggplot2)
library(dplyr)
library(agricolae)

# Subset the dataset for shoot and root biomass analysis
biomass_data <- Plant_traits %>%
  select(Dose, Plant.type, Fresh.Biomass_harvestday..gm., Root.Biomass_harvestday.gm.)

# Calculate mean and standard error for each group (Dose and Plant.type)
biomass_summary <- biomass_data %>%
  group_by(Dose, Plant.type) %>%
  summarise(
    Mean_Shoot = mean(Fresh.Biomass_harvestday..gm., na.rm = TRUE),
    SE_Shoot = sd(Fresh.Biomass_harvestday..gm., na.rm = TRUE) / sqrt(n()),
    Mean_Root = mean(Root.Biomass_harvestday.gm., na.rm = TRUE),
    SE_Root = sd(Root.Biomass_harvestday.gm., na.rm = TRUE) / sqrt(n())
  )

# ANOVA for Shoot and Root Biomass
anova_shoot <- aov(Fresh.Biomass_harvestday..gm. ~ Dose * Plant.type, data = biomass_data)
anova_root <- aov(Root.Biomass_harvestday.gm. ~ Dose * Plant.type, data = biomass_data)

# Perform Tukey's HSD test for shoot and root biomass if ANOVA is significant
letters_shoot <- rep(NA, nrow(biomass_summary))
letters_root <- rep(NA, nrow(biomass_summary))

# Check if ANOVA is significant for Shoot Biomass
if (summary(anova_shoot)[[1]][["Pr(>F)"]][1] < 0.05) {
  tukey_shoot <- HSD.test(anova_shoot, "Dose", group = TRUE)
  for (i in 1:nrow(biomass_summary)) {
    group <- biomass_summary$Dose[i]
    letters_shoot[i] <- tukey_shoot$groups[as.character(group), "groups"]
  }
}

# Check if ANOVA is significant for Root Biomass
if (summary(anova_root)[[1]][["Pr(>F)"]][1] < 0.05) {
  tukey_root <- HSD.test(anova_root, "Dose", group = TRUE)
  for (i in 1:nrow(biomass_summary)) {
    group <- biomass_summary$Dose[i]
    letters_root[i] <- tukey_root$groups[as.character(group), "groups"]
  }
}

# Add Tukey letters to the biomass_summary
biomass_summary$letters_shoot <- letters_shoot
biomass_summary$letters_root <- letters_root

# Create bar plot for shoot and root biomass with Tukey letters
ggplot(biomass_summary, aes(x = Dose, fill = Plant.type)) +
  geom_bar(aes(y = Mean_Shoot), stat = "identity", position = position_dodge(width = 0.9), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Shoot - SE_Shoot, ymax = Mean_Shoot + SE_Shoot), width = 0.2, position = position_dodge(0.9)) +
  geom_bar(aes(y = Mean_Root), stat = "identity", position = position_dodge(width = 0.9), colour = "black", alpha = 0.5) +
  geom_errorbar(aes(ymin = Mean_Root - SE_Root, ymax = Mean_Root + SE_Root), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Plant Biomass (Shoot and Root) by Dose and Plant Type", x = "Dose", y = "Biomass (gm)") +
  theme_minimal() +
  facet_wrap(~ Plant.type) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # Add Tukey letters for shoot biomass
  geom_text(aes(y = Mean_Shoot + SE_Shoot + 0.5, label = letters_shoot), position = position_dodge(0.9), vjust = -0.5) +
  # Add Tukey letters for root biomass
  geom_text(aes(y = Mean_Root + SE_Root + 0.5, label = letters_root), position = position_dodge(0.9), vjust = -0.5)



# Bar plot for Shoot Biomass
ggplot(biomass_summary, aes(x = Dose, fill = Plant.type)) +
  geom_bar(aes(y = Mean_Shoot), stat = "identity", position = position_dodge(width = 0.9), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Shoot - SE_Shoot, ymax = Mean_Shoot + SE_Shoot), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Shoot Biomass by Dose and Plant Type", x = "Dose", y = "Shoot Biomass (gm)") +
  theme_minimal() +
  facet_wrap(~ Plant.type) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # Add Tukey letters for shoot biomass
  geom_text(aes(y = Mean_Shoot + SE_Shoot + 0.5, label = letters_shoot), position = position_dodge(0.9), vjust = -0.5)


# Bar plot for Root Biomass
ggplot(biomass_summary, aes(x = Dose, fill = Plant.type)) +
  geom_bar(aes(y = Mean_Root), stat = "identity", position = position_dodge(width = 0.9), colour = "black", alpha = 0.5) +
  geom_errorbar(aes(ymin = Mean_Root - SE_Root, ymax = Mean_Root + SE_Root), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Root Biomass by Dose and Plant Type", x = "Dose", y = "Root Biomass (gm)") +
  theme_minimal() +
  facet_wrap(~ Plant.type) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # Add Tukey letters for root biomass
  geom_text(aes(y = Mean_Root + SE_Root + 0.5, label = letters_root), position = position_dodge(0.9), vjust = -0.5)

# Subset the dataset for plant height analysis
plant_height_data <- Plant_traits %>%
  select(Dose, Plant.type, Plant_Height_Harvestday.cm.)

# Calculate mean and standard error for Plant Height
height_summary <- plant_height_data %>%
  group_by(Dose, Plant.type) %>%
  summarise(
    Mean_Height = mean(Plant_Height_Harvestday.cm., na.rm = TRUE),
    SE_Height = sd(Plant_Height_Harvestday.cm., na.rm = TRUE) / sqrt(n())
  )

# ANOVA for Plant Height
anova_height <- aov(Plant_Height_Harvestday.cm. ~ Dose * Plant.type, data = plant_height_data)

# Perform Tukey's HSD test for Plant Height if ANOVA is significant
letters_height <- rep(NA, nrow(height_summary))

if (summary(anova_height)[[1]][["Pr(>F)"]][1] < 0.05) {
  tukey_height <- HSD.test(anova_height, "Dose", group = TRUE)
  for (i in 1:nrow(height_summary)) {
    group <- height_summary$Dose[i]
    letters_height[i] <- tukey_height$groups[as.character(group), "groups"]
  }
}

# Add Tukey letters to the height_summary
height_summary$ plant_traits <- letters_height

# Plot Plant Height
ggplot(height_summary, aes(x = Dose, y = Mean_Height, fill = Plant.type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Height - SE_Height, ymax = Mean_Height + SE_Height), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Plant Height by Dose and Plant Type", x = "Dose", y = "Plant Height (cm)") +
  theme_minimal() +
  facet_wrap(~ Plant.type) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(y = Mean_Height + SE_Height + 0.5, label = letters_height), position = position_dodge(0.9), vjust = -0.5)

















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

# Subset the dataset for plant height analysis, including Fertilizer Type
plant_height_data <- Plant_traits %>%
  select(Dose, Plant.type, Plant_Height_Harvestday.cm., Treatment) %>%
  mutate(Fertilizer_Type = ifelse(grepl("UF", Treatment), "UF", 
                                  ifelse(grepl("MF", Treatment), "MF", "None")))

# Calculate mean and standard error for Plant Height by Fertilizer Type and Dose
height_summary <- plant_height_data %>%
  group_by(Dose, Plant.type, Fertilizer_Type) %>%
  summarise(
    Mean_Height = mean(Plant_Height_Harvestday.cm., na.rm = TRUE),
    SE_Height = sd(Plant_Height_Harvestday.cm., na.rm = TRUE) / sqrt(n())
  )

# ANOVA for Plant Height
anova_height <- aov(Plant_Height_Harvestday.cm. ~ Dose * Fertilizer_Type * Plant.type, data = plant_height_data)

# Initialize letters_height to avoid issues
letters_height <- rep(NA, nrow(height_summary))

# Check if the ANOVA results are significant for Plant Height
if (summary(anova_height)[[1]][["Pr(>F)"]][1] < 0.05) {
  # Perform Tukey's HSD test for Plant Height
  tukey_height <- HSD.test(anova_height, "Dose", group = TRUE)
  
  # Assign Tukey's letters based on groups
  for (i in 1:nrow(height_summary)) {
    group <- height_summary$Dose[i]
    letters_height[i] <- tukey_height$groups[as.character(group), "groups"]
  }
}

# Add Tukey letters to the height_summary
height_summary$letters_height <- letters_height

# Plot Plant Height with Tukey letters
ggplot(height_summary, aes(x = as.factor(Dose), y = Mean_Height, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Height - SE_Height, ymax = Mean_Height + SE_Height), width = 0.2, position = position_dodge(0.8)) +
  labs(title = "Plant Height by Dose, Fertilizer Type, and Plant Type", x = "Dose", y = "Plant Height (cm)") +
  theme_minimal() +
  facet_wrap(~ Plant.type) +  # Facet by Plant Type
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(y = Mean_Height + SE_Height + 0.5, label = letters_height), position = position_dodge(0.8), vjust = -0.5) +
  scale_fill_manual(values = c("UF" = "blue", "MF" = "red", "None" = "grey"))  # Custom colors for fertilizer types
data(plant_traits)
head(plant_traits)







# Load necessary libraries
library(ggplot2)
library(dplyr)

# Set the working directory if required (adjust path accordingly)
setwd("C:\\Users\\90958427\\OneDrive - Western Sydney University\\PolyTunnelS33_ExperimeNT1_dose\\Modified Data File")

# Load the dataset
Plant_traits <- read.csv("Biomass_height_stem_data_exp1.csv")

# Inspect the data
head(Plant_traits)

# Ensure that Dose is treated as a factor for analysis
Plant_traits$Dose <- factor(Plant_traits$Dose..N.kg.ha.)

# Visualizing C:N ratio under different nitrogen doses by Plant type
ggplot(Plant_traits, aes(x = Dose, y = Leaf.C.N..ratio., fill = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "C:N Ratio in Leaves by Nitrogen Dose and Plant Type", 
       x = "Nitrogen Dose (kg/ha)", 
       y = "Leaf C:N Ratio") +
  theme_minimal()

# Perform linear regression analysis between Nitrogen Dose and C:N Ratio
m1_cn_ratio <- lm(Leaf.C.N..ratio. ~ Dose * Plant.type, data = Plant_traits)

# Summary of the regression model
summary(m1_cn_ratio)

# Checking residuals for linearity and normality
residualPlot(m1_cn_ratio)
qqPlot(m1_cn_ratio)

# ANOVA test for significance of effects
Anova(m1_cn_ratio)

# Perform Tukey HSD Post-hoc test to check pairwise differences between doses and plant types
pairwise_comparisons_cn_ratio <- emmeans(m1_cn_ratio, ~ Dose | Plant.type)
cld_cn_ratio <- cld(pairwise_comparisons_cn_ratio, Letters = letters, adjust = "tukey")

# Convert compact letter display results to a data frame
cld_cn_ratio_df <- as.data.frame(cld_cn_ratio)

# Merge the Tukey letters with summary data (means and standard errors)
summary_data <- merge(summary, cld_cn_ratio_df[, c("Dose", "Plant.type", ".group")], by = c("Dose", "Plant.type"))

# Visualize C:N ratio by Nitrogen Dose and Plant Type with Means, Standard Errors, and Tukey Letters
ggplot(summary_data, aes(x = Dose, y = mean_cn_ratio, fill = Dose)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = mean_cn_ratio - se_cn_ratio, ymax = mean_cn_ratio + se_cn_ratio), 
                width = 0.2, position = position_dodge(0.9)) +
  facet_wrap(~Plant.type) +
  geom_text(aes(label = .group, y = mean_cn_ratio + se_cn_ratio + 0.05), position = position_dodge(0.9), vjust = -0.5) +
  labs(title = "C:N Ratio in Leaves by Nitrogen Dose and Plant Type with Tukey Letters", 
       x = "Nitrogen Dose (kg/ha)", 
       y = "Mean Leaf C:N Ratio") +
  theme_minimal()













# Load necessary libraries
library(ggplot2)
library(dplyr)
library(emmeans)
library(multcompView)

# Load the dataset
Plant_traits <- read.csv("Biomass_height_stem_data_exp1.csv")

# Ensure that Dose is treated as a factor for analysis
Plant_traits$Dose <- factor(Plant_traits$Dose..N.kg.ha.)

# Calculate mean and standard error for each group (Dose and Plant type)
summary_data <- Plant_traits %>%
  group_by(Dose, Plant.type) %>%
  summarise(
    mean_cn_ratio = mean(Leaf.C.N..ratio., na.rm = TRUE),
    se_cn_ratio = sd(Leaf.C.N..ratio., na.rm = TRUE) / sqrt(n())
  )

# Perform linear regression analysis between Nitrogen Dose and C:N Ratio
m1_cn_ratio <- lm(Leaf.C.N..ratio. ~ Dose * Plant.type, data = Plant_traits)

# Perform Tukey HSD Post-hoc test to check pairwise differences between doses and plant types
pairwise_comparisons_cn_ratio <- emmeans(m1_cn_ratio, ~ Dose | Plant.type)

# Since the Tukey adjustment isn't appropriate for multiple groups, sidak was used
cld_cn_ratio <- cld(pairwise_comparisons_cn_ratio, Letters = letters, adjust = "sidak")

# Convert compact letter display results to a data frame
cld_cn_ratio_df <- as.data.frame(cld_cn_ratio)

# Merge the Tukey letters with summary data (means and standard errors)
summary_data <- merge(summary_data, cld_cn_ratio_df[, c("Dose", "Plant.type", ".group")], by = c("Dose", "Plant.type"))

# Visualize C:N ratio by Nitrogen Dose and Plant Type with Means, Standard Errors, and Tukey Letters
ggplot(summary_data, aes(x = Dose, y = mean_cn_ratio, fill = Dose)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = mean_cn_ratio - se_cn_ratio, ymax = mean_cn_ratio + se_cn_ratio), 
                width = 0.2, position = position_dodge(0.9)) +
  facet_wrap(~Plant.type) +
  geom_text(aes(label = .group, y = mean_cn_ratio + se_cn_ratio + 0.05), position = position_dodge(0.9), vjust = -0.5) +
  labs(title = "C:N Ratio in Leaves by Nitrogen Dose and Plant Type with Tukey Letters", 
       x = "Nitrogen Dose (kg/ha)", 
       y = "Mean Leaf C:N Ratio") +
scale_fill_manual(values = c("grey20", "grey50", "grey80")) +
  theme_minimal(base_size = 15) +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = "grey"))

# Perform linear regression analysis between Nitrogen Dose and C:N Ratio
m1_cn_ratio <- lm(Leaf.C.N..ratio. ~ Dose * Plant.type, data = Plant_traits)

# Summary of the regression model
summary(m1_cn_ratio)

# Checking residuals for linearity and normality
residualPlot(m1_cn_ratio)
qqPlot(m1_cn_ratio)

# ANOVA test for significance of effects
Anova(m1_cn_ratio)


# Interpretation: Look for the effects of nitrogen doses and plant type on C:N ratios.
# If high nitrogen doses result in lower C:N ratios, it could indicate excess nitrogen relative to carbon.

