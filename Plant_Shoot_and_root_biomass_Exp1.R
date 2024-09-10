# Load necessary libraries
library(ggplot2)
library(dplyr)
library(car)
library(emmeans)
library(multcomp)

# Set working directory  ## you don't need to do this step when using an RStudio project. Plus the working directory should be the main directory, not the subfolder
# setwd("C:\\Users\\90958427\\OneDrive - Western Sydney University\\PolyTunnelS33_ExperimeNT1_dose\\Modified Data File")
list.files("Modified Data File")

# Load the dataset
Plant_traits <- read.csv("Modified Data File/Biomass_height_stem_data_exp1.csv")

# Ensure necessary columns are treated as factors
Plant_traits <- Plant_traits %>%
  mutate(
    Dose = factor(Dose..N.kg.ha., levels = c("0", "100", "200")),  # Convert Dose column to factor
    Fertilizer_Type = factor(Fertilizer.type, levels = c('None', 'MF', 'UF')),                     # Convert Fertilizer.type to Fertilizer_Type
    Plant.type = factor(Plant.type)                                # Ensure Plant.type is a factor
  )
Plant_traits %>% 
  group_by(Plant.type, Fertilizer_Type, Dose) %>% 
  summarise(n())

# Calculate mean and standard errors for shoot and root biomass
biomass_summary <- Plant_traits %>%
  group_by(Dose, Plant.type, Fertilizer_Type) %>%
  summarise(
    Mean_Shoot = mean(Fresh.Biomass_harvestday..gm., na.rm = TRUE),
    SE_Shoot = sd(Fresh.Biomass_harvestday..gm., na.rm = TRUE) / sqrt(n()),
    Mean_Root = mean(Root.Biomass_harvestday.gm., na.rm = TRUE),
    SE_Root = sd(Root.Biomass_harvestday.gm., na.rm = TRUE) / sqrt(n())
  ) %>% ungroup()

### SHOOT BIOMASS ANALYSIS ###
# Model for shoot biomass
m1_shoot <- lm(Fresh.Biomass_harvestday..gm. ~ Dose * Fertilizer_Type * Plant.type 
               - Dose:Fertilizer_Type:Plant.type, data = Plant_traits)
summary(m1_shoot)

# Check residuals for shoot biomass
residualPlot(m1_shoot)
qqPlot(m1_shoot)

# Test significance of the model effects for shoot biomass
Anova(m1_shoot, type = "II")

# Perform multiple comparison test (Tukey HSD) for shoot biomass
pairwise_comparisons_shoot <- emmeans(m1_shoot, ~ Dose + Fertilizer_Type | Plant.type)
letters_shoot <- cld(pairwise_comparisons_shoot, Letters=letters) %>%
  mutate(.group = stringr::str_trim(.group)) %>% 
  as.data.frame()

# Merge the letters for shoot biomass with biomass_summary using Dose and Plant.type
biomass_summary_shoot <- biomass_summary %>%
  left_join(letters_shoot, by = c("Dose", "Fertilizer_Type", "Plant.type"))

# SHOOT BIOMASS PLOT
ggplot(biomass_summary_shoot, aes(x = Dose, y = Mean_Shoot, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Shoot - SE_Shoot, ymax = Mean_Shoot + SE_Shoot), 
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Shoot Biomass by Dose and Fertilizer Type", x = "Dose (N kg/ha)", 
       y = "Shoot Biomass (g, +/- SE)", fill = 'Fertilizer') +
  facet_wrap(~ Plant.type) +
  scale_fill_manual(values = c("UF" = "red", "MF" = "blue", "None" = "grey")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(y = Mean_Shoot + SE_Shoot + 3, label = .group), 
            position = position_dodge(0.9))

### ROOT BIOMASS ANALYSIS ###
# Model for root biomass
m1_root <- lm(Root.Biomass_harvestday.gm. ~ Dose * Fertilizer_Type * Plant.type, data = Plant_traits)
summary(m1_root)

# Check residuals for root biomass
residualPlot(m1_root)
qqPlot(m1_root)

# Test significance of the model effects for root biomass
Anova(m1_root, type = "II")

# Perform multiple comparison test (Tukey HSD) for root biomass
pairwise_comparisons_root <- emmeans(m1_root, ~ Dose | Plant.type)
letters_root <- cld(pairwise_comparisons_root) %>%
  as.data.frame()

# Merge the letters for root biomass with biomass_summary using Dose and Plant.type
biomass_summary_root <- biomass_summary %>%
  left_join(letters_root, by = c("Dose", "Plant.type"))

# ROOT BIOMASS PLOT
ggplot(biomass_summary_root, aes(x = Dose, y = Mean_Root, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), colour = "black", alpha = 0.5) +
  geom_errorbar(aes(ymin = Mean_Root - SE_Root, ymax = Mean_Root + SE_Root), 
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Root Biomass by Dose and Fertilizer Type", x = "Dose (N kg/ha)", y = "Root Biomass (gm)") +
  facet_wrap(~ Plant.type) +
  scale_fill_manual(values = c("UF" = "red", "MF" = "blue", "C" = "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(y = Mean_Root + SE_Root + 0.5, label = .group), 
            position = position_dodge(0.9), vjust = -0.5)
