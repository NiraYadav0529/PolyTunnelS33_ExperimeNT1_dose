# Load necessary libraries
library(ggplot2)
library(dplyr)
library(agricolae)
library(tidyverse)
library(car)
library(emmeans)
library(ggplot2)
Plant_traits <-read.csv("Biomass_height_stem_data_exp1.csv")
# Load the dataset
head(Plant_traits)
colnames(Plant_traits)
# Subset the dataset to include relevant columns and convert variables to factors
biomass_data <- Plant_traits %>%
  select(Dose..N.kg.ha., Plant.type, Fertilizer.type, Fresh.Biomass_harvestday..gm., Root.Biomass_harvestday.gm.) %>%
  mutate(
    Dose = factor(Dose..N.kg.ha., levels = c("0", "100", "200")),   # Convert Dose to factor
    Fertilizer_Type = factor(Fertilizer.type),                      # Convert Fertilizer type to factor
    Plant.type = factor(Plant.type)                                 # Convert Plant type to factor
  )

# Calculate mean and standard errors for shoot and root biomass
biomass_summary <- biomass_data %>%
  group_by(Dose, Plant.type, Fertilizer_Type) %>%
  summarise(
    Mean_Shoot = mean(Fresh.Biomass_harvestday..gm., na.rm = TRUE),
    SE_Shoot = sd(Fresh.Biomass_harvestday..gm., na.rm = TRUE) / sqrt(n()),
    Mean_Root = mean(Root.Biomass_harvestday.gm., na.rm = TRUE),
    SE_Root = sd(Root.Biomass_harvestday.gm., na.rm = TRUE) / sqrt(n())
  )

# Perform ANOVA for Shoot and Root Biomass
anova_shoot <- aov(Fresh.Biomass_harvestday..gm. ~ Dose * Fertilizer_Type * Plant.type, data = biomass_data)
anova_root <- aov(Root.Biomass_harvestday.gm. ~ Dose * Fertilizer_Type * Plant.type, data = biomass_data)

summary(anova_shoot)
summary(anova_root)





# Perform Tukey's HSD test for shoot and root biomass
letters_shoot <- rep(NA, nrow(biomass_summary))
letters_root <- rep(NA, nrow(biomass_summary))

# Tukey's HSD for Shoot Biomass if ANOVA is significant
if (summary(anova_shoot)[[1]][["Pr(>F)"]][1] < 0.05) {
  tukey_shoot <- HSD.test(anova_shoot, "Dose", group = TRUE)
  for (i in 1:nrow(biomass_summary)) {
    group <- biomass_summary$Dose[i]
    letters_shoot[i] <- tukey_shoot$groups[as.character(group), "groups"]
  }
}

# Tukey's HSD for Root Biomass if ANOVA is significant
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

# Convert Dose to factor for proper ordering in plots
biomass_summary$Dose <- factor(biomass_summary$Dose, levels = c("0", "100", "200"))

### Shoot Biomass Plot
ggplot(biomass_summary, aes(x = Dose, y = Mean_Shoot, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Shoot - SE_Shoot, ymax = Mean_Shoot + SE_Shoot), 
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Shoot Biomass by Dose and Fertilizer Type", x = "Dose", y = "Shoot Biomass (gm)") +
  facet_wrap(~ Plant.type) +
  scale_fill_manual(values = c("UF" = "red", "MF" = "blue", "C" = "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(y = Mean_Shoot + SE_Shoot + 0.5, label = letters_shoot), position = position_dodge(0.9), vjust = -0.5)

### Root Biomass Plot
ggplot(biomass_summary, aes(x = Dose, y = Mean_Root, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), colour = "black", alpha = 0.5) +
  geom_errorbar(aes(ymin = Mean_Root - SE_Root, ymax = Mean_Root + SE_Root), 
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Root Biomass by Dose and Fertilizer Type", x = "Dose", y = "Root Biomass (gm)") +
  facet_wrap(~ Plant.type) +
  scale_fill_manual(values = c("UF" = "red", "MF" = "blue", "C" = "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(y = Mean_Root + SE_Root + 0.5, label = letters_root), position = position_dodge(0.9), vjust = -0.5)


# Root Biomass Plot
ggplot(biomass_summary, aes(x = Dose, y = Mean_Root, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), 
           colour = "black") +
  geom_errorbar(aes(ymin = Mean_Root - SE_Root, ymax = Mean_Root + SE_Root), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  labs(title = "Root Biomass at Harvest Day", 
       x = "Dose (N kg/ha)", 
       y = "Root Biomass (g, +/- SE)", 
       fill = 'Fertilizer') +
  facet_wrap(~ Plant.type, 
             labeller = as_labeller(c("L" = "Lucerne", "P" = "Phalaris"), 
                                    default = label_value)) +
  scale_fill_manual(values = c("UF" = "black", "MF" = "white", "C" = "grey")) +  # Adjust colors as needed
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),  # Center the title
        strip.text = element_text(face = "bold")) +  # Bold facet labels
  geom_text(aes(y = Mean_Root + SE_Root + 3, label = letters_root), 
            position = position_dodge2(0.9, preserve = 'single'))





### Shoot Biomass Plot
ggplot(biomass_summary, aes(x = Dose, y = Mean_Shoot, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Shoot - SE_Shoot, ymax = Mean_Shoot + SE_Shoot), 
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Shoot Biomass by Dose and Fertilizer Type", x = "Dose (N kg/ha)", y = "Shoot Biomass (gm)") +
  facet_wrap(~ Plant.type) +
  scale_fill_manual(values = c("UF" = "red", "MF" = "blue", "C" = "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(y = Mean_Shoot + SE_Shoot + 0.5, label = letters_shoot), position = position_dodge(0.9), vjust = -0.5)

### Root Biomass Plot
ggplot(biomass_summary, aes(x = Dose, y = Mean_Root, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), colour = "black", alpha = 0.5) +
  geom_errorbar(aes(ymin = Mean_Root - SE_Root, ymax = Mean_Root + SE_Root), 
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Root Biomass by Dose and Fertilizer Type", x = "Dose (N kg/ha)", y = "Root Biomass (gm)") +
  facet_wrap(~ Plant.type) +
  scale_fill_manual(values = c("UF" = "red", "MF" = "blue", "C" = "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(y = Mean_Root + SE_Root + 0.5, label = letters_root), position = position_dodge(0.9), vjust = -0.5)











# Subset the dataset to include relevant columns for Root Length, Shoot Height, and Nodules Count
traits_data <- Plant_traits %>%
  select(Dose..N.kg.ha., Plant.type, Fertilizer.type, Root.Length..cm., Plant_Height_Harvestday.cm., Nodules.Count.no..) %>%
  mutate(
    Dose = factor(Dose..N.kg.ha., levels = c("0", "100", "200")),   # Convert Dose to factor
    Fertilizer_Type = factor(Fertilizer.type),                      # Convert Fertilizer type to factor
    Plant.type = factor(Plant.type)                                 # Convert Plant type to factor
  )

# Calculate mean and standard errors for root length, shoot height, and nodules count
traits_summary <- traits_data %>%
  group_by(Dose, Plant.type, Fertilizer_Type) %>%
  summarise(
    Mean_Root_Length = mean(Root.Length..cm., na.rm = TRUE),
    SE_Root_Length = sd(Root.Length..cm., na.rm = TRUE) / sqrt(n()),
    Mean_Shoot_Height = mean(Plant_Height_Harvestday.cm., na.rm = TRUE),
    SE_Shoot_Height = sd(Plant_Height_Harvestday.cm., na.rm = TRUE) / sqrt(n()),
    Mean_Nodules_Count = mean(Nodules.Count.no.., na.rm = TRUE),
    SE_Nodules_Count = sd(Nodules.Count.no.., na.rm = TRUE) / sqrt(n())
  )

# Perform ANOVA for Root Length, Shoot Height, and Nodules Count
anova_root_length <- aov(Root.Length..cm. ~ Dose * Fertilizer_Type * Plant.type, data = traits_data)
anova_shoot_height <- aov(Plant_Height_Harvestday.cm. ~ Dose * Fertilizer_Type * Plant.type, data = traits_data)
anova_nodules <- aov(Nodules.Count.no.. ~ Dose * Fertilizer_Type * Plant.type, data = traits_data)

# Perform Tukey's HSD test for root length, shoot height, and nodules count
letters_root_length <- rep(NA, nrow(traits_summary))
letters_shoot_height <- rep(NA, nrow(traits_summary))
letters_nodules <- rep(NA, nrow(traits_summary))

# Tukey's HSD for Root Length if ANOVA is significant
if (summary(anova_root_length)[[1]][["Pr(>F)"]][1] < 0.05) {
  tukey_root_length <- HSD.test(anova_root_length, "Dose", group = TRUE)
  for (i in 1:nrow(traits_summary)) {
    group <- traits_summary$Dose[i]
    letters_root_length[i] <- tukey_root_length$groups[as.character(group), "groups"]
  }
}

# Tukey's HSD for Shoot Height if ANOVA is significant
if (summary(anova_shoot_height)[[1]][["Pr(>F)"]][1] < 0.05) {
  tukey_shoot_height <- HSD.test(anova_shoot_height, "Dose", group = TRUE)
  for (i in 1:nrow(traits_summary)) {
    group <- traits_summary$Dose[i]
    letters_shoot_height[i] <- tukey_shoot_height$groups[as.character(group), "groups"]
  }
}

# Tukey's HSD for Nodules Count if ANOVA is significant
if (summary(anova_nodules)[[1]][["Pr(>F)"]][1] < 0.05) {
  tukey_nodules <- HSD.test(anova_nodules, "Dose", group = TRUE)
  for (i in 1:nrow(traits_summary)) {
    group <- traits_summary$Dose[i]
    letters_nodules[i] <- tukey_nodules$groups[as.character(group), "groups"]
  }
}

# Add Tukey letters to the traits_summary
traits_summary$letters_root_length <- letters_root_length
traits_summary$letters_shoot_height <- letters_shoot_height
traits_summary$letters_nodules <- letters_nodules

# Convert Dose to factor for proper ordering in plots
traits_summary$Dose <- factor(traits_summary$Dose, levels = c("0", "100", "200"))

### Root Length Plot
ggplot(traits_summary, aes(x = Dose, y = Mean_Root_Length, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Root_Length - SE_Root_Length, ymax = Mean_Root_Length + SE_Root_Length), 
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Root Length by Dose and Fertilizer Type", x = "Dose", y = "Root Length (cm)") +
  facet_wrap(~ Plant.type) +
  scale_fill_manual(values = c("UF" = "red", "MF" = "blue", "C" = "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(y = Mean_Root_Length + SE_Root_Length + 0.5, label = letters_root_length), position = position_dodge(0.9), vjust = -0.5)

### Shoot Height at Harvest Day Plot
ggplot(traits_summary, aes(x = Dose, y = Mean_Shoot_Height, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Shoot_Height - SE_Shoot_Height, ymax = Mean_Shoot_Height + SE_Shoot_Height), 
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Shoot Height at Harvest Day by Dose and Fertilizer Type", x = "Dose", y = "Shoot Height (cm)") +
  facet_wrap(~ Plant.type) +
  scale_fill_manual(values = c("UF" = "red", "MF" = "blue", "C" = "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(y = Mean_Shoot_Height + SE_Shoot_Height + 0.5, label = letters_shoot_height), position = position_dodge(0.9), vjust = -0.5)

### Nodules Count Plot
ggplot(traits_summary, aes(x = Dose, y = Mean_Nodules_Count, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Nodules_Count - SE_Nodules_Count, ymax = Mean_Nodules_Count + SE_Nodules_Count), 
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Nodules Count by Dose and Fertilizer Type", x = "Dose", y = "Nodules Count") +
  facet_wrap(~ Plant.type) +
  scale_fill_manual(values = c("UF" = "red", "MF" = "blue", "C" = "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(y = Mean_Nodules_Count + SE_Nodules_Count + 0.5, label = letters_nodules), position = position_dodge(0.9), vjust = -0.5)


















