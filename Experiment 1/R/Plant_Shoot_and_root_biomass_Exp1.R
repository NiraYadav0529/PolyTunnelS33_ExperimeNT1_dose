# Load necessary libraries
library(ggplot2)
library(dplyr)
library(car)
library(emmeans)
library(multcomp)

# Load the dataset
# Plant_traits <- read.csv("Biomass_height_stem_data_exp1.csv")
colnames(Plant_traits)
# Ensure necessary columns are treated as factors
Plant_traits <- Plant_traits %>%
  mutate(
    Dose = factor(Dose..N.kg.ha., levels = c("0", "100", "200")),  # Convert Dose column to factor
    Fertilizer_Type = factor(Fertilizer.type, levels = c('None', 'MF', 'UF')),                     # Convert Fertilizer.type to Fertilizer_Type
    Plant.type = case_match(Plant.type, 'L'~'Lucerne', 'P'~'Phalaris'), # rename levels for labelling facets
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
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve='single'), 
           colour = "black") +
  geom_errorbar(aes(ymin = Mean_Shoot - SE_Shoot, ymax = Mean_Shoot + SE_Shoot), 
                width=0.2, position = position_dodge(width=0.9)) +
  labs(title = "Shoot Biomass 90 days after fertilization(Harvest Day)", x = "Dose (N kg/ha)", 
       y = "Shoot Biomass (g, +/- SE)", fill = 'Fertilizer') +
  facet_wrap(~ Plant.type) +
  scale_fill_manual(values = c("UF" = "black", "MF" = "white", "None" = "grey")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(y = Mean_Shoot + SE_Shoot + 3, label = .group), 
            position = position_dodge2(0.9, preserve='single'))





# Ensure necessary columns are treated as factors (added by Niraj for bold black color)
Plant_traits <- Plant_traits %>%
  mutate(
    Dose = factor(Dose..N.kg.ha., levels = c("0", "100", "200")),  # Convert Dose column to factor
    Fertilizer_Type = factor(Fertilizer.type, levels = c('None', 'MF', 'UF')),                     # Convert Fertilizer.type to Fertilizer_Type
    Plant.type = case_match(Plant.type, 'L' ~ 'Lucerne', 'P' ~ 'Phalaris'), # Rename levels for labelling facets
    Plant.type = factor(Plant.type)                                # Ensure Plant.type is a factor
  )

# Group and summarise
Plant_traits %>% 
  group_by(Plant.type, Fertilizer_Type, Dose) %>% 
  summarise(n()) 

# SHOOT BIOMASS PLOT with bold, black facet labels
ggplot(biomass_summary_shoot, aes(x = Dose, y = Mean_Shoot, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), 
           colour = "black") +
  geom_errorbar(aes(ymin = Mean_Shoot - SE_Shoot, ymax = Mean_Shoot + SE_Shoot), 
                width = 0.2, position = position_dodge2(width = 0.9)) +
  labs(title = "Shoot Biomass 90 days after fertilization (Harvest Day)", 
       x = "Dose (N kg/ha)", 
       y = "Shoot Biomass (g, +/- SE)", 
       fill = 'Fertilizer') +
  facet_wrap(~ Plant.type) +
  scale_fill_manual(values = c("UF" = "black", "MF" = "white", "None" = "grey")) +
  theme_minimal() +
  geom_text(aes(y = Mean_Shoot + SE_Shoot + 3, label = .group), 
            position = position_dodge2(0.9, preserve = 'single')) +
  
  
  # Adjusting the theme for bold text and making it black
  theme(
    axis.title.x = element_text(face = "bold", color = "black", size = 14),
    axis.title.y = element_text(face = "bold", color = "black", size = 14),
    axis.text.x = element_text(face = "bold", color = "black", size = 12),
    axis.text.y = element_text(face = "bold", color = "black", size = 12),
    plot.title = element_text(face = "bold", color = "black", size = 16, hjust = 0.5),
    legend.title = element_text(face = "bold", color = "black", size = 12),
    legend.text = element_text(face = "bold", color = "black", size = 12),
    strip.text = element_text(face = "bold", color = "black", size = 14)
  ) +
  
  # Add y-axis limit to 50
  ylim(0, 50) +
  
  # Add compact letter display (CLD) text on top of bars
  geom_text(aes(y = Mean_Shoot + SE_Shoot + 3, label = .group), 
            position = position_dodge2(0.9, preserve = 'single'))  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Adjusting the theme for bold text and making it black
  theme(
    # Bold the axis titles and make them black
    axis.title.x = element_text(face = "bold", color = "black", size = 14),
    axis.title.y = element_text(face = "bold", color = "black", size = 14),
    
    # Bold the axis text and make it black
    axis.text.x = element_text(face = "bold", color = "black", size = 12),
    axis.text.y = element_text(face = "bold", color = "black", size = 12),
    
    # Bold the plot title and make it black
    plot.title = element_text(face = "bold", color = "black", size = 16, hjust = 0.5),
    
    # Bold the legend title and text, make them black
    legend.title = element_text(face = "bold", color = "black", size = 12),
    legend.text = element_text(face = "bold", color = "black", size = 12),
    
    # Bold and black facet labels (Lucerne, Phalaris)
    strip.text = element_text(face = "bold", color = "black", size = 14)
  )





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
pairwise_comparisons_root <- emmeans(m1_root, ~ Dose + Fertilizer_Type | Plant.type)
letters_root <- cld(pairwise_comparisons_root, Letters = letters) %>%
  mutate(.group = stringr::str_trim(.group)) %>% 
  as.data.frame()

# Merge the letters for root biomass with biomass_summary using Dose, Fertilizer_Type, and Plant.type
biomass_summary_root <- biomass_summary %>%
  left_join(letters_root, by = c("Dose", "Fertilizer_Type", "Plant.type"))

# ROOT BIOMASS PLOT WITH SIGNIFICANCE LETTERS
ggplot(biomass_summary_root, aes(x = Dose, y = Mean_Root, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), colour = "black", alpha = 0.5) +
  geom_errorbar(aes(ymin = Mean_Root - SE_Root, ymax = Mean_Root + SE_Root), 
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Root Biomass at Harvest Day", x = "Dose (N kg/ha)", y = "Root Biomass (g, +/- SE)", fill = 'Fertilizer') +
  facet_wrap(~ Plant.type) +
  scale_fill_manual(values = c("UF" = "black", "MF" = "white", "None" = "grey")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(y = Mean_Root + SE_Root + 3, label = .group), 
            position = position_dodge(0.9))

