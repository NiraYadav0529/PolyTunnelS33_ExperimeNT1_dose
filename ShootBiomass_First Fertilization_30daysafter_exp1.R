# Load necessary libraries
library(ggplot2)
library(dplyr)
library(car)
library(emmeans)
library(multcomp)

# Load the dataset
Plant_traits <- read.csv("Biomass_height_stem_data_exp1.csv")
colnames(Plant_traits)


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
    Mean_Shoot = mean(Fresh_biomass_AF_30days.gm., na.rm = TRUE),
    SE_Shoot = sd(Fresh_biomass_AF_30days.gm., na.rm = TRUE) / sqrt(n()),
     ) %>% ungroup()

### SHOOT BIOMASS ANALYSIS ###
# Model for shoot biomass
m1_shoot <- lm(Fresh_biomass_AF_30days.gm. ~ Dose * Fertilizer_Type * Plant.type 
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
  labs(title = "Shoot Biomass 30 days after fertilization", x = "Dose (N kg/ha)", 
       y = "Shoot Biomass (g, +/- SE)", fill = 'Fertilizer') +
  facet_wrap(~ Plant.type) +
  scale_fill_manual(values = c("UF" = "black", "MF" = "white", "None" = "grey")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(y = Mean_Shoot + SE_Shoot + 3, label = .group), 
            position = position_dodge2(0.9, preserve='single'))















# Load necessary libraries
library(dplyr)
library(ggplot2)
library(emmeans)
library(multcompView)

# Load the dataset (assuming you have already loaded the CSV file)
colnames(Plant_traits)

# Ensure necessary columns are treated as factors
Plant_traits <- Plant_traits %>%
  mutate(
    Dose = factor(Dose..N.kg.ha., levels = c("0", "100", "200")),  # Convert Dose column to factor
    Fertilizer_Type = factor(Fertilizer.type, levels = c('None', 'MF', 'UF')),  # Convert Fertilizer.type to Fertilizer_Type
    Plant.type = case_match(Plant.type, 'L' ~ 'Lucerne', 'P' ~ 'Phalaris'),  # Rename levels for labeling facets
    Plant.type = factor(Plant.type)  # Ensure Plant.type is a factor
  )

# Check the structure
Plant_traits %>% 
  group_by(Plant.type, Fertilizer_Type, Dose) %>% 
  summarise(n())

# Calculate mean and standard errors for shoot biomass
biomass_summary <- Plant_traits %>%
  group_by(Dose, Plant.type, Fertilizer_Type) %>%
  summarise(
    Mean_Shoot = mean(Fresh_biomass_AF_30days.gm., na.rm = TRUE),
    SE_Shoot = sd(Fresh_biomass_AF_30days.gm., na.rm = TRUE) / sqrt(n())
  ) %>% ungroup()

### SHOOT BIOMASS ANALYSIS ###
# Model for shoot biomass
m1_shoot <- lm(Fresh_biomass_AF_30days.gm. ~ Dose * Fertilizer_Type * Plant.type 
               - Dose:Fertilizer_Type:Plant.type, data = Plant_traits)
summary(m1_shoot)

# Check residuals for shoot biomass
residualPlot(m1_shoot)
qqPlot(m1_shoot)

# Test significance of the model effects for shoot biomass
Anova(m1_shoot, type = "II")

# Perform multiple comparison test (Tukey HSD) for shoot biomass
pairwise_comparisons_shoot <- emmeans(m1_shoot, ~ Dose + Fertilizer_Type | Plant.type)
letters_shoot <- cld(pairwise_comparisons_shoot, Letters = letters) %>%
  mutate(.group = stringr::str_trim(.group)) %>%
  as.data.frame()

# Merge the letters for shoot biomass with biomass_summary using Dose and Plant.type
biomass_summary_shoot <- biomass_summary %>%
  left_join(letters_shoot, by = c("Dose", "Fertilizer_Type", "Plant.type"))

# SHOOT BIOMASS PLOT with y-axis limit set to 50
ggplot(biomass_summary_shoot, aes(x = Dose, y = Mean_Shoot, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), 
           colour = "black") +
  geom_errorbar(aes(ymin = Mean_Shoot - SE_Shoot, ymax = Mean_Shoot + SE_Shoot), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  labs(title = "Shoot Biomass 30 days after fertilization", 
       x = "Dose (N kg/ha)", 
       y = "Shoot Biomass (g, +/- SE)", 
       fill = 'Fertilizer') +
  facet_wrap(~ Plant.type) +
  scale_fill_manual(values = c("UF" = "black", "MF" = "white", "None" = "grey")) +
  theme_minimal() +
  
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























# Filter data to include only 'Lucerne'
lucerne_data <- biomass_summary_shoot %>%
  filter(Plant.type == "Lucerne")

# SHOOT BIOMASS PLOT for Lucerne only with y-axis limit set to 50
ggplot(lucerne_data, aes(x = Dose, y = Mean_Shoot, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), 
           colour = "black") +
  geom_errorbar(aes(ymin = Mean_Shoot - SE_Shoot, ymax = Mean_Shoot + SE_Shoot), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  labs(
    title = "Shoot Biomass 30 days after fertilization (Lucerne)", 
    x = "Dose (N kg/ha)", 
    y = "Shoot Biomass (g, +/- SE)", 
    fill = 'Fertilizer'
  ) +
  scale_fill_manual(values = c("UF" = "black", "MF" = "white", "None" = "grey")) +
  theme_minimal() +
  
  # Adjusting the theme for bold text and making it black
  theme(
    axis.title.x = element_text(face = "bold", color = "black", size = 14),
    axis.title.y = element_text(face = "bold", color = "black", size = 14),
    axis.text.x = element_text(face = "bold", color = "black", size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(face = "bold", color = "black", size = 12),
    plot.title = element_text(face = "bold", color = "black", size = 16, hjust = 0.5),
    legend.title = element_text(face = "bold", color = "black", size = 12),
    legend.text = element_text(face = "bold", color = "black", size = 12)
  ) +
  
  # Set y-axis limit to 50
  ylim(0, 50) +
  
  # Add compact letter display (CLD) text on top of bars
  geom_text(
    aes(y = Mean_Shoot + SE_Shoot + 3, label = .group), 
    position = position_dodge2(width = 0.9, preserve = 'single')
  )
