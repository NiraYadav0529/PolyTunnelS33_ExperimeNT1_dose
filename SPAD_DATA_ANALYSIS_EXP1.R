# Load necessary libraries
library(dplyr)
library(ggplot2)
library(emmeans)
library(multcompView)

# Load SPAD data (assuming the CSV file is already loaded as 'Soil_Nutrients')
colnames(Soil_Nutrients)

# Ensure necessary columns are treated as factors
Soil_Nutrients <- Soil_Nutrients %>%
  mutate(
    Dose = factor(Dose..N.kg.ha., levels = c("0", "100", "200")),  # Convert Dose column to factor
    Fertilizer_Type = factor(Fertilizer.type, levels = c('None', 'MF', 'UF')),  # Convert Fertilizer.type to Fertilizer_Type
    Plant.type = case_match(Plant.type, 'L' ~ 'Lucerne', 'P' ~ 'Phalaris'),  # Rename levels for labeling facets
    Plant.type = factor(Plant.type)  # Ensure Plant.type is a factor
  )

# Check structure
Soil_Nutrients %>% 
  group_by(Plant.type, Fertilizer_Type, Dose) %>% 
  summarise(n())

# Calculate mean and standard errors for SPAD data
spad_summary <- Soil_Nutrients %>%
  group_by(Dose, Plant.type, Fertilizer_Type) %>%
  summarise(
    SPAD_mean = mean(SPAD.Data..nmol.ch.mg.fresh.weight., na.rm = TRUE),
    SPAD_se = sd(SPAD.Data..nmol.ch.mg.fresh.weight., na.rm = TRUE) / sqrt(n())
  ) %>% ungroup()

### SPAD Data Analysis ###
# Model for SPAD data
m1_spad <- lm(SPAD.Data..nmol.ch.mg.fresh.weight. ~ Dose * Fertilizer_Type * Plant.type 
              - Dose:Fertilizer_Type:Plant.type, data = Soil_Nutrients)
summary(m1_spad)

# Check residuals for SPAD data
residualPlot(m1_spad)
qqPlot(m1_spad)

# Test significance of the model effects for SPAD data
Anova(m1_spad, type = "II")

# Perform multiple comparison test (Tukey HSD) for SPAD data
pairwise_comparisons_spad <- emmeans(m1_spad, ~ Dose + Fertilizer_Type | Plant.type)
letters_spad <- cld(pairwise_comparisons_spad, Letters = letters) %>%
  mutate(.group = stringr::str_trim(.group)) %>%
  as.data.frame()

# Merge the letters for SPAD data with spad_summary using Dose and Plant.type
spad_summary <- spad_summary %>%
  left_join(letters_spad, by = c("Dose", "Fertilizer_Type", "Plant.type"))

# SPAD PLOT with bold text adjustments
ggplot(spad_summary, aes(x = Dose, y = SPAD_mean, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), 
           colour = "black", show.legend = TRUE) +
  geom_errorbar(aes(ymin = SPAD_mean - SPAD_se, ymax = SPAD_mean + SPAD_se), 
                width = 0.2, position = position_dodge2(width = 0.9)) +
  labs(title = "SPAD (Chlorophyll Content) 30 days after fertilization", 
       x = "Dose (N kg/ha)", 
       y = "SPAD (nmol chlorophyll/mg fresh weight)", 
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
  
  # Add compact letter display (CLD) text on top of bars
  geom_text(aes(y = SPAD_mean + SPAD_se + 0.5, label = .group), 
            position = position_dodge2(0.9, preserve = 'single')) +
  
  # Remove background outlines from the plot
  theme(panel.border = element_blank(), 
        panel.grid.major = element_line(color = "grey"), 
        panel.grid.minor = element_blank())






# SPAD PLOT with bold text adjustments and correct bar width for Dose 0
ggplot(spad_summary, aes(x = Dose, y = SPAD_mean, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.8), 
           colour = "black", show.legend = TRUE, 
           aes(width = ifelse(Dose == 0, 0.45, 0.7))) +  # Adjust width for Dose 0
  geom_errorbar(aes(ymin = SPAD_mean - SPAD_se, ymax = SPAD_mean + SPAD_se), 
                width = 0.2, position = position_dodge(width = 0.8), color = "black") +  # Align error bars
  labs(title = "SPAD (Chlorophyll Content) 30 days after fertilization", 
       x = "Dose (N kg/ha)", 
       y = "SPAD (nmol chlorophyll/mg fresh weight)", 
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
  
  # Add compact letter display (CLD) text on top of bars
  geom_text(aes(y = SPAD_mean + SPAD_se + 0.5, label = .group), 
            position = position_dodge(width = 0.8)) +  # Adjust text position
  # Remove background outlines from the plot
  theme(panel.border = element_blank(), 
        panel.grid.major = element_line(color = "grey"), 
        panel.grid.minor = element_blank())
