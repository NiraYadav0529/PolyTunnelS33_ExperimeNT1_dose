# Load necessary libraries
library(dplyr)
library(ggplot2)
library(emmeans)
library(multcompView)
setwd("C:\\Users\\90958427\\OneDrive - Western Sydney University\\PolyTunnelS33_ExperimeNT1_dose\\Modified Data File")
Soil_Nutrients<- read.csv("Soil_Nutrients_Extraction_EXP1.csv")
head(Soil_Nutrients)
# Load SPAD data (assuming the CSV file is already loaded as 'Soil_Nutrients')
colnames(Soil_Nutrients)

# Load required libraries
library(dplyr)
library(ggplot2)
library(agricolae)

# Summarize SPAD Data
spad_summary <- Soil_Nutrients %>%
  mutate(
    Dose = factor(Dose..N.kg.ha., levels = c("0", "100", "200")),  # Convert Dose to factor
    Fertilizer_Type = factor(Fertilizer.type, levels = c("None", "MF", "UF")),  # Convert Fertilizer type to factor
    Treatment = interaction(Fertilizer_Type, Dose, sep = " ")  # Combine Fertilizer_Type and Dose
  ) %>%
  group_by(Treatment) %>%
  summarise(
    Mean_SPAD = mean(SPAD.Data..nmol.ch.mg.fresh.weight., na.rm = TRUE),
    SE_SPAD = sd(SPAD.Data..nmol.ch.mg.fresh.weight., na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

anova_spad <- aov(SPAD.Data..nmol.ch.mg.fresh.weight. ~ Fertilizer.type * Dose..N.kg.ha., data = Soil_Nutrients)
summary(anova_spad)
# Tukey HSD test for Fertilizer.type
tukey_fertilizer <- HSD.test(anova_spad, "Fertilizer.type", group = TRUE)
print(tukey_fertilizer$groups)

# Tukey HSD test for Dose..N.kg.ha.
tukey_dose <- HSD.test(anova_spad, "Dose..N.kg.ha.", group = TRUE)
print(tukey_dose$groups)
library(emmeans)
library(multcomp)
library(multcompView)
pairwise_spad <- emmeans(anova_spad, ~ Fertilizer.type * Dose..N.kg.ha.)
tukey_letters <- cld(pairwise_spad, Letters = letters, adjust = "tukey")

# Convert to data frame
letters_spad <- as.data.frame(tukey_letters) %>%
  select(Fertilizer.type, Dose..N.kg.ha., .group) %>%
  mutate(Treatment = paste(Fertilizer.type, Dose..N.kg.ha.)) %>%
  rename(Significance = .group)
print(letters_spad)
# Perform pairwise comparisons
pairwise_spad <- emmeans(anova_spad, ~ Fertilizer.type * Dose..N.kg.ha.)

# Add compact letter display (Tukey-adjusted)
tukey_letters <- cld(pairwise_spad, adjust = "tukey", Letters = letters)

# Check the results
print(tukey_letters)

# Convert to data frame
letters_spad <- as.data.frame(tukey_letters) %>%
  select(Fertilizer_Type, Dose, .group) %>%
  mutate(Treatment = paste(Fertilizer_Type, Dose)) %>%
  rename(Significance = .group)

# Inspect Tukey results
print(letters_spad)
str(tukey_letters)
head(tukey_letters)

letters_spad <- as.data.frame(tukey_letters) %>%
  dplyr::select(Fertilizer.type, Dose..N.kg.ha., .group) %>%  # Use exact column names
  dplyr::mutate(Treatment = paste(Fertilizer.type, Dose..N.kg.ha.)) %>%  # Combine treatment info
  dplyr::rename(Significance = .group)  # Rename .group to Significance
colnames(spad_summary)
spad_summary <- spad_summary %>%
  dplyr::left_join(letters_spad, by = "Treatment")  # Merge with Tukey letters

letters_spad <- letters_spad %>%
  dplyr::mutate(Treatment = paste(Fertilizer.type, Dose..N.kg.ha.))  # Create Treatment column
unique(spad_summary$Treatment)
unique(letters_spad$Treatment)

ggplot(spad_summary, aes(x = Treatment, y = Mean_SPAD, fill = Treatment)) +
  geom_bar(stat = "identity", colour = "black", width = 0.7) +
  geom_errorbar(aes(ymin = Mean_SPAD - SE_SPAD, ymax = Mean_SPAD + SE_SPAD), 
                width = 0.2, colour = "black") +
  geom_text(aes(y = Mean_SPAD + SE_SPAD + 1, label = Significance), 
            vjust = -0.5, size = 4) +
  labs(title = "SPAD Data Across Treatments at Harvest Day", 
       x = "Treatment", y = "SPAD (nmol chlorophyll/mg fresh weight)") +
  theme_minimal()
print(letters_spad)
spad_summary <- spad_summary %>%
  left_join(letters_spad, by = "Treatment")



# Check structure
Soil_Nutrients %>% 
  group_by(Plant.type, Fertilizer_Type, Dose) %>% 
  summarise(n())
ggplot(spad_summary, aes(x = Treatment, y = Mean_SPAD, fill = Treatment)) +
  geom_bar(stat = "identity", colour = "black", width = 0.7) +
  geom_errorbar(aes(ymin = Mean_SPAD - SE_SPAD, ymax = Mean_SPAD + SE_SPAD), 
                width = 0.2, colour = "black") +
  geom_text(aes(y = Mean_SPAD + SE_SPAD + 0.5, label = Significance), 
            vjust = -0.5, size = 5) +  # Add letters
  labs(title = "SPAD Data Across Treatments at Harvest Day", 
       x = "Treatment", y = "SPAD (nmol chlorophyll/mg fresh weight)") +
  scale_fill_manual(values = c("None 0" = "#FFC107", 
                               "MF 100" = "#4CAF50", "MF 200" = "#E91E63",
                               "UF 100" = "#03A9F4", "UF 200" = "#673AB7")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    legend.position = "none",
    panel.grid.major = element_line(colour = "grey80"),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(breaks = seq(0, max(spad_summary$Mean_SPAD + spad_summary$SE_SPAD, na.rm = TRUE), by = 5))



# Perform ANOVA
anova_spad <- aov(SPAD.Data..nmol.ch.mg.fresh.weight. ~ Fertilizer.type * Dose..N.kg.ha., data = Soil_Nutrients)
summary(anova_spad)

# Perform pairwise comparisons
library(emmeans)
pairwise_spad <- emmeans(anova_spad, ~ Fertilizer.type * Dose..N.kg.ha.)

library(multcompView)
tukey_letters <- cld(pairwise_spad, adjust = "tukey")
print(tukey_letters)
letters_spad <- as.data.frame(tukey_letters) %>%
  dplyr::select(Fertilizer.type, Dose..N.kg.ha., .group) %>%  # Adjust column names as necessary
  mutate(Treatment = paste(Fertilizer.type, Dose..N.kg.ha.)) %>%
  rename(Significance = .group)
spad_summary <- spad_summary %>%
  left_join(letters_spad, by = "Treatment")
ggplot(spad_summary, aes(x = Treatment, y = Mean_SPAD, fill = Treatment)) +
  geom_bar(stat = "identity", colour = "black", width = 0.7) +
  geom_errorbar(aes(ymin = Mean_SPAD - SE_SPAD, ymax = Mean_SPAD + SE_SPAD), 
                width = 0.2, colour = "black") +
  geom_text(aes(y = Mean_SPAD + SE_SPAD + 0.5, label = Significance), 
            vjust = -0.5, size = 4) +
  labs(title = "SPAD Data Across Treatments at Harvest Day", 
       x = "Treatment", y = "SPAD (nmol chlorophyll/mg fresh weight)") +
  scale_fill_manual(values = c("None 0" = "#FFC107", 
                               "MF 100" = "#4CAF50", "MF 200" = "#E91E63",
                               "UF 100" = "#03A9F4", "UF 200" = "#673AB7")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    legend.position = "none",
    panel.grid.major = element_line(colour = "grey80"),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(breaks = seq(0, max(spad_summary$Mean_SPAD + spad_summary$SE_SPAD, na.rm = TRUE), by = 5))






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
# Install packages if not already installed
install.packages("emmeans")       # For pairwise comparisons
install.packages("multcompView")  # For compact letter display (cld)

# Load libraries
library(emmeans)
library(multcompView)
library(ggplot2)

# Perform multiple comparison test (Tukey HSD) for SPAD data
pairwise_comparisons_spad <- emmeans(m1_spad, ~ Dose + Fertilizer_Type | Plant.type)
letters_spad <- cld2(pairwise_comparisons_spad, Letters = letters) %>%
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

library(dplyr) # For %>% and other data manipulation functions
library(tidyverse) # Includes dplyr, ggplot2, and more
library(ggplot2)
library(multcompView) # For compact letter display (cld)
letters_spad <- cld(pairwise_comparisons_spad) %>%
  mutate(.group = stringr::str_trim(.group)) %>%
  as.data.frame()



# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(emmeans)
library(multcompView)
library(agricolae) # For Tukey's HSD test
# Ensure necessary columns are treated as factors and clean labels
Soil_Nutrients <- Soil_Nutrients %>%
  mutate(
    Dose = factor(Dose..N.kg.ha., levels = c("0", "100", "200")),  # Convert Dose column to factor
    Fertilizer_Type = factor(Fertilizer.type, levels = c('None', 'MF', 'UF')),  # Convert Fertilizer.type
    Plant.type = case_when(
      Plant.type == "L" ~ "Lucerne",
      Plant.type == "P" ~ "Phalaris"
    ),  # Rename Plant.type levels
    Plant.type = factor(Plant.type)  # Ensure Plant.type is a factor
  )

# Check levels of Dose
levels(Soil_Nutrients$Dose)

# Check levels of Fertilizer_Type
levels(Soil_Nutrients$Fertilizer_Type)

# Check levels of Plant.type
levels(Soil_Nutrients$Plant.type)
# Summarize data to check for missing or single levels
Soil_Nutrients %>%
  group_by(Dose, Fertilizer_Type, Plant.type) %>%
  summarise(count = n(), .groups = "drop")
# Remove rows with missing or single levels
Soil_Nutrients <- Soil_Nutrients %>%
  filter(!is.na(Dose), !is.na(Fertilizer_Type), !is.na(Plant.type))
Soil_Nutrients <- Soil_Nutrients %>%
  mutate(
    Dose = droplevels(Dose),
    Fertilizer_Type = droplevels(Fertilizer_Type),
    Plant.type = droplevels(Plant.type)
  )
m1_spad <- lm(SPAD.Data..nmol.ch.mg.fresh.weight. ~ Dose * Fertilizer_Type * Plant.type, 
              data = Soil_Nutrients)
summary(m1_spad)

Soil_Nutrients %>%
  count(Dose, Fertilizer_Type, Plant.type)

# Fit a linear model for SPAD data
m1_spad <- lm(SPAD.Data..nmol.ch.mg.fresh.weight. ~ Dose * Fertilizer_Type * Plant.type, data = Soil_Nutrients)

# Check ANOVA results
anova_results <- Anova(m1_spad, type = "II") # Type II ANOVA for balanced design
print(anova_results)

# Post-hoc pairwise comparisons using emmeans
pairwise_comparisons_spad <- emmeans(m1_spad, ~ Dose + Fertilizer_Type | Plant.type)

# Generate compact letter display (Tukey's post-hoc letters)
letters_spad <- cld(pairwise_comparisons_spad, Letters = letters) %>%
  mutate(.group = stringr::str_trim(.group)) %>%
  as.data.frame()

# Merge Tukey's letters with the summary data
spad_summary <- spad_summary %>%
  left_join(letters_spad, by = c("Dose", "Fertilizer_Type", "Plant.type"))



# Calculate means and standard errors for SPAD data
spad_summary <- Soil_Nutrients %>%
  group_by(Dose, Plant.type, Fertilizer_Type) %>%
  summarise(
    SPAD_mean = mean(SPAD.Data..nmol.ch.mg.fresh.weight., na.rm = TRUE),
    SPAD_se = sd(SPAD.Data..nmol.ch.mg.fresh.weight., na.rm = TRUE) / sqrt(n())
  ) %>% 
  ungroup()










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
