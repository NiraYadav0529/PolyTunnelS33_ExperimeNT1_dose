colnames(Lucerne_exp2)

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(car)
library(emmeans)
library(multcompView)
library(stringr)

# Convert categorical variables to factors
Lucerne_exp2 <- Lucerne_exp2 %>%
  mutate(
    Dose = factor(Dose...N.kg.ha., levels = c("0", "10", "20", "30", "40", "50", "60")),
    Fertilizer_Type = factor(Fertilizer.type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split"))
  )

### SUMMARIZE DATA ###
# Dry_Biomass_shoot_4 summary
biomass_shoot_summary <- Lucerne_exp2 %>%
  group_by(Dose, Fertilizer_Type, Application_Method) %>%
  summarise(
    Mean_Shoot = mean(Dry_Biomass_shoot_4, na.rm = TRUE),
    SE_Shoot = sd(Dry_Biomass_shoot_4, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()

# Dry_Biomass_root_118days summary
biomass_root_summary <- Lucerne_exp2 %>%
  group_by(Dose, Fertilizer_Type, Application_Method) %>%
  summarise(
    Mean_Root = mean(Dry_Biomass_root_118days, na.rm = TRUE),
    SE_Root = sd(Dry_Biomass_root_118days, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()

### MODELING ###
# Linear model for shoot biomass
m_shoot <- lm(Dry_Biomass_shoot_4 ~ Dose * Fertilizer_Type * Application_Method, data = Lucerne_exp2)
summary(m_shoot)
anova_shoot <- Anova(m_shoot, type = "II")  # ANOVA table
print(anova_shoot)

# Tukey HSD test for Dry_Biomass_shoot_4
pairwise_shoot <- emmeans(m_shoot, ~ Dose + Fertilizer_Type | Application_Method)
letters_shoot <- cld(pairwise_shoot, Letters = letters) %>%
  mutate(.group = str_trim(.group)) %>%
  as.data.frame()

# Merge letters with biomass summary for shoot biomass
biomass_shoot_summary <- biomass_shoot_summary %>%
  left_join(letters_shoot, by = c("Dose", "Fertilizer_Type", "Application_Method"))

# Linear model for root biomass
m_root <- lm(Dry_Biomass_root_118days ~ Dose * Fertilizer_Type * Application_Method, data = Lucerne_exp2)
summary(m_root)
anova_root <- Anova(m_root, type = "II")  # ANOVA table
print(anova_root)

# Tukey HSD test for Dry_Biomass_root_118days
pairwise_root <- emmeans(m_root, ~ Dose + Fertilizer_Type | Application_Method)
letters_root <- cld(pairwise_root, Letters = letters) %>%
  mutate(.group = str_trim(.group)) %>%
  as.data.frame()

# Merge letters with biomass summary for root biomass
biomass_root_summary <- biomass_root_summary %>%
  left_join(letters_root, by = c("Dose", "Fertilizer_Type", "Application_Method"))

### PLOT: Dry_Biomass_shoot_4 ###
ggplot(biomass_shoot_summary, aes(x = Dose, y = Mean_Shoot, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), 
           colour = "black") +
  geom_errorbar(aes(ymin = Mean_Shoot - SE_Shoot, ymax = Mean_Shoot + SE_Shoot), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  labs(title = "Dry Biomass Shoot (Harvest Day)", 
       x = "Dose (N kg/ha)", 
       y = "Mean Dry Biomass Shoot (g, +/- SE)", 
       fill = 'Fertilizer') +
  facet_wrap(~ Application_Method) +
  
  # Black & white color scheme
  scale_fill_manual(values = c("UF" = "white", "MF" = "gray", "None" = "black")) +
  
  # Adjust theme
  theme_minimal() +
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
  
  # Add letter-wise comparison text
  geom_text(aes(y = Mean_Shoot + SE_Shoot + 0.5, label = .group), 
            position = position_dodge2(0.9, preserve = 'single'))


### PLOT: Dry_Biomass_root_118days ###
ggplot(biomass_root_summary, aes(x = Dose, y = Mean_Root, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), 
           colour = "black") +
  geom_errorbar(aes(ymin = Mean_Root - SE_Root, ymax = Mean_Root + SE_Root), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  labs(title = "Dry Biomass Root (118 Days)", 
       x = "Dose (N kg/ha)", 
       y = "Mean Dry Biomass Root (g, +/- SE)", 
       fill = 'Fertilizer') +
  facet_wrap(~ Application_Method) +
  
  # Black & white color scheme
  scale_fill_manual(values = c("UF" = "white", "MF" = "gray", "None" = "black")) +
  
  # Adjust theme
  theme_minimal() +
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
  
  # Add letter-wise comparison text
  geom_text(aes(y = Mean_Root + SE_Root + 0.5, label = .group), 
            position = position_dodge2(0.9, preserve = 'single'))



# Load necessary libraries
library(dplyr)
library(ggplot2)
library(car)
library(emmeans)
library(multcompView)
library(stringr)

# Convert categorical variables to factors
Lucerne_exp2 <- Lucerne_exp2 %>%
  mutate(
    Dose = factor(Dose...N.kg.ha., levels = c("0", "10", "20", "30", "40", "50", "60")),
    Fertilizer_Type = factor(Fertilizer.type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split"))
  )

### FUNCTION TO ANALYZE EACH PARAMETER ###
analyze_variable <- function(df, variable, title, y_label) {
  
  # Summarize data
  summary_df <- df %>%
    group_by(Dose, Fertilizer_Type, Application_Method) %>%
    summarise(
      Mean_Value = mean(.data[[variable]], na.rm = TRUE),
      SE_Value = sd(.data[[variable]], na.rm = TRUE) / sqrt(n())
    ) %>%
    ungroup()
  
  # Linear model & ANOVA
  model <- lm(reformulate("Dose * Fertilizer_Type * Application_Method", response = variable), data = df)
  print(summary(model))
  anova_results <- Anova(model, type = "II")
  print(anova_results)
  
  # Tukey HSD test
  pairwise <- emmeans(model, ~ Dose + Fertilizer_Type | Application_Method)
  letters <- cld(pairwise, Letters = letters) %>%
    mutate(.group = str_trim(.group)) %>%
    as.data.frame()
  
  # Merge letters for comparison
  summary_df <- summary_df %>%
    left_join(letters, by = c("Dose", "Fertilizer_Type", "Application_Method"))
  
  # Plot results
  ggplot(summary_df, aes(x = Dose, y = Mean_Value, fill = Fertilizer_Type)) +
    geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), 
             colour = "black") +
    geom_errorbar(aes(ymin = Mean_Value - SE_Value, ymax = Mean_Value + SE_Value), 
                  width = 0.2, position = position_dodge(width = 0.9)) +
    labs(title = title, 
         x = "Dose (N kg/ha)", 
         y = y_label, 
         fill = 'Fertilizer') +
    facet_wrap(~ Application_Method) +
    
    # Black & white color scheme
    scale_fill_manual(values = c("UF" = "white", "MF" = "gray", "None" = "black")) +
    
    # Adjust theme
    theme_minimal() +
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
    
    # Add letter-wise comparison text
    geom_text(aes(y = Mean_Value + SE_Value + 0.5, label = .group), 
              position = position_dodge2(0.9, preserve = 'single'))
}

### RUN ANALYSIS FOR EACH PARAMETER ###

# Chlorophyll Content Before 2nd Dose
analyze_variable(Lucerne_exp2, "Cholorphyll_content.Before2ndDose.", 
                 "Chlorophyll Content Before 2nd Dose", "Chlorophyll Content")

# Chlorophyll Content Before 3rd Dose
analyze_variable(Lucerne_exp2, "Cholorphyll_content.Before3rdDose.", 
                 "Chlorophyll Content Before 3rd Dose", "Chlorophyll Content")

# Chlorophyll Content at Harvest Day
analyze_variable(Lucerne_exp2, "Cholorphyll_content.HarvestDay.", 
                 "Chlorophyll Content at Harvest Day", "Chlorophyll Content")
c
# pH at Harvest Day
analyze_variable(Lucerne_exp2, "pH_8_atHarvestDay", 
                 "Soil pH at Harvest Day", "pH Level")

# EC at Harvest Day
analyze_variable(Lucerne_exp2, "EC._atHarvestDay", 
                 "Electrical Conductivity (EC) at Harvest Day", "EC (dS/m)")

# Root-to-Shoot Ratio
analyze_variable(Lucerne_exp2, "Root.shoot.Ratio", 
                 "Root-to-Shoot Ratio", "Ratio")

# Potential Yield Lost
analyze_variable(Lucerne_exp2, "Potential.Yield.lost", 
                 "Potential Yield Lost", "Yield Lost (g)")


colnames(Lucerne_exp2)



# Load necessary libraries
library(ggplot2)
library(ggpubr)

# Scatter Plot: Dry Biomass Shoot vs. Chlorophyll Content at Harvest Day
ggplot(Lucerne_exp2, aes(x = Cholorphyll_content.HarvestDay., y = Dry_Biomass_shoot_4)) +
  geom_point(color = "black", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Dry Biomass Shoot vs. Chlorophyll Content at Harvest",
       x = "Chlorophyll Content at Harvest Day",
       y = "Dry Biomass Shoot (g)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", color = "black", size = 14, hjust = 0.5),
    axis.title.x = element_text(face = "bold", color = "black", size = 12),
    axis.title.y = element_text(face = "bold", color = "black", size = 12),
    axis.text = element_text(color = "black", size = 10)
  )

# Scatter Plot: Dry Biomass Root vs. Chlorophyll Content at Harvest Day
ggplot(Lucerne_exp2, aes(x = Cholorphyll_content.HarvestDay., y = Dry_Biomass_root_118days)) +
  geom_point(color = "black", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  labs(title = "Dry Biomass Root vs. Chlorophyll Content at Harvest",
       x = "Chlorophyll Content at Harvest Day",
       y = "Dry Biomass Root (g)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", color = "black", size = 14, hjust = 0.5),
    axis.title.x = element_text(face = "bold", color = "black", size = 12),
    axis.title.y = element_text(face = "bold", color = "black", size = 12),
    axis.text = element_text(color = "black", size = 10)
  )

# Calculate correlation coefficients
cor_shoot <- cor(Lucerne_exp2$Cholorphyll_content.HarvestDay., Lucerne_exp2$Dry_Biomass_shoot_4, use = "complete.obs")
cor_root <- cor(Lucerne_exp2$Cholorphyll_content.HarvestDay., Lucerne_exp2$Dry_Biomass_root_118days, use = "complete.obs")

# Print correlation results
print(paste("Correlation between Dry Biomass Shoot and Chlorophyll Content at Harvest:", round(cor_shoot, 3)))
print(paste("Correlation between Dry Biomass Root and Chlorophyll Content at Harvest:", round(cor_root, 3)))
