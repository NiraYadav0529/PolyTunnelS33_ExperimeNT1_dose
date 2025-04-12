# Load necessary libraries
library(dplyr)
library(ggplot2)
library(car)
library(emmeans)
library(multcompView)
library(stringr)
library(ggpubr)

# Convert categorical variables to factors
Phalaris_exp2 <- Phalaris_exp2 %>%
  mutate(
    Dose = factor(Dose_.N.kg.ha., levels = c("0", "20", "40", "60", "80", "100", "120")),
    Fertilizer_Type = factor(Fertilizer_type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split")),
    Treatment_Type = factor(Treatment_Type, levels = c("Control", "Other"))
  )
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


# Chlorophyll Content at 66 Days
analyze_variable(Phalaris_exp2, "CholorophyllContent_66days", 
                 "Chlorophyll Content at 66 Days", "Chlorophyll Content")

# Chlorophyll Content at 125 Days (Harvest Day)
analyze_variable(Phalaris_exp2, "CholorophyllContent_125daysHD", 
                 "Chlorophyll Content at 125 Days (Harvest)", "Chlorophyll Content")

# pH at 125 Days (Harvest Day)
analyze_variable(Phalaris_exp2, "ph_10", 
                 "Soil pH at 125 Days (Harvest)", "pH Level")

# EC at 125 Days (Harvest Day)
analyze_variable(Phalaris_exp2, "EC_10", 
                 "Electrical Conductivity (EC) at 125 Days (Harvest)", "EC (dS/m)")

# Dry Biomass at 66 Days
analyze_variable(Phalaris_exp2, "Dry_Biomass.gm._66days", 
                 "Dry Biomass at 66 Days", "Dry Biomass (g)")

# Dry Biomass at 125 Days (Harvest Day)
analyze_variable(Phalaris_exp2, "Dry_shoot_..Biomass.g._125daysHD", 
                 "Dry Biomass at 125 Days (Harvest)", "Dry Biomass (g)")

# Dry Root Biomass at 125 Days (Harvest Day)
analyze_variable(Phalaris_exp2, "Dry_root_biomass.g._125daysHD", 
                 "Dry Root Biomass at 125 Days (Harvest)", "Dry Biomass (g)")

# Root-to-Shoot Ratio
analyze_variable(Phalaris_exp2, "Root.shoot.Ratio", 
                 "Root-to-Shoot Ratio", "Ratio")

# Potential Yield Lost
analyze_variable(Phalaris_exp2, "Potential.Yield.lost", 
                 "Potential Yield Lost", "Yield Lost (g)")


# Scatter Plot: Dry Root Biomass vs. Chlorophyll Content (125 Days)
ggplot(Phalaris_exp2, aes(x = CholorophyllContent_125daysHD, y = Dry_root_biomass.g._125daysHD)) +
  geom_point(color = "black", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Dry Root Biomass vs. Chlorophyll Content (125 Days)",
       x = "Chlorophyll Content (125 Days)",
       y = "Dry Root Biomass (g)") +
  theme_minimal()

# Scatter Plot: Dry Biomass vs. Chlorophyll Content (66 Days)
ggplot(Phalaris_exp2, aes(x = CholorophyllContent_66days, y = Dry_Biomass.gm._66days)) +
  geom_point(color = "black", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  labs(title = "Dry Biomass vs. Chlorophyll Content (66 Days)",
       x = "Chlorophyll Content (66 Days)",
       y = "Dry Biomass (g)") +
  theme_minimal()

# Calculate correlation coefficients
cor_125 <- cor(Phalaris_exp2$CholorophyllContent_125daysHD, Phalaris_exp2$Dry_root_biomass.g._125daysHD, use = "complete.obs")
cor_66 <- cor(Phalaris_exp2$CholorophyllContent_66days, Phalaris_exp2$Dry_Biomass.gm._66days, use = "complete.obs")

# Print correlation results
print(paste("Correlation at 125 Days:", round(cor_125, 3)))
print(paste("Correlation at 66 Days:", round(cor_66, 3)))
