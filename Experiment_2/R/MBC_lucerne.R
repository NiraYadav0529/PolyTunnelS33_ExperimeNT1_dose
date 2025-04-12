# Load necessary libraries
library(dplyr)
library(ggplot2)
library(car)
library(emmeans)
library(multcompView)
library(stringr)
library(ggpubr)

# Convert categorical variables to factors for Lucerne
Lucerne_MBC <- Lucerne_MBC %>%
  mutate(
    Dose = factor(Dose...N.kg.ha., levels = c("0", "10", "20", "30", "40", "50", "60")),
    Fertilizer_Type = factor(Fertilizer.type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split")),
    Treatment_Type = factor(Treatment_Type, levels = c("Control", "Fertilizer"))
  )

# Convert categorical variables to factors for Phalaris
Phalaris_MBC <- Phalaris_MBC %>%
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


# MBC (Microbial Biomass Carbon)
analyze_variable(Lucerne_MBC, "MBC..mg.kg.", "MBC in Lucerne", "MBC (mg/kg)")
analyze_variable(Phalaris_MBC, "MBC..mg.kg.", "MBC in Phalaris", "MBC (mg/kg)")

# MBN (Microbial Biomass Nitrogen)
analyze_variable(Lucerne_MBC, "MBN..mg.kg.", "MBN in Lucerne", "MBN (mg/kg)")
analyze_variable(Phalaris_MBC, "MBC.N..mg.kg.", "MBN in Phalaris", "MBN (mg/kg)")

# C:N Ratio
Lucerne_MBC <- Lucerne_MBC %>%
  mutate(CN_Ratio = `MBC..mg.kg.` / `MBN..mg.kg.`)
Phalaris_MBC <- Phalaris_MBC %>%
  mutate(CN_Ratio = `MBC..mg.kg.` / `MBC.N..mg.kg.`)

analyze_variable(Lucerne_MBC, "CN_Ratio", "C:N Ratio in Lucerne", "C:N Ratio")
analyze_variable(Phalaris_MBC, "CN_Ratio", "C:N Ratio in Phalaris", "C:N Ratio")


# Scatter Plot: MBC vs. MBN (Lucerne)
ggplot(Lucerne_MBC, aes(x = `MBN..mg.kg.`, y = `MBC..mg.kg.`)) +
  geom_point(color = "black", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "MBC vs. MBN in Lucerne",
       x = "MBN (mg/kg)",
       y = "MBC (mg/kg)") +
  theme_minimal()

# Scatter Plot: C:N Ratio vs. MBC (Lucerne)
ggplot(Lucerne_MBC, aes(x = `CN_Ratio`, y = `MBC..mg.kg.`)) +
  geom_point(color = "black", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  labs(title = "C:N Ratio vs. MBC in Lucerne",
       x = "C:N Ratio",
       y = "MBC (mg/kg)") +
  theme_minimal()

# Scatter Plot: MBC vs. MBN (Phalaris)
ggplot(Phalaris_MBC, aes(x = `MBC.N..mg.kg.`, y = `MBC..mg.kg.`)) +
  geom_point(color = "black", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "MBC vs. MBN in Phalaris",
       x = "MBN (mg/kg)",
       y = "MBC (mg/kg)") +
  theme_minimal()

# Calculate correlation coefficients
cor_lucerne <- cor(Lucerne_MBC$`MBC..mg.kg.`, Lucerne_MBC$`MBN..mg.kg.`, use = "complete.obs")
cor_phalaris <- cor(Phalaris_MBC$`MBC..mg.kg.`, Phalaris_MBC$`MBC.N..mg.kg.`, use = "complete.obs")

# Print correlation results
print(paste("Correlation (Lucerne):", round(cor_lucerne, 3)))
print(paste("Correlation (Phalaris):", round(cor_phalaris, 3)))


