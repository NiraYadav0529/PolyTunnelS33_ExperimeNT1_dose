# Load required libraries
library(tidyverse)
library(ggplot2)
library(emmeans)
library(multcompView)
library(car)  # For Anova
library(ggpubr)  # For residual plots and diagnostics
# Read the dataset
Phalaris_exp2 <- read.csv("Phalaris_data-file_Exp2.csv")

# Ensure dataset is set up with correct factor levels
Phalaris_exp2 <- Phalaris_exp2 %>%
  mutate(
    Dose = factor(Dose_.N.kg.ha., levels = c("0", "10", "20", "30", "40", "50", "60")),
    Fertilizer_Type = factor(Fertilizer_type, levels = c('MF', 'UF')),
    Application_Method = factor(Application_method, levels = c('One-time','Split'))
  )
# Summarize data (mean and SE)
summary_data <- Phalaris_exp2 %>%
  group_by(Dose, Fertilizer_Type, Application_Method) %>%
  summarise(
    Mean_pH = mean(ph_125daysHD, na.rm = TRUE),
    SE_pH = sd(ph_125daysHD, na.rm = TRUE) / sqrt(n()),
    Mean_EC = mean(EC_125daysHD, na.rm = TRUE),
    SE_EC = sd(EC_125daysHD, na.rm = TRUE) / sqrt(n()),
    Mean_SPAD = mean(Spad_Value_125daysHD, na.rm = TRUE),
    SE_SPAD = sd(Spad_Value_125daysHD, na.rm = TRUE) / sqrt(n()),
    Mean_FreshShoot = mean(Fresh_shoot_..Biomass.g._125daysHD, na.rm = TRUE),
    SE_FreshShoot = sd(Fresh_shoot_..Biomass.g._125daysHD, na.rm = TRUE) / sqrt(n()),
    Mean_FreshRoot = mean(Fresh_root_biomass.g._125daysHD, na.rm = TRUE),
    SE_FreshRoot = sd(Fresh_root_biomass.g._125daysHD, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()
# Model for pH
m_pH <- lm(ph_125daysHD ~ Dose * Fertilizer_Type * Application_Method, data = Phalaris_exp2)

# Residual diagnostics
residualPlot(m_pH)
qqPlot(m_pH)

# ANOVA
Anova(m_pH, type = "II")

# Pairwise comparisons and compact letters
pairwise_pH <- emmeans(m_pH, ~ Dose * Fertilizer_Type | Application_Method)
letters_pH <- multcomp::cld(pairwise_pH, Letters = "abc") %>%
  as.data.frame() %>%
  mutate(.group = str_trim(.group))

# Merge pairwise results with summary data
merged_pH <- summary_data %>%
  left_join(letters_pH, by = c("Dose", "Fertilizer_Type", "Application_Method"))

# Filter out rows with NA in Application_Method
filtered_pH <- merged_pH %>%
  filter(!is.na(Application_Method))

# Updated ggplot code
ggplot(filtered_pH, aes(x = Dose, y = Mean_pH, fill = Fertilizer_Type)) + 
  geom_bar(stat = "identity", position = position_dodge(0.9), color = "black") +
  geom_errorbar(aes(ymin = Mean_pH - SE_pH, ymax = Mean_pH + SE_pH), 
                width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(y = Mean_pH + SE_pH + 0.05, label = .group), 
            position = position_dodge(0.9), vjust = -0.5, size = 5) +
  facet_wrap(~ Application_Method) +  # Only shows One-time and Split
  labs(title = "pH at 125 Days", x = "Dose (N kg/ha)", y = "Mean pH") +
  scale_fill_manual(values = c("grey20", "grey50")) +
  theme_minimal(base_size = 15) +
  theme(panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = "grey"),
        legend.position = "right")

# Model for EC
m_EC <- lm(EC_125daysHD ~ Dose * Fertilizer_Type * Application_Method, data = Phalaris_exp2)

# Residual diagnostics
residualPlot(m_EC)
qqPlot(m_EC)
Anova(m_EC, type = "II")

# Pairwise comparisons
pairwise_EC <- emmeans(m_EC, ~ Dose * Fertilizer_Type | Application_Method)
letters_EC <- cld(pairwise_EC, Letters = "abc") %>%
  as.data.frame() %>%
  mutate(.group = str_trim(.group))

# Merge letters with summary data
merged_EC <- summary_data %>%
  left_join(letters_EC, by = c("Dose", "Fertilizer_Type", "Application_Method")) %>%
  filter(!is.na(Application_Method))  # Remove NA Application_Method
ggplot(merged_EC, aes(x = Dose, y = Mean_EC, fill = Fertilizer_Type)) + 
  geom_bar(stat = "identity", position = position_dodge(0.9), color = "black") +
  geom_errorbar(aes(ymin = Mean_EC - SE_EC, ymax = Mean_EC + SE_EC), 
                width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(y = Mean_EC + SE_EC + 0.05, label = .group), 
            position = position_dodge(0.9), vjust = -0.5, size = 5) +
  facet_wrap(~ Application_Method) +
  labs(title = "EC at 125 Days", x = "Dose (N kg/ha)", y = "Mean EC (uS/cm)") +
  scale_fill_manual(values = c("grey20", "grey50")) +
  theme_minimal(base_size = 15)

# Model for SPAD
m_SPAD <- lm(Spad_Value_125daysHD ~ Dose * Fertilizer_Type * Application_Method, data = Phalaris_exp2)

# Residual diagnostics
residualPlot(m_SPAD)
qqPlot(m_SPAD)
Anova(m_SPAD, type = "II")

# Pairwise comparisons
pairwise_SPAD <- emmeans(m_SPAD, ~ Dose * Fertilizer_Type | Application_Method)
letters_SPAD <- cld(pairwise_SPAD, Letters = "abc") %>%
  as.data.frame() %>%
  mutate(.group = str_trim(.group))

# Merge letters with summary data
merged_SPAD <- summary_data %>%
  left_join(letters_SPAD, by = c("Dose", "Fertilizer_Type", "Application_Method")) %>%
  filter(!is.na(Application_Method))  # Remove NA Application_Method
ggplot(merged_SPAD, aes(x = Dose, y = Mean_SPAD, fill = Fertilizer_Type)) + 
  geom_bar(stat = "identity", position = position_dodge(0.9), color = "black") +
  geom_errorbar(aes(ymin = Mean_SPAD - SE_SPAD, ymax = Mean_SPAD + SE_SPAD), 
                width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(y = Mean_SPAD + SE_SPAD + 0.05, label = .group), 
            position = position_dodge(0.9), vjust = -0.5, size = 5) +
  facet_wrap(~ Application_Method) +
  labs(title = "SPAD Value at 125 Days", x = "Dose (N kg/ha)", y = "Mean SPAD Value") +
  scale_fill_manual(values = c("grey20", "grey50")) +
  theme_minimal(base_size = 15)
# Model for Fresh Shoot Biomass
m_FreshShoot <- lm(Fresh_shoot_..Biomass.g._125daysHD ~ Dose * Fertilizer_Type * Application_Method, data = Phalaris_exp2)

# Residual diagnostics
residualPlot(m_FreshShoot)
qqPlot(m_FreshShoot)
Anova(m_FreshShoot, type = "II")

# Pairwise comparisons
pairwise_FreshShoot <- emmeans(m_FreshShoot, ~ Dose * Fertilizer_Type | Application_Method)
letters_FreshShoot <- cld(pairwise_FreshShoot, Letters = "abc") %>%
  as.data.frame() %>%
  mutate(.group = str_trim(.group))

# Merge letters with summary data
merged_FreshShoot <- summary_data %>%
  left_join(letters_FreshShoot, by = c("Dose", "Fertilizer_Type", "Application_Method")) %>%
  filter(!is.na(Application_Method))  # Remove NA Application_Method
ggplot(merged_FreshShoot, aes(x = Dose, y = Mean_FreshShoot, fill = Fertilizer_Type)) + 
  geom_bar(stat = "identity", position = position_dodge(0.9), color = "black") +
  geom_errorbar(aes(ymin = Mean_FreshShoot - SE_FreshShoot, ymax = Mean_FreshShoot + SE_FreshShoot), 
                width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(y = Mean_FreshShoot + SE_FreshShoot + 0.05, label = .group), 
            position = position_dodge(0.9), vjust = -0.5, size = 5) +
  facet_wrap(~ Application_Method) +
  labs(title = "Fresh Shoot Biomass at 125 Days", x = "Dose (N kg/ha)", y = "Mean Fresh Shoot Biomass (g)") +
  scale_fill_manual(values = c("grey20", "grey50")) +
  theme_minimal(base_size = 15)

# Model for Fresh Root Biomass
m_FreshRoot <- lm(Fresh_root_biomass.g._125daysHD ~ Dose * Fertilizer_Type * Application_Method, data = Phalaris_exp2)

# Residual diagnostics
residualPlot(m_FreshRoot)
qqPlot(m_FreshRoot)
Anova(m_FreshRoot, type = "II")

# Pairwise comparisons
pairwise_FreshRoot <- emmeans(m_FreshRoot, ~ Dose * Fertilizer_Type | Application_Method)
letters_FreshRoot <- cld(pairwise_FreshRoot, Letters = "abc") %>%
  as.data.frame() %>%
  mutate(.group = str_trim(.group))

# Merge letters with summary data
merged_FreshRoot <- summary_data %>%
  left_join(letters_FreshRoot, by = c("Dose", "Fertilizer_Type", "Application_Method")) %>%
  filter(!is.na(Application_Method))  # Remove NA Application_Method
ggplot(merged_FreshRoot, aes(x = Dose, y = Mean_FreshRoot, fill = Fertilizer_Type)) + 
  geom_bar(stat = "identity", position = position_dodge(0.9), color = "black") +
  geom_errorbar(aes(ymin = Mean_FreshRoot - SE_FreshRoot, ymax = Mean_FreshRoot + SE_FreshRoot), 
                width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(y = Mean_FreshRoot + SE_FreshRoot + 0.05, label = .group), 
            position = position_dodge(0.9), vjust = -0.5, size = 5) +
  facet_wrap(~ Application_Method) +
  labs(title = "Fresh Root Biomass at 125 Days", x = "Dose (N kg/ha)", y = "Mean Fresh Root Biomass (g)") +
  scale_fill_manual(values = c("grey20", "grey50")) +
  theme_minimal(base_size = 15)


