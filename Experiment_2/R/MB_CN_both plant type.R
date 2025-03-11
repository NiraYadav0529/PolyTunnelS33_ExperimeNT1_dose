setwd("C:/Users/90958427/OneDrive - Western Sydney University/PolyTunnelS33_ExperimeNT1_dose/Experiment_2/Modified_Data_File")
list.files()
Lucerne_MBC <- read.csv("MBC(CN)_Lucerne.csv", check.names = TRUE)
Phalaris_MBC <- read.csv("MB(CN)_Phalaris.csv", check.names = TRUE)
head(Lucerne_MBC)
head(Phalaris_MBC)

# Load required libraries
library(tidyverse)
library(lme4)
library(car)
library(emmeans)
library(multcompView)

# Filter data for HarvestDay
Lucerne_Harvest <- Lucerne_MBC %>% filter(TimePoint == "HarvestDay")

# Convert categorical variables to factors
Lucerne_Harvest <- Lucerne_Harvest %>%
  mutate(
    Dose = factor(Dose...N.kg.ha., levels = c("0", "10", "20", "30", "40", "50", "60")),
    Fertilizer_Type = factor(Fertilizer.type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split")),
    Treatment_Type = factor(Treatment_Type, levels = c("Control", "Fertilizer"))
  )

# Summary statistics (mean and standard error)
summary_stats <- Lucerne_Harvest %>%
  group_by(Fertilizer_Type, Dose, Application_Method) %>%
  summarise(
    MBC_mean = mean(MBC..mg.kg., na.rm = TRUE),
    MBC_se = sd(MBC..mg.kg., na.rm = TRUE) / sqrt(n()),
    MBN_mean = mean(MBN..mg.kg., na.rm = TRUE),
    MBN_se = sd(MBN..mg.kg., na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

print(summary_stats)

# Fit Linear Mixed Model (LMM) for MBC
m1_MBC <- lmer(MBC..mg.kg. ~ Dose * Fertilizer_Type * Application_Method + (1 | Block), data = Lucerne_Harvest)

# Model Diagnostics
plot(m1_MBC)  # Residual plot
qqPlot(resid(m1_MBC))  # Q-Q plot for normality

# ANOVA for significance testing
Anova(m1_MBC, test = "F")

# Summary of the model
summary(m1_MBC)

# Pairwise comparison and letter annotation for MBC
emmeans_mbc <- emmeans(m1_MBC, pairwise ~ Dose * Fertilizer_Type * Application_Method, adjust = "tukey")
letters_mbc <- cld(emmeans_mbc$emmeans, Letters = letters)

# Boxplot for MBC
ggplot(Lucerne_Harvest, aes(x = Dose, y = MBC..mg.kg., fill = Fertilizer_Type)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ Application_Method) +
  theme_minimal() +
  labs(title = "Microbial Biomass Carbon (MBC) by Dose, Fertilizer Type, and Application Method",
       x = "Nitrogen Dose (kg/ha)",
       y = "MBC (mg/kg)",
       fill = "Fertilizer Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Fit Linear Mixed Model (LMM) for MBN
m1_MBN <- lmer(MBN..mg.kg. ~ Dose * Fertilizer_Type * Application_Method + (1 | Block), data = Lucerne_Harvest)

# Model Diagnostics
plot(m1_MBN)  # Residual plot
qqPlot(resid(m1_MBN))  # Q-Q plot for normality

# ANOVA for significance testing
Anova(m1_MBN, test = "F")

# Summary of the model
summary(m1_MBN)

# Pairwise comparison and letter annotation for MBN
emmeans_mbn <- emmeans(m1_MBN, pairwise ~ Dose * Fertilizer_Type * Application_Method, adjust = "tukey")
letters_mbn <- cld(emmeans_mbn$emmeans, Letters = letters)

# Boxplot for MBN
ggplot(Lucerne_Harvest, aes(x = Dose, y = MBN..mg.kg., fill = Fertilizer_Type)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ Application_Method) +
  theme_minimal() +
  labs(title = "Microbial Biomass Nitrogen (MBN) by Dose, Fertilizer Type, and Application Method",
       x = "Nitrogen Dose (kg/ha)",
       y = "MBN (mg/kg)",
       fill = "Fertilizer Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# Calculate C:N ratio
Lucerne_Harvest <- Lucerne_Harvest %>%
  mutate(CN_ratio = MBC..mg.kg. / MBN..mg.kg.)

# Summary statistics for C:N ratio
summary_CN <- Lucerne_Harvest %>%
  group_by(Fertilizer_Type, Dose, Application_Method, Treatment_Type) %>%
  summarise(
    CN_mean = mean(CN_ratio, na.rm = TRUE),
    CN_se = sd(CN_ratio, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Print summary
print(summary_CN)

# Boxplot for C:N ratio
ggplot(Lucerne_Harvest, aes(x = Dose, y = CN_ratio, fill = Fertilizer_Type)) +
  geom_boxplot(alpha = 0.7) +
  facet_grid(Treatment_Type ~ Application_Method) +
  theme_minimal() +
  labs(title = "Microbial Biomass C:N Ratio by Dose, Fertilizer Type, and Application Method",
       x = "Nitrogen Dose (kg/ha)",
       y = "C:N Ratio",
       fill = "Fertilizer Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Linear Mixed Model (LMM) for C:N ratio
m1_CN <- lmer(CN_ratio ~ Dose * Fertilizer_Type * Application_Method * Treatment_Type + (1 | Block), data = Lucerne_Harvest)

# Model Diagnostics
plot(m1_CN)  # Residual plot
qqPlot(resid(m1_CN))  # Q-Q plot for normality

# ANOVA for significance testing
Anova(m1_CN, test = "F")

# Summary of the model
summary(m1_CN)

# Pairwise comparisons and letter annotation for C:N ratio
emmeans_cn <- emmeans(m1_CN, pairwise ~ Dose * Fertilizer_Type * Application_Method * Treatment_Type, adjust = "tukey")
letters_cn <- cld(emmeans_cn$emmeans, Letters = letters)

# Display the C:N ratio means with letters
print(letters_cn)


# Load required libraries
library(tidyverse)
library(lme4)
library(car)
library(emmeans)
library(multcomp)
library(multcompView)

# Filter data for OT after_15days
Lucerne_OT15 <- Lucerne_MBC %>% filter(TimePoint == "OT after_15days")

# Convert categorical variables to factors
Lucerne_OT15 <- Lucerne_OT15 %>%
  mutate(
    Dose = factor(Dose...N.kg.ha., levels = c("0", "10", "20", "30", "40", "50", "60")),
    Fertilizer_Type = factor(Fertilizer.type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split")),
    Treatment_Type = factor(Treatment_Type, levels = c("Control", "Fertilizer"))
  )

# Summary statistics (mean and standard error)
summary_stats_OT15 <- Lucerne_OT15 %>%
  group_by(Fertilizer_Type, Dose, Application_Method) %>%
  summarise(
    MBC_mean = mean(MBC..mg.kg., na.rm = TRUE),
    MBC_se = sd(MBC..mg.kg., na.rm = TRUE) / sqrt(n()),
    MBN_mean = mean(MBN..mg.kg., na.rm = TRUE),
    MBN_se = sd(MBN..mg.kg., na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

print(summary_stats_OT15)
table(Lucerne_OT15$Dose)
table(Lucerne_OT15$Fertilizer_Type)
table(Lucerne_OT15$Application_Method)

m1_MBC_OT15 <- lmer(MBC..mg.kg. ~ Dose * Fertilizer_Type + (1 | Block), data = Lucerne_OT15)
nrow(Lucerne_OT15)

# Model Diagnostics
plot(m1_MBC_OT15)  # Residual plot
qqPlot(resid(m1_MBC_OT15))  # Q-Q plot for normality

# ANOVA for significance testing
Anova(m1_MBC_OT15, test = "F")

# Summary of the model
summary(m1_MBC_OT15)


# Pairwise comparison and letter annotation for MBC
emmeans_mbc_OT15 <- emmeans(m1_MBC_OT15, pairwise ~ Dose * Fertilizer_Type * Application_Method, adjust = "tukey")
letters_mbc_OT15 <- cld(emmeans_mbc_OT15$emmeans, Letters = letters)

# Boxplot for MBC
ggplot(Lucerne_OT15, aes(x = Dose, y = MBC..mg.kg., fill = Fertilizer_Type)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ Application_Method) +
  theme_minimal() +
  labs(title = "Microbial Biomass Carbon (MBC) - OT after 15 Days",
       x = "Nitrogen Dose (kg/ha)",
       y = "MBC (mg/kg)",
       fill = "Fertilizer Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

table(Lucerne_OT15$Dose)
table(Lucerne_OT15$Fertilizer_Type)
table(Lucerne_OT15$Application_Method)

# Identify categorical variables with at least 2 levels
dose_levels <- unique(Lucerne_OT15$Dose)
fertilizer_levels <- unique(Lucerne_OT15$Fertilizer_Type)
application_levels <- unique(Lucerne_OT15$Application_Method)

# Build model formula dynamically
formula_terms <- c()
if (length(dose_levels) > 1) formula_terms <- c(formula_terms, "Dose")
if (length(fertilizer_levels) > 1) formula_terms <- c(formula_terms, "Fertilizer_Type")
if (length(application_levels) > 1) formula_terms <- c(formula_terms, "Application_Method")

# Ensure at least one fixed effect is present
if (length(formula_terms) > 0) {
  formula <- paste("MBN..mg.kg. ~", paste(formula_terms, collapse = " * "), "+ (1 | Block)")
} else {
  formula <- "MBN..mg.kg. ~ (1 | Block)"  # Only random effect if no valid fixed effects
}

# Fit the model
m1_MBN_OT15 <- lmer(as.formula(formula), data = Lucerne_OT15)

# Model Diagnostics
plot(m1_MBN_OT15)  # Residual plot
qqPlot(resid(m1_MBN_OT15))  # Q-Q plot for normality

# ANOVA for significance testing
Anova(m1_MBN_OT15, test = "F")

# Summary of the model
summary(m1_MBN_OT15)



# Pairwise comparison and letter annotation for MBN
emmeans_mbn_OT15 <- emmeans(m1_MBN_OT15, pairwise ~ Dose * Fertilizer_Type * Application_Method, adjust = "tukey")
letters_mbn_OT15 <- cld(emmeans_mbn_OT15$emmeans, Letters = letters)

# Boxplot for MBN
ggplot(Lucerne_OT15, aes(x = Dose, y = MBN..mg.kg., fill = Fertilizer_Type)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ Application_Method) +
  theme_minimal() +
  labs(title = "Microbial Biomass Nitrogen (MBN) - OT after 15 Days",
       x = "Nitrogen Dose (kg/ha)",
       y = "MBN (mg/kg)",
       fill = "Fertilizer Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###fOR PHALARIS 
colnames(Phalaris_MBC)
# Convert categorical variables to factors for Phalaris dataset
Phalaris_exp2 <- Phalaris_MBC %>%
  mutate(
    Dose = factor(Dose_.N.kg.ha., levels = c("0", "20", "40", "60", "80", "100", "120")),
    Fertilizer_Type = factor(Fertilizer_type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split")),
    Treatment_Type = factor(Treatment_Type, levels = c("Control", "Other"))
  )
# Summary statistics for Phalaris dataset
summary_stats_phalaris <- Phalaris_MBC %>%
  group_by(Fertilizer_type, Dose_.N.kg.ha., Application_method) %>%
  summarise(
    MBC_mean = mean(MBC..mg.kg., na.rm = TRUE),
    MBC_se = sd(MBC..mg.kg., na.rm = TRUE) / sqrt(n()),
    MBN_mean = mean(MBC.N..mg.kg., na.rm = TRUE),  # Updated column name
    MBN_se = sd(MBC.N..mg.kg., na.rm = TRUE) / sqrt(n()),  # Updated column name
    .groups = "drop"
  )

print(summary_stats_phalaris)

# Fit LMM for MBC in Phalaris dataset using Phalaris_exp2
m1_MBC_phalaris <- lmer(MBC..mg.kg. ~ Dose * Fertilizer_Type * Application_Method + (1 | Block), data = Phalaris_exp2)

# Model Diagnostics
plot(m1_MBC_phalaris)  # Residual plot


# Model Diagnostics
plot(m1_MBC_phalaris)  # Residual plot
qqPlot(resid(m1_MBC_phalaris))  # Q-Q plot for normality

# ANOVA for significance testing
Anova(m1_MBC_phalaris, test = "F")

# Summary of the model
summary(m1_MBC_phalaris)

# Pairwise comparison and letter annotation for MBC
emmeans_mbc_phalaris <- emmeans(m1_MBC_phalaris, pairwise ~ Dose * Fertilizer_Type * Application_Method, adjust = "tukey")
letters_mbc_phalaris <- cld(emmeans_mbc_phalaris$emmeans, Letters = letters)
# Boxplot for MBC in Phalaris
ggplot(Phalaris_exp2, aes(x = Dose, y = MBC..mg.kg., fill = Fertilizer_Type)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ Application_Method) +
  theme_minimal() +
  labs(title = "Microbial Biomass Carbon (MBC) in Phalaris",
       x = "Nitrogen Dose (kg/ha)",
       y = "MBC (mg/kg)",
       fill = "Fertilizer Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
head(Phalaris_MBC)
Phalaris_MBC <- Phalaris_MBC %>%
  mutate(Dose = factor(Dose_.N.kg.ha., levels = c("0", "20", "40", "60", "80", "100", "120")))
# Fit LMM for MBN in Phalaris dataset
m1_MBN_phalaris <- lmer(MBC.N..mg.kg. ~ Dose * Fertilizer_type * Application_method + (1 | Block), data = Phalaris_MBC)





# Model Diagnostics
plot(m1_MBN_phalaris)  # Residual plot
qqPlot(resid(m1_MBN_phalaris))  # Q-Q plot for normality

# ANOVA for significance testing
Anova(m1_MBN_phalaris, test = "F")

# Summary of the model
summary(m1_MBN_phalaris)

# Pairwise comparison and letter annotation for MBN
emmeans_mbn_phalaris <- emmeans(m1_MBN_phalaris, pairwise ~ Dose * Fertilizer_Type * Application_Method, adjust = "tukey")
letters_mbn_phalaris <- cld(emmeans_mbn_phalaris$emmeans, Letters = letters)

sum(is.na(Phalaris_MBC$Application_method))
colnames(Phalaris_MBC)

# Boxplot for MBN in Phalaris
ggplot(Phalaris_MBC, aes(x = Dose, y = MBC.N..mg.kg., fill = Fertilizer_type)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ Application_method) +
  theme_minimal() +
  labs(title = "Microbial Biomass Nitrogen (MBN) in Phalaris",
       x = "Nitrogen Dose (kg/ha)",
       y = "MBN (mg/kg)",
       fill = "Fertilizer Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate C:N ratio for Phalaris_exp2
Phalaris_exp2 <- Phalaris_exp2 %>%
  mutate(CN_ratio = MBC..mg.kg. / MBC.N..mg.kg.)

# Summary statistics for C:N ratio
summary_CN_phalaris <- Phalaris_exp2 %>%
  group_by(Fertilizer_Type, Dose, Application_Method, Treatment_Type) %>%
  summarise(
    CN_mean = mean(CN_ratio, na.rm = TRUE),
    CN_se = sd(CN_ratio, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Print summary
print(summary_CN_phalaris)

# Boxplot for C:N ratio
ggplot(Phalaris_exp2, aes(x = Dose, y = CN_ratio, fill = Fertilizer_Type)) +
  geom_boxplot(alpha = 0.7) +
  facet_grid(Treatment_Type ~ Application_Method) +
  theme_minimal() +
  labs(title = "Microbial Biomass C:N Ratio in Phalaris",
       x = "Nitrogen Dose (kg/ha)",
       y = "C:N Ratio",
       fill = "Fertilizer Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Fit Linear Mixed Model (LMM) for C:N ratio
m1_CN_phalaris <- lmer(CN_ratio ~ Dose * Fertilizer_Type * Application_Method * Treatment_Type + (1 | Block), data = Phalaris_exp2)

# Model Diagnostics
plot(m1_CN_phalaris)  # Residual plot
qqPlot(resid(m1_CN_phalaris))  # Q-Q plot for normality

# ANOVA for significance testing
Anova(m1_CN_phalaris, test = "F")

# Summary of the model
summary(m1_CN_phalaris)

# Pairwise comparisons and letter annotation for C:N ratio
emmeans_cn_phalaris <- emmeans(m1_CN_phalaris, pairwise ~ Dose * Fertilizer_Type * Application_Method * Treatment_Type, adjust = "tukey")
letters_cn_phalaris <- cld(emmeans_cn_phalaris$emmeans, Letters = letters)

# Display the C:N ratio means with letters
print(letters_cn_phalaris)




# Filter data for OT after_15days
Phalaris_OT15 <- Phalaris_MBC %>% filter(TimePoint == "OT after_15days")

# Convert categorical variables to factors
Phalaris_OT15 <- Phalaris_OT15 %>%
  mutate(
    Dose = factor(Dose_.N.kg.ha., levels = c("0", "20", "40", "60", "80", "100", "120")),
    Fertilizer_Type = factor(Fertilizer_type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split")),
    Treatment_Type = factor(Treatment_Type, levels = c("Control", "Fertilizer"))
  )

# Summary statistics (mean and standard error)
summary_stats_OT15 <- Phalaris_OT15 %>%
  group_by(Fertilizer_Type, Dose, Application_Method) %>%
  summarise(
    MBC_mean = mean(MBC..mg.kg., na.rm = TRUE),
    MBC_se = sd(MBC..mg.kg., na.rm = TRUE) / sqrt(n()),
    MBN_mean = mean(MBC.N..mg.kg., na.rm = TRUE),
    MBN_se = sd(MBC.N..mg.kg., na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

print(summary_stats_OT15)
table(Phalaris_OT15$Dose)
table(Phalaris_OT15$Fertilizer_Type)
table(Phalaris_OT15$Application_Method)

# Fit LMM for MBC in OT after_15days for Phalaris
m1_MBC_OT15 <- lmer(MBC..mg.kg. ~ Dose * Fertilizer_Type + (1 | Block), data = Phalaris_OT15)
nrow(Phalaris_OT15)

# Model Diagnostics
plot(m1_MBC_OT15)  # Residual plot
qqPlot(resid(m1_MBC_OT15))  # Q-Q plot for normality

# ANOVA for significance testing
Anova(m1_MBC_OT15, test = "F")

# Summary of the model
summary(m1_MBC_OT15)

# Pairwise comparison and letter annotation for MBC
emmeans_mbc_OT15 <- emmeans(m1_MBC_OT15, pairwise ~ Dose * Fertilizer_Type * Application_Method, adjust = "tukey")
letters_mbc_OT15 <- cld(emmeans_mbc_OT15$emmeans, Letters = letters)

# Boxplot for MBC
ggplot(Phalaris_OT15, aes(x = Dose, y = MBC..mg.kg., fill = Fertilizer_Type)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ Application_Method) +
  theme_minimal() +
  labs(title = "Microbial Biomass Carbon (MBC) - OT after 15 Days",
       x = "Nitrogen Dose (kg/ha)",
       y = "MBC (mg/kg)",
       fill = "Fertilizer Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Fit LMM for MBN (Microbial Biomass Nitrogen) in OT after_15days for Phalaris
m1_MBN_OT15 <- lmer(MBC.N..mg.kg. ~ Dose * Fertilizer_Type + (1 | Block), data = Phalaris_OT15)

# Model Diagnostics
plot(m1_MBN_OT15)  # Residual plot
qqPlot(resid(m1_MBN_OT15))  # Q-Q plot for normality

# ANOVA for significance testing
Anova(m1_MBN_OT15, test = "F")

# Summary of the model
summary(m1_MBN_OT15)

# Pairwise comparison and letter annotation for MBN
emmeans_mbn_OT15 <- emmeans(m1_MBN_OT15, pairwise ~ Dose * Fertilizer_Type * Application_Method, adjust = "tukey")
letters_mbn_OT15 <- cld(emmeans_mbn_OT15$emmeans, Letters = letters)

# Boxplot for MBN
ggplot(Phalaris_OT15, aes(x = Dose, y = MBC.N..mg.kg., fill = Fertilizer_Type)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ Application_Method) +
  theme_minimal() +
  labs(title = "Microbial Biomass Nitrogen (MBN) - OT after 15 Days",
       x = "Nitrogen Dose (kg/ha)",
       y = "MBN (mg/kg)",
       fill = "Fertilizer Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate C:N ratio for Phalaris
Phalaris_OT15 <- Phalaris_OT15 %>%
  mutate(CN_ratio = MBC..mg.kg. / MBC.N..mg.kg.)

# Summary statistics for C:N ratio
summary_CN_OT15 <- Phalaris_OT15 %>%
  group_by(Fertilizer_Type, Dose, Application_Method, Treatment_Type) %>%
  summarise(
    CN_mean = mean(CN_ratio, na.rm = TRUE),
    CN_se = sd(CN_ratio, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Print summary
print(summary_CN_OT15)

# Boxplot for C:N ratio
ggplot(Phalaris_OT15, aes(x = Dose, y = CN_ratio, fill = Fertilizer_Type)) +
  geom_boxplot(alpha = 0.7) +
  facet_grid(Treatment_Type ~ Application_Method) +
  theme_minimal() +
  labs(title = "Microbial Biomass C:N Ratio - OT after 15 Days",
       x = "Nitrogen Dose (kg/ha)",
       y = "C:N Ratio",
       fill = "Fertilizer Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Fit Linear Mixed Model (LMM) for C:N ratio
m1_CN_OT15 <- lmer(CN_ratio ~ Dose * Fertilizer_Type * Application_Method * Treatment_Type + (1 | Block), data = Phalaris_OT15)

# Model Diagnostics
plot(m1_CN_OT15)  # Residual plot
qqPlot(resid(m1_CN_OT15))  # Q-Q plot for normality

# ANOVA for significance testing
Anova(m1_CN_OT15, test = "F")

# Summary of the model
summary(m1_CN_OT15)

# Pairwise comparisons and letter annotation for C:N ratio
emmeans_cn_OT15 <- emmeans(m1_CN_OT15, pairwise ~ Dose * Fertilizer_Type * Application_Method * Treatment_Type, adjust = "tukey")
letters_cn_OT15 <- cld(emmeans_cn_OT15$emmeans, Letters = letters)

# Display the C:N ratio means with letters
print(letters_cn_OT15)



# Fit LMM for MBC in Phalaris dataset
m1_MBC_phalaris <- lmer(MBC..mg.kg. ~ Dose * Fertilizer_Type * Application_Method + (1 | Block), data = Phalaris_exp2)

# Model Diagnostics
plot(m1_MBC_phalaris)  # Residual plot
qqPlot(resid(m1_MBC_phalaris))  # Q-Q plot for normality

# ANOVA for significance testing
Anova(m1_MBC_phalaris, test = "F")

# Summary of the model
summary(m1_MBC_phalaris)

# Pairwise comparison for MBC
emmeans_mbc_phalaris <- emmeans(m1_MBC_phalaris, pairwise ~ Dose * Fertilizer_Type * Application_Method, adjust = "tukey")
letters_mbc_phalaris <- cld(emmeans_mbc_phalaris$emmeans, Letters = letters)

# Boxplot for MBC
ggplot(Phalaris_exp2, aes(x = Dose, y = MBC..mg.kg., fill = Fertilizer_Type)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ Application_Method) +
  theme_minimal() +
  labs(title = "Microbial Biomass Carbon (MBC) in Phalaris",
       x = "Nitrogen Dose (kg/ha)",
       y = "MBC (mg/kg)",
       fill = "Fertilizer Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Fit LMM for MBN in Phalaris dataset
m1_MBN_phalaris <- lmer(MBC.N..mg.kg. ~ Dose * Fertilizer_Type * Application_Method + (1 | Block), data = Phalaris_exp2)

# Model Diagnostics
plot(m1_MBN_phalaris)  # Residual plot
qqPlot(resid(m1_MBN_phalaris))  # Q-Q plot for normality

# ANOVA for significance testing
Anova(m1_MBN_phalaris, test = "F")

# Summary of the model
summary(m1_MBN_phalaris)

# Pairwise comparison for MBN
emmeans_mbn_phalaris <- emmeans(m1_MBN_phalaris, pairwise ~ Dose * Fertilizer_Type * Application_Method, adjust = "tukey")
letters_mbn_phalaris <- cld(emmeans_mbn_phalaris$emmeans, Letters = letters)

# Boxplot for MBN
ggplot(Phalaris_exp2, aes(x = Dose, y = MBC.N..mg.kg., fill = Fertilizer_Type)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ Application_Method) +
  theme_minimal() +
  labs(title = "Microbial Biomass Nitrogen (MBN) in Phalaris",
       x = "Nitrogen Dose (kg/ha)",
       y = "MBN (mg/kg)",
       fill = "Fertilizer Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate C:N ratio for Phalaris
Phalaris_exp2 <- Phalaris_exp2 %>%
  mutate(CN_ratio = MBC..mg.kg. / MBC.N..mg.kg.)

# Summary statistics for C:N ratio
summary_CN_phalaris <- Phalaris_exp2 %>%
  group_by(Fertilizer_Type, Dose, Application_Method, Treatment_Type) %>%
  summarise(
    CN_mean = mean(CN_ratio, na.rm = TRUE),
    CN_se = sd(CN_ratio, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Print summary
print(summary_CN_phalaris)

# Boxplot for C:N ratio
ggplot(Phalaris_exp2, aes(x = Dose, y = CN_ratio, fill = Fertilizer_Type)) +
  geom_boxplot(alpha = 0.7) +
  facet_grid(Treatment_Type ~ Application_Method) +
  theme_minimal() +
  labs(title = "Microbial Biomass C:N Ratio in Phalaris",
       x = "Nitrogen Dose (kg/ha)",
       y = "C:N Ratio",
       fill = "Fertilizer Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Step 1: Check for missing values
sum(is.na(Phalaris_exp2$CN_ratio))  # Count missing values

# Step 2: Remove missing values
Phalaris_exp2_clean <- na.omit(Phalaris_exp2)

# Step 3: Check Block levels
length(unique(Phalaris_exp2_clean$Block))  # Check if Block has too many levels

# Step 4: If needed, create a grouped Block factor
Phalaris_exp2_clean$Block_group <- as.factor(as.numeric(as.factor(Phalaris_exp2_clean$Block)) %% 5)

# Step 5: Fit LMM if random effect is appropriate
if (length(unique(Phalaris_exp2_clean$Block_group)) < nrow(Phalaris_exp2_clean)) {
  m1_CN_phalaris <- lmer(CN_ratio ~ Dose * Fertilizer_Type * Application_Method * Treatment_Type + (1 | Block_group), 
                         data = Phalaris_exp2_clean)
} else {
  # Step 6: Use GLS if LMM fails
  m1_CN_phalaris_gls <- gls(CN_ratio ~ Dose * Fertilizer_Type * Application_Method * Treatment_Type, 
                            data = Phalaris_exp2_clean)
  summary(m1_CN_phalaris_gls)
}



# Fit LMM for C:N ratio
m1_CN_phalaris <- lmer(CN_ratio ~ Dose * Fertilizer_Type * Application_Method * Treatment_Type + (1 | Block), data = Phalaris_exp2)

# Model Diagnostics
plot(m1_CN_phalaris)  # Residual plot
qqPlot(resid(m1_CN_phalaris))  # Q-Q plot for normality

# ANOVA for significance testing
Anova(m1_CN_phalaris, test = "F")

# Summary of the model
summary(m1_CN_phalaris)

# Pairwise comparisons for C:N ratio
emmeans_cn_phalaris <- emmeans(m1_CN_phalaris, pairwise ~ Dose * Fertilizer_Type * Application_Method * Treatment_Type, adjust = "tukey")
letters_cn_phalaris <- cld(emmeans_cn_phalaris$emmeans, Letters = letters)

# Display results
print(letters_cn_phalaris)



# Load necessary packages
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)

# Read the dataset (replace 'your_data.csv' with actual filename)
# Lucerne_exp2 <- read.csv("your_data.csv", header = TRUE)

# Summary statistics: Mean and Standard Error
summary_stats <- Lucerne_exp2 %>%
  group_by(Dose, Application_Method, Fertilizer_Type) %>%
  summarise(
    Mean_PYL = mean(Potential.Yield.lost, na.rm = TRUE),
    SE_PYL = sd(Potential.Yield.lost, na.rm = TRUE) / sqrt(n())
  )

print(summary_stats)

# Linear Mixed Model (LMM)
lmm_model <- lmer(Potential.Yield.lost ~ Dose + Application_Method + Fertilizer_Type + (1|Block), data = Lucerne_exp2)

# Model summary
summary(lmm_model)

# Visualization: Boxplot of Potential Yield Lost
ggplot(Lucerne_exp2, aes(x = as.factor(Dose), y = Potential.Yield.lost, fill = Fertilizer_Type)) +
  geom_boxplot() +
  facet_wrap(~ Application_Method) +
  labs(title = "Potential Yield Lost by Dose, Application Method, and Fertilizer Type",
       x = "Dose",
       y = "Potential Yield Lost") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

colnames(Phalaris_exp2)

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)

# Convert categorical variables to factors
Phalaris_exp2 <- Phalaris_exp2 %>%
  mutate(
    Dose = factor(Dose_.N.kg.ha., levels = c("0", "20", "40", "60", "80", "100", "120")),
    Fertilizer_Type = factor(Fertilizer_type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split")),
    Treatment_Type = factor(Treatment_Type, levels = c("Control", "Other"))
  )

# Descriptive statistics: Mean & Standard Error
summary_stats <- Phalaris_exp2 %>%
  group_by(Dose, Application_Method, Fertilizer_Type) %>%
  summarise(
    Mean_PYL = mean(Potential.Yield.lost, na.rm = TRUE),
    SE_PYL = sd(Potential.Yield.lost, na.rm = TRUE) / sqrt(n())
  )

print(summary_stats)

# Linear Mixed Model (LMM)
lmm_model <- lmer(Potential.Yield.lost ~ Dose + Application_Method + Fertilizer_Type + (1|Block), 
                  data = Phalaris_exp2)

# Model summary
summary(lmm_model)

# Visualization: Boxplot of Potential Yield Lost
ggplot(Phalaris_exp2, aes(x = Dose, y = Potential.Yield.lost, fill = Fertilizer_Type)) +
  geom_boxplot() +
  facet_wrap(~ Application_Method) +
  labs(title = "Potential Yield Lost in Phalaris by Dose, Application Method, and Fertilizer Type",
       x = "Dose",
       y = "Potential Yield Lost") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")






setwd("C:/Users/90958427/OneDrive - Western Sydney University/PolyTunnelS33_ExperimeNT1_dose/Experiment_2/Modified_Data_File")
list.files()
DP_Phalaris <- read.csv("Dispersion_Percentage_Phalaris.csv", check.names = TRUE)
head(DP_Phalaris)




# Load necessary libraries
library(ggplot2)
library(dplyr)
library(ggpubr)
library(agricolae)

# Read dataset (assuming your data is named DP_Phalaris)
# DP_Phalaris <- read.csv("your_file.csv")

# Selecting relevant columns
df <- DP_Phalaris %>%
  select(Sample.ID, Dose, APPLICATION.TYPE, Fertilizer.type, DP.) %>%
  rename(DP = DP.)  # Renaming column for easy use

# Convert categorical variables to factors
df$Dose <- as.factor(df$Dose)
df$APPLICATION.TYPE <- as.factor(df$APPLICATION.TYPE)
df$Fertilizer.type <- as.factor(df$Fertilizer.type)

# Compute summary statistics (Mean and Standard Error)
summary_stats <- df %>%
  group_by(Dose, APPLICATION.TYPE, Fertilizer.type) %>%
  summarise(
    Mean_DP = mean(DP, na.rm = TRUE),
    Std_Error = sd(DP, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Print summary statistics
print(summary_stats)

# ANOVA Test
anova_model <- aov(DP ~ Dose * APPLICATION.TYPE * Fertilizer.type, data = df)
anova_result <- summary(anova_model)
print(anova_result)  # Print ANOVA results

# Tukey's HSD test using HSD.test
tukey_result <- HSD.test(anova_model, "Dose:APPLICATION.TYPE:Fertilizer.type", group = TRUE)

# Check if tukey_result contains significance groups
if (!is.null(tukey_result$groups)) {
  # Extract significance letters
  letters_table <- as.data.frame(tukey_result$groups)
  letters_table$Group <- rownames(letters_table)  # Ensure Group column exists
  colnames(letters_table) <- c("Mean_DP", "Significance", "Group")
  
  # Merge significance letters with summary stats
  summary_stats <- summary_stats %>%
    mutate(Group = paste(Dose, APPLICATION.TYPE, Fertilizer.type, sep = ":")) %>%
    left_join(letters_table, by = "Group")
  
  # Ensure no missing significance values
  summary_stats$Significance[is.na(summary_stats$Significance)] <- ""
} else {
  # If no significance groups are generated, assign empty values
  summary_stats$Significance <- ""
}

# Check that Group column exists before plotting
if (!"Group" %in% colnames(summary_stats)) {
  summary_stats$Group <- paste(summary_stats$Dose, summary_stats$APPLICATION.TYPE, summary_stats$Fertilizer.type, sep = ":")
}

# Visualizing using a Boxplot with statistical comparisons
p <- ggplot(df, aes(x = interaction(Dose, APPLICATION.TYPE, Fertilizer.type), y = DP, fill = APPLICATION.TYPE)) +
  geom_boxplot(outlier.shape = NA) + # Remove outliers for better visibility
  geom_jitter(width = 0.2, alpha = 0.5) + # Add jittered points
  stat_compare_means(method = "anova", label = "p.signif") + 
  geom_text(data = summary_stats, aes(x = Group, y = max(df$DP, na.rm = TRUE) + 0.05, label = Significance), vjust = -0.5) + # Add significance letters
  labs(title = "Impact of Dose, Application Type, and Fertilizer on Dispersion Percentage",
       x = "Dose, Application Type, and Fertilizer Type", 
       y = "Dispersion Percentage (DP)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels

# Display the plot
print(p)


# Load necessary libraries
library(ggplot2)
library(dplyr)

# Read dataset
DP_Composite_Lucerne <- read.csv("Composite_bulk_DP.csv", check.names = TRUE)

# Selecting relevant columns and renaming them
df <- DP_Composite_Lucerne %>%
  select(Sample.ID, Dose, X, X.1, DP.) %>%
  rename(APPLICATION.TYPE = X, Fertilizer.type = X.1, DP = DP.)  # Rename columns for clarity

# Convert categorical variables to factors
df$APPLICATION.TYPE <- as.factor(df$APPLICATION.TYPE)
df$Fertilizer.type <- as.factor(df$Fertilizer.type)

# Ensure Dose is in numerical order
df$Dose <- as.numeric(as.character(df$Dose))  # Convert Dose to numeric for proper ordering
df <- df %>% arrange(Dose)  # Arrange dataset by Dose

# Generate Simple Boxplot
p <- ggplot(df, aes(x = factor(Dose, levels = unique(Dose)), y = DP, fill = APPLICATION.TYPE)) +
  geom_boxplot(width = 0.5, outlier.shape = NA) +  # Boxplot
  geom_jitter(width = 0.1, size = 3, alpha = 0.7, aes(color = Fertilizer.type)) +  # Show individual points
  labs(title = "Effect of Dose, Application Type, and Fertilizer Type on DP",
       x = "Dose (Ordered)", y = "Dispersion Percentage (DP)", fill = "Application Type", color = "Fertilizer Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

# Display the plot
print(p)

colnames(DP_Composite_Lucerne)






# Load necessary libraries
library(ggplot2)
library(dplyr)

# Read dataset
DP_Composite_Lucerne <- read.csv("Composite_bulk_DP.csv", check.names = TRUE)

# Selecting relevant columns and renaming them
df <- DP_Composite_Lucerne %>%
  select(Dose, Application.type, Fertilizer.type, DP.) %>%
  rename(DP = DP.)  # Rename DP column for simplicity

# Convert categorical variables to factors
df$Dose <- as.numeric(as.character(df$Dose))  # Ensure Dose is numeric for ordering
df$Application.type <- as.factor(df$Application.type)
df$Fertilizer.type <- as.factor(df$Fertilizer.type)

# Generate Grouped Bar Chart
p <- ggplot(df, aes(x = factor(Dose, levels = sort(unique(Dose))), y = DP, fill = Fertilizer.type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "black") +  # Grouped bars
  facet_wrap(~Application.type) +  # Separate by Application Type
  labs(title = "Histogram of DP (%) with Respect to Dose, Differentiated by Fertilizer Type",
       x = "Dose", y = "DP (%)", fill = "Fertilizer Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

# Display the plot
print(p)

