#Soilnutrients data anlysis harvest day (NH4,NO3, PO4)
## load libraries
library(tidyverse)
library(car)
library(emmeans)
library(ggplot2)
library(emmeans)
library(multcomp)
Soil_Nutrients<- read.csv("Soil_Nutrients_Extraction_EXP1.csv")
head(Soil_Nutrients)

# Ensure that Dose, Plant.type, and Fertilizer.type are treated as factors for analysis
Soil_Nutrients$Dose <- factor(Soil_Nutrients$Dose..N.kg.ha.)
Soil_Nutrients$Plant.type <- factor(Soil_Nutrients$Plant.type)
Soil_Nutrients$Fertilizer.type <- factor(Soil_Nutrients$Fertilizer.type)

# Calculate mean and standard error for each group (Dose, Plant type, Fertilizer type)
soil_summary <- Soil_Nutrients %>%
  group_by(Dose, Plant.type, Fertilizer.type) %>%
  summarize(
    NH4_mean = mean(NH4.g.kg..dw.soil, na.rm = TRUE),
    NH4_se = sd(NH4.g.kg..dw.soil, na.rm = TRUE) / sqrt(n()),
    NO3_mean = mean(NO3.g.kg..dw.soil, na.rm = TRUE),
    NO3_se = sd(NO3.g.kg..dw.soil, na.rm = TRUE) / sqrt(n()),
    SPAD_mean = mean(SPAD.Data..nmol.ch.mg.fresh.weight., na.rm = TRUE),
    SPAD_se = sd(SPAD.Data..nmol.ch.mg.fresh.weight., na.rm = TRUE) / sqrt(n())
  )

# 1. Visualize Soil Ammonium by Dose, Plant Type, and Fertilizer Type
ggplot(soil_summary, aes(x = Dose, y = NH4_mean, fill = Fertilizer.type)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = NH4_mean - NH4_se, ymax = NH4_mean + NH4_se), width = 0.2, position = position_dodge(0.9), color = "black") +
  facet_wrap(~ Plant.type) +
  labs(title = "Soil Ammonium by Dose, Plant Type, and Fertilizer Type", x = "Dose", y = "Ammonium (g/kg dw soil)") +
  scale_fill_manual(values = c("grey20", "grey50", "grey80")) + # Set custom colors for Fertilizer.type
  theme_minimal(base_size = 15) +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = "grey"))

# 2. Visualize Soil Nitrate by Dose, Plant Type, and Fertilizer Type
ggplot(soil_summary, aes(x = Dose, y = NO3_mean, fill = Fertilizer.type)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = NO3_mean - NO3_se, ymax = NO3_mean + NO3_se), width = 0.2, position = position_dodge(0.9), color = "black") +
  facet_wrap(~ Plant.type) +
  labs(title = "Soil Nitrate by Dose, Plant Type, and Fertilizer Type", x = "Dose", y = "Nitrate (g/kg dw soil)") +
  scale_fill_manual(values = c("grey20", "grey50", "grey80")) + # Set custom colors for Fertilizer.type
  theme_minimal(base_size = 15) +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = "grey"))

# 3. Visualize SPAD values (chlorophyll content) by Dose, Plant Type, and Fertilizer Type
ggplot(soil_summary, aes(x = Dose, y = SPAD_mean, fill = Fertilizer.type)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = SPAD_mean - SPAD_se, ymax = SPAD_mean + SPAD_se), width = 0.2, position = position_dodge(0.9), color = "black") +
  facet_wrap(~ Plant.type) +
  labs(title = "SPAD (Chlorophyll Content) by Dose, Plant Type, and Fertilizer Type", x = "Dose", y = "SPAD (nmol chlorophyll/mg fresh weight)") +
  scale_fill_manual(values = c("grey20", "grey50", "grey80")) + # Set custom colors for Fertilizer.type
  theme_minimal(base_size = 15) +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = "grey"))

# 4. Fit the model for NH4 data
m1_nh4 <- lm(NH4.g.kg..dw.soil ~ Dose * Plant.type * Fertilizer.type, data = Soil_Nutrients)
summary(m1_nh4)

# Check residuals for linearity and normality
residualPlot(m1_nh4)
qqPlot(m1_nh4)

# Test significance of the model effects (ANOVA)
Anova(m1_nh4, type = "II")

# Perform multiple comparison test (Tukey HSD) for NH4 data
pairwise_comparisons_nh4 <- emmeans(m1_nh4, ~ Dose | Plant.type | Fertilizer.type)
summary(pairwise_comparisons_nh4)

# Visualize significant differences using compact letter display
cld(pairwise_comparisons_nh4)

# 5. Fit the model for NO3 data
m1_no3 <- lm(NO3.g.kg..dw.soil ~ Dose * Plant.type * Fertilizer.type, data = Soil_Nutrients)
summary(m1_no3)

# Check residuals for linearity and normality
residualPlot(m1_no3)
qqPlot(m1_no3)

# Test significance of the model effects (ANOVA)
Anova(m1_no3, type = "II")

# Perform multiple comparison test (Tukey HSD) for NO3 data
pairwise_comparisons_no3 <- emmeans(m1_no3, ~ Dose | Plant.type | Fertilizer.type)
summary(pairwise_comparisons_no3)

# Visualize significant differences using compact letter display
cld(pairwise_comparisons_no3)

# 6. Fit the model for SPAD data
m1_spad <- lm(SPAD.Data..nmol.ch.mg.fresh.weight. ~ Dose * Plant.type * Fertilizer.type, data = Soil_Nutrients)
summary(m1_spad)

# Check residuals for linearity and normality
residualPlot(m1_spad)
qqPlot(m1_spad)

# Test significance of the model effects (ANOVA)
Anova(m1_spad, type = "II")

# Perform multiple comparison test (Tukey HSD) for SPAD data
pairwise_comparisons_spad <- emmeans(m1_spad, ~ Dose | Plant.type | Fertilizer.type)
summary(pairwise_comparisons_spad)

# Visualize significant differences using compact letter display
cld(pairwise_comparisons_spad)


# Correlation Analysis

# Correlation between SPAD readings and soil ammonium, handling missing values
cor_ammonium_spad <- cor(Soil_Nutrients$SPAD.Data..nmol.ch.mg.fresh.weight., 
                         Soil_Nutrients$NH4.g.kg..dw.soil, method = "pearson", use = "complete.obs")
print(cor_ammonium_spad)

# Correlation between SPAD readings and soil nitrate, handling missing values
cor_nitrate_spad <- cor(Soil_Nutrients$SPAD.Data..nmol.ch.mg.fresh.weight., 
                        Soil_Nutrients$NO3.g.kg..dw.soil, method = "pearson", use = "complete.obs")
print(cor_nitrate_spad)

# Modeling Interaction with Treatment (UrVAL vs. MF)

# Fit the model including Treatment for Nitrate
m1_treatment <- lm(log10(NO3.g.kg..dw.soil) ~ Dose * Plant.type * Fertilizer.type, data = Soil_Nutrients)
summary(m1_treatment)

# Check residuals for model with Treatment
residualPlot(m1_treatment)
qqPlot(m1_treatment)

# Test significance of the model effects (ANOVA)
Anova(m1_treatment)

# Perform pairwise comparison test for Treatment
pairwise_comparisons_treatment <- emmeans(m1_treatment, ~ Dose | Plant.type | Fertilizer.type)
summary(pairwise_comparisons_treatment)

# Visualize significant differences with compact letter display
cld(pairwise_comparisons_treatment)

# 3. Correlation Analysis between SPAD Readings and Soil Nutrients

# Correlation between SPAD readings and soil ammonium (NH4), handling missing values
cor_ammonium_spad <- cor(Soil_Nutrients$SPAD.Data..nmol.ch.mg.fresh.weight., 
                         Soil_Nutrients$NH4.g.kg..dw.soil, method = "pearson", use = "complete.obs")
print(cor_ammonium_spad)

# Correlation between SPAD readings and soil nitrate (NO3), handling missing values
cor_nitrate_spad <- cor(Soil_Nutrients$SPAD.Data..nmol.ch.mg.fresh.weight., 
                        Soil_Nutrients$NO3.g.kg..dw.soil, method = "pearson", use = "complete.obs")
print(cor_nitrate_spad)

# NH4 Data: Perform multiple comparison test (Tukey HSD) and get compact letter display
pairwise_comparisons_nh4 <- emmeans(m1_nh4, ~ Dose | Plant.type | Fertilizer.type)
cld_nh4 <- cld(pairwise_comparisons_nh4, Letters = letters, adjust = "tukey")

# Join the cld results with soil_summary
cld_nh4 <- as.data.frame(cld_nh4)
merged_nh4 <- merge(soil_summary, cld_nh4, by = c("Dose", "Plant.type", "Fertilizer.type"))

# Plot NH4 data with compact letter display (CLD) on the plot
ggplot(merged_nh4, aes(x = Dose, y = NH4_mean, fill = Fertilizer.type)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = NH4_mean - NH4_se, ymax = NH4_mean + NH4_se), width = 0.2, position = position_dodge(0.9), color = "black") +
  facet_wrap(~ Plant.type) +
  geom_text(aes(x = Dose, y = NH4_mean + NH4_se + 0.05, label = .group), position = position_dodge(0.9), vjust = 0, color = "black") +
  labs(title = "Soil Ammonium by Dose, Plant Type, and Fertilizer Type", x = "Dose", y = "Ammonium (g/kg dw soil)") +
  scale_fill_manual(values = c("grey20", "grey50", "grey80")) +
  theme_minimal(base_size = 15) +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = "grey"))
# NO3 Data: Perform multiple comparison test (Tukey HSD) and get compact letter display
pairwise_comparisons_no3 <- emmeans(m1_no3, ~ Dose | Plant.type | Fertilizer.type)
cld_no3 <- cld(pairwise_comparisons_no3, Letters = letters, adjust = "tukey")

# Join the cld results with soil_summary
cld_no3 <- as.data.frame(cld_no3)
merged_no3 <- merge(soil_summary, cld_no3, by = c("Dose", "Plant.type", "Fertilizer.type"))

# Plot NO3 data with compact letter display (CLD) on the plot
ggplot(merged_no3, aes(x = Dose, y = NO3_mean, fill = Fertilizer.type)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = NO3_mean - NO3_se, ymax = NO3_mean + NO3_se), width = 0.2, position = position_dodge(0.9), color = "black") +
  facet_wrap(~ Plant.type) +
  geom_text(aes(x = Dose, y = NO3_mean + NO3_se + 0.05, label = .group), position = position_dodge(0.9), vjust = 0, color = "black") +
  labs(title = "Soil Nitrate by Dose, Plant Type, and Fertilizer Type", x = "Dose", y = "Nitrate (g/kg dw soil)") +
  scale_fill_manual(values = c("grey20", "grey50", "grey80")) +
  theme_minimal(base_size = 15) +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = "grey"))
# SPAD Data: Perform multiple comparison test (Tukey HSD) and get compact letter display
pairwise_comparisons_spad <- emmeans(m1_spad, ~ Dose | Plant.type | Fertilizer.type)
cld_spad <- cld(pairwise_comparisons_spad, Letters = letters, adjust = "tukey")

# Join the cld results with soil_summary
cld_spad <- as.data.frame(cld_spad)
merged_spad <- merge(soil_summary, cld_spad, by = c("Dose", "Plant.type", "Fertilizer.type"))

# Plot SPAD data with compact letter display (CLD) on the plot
ggplot(merged_spad, aes(x = Dose, y = SPAD_mean, fill = Fertilizer.type)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = SPAD_mean - SPAD_se, ymax = SPAD_mean + SPAD_se), width = 0.2, position = position_dodge(0.9), color = "black") +
  facet_wrap(~ Plant.type) +
  geom_text(aes(x = Dose, y = SPAD_mean + SPAD_se + 0.05, label = .group), position = position_dodge(0.9), vjust = 0, color = "black") +
  labs(title = "SPAD (Chlorophyll Content) by Dose, Plant Type, and Fertilizer Type", x = "Dose", y = "SPAD (nmol chlorophyll/mg fresh weight)") +
  scale_fill_manual(values = c("grey20", "grey50", "grey80")) +
  theme_minimal(base_size = 15) +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = "grey"))




# Load necessary libraries
library(emmeans)
library(ggplot2)

# Assuming the model has been fit previously
# Fit the model
m1_spad <- lm(SPAD.Data..nmol.ch.mg.fresh.weight. ~ Dose * Plant.type * Fertilizer.type, data = Soil_Nutrients)

# Perform pairwise comparisons using emmeans
pairwise_comparisons_spad <- emmeans(m1_spad, ~ Dose | Plant.type | Fertilizer.type)

# Get compact letter display (CLD)
cld_results <- cld(pairwise_comparisons_spad, Letters = letters, adjust = "tukey")

# Print the compact letter display results
print(cld_results)

# Check the structure to ensure we have the `.group` column for letter groupings
str(cld_results)
# Check the structure of 'cld_results' and 'merged_spad' to make sure they can be merged
print(str(cld_results))
print(str(merged_spad))

# Check the structure of cld_results
print("cld_results structure:")
str(cld_results)

# Check the structure of merged_spad
print("merged_spad structure:")
str(merged_spad)

# Check the first few rows to inspect the columns in cld_results
print("cld_results preview:")
head(cld_results)

# Check the first few rows to inspect the columns in merged_spad
print("merged_spad preview:")
head(merged_spad)
# Ensure cld_results is a data frame
cld_data <- as.data.frame(cld_results)

# Perform the merge based on the common columns (Dose, Plant.type, Fertilizer.type)
merged_spad <- merge(merged_spad, cld_data[, c("Dose", "Plant.type", "Fertilizer.type", ".group")],
                     by = c("Dose", "Plant.type", "Fertilizer.type"),
                     all.x = TRUE)

# Rename .group column to updated_grouping
names(merged_spad)[names(merged_spad) == ".group"] <- "updated_grouping"

# Check if the grouping letters have been added correctly
print("Merged data preview:")
head(merged_spad)
# Now plot the data with the compact letter display (CLD) on top of the bars
ggplot(merged_spad, aes(x = factor(Dose), y = SPAD_mean, fill = Fertilizer.type)) + 
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.9), 
           color = "black", 
           width = ifelse(merged_spad$Dose == 0, 0.45, 0.7)) +  # Control bar width for Dose 0
  geom_errorbar(aes(ymin = SPAD_mean - SPAD_se, ymax = SPAD_mean + SPAD_se), 
                width = 0.2, position = position_dodge(0.9), color = "black") +
  facet_wrap(~ Plant.type) +
  geom_text(aes(x = factor(Dose), y = SPAD_mean + SPAD_se + 0.5, label = updated_grouping), 
            position = position_dodge(0.9), vjust = -0.5, color = "black") +
  labs(title = "SPAD Data(Chlorophyll Content)", 
       x = "Dose", y = "SPAD (nmol chlorophyll/mg fresh weight)") +
  scale_fill_manual(values = c("grey20", "grey50", "grey80")) +  # Set custom colors for Fertilizer type
  theme_minimal(base_size = 15) +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = "grey"))




















# Load required libraries
library(tidyverse)
library(car)
library(emmeans)
library(ggplot2)
library(multcomp)

# Set working directory and load data
setwd("C:\\Users\\90958427\\OneDrive - Western Sydney University\\PolyTunnelS33_ExperimeNT1_dose\\Modified Data File")
Soil_Nutrients <- read.csv("Soil_Nutrients_Extraction_EXP1.csv")

# Ensure that Dose, Plant.type, and Fertilizer.type are treated as factors for analysis
Soil_Nutrients$Dose <- factor(Soil_Nutrients$Dose..N.kg.ha.)
Soil_Nutrients$Plant.type <- factor(Soil_Nutrients$Plant.type)
Soil_Nutrients$Fertilizer.type <- factor(Soil_Nutrients$Fertilizer.type)

# Summarize means and standard errors for each group (Dose, Plant type, Fertilizer type)
soil_summary <- Soil_Nutrients %>%
  group_by(Dose, Plant.type, Fertilizer.type) %>%
  summarize(
    NH4_mean = mean(NH4.g.kg..dw.soil, na.rm = TRUE),
    NH4_se = sd(NH4.g.kg..dw.soil, na.rm = TRUE) / sqrt(n()),
    NO3_mean = mean(NO3.g.kg..dw.soil, na.rm = TRUE),
    NO3_se = sd(NO3.g.kg..dw.soil, na.rm = TRUE) / sqrt(n()),
    SPAD_mean = mean(SPAD.Data..nmol.ch.mg.fresh.weight., na.rm = TRUE),
    SPAD_se = sd(SPAD.Data..nmol.ch.mg.fresh.weight., na.rm = TRUE) / sqrt(n())
  )

### NH4 Analysis ###
# Fit the model for NH4 data
m1_nh4 <- lm(NH4.g.kg..dw.soil ~ Dose * Plant.type * Fertilizer.type, data = Soil_Nutrients)
summary(m1_nh4)

# Check residuals for linearity and normality
residualPlot(m1_nh4)
qqPlot(m1_nh4)

# Test significance of the model effects (ANOVA)
Anova(m1_nh4, type = "II")

# Perform Tukey test and get compact letter display
pairwise_comparisons_nh4 <- emmeans(m1_nh4, ~ Dose | Plant.type | Fertilizer.type)
cld_nh4 <- cld(pairwise_comparisons_nh4, Letters = letters, adjust = "tukey")

# Merge Tukey test results with the soil summary
cld_nh4 <- as.data.frame(cld_nh4)
merged_nh4 <- merge(soil_summary, cld_nh4, by = c("Dose", "Plant.type", "Fertilizer.type"))

# Plot NH4 data with compact letter display
ggplot(merged_nh4, aes(x = Dose, y = NH4_mean, fill = Fertilizer.type)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = NH4_mean - NH4_se, ymax = NH4_mean + NH4_se), width = 0.2, position = position_dodge(0.9), color = "black") +
  facet_wrap(~ Plant.type) +
  geom_text(aes(x = Dose, y = NH4_mean + NH4_se + 0.05, label = .group), position = position_dodge(0.9), vjust = 0, color = "black") +
  labs(title = "Soil Ammonium by Dose, Plant Type, and Fertilizer Type", x = "Dose", y = "Ammonium (g/kg dw soil)") +
  scale_fill_manual(values = c("grey20", "grey50", "grey80")) +
  theme_minimal(base_size = 15) +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = "grey"))

### NO3 Analysis ###
# Fit the model for NO3 data
m1_no3 <- lm(NO3.g.kg..dw.soil ~ Dose * Plant.type * Fertilizer.type, data = Soil_Nutrients)
summary(m1_no3)

# Check residuals for linearity and normality
residualPlot(m1_no3)
qqPlot(m1_no3)

# Test significance of the model effects (ANOVA)
Anova(m1_no3, type = "II")

# Perform Tukey test and get compact letter display
pairwise_comparisons_no3 <- emmeans(m1_no3, ~ Dose | Plant.type | Fertilizer.type)
cld_no3 <- cld(pairwise_comparisons_no3, Letters = letters, adjust = "tukey")

# Merge Tukey test results with the soil summary
cld_no3 <- as.data.frame(cld_no3)
merged_no3 <- merge(soil_summary, cld_no3, by = c("Dose", "Plant.type", "Fertilizer.type"))

# Plot NO3 data with compact letter display
ggplot(merged_no3, aes(x = Dose, y = NO3_mean, fill = Fertilizer.type)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = NO3_mean - NO3_se, ymax = NO3_mean + NO3_se), width = 0.2, position = position_dodge(0.9), color = "black") +
  facet_wrap(~ Plant.type) +
  geom_text(aes(x = Dose, y = NO3_mean + NO3_se + 0.05, label = .group), position = position_dodge(0.9), vjust = 0, color = "black") +
  labs(title = "Soil Nitrate by Dose, Plant Type, and Fertilizer Type", x = "Dose", y = "Nitrate (g/kg dw soil)") +
  scale_fill_manual(values = c("grey20", "grey50", "grey80")) +
  theme_minimal(base_size = 15) +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = "grey"))

### SPAD Analysis ###
# Fit the model for SPAD data
m1_spad <- lm(SPAD.Data..nmol.ch.mg.fresh.weight. ~ Dose * Plant.type * Fertilizer.type, data = Soil_Nutrients)
summary(m1_spad)

# Check residuals for linearity and normality
residualPlot(m1_spad)
qqPlot(m1_spad)

# Test significance of the model effects (ANOVA)
Anova(m1_spad, type = "II")

# Perform Tukey test and get compact letter display
pairwise_comparisons_spad <- emmeans(m1_spad, ~ Dose | Plant.type | Fertilizer.type)
cld_spad <- cld(pairwise_comparisons_spad, Letters = letters, adjust = "tukey")

# Merge Tukey test results with the soil summary
cld_spad <- as.data.frame(cld_spad)
merged_spad <- merge(soil_summary, cld_spad, by = c("Dose", "Plant.type", "Fertilizer.type"))

# Plot SPAD data with compact letter display
ggplot(merged_spad, aes(x = Dose, y = SPAD_mean, fill = Fertilizer.type)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = SPAD_mean - SPAD_se, ymax = SPAD_mean + SPAD_se), width = 0.2, position = position_dodge(0.9), color = "black") +
  facet_wrap(~ Plant.type) +
  geom_text(aes(x = Dose, y = SPAD_mean + SPAD_se + 0.05, label = .group), position = position_dodge(0.9), vjust = 0, color = "black") +
  labs(title = "SPAD (Chlorophyll Content) by Dose, Plant Type, and Fertilizer Type", x = "Dose", y = "SPAD (nmol chlorophyll/mg fresh weight)") +
  scale_fill_manual(values = c("grey20", "grey50", "grey80")) +
  theme_minimal(base_size = 15) +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = "grey"))

### Correlation Analysis ###
# Correlation between SPAD readings and soil ammonium (NH4), handling missing values
cor_ammonium_spad <- cor(Soil_Nutrients$SPAD.Data..nmol.ch.mg.fresh.weight., 
                         Soil_Nutrients$NH4.g.kg..dw.soil, method = "pearson", use = "complete.obs")
print(paste("Correlation between SPAD and NH4:", cor_ammonium_spad))

# Correlation between SPAD readings and soil nitrate (NO3), handling missing values
cor_nitrate_spad <- cor(Soil_Nutrients$SPAD.Data..nmol.ch.mg.fresh.weight., 
                        Soil_Nutrients$NO3.g.kg..dw.soil, method = "pearson", use = "complete.obs")
print(paste("Correlation between SPAD and NO3:", cor_nitrate_spad))

### Treatment Analysis (UrVAL vs. MF) ###
# Fit the model including Treatment for Nitrate (log-transformed)
m1_treatment <- lm(log10(NO3.g.kg..dw.soil) ~ Dose * Plant.type * Fertilizer.type, data = Soil_Nutrients)
summary(m1_treatment)

# Check residuals for model with Treatment
residualPlot(m1_treatment)
qqPlot(m1_treatment)

# Test significance of the model effects (ANOVA)
Anova(m1_treatment)

# Perform pairwise comparison test for Treatment
pairwise_comparisons_treatment <- emmeans(m1_treatment, ~ Dose | Plant.type | Fertilizer.type)
summary(pairwise_comparisons_treatment)

# Visualize significant differences with compact letter display
cld_treatment <- cld(pairwise_comparisons_treatment, Letters = letters, adjust = "tukey")
cld_treatment
