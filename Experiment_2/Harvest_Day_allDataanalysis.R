# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lme4)
library(car)
library(corrplot)
library(FactoMineR)
library(factoextra)

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(car)
library(emmeans)
library(multcompView)

# Set working directory
setwd("C:/Users/90958427/OneDrive - Western Sydney University/PolyTunnelS33_ExperimeNT1_dose/Experiment_2")

# Load the dataset
Lucerne_exp2 <- read.csv("Lucerne_FinalData_analysis.csv", check.names = TRUE)

# Check the structure of the dataset
str(Lucerne_exp2)

# View the first few rows
head(Lucerne_exp2)
colnames(Lucerne_exp2)
# Convert variables to factors
Lucerne_exp2 <- Lucerne_exp2 %>%
  mutate(
    Dose = factor(Dose...N.kg.ha., levels = c("0", "10", "20", "30", "40", "50", "60")),
    Fertilizer_Type = factor(Fertilizer.type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split"))
  )

# Check the transformed dataset
str(Lucerne_exp2)

# Calculate mean and SE for all relevant variables
summary_lucerne <- Lucerne_exp2 %>%
  group_by(Dose, Fertilizer_Type, Application_Method) %>%
  summarise(
    Mean_Fresh_Biomass_Shoot = mean(Fresh_Biomass_4, na.rm = TRUE),
    SE_Fresh_Biomass_Shoot = sd(Fresh_Biomass_4, na.rm = TRUE) / sqrt(n()),
    
    Mean_Dry_Biomass_Shoot = mean(Dry_Biomass_shoot_4, na.rm = TRUE),
    SE_Dry_Biomass_Shoot = sd(Dry_Biomass_shoot_4, na.rm = TRUE) / sqrt(n()),
    
    Mean_Fresh_Biomass_Root = mean(Fresh_Biomass_root_118days, na.rm = TRUE),
    SE_Fresh_Biomass_Root = sd(Fresh_Biomass_root_118days, na.rm = TRUE) / sqrt(n()),
    
    Mean_Dry_Biomass_Root = mean(Dry_Biomass_root_118days, na.rm = TRUE),
    SE_Dry_Biomass_Root = sd(Dry_Biomass_root_118days, na.rm = TRUE) / sqrt(n()),
    
    Mean_Chlorophyll = mean(Cholorphyll_content.HarvestDay., na.rm = TRUE),
    SE_Chlorophyll = sd(Cholorphyll_content.HarvestDay., na.rm = TRUE) / sqrt(n()),
    
    Mean_pH = mean(pH_8, na.rm = TRUE),
    SE_pH = sd(pH_8, na.rm = TRUE) / sqrt(n()),
    
    Mean_EC = mean(EC._8, na.rm = TRUE),
    SE_EC = sd(EC._8, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

# View the summary
head(summary_lucerne)

# Linear model for Fresh Biomass (Shoot)
m1_luc_fresh_shoot <- lm(Fresh_Biomass_4 ~ Dose * Fertilizer_Type * Application_Method, data = Lucerne_exp2)
summary(m1_luc_fresh_shoot)

# Check residuals
residualPlot(m1_luc_fresh_shoot)
qqPlot(m1_luc_fresh_shoot)
Anova(m1_luc_fresh_shoot, type = "II")

# Linear model for Dry Biomass (Shoot)
m1_luc_dry_shoot <- lm(Dry_Biomass_shoot_4 ~ Dose * Fertilizer_Type * Application_Method, data = Lucerne_exp2)
summary(m1_luc_dry_shoot)

# Check residuals
residualPlot(m1_luc_dry_shoot)
qqPlot(m1_luc_dry_shoot)
Anova(m1_luc_dry_shoot, type = "II")

# Linear model for Fresh Biomass (Root)
m1_luc_fresh_root <- lm(Fresh_Biomass_root_118days ~ Dose * Fertilizer_Type * Application_Method, data = Lucerne_exp2)
summary(m1_luc_fresh_root)

# Check residuals
residualPlot(m1_luc_fresh_root)
qqPlot(m1_luc_fresh_root)
Anova(m1_luc_fresh_root, type = "II")

# Linear model for Dry Biomass (Root)
m1_luc_dry_root <- lm(Dry_Biomass_root_118days ~ Dose * Fertilizer_Type * Application_Method, data = Lucerne_exp2)
summary(m1_luc_dry_root)

# Check residuals
residualPlot(m1_luc_dry_root)
qqPlot(m1_luc_dry_root)
Anova(m1_luc_dry_root, type = "II")

# Linear model for Chlorophyll Content
m1_luc_chlorophyll <- lm(Cholorphyll_content.HarvestDay. ~ Dose * Fertilizer_Type * Application_Method, data = Lucerne_exp2)
summary(m1_luc_chlorophyll)

# Check residuals
residualPlot(m1_luc_chlorophyll)
qqPlot(m1_luc_chlorophyll)
Anova(m1_luc_chlorophyll, type = "II")

# Linear model for pH
m1_luc_pH <- lm(pH_8 ~ Dose * Fertilizer_Type * Application_Method, data = Lucerne_exp2)
summary(m1_luc_pH)

# Check residuals
residualPlot(m1_luc_pH)
qqPlot(m1_luc_pH)
Anova(m1_luc_pH, type = "II")

# Linear model for EC
m1_luc_EC <- lm(EC._8 ~ Dose * Fertilizer_Type * Application_Method, data = Lucerne_exp2)
summary(m1_luc_EC)

# Check residuals
residualPlot(m1_luc_EC)
qqPlot(m1_luc_EC)
Anova(m1_luc_EC, type = "II")


# Install the necessary packages (if not already installed)
install.packages("emmeans")
install.packages("multcomp")
install.packages("stringr")


# Load necessary libraries
library(emmeans)
library(multcomp)
library(stringr)
library(dplyr)

# Pairwise comparisons for Fresh Biomass (Shoot)
pairwise_comparisons_fresh_shoot <- emmeans(m1_luc_fresh_shoot, ~ Dose * Fertilizer_Type | Application_Method)

# Generate compact letter display
letters_fresh_shoot <- multcomp::cld(pairwise_comparisons_fresh_shoot, Letters = letters) %>%
  mutate(.group = stringr::str_trim(.group)) %>%
  as.data.frame()

# View the results
head(letters_fresh_shoot)

# Pairwise comparisons for Dry Biomass (Shoot)
pairwise_comparisons_dry_shoot <- emmeans(m1_luc_dry_shoot, ~ Dose * Fertilizer_Type | Application_Method)

# Generate compact letter display
letters_dry_shoot <- multcomp::cld(pairwise_comparisons_dry_shoot, Letters = letters) %>%
  mutate(.group = stringr::str_trim(.group)) %>%
  as.data.frame()

# View the results
head(letters_dry_shoot)

# Pairwise comparisons for Fresh Biomass (Root)
pairwise_comparisons_fresh_root <- emmeans(m1_luc_fresh_root, ~ Dose * Fertilizer_Type | Application_Method)

# Generate compact letter display
letters_fresh_root <- multcomp::cld(pairwise_comparisons_fresh_root, Letters = letters) %>%
  mutate(.group = stringr::str_trim(.group)) %>%
  as.data.frame()

# View the results
head(letters_fresh_root)
# Pairwise comparisons for Dry Biomass (Root)
pairwise_comparisons_dry_root <- emmeans(m1_luc_dry_root, ~ Dose * Fertilizer_Type | Application_Method)

# Generate compact letter display
letters_dry_root <- multcomp::cld(pairwise_comparisons_dry_root, Letters = letters) %>%
  mutate(.group = stringr::str_trim(.group)) %>%
  as.data.frame()

# View the results
head(letters_dry_root)
# Pairwise comparisons for Chlorophyll Content
pairwise_comparisons_chlorophyll <- emmeans(m1_luc_chlorophyll, ~ Dose * Fertilizer_Type | Application_Method)

# Generate compact letter display
letters_chlorophyll <- multcomp::cld(pairwise_comparisons_chlorophyll, Letters = letters) %>%
  mutate(.group = stringr::str_trim(.group)) %>%
  as.data.frame()

# View the results
head(letters_chlorophyll)
# Pairwise comparisons for pH
pairwise_comparisons_pH <- emmeans(m1_luc_pH, ~ Dose * Fertilizer_Type | Application_Method)

# Generate compact letter display
letters_pH <- multcomp::cld(pairwise_comparisons_pH, Letters = letters) %>%
  mutate(.group = stringr::str_trim(.group)) %>%
  as.data.frame()

# View the results
head(letters_pH)
# Pairwise comparisons for EC
pairwise_comparisons_EC <- emmeans(m1_luc_EC, ~ Dose * Fertilizer_Type | Application_Method)

# Generate compact letter display
letters_EC <- multcomp::cld(pairwise_comparisons_EC, Letters = letters) %>%
  mutate(.group = stringr::str_trim(.group)) %>%
  as.data.frame()

# View the results
head(letters_EC)
library(ggplot2)

# Check the structure of letters_dry_shoot
str(letters_dry_shoot)

# Convert to a dataframe if necessary
letters_dry_shoot <- as.data.frame(letters_dry_shoot)

# Merge pairwise comparison results with summary statistics
summary_lucerne_dry_shoot <- left_join(
  summary_lucerne,
  letters_dry_shoot[, c("Dose", "Fertilizer_Type", "Application_Method", ".group")],
  by = c("Dose", "Fertilizer_Type", "Application_Method")
)
# Plot for Dry Biomass (Shoot) with group letters
ggplot(summary_lucerne_dry_shoot, aes(x = Dose, y = Mean_Dry_Biomass_Shoot, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Dry_Biomass_Shoot - SE_Dry_Biomass_Shoot, ymax = Mean_Dry_Biomass_Shoot + SE_Dry_Biomass_Shoot), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = .group), vjust = -0.5, position = position_dodge(0.9)) +
  labs(title = "Dry Biomass (Shoot) at Harvest Day", x = "Dose (N kg/ha)", y = "Mean Biomass (g/m²)") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()



# Pairwise comparisons for Fresh Biomass (Shoot)
pairwise_comparisons_fresh_shoot <- emmeans(m1_luc_fresh_shoot, ~ Dose * Fertilizer_Type | Application_Method)

# Convert to a dataframe
letters_fresh_shoot <- as.data.frame(cld(pairwise_comparisons_fresh_shoot))

# Merge with summary statistics
summary_lucerne_fresh_shoot <- left_join(
  summary_lucerne,
  letters_fresh_shoot[, c("Dose", "Fertilizer_Type", "Application_Method", ".group")],
  by = c("Dose", "Fertilizer_Type", "Application_Method")
)

# View the merged dataframe
head(summary_lucerne_fresh_shoot)
# Pairwise comparisons for Dry Biomass (Shoot)
pairwise_comparisons_dry_shoot <- emmeans(m1_luc_dry_shoot, ~ Dose * Fertilizer_Type | Application_Method)

# Convert to a dataframe
letters_dry_shoot <- as.data.frame(cld(pairwise_comparisons_dry_shoot))

# Merge with summary statistics
summary_lucerne_dry_shoot <- left_join(
  summary_lucerne,
  letters_dry_shoot[, c("Dose", "Fertilizer_Type", "Application_Method", ".group")],
  by = c("Dose", "Fertilizer_Type", "Application_Method")
)

# View the merged dataframe
head(summary_lucerne_dry_shoot)

# Pairwise comparisons for Fresh Biomass (Root)
pairwise_comparisons_fresh_root <- emmeans(m1_luc_fresh_root, ~ Dose * Fertilizer_Type | Application_Method)

# Convert to a dataframe
letters_fresh_root <- as.data.frame(cld(pairwise_comparisons_fresh_root))

# Merge with summary statistics
summary_lucerne_fresh_root <- left_join(
  summary_lucerne,
  letters_fresh_root[, c("Dose", "Fertilizer_Type", "Application_Method", ".group")],
  by = c("Dose", "Fertilizer_Type", "Application_Method")
)

# View the merged dataframe
head(summary_lucerne_fresh_root)


# Pairwise comparisons for Dry Biomass (Root)
pairwise_comparisons_dry_root <- emmeans(m1_luc_dry_root, ~ Dose * Fertilizer_Type | Application_Method)

# Convert to a dataframe
letters_dry_root <- as.data.frame(cld(pairwise_comparisons_dry_root))

# Merge with summary statistics
summary_lucerne_dry_root <- left_join(
  summary_lucerne,
  letters_dry_root[, c("Dose", "Fertilizer_Type", "Application_Method", ".group")],
  by = c("Dose", "Fertilizer_Type", "Application_Method")
)

# View the merged dataframe
head(summary_lucerne_dry_root)
# Pairwise comparisons for Chlorophyll Content
pairwise_comparisons_chlorophyll <- emmeans(m1_luc_chlorophyll, ~ Dose * Fertilizer_Type | Application_Method)

# Convert to a dataframe
letters_chlorophyll <- as.data.frame(cld(pairwise_comparisons_chlorophyll))

# Merge with summary statistics
summary_lucerne_chlorophyll <- left_join(
  summary_lucerne,
  letters_chlorophyll[, c("Dose", "Fertilizer_Type", "Application_Method", ".group")],
  by = c("Dose", "Fertilizer_Type", "Application_Method")
)

# View the merged dataframe
head(summary_lucerne_chlorophyll)
# Pairwise comparisons for pH
pairwise_comparisons_pH <- emmeans(m1_luc_pH, ~ Dose * Fertilizer_Type | Application_Method)

# Convert to a dataframe
letters_pH <- as.data.frame(cld(pairwise_comparisons_pH))

# Merge with summary statistics
summary_lucerne_pH <- left_join(
  summary_lucerne,
  letters_pH[, c("Dose", "Fertilizer_Type", "Application_Method", ".group")],
  by = c("Dose", "Fertilizer_Type", "Application_Method")
)

# View the merged dataframe
head(summary_lucerne_pH)
# Pairwise comparisons for EC
pairwise_comparisons_EC <- emmeans(m1_luc_EC, ~ Dose * Fertilizer_Type | Application_Method)

# Convert to a dataframe
letters_EC <- as.data.frame(cld(pairwise_comparisons_EC))

# Merge with summary statistics
summary_lucerne_EC <- left_join(
  summary_lucerne,
  letters_EC[, c("Dose", "Fertilizer_Type", "Application_Method", ".group")],
  by = c("Dose", "Fertilizer_Type", "Application_Method")
)

# View the merged dataframe
head(summary_lucerne_EC)
# Plot for Fresh Biomass (Shoot)
ggplot(summary_lucerne_fresh_shoot, aes(x = Dose, y = Mean_Fresh_Biomass_Shoot, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Fresh_Biomass_Shoot - SE_Fresh_Biomass_Shoot, ymax = Mean_Fresh_Biomass_Shoot + SE_Fresh_Biomass_Shoot), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = .group), vjust = -0.5, position = position_dodge(0.9)) +
  labs(title = "Fresh Biomass (Shoot) at Harvest Day", x = "Dose (N kg/ha)", y = "Mean Biomass (g/m²)") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()
# Plot for Dry Biomass (Shoot)
ggplot(summary_lucerne_dry_shoot, aes(x = Dose, y = Mean_Dry_Biomass_Shoot, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Dry_Biomass_Shoot - SE_Dry_Biomass_Shoot, ymax = Mean_Dry_Biomass_Shoot + SE_Dry_Biomass_Shoot), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = .group), vjust = -0.5, position = position_dodge(0.9)) +
  labs(title = "Dry Biomass (Shoot) at Harvest Day", x = "Dose (N kg/ha)", y = "Mean Biomass (g/m²)") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()
# Plot for Fresh Biomass (Root)
ggplot(summary_lucerne_fresh_root, aes(x = Dose, y = Mean_Fresh_Biomass_Root, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Fresh_Biomass_Root - SE_Fresh_Biomass_Root, ymax = Mean_Fresh_Biomass_Root + SE_Fresh_Biomass_Root), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = .group), vjust = -0.5, position = position_dodge(0.9)) +
  labs(title = "Fresh Biomass (Root) at Harvest Day", x = "Dose (N kg/ha)", y = "Mean Biomass (g/m²)") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()
# Plot for Dry Biomass (Root)
ggplot(summary_lucerne_dry_root, aes(x = Dose, y = Mean_Dry_Biomass_Root, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Dry_Biomass_Root - SE_Dry_Biomass_Root, ymax = Mean_Dry_Biomass_Root + SE_Dry_Biomass_Root), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = .group), vjust = -0.5, position = position_dodge(0.9)) +
  labs(title = "Dry Biomass (Root) at Harvest Day", x = "Dose (N kg/ha)", y = "Mean Biomass (g/m²)") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()
# Plot for Chlorophyll Content
ggplot(summary_lucerne_chlorophyll, aes(x = Dose, y = Mean_Chlorophyll, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Chlorophyll - SE_Chlorophyll, ymax = Mean_Chlorophyll + SE_Chlorophyll), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = .group), vjust = -0.5, position = position_dodge(0.9)) +
  labs(title = "Chlorophyll Content at Harvest Day", x = "Dose (N kg/ha)", y = "Mean Chlorophyll Content") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()
# Plot for pH
ggplot(summary_lucerne_pH, aes(x = Dose, y = Mean_pH, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_pH - SE_pH, ymax = Mean_pH + SE_pH), width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = .group), vjust = -0.5, position = position_dodge(0.9)) +
  labs(title = "pH at Harvest Day", x = "Dose (N kg/ha)", y = "Mean pH") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()
# Plot for EC
ggplot(summary_lucerne_EC, aes(x = Dose, y = Mean_EC, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_EC - SE_EC, ymax = Mean_EC + SE_EC), width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = .group), vjust = -0.5, position = position_dodge(0.9)) +
  labs(title = "EC at Harvest Day", x = "Dose (N kg/ha)", y = "Mean EC") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()

