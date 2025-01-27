# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lme4)
library(car)
library(corrplot)
library(FactoMineR)
library(factoextra)

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(car)
library(emmeans)
library(multcompView)

# Set working directory
setwd("Experiment_2")

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


# example analysis - pH responses over the entire experiment (Jeff Code)
tmp <- Lucerne_exp2 %>% 
  filter(Application_Method == 'Split') %>% 
  select(Block, Pot.ID, Dose, starts_with('pH')) %>% 
  pivot_longer(cols=starts_with('pH'), 
               names_to='Date', values_to='pH') %>% 
  mutate(Date=factor(Date), 
         Date=fct_relevel(Date, 'pH_BF'), 
         Block = as.character(Block)) %>% 
  filter(pH < 7)
levels(tmp$Date)

# not accounting for non-independence among replicates
m1 <- lm(pH ~ Dose * Date, data=tmp)
plot(m1)
Anova(m1)
install.packages("insight")  # Replace "insight" with the actual package name
library(insight)

# effects are significant to visually inspect predictions
library(ggeffects)
predict_response(m1, c('Date', 'Dose')) %>% plot()
# check in more detail using emmeans


# accounting for non-indpendence ('repeated measures') - 'mixed effects model'
library(lme4)
m1 <- lmer(pH ~ Dose * Date + (1|Block/Pot.ID), data=tmp)
plot(m1)
qqPlot(resid(m1))
Anova(m1, test='F')
summary(m1)
predict_response(m1, c('Date', 'Dose')) %>% plot()




# Check the transformed dataset
str(Lucerne_exp2)


# example analysis - EC responses over the entire experiment (Jeff Code)
tmp <- Lucerne_exp2 %>% 
  filter(Application_method == 'Split') %>% 
  select(Block, Pot.ID, Dose...N.kg.ha., starts_with('EC')) %>% 
  pivot_longer(cols=starts_with('EC'), 
               names_to='Date', values_to='EC') %>% 
  mutate(Date=factor(Date), 
         Date=fct_relevel(Date, 'EC_BF'), 
         Block = as.character(Block)) %>% 
  filter(EC < 7)
levels(tmp$Date)

# not accounting for non-independence among replicates
m1 <- lm(EC ~ Dose...N.kg.ha. * Date, data=tmp)
plot(m1)
Anova(m1)
install.packages("insight")  # Replace "insight" with the actual package name
library(insight)

# effects are significant to visually inspect predictions
library(ggeffects)
predict_response(m1, c('Date', 'Dose...N.kg.ha.')) %>% plot()
# check in more detail using emmeans


# accounting for non-indpendence ('repeated measures') - 'mixed effects model'
library(lme4)
m1 <- lmer(EC ~ Dose...N.kg.ha. * Date + (1|Block/Pot.ID), data=tmp)
plot(m1)
qqPlot(resid(m1))
Anova(m1, test='F')
summary(m1)
predict_response(m1, c('Date', 'Dose...N.kg.ha.')) %>% plot()





# Check the transformed dataset
str(Lucerne_exp2)


# example analysis - DRyBiomass  responses over the entire experiment (Jeff Code)
tmp <- Lucerne_exp2 %>% 
  filter(Application_method == 'Split') %>% 
  select(Block, Pot.ID, Dose...N.kg.ha., starts_with('Dry')) %>% 
  pivot_longer(cols=starts_with('Dry'), 
               names_to='Date', values_to='Dry') %>% 
  mutate(Date=factor(Date), 
         Date=fct_relevel(Date, 'Dry_Biomass_1'), 
         Block = as.character(Block)) %>% 
  filter( Dry< 20)
levels(tmp$Date)

# not accounting for non-independence among replicates
m1 <- lm(Dry ~ Dose...N.kg.ha. * Date, data=tmp)
plot(m1)
Anova(m1)
install.packages("insight")  # Replace "insight" with the actual package name
library(insight)

# effects are significant to visually inspect predictions
library(ggeffects)
predict_response(m1, c('Date', 'Dose...N.kg.ha.')) %>% plot()
# check in more detail using emmeans


# accounting for non-indpendence ('repeated measures') - 'mixed effects model'
library(lme4)
m1 <- lmer(Dry ~ Dose...N.kg.ha. * Date + (1|Block/Pot.ID), data=tmp)
plot(m1)
qqPlot(resid(m1))
Anova(m1, test='F')
summary(m1)
predict_response(m1, c('Date', 'Dose...N.kg.ha.')) %>% plot()





# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(emmeans)
library(forcats)

# Convert variables to factors
Lucerne_exp2 <- Lucerne_exp2 %>%
  mutate(
    Dose = factor(Dose...N.kg.ha., levels = c("0", "10", "20", "30", "40", "50", "60")),
    Fertilizer_Type = factor(Fertilizer.type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split"))
  )

# Filter only rows with Application_Method == "Split"
split_data <- Lucerne_exp2 %>%
  filter(Application_Method == "Split") %>%
  select(
    Block,
    Pot.ID,
    Plant.type,
    Dose,
    Fertilizer_Type,
    Application_Method,
    Dry_Biomass_1,
    Dry_Biomass._2,
    Dry_Biomass_3,
    Dry_Biomass_root_118days
  ) %>%
  pivot_longer(
    cols = c(Dry_Biomass_1, Dry_Biomass._2, Dry_Biomass_3, Dry_Biomass_root_118days),
    names_to = "Time_Point",
    values_to = "Dry_Biomass"
  ) %>%
  mutate(
    Time_Point = factor(Time_Point),
    Block = as.character(Block)
  ) %>%
  drop_na(Dry_Biomass)  # Remove rows with missing Dry_Biomass values

# Check the structure of the reshaped data
str(split_data)

# Visualize data distribution
ggplot(split_data, aes(x = Time_Point, y = Dry_Biomass, color = Fertilizer_Type)) +
  geom_boxplot() +
  facet_grid(cols = vars(Dose)) +
  theme_minimal() +
  labs(title = "Dry Biomass Distribution by Time Point (Split Only)", y = "Dry Biomass", x = "Time Point")

# Visualize data distribution for Split method with Dose as legend
ggplot(split_data, aes(x = Time_Point, y = Dry_Biomass, color = Dose)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Dry Biomass Distribution by Time Point (Split Only)",
    y = "Dry Biomass",
    x = "Time Point",
    color = "Dose (N kg/ha)"
  ) +
  theme(legend.position = "right")  # Ensure legend is on the right



# Fit a linear mixed-effects model
m1_split <- lmer(Dry_Biomass ~ Time_Point * Dose * Fertilizer_Type + (1 | Block/Pot.ID), data = split_data)

# Check model summary and diagnostics
summary(m1_split)
plot(m1_split)
qqnorm(resid(m1_split))
qqline(resid(m1_split))

# Perform ANOVA to test fixed effects
Anova(m1_split, test = "F")
library(emmeans)
library(multcomp)
library(multcompView)
# Pairwise comparisons and significance letters
emmeans_split <- emmeans(m1_split, ~ Time_Point * Dose * Fertilizer_Type)
letters_split <- cld(emmeans_split, adjust = "tukey", Letters = letters)
print(letters_split)






# Visualize model predictions
pred_split <- ggpredict(m1_split, terms = c("Time_Point", "Dose", "Fertilizer_Type"))
ggplot(pred_split, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  labs(x = "Time Point", y = "Predicted Dry Biomass", title = "Dry Biomass Predictions (Split Only)") +
  theme_minimal()

# Visualize significance letters
results_split <- as.data.frame(letters_split)
ggplot(results_split, aes(x = Time_Point, y = emmean, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = .group), position = position_dodge(0.9), vjust = -0.5) +
  labs(y = "Estimated Mean Dry Biomass", x = "Time Point", title = "Significance Letters for Dry Biomass (Split Only)") +
  theme_classic()



# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(emmeans)

# Ensure data is structured properly with both application methods
comparison_data <- Lucerne_exp2 %>%
  select(
    Block,
    Pot.ID,
    Plant.type,
    Dose,
    Fertilizer_Type,
    Application_Method,
    Dry_Biomass_1,
    Dry_Biomass._2,
    Dry_Biomass_3,
    Dry_Biomass_root_118days
  ) %>%
  pivot_longer(
    cols = c(Dry_Biomass_1, Dry_Biomass._2, Dry_Biomass_3, Dry_Biomass_root_118days),
    names_to = "Time_Point",
    values_to = "Dry_Biomass"
  ) %>%
  mutate(
    Time_Point = factor(Time_Point),
    Block = as.character(Block)
  ) %>%
  drop_na(Dry_Biomass)  # Remove rows with missing Dry_Biomass values

# Visualize data distribution for both application methods
ggplot(comparison_data, aes(x = Time_Point, y = Dry_Biomass, fill = Application_Method)) +
  geom_boxplot() +
  facet_wrap(~ Dose) +
  theme_minimal() +
  labs(
    title = "Dry Biomass Distribution by Time Point and Application Method",
    y = "Dry Biomass",
    x = "Time Point",
    fill = "Application Method"
  ) +
  theme(legend.position = "right")

# Fit a linear mixed-effects model to compare application methods
m1 <- lmer(Dry_Biomass ~ Time_Point * Dose * Application_Method + (1 | Block/Pot.ID), data = comparison_data)

# Model summary
summary(m1)

# Perform ANOVA to test fixed effects
anova_results <- Anova(m1, test = "F")
print(anova_results)

# Pairwise comparisons for Application_Method
emmeans_results <- emmeans(m1, ~ Application_Method | Time_Point * Dose)
pairs(emmeans_results)

# Visualize predictions
pred <- ggpredict(m1, terms = c("Time_Point", "Application_Method", "Dose"))
ggplot(pred, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  labs(
    title = "Predicted Dry Biomass by Application Method and Time Point",
    x = "Time Point",
    y = "Predicted Dry Biomass",
    color = "Application Method",
    fill = "Application Method"
  ) +
  theme_minimal()





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

