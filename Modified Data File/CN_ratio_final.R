# Setting the working directory (update the path as needed)
setwd("C:/Users/90958427/OneDrive - Western Sydney University/Niraj Pot Experiment/Modified Data File")
# Reading the CSV file
CN_data <- read.csv("Carbon_Nitrogen_ratio_(CN).csv")
# View the first few rows to verify data was read correctly
head(CN_data)
# Filter out rows with NA values
CN_data_clean <- CN_data %>%
  filter(!is.na(Leaf.C..), !is.na(Leaf.N.), !is.na(Root.C.), !is.na(Root.N.))

# Visualize the distribution of Leaf Carbon (Leaf.C..) by Dose
ggplot(CN_data_clean, aes(x = Dose, y = Leaf.C.., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Leaf Carbon by Dose and Plant Type", 
       x = "Dose", 
       y = "Leaf Carbon (mg/g)")

# Fit the model for Leaf Carbon (Leaf.C..)
m1_leaf_c <- lm(Leaf.C.. ~ Dose * Plant.type, data = CN_data_clean)
summary(m1_leaf_c)

# Check residuals for linearity and normality
residualPlot(m1_leaf_c)  # Plot residuals vs fitted values
qqPlot(m1_leaf_c)         # Q-Q plot to check normality of residuals

# Perform Type II ANOVA to test the significance of effects
Anova(m1_leaf_c, type = "II")

# Perform multiple comparison test
pairwise_comparisons_leaf_c <- emmeans(m1_leaf_c, ~ Dose | Plant.type)
summary(pairwise_comparisons_leaf_c)
multcomp::cld(pairwise_comparisons_leaf_c)

# Repeat the process for Leaf Nitrogen (Leaf.N.)
ggplot(CN_data_clean, aes(x = Dose, y = Leaf.N., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Leaf Nitrogen by Dose and Plant Type", 
       x = "Dose", 
       y = "Leaf Nitrogen (mg/g)")

m1_leaf_n <- lm(Leaf.N. ~ Dose * Plant.type, data = CN_data_clean)
summary(m1_leaf_n)
residualPlot(m1_leaf_n)
qqPlot(m1_leaf_n)
Anova(m1_leaf_n, type = "II")
pairwise_comparisons_leaf_n <- emmeans(m1_leaf_n, ~ Dose | Plant.type)
summary(pairwise_comparisons_leaf_n)
multcomp::cld(pairwise_comparisons_leaf_n)

# Repeat the process for Root Carbon (Root.C.)
ggplot(CN_data_clean, aes(x = Dose, y = Root.C., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Root Carbon by Dose and Plant Type", 
       x = "Dose", 
       y = "Root Carbon (mg/g)")

m1_root_c <- lm(Root.C. ~ Dose * Plant.type, data = CN_data_clean)
summary(m1_root_c)
residualPlot(m1_root_c)
qqPlot(m1_root_c)
Anova(m1_root_c, type = "II")
pairwise_comparisons_root_c <- emmeans(m1_root_c, ~ Dose | Plant.type)
summary(pairwise_comparisons_root_c)
multcomp::cld(pairwise_comparisons_root_c)

# Repeat the process for Root Nitrogen (Root.N.)
ggplot(CN_data_clean, aes(x = Dose, y = Root.N., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Root Nitrogen by Dose and Plant Type", 
       x = "Dose", 
       y = "Root Nitrogen (mg/g)")

m1_root_n <- lm(Root.N. ~ Dose * Plant.type, data = CN_data_clean)
summary(m1_root_n)
residualPlot(m1_root_n)
qqPlot(m1_root_n)
Anova(m1_root_n, type = "II")
pairwise_comparisons_root_n <- emmeans(m1_root_n, ~ Dose | Plant.type)
summary(pairwise_comparisons_root_n)
multcomp::cld(pairwise_comparisons_root_n)

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Ensure that the C:N ratio columns are numeric
CN_data_clean <- CN_data_clean %>%
  mutate(
    Leaf.C.N..ratio. = as.numeric(Leaf.C.N..ratio.),
    Root.C.N..ratio. = as.numeric(Root.C.N..ratio.)
  )

# Check for any conversion issues (e.g., NAs introduced by coercion)
summary(CN_data_clean$Leaf.C.N..ratio.)
summary(CN_data_clean$Root.C.N..ratio.)

# Calculate the average C:N ratio for each Dose and Plant Type
avg_cn_ratio <- CN_data_clean %>%
  group_by(Dose, Plant.type) %>%
  summarize(
    Avg_C_N_Ratio = mean((Leaf.C.N..ratio. + Root.C.N..ratio.) / 2, na.rm = TRUE)
  )

# Visualize the average C:N ratio percentage
ggplot(avg_cn_ratio, aes(x = Dose, y = Avg_C_N_Ratio, fill = Plant.type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Average C:N Ratio Percentage by Dose and Plant Type", 
       x = "Dose", 
       y = "Average C:N Ratio (%)") +
  theme_minimal()

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lme4) # For linear mixed models
library(car)
library(emmeans)

# Ensure that the C:N ratio columns are numeric
CN_data_clean <- CN_data_clean %>%
  mutate(
    Leaf.C.N..ratio. = as.numeric(Leaf.C.N..ratio.),
    Root.C.N..ratio. = as.numeric(Root.C.N..ratio.)
  )

# Check for any conversion issues (e.g., NAs introduced by coercion)
summary(CN_data_clean$Leaf.C.N..ratio.)
summary(CN_data_clean$Root.C.N..ratio.)
# Combine Leaf and Root C:N ratios into one column
CN_data_long <- CN_data_clean %>%
  gather(key = "Type", value = "C_N_Ratio", Leaf.C.N..ratio., Root.C.N..ratio.)

# Box plot with jitter to show all data points, mean, and standard deviation
ggplot(CN_data_long, aes(x = Dose, y = C_N_Ratio, color = Type, fill = Type)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16,
               position = position_dodge(0.8)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2), alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black", position = position_dodge(0.8)) +
  labs(title = "C:N Ratio by Dose and Plant Type",
       x = "Dose", y = "C:N Ratio (%)") +
  theme_minimal() +
  facet_wrap(~ Plant.type)
# Fit linear mixed model
lmm <- lmer(C_N_Ratio ~ Dose * Type + (1 | Plant.type), data = CN_data_long)

# Summarize the model
summary(lmm)

# ANOVA for the mixed model
anova(lmm)

# Perform pairwise comparisons using emmeans
emm <- emmeans(lmm, ~ Dose * Type)
summary(emm)

# Load necessary libraries
library(ggplot2)
install.packages("Hmisc")
library(Hmisc)

# Plotting C:N Ratio with Mean and SD
ggplot2(CN_data_long, aes(x = Dose, y = C_N_Ratio, color = Type, fill = Type)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16,
               position = position_dodge(0.8)) +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", 
               color = "blue", width = 0.2, position = position_dodge(0.8)) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black", position = position_dodge(0.8)) +
  labs(title = "C:N Ratio by Dose and Plant Type with Mean and SD",
       x = "Dose", y = "C:N Ratio (%)") +
  theme_minimal() +
  facet_wrap(~ Plant.type)
install.packages("ggplot2")
library(ggplot2)

