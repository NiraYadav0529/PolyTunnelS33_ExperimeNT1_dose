

# Load the dataset
Plant_traits <- read.csv("Biomass_height_stem_data_exp1.csv")
colnames(Plant_traits)

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(car)
library(emmeans)
library(multcomp)

# Step 1: Check the column names to identify the correct names for Leaf C:N and Root C:N ratios
colnames(Plant_traits)

# Ensure necessary columns are treated as factors
Plant_traits <- Plant_traits %>%
  mutate(
    Dose = factor(Dose..N.kg.ha., levels = c("0", "100", "200")),  # Adjust Dose column
    Fertilizer_Type = factor(Fertilizer.type, levels = c('None', 'MF', 'UF')),  # Adjust Fertilizer type
    Plant.type = factor(Plant.type)  # Ensure Plant.type is a factor
  )

# Step 2: Calculate mean and standard errors for Leaf C:N ratio
cn_summary <- Plant_traits %>%
  group_by(Dose, Plant.type, Fertilizer_Type) %>%
  summarize(
    Leaf_CN_mean = mean(Leaf.C.N..ratio., na.rm = TRUE),  # Replace with correct column name
    Leaf_CN_se = sd(Leaf.C.N..ratio., na.rm = TRUE) / sqrt(n())  # Replace with correct column name
  )

# Step 3: Visualize Leaf C:N ratio by Dose, Plant Type, and Fertilizer Type
ggplot(cn_summary, aes(x = Dose, y = Leaf_CN_mean, fill = Fertilizer_Type)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = Leaf_CN_mean - Leaf_CN_se, ymax = Leaf_CN_mean + Leaf_CN_se), 
                width = 0.2, position = position_dodge(0.9), color = "black") +
  facet_wrap(~ Plant.type) +
  labs(title = "Leaf C:N Ratio by Dose, Plant Type, and Fertilizer Type", 
       x = "Dose", y = "Leaf C:N Ratio") +
  scale_fill_manual(values = c("grey20", "grey50", "grey80")) +
  theme_minimal(base_size = 15) +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = "grey"))

# Step 4: Fit the model for Leaf C:N ratio
m1_leaf_cn <- lm(Leaf.C.N..ratio. ~ Dose * Plant.type * Fertilizer_Type, data = Plant_traits)  # Replace with correct column name
summary(m1_leaf_cn)

# Step 5: Check residuals for linearity and normality
residualPlot(m1_leaf_cn)
qqPlot(m1_leaf_cn)

# Step 6: Test significance of the model effects (ANOVA)
Anova(m1_leaf_cn, type = "II")

# Step 7: Perform multiple comparison test (Tukey HSD) for Leaf C:N ratio
pairwise_comparisons_leaf_cn <- emmeans(m1_leaf_cn, ~ Dose | Plant.type | Fertilizer_Type)
summary(pairwise_comparisons_leaf_cn)

# Step 8: Visualize significant differences using compact letter display for Leaf C:N ratio
cld_leaf_cn <- cld(pairwise_comparisons_leaf_cn)

# Step 9: Join the Tukey test results for Leaf C:N ratio with cn_summary and plot with compact letter display (CLD)
cld_leaf_cn <- as.data.frame(cld_leaf_cn)
merged_leaf_cn <- merge(cn_summary, cld_leaf_cn, by = c("Dose", "Plant.type", "Fertilizer_Type"))

# Step 10: Plot Leaf C:N ratio with Tukey's HSD significant letters
ggplot(merged_leaf_cn, aes(x = Dose, y = Leaf_CN_mean, fill = Fertilizer_Type)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = Leaf_CN_mean - Leaf_CN_se, ymax = Leaf_CN_mean + Leaf_CN_se), 
                width = 0.2, position = position_dodge(0.9), color = "black") +
  facet_wrap(~ Plant.type) +
  geom_text(aes(x = Dose, y = Leaf_CN_mean + Leaf_CN_se + 0.05, label = .group), 
            position = position_dodge(0.9), vjust = 0, color = "black") +
  labs(title = "Leaf C:N Ratio by Dose, Plant Type, and Fertilizer Type", 
       x = "Dose (N kg/ha)", 
       y = "Leaf C:N Ratio") +
  scale_fill_manual(values = c("grey20", "grey50", "grey80")) +
  theme_minimal(base_size = 15) +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = "grey"))

# Step 2: Calculate mean and standard errors for Root C:N ratio
cn_summary_root <- Plant_traits %>%
  group_by(Dose, Plant.type, Fertilizer_Type) %>%
  summarize(
    Root_CN_mean = mean(Root.C.N..ratio., na.rm = TRUE),  # Replace with correct column name for Root C:N ratio
    Root_CN_se = sd(Root.C.N..ratio., na.rm = TRUE) / sqrt(n())  # Replace with correct column name for Root C:N ratio
  )

# Step 3: Visualize Root C:N ratio by Dose, Plant Type, and Fertilizer Type
ggplot(cn_summary_root, aes(x = Dose, y = Root_CN_mean, fill = Fertilizer_Type)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = Root_CN_mean - Root_CN_se, ymax = Root_CN_mean + Root_CN_se), 
                width = 0.2, position = position_dodge(0.9), color = "black") +
  facet_wrap(~ Plant.type) +
  labs(title = "Root C:N Ratio by Dose, Plant Type, and Fertilizer Type", 
       x = "Dose", y = "Root C:N Ratio") +
  scale_fill_manual(values = c("grey20", "grey50", "grey80")) +
  theme_minimal(base_size = 15) +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = "grey"))

# Step 4: Fit the model for Root C:N ratio
m1_root_cn <- lm(Root.C.N..ratio. ~ Dose * Plant.type * Fertilizer_Type, data = Plant_traits)  # Replace with correct column name
summary(m1_root_cn)

# Step 5: Check residuals for linearity and normality
residualPlot(m1_root_cn)
qqPlot(m1_root_cn)

# Step 6: Test significance of the model effects (ANOVA)
Anova(m1_root_cn, type = "II")

# Step 7: Perform multiple comparison test (Tukey HSD) for Root C:N ratio
pairwise_comparisons_root_cn <- emmeans(m1_root_cn, ~ Dose | Plant.type | Fertilizer_Type)
summary(pairwise_comparisons_root_cn)

# Step 8: Visualize significant differences using compact letter display for Root C:N ratio
cld_root_cn <- cld(pairwise_comparisons_root_cn)

# Step 9: Join the Tukey test results for Root C:N ratio with cn_summary and plot with compact letter display (CLD)
cld_root_cn <- as.data.frame(cld_root_cn)
merged_root_cn <- merge(cn_summary_root, cld_root_cn, by = c("Dose", "Plant.type", "Fertilizer_Type"))

# Step 10: Plot Root C:N ratio with Tukey's HSD significant letters
ggplot(merged_root_cn, aes(x = Dose, y = Root_CN_mean, fill = Fertilizer_Type)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = Root_CN_mean - Root_CN_se, ymax = Root_CN_mean + Root_CN_se), 
                width = 0.2, position = position_dodge(0.9), color = "black") +
  facet_wrap(~ Plant.type) +
  geom_text(aes(x = Dose, y = Root_CN_mean + Root_CN_se + 0.05, label = .group), 
            position = position_dodge(0.9), vjust = 0, color = "black") +
  labs(title = "Root C:N Ratio by Dose, Plant Type, and Fertilizer Type", 
       x = "Dose (N kg/ha)", 
       y = "Root C:N Ratio") +
  scale_fill_manual(values = c("grey20", "grey50", "grey80")) +
  theme_minimal(base_size = 15) +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(color = "grey"))
