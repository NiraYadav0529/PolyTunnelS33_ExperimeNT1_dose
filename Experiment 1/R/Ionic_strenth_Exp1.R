View(Ionic_strength_EXP1)
read.csv(Ionic_strength_EXP1.csv)

setwd("C:\\Users\\90958427\\OneDrive - Western Sydney University\\PolyTunnelS33_ExperimeNT1_dose\\Modified Data File")
list.files()

# Reading the CSV file
Ionic_Strength <- read.csv("Ionic_strength_EXP1.csv") 

# View the first few rows of the dataset
head(Ionic_Strength)

# Load necessary libraries
library(ggplot2)
library(ggpubr)

# Visualize the distribution of Ionic Strength by Treatment with pairwise comparisons
ggplot(Ionic_Strength, aes(x = Treatment, y = Ionic.strength, fill = Treatment)) + 
  geom_boxplot() +
  stat_compare_means(method = "anova", label.y = max(Ionic_Strength$Ionic.strength) + 0.1) +  # ANOVA p-value
  stat_compare_means(comparisons = list(c("Control", "U100"), c("Control", "U200"), c("Control", "M100"), c("Control", "M200"), 
                                        c("U100", "U200"), c("U100", "M100"), c("U100", "M200"), 
                                        c("U200", "M100"), c("U200", "M200"), c("M100", "M200")),
                     label = "p.signif", method = "t.test", ref.group = "Control") +  # Pairwise t-test comparisons
  labs(title = "Ionic Strength Across Treatments", x = "Treatment", y = "Ionic Strength (mol/L)") +
  theme_minimal()

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Calculate mean and standard error for each treatment
summary_data <- Ionic_Strength %>%
  group_by(Treatment) %>%
  summarise(
    mean_ionic_strength = mean(Ionic.strength),
    se_ionic_strength = sd(Ionic.strength) / sqrt(n())
  )

# Create a bar plot with error bars
ggplot(summary_data, aes(x = Treatment, y = mean_ionic_strength, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_ionic_strength - se_ionic_strength, ymax = mean_ionic_strength + se_ionic_strength), 
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Mean Ionic Strength Across Treatments", x = "Treatment", y = "Mean Ionic Strength (mol/L)") +
  theme_minimal()
# Perform ANOVA to check for significant differences between treatments
anova_result <- aov(Ionic.strength ~ Treatment, data = Ionic_Strength)
summary(anova_result)

