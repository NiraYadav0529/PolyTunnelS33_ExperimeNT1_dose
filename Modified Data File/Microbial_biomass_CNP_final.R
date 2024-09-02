#soil microbial biomass C, N, and P data
library(tidyverse)
library(car)
library(emmeans)
library(ggplot2)

# Setting the working directory

setwd("C:/Users/90958427/OneDrive - Western Sydney University/Niraj Pot Experiment/Modified Data File")
list.files()

# Reading the CSV file
mbc_data <- read.csv("mbc_CNP-exp1_final.csv") 

# View the first few rows to verify data was read correctly
head(mbc_data)
colnames(mbc_data)

# Visualize the distribution of Microbial Biomass Phosphorus (Soil.conc..ug.P.g..2) by Treatment and Plant Type
ggplot(mbc_data, aes(x = Dose, y = Soil.conc..ug.P.g..2, colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Microbial Biomass Phosphorus by Dose and Plant Type", x = "Dose", y = "Microbial Biomass P (µg/g)")

# Visualize the distribution of Microbial Biomass Carbon (Soil.conc..ug.C.g..2) by Treatment and Plant Type
ggplot(mbc_data, aes(x = Dose, y = Soil.conc..ug.C.g..2, colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Microbial Biomass Carbon by Dose and Plant Type", x = "Dose", y = "Microbial Biomass C (µg/g)")

# Visualize the distribution of Microbial Biomass Nitrogen (Soil.conc..ug.N.g..2) by Treatment and Plant Type
ggplot(mbc_data, aes(x = Dose, y = Soil.conc..ug.N.g..2, colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Microbial Biomass Nitrogen by Dose and Plant Type", x = "Dose", y = "Microbial Biomass N (µg/g)")
# Fit the model for Microbial Biomass Phosphorus (MBP)
m1_p <- lm(Soil.conc..ug.P.g..2 ~ Plant.type * Dose, data = mbc_data)
summary(m1_p)
Anova(m1_p)

# Check residuals for MBP model
residualPlot(m1_p)
qqPlot(m1_p)

# Fit the model for Microbial Biomass Carbon (MBC)
m1_c <- lm(Soil.conc..ug.C.g..2 ~ Plant.type * Dose, data = mbc_data)
summary(m1_c)
Anova(m1_c)

# Check residuals for MBC model
residualPlot(m1_c)
qqPlot(m1_c)

# Fit the model for Microbial Biomass Nitrogen (MBN)
m1_n <- lm(Soil.conc..ug.N.g..2 ~ Plant.type * Dose, data = mbc_data)
summary(m1_n)
Anova(m1_n)

# Check residuals for MBN model
residualPlot(m1_n)
qqPlot(m1_n)
# Perform multiple comparisons for MBP
pairwise_comparisons_p <- emmeans(m1_p, ~ Dose | Plant.type)
summary(pairwise_comparisons_p)
multcomp::cld(pairwise_comparisons_p)

# Perform multiple comparisons for MBC
pairwise_comparisons_c <- emmeans(m1_c, ~ Dose | Plant.type)
summary(pairwise_comparisons_c)
multcomp::cld(pairwise_comparisons_c)

# Perform multiple comparisons for MBN
pairwise_comparisons_n <- emmeans(m1_n, ~ Dose | Plant.type)
summary(pairwise_comparisons_n)
multcomp::cld(pairwise_comparisons_n)
# Combined Box Plot for MBP, MBC, and MBN
ggplot(mbc_data, aes(x = Dose)) + 
  geom_boxplot(aes(y = Soil.conc..ug.P.g..2, fill = "MBP"), position = position_dodge(width = 0.75)) +
  geom_boxplot(aes(y = Soil.conc..ug.C.g..2, fill = "MBC"), position = position_dodge(width = 0.75)) +
  geom_boxplot(aes(y = Soil.conc..ug.N.g..2, fill = "MBN"), position = position_dodge(width = 0.75)) +
  facet_wrap(~Plant.type) +
  scale_fill_manual(values = c("MBP" = "red", "MBC" = "blue", "MBN" = "green")) +
  labs(title = "Microbial Biomass C, N, P by Dose and Plant Type", x = "Dose", y = "Concentration (µg/g)") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

