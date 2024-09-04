# Load required libraries
library(tidyverse)
library(car)
library(emmeans)
library(ggplot2)

# Set working directory (adjust as needed)
setwd("C:\\Users\\90958427\\OneDrive - Western Sydney University\\PolyTunnelS33_ExperimeNT1_dose\\Modified Data File")
list.files()
Plant_traits <-read.csv("Biomass_height_stem_data_exp1.csv")
# Load the dataset
head(Plant_traits)

# Rename and clean up the Site column if necessary
colnames(Plant_traits)[1] <- 'Site'
Plant_traits$Site <- sub(c('ABS00'), '', Plant_traits$Site)
Plant_traits$Site <- sub(c('ABS0'), '', Plant_traits$Site)

# Analysis for Plant Height Before Fertilization
ggplot(Plant_traits, aes(x = Dose, y = Plant_Height_BF..cm., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Plant Height Before Fertilization by Dose and Plant Type", x = "Dose", y = "Plant Height (cm)")

m1_height <- lm((Plant_Height_BF..cm.) ~ Dose * Plant.type , data = Plant_traits)
summary(m1_height)
residualPlot(m1_height)
qqPlot(m1_height)
Anova(m1_height)
multcomp::cld(emmeans(m1_height, ~ Dose | Plant.type))

# Analysis for Stem Count per Pot Before Fertilization
ggplot(Plant_traits, aes(x = Dose, y = Stem_countperPot_BF..no.., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Stem Count per Pot Before Fertilization by Dose and Plant Type", x = "Dose", y = "Stem Count per Pot")

m1_stem <- lm(log10(Stem_countperPot_BF..no..) ~ Dose * Plant.type , data = Plant_traits)
summary(m1_stem)
residualPlot(m1_stem)
qqPlot(m1_stem)
Anova(m1_stem)
multcomp::cld(emmeans(m1_stem, ~ Dose | Plant.type))

# Analysis for Fresh Biomass Before Fertilization
ggplot(Plant_traits, aes(x = Dose, y = Fresh_biomass_BF..gm., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Fresh Biomass Before Fertilization by Dose and Plant Type", x = "Dose", y = "Fresh Biomass (gm)")

m1_biomass <- lm(log10(Fresh_biomass_BF..gm.) ~ Dose * Plant.type , data = Plant_traits)
summary(m1_biomass)
residualPlot(m1_biomass)
qqPlot(m1_biomass)
Anova(m1_biomass)
multcomp::cld(emmeans(m1_biomass, ~ Dose | Plant.type))

# Continue similar analysis for remaining traits:
# Fresh Biomass After 30 Days
ggplot(Plant_traits, aes(x = Dose, y = Fresh_biomass_AF_30days.gm., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Fresh Biomass After 30 Days by Dose and Plant Type", x = "Dose", y = "Fresh Biomass After 30 Days (gm)")

m1_biomass_af30 <- lm((Fresh_biomass_AF_30days.gm.) ~ Dose * Plant.type , data = Plant_traits)
summary(m1_biomass_af30)
residualPlot(m1_biomass_af30)
qqPlot(m1_biomass_af30)
Anova(m1_biomass_af30)
multcomp::cld(emmeans(m1_biomass_af30, ~ Dose | Plant.type))

# Plant Height at Harvest Day
ggplot(Plant_traits, aes(x = Dose, y = Plant_Height_Harvestday.cm., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Plant Height at Harvest Day by Dose and Plant Type", x = "Dose", y = "Plant Height at Harvest Day (cm)")

m1_height_harvest <- lm((Plant_Height_Harvestday.cm.) ~ Dose * Plant.type , data = Plant_traits)
summary(m1_height_harvest)
residualPlot(m1_height_harvest)
qqPlot(m1_height_harvest)
Anova(m1_height_harvest)
multcomp::cld(emmeans(m1_height_harvest, ~ Dose | Plant.type))

# Stem Count at Harvest Day
ggplot(Plant_traits, aes(x = Dose, y = Stem_count_HarvestDay.no.., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Stem Count at Harvest Day by Dose and Plant Type", x = "Dose", y = "Stem Count at Harvest Day")

m1_stem_harvest <- lm((Stem_count_HarvestDay.no..) ~ Dose * Plant.type , data = Plant_traits)
summary(m1_stem_harvest)
residualPlot(m1_stem_harvest)
qqPlot(m1_stem_harvest)
Anova(m1_stem_harvest)
multcomp::cld(emmeans(m1_stem_harvest, ~ Dose | Plant.type))

# Root Length
ggplot(Plant_traits, aes(x = Dose, y = Root.Length..cm., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Root Length by Dose and Plant Type", x = "Dose", y = "Root Length (cm)")

m1_root_length <- lm((Root.Length..cm.) ~ Dose * Plant.type , data = Plant_traits)
summary(m1_root_length)
residualPlot(m1_root_length)
qqPlot(m1_root_length)
Anova(m1_root_length)
multcomp::cld(emmeans(m1_root_length, ~ Dose | Plant.type))

# Nodules Count
ggplot(Plant_traits, aes(x = Dose, y = Nodules.Count.no.., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Nodules Count by Dose and Plant Type", x = "Dose", y = "Nodules Count")

m1_nodules <- lm((Nodules.Count.no..) ~ Dose * Plant.type , data = Plant_traits)
summary(m1_nodules)
residualPlot(m1_nodules)
qqPlot(m1_nodules)
Anova(m1_nodules)
multcomp::cld(emmeans(m1_nodules, ~ Dose | Plant.type))

# Fresh Biomass at Harvest Day
ggplot(Plant_traits, aes(x = Dose, y = Fresh.Biomass_harvestday..gm., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Fresh Biomass at Harvest Day by Dose and Plant Type", x = "Dose", y = "Fresh Biomass at Harvest Day (gm)")

m1_fresh_biomass_harvest <- lm((Fresh.Biomass_harvestday..gm.) ~ Dose * Plant.type , data = Plant_traits)
summary(m1_fresh_biomass_harvest)
residualPlot(m1_fresh_biomass_harvest)
qqPlot(m1_fresh_biomass_harvest)
Anova(m1_fresh_biomass_harvest)
multcomp::cld(emmeans(m1_fresh_biomass_harvest, ~ Dose | Plant.type))

# Root Biomass at Harvest Day
ggplot(Plant_traits, aes(x = Dose, y = Root.Biomass_harvestday.gm., colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type) +
  labs(title = "Root Biomass at Harvest Day by Dose and Plant Type", x = "Dose", y = "Root Biomass at Harvest Day (gm)")

m1_root_biomass_harvest <- lm((Root.Biomass_harvestday.gm.) ~ Dose * Plant.type , data = Plant_traits)
summary(m1_root_biomass_harvest)
residualPlot(m1_root_biomass_harvest)
qqPlot(m1_root_biomass_harvest)
Anova(m1_root_biomass_harvest)
multcomp::cld(emmeans(m1_root_biomass_harvest, ~ Dose | Plant.type))

