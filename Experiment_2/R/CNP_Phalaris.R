setwd("C:/Users/90958427/OneDrive - Western Sydney University/PolyTunnelS33_ExperimeNT1_dose/Experiment_2/Modified_Data_File")
list.files()
CNP_File <- read.csv("CNP_data.csv")
colnames(CNP_File)
head(CNP_File)


library(dplyr)
library(ggplot2)
library(car)
library(emmeans)
library(multcomp)
library(multcompView)
library(stringr)



# Load necessary libraries
library(dplyr)
library(ggplot2)
library(emmeans)
library(multcompView)
library(car)
library(stringr)

# Prepare your dataset
CNP_File <- CNP_File %>%
  filter(Plant == "P") %>%
  mutate(
    Dose = factor(Dose_.N.kg.ha., levels = c("0", "20", "40", "60", "80", "100", "120")),
    Fertilizer_Type = factor(Fertilizer_type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split"))
  )

### 1. SUMMARY ###
CNR_summary <- CNP_File %>%
  group_by(Dose, Fertilizer_Type, Application_Method) %>%
  summarise(
    Mean_CNR = mean(C.N_Root, na.rm = TRUE),
    SE_CNR = sd(C.N_Root, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()

### 2. MODEL + PAIRWISE ###
model_CNR <- lm(C.N_Root ~ Dose * Fertilizer_Type * Application_Method, data = CNP_File)
summary(model_CNR)
Anova(model_CNR, type = "II")

# Tukey HSD test
pairwise_CNR <- emmeans(model_CNR, ~ Dose + Fertilizer_Type | Application_Method)
letters_CNR <- cld(pairwise_CNR, Letters = letters) %>%
  mutate(.group = str_trim(.group)) %>%
  as.data.frame()

# Merge group letters
CNR_summary <- CNR_summary %>%
  left_join(letters_CNR, by = c("Dose", "Fertilizer_Type", "Application_Method"))

### 3. PLOT ###
ggplot(CNR_summary, aes(x = Dose, y = Mean_CNR, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_CNR - SE_CNR, ymax = Mean_CNR + SE_CNR), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  labs(title = "C:N Ratio in Root", 
       x = "Nitrogen Dose (kg/ha)", 
       y = "Mean C:N Root (±SE)", 
       fill = 'Fertilizer') +
  facet_wrap(~ Application_Method) +
  scale_fill_manual(values = c("UF" = "white", "MF" = "gray", "None" = "black")) +
  geom_text(aes(y = Mean_CNR + SE_CNR + 0.5, label = .group), 
            position = position_dodge2(0.9, preserve = 'single')) +
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 12),
    strip.text = element_text(face = "bold", size = 14)
  )

# Load required packages
library(dplyr)
library(ggplot2)
library(lme4)
library(car)
library(emmeans)
library(multcomp)
library(multcompView)
library(stringr)

# STEP 1: Filter and prepare data
cnp_p <- CNP_File %>%
  filter(Plant == "P") %>%
  mutate(
    Dose = factor(Dose_.N.kg.ha.),
    Fertilizer_Type = factor(Fertilizer_type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split")),
    Treatment_Type = factor(Treatment_Type)
  )

model_diag <- lm(C.N_Root ~ Dose + Fertilizer_Type + Application_Method + Treatment_Type, data = cnp_p)

# Residual plot
plot(model_diag$fitted.values, resid(model_diag),
     main = "Residual Plot - C:N Root",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")

# Q-Q plot
qqnorm(resid(model_diag), main = "Q-Q Plot - C:N Root")
qqline(resid(model_diag), col = "red")

CNR_summary <- cnp_p %>%
  group_by(Dose, Fertilizer_Type, Application_Method) %>%
  summarise(
    Mean_CNR = mean(C.N_Root, na.rm = TRUE),
    SE_CNR = sd(C.N_Root, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Linear model
model_CNR <- lm(C.N_Root ~ Dose * Fertilizer_Type * Application_Method, data = cnp_p)
summary(model_CNR)
Anova(model_CNR, type = "II")

# Pairwise comparisons + letter groupings
pairwise_CNR <- emmeans(model_CNR, ~ Dose + Fertilizer_Type | Application_Method)
letters_CNR <- multcomp::cld(pairwise_CNR, Letters = letters) %>%
  mutate(.group = str_trim(.group)) %>%
  as.data.frame()

# Merge with summary
CNR_summary <- CNR_summary %>%
  left_join(letters_CNR, by = c("Dose", "Fertilizer_Type", "Application_Method"))

ggplot(CNR_summary, aes(x = Dose, y = Mean_CNR, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", 
           position = position_dodge2(width = 0.9, preserve = 'single'), 
           colour = "black") +
  geom_errorbar(aes(ymin = Mean_CNR - SE_CNR, ymax = Mean_CNR + SE_CNR), 
                width = 0.2, 
                position = position_dodge2(width = 0.9, preserve = 'single')) +
  geom_text(aes(y = Mean_CNR + SE_CNR + 0.5, label = .group), 
            position = position_dodge2(width = 0.9, preserve = 'single')) +
  labs(title = "C:N Ratio in Root (Phalaris)",
       x = "Nitrogen Dose (kg/ha)", 
       y = "Mean C:N Root (±SE)", 
       fill = "Fertilizer") +
  facet_wrap(~ Application_Method) +
  scale_fill_manual(values = c("UF" = "white", "MF" = "gray", "None" = "black")) +
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 12),
    strip.text = element_text(face = "bold", size = 14)
  )


# Load necessary libraries
library(dplyr)
library(ggplot2)
library(emmeans)
library(multcomp)
library(multcompView)
library(stringr)
library(car)

# STEP 1: Filter for Plant = "P" and convert factors
CNP_P <- CNP_File %>%
  filter(Plant == "P") %>%
  mutate(
    Dose = factor(Dose_.N.kg.ha.),
    Fertilizer_Type = factor(Fertilizer_type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split"))
  )
CNL_summary <- CNP_P %>%
  group_by(Dose, Fertilizer_Type, Application_Method) %>%
  summarise(
    Mean_CNL = mean(C.N_Leaf, na.rm = TRUE),
    SE_CNL = sd(C.N_Leaf, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
# Linear model
model_CNL <- lm(C.N_Leaf ~ Dose * Fertilizer_Type * Application_Method, data = CNP_P)
summary(model_CNL)
Anova(model_CNL, type = "II")

# Pairwise comparisons with letters
pairwise_CNL <- emmeans(model_CNL, ~ Dose + Fertilizer_Type | Application_Method)
letters_CNL <- multcomp::cld(pairwise_CNL, Letters = letters) %>%
  mutate(.group = str_trim(.group)) %>%
  as.data.frame()

# Merge with summary
CNL_summary <- CNL_summary %>%
  left_join(letters_CNL, by = c("Dose", "Fertilizer_Type", "Application_Method"))
ggplot(CNL_summary, aes(x = Dose, y = Mean_CNL, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), 
           colour = "black") +
  geom_errorbar(aes(ymin = Mean_CNL - SE_CNL, ymax = Mean_CNL + SE_CNL), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(y = Mean_CNL + SE_CNL + 0.5, label = .group), 
            position = position_dodge2(0.9, preserve = 'single')) +
  labs(title = "C:N Ratio in Leaf", 
       x = "Dose (N kg/ha)", 
       y = "Mean C:N Leaf (±SE)", 
       fill = "Fertilizer") +
  facet_wrap(~ Application_Method) +
  scale_fill_manual(values = c("UF" = "white", "MF" = "gray", "None" = "black")) +
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 12),
    strip.text = element_text(face = "bold", size = 14)
  )



# Assuming libraries are already loaded from before

# Filter for Plant = "P" and set factor levels
CNP_P <- CNP_File %>%
  filter(Plant == "P") %>%
  mutate(
    Dose = factor(Dose_.N.kg.ha.),
    Fertilizer_Type = factor(Fertilizer_type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split"))
  )
CNS_summary <- CNP_P %>%
  group_by(Dose, Fertilizer_Type, Application_Method) %>%
  summarise(
    Mean_CNS = mean(C.N_Soil, na.rm = TRUE),
    SE_CNS = sd(C.N_Soil, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
# Linear model
model_CNS <- lm(C.N_Soil ~ Dose * Fertilizer_Type * Application_Method, data = CNP_P)
summary(model_CNS)
Anova(model_CNS, type = "II")

# Pairwise comparisons with group letters
pairwise_CNS <- emmeans(model_CNS, ~ Dose + Fertilizer_Type | Application_Method)
letters_CNS <- multcomp::cld(pairwise_CNS, Letters = letters) %>%
  mutate(.group = str_trim(.group)) %>%
  as.data.frame()

# Merge letters into summary table
CNS_summary <- CNS_summary %>%
  left_join(letters_CNS, by = c("Dose", "Fertilizer_Type", "Application_Method"))
ggplot(CNS_summary, aes(x = Dose, y = Mean_CNS, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'),
           colour = "black") +
  geom_errorbar(aes(ymin = Mean_CNS - SE_CNS, ymax = Mean_CNS + SE_CNS),
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(y = Mean_CNS + SE_CNS + 0.5, label = .group),
            position = position_dodge2(0.9, preserve = 'single')) +
  labs(title = "C:N Ratio in Soil",
       x = "Dose (N kg/ha)",
       y = "Mean C:N Soil (±SE)",
       fill = "Fertilizer") +
  facet_wrap(~ Application_Method) +
  scale_fill_manual(values = c("UF" = "white", "MF" = "gray", "None" = "black")) +
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 12),
    strip.text = element_text(face = "bold", size = 14)
  )



# Assuming CNP_File and packages are already loaded

# Load necessary libraries again (if needed)
library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: Filter, rename, and safely select using dplyr::
CNP_P <- CNP_File %>%
  filter(Plant == "P") %>%
  mutate(
    Dose = factor(Dose_.N.kg.ha.),
    Fertilizer_Type = factor(Fertilizer_type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split"))
  ) %>%
  rename(
    MBC = `MB_Soil.conc..ug.C.g.`,
    MBN = `MB_Soil.conc..ug.N.g.`,
    MBP = `MB_Soil.conc..ug.P.g.`
  )

# Now select after renaming
CNP_P_selected <- dplyr::select(CNP_P, Dose, Fertilizer_Type, Application_Method, MBC, MBN, MBP)
mbc_data_long <- tidyr::pivot_longer(CNP_P_selected,
                                     cols = c(MBC, MBN, MBP),
                                     names_to = "MB_Type",
                                     values_to = "Value")
mbc_summary <- mbc_data_long %>%
  group_by(Dose, Fertilizer_Type, Application_Method, MB_Type) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SE = sd(Value, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
ggplot(mbc_summary, aes(x = Dose, y = Mean, fill = MB_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), colour = "black") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                width = 0.2, position = position_dodge(width = 0.9)) +
  facet_wrap(~ Application_Method) +
  labs(title = "Microbial Biomass (C, N, P)",
       x = "Nitrogen Dose (kg/ha)",
       y = "Microbial Biomass (µg/g)",
       fill = "Biomass Type") +
  scale_fill_manual(values = c("MBC" = "black", "MBN" = "gray", "MBP" = "white")) +
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 12),
    strip.text = element_text(face = "bold", size = 14)
  )

# Load libraries if not already
library(dplyr)
library(ggplot2)
library(emmeans)
library(multcomp)
library(multcompView)
library(stringr)
library(car)

# Filter for Plant = P and create factors
CNP_P <- CNP_File %>%
  filter(Plant == "P") %>%
  mutate(
    Dose = factor(Dose_.N.kg.ha.),
    Fertilizer_Type = factor(Fertilizer_type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split"))
  )
SoilP_summary <- CNP_P %>%
  group_by(Dose, Fertilizer_Type, Application_Method) %>%
  summarise(
    Mean_P = mean(`Soil.conc..ug.P.g..NF.`, na.rm = TRUE),
    SE_P = sd(`Soil.conc..ug.P.g..NF.`, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
# Linear model
model_SoilP <- lm(`Soil.conc..ug.P.g..NF.` ~ Dose * Fertilizer_Type * Application_Method, data = CNP_P)
summary(model_SoilP)
Anova(model_SoilP, type = "II")

# Pairwise comparisons
pairwise_SoilP <- emmeans(model_SoilP, ~ Dose + Fertilizer_Type | Application_Method)
letters_SoilP <- multcomp::cld(pairwise_SoilP, Letters = letters) %>%
  mutate(.group = str_trim(.group)) %>%
  as.data.frame()

# Merge letters into summary table
SoilP_summary <- SoilP_summary %>%
  left_join(letters_SoilP, by = c("Dose", "Fertilizer_Type", "Application_Method"))
ggplot(SoilP_summary, aes(x = Dose, y = Mean_P, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'),
           colour = "black") +
  geom_errorbar(aes(ymin = Mean_P - SE_P, ymax = Mean_P + SE_P),
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(y = Mean_P + SE_P + 0.5, label = .group),
            position = position_dodge2(0.9, preserve = 'single')) +
  labs(title = "Soil Available Phosphorus (Bray P) ",
       x = "Dose (N kg/ha)",
       y = "Soil P (µg/g)",
       fill = "Fertilizer") +
  facet_wrap(~ Application_Method) +
  scale_fill_manual(values = c("UF" = "white", "MF" = "gray", "None" = "black")) +
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 12),
    strip.text = element_text(face = "bold", size = 14)
  )

