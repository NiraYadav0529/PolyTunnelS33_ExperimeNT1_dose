list.files()
CNP_lucerne <- read.csv("CNP_data_lucerne.csv")
colnames(CNP_lucerne)
head(CNP_lucerne)

library(dplyr)
library(ggplot2)
library(emmeans)
library(multcomp)
library(multcompView)
library(stringr)
library(car)

# Prepare Lucerne dataset with proper factors
CNP_lucerne <- CNP_lucerne %>%
  mutate(
    Dose = factor(Dose_.N.kg.ha.),
    Fertilizer_Type = factor(Fertilizer_type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split"))
  )
CNR_summary <- CNP_lucerne %>%
  group_by(Dose, Fertilizer_Type, Application_Method) %>%
  summarise(
    Mean_CNR = mean(C.N_Root, na.rm = TRUE),
    SE_CNR = sd(C.N_Root, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
# Fit model
model_CNR <- lm(C.N_Root ~ Dose * Fertilizer_Type * Application_Method, data = CNP_lucerne)
summary(model_CNR)
Anova(model_CNR, type = "II")

# Tukey pairwise with group letters
pairwise_CNR <- emmeans(model_CNR, ~ Dose + Fertilizer_Type | Application_Method)
letters_CNR <- multcomp::cld(pairwise_CNR, Letters = letters) %>%
  mutate(.group = str_trim(.group)) %>%
  as.data.frame()

# Merge with summary
CNR_summary <- CNR_summary %>%
  left_join(letters_CNR, by = c("Dose", "Fertilizer_Type", "Application_Method"))
ggplot(CNR_summary, aes(x = Dose, y = Mean_CNR, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_CNR - SE_CNR, ymax = Mean_CNR + SE_CNR),
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(y = Mean_CNR + SE_CNR + 0.5, label = .group),
            position = position_dodge2(0.9, preserve = 'single')) +
  labs(title = "C:N Ratio in Root (Lucerne)",
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


## Analysis: C.N_Leaf (Lucerne)
CNL_summary <- CNP_lucerne %>%
  group_by(Dose, Fertilizer_Type, Application_Method) %>%
  summarise(
    Mean_CNL = mean(C.N_Leaf, na.rm = TRUE),
    SE_CNL = sd(C.N_Leaf, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
# Linear model
model_CNL <- lm(C.N_Leaf ~ Dose * Fertilizer_Type * Application_Method, data = CNP_lucerne)
summary(model_CNL)
Anova(model_CNL, type = "II")

# Pairwise comparison and group letters
pairwise_CNL <- emmeans(model_CNL, ~ Dose + Fertilizer_Type | Application_Method)
letters_CNL <- multcomp::cld(pairwise_CNL, Letters = letters) %>%
  mutate(.group = str_trim(.group)) %>%
  as.data.frame()

# Merge letters into summary table
CNL_summary <- CNL_summary %>%
  left_join(letters_CNL, by = c("Dose", "Fertilizer_Type", "Application_Method"))
ggplot(CNL_summary, aes(x = Dose, y = Mean_CNL, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'),
           colour = "black") +
  geom_errorbar(aes(ymin = Mean_CNL - SE_CNL, ymax = Mean_CNL + SE_CNL),
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(y = Mean_CNL + SE_CNL + 0.5, label = .group),
            position = position_dodge2(0.9, preserve = 'single')) +
  labs(title = "C:N Ratio in Leaf (Lucerne)",
       x = "Nitrogen Dose (kg/ha)",
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
## Analysis: C.N_Soil (Lucerne)
CNS_summary <- CNP_lucerne %>%
  group_by(Dose, Fertilizer_Type, Application_Method) %>%
  summarise(
    Mean_CNS = mean(C.N_Soil, na.rm = TRUE),
    SE_CNS = sd(C.N_Soil, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
# Linear model
model_CNS <- lm(C.N_Soil ~ Dose * Fertilizer_Type * Application_Method, data = CNP_lucerne)
summary(model_CNS)
Anova(model_CNS, type = "II")

# Pairwise comparison and group letters
pairwise_CNS <- emmeans(model_CNS, ~ Dose + Fertilizer_Type | Application_Method)
letters_CNS <- multcomp::cld(pairwise_CNS, Letters = letters) %>%
  mutate(.group = str_trim(.group)) %>%
  as.data.frame()

# Merge letters into summary
CNS_summary <- CNS_summary %>%
  left_join(letters_CNS, by = c("Dose", "Fertilizer_Type", "Application_Method"))
ggplot(CNS_summary, aes(x = Dose, y = Mean_CNS, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'),
           colour = "black") +
  geom_errorbar(aes(ymin = Mean_CNS - SE_CNS, ymax = Mean_CNS + SE_CNS),
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(y = Mean_CNS + SE_CNS + 0.5, label = .group),
            position = position_dodge2(0.9, preserve = 'single')) +
  labs(title = "C:N Ratio in Soil (Lucerne)",
       x = "Nitrogen Dose (kg/ha)",
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
colnames(CNP_lucerne)
##Combined Plot: Microbial Biomass (Lucerne)
# First, ensure you're using dplyr's select
library(dplyr)
library(tidyr)
library(ggplot2)

# Rename columns for clarity
CNP_lucerne_mbc <- CNP_lucerne %>%
  rename(
    MBC = `MB_Soil.conc..ug.C.g.`,
    MBN = `MB_Soil.conc..ug.N.g.`,
    MBP = `MB_Soil.conc..ug.P.g.`
  )

# Use dplyr::select explicitly to avoid conflicts
CNP_lucerne_mbc_selected <- dplyr::select(CNP_lucerne_mbc, 
                                          Dose = `Dose_.N.kg.ha.`,
                                          Fertilizer_Type = Fertilizer_type,
                                          Application_Method = Application_method,
                                          MBC, MBN, MBP)
mbc_data_lucerne_long <- tidyr::pivot_longer(CNP_lucerne_mbc_selected,
                                             cols = c(MBC, MBN, MBP),
                                             names_to = "MB_Type",
                                             values_to = "Value")
mbc_summary_lucerne <- mbc_data_lucerne_long %>%
  group_by(Dose, Fertilizer_Type, Application_Method, MB_Type) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SE = sd(Value, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
ggplot(mbc_summary_lucerne, aes(x = Dose, y = Mean, fill = MB_Type, group = MB_Type)) +
  geom_bar(stat = "identity", 
           position = position_dodge2(width = 0.9, preserve = "single"), 
           colour = "black") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                width = 0.2, 
                position = position_dodge2(width = 0.9, preserve = "single")) +
  facet_wrap(~ Application_Method) +
  labs(title = "Microbial Biomass (C, N, P) in Lucerne",
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



ggplot(mbc_summary_lucerne, aes(x = Dose, y = Mean, fill = MB_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), colour = "black") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                width = 0.2, position = position_dodge(width = 0.9)) +
  facet_wrap(~ Application_Method) +
  labs(title = "Microbial Biomass (C, N, P) in Lucerne",
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



ggplot(mbc_summary_lucerne, aes(x = Dose, y = Mean, fill = MB_Type, group = MB_Type)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.9), 
           colour = "black") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  facet_wrap(~ Application_Method) +
  labs(title = "Microbial Biomass (C, N, P) in Lucerne",
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



library(dplyr)
library(tidyr)
library(ggplot2)
library(emmeans)
library(multcomp)
library(multcompView)
library(stringr)
library(car)

# Prepare and pivot long
CNP_lucerne_long <- CNP_lucerne %>%
  rename(
    MBC = `MB_Soil.conc..ug.C.g.`,
    MBN = `MB_Soil.conc..ug.N.g.`,
    MBP = `MB_Soil.conc..ug.P.g.`
  ) %>%
  mutate(
    Dose = factor(Dose_.N.kg.ha.),
    Fertilizer_Type = factor(Fertilizer_type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split"))
  ) %>%
  pivot_longer(cols = c(MBC, MBN, MBP),
               names_to = "MB_Type",
               values_to = "Value")

# Summary stats
mbc_summary_lucerne <- CNP_lucerne_long %>%
  group_by(Dose, Fertilizer_Type, Application_Method, MB_Type) %>%
  summarise(
    Mean_Value = mean(Value, na.rm = TRUE),
    SE_Value = sd(Value, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
model_mbc <- lm(Value ~ Dose * Fertilizer_Type * Application_Method * MB_Type, data = CNP_lucerne_long)
summary(model_mbc)
Anova(model_mbc, type = "II")

pairwise_mbc <- emmeans(model_mbc, ~ Dose + Fertilizer_Type + MB_Type | Application_Method)
letters_mbc <- multcomp::cld(pairwise_mbc, Letters = letters) %>%
  mutate(.group = str_trim(.group)) %>%
  as.data.frame()

# Merge letters
mbc_summary_lucerne <- mbc_summary_lucerne %>%
  left_join(letters_mbc, by = c("Dose", "Fertilizer_Type", "Application_Method", "MB_Type"))
ggplot(mbc_summary_lucerne, aes(x = Dose, y = Mean_Value, fill = MB_Type, group = MB_Type)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9),
           colour = "black") +
  geom_errorbar(aes(ymin = Mean_Value - SE_Value, ymax = Mean_Value + SE_Value),
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = Mean_Value + SE_Value + 2, label = .group),
            position = position_dodge(width = 0.9),
            size = 4) +
  facet_wrap(~ Application_Method) +
  labs(title = "Microbial Biomass (C, N, P) in Lucerne",
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

soilP_summary <- CNP_lucerne %>%
  mutate(
    Dose = factor(Dose_.N.kg.ha.),
    Fertilizer_Type = factor(Fertilizer_type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split"))
  ) %>%
  group_by(Dose, Fertilizer_Type, Application_Method) %>%
  summarise(
    Mean_P = mean(`Soil.conc..ug.P.g..NF.`, na.rm = TRUE),
    SE_P = sd(`Soil.conc..ug.P.g..NF.`, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
# Fit model
model_soilP <- lm(`Soil.conc..ug.P.g..NF.` ~ Dose * Fertilizer_type * Application_method, data = CNP_lucerne)
summary(model_soilP)
Anova(model_soilP, type = "II")
# Make sure variables are factors
CNP_lucerne <- CNP_lucerne %>%
  mutate(
    Dose = factor(Dose_.N.kg.ha.),
    Fertilizer_Type = factor(Fertilizer_type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split"))
  )

# Pairwise
pairwise_soilP <- emmeans(model_soilP, ~ Dose + Fertilizer_type | Application_method)
letters_soilP <- multcomp::cld(pairwise_soilP, Letters = letters) %>%
  mutate(.group = str_trim(.group)) %>%
  as.data.frame()

# Merge
soilP_summary <- soilP_summary %>%
  left_join(letters_soilP, by = c("Dose", "Fertilizer_Type", "Application_Method"))
# Rename to match the summary table
letters_soilP <- letters_soilP %>%
  rename(
    Fertilizer_Type = Fertilizer_type,
    Application_Method = Application_method
  )
soilP_summary <- soilP_summary %>%
  left_join(letters_soilP, by = c("Dose", "Fertilizer_Type", "Application_Method"))

ggplot(soilP_summary, aes(x = Dose, y = Mean_P, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", 
           position = position_dodge2(width = 0.9, preserve = 'single'), 
           colour = "black") +
  geom_errorbar(aes(ymin = Mean_P - SE_P, ymax = Mean_P + SE_P),
                width = 0.2, 
                position = position_dodge2(width = 0.9, preserve = 'single')) +
  geom_text(aes(y = Mean_P + SE_P + 0.5, label = .group),
            position = position_dodge2(width = 0.9, preserve = 'single')) +
  facet_wrap(~ Application_Method) +
  labs(title = "Soil Available Phosphorus (Lucerne)",
       x = "Nitrogen Dose (kg/ha)",
       y = "Soil P (µg/g)",
       fill = "Fertilizer") +
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
