#PH FOR PHALARIS ANALYSIS 
# Convert columns to factors and rename levels where applicable
Phalaris_exp2 <- Phalaris_exp2 %>%
  mutate(
    Dose = factor(Dose...N.kg.ha., levels = c("0", "20", "40", "60", "80", "100", "120")),
    Fertilizer_Type = factor(Fertilizer.type, levels = c('None', 'MF', 'UF')),
    Application_Method = factor(Application.method, levels = c('One-time', 'Split'))
  )


###For Phalaris 
colnames(Phalaris_exp2)
# Calculate mean and standard errors for biomass and chlorophyll content measurements
biomass_summary_phalaris <- Phalaris_exp2 %>%
  group_by(Dose, Fertilizer_Type, Application_Method) %>%
  summarise(
    Mean_Biomass_35 = mean(pH_41days, na.rm = TRUE),
    SE_Biomass_35 = sd(pH_41days, na.rm = TRUE) / sqrt(n()),
    Mean_Biomass_66 = mean(pH_66days, na.rm = TRUE),
    SE_Biomass_66 = sd(pH_66days, na.rm = TRUE) / sqrt(n()),
    Mean_Biomass_95 = mean(pH_87days, na.rm = TRUE),
    SE_Biomass_95 = sd(pH_87days, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'  # Prevent warning about grouping in summarise
  )

# Model for Fresh Biomass at 35 days
m1_ph_35 <- lm(pH_41days ~ Dose * Fertilizer_Type * Application_Method, data = Phalaris_exp2)
summary(m1_ph_35)
library(car)
# Check residuals for 35-day biomass model
residualPlot(m1_ph_35)   # Residual plot
qqPlot(m1_ph_35)         # Q-Q plot
Anova(m1_ph_35, type = "II")

# Model for Fresh Biomass at 66 days
m1_ph_66 <- lm(pH_66days ~ Dose * Fertilizer_Type * Application_Method, data = Phalaris_exp2)
summary(m1_ph_66)

# Check residuals for 66-day biomass model
residualPlot(m1_ph_66)   # Residual plot
qqPlot(m1_ph_66)         # Q-Q plot
Anova(m1_ph_66, type = "II")

# Model for Fresh Biomass at 95 days
m1_ph_95 <- lm(pH_87days ~ Dose * Fertilizer_Type * Application_Method, data = Phalaris_exp2)
summary(m1_ph_95)

# Check residuals for 95-day biomass model
residualPlot(m1_ph_95)   # Residual plot
qqPlot(m1_ph_95)         # Q-Q plot
Anova(m1_ph_95, type = "II")



# Pairwise comparisons for Biomass at 35 days
pairwise_comparisons_35 <- emmeans(m1_ph_35, ~ Dose * Fertilizer_Type | Application_Method)
letters_35 <- cld(pairwise_comparisons_35, Letters = letters) %>%
  mutate(.group = str_trim(.group)) %>%
  as.data.frame()

# Pairwise comparisons for Biomass at 66 days
pairwise_comparisons_66 <- emmeans(m1_ph_66, ~ Dose * Fertilizer_Type | Application_Method)
letters_66 <- cld(pairwise_comparisons_66, Letters = letters) %>%
  mutate(.group = str_trim(.group)) %>%
  as.data.frame()

# Pairwise comparisons for Biomass at 95 days
pairwise_comparisons_95 <- emmeans(m1_ph_95, ~ Dose * Fertilizer_Type | Application_Method)
letters_95 <- cld(pairwise_comparisons_95, Letters = letters) %>%
  mutate(.group = str_trim(.group)) %>%
  as.data.frame()

head("biomass_summary_phalaris")

# Summarize Data for Plotting Biomass at 35 days
ph_summary_35 <- left_join(biomass_summary_phalaris %>% select(Dose, Fertilizer_Type, Application_Method, Mean_Biomass_35, SE_Biomass_35),
                                letters_35, by = c("Dose", "Fertilizer_Type", "Application_Method"))

# Summarize Data for Plotting Biomass at 66 days
ph_summary_66 <- left_join(biomass_summary_phalaris %>% select(Dose, Fertilizer_Type, Application_Method, Mean_Biomass_66, SE_Biomass_66),
                                letters_66, by = c("Dose", "Fertilizer_Type", "Application_Method"))

# Summarize Data for Plotting Biomass at 95 days
ph_summary_95 <- left_join(biomass_summary_phalaris %>% select(Dose, Fertilizer_Type, Application_Method, Mean_Biomass_95, SE_Biomass_95),
                                letters_95, by = c("Dose", "Fertilizer_Type", "Application_Method"))



# Plot for Fresh Biomass at 35 Days
ggplot(ph_summary_35, aes(x = Dose, y = Mean_Biomass_35, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Biomass_35 - SE_Biomass_35, ymax = Mean_Biomass_35 + SE_Biomass_35), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = .group), vjust = -0.5, position = position_dodge(0.9)) + 
  labs(title = "ph at 35 Days", x = "Dose (N kg/ha)", y = "Mean Biomass (g/m²)") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()

# Plot for Fresh Biomass at 66 Days
ggplot(ph_summary_66, aes(x = Dose, y = Mean_Biomass_66, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Biomass_66 - SE_Biomass_66, ymax = Mean_Biomass_66 + SE_Biomass_66), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = .group), vjust = -0.5, position = position_dodge(0.9)) + 
  labs(title = "ph at 66 Days", x = "Dose (N kg/ha)", y = "Mean Biomass (g/m²)") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()



# Plot for Fresh Biomass at 95 Days
ggplot(ph_summary_95, aes(x = Dose, y = Mean_Biomass_95, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Biomass_95 - SE_Biomass_95, ymax = Mean_Biomass_95 + SE_Biomass_95), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = .group), vjust = -0.5, position = position_dodge(0.9)) + 
  labs(title = "ph at 95 Days", x = "Dose (N kg/ha)", y = "Mean Biomass (g/m²)") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()






#PH FOR lUCERNE  ANALYSIS 


# Load datasets
Lucerne_exp2 <- read.csv("Lucerne_data-file_Exp2.csv")
head(Lucerne_exp2)
colnames(Lucerne_exp2)
# Convert columns to factors and rename levels where applicable
Lucerne_exp2 <- Lucerne_exp2 %>%
  mutate(
    Dose = factor(Dose...N.kg.ha., levels = c("0", "10", "20", "30", "40", "50", "60")),
    Fertilizer_Type = factor(Fertilizer.type, levels = c('None', 'MF', 'UF')),
    Application_Method = factor(Application_method, levels = c('One-time', 'Split'))
  )

# Calculate mean and standard errors for biomass measurements
biomass_summary <- Lucerne_exp2 %>%
  group_by(Dose, Fertilizer_Type, Application_Method) %>%
  summarise(
    Mean_Biomass_30 = mean(pH_48days, na.rm = TRUE),
    SE_Biomass_30 = sd(pH_48days, na.rm = TRUE) / sqrt(n()),
    Mean_Biomass_56 = mean(pH_76days, na.rm = TRUE),
    SE_Biomass_56 = sd(pH_76days, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'  # Prevent warning about grouping in summarise
  )

# Model for Fresh Biomass at 30 days
m1_PH_48 <- lm(pH_48days ~ Dose * Fertilizer_Type * Application_Method, data = Lucerne_exp2)
summary(m1_PH_48)

# Check residuals for 30-day biomass model
residualPlot(m1_PH_48)   # Residual plot
qqPlot(m1_PH_48)          # Q-Q plot
Anova(m1_PH_48, type = "II")

# Model for Fresh Biomass at 56 days
m1_PH_76 <- lm(pH_76days ~ Dose * Fertilizer_Type * Application_Method, data = Lucerne_exp2)
summary(m1_PH_76)

# Check residuals for 56-day biomass model
residualPlot(m1_PH_76)   # Residual plot
qqPlot(m1_PH_76)          # Q-Q plot
Anova(m1_PH_76, type = "II")

# Pairwise comparisons for Biomass at 30 days
pairwise_comparisons_30 <- emmeans(m1_PH_48, ~ Dose * Fertilizer_Type | Application_Method)
letters_30 <- cld(pairwise_comparisons_30, Letters = letters) %>%
  mutate(.group = str_trim(.group)) %>%
  as.data.frame()

# Pairwise comparisons for Biomass at 56 days
pairwise_comparisons_56 <- emmeans(m1_PH_76, ~ Dose * Fertilizer_Type | Application_Method)
letters_56 <- cld(pairwise_comparisons_56, Letters = letters) %>%
  mutate(.group = str_trim(.group)) %>%
  as.data.frame()

# Summarize Data for Plotting
PH_summary_48 <- left_join(biomass_summary %>% select(Dose, Fertilizer_Type, Application_Method, Mean_Biomass_30, SE_Biomass_30),
                                letters_30, by = c("Dose", "Fertilizer_Type", "Application_Method"))

PH_summary_56 <- left_join(biomass_summary %>% select(Dose, Fertilizer_Type, Application_Method, Mean_Biomass_56, SE_Biomass_56),
                                letters_56, by = c("Dose", "Fertilizer_Type", "Application_Method"))

# Plot for Fresh Biomass at 30 Days
ggplot(PH_summary_48, aes(x = Dose, y = Mean_Biomass_30, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Biomass_30 - SE_Biomass_30, ymax = Mean_Biomass_30 + SE_Biomass_30), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = .group), vjust = -0.5, position = position_dodge(0.9)) + 
  labs(title = "PH at 48 Days", x = "Dose (N kg/ha)", y = "Mean Biomass (g/m²)") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()

# Plot for Fresh Biomass at 56 Days
ggplot(PH_summary_56, aes(x = Dose, y = Mean_Biomass_56, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Biomass_56 - SE_Biomass_56, ymax = Mean_Biomass_56 + SE_Biomass_56), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = .group), vjust = -0.5, position = position_dodge(0.9)) + 
  labs(title = "ph at 76 Days", x = "Dose (N kg/ha)", y = "Mean Biomass (g/m²)") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()




# Calculate mean and standard errors for biomass measurements
biomass_summary <- Lucerne_exp2 %>%
  group_by(Dose, Fertilizer_Type, Application_Method) %>%
  summarise(
    Mean_Biomass_30 = mean(Fresh.Biomass.gm...30days., na.rm = TRUE),
    SE_Biomass_30 = sd(Fresh.Biomass.gm...30days., na.rm = TRUE) / sqrt(n()),
    Mean_Biomass_56 = mean(Fresh.Biomass..gm._56days_2ndSplitdose, na.rm = TRUE),
    SE_Biomass_56 = sd(Fresh.Biomass..gm._56days_2ndSplitdose, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'  # Prevent warning about grouping in summarise
  )

# Model for Fresh Biomass at 30 days
m1_biomass_30 <- lm(Fresh.Biomass.gm...30days. ~ Dose * Fertilizer_Type * Application_Method, data = Lucerne_exp2)
summary(m1_biomass_30)

# Check residuals for 30-day biomass model
residualPlot(m1_biomass_30)   # Residual plot
qqPlot(m1_biomass_30)          # Q-Q plot
Anova(m1_biomass_30, type = "II")

# Model for Fresh Biomass at 56 days
m1_biomass_56 <- lm(Fresh.Biomass..gm._56days_2ndSplitdose ~ Dose * Fertilizer_Type * Application_Method, data = Lucerne_exp2)
summary(m1_biomass_56)

# Check residuals for 56-day biomass model
residualPlot(m1_biomass_56)   # Residual plot
qqPlot(m1_biomass_56)          # Q-Q plot
Anova(m1_biomass_56, type = "II")

# Pairwise comparisons for Biomass at 30 days
pairwise_comparisons_30 <- emmeans(m1_biomass_30, ~ Dose * Fertilizer_Type | Application_Method)
letters_30 <- cld(pairwise_comparisons_30, Letters = letters) %>%
  mutate(.group = str_trim(.group)) %>%
  as.data.frame()

# Pairwise comparisons for Biomass at 56 days
pairwise_comparisons_56 <- emmeans(m1_biomass_56, ~ Dose * Fertilizer_Type | Application_Method)
letters_56 <- cld(pairwise_comparisons_56, Letters = letters) %>%
  mutate(.group = str_trim(.group)) %>%
  as.data.frame()

# Summarize Data for Plotting
biomass_summary_30 <- left_join(biomass_summary %>% select(Dose, Fertilizer_Type, Application_Method, Mean_Biomass_30, SE_Biomass_30),
                                letters_30, by = c("Dose", "Fertilizer_Type", "Application_Method"))

biomass_summary_56 <- left_join(biomass_summary %>% select(Dose, Fertilizer_Type, Application_Method, Mean_Biomass_56, SE_Biomass_56),
                                letters_56, by = c("Dose", "Fertilizer_Type", "Application_Method"))

# Plot for Fresh Biomass at 30 Days
ggplot(biomass_summary_30, aes(x = Dose, y = Mean_Biomass_30, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Biomass_30 - SE_Biomass_30, ymax = Mean_Biomass_30 + SE_Biomass_30), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = .group), vjust = -0.5, position = position_dodge(0.9)) + 
  labs(title = "Fresh Biomass at 30 Days", x = "Dose (N kg/ha)", y = "Mean Biomass (g/m²)") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()

# Plot for Fresh Biomass at 56 Days
ggplot(biomass_summary_56, aes(x = Dose, y = Mean_Biomass_56, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Biomass_56 - SE_Biomass_56, ymax = Mean_Biomass_56 + SE_Biomass_56), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = .group), vjust = -0.5, position = position_dodge(0.9)) + 
  labs(title = "Fresh Biomass at 56 Days", x = "Dose (N kg/ha)", y = "Mean Biomass (g/m²)") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()

