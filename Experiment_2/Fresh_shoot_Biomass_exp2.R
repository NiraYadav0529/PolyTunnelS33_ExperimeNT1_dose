# Set working directory and load data
#setwd("C:/Users/90958427/OneDrive - Western Sydney University/PolyTunnelS33_ExperimeNT1_dose/Experiment_2")
list.files()

# Load datasets
Lucerne_exp2 <- read.csv("Lucerne_data-file_Exp2.csv")
Phalaris_exp2 <- read.csv("Phalaris_data-file_Exp2.csv")

# View the first few rows and column names
head(Lucerne_exp2)
colnames(Lucerne_exp2)

#LucerneDataset

# Load packages in the recommended order
library(dplyr)
library(ggplot2)
library(car)           # For Anova and residual plots
library(emmeans)       # For pairwise comparisons
library(multcomp)      # For compact letter display with cld()
library(multcompView)  # Optional for multcomp letters display
library(stringr)       # For string manipulation
# Ensure dataset is set up with factor levels
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
    Mean_Biomass_30 = mean(Fresh.Biomass.gm...30days., na.rm = TRUE),
    SE_Biomass_30 = sd(Fresh.Biomass.gm...30days., na.rm = TRUE) / sqrt(n()),
    Mean_Biomass_56 = mean(Fresh.Biomass..gm._56days_2ndSplitdose, na.rm = TRUE),
    SE_Biomass_56 = sd(Fresh.Biomass..gm._56days_2ndSplitdose, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()  # Make sure data is ungrouped for further operations

# Model for log-transformed Fresh Biomass at 30 days
m1_biomass_30 <- lm(log10(Fresh.Biomass.gm...30days.) ~ Dose * Fertilizer_Type * Application_Method, data = Lucerne_exp2)
summary(m1_biomass_30)

# Perform pairwise comparisons
pairwise_comparisons_30 <- emmeans(m1_biomass_30, ~ Dose * Fertilizer_Type | Application_Method)

# Apply compact letter display with cld
letters_30 <- cld(pairwise_comparisons_30, Letters = "abc") %>%
  as.data.frame() %>%
  mutate(.group = str_trim(.group))

# Pairwise comparisons for Biomass at 56 days
pairwise_comparisons_56 <- emmeans(m1_biomass_30, ~ Dose * Fertilizer_Type | Application_Method)
letters_30 <- cld(pairwise_comparisons_56, Letters = letters) %>%
  mutate(.group = str_trim(.group)) %>%
  as.data.frame()
str(biomass_summary)
# Summarize Data for Plotting
biomass_summary_30 <- left_join(
  biomass_summary %>%
    select(Dose, Fertilizer_Type, Application_Method, Mean_Biomass_30_days, SE_Biomass_30_days),
  letters_30,
  by = c("Dose", "Fertilizer_Type", "Application_Method")
)
# Check the structure of both data frames
str(biomass_summary)
str(letters_30)
# Check column names of letters_30
colnames(letters_30)
str(biomass_summary)
str(letters_30)
install.packages("janitor")
library(janitor)
biomass_summary <- biomass_summary %>% clean_names()
letters_30 <- letters_30 %>% clean_names()
biomass_summary_30 <- left_join(
  biomass_summary,
  letters_30, 
  by = c("Dose", "Fertilizer_Type", "Application_Method")
)
letters_30 <- letters_30 %>%
  rename(Group = .group)

biomass_summary_30 <- left_join(
  biomass_summary,
  letters_30 %>% select(Dose, Fertilizer_Type, Application_Method, Group), 
  by = c("Dose", "Fertilizer_Type", "Application_Method")
)


# Perform the join with proper selection of columns
biomass_summary_30 <- left_join(
  biomass_summary,
  letters_30 %>% select(Dose, Fertilizer_Type, Application_Method, .group), 
  by = c("Dose", "Fertilizer_Type", "Application_Method")
)



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





###For PhAlaris data set 
colnames(Phalaris_exp2)
library(stringr)      # For string manipulation


# Load necessary libraries
library(dplyr)


# Convert columns to factors and rename levels where applicable
Phalaris_exp2 <- Phalaris_exp2 %>%
  mutate(
    Dose = factor(Dose...N.kg.ha., levels = c("0", "10", "20", "30", "40", "50", "60")),
    Fertilizer_Type = factor(Fertilizer.type, levels = c('None', 'MF', 'UF')),
    Application_Method = factor(Application.method, levels = c('One-time', 'Split'))
  )


###For Phalaris 
colnames(Phalaris_exp2)
# Calculate mean and standard errors for biomass and chlorophyll content measurements
biomass_summary_phalaris <- Phalaris_exp2 %>%
  group_by(Dose, Fertilizer_Type, Application_Method) %>%
  summarise(
    Mean_Biomass_35 = mean(Fresh_Biomass..gm._35days, na.rm = TRUE),
    SE_Biomass_35 = sd(Fresh_Biomass..gm._35days, na.rm = TRUE) / sqrt(n()),
    Mean_Biomass_66 = mean(Fresh.Biomass.gm._66days, na.rm = TRUE),
    SE_Biomass_66 = sd(Fresh.Biomass.gm._66days, na.rm = TRUE) / sqrt(n()),
    Mean_Biomass_95 = mean(Fresh.Biomass..gm._95days, na.rm = TRUE),
    SE_Biomass_95 = sd(Fresh.Biomass..gm._95days, na.rm = TRUE) / sqrt(n()),
    Mean_ChlorophyllContent_66 = mean(Cholorphyll.content..Spad._66days, na.rm = TRUE),
    SE_ChlorophyllContent_66 = sd(Cholorphyll.content..Spad._66days, na.rm = TRUE) / sqrt(n()),
    Mean_ChlorophyllContent_95 = mean(Cholorphyll_content_95days, na.rm = TRUE),
    SE_ChlorophyllContent_95 = sd(Cholorphyll_content_95days, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'  # Prevent warning about grouping in summarise
  )

# Model for Fresh Biomass at 35 days
m1_biomass_35 <- lm(Fresh_Biomass..gm._35days ~ Dose * Fertilizer_Type * Application_Method, data = Phalaris_exp2)
summary(m1_biomass_35)

# Check residuals for 35-day biomass model
residualPlot(m1_biomass_35)   # Residual plot
qqPlot(m1_biomass_35)         # Q-Q plot
Anova(m1_biomass_35, type = "II")

# Model for Fresh Biomass at 66 days
m1_biomass_66 <- lm(Fresh.Biomass.gm._66days ~ Dose * Fertilizer_Type * Application_Method, data = Phalaris_exp2)
summary(m1_biomass_66)

# Check residuals for 66-day biomass model
residualPlot(m1_biomass_66)   # Residual plot
qqPlot(m1_biomass_66)         # Q-Q plot
Anova(m1_biomass_66, type = "II")

# Model for Fresh Biomass at 95 days
m1_biomass_95 <- lm(Fresh.Biomass..gm._95days ~ Dose * Fertilizer_Type * Application_Method, data = Phalaris_exp2)
summary(m1_biomass_95)

# Check residuals for 95-day biomass model
residualPlot(m1_biomass_95)   # Residual plot
qqPlot(m1_biomass_95)         # Q-Q plot
Anova(m1_biomass_95, type = "II")

# Model for Chlorophyll Content at 66 days
m1_chlorophyll_66 <- lm(Cholorphyll.content..Spad._66days ~ Dose * Fertilizer_Type * Application_Method, data = Phalaris_exp2)
summary(m1_chlorophyll_66)

# Check residuals for 66-day chlorophyll content model
residualPlot(m1_chlorophyll_66)  # Residual plot
qqPlot(m1_chlorophyll_66)        # Q-Q plot
Anova(m1_chlorophyll_66, type = "II")

# Model for Chlorophyll Content at 95 days
m1_chlorophyll_95 <- lm(Cholorphyll_content_95days ~ Dose * Fertilizer_Type * Application_Method, data = Phalaris_exp2)
summary(m1_chlorophyll_95)

# Check residuals for 95-day chlorophyll content model
residualPlot(m1_chlorophyll_95)  # Residual plot
qqPlot(m1_chlorophyll_95)        # Q-Q plot
Anova(m1_chlorophyll_95, type = "II")

# Pairwise comparisons for Biomass at 35 days
pairwise_comparisons_35 <- emmeans(m1_biomass_35, ~ Dose * Fertilizer_Type | Application_Method)
letters_35 <- cld(pairwise_comparisons_35, Letters = letters) %>%
  mutate(.group = str_trim(.group)) %>%
  as.data.frame()

# Pairwise comparisons for Biomass at 66 days
pairwise_comparisons_66 <- emmeans(m1_biomass_66, ~ Dose * Fertilizer_Type | Application_Method)
letters_66 <- cld(pairwise_comparisons_66, Letters = letters) %>%
  mutate(.group = str_trim(.group)) %>%
  as.data.frame()

# Pairwise comparisons for Biomass at 95 days
pairwise_comparisons_95 <- emmeans(m1_biomass_95, ~ Dose * Fertilizer_Type | Application_Method)
letters_95 <- cld(pairwise_comparisons_95, Letters = letters) %>%
  mutate(.group = str_trim(.group)) %>%
  as.data.frame()

# Pairwise comparisons for Chlorophyll Content at 66 days
pairwise_comparisons_chlorophyll_66 <- emmeans(m1_chlorophyll_66, ~ Dose * Fertilizer_Type | Application_Method)
letters_chlorophyll_66 <- cld(pairwise_comparisons_chlorophyll_66, Letters = letters) %>%
  mutate(.group = str_trim(.group)) %>%
  as.data.frame()

# Pairwise comparisons for Chlorophyll Content at 95 days
pairwise_comparisons_chlorophyll_95 <- emmeans(m1_chlorophyll_95, ~ Dose * Fertilizer_Type | Application_Method)
letters_chlorophyll_95 <- cld(pairwise_comparisons_chlorophyll_95, Letters = letters) %>%
  mutate(.group = str_trim(.group)) %>%
  as.data.frame()

# Summarize Data for Plotting Biomass at 35 days
biomass_summary_35 <- left_join(biomass_summary_phalaris %>% select(Dose, Fertilizer_Type, Application_Method, Mean_Biomass_35, SE_Biomass_35),
                                letters_35, by = c("Dose", "Fertilizer_Type", "Application_Method"))

# Summarize Data for Plotting Biomass at 66 days
biomass_summary_66 <- left_join(biomass_summary_phalaris %>% select(Dose, Fertilizer_Type, Application_Method, Mean_Biomass_66, SE_Biomass_66),
                                letters_66, by = c("Dose", "Fertilizer_Type", "Application_Method"))

# Summarize Data for Plotting Biomass at 95 days
biomass_summary_95 <- left_join(biomass_summary_phalaris %>% select(Dose, Fertilizer_Type, Application_Method, Mean_Biomass_95, SE_Biomass_95),
                                letters_95, by = c("Dose", "Fertilizer_Type", "Application_Method"))

# Summarize Data for Plotting Chlorophyll Content at 66 days
chlorophyll_summary_66 <- left_join(biomass_summary_phalaris %>% select(Dose, Fertilizer_Type, Application_Method, Mean_ChlorophyllContent_66, SE_ChlorophyllContent_66),
                                    letters_chlorophyll_66, by = c("Dose", "Fertilizer_Type", "Application_Method"))

# Summarize Data for Plotting Chlorophyll Content at 95 days
chlorophyll_summary_95 <- left_join(biomass_summary_phalaris %>% select(Dose, Fertilizer_Type, Application_Method, Mean_ChlorophyllContent_95, SE_ChlorophyllContent_95),
                                    letters_chlorophyll_95, by = c("Dose", "Fertilizer_Type", "Application_Method"))

# Plot for Fresh Biomass at 35 Days
ggplot(biomass_summary_35, aes(x = Dose, y = Mean_Biomass_35, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Biomass_35 - SE_Biomass_35, ymax = Mean_Biomass_35 + SE_Biomass_35), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = .group), vjust = -0.5, position = position_dodge(0.9)) + 
  labs(title = "Fresh Biomass at 35 Days", x = "Dose (N kg/ha)", y = "Mean Biomass (g/m²)") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()

# Plot for Fresh Biomass at 66 Days
ggplot(biomass_summary_66, aes(x = Dose, y = Mean_Biomass_66, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Biomass_66 - SE_Biomass_66, ymax = Mean_Biomass_66 + SE_Biomass_66), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = .group), vjust = -0.5, position = position_dodge(0.9)) + 
  labs(title = "Fresh Biomass at 66 Days", x = "Dose (N kg/ha)", y = "Mean Biomass (g/m²)") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()

# Plot for Chlorophyll Content at 66 Days
ggplot(chlorophyll_summary_66, aes(x = Dose, y = Mean_ChlorophyllContent_66, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_ChlorophyllContent_66 - SE_ChlorophyllContent_66, 
                    ymax = Mean_ChlorophyllContent_66 + SE_ChlorophyllContent_66), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = .group), vjust = -0.5, position = position_dodge(0.9)) + 
  labs(title = "Chlorophyll Content at 66 Days", x = "Dose (N kg/ha)", y = "Mean Chlorophyll Content (SPAD)") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()

# Plot for Chlorophyll Content at 95 Days
ggplot(chlorophyll_summary_95, aes(x = Dose, y = Mean_ChlorophyllContent_95, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_ChlorophyllContent_95 - SE_ChlorophyllContent_95, 
                    ymax = Mean_ChlorophyllContent_95 + SE_ChlorophyllContent_95), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = .group), vjust = -0.5, position = position_dodge(0.9)) + 
  labs(title = "Chlorophyll Content at 95 Days", x = "Dose (N kg/ha)", y = "Mean Chlorophyll Content (SPAD)") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()


# Plot for Fresh Biomass at 95 Days
ggplot(biomass_summary_95, aes(x = Dose, y = Mean_Biomass_95, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Biomass_95 - SE_Biomass_95, ymax = Mean_Biomass_95 + SE_Biomass_95), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = .group), vjust = -0.5, position = position_dodge(0.9)) + 
  labs(title = "Fresh Biomass at 95 Days", x = "Dose (N kg/ha)", y = "Mean Biomass (g/m²)") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()


