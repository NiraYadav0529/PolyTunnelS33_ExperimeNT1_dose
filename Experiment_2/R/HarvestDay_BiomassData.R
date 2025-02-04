# Set working directory and load data
setwd("C:/Users/90958427/OneDrive - Western Sydney University/PolyTunnelS33_ExperimeNT1_dose/Experiment_2")
list.files()

# Load datasets
Lucerne_Biomass <- read.csv("Lucerne_Biomass_data_Harvestday.csv")
Phalaris_Biomass <- read.csv("Phalaris_Biomass_data_Harvestday.csv")

# View the first few rows and column names
head(Lucerne_Biomass)
colnames(Lucerne_Biomass)

# Load required libraries
library(dplyr)
library(ggplot2)
library(car)
library(emmeans)
library(multcomp)
library(multcompView)
library(stringr)
library(janitor)

# Prepare the dataset with proper factor levels for Lucerne
Lucerne_Dataset <- Lucerne_Biomass %>%
  mutate(
    Dose = factor(Dose...N.kg.ha., levels = c("0", "10", "20", "30", "40", "50", "60")),
    Fertilizer_Type = factor(Fertilizer.type, levels = c("Control", "MF", "UF")),
    Application_Method = factor(Application.method, levels = c("Split", "One-time"))
  )

# Calculate mean and standard errors for biomass measurements
biomass_summary <- Lucerne_Dataset %>%
  group_by(Dose, Fertilizer_Type, Application_Method) %>%
  summarise(
    Mean_Biomass_Shoot = mean(Fresh.Biomass_shoot, na.rm = TRUE),
    SE_Biomass_Shoot = sd(Fresh.Biomass_shoot, na.rm = TRUE) / sqrt(n()),
    Mean_Shoot_Dry_Weight = mean(Shoot.Dry.weight..g., na.rm = TRUE),
    SE_Shoot_Dry_Weight = sd(Shoot.Dry.weight..g., na.rm = TRUE) / sqrt(n()),
    Mean_Biomass_Root = mean(Fresh_Biomass_root, na.rm = TRUE),
    SE_Biomass_Root = sd(Fresh_Biomass_root, na.rm = TRUE) / sqrt(n()),
    Mean_Root_Dry_Weight = mean(Root.Dry.weight..g., na.rm = TRUE),
    SE_Root_Dry_Weight = sd(Root.Dry.weight..g., na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()

# Ensure there are no unused levels or missing data
Lucerne_Dataset <- Lucerne_Dataset %>%
  droplevels() %>%
  filter(!is.na(Fresh.Biomass_shoot) & !is.na(Dose) & !is.na(Fertilizer_Type) & !is.na(Application_Method))

# Fit the model for log-transformed Fresh Biomass of Shoot
m1_biomass_shoot <- lm(log10(Fresh.Biomass_shoot) ~ Dose * Fertilizer_Type * Application_Method, data = Lucerne_Dataset)
summary(m1_biomass_shoot)

# Perform pairwise comparisons
pairwise_comparisons_shoot <- emmeans(m1_biomass_shoot, ~ Dose * Fertilizer_Type | Application_Method)

# Apply compact letter display
letters_shoot <- cld(pairwise_comparisons_shoot, Letters = "abc") %>%
  as.data.frame() %>%
  mutate(.group = str_trim(.group))

# Summarize data for plotting Fresh Biomass of Shoot
biomass_summary_shoot <- left_join(
  biomass_summary %>%
    select(Dose, Fertilizer_Type, Application_Method, Mean_Biomass_Shoot, SE_Biomass_Shoot),
  letters_shoot %>% select(Dose, Fertilizer_Type, Application_Method, .group),
  by = c("Dose", "Fertilizer_Type", "Application_Method")
)

str(biomass_summary)
head(biomass_summary)
colnames(biomass_summary)

biomass_summary_shoot <- dplyr::select(
  biomass_summary, 
  Dose, Fertilizer_Type, Application_Method, Mean_Biomass_Shoot, SE_Biomass_Shoot
)

str(letters_shoot)
head(letters_shoot)
biomass_summary_shoot <- biomass_summary %>%
  dplyr::select(Dose, Fertilizer_Type, Application_Method, Mean_Biomass_Shoot, SE_Biomass_Shoot) %>%
  dplyr::left_join(
    letters_shoot %>% dplyr::select(Dose, Fertilizer_Type, Application_Method, Group),
    by = c("Dose", "Fertilizer_Type", "Application_Method")
  )
str(biomass_summary_shoot)

# Debug and check biomass_summary
str(biomass_summary)
head(biomass_summary)

# Check letters_shoot
str(letters_shoot)

# Rename and select columns in letters_shoot
letters_shoot <- letters_shoot %>%
  rename(Group = .group) %>%
  dplyr::select(Dose, Fertilizer_Type, Application_Method, Group)

# Rejoin data
biomass_summary_shoot <- biomass_summary %>%
  dplyr::select(Dose, Fertilizer_Type, Application_Method, Mean_Biomass_Shoot, SE_Biomass_Shoot) %>%
  dplyr::left_join(letters_shoot, by = c("Dose", "Fertilizer_Type", "Application_Method"))

# Check final structure
str(biomass_summary_shoot)

# Plot Fresh Biomass of Shoot
ggplot(biomass_summary_shoot, aes(x = Dose, y = Mean_Biomass_Shoot, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Biomass_Shoot - SE_Biomass_Shoot, ymax = Mean_Biomass_Shoot + SE_Biomass_Shoot),
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = Group), vjust = -0.5, position = position_dodge(0.9)) +
  labs(title = "Fresh Biomass of Shoot", x = "Dose (N kg/ha)", y = "Mean Fresh Biomass (g/m²)") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()


# Dry Shoot Biomass Analysis and Plot

# Fit the model for Dry Shoot Biomass
m1_dry_shoot <- lm(log10(Shoot.Dry.weight..g.) ~ Dose * Fertilizer_Type * Application_Method, data = Lucerne_Dataset)
summary(m1_dry_shoot)

# Perform pairwise comparisons for Dry Shoot Biomass
pairwise_comparisons_dry_shoot <- emmeans(m1_dry_shoot, ~ Dose * Fertilizer_Type | Application_Method)

# Apply compact letter display
letters_dry_shoot <- cld(pairwise_comparisons_dry_shoot, Letters = "abc") %>%
  as.data.frame() %>%
  mutate(Group = str_trim(.group))

# Summarize data for plotting
biomass_summary_dry_shoot <- biomass_summary %>%
  dplyr::select(Dose, Fertilizer_Type, Application_Method, Mean_Shoot_Dry_Weight, SE_Shoot_Dry_Weight) %>%
  dplyr::left_join(
    letters_dry_shoot %>% dplyr::select(Dose, Fertilizer_Type, Application_Method, Group),
    by = c("Dose", "Fertilizer_Type", "Application_Method")
  )

# Plot for Dry Shoot Biomass
ggplot(biomass_summary_dry_shoot, aes(x = Dose, y = Mean_Shoot_Dry_Weight, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Shoot_Dry_Weight - SE_Shoot_Dry_Weight, ymax = Mean_Shoot_Dry_Weight + SE_Shoot_Dry_Weight),
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = Group), vjust = -0.5, position = position_dodge(0.9)) +
  labs(title = "Dry Shoot Biomass", x = "Dose (N kg/ha)", y = "Mean Dry Biomass (g/m²)") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()


# Fresh Root Biomass Analysis and Plot

# Fit the model for Fresh Root Biomass
m1_fresh_root <- lm(log10(Fresh_Biomass_root) ~ Dose * Fertilizer_Type * Application_Method, data = Lucerne_Dataset)
summary(m1_fresh_root)

# Perform pairwise comparisons for Fresh Root Biomass
pairwise_comparisons_fresh_root <- emmeans(m1_fresh_root, ~ Dose * Fertilizer_Type | Application_Method)

# Apply compact letter display
letters_fresh_root <- cld(pairwise_comparisons_fresh_root, Letters = "abc") %>%
  as.data.frame() %>%
  mutate(Group = str_trim(.group))

# Summarize data for plotting
biomass_summary_fresh_root <- biomass_summary %>%
  dplyr::select(Dose, Fertilizer_Type, Application_Method, Mean_Biomass_Root, SE_Biomass_Root) %>%
  dplyr::left_join(
    letters_fresh_root %>% dplyr::select(Dose, Fertilizer_Type, Application_Method, Group),
    by = c("Dose", "Fertilizer_Type", "Application_Method")
  )

# Plot for Fresh Root Biomass
ggplot(biomass_summary_fresh_root, aes(x = Dose, y = Mean_Biomass_Root, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Biomass_Root - SE_Biomass_Root, ymax = Mean_Biomass_Root + SE_Biomass_Root),
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = Group), vjust = -0.5, position = position_dodge(0.9)) +
  labs(title = "Fresh Root Biomass", x = "Dose (N kg/ha)", y = "Mean Fresh Biomass (g/m²)") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()


# Dry Root Biomass Analysis and Plot

# Fit the model for Dry Root Biomass
m1_dry_root <- lm(log10(Root.Dry.weight..g.) ~ Dose * Fertilizer_Type * Application_Method, data = Lucerne_Dataset)
summary(m1_dry_root)

# Perform pairwise comparisons for Dry Root Biomass
pairwise_comparisons_dry_root <- emmeans(m1_dry_root, ~ Dose * Fertilizer_Type | Application_Method)

# Apply compact letter display
letters_dry_root <- cld(pairwise_comparisons_dry_root, Letters = "abc") %>%
  as.data.frame() %>%
  mutate(Group = str_trim(.group))

# Summarize data for plotting
biomass_summary_dry_root <- biomass_summary %>%
  dplyr::select(Dose, Fertilizer_Type, Application_Method, Mean_Root_Dry_Weight, SE_Root_Dry_Weight) %>%
  dplyr::left_join(
    letters_dry_root %>% dplyr::select(Dose, Fertilizer_Type, Application_Method, Group),
    by = c("Dose", "Fertilizer_Type", "Application_Method")
  )

# Plot for Dry Root Biomass
ggplot(biomass_summary_dry_root, aes(x = Dose, y = Mean_Root_Dry_Weight, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Root_Dry_Weight - SE_Root_Dry_Weight, ymax = Mean_Root_Dry_Weight + SE_Root_Dry_Weight),
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = Group), vjust = -0.5, position = position_dodge(0.9)) +
  labs(title = "Dry Root Biomass", x = "Dose (N kg/ha)", y = "Mean Dry Biomass (g/m²)") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()





# Load datasets

Phalaris_Biomass <- read.csv("Phalaris_Biomass_data_Harvestday.csv")

# View the first few rows and column names
head(Phalaris_Biomass)
colnames(Phalaris_Biomass)

# Load required libraries
library(dplyr)
library(ggplot2)
library(car)
library(emmeans)
library(multcomp)
library(multcompView)
library(stringr)
library(janitor)

# Prepare the dataset with proper factor levels for Phalaris
Phalaris_Dataset <- Phalaris_Biomass %>%
  mutate(
    Dose = factor(Dose...N.kg.ha., levels = c("0", "20", "40", "60", "80", "100", "120")),
    Fertilizer_Type = factor(Fertilizer.type, levels = c("Control", "UF", "MF")),
    Application_Method = factor(Application.method, levels = c("Split", "One-time"))
  )

# Calculate mean and standard errors for biomass measurements
biomass_summary_phalaris <- Phalaris_Dataset %>%
  group_by(Dose, Fertilizer_Type, Application_Method) %>%
  summarise(
    Mean_Biomass_Shoot = mean(Fresh.Biomass_shoot, na.rm = TRUE),
    SE_Biomass_Shoot = sd(Fresh.Biomass_shoot, na.rm = TRUE) / sqrt(n()),
    Mean_Shoot_Dry_Weight = mean(Shoot.Dry.weight..g., na.rm = TRUE),
    SE_Shoot_Dry_Weight = sd(Shoot.Dry.weight..g., na.rm = TRUE) / sqrt(n()),
    Mean_Biomass_Root = mean(Fresh_Biomass_root, na.rm = TRUE),
    SE_Biomass_Root = sd(Fresh_Biomass_root, na.rm = TRUE) / sqrt(n()),
    Mean_Root_Dry_Weight = mean(Root.Dry.weight..g., na.rm = TRUE),
    SE_Root_Dry_Weight = sd(Root.Dry.weight..g., na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()

# Ensure there are no unused levels or missing data
Phalaris_Dataset <- Phalaris_Dataset %>%
  droplevels() %>%
  filter(!is.na(Fresh.Biomass_shoot) & !is.na(Dose) & !is.na(Fertilizer_Type) & !is.na(Application_Method))

# Fit the model for log-transformed Fresh Biomass of Shoot
m1_biomass_shoot <- lm((Fresh.Biomass_shoot) ~ Dose * Fertilizer_Type * Application_Method, data = Phalaris_Dataset)
summary(m1_biomass_shoot)


# Perform pairwise comparisons for Fresh Biomass of Shoot
pairwise_comparisons_shoot <- emmeans(m1_biomass_shoot, ~ Dose * Fertilizer_Type | Application_Method)


# Perform pairwise comparisons for Fresh Biomass of Shoot
pairwise_comparisons_shoot <- emmeans(m1_biomass_shoot, ~ Dose * Fertilizer_Type | Application_Method)

# Apply compact letter display
letters_shoot <- cld(pairwise_comparisons_shoot, Letters = "abc") %>%
  as.data.frame() %>%
  mutate(.group = str_trim(.group))
colnames(biomass_summary_phalaris)
colnames(letters_shoot)
# Summarize data for plotting Fresh Biomass of Shoot
biomass_summary_shoot <- left_join(
  biomass_summary_phalaris %>%
    select(Dose, Fertilizer_Type, Application_Method, Mean_Biomass_Shoot, SE_Biomass_Shoot),
  letters_shoot %>%
    select(Dose, Fertilizer_Type, Application_Method, .group),
  by = c("Dose", "Fertilizer_Type", "Application_Method")
)
biomass_summary_phalaris <- biomass_summary_phalaris %>%
  group_by(Dose, Fertilizer_Type, Application_Method) %>%
  summarise(
    Mean_Biomass_Shoot = mean(Fresh.Biomass_shoot, na.rm = TRUE),
    SE_Biomass_Shoot = sd(Fresh.Biomass_shoot, na.rm = TRUE) / sqrt(n())
  )

str(biomass_summary_phalaris)
str(letters_shoot)


# Plot Fresh Biomass of Shoot
ggplot(biomass_summary_shoot, aes(x = Dose, y = Mean_Biomass_Shoot, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Biomass_Shoot - SE_Biomass_Shoot, ymax = Mean_Biomass_Shoot + SE_Biomass_Shoot),
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = .group), vjust = -0.5, position = position_dodge(0.9)) +
  labs(title = "Fresh Biomass of Shoot", x = "Dose (N kg/ha)", y = "Mean Fresh Biomass (g/m²)") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()

# Dry Shoot Biomass Analysis and Plot
m1_dry_shoot <- lm(log10(Shoot.Dry.weight..g.) ~ Dose * Fertilizer_Type * Application_Method, data = Phalaris_Dataset)
summary(m1_dry_shoot)

pairwise_comparisons_dry_shoot <- emmeans(m1_dry_shoot, ~ Dose * Fertilizer_Type | Application_Method)

letters_dry_shoot <- cld(pairwise_comparisons_dry_shoot, Letters = "abc") %>%
  as.data.frame() %>%
  mutate(Group = str_trim(.group))

biomass_summary_dry_shoot <- biomass_summary_phalaris %>%
  dplyr::select(Dose, Fertilizer_Type, Application_Method, Mean_Shoot_Dry_Weight, SE_Shoot_Dry_Weight) %>%
  dplyr::left_join(
    letters_dry_shoot %>% dplyr::select(Dose, Fertilizer_Type, Application_Method, Group),
    by = c("Dose", "Fertilizer_Type", "Application_Method")
  )

# Plot for Dry Shoot Biomass
ggplot(biomass_summary_dry_shoot, aes(x = Dose, y = Mean_Shoot_Dry_Weight, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Shoot_Dry_Weight - SE_Shoot_Dry_Weight, ymax = Mean_Shoot_Dry_Weight + SE_Shoot_Dry_Weight),
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = Group), vjust = -0.5, position = position_dodge(0.9)) +
  labs(title = "Dry Shoot Biomass", x = "Dose (N kg/ha)", y = "Mean Dry Biomass (g/m²)") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()

# Fresh Root Biomass Analysis and Plot
m1_fresh_root <- lm(log10(Fresh_Biomass_root) ~ Dose * Fertilizer_Type * Application_Method, data = Phalaris_Dataset)
summary(m1_fresh_root)

pairwise_comparisons_fresh_root <- emmeans(m1_fresh_root, ~ Dose * Fertilizer_Type | Application_Method)

letters_fresh_root <- cld(pairwise_comparisons_fresh_root, Letters = "abc") %>%
  as.data.frame() %>%
  mutate(Group = str_trim(.group))

biomass_summary_fresh_root <- biomass_summary_phalaris %>%
  dplyr::select(Dose, Fertilizer_Type, Application_Method, Mean_Biomass_Root, SE_Biomass_Root) %>%
  dplyr::left_join(
    letters_fresh_root %>% dplyr::select(Dose, Fertilizer_Type, Application_Method, Group),
    by = c("Dose", "Fertilizer_Type", "Application_Method")
  )

# Plot for Fresh Root Biomass
ggplot(biomass_summary_fresh_root, aes(x = Dose, y = Mean_Biomass_Root, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Biomass_Root - SE_Biomass_Root, ymax = Mean_Biomass_Root + SE_Biomass_Root),
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = Group), vjust = -0.5, position = position_dodge(0.9)) +
  labs(title = "Fresh Root Biomass", x = "Dose (N kg/ha)", y = "Mean Fresh Biomass (g/m²)") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()

# Dry Root Biomass Analysis and Plot
m1_dry_root <- lm(log10(Root.Dry.weight..g.) ~ Dose * Fertilizer_Type * Application_Method, data = Phalaris_Dataset)
summary(m1_dry_root)

pairwise_comparisons_dry_root <- emmeans(m1_dry_root, ~ Dose * Fertilizer_Type | Application_Method)

letters_dry_root <- cld(pairwise_comparisons_dry_root, Letters = "abc") %>%
  as.data.frame() %>%
  mutate(Group = str_trim(.group))

biomass_summary_dry_root <- biomass_summary_phalaris %>%
  dplyr::select(Dose, Fertilizer_Type, Application_Method, Mean_Root_Dry_Weight, SE_Root_Dry_Weight) %>%
  dplyr::left_join(
    letters_dry_root %>% dplyr::select(Dose, Fertilizer_Type, Application_Method, Group),
    by = c("Dose", "Fertilizer_Type", "Application_Method")
  )

# Plot for Dry Root Biomass
ggplot(biomass_summary_dry_root, aes(x = Dose, y = Mean_Root_Dry_Weight, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), colour = "black") +
  geom_errorbar(aes(ymin = Mean_Root_Dry_Weight - SE_Root_Dry_Weight, ymax = Mean_Root_Dry_Weight + SE_Root_Dry_Weight),
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = Group), vjust = -0.5, position = position_dodge(0.9)) +
  labs(title = "Dry Root Biomass", x = "Dose (N kg/ha)", y = "Mean Dry Biomass (g/m²)") +
  facet_wrap(~ Application_Method, scales = "free") +
  theme_minimal()
