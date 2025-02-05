setwd("C:/Users/90958427/OneDrive - Western Sydney University/PolyTunnelS33_ExperimeNT1_dose/Experiment_2/Modified_Data_File")
list.files()
Phalaris_exp2 <- read.csv("Phalaris_FinalData_analysis.csv", check.names = TRUE)
colnames( Phalaris_exp2)
##loading required libraries 

library(lme4)      # Linear mixed-effects modeling
library(car)       # ANOVA and model diagnostics
library(ggplot2)   # Visualization
library(dplyr)     # Data manipulation
library(tidyr)     # Data reshaping
library(ggeffects) # Predicted values extraction

##converting catagorical variables to factors 
Phalaris_exp2 <- Phalaris_exp2 %>%
  mutate(
    Dose = factor(Dose_.N.kg.ha., levels = c("0", "20", "40", "60", "80", "100", "120")),
    Fertilizer_Type = factor(Fertilizer_type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split"))
  )


##Step 3: Extract and Reshape pH-related Columns
pH_columns <- grep("^pH_", names(Phalaris_exp2), value = TRUE)

pH_data <- Phalaris_exp2 %>%
  select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method, all_of(pH_columns)) %>%
  pivot_longer(cols = starts_with("pH_"), names_to = "Date", values_to = "pH_Level") %>%
  mutate(Date = factor(Date), Combined.pot.id = as.character(Combined.pot.id)) %>%
  drop_na(pH_Level)
###Step 4: Ensure Correct Ordering of Time Points
pH_levels_order <- paste0("pH_", 1:10)

pH_data$Date <- factor(pH_data$Date, levels = pH_levels_order, ordered = TRUE)


##Step 5: Apply log10 Transformation to pH Levels
pH_data <- pH_data %>%
  mutate(log_pH_Level = log10(pH_Level))
##Step 6: Fit Linear Mixed-Effects Model for pH
m1_pH <- lmer(log_pH_Level ~ Dose * Date + (1|Combined.pot.id), data = pH_data)
plot(m1_pH)
qqPlot(resid(m1_pH))
Anova(m1_pH, test="F")
summary(m1_pH)
##Step 7: Separate One-Time and Split Application for pH

pH_one_time <- pH_data %>% filter(Application_Method == "One-time")
pH_split <- pH_data %>% filter(Application_Method == "Split")

##Step 8: Boxplots for pH 1.One-time Application
ggplot(pH_one_time, aes(x = Date, y = log_pH_Level, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = mean(pH_one_time$log_pH_Level[pH_one_time$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "log10(pH) Levels Over Time (One-time Application)", x = "Date", y = "log10(pH)", fill = "Dose", color = "Dose") +
  scale_x_discrete(limits = pH_levels_order) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))
##Box Plot for pH (Split Application) 2. Split application 
ggplot(pH_split, aes(x = Date, y = log_pH_Level, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = mean(pH_split$log_pH_Level[pH_split$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "log10(pH) Levels Over Time (Split Application)",
       x = "Date", y = "log10(pH)", fill = "Dose", color = "Dose") +
  scale_x_discrete(limits = pH_levels_order) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))
head(Phalaris_exp2)


Phalaris_exp2 <- Phalaris_exp2 %>%
  mutate(Application_method = ifelse(Treatment_Type == "Control", "None", Application_method))
pH_columns <- grep("^pH_", names(Phalaris_exp2), value = TRUE)

pH_data <- Phalaris_exp2 %>%
  select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method, Treatment_Type, all_of(pH_columns)) %>%
  pivot_longer(cols = starts_with("pH_"), names_to = "Date", values_to = "pH_Level") %>%
  mutate(Date = factor(Date), Combined.pot.id = as.character(Combined.pot.id)) %>%
  drop_na(pH_Level)
pH_data_filtered <- pH_data %>%
  filter(Application_Method == "Split" | Treatment_Type == "Control")
pH_data_filtered <- pH_data_filtered %>%
  mutate(log_pH_Level = log10(pH_Level))
m1_pH <- lmer(log_pH_Level ~ Dose * Date + (1|Combined.pot.id), data = pH_data_filtered)
plot(m1_pH)
qqPlot(resid(m1_pH))
Anova(m1_pH, test="F")
summary(m1_pH)
pH_one_time <- pH_data_filtered %>% filter(Application_Method == "One-time" | Treatment_Type == "Control")
pH_split <- pH_data_filtered %>% filter(Application_Method == "Split" | Treatment_Type == "Control")
ggplot(pH_one_time, aes(x = Date, y = log_pH_Level, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = mean(pH_one_time$log_pH_Level[pH_one_time$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "log10(pH) Levels Over Time (One-time Application, Including Control)",
       x = "Date", y = "log10(pH)", fill = "Dose", color = "Dose") +
  scale_x_discrete(limits = unique(pH_data_filtered$Date)) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(pH_split, aes(x = Date, y = log_pH_Level, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = mean(pH_split$log_pH_Level[pH_split$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "log10(pH) Levels Over Time (Split Application, Including Control)",
       x = "Date", y = "log10(pH)", fill = "Dose", color = "Dose") +
  scale_x_discrete(limits = unique(pH_data_filtered$Date)) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))

# Select data for one-time application including control and doses 60 and 120
pH_one_time_updated <- pH_data_filtered %>%
  filter(Application_Method == "One-time" | Treatment_Type == "Control" | Dose %in% c("0", "60", "120")) 

# Generate the box plot
ggplot(pH_one_time_updated, aes(x = Date, y = log_pH_Level, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +  # Boxplot without outlier points
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +  # Jittered points for visibility
  geom_hline(yintercept = mean(pH_one_time_updated$log_pH_Level[pH_one_time_updated$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black") +  # Dashed line for control (Dose 0)
  theme_minimal() +
  labs(title = "log10(pH) Levels Over Time (One-time Application, Including Control, 60 & 120 Doses)",
       x = "Date", y = "log10(pH)", fill = "Dose", color = "Dose") +
  scale_x_discrete(limits = unique(pH_data_filtered$Date)) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))


ec_columns <- grep("^EC_", names(Phalaris_exp2), value = TRUE)

ec_data <- Phalaris_exp2 %>%
  select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method, Treatment_Type, all_of(ec_columns)) %>%
  pivot_longer(cols = starts_with("EC_"), names_to = "Date", values_to = "EC_Level") %>%
  mutate(Date = factor(Date), Combined.pot.id = as.character(Combined.pot.id)) %>%
  drop_na(EC_Level)


ec_one_time <- ec_data %>%
  filter(Application_Method == "One-time" | Treatment_Type == "Control" | Dose %in% c("0", "60", "120"))
ec_one_time <- ec_one_time %>%
  mutate(log_EC_Level = log10(EC_Level))


m1_EC <- lmer(log_EC_Level ~ Dose * Date + (1|Combined.pot.id), data = ec_one_time)
plot(m1_EC)
qqPlot(resid(m1_EC))
Anova(m1_EC, test="F")
summary(m1_EC)

ggplot(ec_one_time, aes(x = Date, y = log_EC_Level, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = mean(ec_one_time$log_EC_Level[ec_one_time$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "log10(EC) Levels Over Time (One-time Application, Including Control, 60 & 120 Doses)",
       x = "Date", y = "log10(EC)", fill = "Dose", color = "Dose") +
  scale_x_discrete(limits = unique(ec_one_time$Date)) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))
ec_split <- ec_data %>%
  filter(Application_Method == "Split" | Treatment_Type == "Control")
ec_split <- ec_split %>%
  mutate(log_EC_Level = log10(EC_Level))


ggplot(ec_split, aes(x = Date, y = log_EC_Level, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = mean(ec_split$log_EC_Level[ec_split$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "log10(EC) Levels Over Time (Split Application, Including Control)",
       x = "Date", y = "log10(EC)", fill = "Dose", color = "Dose") +
  scale_x_discrete(limits = unique(ec_split$Date)) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))

chlorophyll_columns <- grep("CholorophyllContent", names(Phalaris_exp2), value = TRUE)

chlorophyll_data <- Phalaris_exp2 %>%
  select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method, Treatment_Type, all_of(chlorophyll_columns)) %>%
  pivot_longer(cols = starts_with("CholorophyllContent"), names_to = "Date", values_to = "Chlorophyll_Level") %>%
  mutate(Date = factor(Date), Combined.pot.id = as.character(Combined.pot.id)) %>%
  drop_na(Chlorophyll_Level)
chlorophyll_data <- chlorophyll_data %>%
  mutate(log_Chlorophyll_Level = log10(Chlorophyll_Level))
m1_chlorophyll <- lmer(log_Chlorophyll_Level ~ Dose * Date + (1|Combined.pot.id), data = chlorophyll_data)
plot(m1_chlorophyll)
qqPlot(resid(m1_chlorophyll))
Anova(m1_chlorophyll, test="F")
summary(m1_chlorophyll)

chlorophyll_one_time <- chlorophyll_data %>%
  filter(Application_Method == "One-time" | Treatment_Type == "Control")

chlorophyll_split <- chlorophyll_data %>%
  filter(Application_Method == "Split" | Treatment_Type == "Control")
ggplot(chlorophyll_one_time, aes(x = Date, y = log_Chlorophyll_Level, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = mean(chlorophyll_one_time$log_Chlorophyll_Level[chlorophyll_one_time$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "log10(Chlorophyll Content) Over Time (One-time Application, Including Control)",
       x = "Date", y = "log10(Chlorophyll Content)", fill = "Dose", color = "Dose") +
  scale_x_discrete(limits = unique(chlorophyll_data$Date)) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(chlorophyll_split, aes(x = Date, y = log_Chlorophyll_Level, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = mean(chlorophyll_split$log_Chlorophyll_Level[chlorophyll_split$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "log10(Chlorophyll Content) Over Time (Split Application, Including Control)",
       x = "Date", y = "log10(Chlorophyll Content)", fill = "Dose", color = "Dose") +
  scale_x_discrete(limits = unique(chlorophyll_data$Date)) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))
chlorophyll_data$Predicted <- predict(m1_chlorophyll)

ggplot(chlorophyll_data, aes(x = Date, y = Predicted, group = Dose, color = Dose)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(title = "Predicted log10(Chlorophyll Content) Over Time",
       x = "Date", y = "Predicted log10(Chlorophyll Content)", color = "Dose") +
  scale_x_discrete(limits = unique(chlorophyll_data$Date)) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))

biomass_columns <- c("Dry_Biomass..gm._35days", 
                     "Dry_Biomass.gm._66days", 
                     "Dry_Biomass..gm._95days", 
                     "Dry_shoot_..Biomass.g._125daysHD", 
                     "Dry_root_biomass.g._125daysHD")

biomass_data <- Phalaris_exp2 %>%
  select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method, Treatment_Type, all_of(biomass_columns)) %>%
  pivot_longer(cols = biomass_columns, names_to = "Date", values_to = "Dry_Biomass") %>%
  mutate(Date = factor(Date), Combined.pot.id = as.character(Combined.pot.id)) %>%
  drop_na(Dry_Biomass)

biomass_data <- biomass_data %>%
  mutate(log_Dry_Biomass = log10(Dry_Biomass))

m1_biomass <- lmer(log_Dry_Biomass ~ Dose * Date + (1|Combined.pot.id), data = biomass_data)
plot(m1_biomass)
qqPlot(resid(m1_biomass))
Anova(m1_biomass, test="F")
summary(m1_biomass)


biomass_one_time <- biomass_data %>%
  filter(Application_Method == "One-time" | Treatment_Type == "Control")

biomass_split <- biomass_data %>%
  filter(Application_Method == "Split" | Treatment_Type == "Control")

ggplot(biomass_one_time, aes(x = Date, y = log_Dry_Biomass, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = mean(biomass_one_time$log_Dry_Biomass[biomass_one_time$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "log10(Dry Biomass) Over Time (One-time Application, Including Control)",
       x = "Date", y = "log10(Dry Biomass)", fill = "Dose", color = "Dose") +
  scale_x_discrete(limits = unique(biomass_data$Date)) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(biomass_split, aes(x = Date, y = log_Dry_Biomass, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = mean(biomass_split$log_Dry_Biomass[biomass_split$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "log10(Dry Biomass) Over Time (Split Application, Including Control)",
       x = "Date", y = "log10(Dry Biomass)", fill = "Dose", color = "Dose") +
  scale_x_discrete(limits = unique(biomass_data$Date)) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(biomass_split, aes(x = Date, y = log_Dry_Biomass, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = mean(biomass_split$log_Dry_Biomass[biomass_split$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "log10(Dry Biomass) Over Time (Split Application, Including Control)",
       x = "Date", y = "log10(Dry Biomass)", fill = "Dose", color = "Dose") +
  scale_x_discrete(limits = unique(biomass_data$Date)) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))

biomass_data$Predicted <- predict(m1_biomass)

ggplot(biomass_data, aes(x = Date, y = Predicted, group = Dose, color = Dose)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(title = "Predicted log10(Dry Biomass) Over Time",
       x = "Date", y = "Predicted log10(Dry Biomass)", color = "Dose") +
  scale_x_discrete(limits = unique(biomass_data$Date)) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))
