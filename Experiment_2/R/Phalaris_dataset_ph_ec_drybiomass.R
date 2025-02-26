
setwd("C:/Users/90958427/OneDrive - Western Sydney University/PolyTunnelS33_ExperimeNT1_dose/Experiment_2/Modified_Data_File")
list.files()
Phalaris_exp2 <- read.csv("Phalaris_FinalData_analysis.csv", check.names = TRUE)
colnames(Phalaris_exp2)

##loading required libraries 
library(lme4)      # Linear mixed-effects modeling
library(car)       # ANOVA and model diagnostics
library(ggplot2)   # Visualization
library(dplyr)     # Data manipulation
library(tidyr)     # Data reshaping
library(ggeffects) # Predicted values extraction

##converting categorical variables to factors 
Phalaris_exp2 <- Phalaris_exp2 %>%
  mutate(
    Dose = factor(Dose_.N.kg.ha., levels = c("0", "20", "40", "60", "80", "100", "120")),
    Fertilizer_Type = factor(Fertilizer_type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split")),
    Treatment_Type = factor(Treatment_Type, levels = c("Control", "Other"))
  )

## Step 3: Extract and Reshape pH-related Columns
pH_columns <- grep("^pH_[0-9]+$|^ph_10$", names(Phalaris_exp2), value = TRUE)

pH_data <- Phalaris_exp2 %>%
  dplyr::select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method, Treatment_Type, all_of(pH_columns)) %>%
  pivot_longer(cols = all_of(pH_columns), names_to = "Date", values_to = "pH_Level") %>%
  mutate(Date = factor(Date), Combined.pot.id = as.character(Combined.pot.id)) %>%
  drop_na(pH_Level)

### Step 4: Ensure Correct Ordering of Time Points
pH_levels_order <- c(paste0("pH_", 1:9), "ph_10")

pH_data$Date <- factor(pH_data$Date, levels = pH_levels_order, ordered = TRUE)


## Step 5: Fit Linear Mixed-Effects Model for pH
m1_pH <- lmer(pH_Level ~ Dose * Date * Application_Method + (1|Combined.pot.id), data = pH_data)
plot(m1_pH)
qqPlot(resid(m1_pH))
Anova(m1_pH, test="F")
summary(m1_pH)

## Step 6: Separate One-Time and Split Application for pH
pH_one_time <- pH_data %>% filter(Application_Method == "One-time" | Treatment_Type == "Control")
pH_split <- pH_data %>% filter(Application_Method == "Split" | Treatment_Type == "Control")




## Step 7: Boxplots for pH (One-time Application including Control)
ggplot(pH_one_time, aes(x = Date, y = pH_Level, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = mean(pH_one_time$pH_Level[pH_one_time$Dose == "0" & pH_one_time$Treatment_Type == "Control"], na.rm = TRUE),
             linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "pH Levels Over Time (One-time Application, Including Control)",
       x = "Date", y = "pH", fill = "Dose", color = "Dose") +
  scale_x_discrete(limits = pH_levels_order) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))

## Box Plot for pH (Split Application including Control)
ggplot(pH_split, aes(x = Date, y = pH_Level, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = mean(pH_split$pH_Level[pH_split$Dose == "0" & pH_split$Treatment_Type == "Control"], na.rm = TRUE),
             linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "pH Levels Over Time (Split Application, Including Control)",
       x = "Date", y = "pH", fill = "Dose", color = "Dose") +
  scale_x_discrete(limits = pH_levels_order) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))

## Updated data filtering for One-time and Split applications with controls
pH_data_filtered <- pH_data %>%
  filter(Application_Method == "Split" | Treatment_Type == "Control")
m1_pH <- lmer(pH_Level ~ Dose * Date * Application_Method + (1|Combined.pot.id), data = pH_data_filtered)
plot(m1_pH)
qqPlot(resid(m1_pH))
Anova(m1_pH, test="F")
summary(m1_pH)

pH_one_time_updated <- pH_data_filtered %>%
  filter(Application_Method == "One-time" | Treatment_Type == "Control" | Dose %in% c("0", "60", "120")) 

## Generate the updated box plot with control included
ggplot(pH_one_time_updated, aes(x = Date, y = pH_Level, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +  # Boxplot without outlier points
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +  # Jittered points for visibility
  geom_hline(yintercept = mean(pH_one_time_updated$pH_Level[pH_one_time_updated$Dose == "0" & pH_one_time_updated$Treatment_Type == "Control"], na.rm = TRUE),
             linetype = "dashed", color = "black") +  # Dashed line for control (Dose 0)
  theme_minimal() +
  labs(title = "pH Levels Over Time (One-time Application, Including Control, 60 & 120 Doses)",
       x = "Date", y = "pH", fill = "Dose", color = "Dose") +
  scale_x_discrete(limits = unique(pH_data_filtered$Date)) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))




##Ec Data for Phalaris

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



unique(pH_one_time_updated$Fertilizer_Type)
scale_color_manual(values = c("None" = "black", "MF" = "darkgreen", "UF" = "blue"))
pH_one_time_updated$Fertilizer_Type <- factor(
  pH_one_time_updated$Fertilizer_Type, 
  levels = c("None", "UF", "MF")
)
pH_one_time_updated %>% filter(Fertilizer_Type == "MF") %>% summary()



ggplot(pH_one_time_updated, aes(x = Date, y = pH_Level, fill = Dose, color = Fertilizer_Type)) +
  geom_boxplot(alpha = 0.6, position = position_dodge(width = 0.8), outlier.shape = NA) +
  geom_jitter(aes(shape = Fertilizer_Type), alpha = 0.7, size = 1.5, position = position_dodge(width = 0.8)) + 
  theme_minimal() +
  labs(
    title = "pH Levels Over Time (One-time Application, Including Control, 60 & 120 Doses)",
    x = "Date",
    y = "pH",
    fill = "Dose (kg N/ha)",
    color = "Fertilizer Type",
    shape = "Fertilizer Type"
  ) +
  scale_x_discrete(limits = pH_levels_order) +
  scale_fill_manual(values = c("0" = "red", "60" = "green", "120" = "blue")) + # Dose colors
  scale_color_manual(values = c("None" = "black", "UF" = "blue", "MF" = "darkgreen")) + # Adjust colors for Fertilizer Type
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  ) +
  geom_hline(yintercept = mean(pH_one_time$pH_Level[pH_one_time$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black")




# One-time Application Plot (Including Control) for pH with Fertilizer Type & Dose
ggplot(pH_one_time_updated, aes(x = Date, y = pH_Level, fill = Dose, color = Fertilizer_Type)) +
  geom_boxplot(alpha = 0.6, position = position_dodge(width = 0.8), outlier.shape = NA) +
  geom_jitter(aes(shape = Fertilizer_Type), alpha = 0.7, size = 1.5, position = position_dodge(width = 0.8)) + 
  theme_minimal() +
  labs(
    title = "pH Levels Over Time (One-time Application, Including Control, 60 & 120 Doses)",
    x = "Date",
    y = "pH",
    fill = "Dose (kg N/ha)",
    color = "Fertilizer Type",
    shape = "Fertilizer Type"
  ) +
  scale_x_discrete(limits = pH_levels_order) +
  scale_fill_manual(values = c("0" = "red", "60" = "green", "120" = "blue")) + # Dose colors
  scale_color_manual(values = c("None" = "black", "MF" = "darkgreen", "UF" = "blue")) + # Fertilizer colors
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  geom_hline(yintercept = mean(pH_one_time_updated$pH_Level[pH_one_time_updated$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black")


#ph changes over time 

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Ensure Dose is treated as a numeric variable and convert categorical variables
Phalaris_exp2 <- Phalaris_exp2 %>%
  mutate(
    Dose = as.numeric(as.character(Dose_.N.kg.ha.)),  # Convert Dose to numeric
    Fertilizer_Type = as.factor(Fertilizer_type),      # Convert Fertilizer type to factor
    Application_Method = as.factor(Application_method) # Convert Application method to factor
  )

# Check if Dose conversion is successful
summary(Phalaris_exp2$Dose)

# Create a summary table for Mean pH Change and Standard Error (SE)
summary_pH_Phalaris <- Phalaris_exp2 %>%
  group_by(Dose, Fertilizer_Type, Application_Method) %>% 
  summarise(
    Mean_pH = mean(PH.Chang, na.rm = TRUE),  # Calculate mean pH change
    SD_pH = sd(PH.Chang, na.rm = TRUE),      # Standard deviation
    SE = SD_pH / sqrt(n())                   # Standard error
  ) %>%
  ungroup()

# Check if summary_pH_Phalaris is created correctly
head(summary_pH_Phalaris)

# Visualization of pH unit changes with N-added rate, Fertilizer Type, and Application Method
ggplot(summary_pH_Phalaris, aes(x = Mean_pH, y = as.factor(Dose), color = Fertilizer_Type, shape = Application_Method)) +
  geom_point(size = 4, position = position_dodge(width = 0.3)) +  # Add mean pH change points
  geom_errorbarh(aes(xmin = Mean_pH - SE, xmax = Mean_pH + SE), height = 0.2, position = position_dodge(width = 0.3)) +  # Error bars
  theme_minimal() +
  labs(
    x = "pH Unit Change",
    y = "N-added rates (kg N/ha)",
    title = "Effect of Nitrogen Addition, Fertilizer Type, and Application Method on Soil pH (Phalaris)",
    color = "Fertilizer Type",
    shape = "Application Method"
  ) +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "right"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")  # Reference line at 0






























# Install missing packages if necessary
required_packages <- c("lme4", "car", "ggplot2", "dplyr", "tidyr", "ggeffects")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(lme4)      # Linear mixed-effects modeling
library(car)       # ANOVA and model diagnostics
library(ggplot2)   # Visualization
library(dplyr)     # Data manipulation
library(tidyr)     # Data reshaping
library(ggeffects) # Predicted values extraction

# Convert categorical variables to factors
pH_data <- Phalaris_exp2 %>%
  dplyr::select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method, Treatment_Type, all_of(pH_columns)) %>%
  pivot_longer(cols = all_of(pH_columns), names_to = "Date", values_to = "pH_Level") %>%
  mutate(Date = factor(Date), Combined.pot.id = as.character(Combined.pot.id)) %>%
  drop_na(pH_Level)
conflicts()

print(pH_columns)

pH_columns <- names(Phalaris_exp2)[grepl("^pH_", names(Phalaris_exp2))]

pH_data <- Phalaris_exp2 %>%
  dplyr::select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method, Treatment_Type, any_of(pH_columns)) %>%
  pivot_longer(cols = any_of(pH_columns), names_to = "Date", values_to = "pH_Level") %>%
  mutate(Date = factor(Date), Combined.pot.id = as.character(Combined.pot.id)) %>%
  drop_na(pH_Level)
class(Phalaris_exp2)
str(Phalaris_exp2)

Phalaris_exp2 <- as.data.frame(Phalaris_exp2)

head(Phalaris_exp2)
dim(Phalaris_exp2)
# Ensure dplyr is used
pH_data <- Phalaris_exp2 %>%
  dplyr::select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method, Treatment_Type, any_of(pH_columns)) %>%
  pivot_longer(cols = any_of(pH_columns), names_to = "Date", values_to = "pH_Level") %>%
  mutate(Date = factor(Date), Combined.pot.id = as.character(Combined.pot.id)) %>%
  drop_na(pH_Level)


# Fit the model
m1_pH <- lmer(pH_Level ~ Dose * Date + (1|Combined.pot.id), data = pH_one_time)

# Model diagnostics
plot(m1_pH)
qqPlot(resid(m1_pH))
Anova(m1_pH, test="F")
summary(m1_pH)

ggplot(pH_one_time, aes(x = Date, y = pH_Level, fill = Dose, color = Fertilizer_Type)) +
  geom_boxplot(alpha = 0.6, position = position_dodge(width = 0.8), outlier.shape = NA) +
  geom_jitter(aes(shape = Fertilizer_Type), alpha = 0.7, size = 1.5, position = position_dodge(width = 0.8)) + 
  theme_minimal() +
  labs(
    title = "pH Levels Over Time (One-time Application, Including Control, 60 & 120 Doses)",
    x = "Date",
    y = "pH",
    fill = "Dose (kg N/ha)",
    color = "Fertilizer Type",
    shape = "Fertilizer Type"
  ) +
  scale_x_discrete(limits = pH_levels_order) +
  scale_fill_manual(values = c("0" = "red", "60" = "green", "120" = "blue")) +
  scale_color_manual(values = c("None" = "black", "MF" = "darkgreen", "UF" = "blue")) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = mean(pH_one_time$pH_Level[pH_one_time$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black")

# Identify EC columns
ec_columns <- grep("^EC_", names(Phalaris_exp2), value = TRUE)

# Reshape EC data into long format
ec_data <- Phalaris_exp2 %>%
  dplyr::select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method, Treatment_Type, all_of(ec_columns)) %>%
  pivot_longer(cols = all_of(ec_columns), names_to = "Date", values_to = "EC_Level") %>%
  mutate(Date = factor(Date), Combined.pot.id = as.character(Combined.pot.id)) %>%
  drop_na(EC_Level)

# Apply log transformation
ec_data <- ec_data %>% mutate(log_EC_Level = log10(EC_Level))

# Filter One-time application & Control
ec_one_time <- ec_data %>% filter(Application_Method == "One-time" | Treatment_Type == "Control")


# One-time Application Plot (Including Control) for pH with Fertilizer Type & Dose
ggplot(pH_one_time, aes(x = Date, y = pH_Level, fill = Dose, color = Fertilizer_Type)) +
  geom_boxplot(alpha = 0.6, position = position_dodge(width = 0.8), outlier.shape = NA) +
  geom_jitter(aes(shape = Fertilizer_Type), alpha = 0.7, size = 1.5, position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(
    title = "pH Levels Over Time (One-time Application, Including Control)",
    x = "Date", y = "pH",
    fill = "Dose (kg N/ha)", color = "Fertilizer Type", shape = "Fertilizer Type"
  ) +
  scale_x_discrete(limits = pH_levels_order) +
  scale_fill_manual(values = c("0" = "red", "60" = "green", "120" = "blue")) +  # Dose colors
  scale_color_manual(values = c("None" = "black", "MF" = "darkgreen", "UF" = "blue")) +  # Fertilizer colors
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = mean(pH_one_time$pH_Level[pH_one_time$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black")





# Ensure all doses (0, 60, 120) are included
ec_one_time$Dose <- factor(ec_one_time$Dose, levels = c("0", "60", "120"))

ggplot(ec_one_time, aes(x = Date, y = log_EC_Level, fill = Dose, color = Fertilizer_Type)) +
  geom_boxplot(alpha = 0.6, position = position_dodge(width = 0.8), outlier.shape = NA) +
  geom_jitter(aes(shape = Fertilizer_Type), alpha = 0.7, size = 1.5, position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(
    title = "log10(EC) Levels Over Time (One-time Application, Including Control)",
    x = "Date", y = "log10(EC)",
    fill = "Dose (kg N/ha)", color = "Fertilizer Type", shape = "Fertilizer Type"
  ) +
  scale_x_discrete(limits = unique(ec_one_time$Date)) +
  scale_fill_manual(values = c("0" = "red", "60" = "green", "120" = "blue"), 
                    breaks = c("0", "60", "120")) +  # Force all doses in the legend
  scale_color_manual(values = c("None" = "black", "MF" = "darkgreen", "UF" = "blue")) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = mean(ec_one_time$log_EC_Level[ec_one_time$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black")



chlorophyll_one_time$Dose <- factor(chlorophyll_one_time$Dose, levels = c("0", "60", "120"))
biomass_one_time$Dose <- factor(biomass_one_time$Dose, levels = c("0", "60", "120"))
levels(chlorophyll_one_time$Dose)
levels(biomass_one_time$Dose)
scale_fill_manual(values = c("0" = "red", "60" = "green", "120" = "blue"), drop = FALSE)
chlorophyll_one_time %>% filter(Dose == "120") %>% summary()
biomass_one_time %>% filter(Dose == "120") %>% summary()

ggplot(chlorophyll_one_time, aes(x = Date, y = log_Chlorophyll_Level, fill = Dose, color = Fertilizer_Type)) +
  geom_boxplot(alpha = 0.6, position = position_dodge(width = 0.8), outlier.shape = NA) +
  geom_jitter(aes(shape = Fertilizer_Type), alpha = 0.7, size = 1.5, position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(
    title = "log10(Chlorophyll Content) Over Time (One-time Application, Including Control)",
    x = "Date", y = "log10(Chlorophyll Content)",
    fill = "Dose (kg N/ha)", color = "Fertilizer Type", shape = "Fertilizer Type"
  ) +
  scale_x_discrete(limits = unique(chlorophyll_one_time$Date)) +
  scale_fill_manual(values = c("0" = "red", "60" = "green", "120" = "blue"), drop = FALSE) + # Fix legend issue
  scale_color_manual(values = c("None" = "black", "MF" = "darkgreen", "UF" = "blue")) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = mean(chlorophyll_one_time$log_Chlorophyll_Level[chlorophyll_one_time$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black")




ggplot(biomass_one_time, aes(x = Date, y = log_Dry_Biomass, fill = Dose, color = Fertilizer_Type)) +
  geom_boxplot(alpha = 0.6, position = position_dodge(width = 0.8), outlier.shape = NA) +
  geom_jitter(aes(shape = Fertilizer_Type), alpha = 0.7, size = 1.5, position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(
    title = "log10(Dry Biomass) Over Time (One-time Application, Including Control)",
    x = "Date", y = "log10(Dry Biomass)",
    fill = "Dose (kg N/ha)", color = "Fertilizer Type", shape = "Fertilizer Type"
  ) +
  scale_x_discrete(limits = unique(biomass_one_time$Date)) +
  scale_fill_manual(values = c("0" = "red", "60" = "green", "120" = "blue"), drop = FALSE) + # Ensures "120" is shown in the legend
  scale_color_manual(values = c("None" = "black", "MF" = "darkgreen", "UF" = "blue")) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = mean(biomass_one_time$log_Dry_Biomass[biomass_one_time$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black")



