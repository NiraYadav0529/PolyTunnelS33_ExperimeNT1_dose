library(lme4)      # For linear mixed-effects modeling
library(car)       # For ANOVA and diagnostic plots
library(ggplot2)   # For visualization
library(dplyr)     # For data manipulation
library(tidyr)     # For reshaping data
library(ggeffects) # For obtaining predicted values
#Convert categorical variables to factors
Lucerne_exp2 <- Lucerne_exp2 %>%
  mutate(
    Dose = factor(Dose...N.kg.ha., levels = c("0", "10", "20", "30", "40", "50", "60")),
    Fertilizer_Type = factor(Fertilizer.type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split"))
  )
##Extract pH-related columns
pH_columns <- grep("^pH_", names(Lucerne_exp2), value = TRUE)

pH_data <- Lucerne_exp2 %>%
  select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method, all_of(pH_columns)) %>%
  pivot_longer(cols = starts_with("pH_"), names_to = "Date", values_to = "pH_Level") %>%
  mutate(
    Date = factor(Date),
    Combined.pot.id = as.character(Combined.pot.id)
  ) %>%
  drop_na(pH_Level)  # Remove missing values
##Ensure correct time order
pH_levels_order <- c("pH_BF", "pH_2.AF_1st_Split.", "pH_3_AF_2nd_Split", 
                     "pH_4_AF_2nd_Split", "pH_5_AF_3nd_Split", "pH_6_AF_3nd_Split", 
                     "pH_7_AF_3nd_Split", "pH_8_atHarvestDay")

pH_data$Date <- factor(pH_data$Date, levels = pH_levels_order, ordered = TRUE)

##Step 4: Apply log10 Transformation to pH Levels
pH_data <- pH_data %>%
  mutate(log_pH_Level = log10(pH_Level))
##Step 5: Fit Linear Mixed Effects Model (LMM)
m1_pH <- lmer(log_pH_Level ~ Dose * Date + (1|Combined.pot.id), data = pH_data)

# Model Diagnostics
plot(m1_pH)                
qqPlot(resid(m1_pH))       

# ANOVA for significance testing
Anova(m1_pH, test="F")

# Summary
summary(m1_pH)


##Step 6: Split Data by Application Method
pH_one_time <- pH_data %>% filter(Application_Method == "One-time")
pH_split <- pH_data %>% filter(Application_Method == "Split")

##Step 7: Boxplots of log10(pH) Levels
##(A) One-time Application

ggplot(pH_one_time, aes(x = Date, y = log_pH_Level, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +
  theme_minimal() +
  labs(
    title = "log10(pH) Levels Over Time (One-time Application)",
    x = "Date",
    y = "log10(pH)",
    fill = "Dose",
    color = "Dose"
  ) +
  scale_x_discrete(limits = pH_levels_order) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  geom_hline(yintercept = mean(pH_one_time$log_pH_Level[pH_one_time$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black")

##(B) Split Application
ggplot(pH_split, aes(x = Date, y = log_pH_Level, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +
  theme_minimal() +
  labs(
    title = "log10(pH) Levels Over Time (Split Application)",
    x = "Date",
    y = "log10(pH)",
    fill = "Dose",
    color = "Dose"
  ) +
  scale_x_discrete(limits = pH_levels_order) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  geom_hline(yintercept = mean(pH_split$log_pH_Level[pH_split$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black")
##Step 8: Fit Model for Predicted Values
m1_pH_control <- lmer(log_pH_Level ~ Dose * Date * Application_Method + (1|Combined.pot.id), data = pH_data)

# Model Diagnostics
plot(m1_pH_control)
qqPlot(resid(m1_pH_control))

# ANOVA
Anova(m1_pH_control, test="F")

# Summary
summary(m1_pH_control)
##Step 9: Generate Predicted Values from Model
predicted_pH <- ggpredict(m1_pH_control, terms = c("Date", "Dose", "Application_Method"))

# Convert to data frame
predicted_pH_df <- as.data.frame(predicted_pH)


##Step 10: Plot Predicted log10(pH) Over Time
ggplot(predicted_pH_df, aes(x = x, y = predicted, group = group, color = group)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  facet_wrap(~facet, ncol = 1) +
  theme_minimal() +
  labs(
    title = "Predicted log10(pH) Levels Over Time",
    x = "Date",
    y = "Predicted log10(pH)",
    color = "Dose"
  ) +
  scale_x_discrete(limits = pH_levels_order) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#check column name 
colnames(Lucerne_exp2)

## Extract and Reshape EC-related Columns
ec_columns <- grep("^EC_", names(Lucerne_exp2), value = TRUE)

ec_data <- Lucerne_exp2 %>%
  select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method, all_of(ec_columns)) %>%
  pivot_longer(cols = starts_with("EC_"), names_to = "Date", values_to = "EC_Level") %>%
  mutate(
    Date = factor(Date),
    Combined.pot.id = as.character(Combined.pot.id)
  ) %>%
  drop_na(EC_Level)
##Ensure Correct Ordering of Time Points
ec_levels_order <- c("EC_BF", "EC_1.AF_1st_Split.", "EC_2.AF_1st_Split.", 
                     "EC_3_AF_2nd_Split", "EC_4_AF_2nd_Split", "EC_5_AF_3nd_Split", 
                     "EC_6_AF_3nd_Split", "EC_7_AF_3nd_Split", "EC._atHarvestDay")

ec_data$Date <- factor(ec_data$Date, levels = ec_levels_order, ordered = TRUE)
##Step 3: Apply log10 Transformation to EC
ec_data <- ec_data %>%
  mutate(log_EC_Level = log10(EC_Level))
##Step 4: Fit Linear Mixed-Effects Model for EC
m1_EC <- lmer(log_EC_Level ~ Dose * Date + (1|Combined.pot.id), data = ec_data)

# Model Diagnostics
plot(m1_EC)                
qqPlot(resid(m1_EC))       

# ANOVA for fixed effects significance
Anova(m1_EC, test="F")

# Summary of the model
summary(m1_EC)
##Step 5: Separate One-Time and Split Application for EC
ec_one_time <- ec_data %>% filter(Application_Method == "One-time")
ec_split <- ec_data %>% filter(Application_Method == "Split")
##Step 6: Boxplots for EC (A) One-time Application
ggplot(ec_one_time, aes(x = Date, y = log_EC_Level, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = mean(ec_one_time$log_EC_Level[ec_one_time$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "log10(EC) Levels Over Time (One-time Application)",
       x = "Date", y = "log10(EC)", fill = "Dose", color = "Dose") +
  scale_x_discrete(limits = ec_levels_order) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))
##(B) Split Application
ggplot(ec_split, aes(x = Date, y = log_EC_Level, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = mean(ec_split$log_EC_Level[ec_split$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "log10(EC) Levels Over Time (Split Application)",
       x = "Date", y = "log10(EC)", fill = "Dose", color = "Dose") +
  scale_x_discrete(limits = ec_levels_order) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))
##Step 7: Fit Model for Predicted Values

m1_EC_control <- lmer(log_EC_Level ~ Dose * Date * Application_Method + (1|Combined.pot.id), data = ec_data)

# Model Diagnostics
plot(m1_EC_control)
qqPlot(resid(m1_EC_control))

# ANOVA
Anova(m1_EC_control, test="F")

# Summary
summary(m1_EC_control)
predicted_EC <- ggpredict(m1_EC_control, terms = c("Date", "Dose", "Application_Method"))

# Convert to data frame
predicted_EC_df <- as.data.frame(predicted_EC)


##Step 9: Plot Predicted log10(EC) Over Time
ggplot(predicted_EC_df, aes(x = x, y = predicted, group = group, color = group)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  facet_wrap(~facet, ncol = 1) +
  theme_minimal() +
  labs(
    title = "Predicted log10(EC) Levels Over Time",
    x = "Date",
    y = "Predicted log10(EC)",
    color = "Dose"
  ) +
  scale_x_discrete(limits = ec_levels_order) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



##For cholorphyll content data of Lucerne 
#Data Preprocessing for Chlorophyll Content
##Extract and Reshape Chlorophyll-related Columns

chlorophyll_columns <- grep("Cholorphyll_content", names(Lucerne_exp2), value = TRUE)

chlorophyll_data <- Lucerne_exp2 %>%
  select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method, all_of(chlorophyll_columns)) %>%
  pivot_longer(cols = starts_with("Cholorphyll_content"), names_to = "Date", values_to = "Chlorophyll_Level") %>%
  mutate(Date = factor(Date), Combined.pot.id = as.character(Combined.pot.id)) %>%
  drop_na(Chlorophyll_Level)
##Ensure Correct Ordering of Time Points
chlorophyll_levels_order <- c("Cholorphyll_content.Before2ndDose.", 
                              "Cholorphyll_content.Before3rdDose.", 
                              "Cholorphyll_content.HarvestDay.")

chlorophyll_data$Date <- factor(chlorophyll_data$Date, levels = chlorophyll_levels_order, ordered = TRUE)
##Step 3: Apply log10 Transformation to Chlorophyll Content
chlorophyll_data <- chlorophyll_data %>%
  mutate(log_Chlorophyll_Level = log10(Chlorophyll_Level))
##Step 4: Fit Linear Mixed-Effects Model for Chlorophyll Content
m1_chlorophyll <- lmer(log_Chlorophyll_Level ~ Dose * Date + (1|Combined.pot.id), data = chlorophyll_data)

# Model Diagnostics
plot(m1_chlorophyll)                
qqPlot(resid(m1_chlorophyll))       

# ANOVA for fixed effects significance
Anova(m1_chlorophyll, test="F")

# Summary of the model
summary(m1_chlorophyll)


##Step 5: Separate One-Time and Split Application for Chlorophyll Content
chlorophyll_one_time <- chlorophyll_data %>% filter(Application_Method == "One-time")
chlorophyll_split <- chlorophyll_data %>% filter(Application_Method == "Split")

##Step 6: Boxplots for Chlorophyll Content
#) One-time Application
ggplot(chlorophyll_one_time, aes(x = Date, y = log_Chlorophyll_Level, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = mean(chlorophyll_one_time$log_Chlorophyll_Level[chlorophyll_one_time$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "log10(Chlorophyll Content) Over Time (One-time Application)",
       x = "Date", y = "log10(Chlorophyll Content)", fill = "Dose", color = "Dose") +
  scale_x_discrete(limits = chlorophyll_levels_order) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))
#(B) Split Application
ggplot(chlorophyll_split, aes(x = Date, y = log_Chlorophyll_Level, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = mean(chlorophyll_split$log_Chlorophyll_Level[chlorophyll_split$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "log10(Chlorophyll Content) Over Time (Split Application)",
       x = "Date", y = "log10(Chlorophyll Content)", fill = "Dose", color = "Dose") +
  scale_x_discrete(limits = chlorophyll_levels_order) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))


##Predicted model 
m1_chlorophyll_control <- lmer(log_Chlorophyll_Level ~ Dose * Date * Application_Method + (1|Combined.pot.id), data = chlorophyll_data)

# Model Diagnostics
plot(m1_chlorophyll_control)
qqPlot(resid(m1_chlorophyll_control))

# ANOVA
Anova(m1_chlorophyll_control, test="F")

# Summary
summary(m1_chlorophyll_control)
predicted_chlorophyll <- ggpredict(m1_chlorophyll_control, terms = c("Date", "Dose", "Application_Method"))

# Convert to data frame
predicted_chlorophyll_df <- as.data.frame(predicted_chlorophyll)
ggplot(predicted_chlorophyll_df, aes(x = x, y = predicted, group = group, color = group)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  facet_wrap(~facet, ncol = 1) +
  theme_minimal() +
  labs(
    title = "Predicted log10(Chlorophyll Content) Over Time",
    x = "Date",
    y = "Predicted log10(Chlorophyll Content)",
    color = "Dose"
  ) +
  scale_x_discrete(limits = chlorophyll_levels_order) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


##Step 2: Data Preprocessing for Dry Biomass
#Extract and Reshape Dry Biomass-related Columns
biomass_columns <- grep("Dry_Biomass", names(Lucerne_exp2), value = TRUE)

biomass_data <- Lucerne_exp2 %>%
  select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method, all_of(biomass_columns)) %>%
  pivot_longer(cols = starts_with("Dry_Biomass"), names_to = "Date", values_to = "Dry_Biomass") %>%
  mutate(Date = factor(Date), Combined.pot.id = as.character(Combined.pot.id)) %>%
  drop_na(Dry_Biomass)
##Ensure Correct Ordering of Time Points
biomass_levels_order <- c("Dry_Biomass_1", "Dry_Biomass._2", "Dry_Biomass_3", 
                          "Dry_Biomass_shoot_4", "Dry_Biomass_root_118days")

biomass_data$Date <- factor(biomass_data$Date, levels = biomass_levels_order, ordered = TRUE)

##Step 3: Apply log10 Transformation to Dry Biomass
biomass_data <- biomass_data %>%
  mutate(log_Dry_Biomass = log10(Dry_Biomass))
##Step 4: Fit Linear Mixed-Effects Model for Dry Biomass

m1_biomass <- lmer(log_Dry_Biomass ~ Dose * Date + (1|Combined.pot.id), data = biomass_data)

# Model Diagnostics
plot(m1_biomass)                
qqPlot(resid(m1_biomass))       

# ANOVA for fixed effects significance
Anova(m1_biomass, test="F")

# Summary of the model
summary(m1_biomass)
##Step 5: Separate One-Time and Split Application for Dry Biomass
biomass_one_time <- biomass_data %>% filter(Application_Method == "One-time")
biomass_split <- biomass_data %>% filter(Application_Method == "Split")


#Step 6: Boxplots for Dry Biomass (A) One-time Application
ggplot(biomass_one_time, aes(x = Date, y = log_Dry_Biomass, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = mean(biomass_one_time$log_Dry_Biomass[biomass_one_time$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "log10(Dry Biomass) Over Time (One-time Application)",
       x = "Date", y = "log10(Dry Biomass)", fill = "Dose", color = "Dose") +
  scale_x_discrete(limits = biomass_levels_order) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))
##(B) Split Application
ggplot(biomass_split, aes(x = Date, y = log_Dry_Biomass, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = Dose), width = 0.2, alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = mean(biomass_split$log_Dry_Biomass[biomass_split$Dose == "0"], na.rm = TRUE),
             linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "log10(Dry Biomass) Over Time (Split Application)",
       x = "Date", y = "log10(Dry Biomass)", fill = "Dose", color = "Dose") +
  scale_x_discrete(limits = biomass_levels_order) +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))


##Step 7: Fit Model for Predicted Values
m1_biomass_control <- lmer(log_Dry_Biomass ~ Dose * Date * Application_Method + (1|Combined.pot.id), data = biomass_data)

# Model Diagnostics
plot(m1_biomass_control)
qqPlot(resid(m1_biomass_control))

# ANOVA
Anova(m1_biomass_control, test="F")

# Summary
summary(m1_biomass_control)
predicted_biomass <- ggpredict(m1_biomass_control, terms = c("Date", "Dose", "Application_Method"))

# Convert to data frame
predicted_biomass_df <- as.data.frame(predicted_biomass)
ggplot(predicted_biomass_df, aes(x = x, y = predicted, group = group, color = group)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  facet_wrap(~facet, ncol = 1) +
  theme_minimal() +
  labs(
    title = "Predicted log10(Dry Biomass) Over Time",
    x = "Date",
    y = "Predicted log10(Dry Biomass)",
    color = "Dose"
  ) +
  scale_x_discrete(limits = biomass_levels_order) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

