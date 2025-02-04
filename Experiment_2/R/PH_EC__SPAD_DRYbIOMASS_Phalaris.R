# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(car)
library(emmeans)

# Load datasets
Lucerne_exp2 <- read.csv("Lucerne_FinalData_analysis.csv")
Phalaris_exp2 <- read.csv("Phalaris_FinalData_analysis.csv")
colnames(Phalaris_exp2)
head(Phalaris_exp2)
# Convert variables to factors
Lucerne_exp2 <- Lucerne_exp2 %>%
  mutate(
    Dose = factor(Dose...N.kg.ha., levels = c("0", "10", "20", "30", "40", "50", "60")),
    Fertilizer_Type = factor(Fertilizer.type, levels = c("None", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split"))
  )

# Select chlorophyll content columns along with identifiers
chlorophyll_data <- Lucerne_exp2 %>%
  select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method,
         Cholorphyll_content.Before2ndDose., 
         Cholorphyll_content.Before3rdDose., 
         Cholorphyll_content.HarvestDay.)

# Reshape data to long format
chlorophyll_long <- chlorophyll_data %>%
  pivot_longer(
    cols = starts_with("Cholorphyll_content"),
    names_to = "Time_Point",
    values_to = "Chlorophyll_Content"
  ) %>%
  mutate(
    Time_Point = factor(Time_Point),
    Combined.pot.id = as.character(Combined.pot.id)
  ) %>%
  drop_na(Chlorophyll_Content)  # Remove rows with missing values

 # Separate data by Application Method
chlorophyll_split <- chlorophyll_long %>% filter(Application_Method == "Split")
chlorophyll_one_time <- chlorophyll_long %>% filter(Application_Method == "One-time")

# --- Fit a Linear Mixed Effects Model ---
m1_chlorophyll <- lmer(Chlorophyll_Content ~ Dose * Time_Point + (1 | Combined.pot.id), 
                       data = chlorophyll_long)

# Model diagnostics
plot(m1_chlorophyll)
qqPlot(resid(m1_chlorophyll))  # QQ plot of residuals

# ANOVA to test significance of fixed effects
Anova(m1_chlorophyll, test = "F")

# Summary of the model
summary(m1_chlorophyll)

# --- Prediction Plot for Chlorophyll Content Using ggeffects ---
library(ggeffects)
predict_plot <- predict_response(m1_chlorophyll, c("Time_Point", "Dose")) %>% plot()
print(predict_plot)

# --- Visualization: Boxplots for Each Application Method ---
# For Split Application
ggplot(chlorophyll_split, aes(x = Time_Point, y = Chlorophyll_Content, color = Dose)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Chlorophyll Content Across Time Points (Split Application Method)",
    y = "Chlorophyll Content",
    x = "Time Point",
    color = "Dose (N kg/ha)"
  ) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# For One-time Application with Fertilizer_Type facets
ggplot(chlorophyll_one_time, aes(x = Time_Point, y = Chlorophyll_Content, color = Dose)) +
  geom_boxplot() +
  facet_wrap(~ Fertilizer_Type) +  # Facet by Fertilizer Type
  theme_minimal() +
  labs(
    title = "Chlorophyll Content Across Time Points (One-time Application Method)",
    y = "Chlorophyll Content",
    x = "Time Point",
    color = "Dose (N kg/ha)"
  ) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

colnames(Phalaris_exp2)
head(Phalaris_exp2)
# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(car)
library(emmeans)

# Convert variables to factors
Phalaris_exp2 <- Phalaris_exp2 %>%
  mutate(
    Dose = factor(Dose_.N.kg.ha., levels = c("0", "20", "40", "60", "80", "100", "120")),
    Fertilizer_Type = factor(Fertilizer_type, levels = c("Control", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split"))
  )

# Select chlorophyll content columns along with identifiers
chlorophyll_data <- Phalaris_exp2 %>%
  select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method,
         CholorophyllContent_66days, 
         CholorophyllContent_95days, 
         CholorophyllContent_125daysHD)

# Reshape data to long format
chlorophyll_long <- chlorophyll_data %>%
  pivot_longer(
    cols = starts_with("CholorophyllContent"),
    names_to = "Time_Point",
    values_to = "Chlorophyll_Content"
  ) %>%
  mutate(
    Time_Point = factor(Time_Point),
    Combined.pot.id = as.character(Combined.pot.id)
  ) %>%
  drop_na(Chlorophyll_Content)  # Remove rows with missing values

# Separate data by Application Method
chlorophyll_split <- chlorophyll_long %>% filter(Application_Method == "Split")
chlorophyll_one_time <- chlorophyll_long %>% filter(Application_Method == "One-time")

# --- Fit a Linear Mixed Effects Model ---
m1_chlorophyll <- lmer(Chlorophyll_Content ~ Dose * Time_Point + (1 | Combined.pot.id), 
                       data = chlorophyll_long)

# Model diagnostics
plot(m1_chlorophyll)
qqPlot(resid(m1_chlorophyll))  # QQ plot of residuals

# ANOVA to test significance of fixed effects
Anova(m1_chlorophyll, test = "F")

# Summary of the model
summary(m1_chlorophyll)

# --- Prediction Plot for Chlorophyll Content Using ggeffects ---
library(ggeffects)
predict_plot <- predict_response(m1_chlorophyll, c("Time_Point", "Dose")) %>% plot()
print(predict_plot)

# --- Visualization: Boxplots for Each Application Method ---
# For Split Application
ggplot(chlorophyll_split, aes(x = Time_Point, y = Chlorophyll_Content, color = Dose)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Chlorophyll Content Across Time Points (Split Application Method)",
    y = "Chlorophyll Content",
    x = "Time Point",
    color = "Dose (N kg/ha)"
  ) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# For One-time Application with Fertilizer_Type facets
ggplot(chlorophyll_one_time, aes(x = Time_Point, y = Chlorophyll_Content, color = Dose)) +
  geom_boxplot() +
  facet_wrap(~ Fertilizer_Type) +
  theme_minimal() +
  labs(
    title = "Chlorophyll Content Across Time Points (One-time Application Method)",
    y = "Chlorophyll Content",
    x = "Time Point",
    color = "Dose (N kg/ha)"
  ) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
#pH Analysis FOR PHALARIS
# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(car)
library(ggeffects)

# Convert variables to factors
Phalaris_exp2 <- Phalaris_exp2 %>%
  mutate(
    Dose = factor(Dose_.N.kg.ha., levels = c("0", "20", "40", "60", "80", "100", "120")),
    Fertilizer_Type = factor(Fertilizer_type, levels = c("Control", "MF", "UF")),
    Application_Method = factor(Application_method, levels = c("One-time", "Split"))
  )

# Select pH columns dynamically
pH_columns <- grep("^pH_", names(Phalaris_exp2), value = TRUE)

# Select relevant columns and reshape data
pH_data <- Phalaris_exp2 %>%
  select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method, all_of(pH_columns)) %>%
  pivot_longer(
    cols = starts_with("pH_"),
    names_to = "Time_Point",
    values_to = "pH_Value"
  ) %>%
  mutate(Time_Point = factor(Time_Point)) %>%
  drop_na(pH_Value)

# Separate data by Application Method
pH_split <- pH_data %>% filter(Application_Method == "Split")
pH_one_time <- pH_data %>% filter(Application_Method == "One-time")

# --- Fit a Linear Mixed Effects Model ---
m1_pH <- lmer(pH_Value ~ Dose * Time_Point + (1 | Combined.pot.id), data = pH_data)

# Model diagnostics
plot(m1_pH)
qqPlot(resid(m1_pH))
Anova(m1_pH, test = "F")
summary(m1_pH)

# --- Prediction Plot for pH Using ggeffects ---
predict_plot_pH <- predict_response(m1_pH, c("Time_Point", "Dose")) %>% plot()
print(predict_plot_pH)

# --- Visualization: Boxplots for Each Application Method ---
ggplot(pH_split, aes(x = Time_Point, y = pH_Value, color = Dose)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "pH Levels Across Time Points (Split Application Method)",
    y = "pH Value",
    x = "Time Point",
    color = "Dose (N kg/ha)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(pH_one_time, aes(x = Time_Point, y = pH_Value, color = Dose)) +
  geom_boxplot() +
  facet_wrap(~ Fertilizer_Type) +
  theme_minimal() +
  labs(
    title = "pH Levels Across Time Points (One-time Application Method)",
    y = "pH Value",
    x = "Time Point",
    color = "Dose (N kg/ha)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#EC ANALYSIS 
# Select EC columns dynamically
EC_columns <- grep("^EC_", names(Phalaris_exp2), value = TRUE)

# Select relevant columns and reshape data
EC_data <- Phalaris_exp2 %>%
  select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method, all_of(EC_columns)) %>%
  pivot_longer(
    cols = starts_with("EC_"),
    names_to = "Time_Point",
    values_to = "EC_Value"
  ) %>%
  mutate(Time_Point = factor(Time_Point)) %>%
  drop_na(EC_Value)

# Separate data by Application Method
EC_split <- EC_data %>% filter(Application_Method == "Split")
EC_one_time <- EC_data %>% filter(Application_Method == "One-time")

# --- Fit a Linear Mixed Effects Model ---
m1_EC <- lmer(EC_Value ~ Dose * Time_Point + (1 | Combined.pot.id), data = EC_data)

# Model diagnostics
plot(m1_EC)
qqPlot(resid(m1_EC))
Anova(m1_EC, test = "F")
summary(m1_EC)

# --- Prediction Plot for EC Using ggeffects ---
predict_plot_EC <- predict_response(m1_EC, c("Time_Point", "Dose")) %>% plot()
print(predict_plot_EC)

# --- Visualization: Boxplots for Each Application Method ---
ggplot(EC_split, aes(x = Time_Point, y = EC_Value, color = Dose)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "EC Levels Across Time Points (Split Application Method)",
    y = "EC Value",
    x = "Time Point",
    color = "Dose (N kg/ha)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(EC_one_time, aes(x = Time_Point, y = EC_Value, color = Dose)) +
  geom_boxplot() +
  facet_wrap(~ Fertilizer_Type) +
  theme_minimal() +
  labs(
    title = "EC Levels Across Time Points (One-time Application Method)",
    y = "EC Value",
    x = "Time Point",
    color = "Dose (N kg/ha)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#DryBiomass Analysis 
# Select Dry Biomass columns dynamically
Dry_columns <- grep("^Dry_", names(Phalaris_exp2), value = TRUE)

# Select relevant columns and reshape data
Dry_data <- Phalaris_exp2 %>%
  select(Combined.pot.id, Dose, Fertilizer_Type, Application_Method, all_of(Dry_columns)) %>%
  pivot_longer(
    cols = starts_with("Dry_"),
    names_to = "Time_Point",
    values_to = "Dry_Biomass"
  ) %>%
  mutate(Time_Point = factor(Time_Point)) %>%
  drop_na(Dry_Biomass)

# Separate data by Application Method
Dry_split <- Dry_data %>% filter(Application_Method == "Split")
Dry_one_time <- Dry_data %>% filter(Application_Method == "One-time")

# --- Fit a Linear Mixed Effects Model ---
m1_Dry <- lmer(Dry_Biomass ~ Dose * Time_Point + (1 | Combined.pot.id), data = Dry_data)

# Model diagnostics
plot(m1_Dry)
qqPlot(resid(m1_Dry))
Anova(m1_Dry, test = "F")
summary(m1_Dry)

# --- Prediction Plot for Dry Biomass Using ggeffects ---
predict_plot_Dry <- predict_response(m1_Dry, c("Time_Point", "Dose")) %>% plot()
print(predict_plot_Dry)

# --- Visualization: Boxplots for Each Application Method ---
ggplot(Dry_split, aes(x = Time_Point, y = Dry_Biomass, color = Dose)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Dry Biomass Across Time Points (Split Application Method)",
    y = "Dry Biomass",
    x = "Time Point",
    color = "Dose (N kg/ha)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(Dry_one_time, aes(x = Time_Point, y = Dry_Biomass, color = Dose)) +
  geom_boxplot() +
  facet_wrap(~ Fertilizer_Type) +
  theme_minimal() +
  labs(
    title = "Dry Biomass Across Time Points (One-time Application Method)",
    y = "Dry Biomass",
    x = "Time Point",
    color = "Dose (N kg/ha)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


colnames(Phalaris_exp2)
colnames(Lucerne_exp2)
# Load necessary libraries
library(ggplot2)

# Load datasets
Phalaris_exp2 <- read.csv("Phalaris_exp2.csv")  # Adjust filename if needed
Lucerne_exp2 <- read.csv("Lucerne_exp2.csv")  # Adjust filename if needed

# Ensure column names match exactly
colnames(Phalaris_exp2)
colnames(Lucerne_exp2)

# Rename columns if necessary to match across datasets
# names(Phalaris_exp2)[which(names(Phalaris_exp2) == "old_column_name")] <- "new_column_name"
# names(Lucerne_exp2)[which(names(Lucerne_exp2) == "old_column_name")] <- "new_column_name"

# Extract relevant columns
dose_phalaris <- Phalaris_exp2$Dose_.N.kg.ha.
dose_lucerne <- Lucerne_exp2$Dose...N.kg.ha.

# Comparisons
comparisons <- list(
  list(Phalaris = "Dry_Biomass..gm._35days", Lucerne = "Dry_Biomass_1", Time = "35 Days"),
  list(Phalaris = "Dry_Biomass.gm._66days", Lucerne = "Dry_Biomass._2", Time = "66 Days"),
  list(Phalaris = "Dry_Biomass..gm._95days", Lucerne = "Dry_Biomass_3", Time = "95 Days"),
  list(Phalaris = "Dry_shoot_..Biomass.g._125daysHD", Lucerne = "Dry_Biomass_shoot_4", Time = "125 Days (Shoot)"),
  list(Phalaris = "Dry_root_biomass.g._125daysHD", Lucerne = "Dry_Biomass_root_118days", Time = "125 Days (Root)")
)

# Loop through comparisons and plot
for (comp in comparisons) {
  df_phalaris <- data.frame(Dose = dose_phalaris, Biomass = Phalaris_exp2[[comp$Phalaris]], Plant = "Phalaris")
  df_lucerne <- data.frame(Dose = dose_lucerne, Biomass = Lucerne_exp2[[comp$Lucerne]], Plant = "Lucerne")
  
  # Combine datasets
  df_combined <- rbind(df_phalaris, df_lucerne)
  
  # Remove NA values (optional)
  df_combined <- na.omit(df_combined)
  
  # Create scatter plot with regression line
  plot <- ggplot(df_combined, aes(x = Dose, y = Biomass, color = Plant)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      title = paste("Dose vs Dry Biomass at", comp$Time),
      x = "Dose (N kg/ha)",
      y = "Dry Biomass (g)"
    ) +
    theme_minimal()
  
  print(plot)  # Display plot
}


# Load necessary libraries
library(ggplot2)
library(ggrepel)  # For better label placement

# Load datasets
Phalaris_exp2 <- read.csv("Phalaris_exp2.csv")  # Adjust filename if needed
Lucerne_exp2 <- read.csv("Lucerne_exp2.csv")  # Adjust filename if needed

# Extract relevant columns
dose_phalaris <- Phalaris_exp2$Dose_.N.kg.ha.
dose_lucerne <- Lucerne_exp2$Dose...N.kg.ha.

# Comparisons
comparisons <- list(
  list(Phalaris = "Dry_Biomass..gm._35days", Lucerne = "Dry_Biomass_1", Time = "35 Days"),
  list(Phalaris = "Dry_Biomass.gm._66days", Lucerne = "Dry_Biomass._2", Time = "66 Days"),
  list(Phalaris = "Dry_Biomass..gm._95days", Lucerne = "Dry_Biomass_3", Time = "95 Days"),
  list(Phalaris = "Dry_shoot_..Biomass.g._125daysHD", Lucerne = "Dry_Biomass_shoot_4", Time = "125 Days (Shoot)"),
  list(Phalaris = "Dry_root_biomass.g._125daysHD", Lucerne = "Dry_Biomass_root_118days", Time = "125 Days (Root)")
)

# Loop through comparisons and plot
for (comp in comparisons) {
  df_phalaris <- data.frame(Dose = dose_phalaris, Biomass = Phalaris_exp2[[comp$Phalaris]], Plant = "Phalaris")
  df_lucerne <- data.frame(Dose = dose_lucerne, Biomass = Lucerne_exp2[[comp$Lucerne]], Plant = "Lucerne")
  
  # Combine datasets
  df_combined <- rbind(df_phalaris, df_lucerne)
  
  # Remove NA values
  df_combined <- na.omit(df_combined)
  
  # Create scatter plot with regression line and improved design
  plot <- ggplot(df_combined, aes(x = Dose, y = Biomass, color = Plant, label = round(Biomass, 2))) +
    geom_point(size = 4, alpha = 0.8) +  # Bigger, semi-transparent points
    geom_smooth(method = "lm", se = FALSE, linewidth = 1.5) +  # Thicker regression line
    geom_text_repel(size = 4, fontface = "bold") +  # Data labels for better readability
    labs(
      title = paste("Effect of Dose on Dry Biomass at", comp$Time),
      x = "Dose (N kg/ha)",
      y = "Dry Biomass (g)"
    ) +
    theme_minimal(base_size = 14) +  # Larger text overall
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  # Bold title
      axis.title.x = element_text(face = "bold", size = 14),  # Bold x-axis title
      axis.title.y = element_text(face = "bold", size = 14),  # Bold y-axis title
      axis.text = element_text(size = 12, color = "black"),  # Darker axis labels
      axis.line = element_line(color = "black"),  # Darker axis lines
      panel.grid.major = element_line(color = "grey80"),  # Subtle grid
      legend.title = element_blank(),  # No legend title
      legend.text = element_text(size = 12)  # Larger legend text
    ) +
    scale_x_continuous(breaks = unique(df_combined$Dose))  # Show all dose values on x-axis
  
  print(plot)  # Display plot
}

# Load necessary libraries
library(ggplot2)
library(ggrepel)  # For better label placement
library(dplyr)  # For filtering data

# Load datasets
Phalaris_exp2 <- read.csv("Phalaris_exp2.csv")  # Adjust filename if needed
Lucerne_exp2 <- read.csv("Lucerne_exp2.csv")  # Adjust filename if needed

# Define column names for filtering
application_column_phalaris <- "Application_method"  # Adjust if needed
application_column_lucerne <- "Application_method"  # Adjust if needed

# Filter datasets for Split and One-Time applications separately
split_phalaris <- Phalaris_exp2 %>% filter(!!as.name(application_column_phalaris) == "Split")
split_lucerne <- Lucerne_exp2 %>% filter(!!as.name(application_column_lucerne) == "Split")

one_time_phalaris <- Phalaris_exp2 %>% filter(!!as.name(application_column_phalaris) == "One-Time")
one_time_lucerne <- Lucerne_exp2 %>% filter(!!as.name(application_column_lucerne) == "One-Time")

# Define comparisons
comparisons <- list(
  list(Phalaris = "Dry_Biomass..gm._35days", Lucerne = "Dry_Biomass_1", Time = "35 Days"),
  list(Phalaris = "Dry_Biomass.gm._66days", Lucerne = "Dry_Biomass._2", Time = "66 Days"),
  list(Phalaris = "Dry_Biomass..gm._95days", Lucerne = "Dry_Biomass_3", Time = "95 Days"),
  list(Phalaris = "Dry_shoot_..Biomass.g._125daysHD", Lucerne = "Dry_Biomass_shoot_4", Time = "125 Days (Shoot)"),
  list(Phalaris = "Dry_root_biomass.g._125daysHD", Lucerne = "Dry_Biomass_root_118days", Time = "125 Days (Root)")
)

# Function to generate plots
generate_plots <- function(phalaris_data, lucerne_data, application_type) {
  for (comp in comparisons) {
    df_phalaris <- data.frame(Dose = phalaris_data$Dose_.N.kg.ha., Biomass = phalaris_data[[comp$Phalaris]], Plant = "Phalaris")
    df_lucerne <- data.frame(Dose = lucerne_data$Dose...N.kg.ha., Biomass = lucerne_data[[comp$Lucerne]], Plant = "Lucerne")
    
    # Combine datasets
    df_combined <- rbind(df_phalaris, df_lucerne)
    
    # Remove NA values
    df_combined <- na.omit(df_combined)
    
    # Create scatter plot with regression line and improved design
    plot <- ggplot(df_combined, aes(x = Dose, y = Biomass, color = Plant, label = round(Biomass, 2))) +
      geom_point(size = 4, alpha = 0.8) +  # Bigger, semi-transparent points
      geom_smooth(method = "lm", se = FALSE, linewidth = 1.5) +  # Thicker regression line
      geom_text_repel(size = 4, fontface = "bold") +  # Data labels for better readability
      labs(
        title = paste(application_type, "Application: Dose vs Dry Biomass at", comp$Time),
        x = "Dose (N kg/ha)",
        y = "Dry Biomass (g)"
      ) +
      theme_minimal(base_size = 14) +  # Larger text overall
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  # Bold title
        axis.title.x = element_text(face = "bold", size = 14),  # Bold x-axis title
        axis.title.y = element_text(face = "bold", size = 14),  # Bold y-axis title
        axis.text = element_text(size = 12, color = "black"),  # Darker axis labels
        axis.line = element_line(color = "black"),  # Darker axis lines
        panel.grid.major = element_line(color = "grey80"),  # Subtle grid
        legend.title = element_blank(),  # No legend title
        legend.text = element_text(size = 12)  # Larger legend text
      ) +
      scale_x_continuous(breaks = unique(df_combined$Dose))  # Show all dose values on x-axis
    
    print(plot)  # Display plot
  }
}

# Generate plots for Split Application
generate_plots(split_phalaris, split_lucerne, "Split")

# Generate plots for One-Time Application
generate_plots(one_time_phalaris, one_time_lucerne, "One-Time")


# Load necessary libraries
library(ggplot2)
library(ggrepel)  
library(dplyr)

# Load datasets
Phalaris_exp2 <- read.csv("Phalaris_exp2.csv")
Lucerne_exp2 <- read.csv("Lucerne_exp2.csv")

# Debugging: Check unique values in Application Method column
print("Unique values in Phalaris Application column:")
print(unique(Phalaris_exp2$Application_method))
print("Unique values in Lucerne Application column:")
print(unique(Lucerne_exp2$Application_method))

# Convert application method to lowercase for consistent filtering
Phalaris_exp2$Application_method <- tolower(Phalaris_exp2$Application_method)
Lucerne_exp2$Application_method <- tolower(Lucerne_exp2$Application_method)

# Filter datasets for Split and One-Time applications
split_phalaris <- Phalaris_exp2 %>% filter(Application_method == "split")
split_lucerne <- Lucerne_exp2 %>% filter(Application_method == "split")

one_time_phalaris <- Phalaris_exp2 %>% filter(Application_method == "one-time")
one_time_lucerne <- Lucerne_exp2 %>% filter(Application_method == "one-time")

# Debugging: Check number of rows after filtering
print(paste("Rows in split_phalaris:", nrow(split_phalaris)))
print(paste("Rows in split_lucerne:", nrow(split_lucerne)))
print(paste("Rows in one_time_phalaris:", nrow(one_time_phalaris)))
print(paste("Rows in one_time_lucerne:", nrow(one_time_lucerne)))

# Define comparisons
comparisons <- list(
  list(Phalaris = "Dry_Biomass..gm._35days", Lucerne = "Dry_Biomass_1", Time = "35 Days"),
  list(Phalaris = "Dry_Biomass.gm._66days", Lucerne = "Dry_Biomass._2", Time = "66 Days"),
  list(Phalaris = "Dry_Biomass..gm._95days", Lucerne = "Dry_Biomass_3", Time = "95 Days"),
  list(Phalaris = "Dry_shoot_..Biomass.g._125daysHD", Lucerne = "Dry_Biomass_shoot_4", Time = "125 Days (Shoot)"),
  list(Phalaris = "Dry_root_biomass.g._125daysHD", Lucerne = "Dry_Biomass_root_118days", Time = "125 Days (Root)")
)

# Function to generate plots
generate_plots <- function(phalaris_data, lucerne_data, application_type) {
  # Check if both datasets have data
  if (nrow(phalaris_data) == 0 | nrow(lucerne_data) == 0) {
    print(paste("No data available for", application_type, "Application. Skipping plots."))
    return()
  }
  
  for (comp in comparisons) {
    df_phalaris <- data.frame(Dose = phalaris_data$Dose_.N.kg.ha., Biomass = phalaris_data[[comp$Phalaris]], Plant = "Phalaris")
    df_lucerne <- data.frame(Dose = lucerne_data$Dose...N.kg.ha., Biomass = lucerne_data[[comp$Lucerne]], Plant = "Lucerne")
    
    # Combine datasets
    df_combined <- rbind(df_phalaris, df_lucerne)
    
    # Remove NA values
    df_combined <- na.omit(df_combined)
    
    # Skip if dataframe is empty after removing NA values
    if (nrow(df_combined) == 0) {
      print(paste("No valid data for", comp$Time, "in", application_type, "Application. Skipping plot."))
      next
    }
    
    # Create scatter plot with regression line and improved design
    plot <- ggplot(df_combined, aes(x = Dose, y = Biomass, color = Plant, label = round(Biomass, 2))) +
      geom_point(size = 4, alpha = 0.8) +  
      geom_smooth(method = "lm", se = FALSE, linewidth = 1.5) +  
      geom_text_repel(size = 4, fontface = "bold") +  
      labs(
        title = paste(application_type, "Application: Dose vs Dry Biomass at", comp$Time),
        x = "Dose (N kg/ha)",
        y = "Dry Biomass (g)"
      ) +
      theme_minimal(base_size = 14) +  
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  
        axis.title.x = element_text(face = "bold", size = 14),  
        axis.title.y = element_text(face = "bold", size = 14),  
        axis.text = element_text(size = 12, color = "black"),  
        axis.line = element_line(color = "black"),  
        panel.grid.major = element_line(color = "grey80"),  
        legend.title = element_blank(),  
        legend.text = element_text(size = 12)  
      ) +
      scale_x_continuous(breaks = unique(df_combined$Dose))  
    
    print(plot)  
  }
}

# Generate plots for Split Application
generate_plots(split_phalaris, split_lucerne, "Split")

# Generate plots for One-Time Application
generate_plots(one_time_phalaris, one_time_lucerne, "One-Time")








# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load datasets
Phalaris_exp2 <- read.csv("Phalaris_exp2.csv")
Lucerne_exp2 <- read.csv("Lucerne_exp2.csv")

# Convert application method to lowercase for consistent filtering
Phalaris_exp2$Application_method <- tolower(Phalaris_exp2$Application_method)
Lucerne_exp2$Application_method <- tolower(Lucerne_exp2$Application_method)

# Filter datasets for Split and One-Time applications
split_phalaris <- Phalaris_exp2 %>% filter(Application_method == "split")
split_lucerne <- Lucerne_exp2 %>% filter(Application_method == "split")

one_time_phalaris <- Phalaris_exp2 %>% filter(Application_method == "one-time")
one_time_lucerne <- Lucerne_exp2 %>% filter(Application_method == "one-time")

# Define comparisons
comparisons <- list(
  list(Phalaris = "Dry_Biomass..gm._35days", Lucerne = "Dry_Biomass_1", Time = "35 Days"),
  list(Phalaris = "Dry_Biomass.gm._66days", Lucerne = "Dry_Biomass._2", Time = "66 Days"),
  list(Phalaris = "Dry_Biomass..gm._95days", Lucerne = "Dry_Biomass_3", Time = "95 Days"),
  list(Phalaris = "Dry_shoot_..Biomass.g._125daysHD", Lucerne = "Dry_Biomass_shoot_4", Time = "125 Days (Shoot)"),
  list(Phalaris = "Dry_root_biomass.g._125daysHD", Lucerne = "Dry_Biomass_root_118days", Time = "125 Days (Root)")
)

# Function to generate a single combined plot
generate_combined_plot <- function(phalaris_data, lucerne_data, application_type) {
  # Check if both datasets have data
  if (nrow(phalaris_data) == 0 | nrow(lucerne_data) == 0) {
    print(paste("No data available for", application_type, "Application. Skipping plot."))
    return()
  }
  
  # Initialize empty dataframe for combined data
  combined_data <- data.frame()
  
  # Loop through each comparison and add data to combined dataframe
  for (comp in comparisons) {
    df_phalaris <- data.frame(
      Dose = phalaris_data$Dose_.N.kg.ha.,
      Biomass = phalaris_data[[comp$Phalaris]],
      Plant = "Phalaris",
      Time = comp$Time
    )
    
    df_lucerne <- data.frame(
      Dose = lucerne_data$Dose...N.kg.ha.,
      Biomass = lucerne_data[[comp$Lucerne]],
      Plant = "Lucerne",
      Time = comp$Time
    )
    
    # Combine datasets
    df_combined <- rbind(df_phalaris, df_lucerne)
    
    # Remove NA values
    df_combined <- na.omit(df_combined)
    
    # Add to overall combined data
    combined_data <- rbind(combined_data, df_combined)
  }
  
  # If no valid data, skip plot
  if (nrow(combined_data) == 0) {
    print(paste("No valid data for", application_type, "Application. Skipping plot."))
    return()
  }
  
  # Create a single combined scatter plot with regression lines
  plot <- ggplot(combined_data, aes(x = Dose, y = Biomass, color = Plant, shape = Time)) +
    geom_point(size = 3, alpha = 0.8) +  # Scatter points
    geom_smooth(method = "lm", se = FALSE, aes(linetype = Time), linewidth = 1.2) +  # Regression lines
    labs(
      title = paste(application_type, "Application: Dose vs Dry Biomass"),
      x = "Dose (N kg/ha)",
      y = "Dry Biomass (g)",
      color = "Plant Type",
      shape = "Time Period",
      linetype = "Time Period"
    ) +
    theme_minimal(base_size = 14) +  
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  
      axis.title.x = element_text(face = "bold", size = 14),  
      axis.title.y = element_text(face = "bold", size = 14),  
      axis.text = element_text(size = 12, color = "black"),  
      axis.line = element_line(color = "black"),  
      panel.grid.major = element_line(color = "grey80"),  
      legend.text = element_text(size = 12),  
      legend.title = element_text(face = "bold")
    ) +
    scale_x_continuous(breaks = unique(combined_data$Dose))  # Ensure all dose values appear
  
  print(plot)  # Display plot
}

# Generate combined plot for Split Application
generate_combined_plot(split_phalaris, split_lucerne, "Split")

# Generate combined plot for One-Time Application
generate_combined_plot(one_time_phalaris, one_time_lucerne, "One-Time")


