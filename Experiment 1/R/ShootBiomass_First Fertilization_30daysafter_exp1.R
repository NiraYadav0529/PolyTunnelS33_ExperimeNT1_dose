# Load necessary libraries
library(ggplot2)
library(dplyr)
library(car)
library(emmeans)
library(multcomp)

# Load the dataset
Plant_traits <- read.csv("Biomass_height_stem_data_exp1.csv")
colnames(Plant_traits)


# Ensure necessary columns are treated as factors
Plant_traits <- Plant_traits %>%
  mutate(
    Dose = factor(Dose..N.kg.ha., levels = c("0", "100", "200")),  # Convert Dose column to factor
    Fertilizer_Type = factor(Fertilizer.type, levels = c('None', 'MF', 'UF')),                     # Convert Fertilizer.type to Fertilizer_Type
    Plant.type = factor(Plant.type)                                # Ensure Plant.type is a factor
  )
Plant_traits %>% 
  group_by(Plant.type, Fertilizer_Type, Dose) %>% 
  summarise(n())

# Calculate mean and standard errors for shoot and root biomass
biomass_summary <- Plant_traits %>%
  group_by(Dose, Plant.type, Fertilizer_Type) %>%
  summarise(
    Mean_Shoot = mean(Fresh_biomass_AF_30days.gm., na.rm = TRUE),
    SE_Shoot = sd(Fresh_biomass_AF_30days.gm., na.rm = TRUE) / sqrt(n()),
     ) %>% ungroup()

### SHOOT BIOMASS ANALYSIS ###
# Model for shoot biomass
m1_shoot <- lm(Fresh_biomass_AF_30days.gm. ~ Dose * Fertilizer_Type * Plant.type 
               - Dose:Fertilizer_Type:Plant.type, data = Plant_traits)
summary(m1_shoot)

# Check residuals for shoot biomass
residualPlot(m1_shoot)
qqPlot(m1_shoot)

# Test significance of the model effects for shoot biomass
Anova(m1_shoot, type = "II")

# Perform multiple comparison test (Tukey HSD) for shoot biomass
pairwise_comparisons_shoot <- emmeans(m1_shoot, ~ Dose + Fertilizer_Type | Plant.type)
letters_shoot <- cld(pairwise_comparisons_shoot, Letters=letters) %>%
  mutate(.group = stringr::str_trim(.group)) %>% 
  as.data.frame()

# Merge the letters for shoot biomass with biomass_summary using Dose and Plant.type
biomass_summary_shoot <- biomass_summary %>%
  left_join(letters_shoot, by = c("Dose", "Fertilizer_Type", "Plant.type"))






# SHOOT BIOMASS PLOT
ggplot(biomass_summary_shoot, aes(x = Dose, y = Mean_Shoot, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve='single'), 
           colour = "black") +
  geom_errorbar(aes(ymin = Mean_Shoot - SE_Shoot, ymax = Mean_Shoot + SE_Shoot), 
                width=0.2, position = position_dodge(width=0.9)) +
  labs(title = "Shoot Biomass 30 days after fertilization", x = "Dose (N kg/ha)", 
       y = "Shoot Biomass (g, +/- SE)", fill = 'Fertilizer') +
  facet_wrap(~ Plant.type) +
  scale_fill_manual(values = c("UF" = "black", "MF" = "white", "None" = "grey")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(y = Mean_Shoot + SE_Shoot + 3, label = .group), 
            position = position_dodge2(0.9, preserve='single'))















# Load necessary libraries
library(dplyr)
library(ggplot2)
library(emmeans)
library(multcompView)

# Load the dataset (assuming you have already loaded the CSV file)
colnames(Plant_traits)

# Ensure necessary columns are treated as factors
Plant_traits <- Plant_traits %>%
  mutate(
    Dose = factor(Dose..N.kg.ha., levels = c("0", "100", "200")),  # Convert Dose column to factor
    Fertilizer_Type = factor(Fertilizer.type, levels = c('None', 'MF', 'UF')),  # Convert Fertilizer.type to Fertilizer_Type
    Plant.type = case_match(Plant.type, 'L' ~ 'Lucerne', 'P' ~ 'Phalaris'),  # Rename levels for labeling facets
    Plant.type = factor(Plant.type)  # Ensure Plant.type is a factor
  )

# Check the structure
Plant_traits %>% 
  group_by(Plant.type, Fertilizer_Type, Dose) %>% 
  summarise(n())

# Calculate mean and standard errors for shoot biomass
biomass_summary <- Plant_traits %>%
  group_by(Dose, Plant.type, Fertilizer_Type) %>%
  summarise(
    Mean_Shoot = mean(Fresh_biomass_AF_30days.gm., na.rm = TRUE),
    SE_Shoot = sd(Fresh_biomass_AF_30days.gm., na.rm = TRUE) / sqrt(n())
  ) %>% ungroup()

### SHOOT BIOMASS ANALYSIS ###
# Model for shoot biomass
m1_shoot <- lm(Fresh_biomass_AF_30days.gm. ~ Dose * Fertilizer_Type * Plant.type 
               - Dose:Fertilizer_Type:Plant.type, data = Plant_traits)
summary(m1_shoot)

# Check residuals for shoot biomass
residualPlot(m1_shoot)
qqPlot(m1_shoot)

# Test significance of the model effects for shoot biomass
Anova(m1_shoot, type = "II")

# Perform multiple comparison test (Tukey HSD) for shoot biomass
pairwise_comparisons_shoot <- emmeans(m1_shoot, ~ Dose + Fertilizer_Type | Plant.type)
letters_shoot <- cld(pairwise_comparisons_shoot, Letters = letters) %>%
  mutate(.group = stringr::str_trim(.group)) %>%
  as.data.frame()

# Merge the letters for shoot biomass with biomass_summary using Dose and Plant.type
biomass_summary_shoot <- biomass_summary %>%
  left_join(letters_shoot, by = c("Dose", "Fertilizer_Type", "Plant.type"))

# SHOOT BIOMASS PLOT with y-axis limit set to 50
ggplot(biomass_summary_shoot, aes(x = Dose, y = Mean_Shoot, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), 
           colour = "black") +
  geom_errorbar(aes(ymin = Mean_Shoot - SE_Shoot, ymax = Mean_Shoot + SE_Shoot), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  labs(title = "Shoot Biomass 30 days after fertilization", 
       x = "Dose (N kg/ha)", 
       y = "Shoot Biomass (g, +/- SE)", 
       fill = 'Fertilizer') +
  facet_wrap(~ Plant.type) +
  scale_fill_manual(values = c("UF" = "black", "MF" = "white", "None" = "grey")) +
  theme_minimal() +
  
  # Adjusting the theme for bold text and making it black
  theme(
    axis.title.x = element_text(face = "bold", color = "black", size = 14),
    axis.title.y = element_text(face = "bold", color = "black", size = 14),
    axis.text.x = element_text(face = "bold", color = "black", size = 12),
    axis.text.y = element_text(face = "bold", color = "black", size = 12),
    plot.title = element_text(face = "bold", color = "black", size = 16, hjust = 0.5),
    legend.title = element_text(face = "bold", color = "black", size = 12),
    legend.text = element_text(face = "bold", color = "black", size = 12),
    strip.text = element_text(face = "bold", color = "black", size = 14)
  ) +
  
  # Add y-axis limit to 50
  ylim(0, 50) +
  
  # Add compact letter display (CLD) text on top of bars
  geom_text(aes(y = Mean_Shoot + SE_Shoot + 3, label = .group), 
            position = position_dodge2(0.9, preserve = 'single'))























# Filter data to include only 'Lucerne'
lucerne_data <- biomass_summary_shoot %>%
  filter(Plant.type == "Lucerne")

# SHOOT BIOMASS PLOT for Lucerne only with y-axis limit set to 50
ggplot(lucerne_data, aes(x = Dose, y = Mean_Shoot, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), 
           colour = "black") +
  geom_errorbar(aes(ymin = Mean_Shoot - SE_Shoot, ymax = Mean_Shoot + SE_Shoot), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  labs(
    title = "Shoot Biomass 30 days after fertilization (Lucerne)", 
    x = "Dose (N kg/ha)", 
    y = "Shoot Biomass (g, +/- SE)", 
    fill = 'Fertilizer'
  ) +
  scale_fill_manual(values = c("UF" = "black", "MF" = "white", "None" = "grey")) +
  theme_minimal() +
  
  # Adjusting the theme for bold text and making it black
  theme(
    axis.title.x = element_text(face = "bold", color = "black", size = 14),
    axis.title.y = element_text(face = "bold", color = "black", size = 14),
    axis.text.x = element_text(face = "bold", color = "black", size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(face = "bold", color = "black", size = 12),
    plot.title = element_text(face = "bold", color = "black", size = 16, hjust = 0.5),
    legend.title = element_text(face = "bold", color = "black", size = 12),
    legend.text = element_text(face = "bold", color = "black", size = 12)
  ) +
  
  # Set y-axis limit to 50
  ylim(0, 50) +
  
  # Add compact letter display (CLD) text on top of bars
  geom_text(
    aes(y = Mean_Shoot + SE_Shoot + 3, label = .group), 
    position = position_dodge2(width = 0.9, preserve = 'single')
  )





# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(emmeans)   # For Tukey HSD and group letters
library(car)       # For Anova (Type II)

# Step 1: Preprocess data
GHZs_Data_long <- GHZs_Data_long %>%
  mutate(
    Dose = factor(Dose..N.kg.ha., levels = c("0", "100", "200")),  # Convert Dose column to factor
    Fertilizer_Type = factor(Fertilizer_Type, levels = c('None', 'MF', 'UF')),  # Convert Fertilizer.type to Fertilizer_Type
    Plant.type = factor(Plant.type)  # Ensure Plant.type is a factor
  )

# Step 2: Define function for day-wise analysis and plotting
analyze_and_plot <- function(data, test_name) {
  # Filter the data for the specific test
  test_data <- data %>%
    filter(Test.Name == test_name)
  
  # Store plots for each day
  test_day_plots <- list()
  
  for (day in unique(test_data$Timepoint)) {
    
    # Filter data for the specific day
    day_data <- test_data %>%
      filter(Timepoint == day)
    
    # Calculate means and standard errors for each group
    biomass_summary <- day_data %>%
      group_by(Dose, Plant.type, Fertilizer_Type) %>%
      summarize(
        Mean_Flux = mean(Flux, na.rm = TRUE),
        SE_Flux = sd(Flux, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      )
    
    # Fit linear model
    day_model <- lm(Flux ~ Dose * Fertilizer_Type * Plant.type - Dose:Fertilizer_Type:Plant.type, data = day_data)
    
    # Residual diagnostics
    residual_plot <- ggplot(day_data, aes(x = predict(day_model), y = residuals(day_model))) +
      geom_point(alpha = 0.6, color = "blue") +
      geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
      labs(
        title = paste("Residual Plot for", test_name, "on Day", day),
        x = "Predicted Flux",
        y = "Residuals"
      ) +
      theme_minimal()
    
    qq_plot <- ggplot(day_data, aes(sample = residuals(day_model))) +
      stat_qq() +
      stat_qq_line(color = "red", linetype = "dashed") +
      labs(
        title = paste("QQ Plot for", test_name, "on Day", day),
        x = "Theoretical Quantiles",
        y = "Sample Quantiles"
      ) +
      theme_minimal()
    
    # Print residual diagnostics
    print(residual_plot)
    print(qq_plot)
    
    # Perform ANOVA (Type II) and Tukey HSD test
    print(Anova(day_model, type = "II"))  # Print ANOVA results
    
    pairwise_comparisons <- emmeans(day_model, ~ Dose + Fertilizer_Type | Plant.type)
    letters <- cld(pairwise_comparisons, Letters = letters) %>%
      mutate(.group = stringr::str_trim(.group)) %>%
      as.data.frame()
    
    # Merge letters with summary data
    biomass_summary <- biomass_summary %>%
      left_join(letters, by = c("Dose", "Fertilizer_Type", "Plant.type"))
    
    # Plot flux data
    day_plot <- ggplot(biomass_summary, aes(x = Dose, y = Mean_Flux, fill = Fertilizer_Type)) +
      geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), color = "black") +
      geom_errorbar(
        aes(ymin = Mean_Flux - SE_Flux, ymax = Mean_Flux + SE_Flux),
        width = 0.2,
        position = position_dodge(width = 0.9)
      ) +
      geom_text(
        aes(y = Mean_Flux + SE_Flux + 3, label = .group),
        position = position_dodge2(0.9, preserve = 'single')
      ) +
      labs(
        title = paste(test_name, "Flux on Day", day),
        x = "Nitrogen Dose (kg/ha)",
        y = "Flux (umol/min/m2)",
        fill = "Fertilizer"
      ) +
      facet_wrap(~ Plant.type) +
      scale_fill_manual(values = c("UF" = "black", "MF" = "white", "None" = "grey")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Store plot
    test_day_plots[[as.character(day)]] <- day_plot
    print(day_plot)  # Print plot
  }
  
  return(test_day_plots)
}

# Step 3: Analyze and plot for CH4, CO2, and N2O
CH4_day_plots <- analyze_and_plot(GHZs_Data_long, "CH4")
CO2_day_plots <- analyze_and_plot(GHZs_Data_long, "CO2")
N2O_day_plots <- analyze_and_plot(GHZs_Data_long, "N2O")





# Load necessary libraries
library(ggplot2)
library(dplyr)
library(emmeans)
library(car)       # For ANOVA (Type II)
library(stringr)   # For string manipulation

colnames(GHZs_Data_long)
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(emmeans)
library(car)       # For ANOVA (Type II)
library(stringr)   # For string manipulation

# Step 1: Preprocess data
GHZs_Data_long <- GHZs_Data_long %>%
  mutate(
    Dose = factor(Dose..N.kg.ha., levels = c("0", "100", "200")),  # Convert Dose column to factor
    Fertilizer_Type = factor(Fertilizer_Type, levels = c('None', 'MF', 'UF')),  # Convert Fertilizer_Type to a factor
    Plant.type = factor(Plant.type)  # Ensure Plant.type is a factor
  )

# Step 2: Define the analysis function for 1st day
analyze_1st_day <- function(data, test_name, flux_column_name) {
  # Filter data for the specific test
  day_data <- data %>%
    filter(Test.Name == test_name) %>%                      # Filter for the specific test (e.g., CH4)
    mutate(Flux = .data[[flux_column_name]])                # Rename the selected column to Flux
  
  # Select relevant columns without renaming inside select()
  day_data <- day_data %>%
    select(Dose, Fertilizer_Type, Plant.type, Flux)
  
  # Calculate means and standard errors for each group
  biomass_summary <- day_data %>%
    group_by(Dose, Plant.type, Fertilizer_Type) %>%
    summarize(
      Mean_Flux = mean(Flux, na.rm = TRUE),                 # Calculate mean flux
      SE_Flux = sd(Flux, na.rm = TRUE) / sqrt(n()),         # Calculate standard error
      .groups = "drop"
    )
  
  # Fit linear model
  day_model <- lm(Flux ~ Dose * Fertilizer_Type * Plant.type - Dose:Fertilizer_Type:Plant.type, data = day_data)
  
  # Residual diagnostics
  residual_plot <- ggplot(day_data, aes(x = predict(day_model), y = residuals(day_model))) +
    geom_point(alpha = 0.6, color = "blue") +
    geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
    labs(
      title = paste("Residual Plot for", test_name, "on 1st Day"),
      x = "Predicted Flux",
      y = "Residuals"
    ) +
    theme_minimal()
  
  qq_plot <- ggplot(day_data, aes(sample = residuals(day_model))) +
    stat_qq() +
    stat_qq_line(color = "red", linetype = "dashed") +
    labs(
      title = paste("QQ Plot for", test_name, "Residuals on 1st Day"),
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    theme_minimal()
  
  # Print residual diagnostics
  print(residual_plot)
  print(qq_plot)
  
  # Perform ANOVA (Type II)
  anova_results <- Anova(day_model, type = "II")
  print(anova_results)
  
  # Perform Tukey HSD test for multiple comparisons
  pairwise_comparisons <- emmeans(day_model, ~ Dose + Fertilizer_Type | Plant.type)
  letters <- cld(pairwise_comparisons, Letters = letters) %>%
    mutate(.group = stringr::str_trim(.group)) %>%
    as.data.frame()
  
  # Merge letters with summary data
  biomass_summary <- biomass_summary %>%
    left_join(letters, by = c("Dose", "Fertilizer_Type", "Plant.type"))
  
  # Plot flux data
  day_plot <- ggplot(biomass_summary, aes(x = Dose, y = Mean_Flux, fill = Fertilizer_Type)) +
    geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), color = "black") +
    geom_errorbar(
      aes(ymin = Mean_Flux - SE_Flux, ymax = Mean_Flux + SE_Flux),
      width = 0.2,
      position = position_dodge(width = 0.9)
    ) +
    geom_text(
      aes(y = Mean_Flux + SE_Flux + 3, label = .group),
      position = position_dodge2(0.9, preserve = 'single')
    ) +
    labs(
      title = paste(test_name, "Flux on 1st Day"),
      x = "Nitrogen Dose (kg/ha)",
      y = "Flux (umol/min/m2)",
      fill = "Fertilizer"
    ) +
    facet_wrap(~ Plant.type) +
    scale_fill_manual(values = c("UF" = "black", "MF" = "white", "None" = "grey")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Print the plot
  print(day_plot)
  
  return(day_plot) # Return the plot
}

# Step 3: Analyze 1st day data for CH4
CH4_1st_day_plot <- analyze_1st_day(GHZs_Data_long, "CH4", "AF_1stday_flux..umol.min.m2.")

# Step 4: Analyze 1st day data for CO2
CO2_1st_day_plot <- analyze_1st_day(GHZs_Data_long, "CO2", "AF_1stday_flux..umol.min.m2.")

# Step 5: Analyze 1st day data for N2O
N2O_1st_day_plot <- analyze_1st_day(GHZs_Data_long, "N2O", "AF_1stday_flux..umol.min.m2.")
