setwd("C:\\Users\\90958427\\OneDrive - Western Sydney University\\PolyTunnelS33_ExperimeNT1_dose\\Modified Data File")
list.files()

GHZs_Data <-read.csv("Final_Data_for analysis_ghZ.csv")
str(GHZs_Data)
# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
colnames(GHZs_Data)
# Reshape data into tidy format
GHZs_Data_long <- GHZs_Data %>%
  pivot_longer(
    cols = starts_with("BF.flux") | starts_with("AF_"),
    names_to = "Timepoint",  # Create a column for time points
    values_to = "Flux"       # Store flux values in this column
  ) %>%
  mutate(
    Timepoint = case_when(  # Map timepoints to numeric time
      Timepoint == "BF.flux..umol.min.m2." ~ 0,
      Timepoint == "AF_1stday_flux..umol.min.m2." ~ 1,
      Timepoint == "AF_3rdday_flux..umol.min.m2." ~ 3,
      Timepoint == "AF_5thday_flux..umol.min.m2." ~ 5,
      Timepoint == "AF_7thday_flux..umol.min.m2." ~ 7
    )
  )

# Perform Linear Regression
model <- lm(Flux ~ Dose..N.kg.ha. * Fertilizer_Type * Plant.type * Test.Name, data = GHZs_Data_long)

# Summarize the regression results
summary(model)

# Perform ANOVA
anova_results <- anova(model)
print(anova_results)

# Extract standard errors and residuals
GHZs_Data_long <- GHZs_Data_long %>%
  mutate(
    Residuals = residuals(model),
    Predicted = predict(model)
  )

# Calculate means and standard errors for the plot
plot_data <- GHZs_Data_long %>%
  group_by(Timepoint, Dose..N.kg.ha., Fertilizer_Type, Plant.type, Test.Name) %>%
  summarize(
    mean_flux = mean(Flux, na.rm = TRUE),
    se_flux = sd(Flux, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Create time-series plot
ggplot(plot_data, aes(
  x = Timepoint, y = mean_flux,
  color = as.factor(Dose..N.kg.ha.), group = interaction(Fertilizer_Type, Plant.type, Dose..N.kg.ha.)
)) +
  geom_line(size = 1) +  # Line for mean flux
  geom_point(size = 2) +  # Points for mean flux
  geom_errorbar(aes(ymin = mean_flux - se_flux, ymax = mean_flux + se_flux), width = 0.2) +  # Error bars
  facet_wrap(~ Test.Name, scales = "free_y", ncol = 2) +  # Facet by gas type
  scale_color_brewer(palette = "Set1", name = "Dose (N kg/ha)") +
  labs(
    title = "Flux Changes Over Time for Different Gases",
    x = "Time (Days)",
    y = "Flux (umol/min/m2)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),  # Remove gridlines
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    panel.background = element_rect(fill = "white", color = NA),  # White background
    strip.background = element_rect(fill = "lightgray", color = "black"),  # Facet background
    strip.text = element_text(face = "bold", size = 12)  # Bold facet titles
  )






# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(agricolae) # For letter-wise comparison (HSD.test)

# Function to perform pairwise comparisons and add significance letters
get_significance_letters <- function(model, factor_name) {
  anova_res <- aov(model)
  hsd <- HSD.test(anova_res, factor_name, group = TRUE)
  hsd$groups <- hsd$groups %>%
    mutate(level = rownames(hsd$groups))
  return(hsd$groups)
}


# Step 1: Filter CH4 data
CH4_data <- GHZs_Data_long %>%
  filter(Test.Name == "CH4")

# Step 2: Analyze Data for Each Day
CH4_day_plots <- list() # Store plots for each day

for (day in unique(CH4_data$Timepoint)) {
  
  # Filter data for the specific day
  day_data <- CH4_data %>%
    filter(Timepoint == day)
  
  # Fit a linear model for the day
  day_model <- lm(Flux ~ Dose..N.kg.ha. * Fertilizer_Type * Plant.type, data = day_data)
  
  # Residual diagnostics: Residual plot
  residual_plot <- day_data %>%
    mutate(
      Residuals = residuals(day_model),
      Predicted = predict(day_model)
    ) %>%
    ggplot(aes(x = Predicted, y = Residuals)) +
    geom_point(alpha = 0.6, color = "blue") +
    geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
    labs(
      title = paste("Residual Plot for CH4 on Day", day),
      x = "Predicted Flux",
      y = "Residuals"
    ) +
    theme_minimal()
  
  print(residual_plot) # Print residual plot
  
  # Residual diagnostics: QQ plot
  qq_plot <- day_data %>%
    mutate(Residuals = residuals(day_model)) %>%
    ggplot(aes(sample = Residuals)) +
    stat_qq() +
    stat_qq_line(color = "red", linetype = "dashed") +
    labs(
      title = paste("QQ Plot for CH4 Residuals on Day", day),
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    theme_minimal()
  
  print(qq_plot) # Print QQ plot
  
  # Calculate group means and standard errors
  plot_data <- day_data %>%
    group_by(Dose..N.kg.ha., Fertilizer_Type, Plant.type) %>%
    summarize(
      mean_flux = mean(Flux, na.rm = TRUE),
      se_flux = sd(Flux, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  # Perform Tukey HSD test and add significance letters
  letters <- get_significance_letters(day_model, "Dose..N.kg.ha.")
  letters <- letters %>%
    mutate(level = as.numeric(as.character(level))) # Convert to numeric
  
  # Join significance letters with plot data
  plot_data <- plot_data %>%
    mutate(Dose..N.kg.ha. = as.numeric(Dose..N.kg.ha.)) %>% # Ensure type consistency
    left_join(letters, by = c("Dose..N.kg.ha." = "level"))
  
  # Visualize day data
  day_plot <- ggplot(plot_data, aes(
    x = Dose..N.kg.ha., y = mean_flux,
    fill = Fertilizer_Type, group = interaction(Fertilizer_Type, Plant.type)
  )) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = mean_flux - se_flux, ymax = mean_flux + se_flux), 
                  position = position_dodge(width = 0.9), width = 0.2) +
    geom_text(aes(label = groups), position = position_dodge(width = 0.9), vjust = -0.5) +
    facet_wrap(~ Plant.type, scales = "free_y") +
    labs(
      title = paste("CH4 Flux by Fertilizer Type, Dose, and Plant Type on Day", day),
      x = "Nitrogen Dose (kg/ha)",
      y = "Flux (umol/min/m2)"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1", name = "Fertilizer Type")
  
  CH4_day_plots[[as.character(day)]] <- day_plot # Save plot for later
  print(day_plot) # Print plot
}


# Step 1: Define a function for repeated analysis
analyze_test_name <- function(data, test_name) {
  # Filter data for the specific test name
  test_data <- data %>%
    filter(Test.Name == test_name)
  
  # Store plots for each day
  test_day_plots <- list()
  
  for (day in unique(test_data$Timepoint)) {
    
    # Filter data for the specific day
    day_data <- test_data %>%
      filter(Timepoint == day)
    
    # Fit a linear model for the day
    day_model <- lm(Flux ~ Dose..N.kg.ha. * Fertilizer_Type * Plant.type, data = day_data)
    
    # Residual diagnostics: Residual plot
    residual_plot <- day_data %>%
      mutate(
        Residuals = residuals(day_model),
        Predicted = predict(day_model)
      ) %>%
      ggplot(aes(x = Predicted, y = Residuals)) +
      geom_point(alpha = 0.6, color = "blue") +
      geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
      labs(
        title = paste("Residual Plot for", test_name, "on Day", day),
        x = "Predicted Flux",
        y = "Residuals"
      ) +
      theme_minimal()
    
    print(residual_plot) # Print residual plot
    
    # Residual diagnostics: QQ plot
    qq_plot <- day_data %>%
      mutate(Residuals = residuals(day_model)) %>%
      ggplot(aes(sample = Residuals)) +
      stat_qq() +
      stat_qq_line(color = "red", linetype = "dashed") +
      labs(
        title = paste("QQ Plot for", test_name, "Residuals on Day", day),
        x = "Theoretical Quantiles",
        y = "Sample Quantiles"
      ) +
      theme_minimal()
    
    print(qq_plot) # Print QQ plot
    
    # Calculate group means and standard errors
    plot_data <- day_data %>%
      group_by(Dose..N.kg.ha., Fertilizer_Type, Plant.type) %>%
      summarize(
        mean_flux = mean(Flux, na.rm = TRUE),
        se_flux = sd(Flux, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      )
    
    # Perform Tukey HSD test and add significance letters
    letters <- get_significance_letters(day_model, "Dose..N.kg.ha.")
    letters <- letters %>%
      mutate(level = as.numeric(as.character(level))) # Convert to numeric
    
    # Join significance letters with plot data
    plot_data <- plot_data %>%
      mutate(Dose..N.kg.ha. = as.numeric(Dose..N.kg.ha.)) %>% # Ensure type consistency
      left_join(letters, by = c("Dose..N.kg.ha." = "level"))
    
    # Visualize day data
    day_plot <- ggplot(plot_data, aes(
      x = Dose..N.kg.ha., y = mean_flux,
      fill = Fertilizer_Type, group = interaction(Fertilizer_Type, Plant.type)
    )) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = mean_flux - se_flux, ymax = mean_flux + se_flux), 
                    position = position_dodge(width = 0.9), width = 0.2) +
      geom_text(aes(label = groups), position = position_dodge(width = 0.9), vjust = -0.5) +
      facet_wrap(~ Plant.type, scales = "free_y") +
      labs(
        title = paste(test_name, "Flux by Fertilizer Type, Dose, and Plant Type on Day", day),
        x = "Nitrogen Dose (kg/ha)",
        y = "Flux (umol/min/m2)"
      ) +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1", name = "Fertilizer Type")
    
    test_day_plots[[as.character(day)]] <- day_plot # Save plot for later
    print(day_plot) # Print plot
  }
  
  return(test_day_plots) # Return all day-specific plots for the test name
}

# Step 2: Analyze CO2 and N2O using the function
CO2_day_plots <- analyze_test_name(GHZs_Data_long, "CO2")
N2O_day_plots <- analyze_test_name(GHZs_Data_long, "N2O")




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



GHZs_Data_longer <- GHZs_Data %>%
  pivot_longer(
    cols = c(
      "AF_1stday_flux..umol.min.m2.",
      "AF_3rdday_flux..umol.min.m2.",
      "AF_5thday_flux..umol.min.m2.",
      "AF_7thday_flux..umol.min.m2."
    ),  # Specify the columns explicitly
    names_to = "Timepoint",     # Create a new column named "Timepoint"
    values_to = "Flux"          # Move flux values into a single column
  ) %>%
  mutate(
    Dose = factor(Dose..N.kg.ha., levels = c("0", "100", "200")),  # Rename Dose column to factor
    Fertilizer_Type = factor(Fertilizer_Type, levels = c("None", "MF", "UF")),  # Ensure factor levels
    Plant.type = factor(Plant.type),  # Ensure Plant.type is a factor
    Timepoint = case_when(
      Timepoint == "AF_1stday_flux..umol.min.m2." ~ "Day 1",
      Timepoint == "AF_3rdday_flux..umol.min.m2." ~ "Day 3",
      Timepoint == "AF_5thday_flux..umol.min.m2." ~ "Day 5",
      Timepoint == "AF_7thday_flux..umol.min.m2." ~ "Day 7"
    ),
    Timepoint = factor(Timepoint, levels = c("Day 1", "Day 3", "Day 5", "Day 7"))  # Ensure ordered factor
  )


# Step 2: Reuse the analysis function
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
        title = paste("Residual Plot for", test_name, "on", day),
        x = "Predicted Flux",
        y = "Residuals"
      ) +
      theme_minimal()
    
    qq_plot <- ggplot(day_data, aes(sample = residuals(day_model))) +
      stat_qq() +
      stat_qq_line(color = "red", linetype = "dashed") +
      labs(
        title = paste("QQ Plot for", test_name, "on", day),
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
        title = paste(test_name, "Flux on", day),
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

# Step 3: Analyze and plot for CH4, CO2, and N2O across all days
CH4_day_plots <- analyze_and_plot(GHZs_Data_longer, "CH4")
CO2_day_plots <- analyze_and_plot(GHZs_Data_longer, "CO2")
N2O_day_plots <- analyze_and_plot(GHZs_Data_longer, "N2O")


unique(GHZs_Data_longer$Timepoint)

# Analyze and plot CH4
CH4_day_plots <- analyze_and_plot(GHZs_Data_longer, "CH4")

# Analyze and plot CO2
CO2_day_plots <- analyze_and_plot(GHZs_Data_longer, "CO2")

# Analyze and plot N2O
N2O_day_plots <- analyze_and_plot(GHZs_Data_longer, "N2O")
# CH4 plots for Day 5 and Day 7
CH4_day5_plot <- CH4_day_plots[["Day 5"]]
CH4_day7_plot <- CH4_day_plots[["Day 7"]]

# CO2 plots for Day 5 and Day 7
CO2_day5_plot <- CO2_day_plots[["Day 5"]]
CO2_day7_plot <- CO2_day_plots[["Day 7"]]

# N2O plots for Day 5 and Day 7
N2O_day5_plot <- N2O_day_plots[["Day 5"]]
N2O_day7_plot <- N2O_day_plots[["Day 7"]]
print(CH4_day5_plot)
print(CH4_day7_plot)

print(CO2_day5_plot)
print(CO2_day7_plot)

print(N2O_day5_plot)
print(N2O_day7_plot)
# Save CH4 plots
ggsave("CH4_Day5.png", plot = CH4_day5_plot, width = 8, height = 6)
ggsave("CH4_Day7.png", plot = CH4_day7_plot, width = 8, height = 6)

# Save CO2 plots
ggsave("CO2_Day5.png", plot = CO2_day5_plot, width = 8, height = 6)
ggsave("CO2_Day7.png", plot = CO2_day7_plot, width = 8, height = 6)

# Save N2O plots
ggsave("N2O_Day5.png", plot = N2O_day5_plot, width = 8, height = 6)
ggsave("N2O_Day7.png", plot = N2O_day7_plot, width = 8, height = 6)


colnames(GHZs_Data)


# Filter data for Day 5 and Day 7
GHZs_Data_longer_5_7 <- GHZs_Data %>%
  pivot_longer(
    cols = c(
      "AF_5thday_flux..umol.min.m2.",
      "AF_7thday_flux..umol.min.m2."
    ),  # Select only Day 5 and Day 7 columns
    names_to = "Timepoint",     # Create a new column for timepoint
    values_to = "Flux"          # Move flux values into a single column
  ) %>%
  mutate(
    Dose = factor(Dose..N.kg.ha., levels = c("0", "100", "200")),  # Rename Dose column to factor
    Fertilizer_Type = factor(Fertilizer_Type, levels = c("None", "MF", "UF")),  # Ensure factor levels
    Plant.type = factor(Plant.type),  # Ensure Plant.type is a factor
    Timepoint = case_when(
      Timepoint == "AF_5thday_flux..umol.min.m2." ~ "Day 5",
      Timepoint == "AF_7thday_flux..umol.min.m2." ~ "Day 7"
    ),
    Timepoint = factor(Timepoint, levels = c("Day 5", "Day 7"))  # Ensure ordered factor
  )
# Analyze and plot CH4 for Day 5 and Day 7
CH4_day_plots_5_7 <- analyze_and_plot(GHZs_Data_longer_5_7, "CH4")

# Analyze and plot CO2 for Day 5 and Day 7
CO2_day_plots_5_7 <- analyze_and_plot(GHZs_Data_longer_5_7, "CO2")

# Analyze and plot N2O for Day 5 and Day 7
N2O_day_plots_5_7 <- analyze_and_plot(GHZs_Data_longer_5_7, "N2O")
# Access CH4 plots for Day 5 and Day 7
CH4_day5_plot <- CH4_day_plots_5_7[["Day 5"]]
CH4_day7_plot <- CH4_day_plots_5_7[["Day 7"]]

# Access CO2 plots for Day 5 and Day 7
CO2_day5_plot <- CO2_day_plots_5_7[["Day 5"]]
CO2_day7_plot <- CO2_day_plots_5_7[["Day 7"]]

# Access N2O plots for Day 5 and Day 7
N2O_day5_plot <- N2O_day_plots_5_7[["Day 5"]]
N2O_day7_plot <- N2O_day_plots_5_7[["Day 7"]]









GHZs_Data_long <- GHZs_Data %>%
  pivot_longer(
    cols = c(
      "AF_3rdday_flux..umol.min.m2.",
      "AF_5thday_flux..umol.min.m2.",
      "AF_7thday_flux..umol.min.m2."
    ),  # Include only Day 3, Day 5, and Day 7
    names_to = "Timepoint",     # Create a new column for the timepoints
    values_to = "Flux"          # Move flux values into a single column
  ) %>%
  mutate(
    Dose = factor(Dose..N.kg.ha., levels = c("0", "100", "200")),  # Ensure Dose is a factor
    Fertilizer_Type = factor(Fertilizer_Type, levels = c("None", "MF", "UF")),  # Ensure Fertilizer_Type is a factor
    Plant.type = factor(Plant.type),  # Ensure Plant.type is a factor
    Timepoint = case_when(
      Timepoint == "AF_3rdday_flux..umol.min.m2." ~ "Day 3",
      Timepoint == "AF_5thday_flux..umol.min.m2." ~ "Day 5",
      Timepoint == "AF_7thday_flux..umol.min.m2." ~ "Day 7"
    ),
    Timepoint = factor(Timepoint, levels = c("Day 3", "Day 5", "Day 7"))  # Ensure Timepoint is an ordered factor
  )

analyze_and_plot <- function(data, test_name) {
  # Filter the data for the specific test (CH4, CO2, N2O)
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
        title = paste("Residual Plot for", test_name, "on", day),
        x = "Predicted Flux",
        y = "Residuals"
      ) +
      theme_minimal()
    
    qq_plot <- ggplot(day_data, aes(sample = residuals(day_model))) +
      stat_qq() +
      stat_qq_line(color = "red", linetype = "dashed") +
      labs(
        title = paste("QQ Plot for", test_name, "on", day),
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
        title = paste(test_name, "Flux on", day),
        x = "Nitrogen Dose (kg/ha)",
        y = "Flux (umol/min/m2)",
        fill = "Fertilizer"
      ) +
      facet_wrap(~ Plant.type) +
      scale_fill_manual(values = c("UF" = "black", "MF" = "white", "None" = "grey")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Store plot
    test_day_plots[[as.character(day)]] <- list(
      Residual_Plot = residual_plot,
      QQ_Plot = qq_plot,
      Bar_Plot = day_plot
    )
    print(day_plot)  # Print bar plot
  }
  
  return(test_day_plots)
}
# Analyze and plot CH4
CH4_day_plots <- analyze_and_plot(GHZs_Data_long, "CH4")

# Analyze and plot CO2
CO2_day_plots <- analyze_and_plot(GHZs_Data_long, "CO2")

# Analyze and plot N2O
N2O_day_plots <- analyze_and_plot(GHZs_Data_long, "N2O")


# Filter and reshape data for Day 7
GHZs_Data_day7 <- GHZs_Data %>%
  pivot_longer(
    cols = "AF_7thday_flux..umol.min.m2.",  # Include only Day 7
    names_to = "Timepoint",                # Create a new column for the timepoint
    values_to = "Flux"                     # Move flux values into a single column
  ) %>%
  mutate(
    Dose = factor(Dose..N.kg.ha., levels = c("0", "100", "200")),  # Ensure Dose is a factor
    Fertilizer_Type = factor(Fertilizer_Type, levels = c("None", "MF", "UF")),  # Ensure Fertilizer_Type is a factor
    Plant.type = factor(Plant.type),  # Ensure Plant.type is a factor
    Timepoint = "Day 7"  # Set the timepoint explicitly
  )

analyze_and_plot_day7 <- function(data, test_name) {
  # Filter the data for the specific test (CH4, CO2, N2O)
  test_data <- data %>%
    filter(Test.Name == test_name)
  
  # Store plots
  plots <- list()
  
  # Calculate means and standard errors for each group
  biomass_summary <- test_data %>%
    group_by(Dose, Plant.type, Fertilizer_Type) %>%
    summarize(
      Mean_Flux = mean(Flux, na.rm = TRUE),
      SE_Flux = sd(Flux, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  # Fit linear model
  model <- lm(Flux ~ Dose * Fertilizer_Type * Plant.type - Dose:Fertilizer_Type:Plant.type, data = test_data)
  
  # Residual diagnostics: Residual Plot
  residual_plot <- ggplot(test_data, aes(x = predict(model), y = residuals(model))) +
    geom_point(alpha = 0.6, color = "blue") +
    geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
    labs(
      title = paste("Residual Plot for", test_name, "on Day 7"),
      x = "Predicted Flux",
      y = "Residuals"
    ) +
    theme_minimal()
  
  # Residual diagnostics: QQ Plot
  qq_plot <- ggplot(test_data, aes(sample = residuals(model))) +
    stat_qq() +
    stat_qq_line(color = "red", linetype = "dashed") +
    labs(
      title = paste("QQ Plot for", test_name, "on Day 7"),
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    theme_minimal()
  
  # Print residual diagnostics
  print(residual_plot)
  print(qq_plot)
  
  # Perform ANOVA (Type II) and Tukey HSD test
  print(Anova(model, type = "II"))  # Print ANOVA results
  
  pairwise_comparisons <- emmeans(model, ~ Dose + Fertilizer_Type | Plant.type)
  letters <- cld(pairwise_comparisons, Letters = letters) %>%
    mutate(.group = stringr::str_trim(.group)) %>%
    as.data.frame()
  
  # Merge letters with summary data
  biomass_summary <- biomass_summary %>%
    left_join(letters, by = c("Dose", "Fertilizer_Type", "Plant.type"))
  
  # Create bar plot with group comparisons
  bar_plot <- ggplot(biomass_summary, aes(x = Dose, y = Mean_Flux, fill = Fertilizer_Type)) +
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
      title = paste(test_name, "Flux on Day 7"),
      x = "Nitrogen Dose (kg/ha)",
      y = "Flux (umol/min/m2)",
      fill = "Fertilizer"
    ) +
    facet_wrap(~ Plant.type) +
    scale_fill_manual(values = c("UF" = "black", "MF" = "white", "None" = "grey")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Store plots
  plots$residual_plot <- residual_plot
  plots$qq_plot <- qq_plot
  plots$bar_plot <- bar_plot
  
  # Print the bar plot
  print(bar_plot)
  
  return(plots)
}
# Analyze and plot CH4 for Day 7
CH4_day7_plots <- analyze_and_plot_day7(GHZs_Data_day7, "CH4")

# Analyze and plot CO2 for Day 7
CO2_day7_plots <- analyze_and_plot_day7(GHZs_Data_day7, "CO2")

# Analyze and plot N2O for Day 7
N2O_day7_plots <- analyze_and_plot_day7(GHZs_Data_day7, "N2O")






# Reshape data to include all time points
GHZs_Data_longer <- GHZs_Data %>%
  pivot_longer(
    cols = c(
      "BF.flux..umol.min.m2.",
      "AF_1stday_flux..umol.min.m2.",
      "AF_3rdday_flux..umol.min.m2.",
      "AF_5thday_flux..umol.min.m2.",
      "AF_7thday_flux..umol.min.m2."
    ),  # Include all flux time points
    names_to = "Timepoint",        # New column for timepoints
    values_to = "Flux"             # New column for flux values
  ) %>%
  mutate(
    Dose = factor(Dose..N.kg.ha., levels = c("0", "100", "200")),  # Ensure Dose is a factor
    Fertilizer_Type = factor(Fertilizer_Type, levels = c("None", "MF", "UF")),  # Ensure Fertilizer_Type is a factor
    Plant.type = factor(Plant.type),  # Ensure Plant.type is a factor
    Timepoint = case_when(
      Timepoint == "BF.flux..umol.min.m2." ~ 0,
      Timepoint == "AF_1stday_flux..umol.min.m2." ~ 1,
      Timepoint == "AF_3rdday_flux..umol.min.m2." ~ 3,
      Timepoint == "AF_5thday_flux..umol.min.m2." ~ 5,
      Timepoint == "AF_7thday_flux..umol.min.m2." ~ 7
    ),
    Timepoint = as.numeric(Timepoint)  # Convert timepoints to numeric for plotting
  )
#Step 2: Define a Function to Create the Trend Plot
plot_flux_trend <- function(data, test_name) {
  # Filter data for the specific test
  test_data <- data %>%
    filter(Test.Name == test_name)
  
  # Calculate mean and standard error for each group
  summary_data <- test_data %>%
    group_by(Timepoint, Dose, Fertilizer_Type, Plant.type) %>%
    summarize(
      Mean_Flux = mean(Flux, na.rm = TRUE),
      SE_Flux = sd(Flux, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  # Plot the trend with standard errors and smoothing lines
  trend_plot <- ggplot(summary_data, aes(x = Timepoint, y = Mean_Flux, color = Fertilizer_Type, group = interaction(Dose, Fertilizer_Type))) +
    geom_line(aes(linetype = Dose), size = 1) +  # Add trend lines for each group
    geom_point(size = 2) +  # Add points for each group
    geom_errorbar(aes(ymin = Mean_Flux - SE_Flux, ymax = Mean_Flux + SE_Flux), width = 0.2) +  # Add error bars
    geom_smooth(aes(group = interaction(Dose, Fertilizer_Type)), method = "loess", se = FALSE, linetype = "dashed", size = 0.8) +  # Add smoothing line
    labs(
      title = paste("Flux Trends for", test_name),
      x = "Timepoint (Days)",
      y = "Flux (umol/min/m2)",
      color = "Fertilizer Type",
      linetype = "Dose"
    ) +
    facet_wrap(~ Plant.type, scales = "free_y") +  # Facet by Plant.type
    scale_color_manual(values = c("UF" = "black", "MF" = "blue", "None" = "red")) +  # Custom colors
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  return(trend_plot)
}
#Step 3: Generate Plots for Each Test Name
# Generate plot for CH4
CH4_trend_plot <- plot_flux_trend(GHZs_Data_longer, "CH4")
print(CH4_trend_plot)

# Generate plot for CO2
CO2_trend_plot <- plot_flux_trend(GHZs_Data_longer, "CO2")
print(CO2_trend_plot)

# Generate plot for N2O
N2O_trend_plot <- plot_flux_trend(GHZs_Data_longer, "N2O")
print(N2O_trend_plot)


#Updated Code for a Clearer Visualization

plot_flux_trend_simplified <- function(data, test_name) {
  # Filter data for the specific test
  test_data <- data %>%
    filter(Test.Name == test_name)
  
  # Calculate mean and standard error for each group
  summary_data <- test_data %>%
    group_by(Timepoint, Dose, Fertilizer_Type, Plant.type) %>%
    summarize(
      Mean_Flux = mean(Flux, na.rm = TRUE),
      SE_Flux = sd(Flux, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  # Plot the trend with separate horizontal panels for Fertilizer_Type
  trend_plot <- ggplot(summary_data, aes(x = Timepoint, y = Mean_Flux, color = Dose, group = Dose)) +
    geom_line(size = 1) +  # Add trend lines for each Dose
    geom_point(size = 2) +  # Add points for each Dose
    geom_errorbar(aes(ymin = Mean_Flux - SE_Flux, ymax = Mean_Flux + SE_Flux), width = 0.2) +  # Add error bars
    facet_grid(Fertilizer_Type ~ Plant.type, scales = "free_y") +  # Separate by Fertilizer Type (rows) and Plant.type (columns)
    labs(
      title = paste("Flux Trends for", test_name),
      x = "Timepoint (Days)",
      y = "Flux (umol/min/m2)",
      color = "Dose (kg/ha)"
    ) +
    scale_color_manual(values = c("black", "blue", "red")) +  # Custom colors for Dose
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text.y = element_text(angle = 0, size = 10)  # Adjust panel labels for readability
    )
  
  return(trend_plot)
}
# Generate plot for CH4
CH4_trend_plot_simplified <- plot_flux_trend_simplified(GHZs_Data_longer, "CH4")
print(CH4_trend_plot_simplified)

# Generate plot for CO2
CO2_trend_plot_simplified <- plot_flux_trend_simplified(GHZs_Data_longer, "CO2")
print(CO2_trend_plot_simplified)

# Generate plot for N2O
N2O_trend_plot_simplified <- plot_flux_trend_simplified(GHZs_Data_longer, "N2O")
print(N2O_trend_plot_simplified)



plot_flux_trend_with_borders <- function(data, test_name) {
  # Filter data for the specific test
  test_data <- data %>%
    filter(Test.Name == test_name)
  
  # Calculate mean and standard error for each group
  summary_data <- test_data %>%
    group_by(Timepoint, Dose, Fertilizer_Type, Plant.type) %>%
    summarize(
      Mean_Flux = mean(Flux, na.rm = TRUE),
      SE_Flux = sd(Flux, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  # Plot the trend with separate horizontal panels for Fertilizer_Type
  trend_plot <- ggplot(summary_data, aes(x = Timepoint, y = Mean_Flux, color = Dose, group = Dose)) +
    geom_line(size = 1) +  # Add trend lines for each Dose
    geom_point(size = 2) +  # Add points for each Dose
    geom_errorbar(aes(ymin = Mean_Flux - SE_Flux, ymax = Mean_Flux + SE_Flux), width = 0.2) +  # Add error bars
    facet_grid(Fertilizer_Type ~ Plant.type, scales = "free_y") +  # Separate by Fertilizer_Type and Plant.type
    labs(
      title = paste("Flux Trends for", test_name),
      x = "Timepoint (Days)",
      y = "Flux (umol/min/m2)",
      color = "Dose (kg/ha)"
    ) +
    scale_color_manual(values = c("black", "blue", "red")) +  # Custom colors for Dose
    scale_x_continuous(
      breaks = seq(0, 7, 1)  # Ensure all days (0 to 7) appear on the x-axis
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),  # Align x-axis text
      axis.text.y = element_text(size = 10),
      axis.title.x = element_text(face = "bold", size = 12),
      axis.title.y = element_text(face = "bold", size = 12),
      axis.line.x = element_line(color = "black", size = 1),  # Bold x-axis line
      panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add border around plot
      strip.text.y = element_text(angle = 0, size = 10)  # Adjust panel labels for readability
    )
  
  return(trend_plot)
}

# Generate plot for CH4
CH4_trend_plot_bordered <- plot_flux_trend_with_borders(GHZs_Data_longer, "CH4")
print(CH4_trend_plot_bordered)

# Generate plot for CO2
CO2_trend_plot_bordered <- plot_flux_trend_with_borders(GHZs_Data_longer, "CO2")
print(CO2_trend_plot_bordered)

# Generate plot for N2O
N2O_trend_plot_bordered <- plot_flux_trend_with_borders(GHZs_Data_longer, "N2O")
print(N2O_trend_plot_bordered)

