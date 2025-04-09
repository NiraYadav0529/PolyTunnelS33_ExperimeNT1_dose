# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(agricolae)
colnames(ph_ec_data)
# Reshape data to long format for pH values
ph_long <- ph_ec_data %>%
  dplyr::select(Plant.type, Fertilizer.type, Dose..N.kg.ha.,
         pH_beforefertilization, pH_afterFertilization_7thday,
         pH_Afterfertilization_30days, pH.Harvest.day_after2ndfertilization_60thday) %>%
  pivot_longer(
    cols = c(pH_beforefertilization, pH_afterFertilization_7thday,
             pH_Afterfertilization_30days, pH.Harvest.day_after2ndfertilization_60thday),
    names_to = "Time",
    values_to = "pH"
  ) %>%
  mutate(
    Dose = factor(Dose..N.kg.ha., levels = c("0", "100", "200")),  # Convert Dose to factor
    Fertilizer_Type = factor(Fertilizer.type, levels = c("None", "MF", "UF")),  # Fertilizer type to factor
    Plant.type = factor(Plant.type),  # Convert Plant type to factor
    Time = factor(Time,  # Rename and order time points
                  levels = c("pH_beforefertilization",
                             "pH_afterFertilization_7thday",
                             "pH_Afterfertilization_30days",
                             "pH.Harvest.day_after2ndfertilization_60thday"),
                  labels = c("Before Fertilization", "7 Days", "30 Days", "60 Days"))
  )

# Calculate mean and SE for each group
ph_summary <- ph_long %>%
  group_by(Time, Dose, Plant.type, Fertilizer_Type) %>%
  summarise(
    Mean_pH = mean(pH, na.rm = TRUE),
    SE_pH = sd(pH, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()
# Fit ANOVA model
anova_result_ph <- aov(pH ~ Time * Dose * Fertilizer_Type * Plant.type, data = ph_long)
summary(anova_result_ph)

# Perform Tukey's HSD test for pH across Time
tukey_ph <- HSD.test(anova_result_ph, "Time", group = TRUE)

# Extract Tukey's groups (letters)
tukey_letters <- tukey_ph$groups
tukey_letters <- tukey_letters[order(rownames(tukey_letters)), ]  # Ensure correct ordering
tukey_letters$Time <- rownames(tukey_letters)  # Add Time column

# Merge Tukey letters with summary data
ph_summary <- ph_summary %>%
  left_join(tukey_letters, by = "Time") %>%
  rename(Significance = groups)

# pH Bar Plot with Error Bars and Tukey Letters
ggplot(ph_summary, aes(x = Time, y = Mean_pH, fill = Dose)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), colour = "black") +
  geom_errorbar(aes(ymin = Mean_pH - SE_pH, ymax = Mean_pH + SE_pH),
                width = 0.2, position = position_dodge(0.8)) +
  geom_text(aes(y = Mean_pH + SE_pH + 0.1, label = Significance),
            position = position_dodge(0.8), vjust = -0.5) +
  facet_wrap(~ Fertilizer_Type * Plant.type) +
  labs(title = "pH Across Different Time Points", 
       x = "Time (Days)", y = "pH", fill = "Dose (N kg/ha)") +
  scale_fill_manual(values = c("0" = "orange", "100" = "purple", "200" = "green")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
  )

library(ggplot2)
library(dplyr)

# Modify the plot to use black and white styling
ggplot(ph_summary, aes(x = Time, y = Mean_pH, fill = Dose)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
           colour = "black", show.legend = TRUE, width = 0.6) +
  geom_errorbar(aes(ymin = Mean_pH - SE_pH, ymax = Mean_pH + SE_pH), 
                position = position_dodge(width = 0.8), 
                width = 0.2, colour = "black") +
  geom_text(aes(y = Mean_pH + SE_pH + 0.05, label = Significance), 
            position = position_dodge(width = 0.8), vjust = -0.5, size = 3.5) +
  facet_grid(Plant.type ~ Fertilizer_Type, 
             labeller = labeller(Fertilizer_Type = c("None" = "None", "MF" = "MF", "UF" = "UF"),
                                 Plant.type = c("L" = "Lucerne", "P" = "Phalaris"))) +
  labs(title = "pH Across Different Time Points by Plant Species", 
       x = "Time", y = "pH", fill = "Dose (N kg/ha)") +
  scale_fill_manual(values = c("0" = "white", "100" = "grey50", "200" = "black"),
                    guide = guide_legend(override.aes = list(colour = "black", size = 4))) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    panel.grid.major = element_line(colour = "grey80"),
    panel.grid.minor = element_blank()
  )

# Reorder the time points
ph_summary <- ph_summary %>%
  mutate(
    Time = factor(Time, levels = c("Before Fertilization", "7 Days", "30 Days", "60 Days")) # Desired order
  )

# Remove duplicate Tukey letters (optional: check and clean your data)
ph_summary <- ph_summary %>%
  distinct(Time, Treatment, .keep_all = TRUE)  # Ensure only unique letters per treatment

# Updated Plot with Reordered Time Points
ggplot(ph_summary, aes(x = Time, y = Mean_pH, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), colour = "black") +
  geom_errorbar(aes(ymin = Mean_pH - SE_pH, ymax = Mean_pH + SE_pH), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  # Add Tukey's letters with adjusted positioning and no duplication
  geom_text(aes(y = Mean_pH + SE_pH + 0.15, label = Significance), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  labs(title = "pH Across Different Time Points by Treatment and Dose", 
       x = "Time (Days)", y = "pH", fill = "Treatment") +
  scale_fill_manual(values = c("None 0" = "white", 
                               "UF 100" = "grey70", "UF 200" = "black", 
                               "MF 100" = "grey50", "MF 200" = "darkgrey")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    panel.grid.major = element_line(colour = "grey80"),
    panel.grid.minor = element_blank()
  )


# Updated Plot with Colors and Improved Y-Axis Grid
ggplot(ph_summary, aes(x = Time, y = Mean_pH, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), colour = "black") +
  geom_errorbar(aes(ymin = Mean_pH - SE_pH, ymax = Mean_pH + SE_pH), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  # Add Tukey's letters with adjusted positioning
  geom_text(aes(y = Mean_pH + SE_pH + 0.15, label = Significance), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  labs(title = "pH Across Different Time Points by Treatment and Dose", 
       x = "Time (Days)", y = "pH", fill = "Treatment") +
  scale_fill_manual(values = c("None 0" = "#FFC107",  # Vibrant yellow
                               "UF 100" = "#03A9F4",  # Bright blue
                               "UF 200" = "#673AB7",  # Deep purple
                               "MF 100" = "#4CAF50",  # Bright green
                               "MF 200" = "#E91E63")) +  # Pink
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    panel.grid.major = element_line(colour = "grey90"),  # Light gridlines
    panel.grid.minor = element_blank()
  ) +
  # Ensure y-axis has all numbers (e.g., integers)
  scale_y_continuous(breaks = seq(0, 7, by = 1))  # Adjust ticks for clarity



# Reshape EC data to long format
ec_long <- ph_ec_data %>%
  select(Plant.type, Fertilizer.type, Dose..N.kg.ha.,
         EC_beforefertilization, EC_Afterfertilization_30days, EC.harvest.day) %>%
  pivot_longer(
    cols = c(EC_beforefertilization, EC_Afterfertilization_30days, EC.harvest.day),
    names_to = "Time",
    values_to = "EC"
  ) %>%
  mutate(
    Dose = factor(Dose..N.kg.ha., levels = c("0", "100", "200")),  # Convert Dose to factor
    Fertilizer_Type = factor(Fertilizer.type, levels = c("None", "MF", "UF")),  # Convert Fertilizer.type to factor
    Plant.type = factor(Plant.type),  # Convert Plant type to factor
    Time = factor(Time,  # Rename and order time points
                  levels = c("EC_beforefertilization", "EC_Afterfertilization_30days", "EC.harvest.day"),
                  labels = c("Before Fertilization", "30 Days", "60 Days"))
  )

# Calculate mean and SE for each group
ec_summary <- ec_long %>%
  group_by(Time, Dose, Plant.type, Fertilizer_Type) %>%
  summarise(
    Mean_EC = mean(EC, na.rm = TRUE),
    SE_EC = sd(EC, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup() %>%
  mutate(Treatment = interaction(Fertilizer_Type, Dose, sep = " "))  # Combine Treatment and Dose

# Perform ANOVA for EC
anova_result_ec <- aov(EC ~ Time * Dose * Fertilizer_Type * Plant.type, data = ec_long)
summary(anova_result_ec)

# Tukey's HSD test for EC
library(agricolae)
tukey_ec <- HSD.test(anova_result_ec, "Time", group = TRUE)

# Extract Tukey's letters
tukey_letters_ec <- tukey_ec$groups
tukey_letters_ec <- tukey_letters_ec[order(rownames(tukey_letters_ec)), ]
tukey_letters_ec$Time <- rownames(tukey_letters_ec)

# Merge Tukey letters with EC summary
ec_summary <- ec_summary %>%
  left_join(tukey_letters_ec, by = "Time") %>%
  rename(Significance = groups)

# Bar Plot for EC with Similar Visualization
ggplot(ec_summary, aes(x = Time, y = Mean_EC, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), colour = "black") +
  geom_errorbar(aes(ymin = Mean_EC - SE_EC, ymax = Mean_EC + SE_EC), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  # Add Tukey's letters with adjusted positioning
  geom_text(aes(y = Mean_EC + SE_EC + 0.1, label = Significance), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  labs(title = "Electrical Conductivity (EC) Across Different Time Points by Treatment and Dose", 
       x = "Time (Days)", y = "EC (dS/m)", fill = "Treatment") +
  scale_fill_manual(values = c("None 0" = "#FFC107", 
                               "UF 100" = "#03A9F4", "UF 200" = "#673AB7", 
                               "MF 100" = "#4CAF50", "MF 200" = "#E91E63")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    panel.grid.major = element_line(colour = "grey90"),  # Light gridlines
    panel.grid.minor = element_blank()
  ) +
  # Y-axis with evenly spaced ticks
  scale_y_continuous(breaks = seq(0, max(ec_summary$Mean_EC + ec_summary$SE_EC, na.rm = TRUE), by = 0.5))


# Remove duplicate Tukey letters
ec_summary <- ec_summary %>%
  distinct(Time, Treatment, .keep_all = TRUE)  # Ensure only unique letters per treatment

# Updated Plot
ggplot(ec_summary, aes(x = Time, y = Mean_EC, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), colour = "black") +
  geom_errorbar(aes(ymin = Mean_EC - SE_EC, ymax = Mean_EC + SE_EC), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  # Add Tukey's letters with adjusted positioning and no duplicates
  geom_text(aes(y = Mean_EC + SE_EC + 0.05, label = Significance), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  labs(title = "Electrical Conductivity (EC) Across Different Time Points by Treatment and Dose", 
       x = "Time (Days)", y = "EC (dS/m)", fill = "Treatment") +
  scale_fill_manual(values = c("None 0" = "#FFC107", 
                               "UF 100" = "#03A9F4", "UF 200" = "#673AB7", 
                               "MF 100" = "#4CAF50", "MF 200" = "#E91E63")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    panel.grid.major = element_line(colour = "grey80"),  # Subtle gridlines for better clarity
    panel.grid.minor = element_line(colour = "grey90")   # Minor gridlines for better y-axis values
  ) +
  # Adjust y-axis ticks to display more values
  scale_y_continuous(
    breaks = seq(0, max(ec_summary$Mean_EC + ec_summary$SE_EC, na.rm = TRUE), by = 0.1)
  )



# Load necessary libraries
library(dplyr)
library(ggplot2)
library(emmeans)
library(multcompView)

# Reading the pH and EC data (assuming CSV is already loaded as 'ph_ec_data')
colnames(ph_ec_data)

# Ensure necessary columns are treated as factors for pH and EC data
ph_ec_data <- ph_ec_data %>%
  mutate(
    Dose = factor(Dose..N.kg.ha., levels = c("0", "100", "200")),  # Convert Dose column to factor
    Fertilizer_Type = factor(Fertilizer.type, levels = c('None', 'MF', 'UF')),  # Convert Fertilizer.type to Fertilizer_Type
    Plant.type = case_match(Plant.type, 'L' ~ 'Lucerne', 'P' ~ 'Phalaris'),  # Rename levels for labeling facets
    Plant.type = factor(Plant.type)  # Ensure Plant.type is a factor
  )

# Check structure of the dataset
ph_ec_data %>% 
  group_by(Plant.type, Fertilizer_Type, Dose) %>% 
  summarise(n())

# Restructure the pH data for analysis, focusing on the 90th Day (Harvest Day)
ph_long_90 <- ph_ec_data %>%
  pivot_longer(cols = c(pH.Harvest.day_after2ndfertilization_60thday), 
               names_to = "Time_Point", values_to = "pH") %>%
  mutate(Time_Point = factor("90th Day")) %>%
  mutate(pH = as.numeric(pH))  # Ensure pH is numeric

# Restructure the EC data for analysis, focusing on the 90th Day (Harvest Day)
ec_long_90 <- ph_ec_data %>%
  pivot_longer(cols = c(EC.harvest.day), 
               names_to = "Time_Point", values_to = "EC") %>%
  mutate(Time_Point = factor("90th Day")) %>%
  mutate(EC = as.numeric(EC))  # Ensure EC is numeric

# Summarize the pH data for 90th Day
pH_summary_90 <- ph_long_90 %>%
  group_by(Dose, Plant.type, Fertilizer_Type, Time_Point) %>%
  summarise(
    Mean_pH = mean(pH, na.rm = TRUE),
    SE_pH = sd(pH, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()

# Summarize the EC data for 90th Day
EC_summary_90 <- ec_long_90 %>%
  group_by(Dose, Plant.type, Fertilizer_Type, Time_Point) %>%
  summarise(
    Mean_EC = mean(EC, na.rm = TRUE),
    SE_EC = sd(EC, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()

### pH Data Analysis (90th Day) ###
# Model for pH data
m1_ph_90 <- lm(pH ~ Dose * Fertilizer_Type * Plant.type, data = ph_long_90)
summary(m1_ph_90)

# Check residuals for pH data
residualPlot(m1_ph_90)
qqPlot(m1_ph_90)

# Test significance of the model effects for pH data
Anova(m1_ph_90, type = "II")

# Perform multiple comparison test (Tukey HSD) for pH data
pairwise_comparisons_ph_90 <- emmeans(m1_ph_90, ~ Dose + Fertilizer_Type | Plant.type)
letters_ph_90 <- cld(pairwise_comparisons_ph_90, Letters = letters) %>%
  mutate(.group = stringr::str_trim(.group)) %>%
  as.data.frame()

# Merge the letters for pH data with pH_summary_90 using Dose and Plant.type
pH_summary_90 <- pH_summary_90 %>%
  left_join(letters_ph_90, by = c("Dose", "Fertilizer_Type", "Plant.type"))

### EC Data Analysis (90th Day) ###
# Model for EC data
m1_ec_90 <- lm(EC ~ Dose * Fertilizer_Type * Plant.type, data = ec_long_90)
summary(m1_ec_90)

# Check residuals for EC data
residualPlot(m1_ec_90)
qqPlot(m1_ec_90)

# Test significance of the model effects for EC data
Anova(m1_ec_90, type = "II")

# Perform multiple comparison test (Tukey HSD) for EC data
pairwise_comparisons_ec_90 <- emmeans(m1_ec_90, ~ Dose + Fertilizer_Type | Plant.type)
letters_ec_90 <- cld(pairwise_comparisons_ec_90, Letters = letters) %>%
  mutate(.group = stringr::str_trim(.group)) %>%
  as.data.frame()

# Merge the letters for EC data with EC_summary_90 using Dose and Plant.type
EC_summary_90 <- EC_summary_90 %>%
  left_join(letters_ec_90, by = c("Dose", "Fertilizer_Type", "Plant.type"))

# Custom colorblind-friendly palette
colorblind_palette <- c("None" = "black", "MF" = "#0072B2", "UF" = "#D55E00")

# Shapes for different doses
shapes <- c("0" = 19, "100" = 21, "200" = 23)

# Line types for fertilizer types
line_types <- c("None" = "solid", "MF" = "dashed", "UF" = "dotted")

### pH Plot for 90th Day ###
pH_plot_90 <- ggplot(pH_summary_90, aes(x = Dose, y = Mean_pH, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), 
           colour = "black") +
  geom_errorbar(aes(ymin = Mean_pH - SE_pH, ymax = Mean_pH + SE_pH), 
                width = 0.2, position = position_dodge2(0.9)) +
  labs(title = "pH 90th Day (Harvest Day)", 
       x = "Dose (N kg/ha)", 
       y = "pH (+/- SE)", 
       fill = 'Fertilizer') +
  facet_wrap(~ Plant.type) +
  scale_fill_manual(values = colorblind_palette) +
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
  
  # Add compact letter display (CLD) text on top of bars
  geom_text(aes(y = Mean_pH + SE_pH + 0.1, label = .group), 
            position = position_dodge2(0.9, preserve = 'single'))

### EC Plot for 90th Day ###
EC_plot_90 <- ggplot(EC_summary_90, aes(x = Dose, y = Mean_EC, fill = Fertilizer_Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = 'single'), 
           colour = "black") +
  geom_errorbar(aes(ymin = Mean_EC - SE_EC, ymax = Mean_EC + SE_EC), 
                width = 0.2, position = position_dodge2(0.9)) +
  labs(title = "EC 90th Day (Harvest Day)", 
       x = "Dose (N kg/ha)", 
       y = "EC (+/- SE)", 
       fill = 'Fertilizer') +
  facet_wrap(~ Plant.type) +
  scale_fill_manual(values = colorblind_palette) +
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
  
  # Add compact letter display (CLD) text on top of bars
  geom_text(aes(y = Mean_EC + SE_EC + 0.1, label = .group), 
            position = position_dodge2(0.9, preserve = 'single'))

# Print the plots
print(pH_plot_90)
print(EC_plot_90)




# Custom black, white, and grey color palette for Fertilizer Type
black_white_grey_palette <- c("None" = "grey", "MF" = "white", "UF" = "black")

# Set the dodge position (so both bars and error bars are aligned the same)
dodge_position <- position_dodge(width = 0.9)  # Adjust the dodge width

### pH Plot for 90th Day with Dose 0 Width Fixed ###
pH_plot_90_fixed <- ggplot(pH_summary_90, aes(x = Dose, y = Mean_pH, fill = Fertilizer_Type)) +
  # Adjusting bar width for all doses, especially for Dose 0
  geom_bar(stat = "identity", position = dodge_position, colour = "black", 
           aes(width = ifelse(Dose == 0, 0.45, 0.7))) +  # Adjusting Dose 0 to half the width
  geom_errorbar(aes(ymin = Mean_pH - SE_pH, ymax = Mean_pH + SE_pH), 
                width = 0.2, position = dodge_position, color = "black") +  # Error bars properly aligned
  labs(title = "pH Harvest Day", 
       x = "Dose (N kg/ha)", 
       y = "pH (+/- SE)", 
       fill = 'Fertilizer') +
  facet_wrap(~ Plant.type) +
  scale_fill_manual(values = black_white_grey_palette) +  # Use black, white, grey for bar fill
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
  
  # Add compact letter display (CLD) text on top of bars
  geom_text(aes(y = Mean_pH + SE_pH + 0.1, label = .group), 
            position = dodge_position)  # Adjust text position above bars

### EC Plot for 90th Day with Dose 0 Width Fixed ###
EC_plot_90_fixed <- ggplot(EC_summary_90, aes(x = Dose, y = Mean_EC, fill = Fertilizer_Type)) +
  # Adjusting bar width for all doses, especially for Dose 0
  geom_bar(stat = "identity", position = dodge_position, colour = "black", 
           aes(width = ifelse(Dose == 0, 0.45, 0.7))) +  # Adjusting Dose 0 to half the width
  geom_errorbar(aes(ymin = Mean_EC - SE_EC, ymax = Mean_EC + SE_EC), 
                width = 0.2, position = dodge_position, color = "black") +  # Error bars properly aligned
  labs(title = "EC Harvest Day", 
       x = "Dose (N kg/ha)", 
       y = "EC (+/- SE)", 
       fill = 'Fertilizer') +
  facet_wrap(~ Plant.type) +
  scale_fill_manual(values = black_white_grey_palette) +  # Use black, white, grey for bar fill
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
  
  # Add compact letter display (CLD) text on top of bars
  geom_text(aes(y = Mean_EC + SE_EC + 0.1, label = .group), 
            position = dodge_position)  # Adjust text position above bars

# Print the updated plots
print(pH_plot_90_fixed)
print(EC_plot_90_fixed)

