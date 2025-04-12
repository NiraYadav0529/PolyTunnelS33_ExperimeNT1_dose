setwd("C:/Users/90958427/OneDrive - Western Sydney University/PolyTunnelS33_ExperimeNT1_dose/Combined 1& 2 Analysis")
list.files()
ph_ec_exp1 <- read.csv("ph_ec_exp1.csv")
colnames(ph_ec_exp1)
## Exp 1 , lucern first 
# Reshape the pH columns into long format using your actual column names
ph_long <- ph_ec_exp1 %>%
  select(
    Plant.type = Plant_type,
    Fertilizer = Fertilizer_type,
    Dose = Dose_.N.kg.ha.,
    Rep = S.No.for.plant,
    pH_before = pH_BF_0days,
    pH_day7 = pH_AF_7thdays,
    pH_day30 = pH_AF_30thdays,
    pH_harvest = pH_HD_140days
  ) %>%
  pivot_longer(cols = starts_with("pH_"),
               names_to = "Time",
               values_to = "pH") %>%
  mutate(
    Time = recode(Time,
                  pH_before = "Day0",
                  pH_day7 = "Day7",
                  pH_day30 = "Day30",
                  pH_harvest = "Harvest"),
    Time = factor(Time, levels = c("Day0", "Day7", "Day30", "Harvest")),
    Plant.type = factor(Plant.type, levels = c("L", "P"), labels = c("Lucerne", "Phalaris")),
    Fertilizer = factor(Fertilizer, levels = c("UF", "MF", "None"), labels = c("UrVAL", "Mineral Fertilizer", "Control"))
  )
# Run for Lucerne only (repeat for Phalaris)
lucerne_ph <- ph_long %>% filter(Plant.type == "Lucerne")

# Repeated measures mixed model
mod_lucerne <- lmer(pH ~ Fertilizer * Dose * Time + (1|Rep), data = lucerne_ph)

# ANOVA table
anova(mod_lucerne)

# Post-hoc comparisons
emm <- emmeans(mod_lucerne, ~ Fertilizer * Dose * Time)
pairs(emm, simple = "each", adjust = "tukey")


# Plot for Lucerne (repeat for Phalaris)
ggplot(lucerne_ph, aes(x = Time, y = pH, group = interaction(Fertilizer, Dose), color = Fertilizer, linetype = as.factor(Dose))) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "Soil pH over Time (Lucerne)",
       y = "Soil pH",
       color = "Fertilizer Type",
       linetype = "N Dose (kg/ha)") +
  theme_minimal()

# Set up the theme and colorless styling
ggplot(lucerne_ph, aes(x = Time, y = pH, 
                       group = interaction(Fertilizer, Dose), 
                       shape = Fertilizer, 
                       linetype = as.factor(Dose))) +
  stat_summary(fun = mean, geom = "line", size = 1, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, color = "black") +
  stat_summary(fun = mean, geom = "point", size = 3, fill = "white") +
  labs(title = "Soil pH over Time - Lucerne (Exp 1)",
       y = "Soil pH",
       x = "Time Point",
       shape = "Fertilizer Type",
       linetype = "N Dose (kg/ha)") +
  theme_minimal() +
  theme(legend.position = "right")

library(dplyr)

# Summary of mean ± SD
lucerne_summary <- lucerne_ph %>%
  group_by(Fertilizer, Dose, Time) %>%
  summarise(
    Mean_pH = round(mean(pH, na.rm = TRUE), 2),
    SD_pH = round(sd(pH, na.rm = TRUE), 2),
    n = n()
  )

print(lucerne_summary)
# Three-way ANOVA
lucerne_aov <- aov(pH ~ Fertilizer * Dose * Time, data = lucerne_ph)
summary(lucerne_aov)
# Load emmeans if not already loaded
library(emmeans)

# Post-hoc comparison
lucerne_emm <- emmeans(lucerne_aov, ~ Fertilizer * Dose * Time)
pairs(lucerne_emm, adjust = "tukey")

##for phalaris type exp 1
phalaris_ph <- ph_long %>% filter(Plant.type == "Phalaris")

phalaris_summary <- phalaris_ph %>%
  group_by(Fertilizer, Dose, Time) %>%
  summarise(
    Mean_pH = round(mean(pH, na.rm = TRUE), 2),
    SD_pH = round(sd(pH, na.rm = TRUE), 2),
    n = n()
  )

print(phalaris_summary)

phalaris_aov <- aov(pH ~ Fertilizer * Dose * Time, data = phalaris_ph)
summary(phalaris_aov)

phalaris_emm <- emmeans(phalaris_aov, ~ Fertilizer * Dose * Time)
pairs(phalaris_emm, adjust = "tukey")

ggplot(phalaris_ph, aes(x = Time, y = pH,
                        group = interaction(Fertilizer, Dose),
                        shape = Fertilizer,
                        linetype = as.factor(Dose))) +
  stat_summary(fun = mean, geom = "line", size = 1, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, color = "black") +
  stat_summary(fun = mean, geom = "point", size = 3, fill = "white") +
  labs(title = "Soil pH over Time - Phalaris(Exp 1)",
       y = "Soil pH",
       x = "Time Point",
       shape = "Fertilizer Type",
       linetype = "N Dose (kg/ha)") +
  theme_minimal() +
  theme(legend.position = "right")
# Combine both plant datasets for plotting
combined_ph <- ph_long %>%
  filter(Fertilizer != "Control")  # Optional: remove controls for clarity

ggplot(combined_ph, aes(x = Time, y = pH,
                        group = interaction(Fertilizer, Dose),
                        shape = Fertilizer,
                        linetype = as.factor(Dose))) +
  stat_summary(fun = mean, geom = "line", size = 1, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, color = "black") +
  stat_summary(fun = mean, geom = "point", size = 3, fill = "white") +
  labs(title = "Soil pH over Time (Exp 1)",
       y = "Soil pH",
       x = "Time Point",
       shape = "Fertilizer Type",
       linetype = "N Dose (kg/ha)") +
  facet_wrap(~ Plant.type) +
  theme_minimal() +
  theme(legend.position = "bottom")

##Exp 2 starts here
list.files()
ph_ec_lucerne2 <- read.csv("ph_ec_lucerne_exp2.csv")
colnames(ph_ec_lucerne2)
head(ph_ec_lucerne2)



##For lucerne first
library(tidyverse)

# Rename for simplicity
df <- ph_ec_lucerne2

# Create long-format pH data with correct time day mapping
ph_long_lucerne <- df %>%
  select(
    PotID = Pot.ID,
    Plant = Plant.type,
    Fertilizer = Fertilizer.type,
    Method = Application_method,
    Dose = Dose...N.kg.ha.,
    pH_Day30 = pH_BF_0days,
    pH_Day45 = pH_AF.15days._1stDose,
    pH_Day55 = pH_AF.30.days._1stDose,
    pH_Day60 = pH_3._before_2nd_SplitDose.60days.,
    pH_Day75 = pH_4_AF_2nd_Split.75days.,
    pH_Day90 = pH_5_before_3rdsplitDose..90days.,
    pH_Day105 = pH_6_AF_3rd_Splitdose.105days.,
    pH_Day110 = pH_7_AF_3rd_Splitdose.110days.,
    pH_Day125 = pH_8_atHarvestDay..125days.
  ) %>%
  pivot_longer(cols = starts_with("pH_"),
               names_to = "Timepoint",
               values_to = "pH") %>%
  mutate(
    Time = case_when(
      Timepoint == "pH_Day30" ~ 30,
      Timepoint == "pH_Day45" ~ 45,
      Timepoint == "pH_Day55" ~ 55,
      Timepoint == "pH_Day60" ~ 60,
      Timepoint == "pH_Day75" ~ 75,
      Timepoint == "pH_Day90" ~ 90,
      Timepoint == "pH_Day105" ~ 105,
      Timepoint == "pH_Day110" ~ 110,
      Timepoint == "pH_Day125" ~ 125
    ),
    Plant = factor(Plant, levels = c("L"), labels = c("Lucerne")),
    Fertilizer = factor(Fertilizer, levels = c("UF", "MF", "None"), 
                        labels = c("UrVAL", "Mineral Fertilizer", "Control")),
    Method = factor(Method),
    Dose = as.factor(Dose)
  )
ph_summary <- ph_long_lucerne %>%
  group_by(Fertilizer, Method, Dose, Time) %>%
  summarise(
    Mean_pH = round(mean(pH, na.rm = TRUE), 2),
    SD_pH = round(sd(pH, na.rm = TRUE), 2),
    n = n()
  )
print(ph_summary)

ggplot(ph_long_lucerne, aes(x = Time, y = pH,
                            group = interaction(Fertilizer, Dose),
                            shape = Fertilizer,
                            linetype = Dose)) +
  stat_summary(fun = mean, geom = "line", size = 1, color = "black") +
  stat_summary(fun = mean, geom = "point", size = 3, fill = "white") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 1.5, color = "black") +
  facet_wrap(~ Method) +
  labs(title = "Soil pH Change Over Time - Lucerne (Exp 2)",
       x = "Days After Fertilization",
       y = "Soil pH",
       shape = "Fertilizer",
       linetype = "Dose (kg N/ha)") +
  theme_minimal()
lucerne_aov2 <- aov(pH ~ Fertilizer * Method * Dose * Time, data = ph_long_lucerne)
summary(lucerne_aov2)

library(emmeans)

lucerne_emm2 <- emmeans(lucerne_aov2, ~ Fertilizer * Method * Dose * Time)
pairs(lucerne_emm2, adjust = "tukey")


library(ggplot2)

# 1. Prepare Control data with Method info
control_labeled <- ph_long_lucerne %>%
  filter(Fertilizer == "Control") %>%
  mutate(Fertilizer = "Control")  # Ensure consistent factor level

# 2. Treatment-only data
treatments_only <- ph_long_lucerne %>%
  filter(Fertilizer != "Control")

# 3. Combine all for full plot
full_data <- bind_rows(treatments_only, control_labeled)

# 4. Plot
ggplot(full_data, aes(x = Time, y = pH,
                      group = interaction(Fertilizer, Dose),
                      linetype = Dose,
                      shape = Fertilizer,
                      color = Fertilizer)) +
  # Horizontal dashed target line
  geom_hline(yintercept = 6.2, linetype = "dashed", color = "gray70", size = 0.6) +
  
  # Mean lines
  stat_summary(fun = mean, geom = "line", size = 1) +
  
  # Mean points
  stat_summary(fun = mean, geom = "point", size = 2.5, fill = "white") +
  
  # Error bars
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 1) +
  
  facet_wrap(~ Method, nrow = 1, labeller = label_both) +
  
  scale_shape_manual(values = c("UrVAL" = 16, "Mineral Fertilizer" = 17, "Control" = 15)) +
  scale_color_manual(values = c("UrVAL" = "black", "Mineral Fertilizer" = "gray30", "Control" = "gray10")) +
  scale_linetype_manual(values = c("10" = "solid", "20" = "dotted", "30" = "dotdash",
                                   "40" = "twodash", "50" = "longdash", "60" = "F1", "0" = "longdash")) +
  
  labs(
    title = "Soil pH Change Over Time - Lucerne (Exp 2)",
    subtitle = "Dashed gray line = target pH 6.2 | Control shown in both panels",
    x = "Days After Fertilization",
    y = "Soil pH",
    shape = "Fertilizer Type",
    color = "Fertilizer Type",
    linetype = "Dose (kg N/ha)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1.2, "lines")
  )


# Define color palette for each dose level
dose_colors <- c(
  "0" = "black",       # Control
  "10" = "gray20",
  "20" = "gray40",
  "30" = "gray60",
  "40" = "gray80",
  "50" = "brown",
  "60" = "darkred"
)

# Full combined data (as before)
full_data <- bind_rows(treatments_only, control_labeled)

# Plot
ggplot(full_data, aes(x = Time, y = pH,
                      group = interaction(Fertilizer, Dose),
                      shape = Fertilizer,
                      color = as.factor(Dose))) +  # Color by Dose
  geom_hline(yintercept = 6.2, linetype = "dashed", color = "gray70", size = 0.6) +
  
  # Mean lines per treatment/dose
  stat_summary(fun = mean, geom = "line", size = 1.1) +
  
  # Mean points
  stat_summary(fun = mean, geom = "point", size = 2.5, fill = "white") +
  
  # Error bars
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 1) +
  
  facet_wrap(~ Method, nrow = 1, labeller = label_both) +
  
  scale_shape_manual(values = c("UrVAL" = 16, "Mineral Fertilizer" = 17, "Control" = 15)) +
  scale_color_manual(values = dose_colors, name = "Dose (kg N/ha)") +
  
  labs(
    title = "Soil pH Change Over Time - Lucerne (Exp 2)",
    subtitle = "Different colors for N dose | Control (black dashed line) used as baseline",
    x = "Days After Fertilization",
    y = "Soil pH",
    shape = "Fertilizer Type"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1.2, "lines"),
    legend.key.width = unit(1.2, "cm")
  )



##for phalaris type 

list.files()
ph_ec_phalaris2 <- read.csv("ph_ec_phalaris_exp2.csv")
colnames(ph_ec_phalaris2)
head(ph_ec_phalaris2)

library(tidyverse)

# Load your dataset (assuming it's already read into ph_ec_phalaris2)
df_phalaris <- ph_ec_phalaris2

# Create long-format pH dataset
ph_long_phalaris <- df_phalaris %>%
  select(
    PotID = Pot.ID,
    Plant = Plant,
    Fertilizer = Fertilizer_type,
    Method = Application_method,
    Dose = Dose_.N.kg.ha.,
    pH_Day35 = pH_BF_0days,
    pH_Day40 = pH_2_AF_1stdose.15days.,
    pH_Day53 = pH_3_before_2nddose.30days.,
    pH_Day65 = pH_4_after_2nddose.45days.,
    pH_Day80 = pH_5_before_3rddose.60days.,
    pH_Day90 = pH_6_after_3rddose.70days.,
    pH_Day105 = pH_7_after_3rddose.90days.,
    pH_Harvest = ph_8_atHD_105days
  ) %>%
  pivot_longer(cols = starts_with("pH_"),
               names_to = "Timepoint",
               values_to = "pH") %>%
  mutate(
    Time = case_when(
      Timepoint == "pH_Day35" ~ 35,
      Timepoint == "pH_Day40" ~ 40,
      Timepoint == "pH_Day53" ~ 53,
      Timepoint == "pH_Day65" ~ 65,
      Timepoint == "pH_Day80" ~ 80,
      Timepoint == "pH_Day90" ~ 90,
      Timepoint == "pH_Day105" ~ 105,
      Timepoint == "pH_Harvest" ~ 125
    ),
    Plant = "Phalaris",
    Fertilizer = factor(Fertilizer, levels = c("UF", "MF", "None"),
                        labels = c("UrVAL", "Mineral Fertilizer", "Control")),
    Method = factor(Method),
    Dose = as.factor(Dose)
  )
ph_summary_phalaris <- ph_long_phalaris %>%
  group_by(Fertilizer, Method, Dose, Time) %>%
  summarise(
    mean_pH = mean(pH, na.rm = TRUE),
    sd_pH = sd(pH, na.rm = TRUE),
    .groups = "drop"
  )
phalaris_aov <- aov(pH ~ Fertilizer * Dose * Time , data = ph_long_phalaris)
summary(phalaris_aov)



# Fix naming: use correct column names from your data
df_phalaris <- ph_long_phalaris  # assuming you've already created this earlier

# Define colors per dose
dose_colors <- c(
  "0" = "black",
  "20" = "gray20",
  "40" = "gray40",
  "60" = "gray60",
  "80" = "gray80",
  "100" = "brown",
  "120" = "darkred"
)

# Make the plot
ggplot(df_phalaris, aes(x = Time, y = pH,
                        group = interaction(Fertilizer, Dose),
                        shape = Fertilizer,
                        color = as.factor(Dose))) +
  
  # Add reference soil pH line
  geom_hline(yintercept = 6.2, linetype = "dashed", color = "gray70", size = 0.6) +
  
  # Plot mean lines
  stat_summary(fun = mean, geom = "line", size = 1.1) +
  
  # Plot mean points
  stat_summary(fun = mean, geom = "point", size = 2.5, fill = "white") +
  
  # Add error bars
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 1) +
  
  # Show separately for split vs. one-time
  facet_wrap(~ Method, nrow = 1, labeller = label_both) +
  
  # Styling
  scale_shape_manual(values = c("UrVAL" = 16, "Mineral Fertilizer" = 17, "Control" = 15)) +
  scale_color_manual(values = dose_colors, name = "Dose (kg N/ha)") +
  
  labs(
    title = "Soil pH Change Over Time - Phalaris (Exp 2)",
    subtitle = "Control = black line | Dashed line shows baseline pH = 6.2",
    x = "Days After Fertilization",
    y = "Soil pH",
    shape = "Fertilizer Type"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1.2, "lines"),
    legend.key.width = unit(1.2, "cm")
  )
