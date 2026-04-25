# Load required libraries
library(tidyverse)
library(ggplot2)
library(sjPlot)     # For regression plots
library(ggeffects)  # For marginal effects

# Create folder for plots if not exists
if(!dir.exists("plots")) dir.create("plots")

# [1] Wage distribution plot
p1 <- ggplot(df_full, aes(x = ln_wage)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "#A9CCE3", color = "white", alpha = 0.8) +
  geom_density(color = "#2C3E50", size = 1.2) +
  labs(x = "Log(Monthly Wage)", y = "Density") +
  theme_minimal(base_size = 14)

# Save histogram
ggsave("plots/01_wage_distrubution.png", plot = p1, width = 8, height = 5, dpi = 300)

# [2] Forest plot comparing young vs. middle-aged cohorts
col_middle = "#D0021B"
col_young = "#4A90E2"
p2 <- plot_models(model_young, model_middle,
                  m.labels = c("Young Cohort (15-35)", "Middle-aged Cohort (36-55)"),
                  show.values = TRUE, show.p = TRUE,
                  dot.size = 2.5, line.size = 1,
                  rm.terms = c("reg_iHighlands", "reg_iMRD", "reg_iNMM", "reg_iRRD", "reg_iSE", "RRD", "SE"),
                  title = "", 
                  axis.title = "Effect on Log(Wage)",
                  colors = c(col_middle, col_young)) +
  theme(legend.position = "bottom")

# Save forest plot
ggsave("plots/02_forest_plot.png", plot = p2, width = 9, height = 6, dpi = 300)

# [3] Marginal effects of education × service segment
interact_pred <- ggpredict(model_middle, terms = c("educ", "servseg"))
df_interact <- as.data.frame(interact_pred)

p3 <- ggplot(df_interact, aes(x = as.factor(x), y = predicted, color = group, group = group)) + 
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(x = "Education (0 = No Degree, 1 = Degree+)",
       y = "Predicted Log(Wage)",
       color = "Service Segment \n(0 = Low-end, 1 = High-end)") +
  scale_color_manual(values = c("0" = "#8E8E93", "1" = "#27AE60")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

# Save interaction effect plot
ggsave("plots/03_interaction_effect.png", plot = p3, width = 8, height = 5, dpi = 300)

# [4] Mincer curve (wage-experience profile)
p4 <- ggplot() +
    geom_smooth(data = df_young, aes(x = exp, y = ln_wage, color = "Young Cohort (15-35)"),
                method = "lm", formula = y ~ poly(x, 2), se = TRUE, size = 1.2) +
    geom_smooth(data = df_middle, aes(x = exp, y = ln_wage, color = "Middle-aged Cohort (36-55)"),
                method = "lm", formula = y ~ poly(x, 2), se = TRUE, size = 1.2) +
    labs(x = "Years of Potential Experience", y = "Log Monthly Wage", color = "Age Cohort") +
    scale_color_manual(values = c("Young Cohort (15-35)" = "#4A90E2", 
                                  "Middle-aged Cohort (36-55)" = "#D0021B")) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")

# Save Mincer curve plot
ggsave("plots/04_mincer_curve.png", plot = p4, width = 8, height = 5, dpi = 300)