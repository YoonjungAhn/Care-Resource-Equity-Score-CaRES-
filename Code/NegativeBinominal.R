# ==============================================================================
# NEGATIVE BINOMIAL REGRESSION ANALYSIS FOR POWER OUTAGES
# Examining demographic and climate predictors of extended outages (>8 hours)
# ==============================================================================

# Load required libraries
library(data.table)
library(MASS)
library(tidyverse)
library(broom)  # For tidy model output

cat("==============================================================================\n")
cat("NEGATIVE BINOMIAL REGRESSION ANALYSIS\n")
cat("Outcome: Number of power outages lasting >8 hours\n")
cat("==============================================================================\n\n")

# Read data
data <- fread("./Poweroutage/Code/final_code/dataformodel.csv")

# Data cleaning
data_clean <- data %>%
  filter(!is.na(Number_of_outages_more_than_8hrs) & 
           !is.na(total_population) & 
           !is.na(age_65over) & 
           !is.na(extreme_heat_days) &
           !is.na(GEOID))

cat("Analysis sample: ", nrow(data_clean), "counties\n")
cat("Observations excluded due to missing data: ", nrow(data) - nrow(data_clean), "\n\n") #I wanted to QC and check this more

# ==============================================================================
# DESCRIPTIVE STATISTICS
# ==============================================================================

cat("### OUTCOME DISTRIBUTION ###\n")
cat("Mean outages >8hrs:", round(mean(data_clean$Number_of_outages_more_than_8hrs), 2), "\n")
cat("Median:", median(data_clean$Number_of_outages_more_than_8hrs), "\n")
cat("Range:", range(data_clean$Number_of_outages_more_than_8hrs), "\n")
cat("% Counties with zero long outages:", 
    round(sum(data_clean$Number_of_outages_more_than_8hrs == 0)/nrow(data_clean)*100, 1), "%\n\n")

# ==============================================================================
# NEGATIVE BINOMIAL REGRESSION
# ==============================================================================

cat("### NEGATIVE BINOMIAL REGRESSION MODEL ###\n\n")

model_nb <- glm.nb(Number_of_outages_more_than_8hrs ~ 
                     total_population + 
                     age_65over + 
                     prc_65older_livealone +
                     acuindex2_cty +
                     pctblack_mds3_cty +
                     pcthisp_mds3_cty +
                     extreme_heat_days +
                     tornado_expose,
                   data = data_clean)

# Display standard output
summary(model_nb)

# ==============================================================================
# CALCULATE INCIDENCE RATE RATIOS (IRR)
# ==============================================================================

cat("\n### INCIDENCE RATE RATIOS (IRR) ###\n")
cat("IRR = exp(coefficient): Multiplicative effect on expected count\n")
cat("IRR > 1: Increased risk | IRR < 1: Decreased risk | IRR = 1: No effect\n\n")

# Extract coefficients and calculate IRRs
coef_nb <- summary(model_nb)$coefficients
irr <- exp(coef_nb[,1])
irr_ci_lower <- exp(coef_nb[,1] - 1.96*coef_nb[,2])
irr_ci_upper <- exp(coef_nb[,1] + 1.96*coef_nb[,2])

# Create results table
results <- data.frame(
  Variable = rownames(coef_nb),
  Coefficient = round(coef_nb[,1], 4),
  SE = round(coef_nb[,2], 4),
  IRR = round(irr, 3),
  CI_Lower = round(irr_ci_lower, 3),
  CI_Upper = round(irr_ci_upper, 3),
  z_value = round(coef_nb[,3], 3),
  p_value = round(coef_nb[,4], 4),
  Sig = ifelse(coef_nb[,4] < 0.001, "***",
               ifelse(coef_nb[,4] < 0.01, "**",
                      ifelse(coef_nb[,4] < 0.05, "*", "")))
)

print(results, row.names = FALSE)

# ==============================================================================
# INTERPRETATION OF KEY FINDINGS
# ==============================================================================

cat("\n### KEY FINDINGS (IRR Interpretation) ###\n\n")

# Calculate percent changes for easier interpretation
pct_change <- function(irr) {round((irr - 1) * 100, 1)}

# ==============================================================================
# MODEL DIAGNOSTICS
# ==============================================================================

cat("### MODEL DIAGNOSTICS ###\n")
cat("Theta (dispersion):", round(model_nb$theta, 3), "\n")
cat("SE of theta:", round(model_nb$SE.theta, 3), "\n")
cat("AIC:", round(AIC(model_nb), 1), "\n")
cat("Residual deviance:", round(model_nb$deviance, 1), "on", model_nb$df.residual, "df\n\n")

# Test for remaining overdispersion
cat("Residual deviance/df ratio:", round(model_nb$deviance/model_nb$df.residual, 2), "\n")
cat("(Values close to 1 indicate good fit)\n\n")

# ==============================================================================
# CREATE PUBLICATION-READY TABLE
# ==============================================================================

# Format for manuscript
pub_table <- data.frame(
  Variable = c("Demographic Characteristics",
               "  Total population", 
               "  Population aged 65+",
               "  % Aged 65+ living alone",
               "Nursing Home Characteristics",
               "  Acuity Index",
               "  % Black residents", 
               "  % Hispanic residents",
               "Climate Exposures",
               "  Extreme heat days",
               "  Tornado exposure"),
  IRR = c("", 
          sprintf("%.3f%s", irr[2], results$Sig[2]),
          sprintf("%.3f%s", irr[3], results$Sig[3]),
          sprintf("%.3f%s", irr[4], results$Sig[4]),
          "",
          sprintf("%.3f%s", irr[5], results$Sig[5]),
          sprintf("%.3f%s", irr[6], results$Sig[6]),
          sprintf("%.3f%s", irr[7], results$Sig[7]),
          "",
          sprintf("%.3f%s", irr[8], results$Sig[8]),
          sprintf("%.3f%s", irr[9], results$Sig[9])),
  CI_95 = c("",
            sprintf("(%.3f, %.3f)", irr_ci_lower[2], irr_ci_upper[2]),
            sprintf("(%.3f, %.3f)", irr_ci_lower[3], irr_ci_upper[3]),
            sprintf("(%.3f, %.3f)", irr_ci_lower[4], irr_ci_upper[4]),
            "",
            sprintf("(%.3f, %.3f)", irr_ci_lower[5], irr_ci_upper[5]),
            sprintf("(%.3f, %.3f)", irr_ci_lower[6], irr_ci_upper[6]),
            sprintf("(%.3f, %.3f)", irr_ci_lower[7], irr_ci_upper[7]),
            "",
            sprintf("(%.3f, %.3f)", irr_ci_lower[8], irr_ci_upper[8]),
            sprintf("(%.3f, %.3f)", irr_ci_lower[9], irr_ci_upper[9]))
)

cat("### PUBLICATION-READY TABLE ###\n")
print(pub_table, row.names = FALSE)

# Save results
write.csv(results, "/Users/y943a214/Yoonjung/Research/Poweroutage/Draft/table/negative_binomial_IRR_full_results.csv", row.names = FALSE)
write.csv(pub_table, "/Users/y943a214/Yoonjung/Research/Poweroutage/Draft/table/negative_binomial_IRR_publication_table.csv", row.names = FALSE)

cat("\n\nResults saved to:\n")
cat("- negative_binomial_IRR_full_results.csv (complete statistics)\n")
cat("- negative_binomial_IRR_publication_table.csv (formatted for manuscript)\n")

# ==============================================================================
# ROBUSTNESS CHECK: Compare with Poisson
# ==============================================================================

cat("\n### ROBUSTNESS CHECK ###\n")
model_poisson <- glm(Number_of_outages_more_than_8hrs ~ 
                       total_population + age_65over + prc_65older_livealone +
                       acuindex2_cty + pctblack_mds3_cty + pcthisp_mds3_cty +
                       extreme_heat_days + tornado_expose,
                     family = poisson, data = data_clean)

cat("Model comparison:\n")
cat("Poisson AIC:", AIC(model_poisson), "\n")
cat("Negative Binomial AIC:", AIC(model_nb), "\n")
cat("Difference:", AIC(model_poisson) - AIC(model_nb), "(NB is better if positive)\n")




# ==============================================================================
# Making coef plot
# ==============================================================================
library(broom)
library(ggplot2)
library(dplyr)

# Extract coefficients and confidence intervals
coefs <- tidy(model_nb, conf.int = TRUE)

# Clean up variable names for plotting (optional)
coefs <- coefs %>%
  filter(term != "(Intercept)") %>%
  mutate(term = recode(term,
                       total_population = "Total Population",
                       age_65over = "Population 65+",
                       prc_65older_livealone = "% 65+ Living Alone",
                       acuindex2_cty = "AC Vulnerability Index",
                       pctblack_mds3_cty = "% Black",
                       pcthisp_mds3_cty = "% Hispanic",
                       extreme_heat_days = "Extreme Heat Days",
                       tornado_expose = "Tornado Exposure"))

# Plot
ggplot(coefs, aes(x = term, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  coord_flip() +
  labs(title = "Coefficient Plot (Negative Binomial Model)",
       x = "Predictors", y = "Estimate (log scale)") +
  theme_minimal(base_size = 14)



# install.packages("ggstatsplot")  # if not installed
library(broom)
library(ggplot2)
library(viridis)
library(dplyr)

# Step 1: Extract model output
model_df <- tidy(model_nb, conf.int = TRUE)

# Step 2: Remove intercept and apply human-readable labels
new_labels <- c(
  total_population = "Total Population",
  age_65over = "Population 65+",
  prc_65older_livealone = "% 65+ Living Alone",
  acuindex2_cty = "Intensive Care Needs",
  pctblack_mds3_cty = "% Black",
  pcthisp_mds3_cty = "% Hispanic",
  extreme_heat_days = "Extreme Heat Days",
  tornado_expose = "Tropical Cyclone Exposure"
)

model_df <- model_df %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = recode(term, !!!new_labels)
  )

# Step 3: Plot using ggplot2
ggplot(model_df, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(size =1, color = viridis::rocket(1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  coord_flip() +
  labs(
    title = "Negative Binomial Regression Coefficients",
    x = "Predictors",
    y = "Estimate (log scale)"
  ) +
  theme_minimal(base_size = 15)
library(broom)
library(ggplot2)
library(dplyr)
library(viridis)
library(gridExtra)     # For tableGrob
library(patchwork)     # For combining plots

# Step 1: Tidy the model and prepare labels
model_df <- tidy(model_nb, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = recode(term,
                  total_population = "Total Population",
                  age_65over = "Population 65+",
                  prc_65older_livealone = "% 65+ Living Alone",
                  acuindex2_cty = "Intensive Care Needs",
                  pctblack_mds3_cty = "% Black African American",
                  pcthisp_mds3_cty = "% Hispanic",
                  extreme_heat_days = "Extreme Heat Days",
                  tornado_expose = "Tropical Cyclone Exposure"
    ),
    sig_level = case_when(
      p.value < 0.001 ~ "p<0.001",
      p.value < 0.01  ~ "p<0.01",
      p.value < 0.05  ~ "p<0.05",
      TRUE ~ "NS"
    )
  )%>%arrange(term) %>%  # Sort alphabetically
  mutate(term = factor(term, levels = term))  # Keep alphabetical order in plot

# Step 3: Create the summary table
table_data <- model_df %>%
  select(Predictor = term, Estimate = estimate, `Lower 95% CI` = conf.low,
         `Upper 95% CI` = conf.high, `Significance` = sig_level) %>%
  mutate(across(where(is.numeric), round, 3))

summary_table <- tableGrob(table_data, rows = NULL)


# Define dot shapes
sig_shapes <- c("p<0.001" = 19, "p<0.01" = 17, "p<0.05" = 15, "NS" = 1)
term_levels <- sort(unique(model_df$term))  # Alphabetical term order
model_df <- model_df %>%
  mutate(term = factor(term, levels = rev(term_levels)))  # 👈 Reverse levels for coord_flip

# Step 2: Create the coefficient plot (no legend)

coef_plot <- ggplot(model_df, aes(x = term, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  size = 1.2, color = viridis::rocket(1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  coord_flip() +
  labs(
    title = "",
    x = "Predictors",
    y = "Estimate (log scale)"
  ) +
  theme_minimal(base_size = 15)



# Step 4: Combine vertically (plot on top, table below)
combined_plot <- coef_plot / wrap_elements(summary_table) +
  plot_layout(heights = c(3, 1))  # Adjust height ratio as needed

# Display the final output
combined_plot


ggsave('./Poweroutage/Draft/figure/NBresult.png', dpi=300, width =10, height =10, units = "in")
             