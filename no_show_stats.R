# V. Stevens Oct 2024
# Data: 2023 and 2024 completed appts and no shows, Mon-Sat
# 2024 data until 10/8/24
# This script completes prelim analysis of no show rates

setwd("C:\\Users\\m296398\\Desktop\\no_show_stats")

###############
### Imports ###
###############
# NOTE: run reformatting.R to acquire prior imports and reformatted datasets
# install.packages("tableone") # for summary stats
install.packages("gtsummary") # for summary stats
install.packages("ggplot2") # for figures

library(gtsummary)
library(ggplot2)
library(rstatix) # fishers
library(tidyr) # fishers
library(carData) # effects plots
library(effects) # effects plots
library(lme4) # mixed effects model


# refactor into categorical vars, ordinal if necessary
# complete vs no show = nominal
data_2023$apptslotstatus <- factor(data_2023$apptslotstatus)
data_2024$apptslotstatus <- factor(data_2024$apptslotstatus)
combined_data$apptslotstatus <- factor(combined_data$apptslotstatus)
# specialty = nominal
data_2023$appt.schdlng.prvdr.spclty <- factor(data_2023$appt.schdlng.prvdr.spclty)
data_2024$appt.schdlng.prvdr.spclty <- factor(data_2024$appt.schdlng.prvdr.spclty)
combined_data$appt.schdlng.prvdr.spclty <- factor(combined_data$appt.schdlng.prvdr.spclty)
# apptday = ordinal (because subgrouping weekdays too)
data_2023$apptday <- factor(data_2023$apptday, 
                            levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
                            ordered = TRUE)
data_2024$apptday <- factor(data_2024$apptday, 
                            levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
                            ordered = TRUE)
combined_data$apptday <- factor(combined_data$apptday, 
                            levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
                            ordered = TRUE)
# apptmnth = ordinal
data_2023$apptmnth <- factor(data_2023$apptmnth, 
                             levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                             ordered = TRUE)  # Ordinal factor for months
data_2024$apptmnth <- factor(data_2024$apptmnth, 
                             levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October"),
                             ordered = TRUE)
# start time = ordinal
data_2023$apptstarttime <- factor(data_2023$apptstarttime, 
                                  levels = c("Early AM", "AM", "PM"),
                                  ordered = TRUE)
data_2024$apptstarttime <- factor(data_2024$apptstarttime, 
                                  levels = c("AM", "PM"),
                                  ordered = TRUE)

cols <- colnames(data_2023)
str(data_2023)
str(data_2024)



########################
## Summary Statistics ##
########################

## Compute descriptive stats for 2024
summary_table_2024 <- tbl_summary(
  data = data_2024 %>% select(-apptdate, -appt.schdlng.prvdr),  # Exclude unnecessary columns
  by = apptslotstatus,  # Group by appointment status (Completed vs No Show)
  statistic = list(all_categorical() ~ "{n} ({p}%)"),  # Show counts and percentages for categorical variables
  label = list( # make the categories more human readable
    apptday ~ "Appointment Day",
    apptmnth ~ "Appointment Month",
    apptstarttime ~ "Start Time",
    appt.schdlng.prvdr.spclty ~ "Specialty"
  ),
  missing_text = "Missing" # in case of missing values. should be nonexistent w this raformatted data
) |>
  add_p(
    test = list(all_categorical() ~ "chisq.test"),  # Perform Chi-Square Test
    test.args = list(all_categorical() ~ list(simulate.p.value = TRUE)),  # Simulation for small samples
    pvalue_fun = function(x) style_pvalue(x, digits = 3)  # Format p-values
  ) |>
  add_q(method = "BH") |> # adjusting p value for multiple comparisons, using Benjamini-Hochberg adjustment
  add_overall() |> # add column with overall summary statistics
  #add_n()
  bold_labels()

# Display the updated summary table
summary_table_2024

## Compute descriptive stats for 2023
summary_table_2023 <- tbl_summary(
  data = data_2023 %>% select(-apptdate, -appt.schdlng.prvdr),  # Exclude unnecessary columns
  by = apptslotstatus,  # Group by appointment status (Completed vs No Show)
  statistic = list(all_categorical() ~ "{n} ({p}%)"),  # Show counts and percentages for categorical variables
  label = list( # make the categories more human readable
    apptday ~ "Appointment Day",
    apptmnth ~ "Appointment Month",
    apptstarttime ~ "Start Time",
    appt.schdlng.prvdr.spclty ~ "Specialty"
  ),
  missing_text = "Missing" # in case of missing values. should be nonexistent w this raformatted data
) |>
  add_p(
    test = list(all_categorical() ~ "chisq.test"),  # Perform Chi-Square Test
    test.args = list(all_categorical() ~ list(simulate.p.value = TRUE)),  # Simulation for small samples
    pvalue_fun = function(x) style_pvalue(x, digits = 3)  # Format p-values
  ) |>
  add_q(method = "BH") |> # adjusting p value for multiple comparisons, using Benjamini-Hochberg adjustment
  add_overall() |> # add column with overall summary statistics
  #add_n()
  bold_labels()

# Display the updated summary table
summary_table_2023


# Find the unique levels across both data frames for `apptmnth` and `apptstarttime`
all_month_levels <- union(levels(data_2023$apptmnth), levels(data_2024$apptmnth))
all_time_levels <- union(levels(data_2023$apptstarttime), levels(data_2024$apptstarttime))

# Apply the standardized levels to combined_data data frame
combined_data$apptmnth <- factor(combined_data$apptmnth, levels = all_month_levels, ordered = TRUE)
combined_data$apptstarttime <- factor(combined_data$apptstarttime, levels = all_time_levels, ordered = TRUE)

## Compute descriptive stats for total, 2023-24
summary_table_total <- tbl_summary(
  data = combined_data %>% select(-apptdate, -appt.schdlng.prvdr),  # Exclude unnecessary columns
  by = apptslotstatus,  # Group by appointment status (Completed vs No Show)
  statistic = list(all_categorical() ~ "{n} ({p}%)"),  # Show counts and percentages for categorical variables
  label = list( # make the categories more human readable
    apptday ~ "Appointment Day",
    apptmnth ~ "Appointment Month",
    apptstarttime ~ "Start Time",
    appt.schdlng.prvdr.spclty ~ "Specialty"
  ),
  missing_text = "Missing" # in case of missing values. should be nonexistent w this raformatted data
) |>
  add_p(
    test = list(all_categorical() ~ "chisq.test"),  # Perform Chi-Square Test
    test.args = list(all_categorical() ~ list(simulate.p.value = TRUE)),  # Simulation for small samples
    pvalue_fun = function(x) style_pvalue(x, digits = 3)  # Format p-values
  ) |>
  add_q(method = "BH") |> # adjusting p value for multiple comparisons, using Benjamini-Hochberg adjustment
  add_overall() |> # add column with overall summary statistics
  #add_n()
  bold_labels()

# Display the updated summary table
summary_table_total


################################################
## Fisher's: Saturday vs Weekday Primary Care ##
################################################
## Fisher's exact test, since there are some groups with < 5 no shows
### compare no show rates within primary care only, weekdays vs saturdays
### and between student run clinics 
saturdays_creighton <- data_2024 %>%
  filter(apptday == "Saturday" & 
           (appt.schdlng.prvdr == "CREIGHTON_IM" | appt.schdlng.prvdr == "CREIGHTON_FM" | appt.schdlng.prvdr == "CREIGHTON_PEDS" | appt.schdlng.prvdr == "HAZIN_M"))

saturdays_mayo <- data_2024 %>%
  filter(apptday == "Saturday" & 
           (appt.schdlng.prvdr == "MAYO_IM" | appt.schdlng.prvdr == "MAYO_FM" | appt.schdlng.prvdr == "MAYO_GYNE"))

saturdays_uofa <- data_2024 %>%
  filter(apptday == "Saturday" & appt.schdlng.prvdr == "UOFA_STUDENT_CLINIC")

weekday_primary_care <- data_2024 %>%
  filter(apptday != "Saturday" & appt.schdlng.prvdr.spclty == "Primary Care")

provider_vector <- c("Weekday Primary Care", "Creighton Saturday Clinic", "Mayo Saturday Clinic", "U of A Saturday Clinic")

ns_df <- data.frame(Column1 = provider_vector)
colnames(ns_df) <- c("Provider")
ns_df$No_Show_Count <- c(sum(weekday_primary_care$apptslotstatus == "No Show"),
                         sum(saturdays_creighton$apptslotstatus == "No Show"),
                         sum(saturdays_mayo$apptslotstatus == "No Show"),
                         sum(saturdays_uofa$apptslotstatus == "No Show"))
ns_df$Total_Count <- c(nrow(weekday_primary_care),
                            nrow(saturdays_creighton),
                            nrow(saturdays_mayo),
                            nrow(saturdays_uofa))

# Step 1: Calculate Show Counts and No-Show Proportion
ns_df <- ns_df %>%
  mutate(
    Show_Count = Total_Count - No_Show_Count,
    No_Show_Proportion = No_Show_Count / Total_Count
  )

# Step 2: Generate All Pairwise Comparisons
pairwise_results <- combn(seq_len(nrow(ns_df)), 2, function(idx) {
  provider1 <- ns_df[idx[1], ]
  provider2 <- ns_df[idx[2], ]
  
  # Create contingency table for the two providers
  contingency_table <- matrix(
    c(provider1$No_Show_Count, provider1$Show_Count,
      provider2$No_Show_Count, provider2$Show_Count),
    nrow = 2
  )
  
  # Run Fisher's Exact Test
  test_result <- fisher.test(contingency_table)
  
  # Return results in a data frame
  data.frame(
    comparison = paste(provider1$Provider, "vs", provider2$Provider),
    p_value = test_result$p.value
  )
}, simplify = FALSE) %>%
  bind_rows()

# # Step 3: Adjust p-values 
# Assuming `pairwise_results` has columns `Provider1`, `Provider2`, and `p_adj`
# Ensure that `pairwise_results` has these columns from the pairwise test results
pairwise_results <- pairwise_results %>%
  mutate(
    Provider1 = str_extract(comparison, "^[^ ]+.*?(?= vs)"),    # Extract first provider
    Provider2 = str_extract(comparison, "(?<=vs ).*$"),  # Extract second provider
    p_adj = p.adjust(p_value, method = "BH"), # a little less robust than other comparisons like Bonferroni correction
    label = paste("p =", format(p_adj, digits = 3))
  )

# Set the order of providers in the desired order
ns_df$Provider <- factor(ns_df$Provider, levels = c("Weekday Primary Care", "Creighton Saturday Clinic", "Mayo Saturday Clinic", "U of A Saturday Clinic"))

p <- ggplot(ns_df, aes(x = Provider, y = No_Show_Proportion * 100, fill = Provider)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = scales::percent(No_Show_Proportion, accuracy = 0.1)), 
            vjust = -0.7, size = 4) +  # Add proportion labels
  labs(title = "No-Show Proportion by Primary Care Provider (2024)", 
       x = "Provider", 
       y = "No-Show Proportion (%)") +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove the legend
    plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 18, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10), size = 12),
    axis.title.y = element_text(margin = margin(r = 10), size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) +
  scale_y_continuous(limits = c(0, 30), expand = c(0, 0))  # Adjust y-axis limits if needed

# Add p-value annotations
position_offset <- 3  # Adjust this value for better spacing above bars

# Add annotations 
for (i in 1:3) {
  provider1 <- pairwise_results$Provider1[i]
  provider2 <- pairwise_results$Provider2[i]
  p_val_label <- paste("p =", format(pairwise_results$p_value[i], digits = 3))
  
  # Get y positions for providers
  provider1_pos <- ns_df$No_Show_Proportion[ns_df$Provider == provider1] * 100
  provider2_pos <- ns_df$No_Show_Proportion[ns_df$Provider == provider2] * 100
  
  max_y <- max(provider1_pos, provider2_pos) + (i * position_offset)  # Offset to prevent overlap
  
  p <- p +
    annotate("segment", 
             x = which(levels(ns_df$Provider) == provider1), 
             xend = which(levels(ns_df$Provider) == provider2), 
             y = max_y, yend = max_y, color = "black") +
    annotate("text", 
             x = mean(c(which(levels(ns_df$Provider) == provider1), 
                        which(levels(ns_df$Provider) == provider2))),
             y = max_y + 0.7, label = p_val_label, size = 4)
}

# Display the plot
print(p)



#########################
## Mixed Effects Model ##
#########################
# fixed effects = day and specialty, random effect = month and start time
# account for variability of month and appt time, analyze no show rates capturing fixed patterns across days and specialties
# Constructing the mixed-effects logistic regression model
mixed_model <- glmer(
  apptslotstatus == "No Show" ~ 
    apptday + appt.schdlng.prvdr.spclty +          # Fixed effects
    # (1 | apptmnth) +  # Random effect for month -- took out because variance low
    (1 | apptstarttime),          # Random effect for start time
  data = combined_data,
  family = binomial
)
summary(mixed_model)
plot(allEffects(mixed_model)) # to visualize fixed effects -- marginal effect of each predictor on the probability of no show

## residual diagnostics
# check for normality of residuals: should follow normal distribution if assumptions met
qqnorm(resid(mixed_model))
qqline(resid(mixed_model))

# plot redisuals vs fitted values -- should show random scatter without patterns if model is well-fitted
plot(fitted(mixed_model), resid(mixed_model))

## likelihood ratio test
# Compare your mixed-effects model to a simpler model (e.g., without random effects) 
# to test if including the random effects significantly improves the model.
simpler_model <- glm(apptslotstatus == "No Show" ~ 
                       apptday + appt.schdlng.prvdr.spclty, family = binomial, data = combined_data)
anova(simpler_model, mixed_model, test = "Chisq") # low p value --> including random effect improved model

## AIC/BIC comparison:  Lower values indicate a better fit, accounting for complexity.
AIC(mixed_model, simpler_model) # pretty close in AIC, simple model slightly better

## Intraclass Correlation Coefficient
icc <- as.numeric(VarCorr(mixed_model)$apptstarttime / (VarCorr(mixed_model)$apptstarttime + attr(VarCorr(mixed_model), "sc")^2))
# icc = 0.223371 # moderate amount of clustering within appt start time, should be included as random effect




###############
### Figures ###
###############
# ggplot(combined_data, aes(x = appt.schdlng.prvdr.spclty, y = ..prop.., group = 1)) +
#   geom_bar() +
#   labs(title = "Proportion of Each Specialty (Grouped), Overall", x = "Specialty", y = "Proportion") +
#   theme(axis.text.x = element_text(angle = 70, hjust = 1))
# 
# ggplot(data_2024, aes(x = appt.schdlng.prvdr.spclty, y = ..prop.., group = 1)) +
#   geom_bar() +
#   labs(title = "Proportion of Each Specialty", x = "Specialty", y = "Proportion") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

