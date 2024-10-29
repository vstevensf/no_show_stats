# V. Stevens 10/9/24
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

# library(tableone)
library(gtsummary)
library(ggplot2)

########################
## Summary Statistics ##
########################

## Combined data (completed appts + no shows), per year
data_2023 <- bind_rows(completed_appts_2023, completed_sats_2023, noshows_2023, noshows_sats_2023)
# delete rogue 415 am time in 2023 (this is outside business hours on thursday)
data_2023 <- data_2023 %>%
  filter(apptstarttime != as_hms("04:15:00"))
# and the random MAMMOs at and after 4 pm for uniform afternoon times
data_2023 <- data_2023 %>%
  filter(apptstarttime < as_hms("16:00:00"))
# and finish making combined data
data_2023 <- data_2023 %>%
  select(-apptcheckintime, -apptcancelreason) # 7731 x 7, 6667 complete, 1064 no shows
sapply(data_2023, function(x) sum(is.na(x)))

data_2024 <- bind_rows(completed_appts_2024, completed_sats_2024, noshows_2024, noshows_sats_2024)
data_2024 <- data_2024 %>%
  select(-apptcheckintime, -apptcancelreason) # 5712 x 7, 4950 complete, 762 no shows
sapply(data_2024, function(x) sum(is.na(x))) 

# categorization rewording of completed vs no show
data_2023 <- data_2023 %>%
  mutate(apptslotstatus = case_when(
    apptslotstatus == "3 - Checked Out" ~ "Completed",
    apptslotstatus == "4 - Charge Entered" ~ "Completed",
    apptslotstatus == "x - Cancelled" ~ "No Show",
    TRUE ~ apptslotstatus  # Keep other values unchanged
  ))
data_2024 <- data_2024 %>%
  mutate(apptslotstatus = case_when(
    apptslotstatus == "3 - Checked Out" ~ "Completed",
    apptslotstatus == "4 - Charge Entered" ~ "Completed",
    apptslotstatus == "x - Cancelled" ~ "No Show",
    TRUE ~ apptslotstatus  # Keep other values unchanged
  ))

# time categorization into ranges (AM vs PM)
sort(unique(data_2023$apptstarttime))
sort(unique(data_2024$apptstarttime))
# there were some appts 7-8 am saturdays in 2023, categorized as "early AM"
data_2023 <- data_2023 %>%
  mutate(apptstarttime = case_when(
    apptstarttime < as_hms("08:00:00") ~ "Early AM",
    apptstarttime >= as_hms("08:00:00") & apptstarttime < as_hms("12:00:00") ~ "AM",
    apptstarttime >= as_hms("12:00:00") ~ "PM"
  ))
data_2024 <- data_2024 %>%
  mutate(apptstarttime = case_when(
    apptstarttime < as_hms("12:00:00") ~ "AM",
    apptstarttime >= as_hms("12:00:00") ~ "PM"
  ))
unique(data_2023$apptstarttime)
unique(data_2024$apptstarttime)

# Reformat months to have only the first letter capitalized
data_2023$apptmnth <- str_to_title(str_to_lower(data_2023$apptmnth))
data_2024$apptmnth <- str_to_title(str_to_lower(data_2024$apptmnth))
data_2023$apptmnth <- str_trim(data_2023$apptmnth)
data_2024$apptmnth <- str_trim(data_2024$apptmnth)

# get rid of extra spaces in day
data_2023$apptday <- str_trim(data_2023$apptday)
data_2024$apptday <- str_trim(data_2024$apptday)


### Run the descriptive stats before the following too!!
# Find the unique levels across both data frames for `apptmnth` and `apptstarttime`
all_month_levels <- union(levels(data_2023$apptmnth), levels(data_2024$apptmnth))
all_time_levels <- union(levels(data_2023$apptstarttime), levels(data_2024$apptstarttime))

# Apply the standardized levels to both data frames
data_2023$apptmnth <- factor(data_2023$apptmnth, levels = all_month_levels, ordered = TRUE)
data_2024$apptmnth <- factor(data_2024$apptmnth, levels = all_month_levels, ordered = TRUE)

data_2023$apptstarttime <- factor(data_2023$apptstarttime, levels = all_time_levels, ordered = TRUE)
data_2024$apptstarttime <- factor(data_2024$apptstarttime, levels = all_time_levels, ordered = TRUE)

combined_data <- bind_rows(data_2023, data_2024) # 13443 x 7

## Compute descriptive stats for 2023
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
### Then continue

# Combine Fam Med and Internal Med into primary care
unique(data_2024$appt.schdlng.prvdr.spclty)
data_2024 <- data_2024 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    appt.schdlng.prvdr.spclty %in% c("Internal Medicine", "Family Medicine") ~ "Primary Care",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep other specialties unchanged
  ))
data_2023 <- data_2023 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    appt.schdlng.prvdr.spclty %in% c("Internal Medicine", "Family Medicine") ~ "Primary Care",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep other specialties unchanged
  ))
unique(data_2024$appt.schdlng.prvdr.spclty)
unique(data_2023$appt.schdlng.prvdr.spclty)

# and all pediatric subspecialties under pediatric umbrella
data_2024 <- data_2024 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    grepl("Pediat", appt.schdlng.prvdr.spclty, ignore.case = TRUE) ~ "Pediatric Medicine",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep other specialties unchanged
  ))
data_2023 <- data_2023 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    grepl("Pediat", appt.schdlng.prvdr.spclty, ignore.case = TRUE) ~ "Pediatric Medicine",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep other specialties unchanged
  ))
unique(data_2024$appt.schdlng.prvdr.spclty)
unique(data_2023$appt.schdlng.prvdr.spclty)

# and all surgical subspecialties under "Surgery"
data_2024 <- data_2024 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    grepl("Surgery", appt.schdlng.prvdr.spclty, ignore.case = TRUE) ~ "Surgery",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep other specialties unchanged
  ))
data_2023 <- data_2023 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    grepl("Surgery", appt.schdlng.prvdr.spclty, ignore.case = TRUE) ~ "Surgery",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep other specialties unchanged
  ))
unique(data_2024$appt.schdlng.prvdr.spclty)
data_2024 <- data_2024 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    appt.schdlng.prvdr.spclty %in% c("Podiatry") ~ "Surgery",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep other specialties unchanged
  ))
data_2023 <- data_2023 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    appt.schdlng.prvdr.spclty %in% c("Podiatry") ~ "Surgery",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep other specialties unchanged
  ))
data_2024 <- data_2024 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    appt.schdlng.prvdr.spclty %in% c("Surgery") ~ "Surgical Care",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep other specialties unchanged
  ))
data_2023 <- data_2023 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    appt.schdlng.prvdr.spclty %in% c("Surgery") ~ "Surgical Care",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep other specialties unchanged
  ))
unique(data_2024$appt.schdlng.prvdr.spclty)
unique(data_2023$appt.schdlng.prvdr.spclty)

# and all medical subspecialties under "Medical Specialties"
data_2024 <- data_2024 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    appt.schdlng.prvdr.spclty %in% c("Cardiology", "Pulmonary Medicine", "Gastroenterology", "Rheumatology", "Allergy/Immunology", "Neurology", "Nephrology", "Infectious Disease", "Dermatology", "Endocrinology") ~ "Medical Specialties",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep other specialties unchanged
  ))
data_2023 <- data_2023 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    appt.schdlng.prvdr.spclty %in% c("Cardiology", "Pulmonary Medicine", "Gastroenterology", "Rheumatology", "Allergy/Immunology", "Neurology", "Nephrology", "Infectious Disease", "Dermatology") ~ "Medical Specialties",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep other specialties unchanged
  ))
unique(data_2024$appt.schdlng.prvdr.spclty)
unique(data_2023$appt.schdlng.prvdr.spclty)

# and diagnostic specialties
data_2024 <- data_2024 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    appt.schdlng.prvdr.spclty %in% c("Laboratory Medicine/Diagnostics", "Diagnostic Radiology") ~ "Diagnostic Specialties",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep other specialties unchanged
  ))
data_2023 <- data_2023 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    appt.schdlng.prvdr.spclty %in% c("Laboratory Medicine/Diagnostics", "Diagnostic Radiology") ~ "Diagnostic Specialties",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep other specialties unchanged
  ))
unique(data_2024$appt.schdlng.prvdr.spclty)
unique(data_2023$appt.schdlng.prvdr.spclty)

# rename psych and beh health
data_2024 <- data_2024 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    appt.schdlng.prvdr.spclty %in% c("Psychiatry/Psychology") ~ "Psychiatry & Behavioral Health",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep other specialties unchanged
  ))
data_2023 <- data_2023 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    appt.schdlng.prvdr.spclty %in% c("Psychiatry/Psychology") ~ "Psychiatry & Behavioral Health",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep other specialties unchanged
  ))
unique(data_2024$appt.schdlng.prvdr.spclty)
unique(data_2023$appt.schdlng.prvdr.spclty)

# rename women's health
data_2024 <- data_2024 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    appt.schdlng.prvdr.spclty %in% c("Gynecology") ~ "Women's Health",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep other specialties unchanged
  ))
data_2023 <- data_2023 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    appt.schdlng.prvdr.spclty %in% c("Gynecology") ~ "Women's Health",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep other specialties unchanged
  ))
unique(data_2024$appt.schdlng.prvdr.spclty)
unique(data_2023$appt.schdlng.prvdr.spclty)

# rename eye stuff
data_2024 <- data_2024 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    appt.schdlng.prvdr.spclty %in% c("Ophthalmology/Optometry") ~ "Eye & Vision Care",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep other specialties unchanged
  ))
data_2023 <- data_2023 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    appt.schdlng.prvdr.spclty %in% c("Ophthalmology/Optometry") ~ "Eye & Vision Care",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep other specialties unchanged
  ))
unique(data_2024$appt.schdlng.prvdr.spclty)
unique(data_2023$appt.schdlng.prvdr.spclty)

# rename ent stuff
data_2024 <- data_2024 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    appt.schdlng.prvdr.spclty %in% c("Otolaryngology/Audiology") ~ "Otolaryngology & Audiologic Care",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep other specialties unchanged
  ))
data_2023 <- data_2023 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    appt.schdlng.prvdr.spclty %in% c("Otolaryngology/Audiology") ~ "Otolaryngology & Audiologic Care",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep other specialties unchanged
  ))
unique(data_2024$appt.schdlng.prvdr.spclty)
unique(data_2023$appt.schdlng.prvdr.spclty)

# group rehab and supportive care
data_2024 <- data_2024 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    appt.schdlng.prvdr.spclty %in% c("Wound Care", "Occupational Therapy", "Physical Therapy", "Speech Language Pathology", "Nutrition") ~ "Rehabilitative & Supportive Care",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep other specialties unchanged
  ))
data_2023 <- data_2023 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    appt.schdlng.prvdr.spclty %in% c("Wound Care", "Occupational Therapy", "Physical Therapy", "Speech Language Pathology", "Nutrition") ~ "Rehabilitative & Supportive Care",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep other specialties unchanged
  ))
unique(data_2024$appt.schdlng.prvdr.spclty)
unique(data_2023$appt.schdlng.prvdr.spclty)

# rename acupuncture
data_2024 <- data_2024 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    appt.schdlng.prvdr.spclty %in% c("Acupuncture/Herbal Medicine") ~ "Complementary & Alternative Medicine",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep other specialties unchanged
  ))
data_2023 <- data_2023 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    appt.schdlng.prvdr.spclty %in% c("Acupuncture/Herbal Medicine") ~ "Complementary & Alternative Medicine",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep other specialties unchanged
  ))
unique(data_2024$appt.schdlng.prvdr.spclty)
unique(data_2023$appt.schdlng.prvdr.spclty)


# refactor into categorical vars, ordinal if necessary
# complete vs no show = nominal
data_2023$apptslotstatus <- factor(data_2023$apptslotstatus)
data_2024$apptslotstatus <- factor(data_2024$apptslotstatus)
# specialty = nominal
data_2023$appt.schdlng.prvdr.spclty <- factor(data_2023$appt.schdlng.prvdr.spclty)
data_2024$appt.schdlng.prvdr.spclty <- factor(data_2024$appt.schdlng.prvdr.spclty)
# apptday = ordinal (because subgrouping weekdays too)
data_2023$apptday <- factor(data_2023$apptday, 
                            levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
                            ordered = TRUE)
data_2024$apptday <- factor(data_2024$apptday, 
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

# overall: chi sdq test for if no show appt is independent of appointment day
# Convert `apptday` to Weekday/Saturday binary
combined_data$day_type <- ifelse(combined_data$apptday == "Saturday", "Saturday", "Weekday")

# Create contingency table
table_no_show <- table(combined_data$day_type, combined_data$apptslotstatus)

# Perform Chi-Square Test
chi_square_result <- chisq.test(table_no_show)
chi_square_result

# # Check if degrees of freedom is available
# chi_sq_table <- data.frame(
#   Statistic = ifelse(is.na(chi_square_result$statistic), "NA (Monte Carlo Simulation)", chi_square_result$statistic),
#   DF = ifelse(is.na(chi_square_result$parameter), "-", chi_square_result$parameter),
#   p_value = chi_square_result$p.value
# )

# Install if not already installed
# install.packages("gt")

library(gt)

# Extract Chi-Square test values
chi_sq_table <- data.frame(
  Statistic = chi_square_result$statistic,
  DF = chi_square_result$parameter,
  p_value = chi_square_result$p.value
)

# Format the p-value for readability
chi_sq_table <- chi_sq_table %>%
  dplyr::mutate(p_value = scales::pvalue(p_value, accuracy = 0.001))

# Create the gt table
chi_sq_table_gt <- chi_sq_table %>%
  gt() %>%
  tab_header(
    title = "Chi-Square Test Results: 2023 - 2024",
    subtitle = "Association between Day Type and No-Show Status"
  ) %>%
  fmt_number(
    columns = c(Statistic, DF),
    decimals = 2
  ) %>%
  fmt_number(
    columns = p_value,
    decimals = 3
  ) %>%
  cols_label(
    Statistic = "Chi-Square Statistic",
    DF = "Degrees of Freedom",
    p_value = "p-Value"
  ) %>%
  tab_options(
    table.font.size = "medium",
    table.width = pct(60)
  )

# Display the table
chi_sq_table_gt

## now by all days
# Create the contingency table
no_show_table <- table(combined_data$apptday, combined_data$apptslotstatus)

# Display the table to ensure it looks correct
print(no_show_table)

# Run the Chi-Square test
chi_square_result <- chisq.test(no_show_table)

# Display the result
chi_square_result

# Create a summary table for Chi-Square results
chi_square_summary <- data.frame(
  Statistic = round(chi_square_result$statistic, 2),
  DF = chi_square_result$parameter,
  p_value = formatC(chi_square_result$p.value, format = "e", digits = 3)
)

# Display the summary table
chi_square_summary

library(glue)

# Create and format the gt table
chi_square_summary %>%
  gt() %>%
  tab_header(
    title = "No-Show Rates vs. Day of the Week, 2023-2024"
  ) %>%
  fmt_number(
    columns = c(Statistic, p_value),
    decimals = 2
  ) %>%
  fmt_number(
    columns = DF,
    decimals = 0
  ) %>%
  cols_label(
    p_value = "P-Value"
  ) %>%
  cols_label(
    Statistic = "Chi-Square Statistic"
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = everything())
  )

#########

# Prepare data for plotting: calculate no-show rate for each day of the week
no_show_rate_by_day <- combined_data %>%
  group_by(apptday) %>%
  summarise(
    no_show_rate = mean(apptslotstatus == "No Show") * 100,  # No-show rate in percentage
    total_visits = n()
  )

# Conduct Chi-square test for day of week vs no-show rate
no_show_table <- table(combined_data$apptday, combined_data$apptslotstatus)
chi_square_test <- chisq.test(no_show_table)

# Create a bar plot with no-show rate by day of the week
ggplot(no_show_rate_by_day, aes(x = apptday, y = no_show_rate)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(
    title = "No-Show Rate by Day of the Week",
    x = "Day of the Week",
    y = "No-Show Rate (%)"
  ) +
  theme_minimal() +
  geom_text(aes(label = sprintf("%.1f%%", no_show_rate)), vjust = -0.5, size = 4) +
  annotate(
    "text", x = 4, y = max(no_show_rate_by_day$no_show_rate) + 5,
    label = paste0("Chi-squared: ", round(chi_square_test$statistic, 2), 
                   "\nDF: ", chi_square_test$parameter, 
                   "\nP-value: ", format.pval(chi_square_test$p.value, digits = 3)),
    hjust = 0.5, size = 4, color = "red"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )


#############
# Load necessary packages
library(ggplot2)
library(rcompanion) # for pairwiseNominalIndependence
library(dplyr)

# Step 1: Create the contingency table
no_show_table <- combined_data %>%
  count(apptday, apptslotstatus) %>%
  tidyr::pivot_wider(names_from = apptslotstatus, values_from = n, values_fill = 0)

# Step 2: Run pairwise Chi-squared tests with Bonferroni correction
# Run pairwise comparisons using the pairwiseNominalIndependence function
pairwise_results <- pairwiseNominalIndependence(
  table(combined_data$apptday, combined_data$apptslotstatus),
  method = "bonferroni"
)

# Extract significant results for plotting
significant_comparisons <- pairwise_results %>%
  filter(p.adj.Chisq < 0.05) %>%
  mutate(
    day1 = str_extract(Comparison, "^[^:]+"),      # Extract first day from the pair
    day2 = str_extract(Comparison, "(?<=:).*")     # Extract second day from the pair
  )

# Step 3: Calculate no-show rate by day
no_show_rate_by_day <- combined_data %>%
  group_by(apptday) %>%
  summarise(
    no_show_rate = mean(apptslotstatus == "No Show") * 100,  # No-show rate as percentage
    total_visits = n()
  )

# Step 4: Create the base bar plot
p <- ggplot(no_show_rate_by_day, aes(x = apptday, y = no_show_rate, fill = apptday)) +
  geom_bar(stat = "identity") +
  labs(title = "No-Show Rates by Day of the Week", y = "No-Show Rate (%)", x = "Day of the Week") +
  theme_minimal()

# Step 5: Add annotations for each significant comparison
library(stringr)  # For str_trim

# Clean up day names in both significant_comparisons and no_show_rate_by_day
significant_comparisons <- significant_comparisons %>%
  mutate(
    day1 = str_trim(day1),     # Trim any whitespace around day names
    day2 = str_trim(day2)
  )

no_show_rate_by_day <- no_show_rate_by_day %>%
  mutate(apptday = str_trim(apptday))


library(ggplot2)
library(dplyr)

# # Define the order of the days
# no_show_rate_by_day$apptday <- factor(no_show_rate_by_day$apptday, 
#                                       levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
# 
# # Calculate maximum y value for scaling with a dynamic buffer
# max_y_value <- max(no_show_rate_by_day$no_show_rate) + (nrow(significant_comparisons) * 1.6)
# 
# # Create the bar plot with ordered days, adjusted title, and y-axis limits
# p <- ggplot(no_show_rate_by_day, aes(x = apptday, y = no_show_rate, fill = apptday)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(title = "No-Show Rate by Appointment Day, 2023-2024", x = "Appointment Day", y = "No-show Rate (%)") +
#   scale_y_continuous(limits = c(0, max_y_value)) +
#   theme_minimal() +
#   theme(
#     legend.position = "none",
#     plot.title = element_text(hjust = 0.5, size = 16, face = "bold")  # Center and enlarge title
#   )
# 
# # Adjust the positioning of p-values to avoid overlap
# position_offset <- 1.5  # Adjust the gap between lines as needed
# 
# # Loop through significant comparisons to add annotations
# for (i in 1:nrow(significant_comparisons)) {
#   day1 <- significant_comparisons$day1[i]
#   day2 <- significant_comparisons$day2[i]
#   p_val_label <- paste("p =", format(significant_comparisons$p.adj.Chisq[i], digits = 3))
#   
#   # Get y-positions for day1 and day2
#   day1_pos <- no_show_rate_by_day$no_show_rate[match(day1, no_show_rate_by_day$apptday)]
#   day2_pos <- no_show_rate_by_day$no_show_rate[match(day2, no_show_rate_by_day$apptday)]
#   
#   # Skip if any position is missing
#   if (is.na(day1_pos) | is.na(day2_pos)) {
#     warning(paste("Could not find one or both days:", day1, day2))
#     next
#   }
#   
#   # Define maximum y position with offset to prevent overlap
#   max_y <- max(day1_pos, day2_pos) + i * position_offset
#   
#   # Add annotation lines and p-values
#   p <- p +
#     annotate("segment", x = match(day1, no_show_rate_by_day$apptday), 
#              xend = match(day2, no_show_rate_by_day$apptday), 
#              y = max_y, yend = max_y, color = "black") +
#     annotate("text", x = mean(c(match(day1, no_show_rate_by_day$apptday), 
#                                 match(day2, no_show_rate_by_day$apptday))),
#              y = max_y + 0.5, label = p_val_label, size = 3)
# }
# 
# # Display the plot with annotations
# print(p)

# Define the order of the days
no_show_rate_by_day$apptday <- factor(no_show_rate_by_day$apptday, 
                                      levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Set a slightly higher max y-value for sufficient spacing above the bars
max_y_value <- max(no_show_rate_by_day$no_show_rate) * 2

# Create the bar plot with ordered days, adjusted title, and y-axis limits
p <- ggplot(no_show_rate_by_day, aes(x = apptday, y = no_show_rate, fill = apptday)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "No-Show Rate by Appointment Day, 2023-2024", x = "Appointment Day", y = "No-show Rate (%)") +
  scale_y_continuous(limits = c(0, max_y_value)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")  # Center and enlarge title
  ) +
  geom_text(aes(label = sprintf("%.1f%%", no_show_rate)), vjust = -0.5, size = 3.5)  # Labels above each bar

# Adjusted vertical space for p-values
spacing_above_bars <- max(no_show_rate_by_day$no_show_rate) + 2
spacing_increment <- 1.5  # Reduced increment to fit within the expanded axis

# Loop through significant comparisons and add annotations
for (i in 1:nrow(significant_comparisons)) {
  day1 <- significant_comparisons$day1[i]
  day2 <- significant_comparisons$day2[i]
  p_val_label <- paste("p =", format(significant_comparisons$p.adj.Chisq[i], digits = 3))
  
  # Get x-positions for day1 and day2
  day1_pos <- match(day1, no_show_rate_by_day$apptday)
  day2_pos <- match(day2, no_show_rate_by_day$apptday)
  
  # Skip if any position is missing
  if (is.na(day1_pos) | is.na(day2_pos)) {
    warning(paste("Could not find one or both days:", day1, day2))
    next
  }
  
  # Define y-position for the line and text, adjusting by `spacing_increment`
  y_pos <- spacing_above_bars + (i - 1) * spacing_increment
  
  # Add annotation lines and p-value labels, adjusting label position slightly above the line
  p <- p +
    annotate("segment", x = day1_pos, xend = day2_pos, y = y_pos, yend = y_pos, color = "black") +
    annotate("text", x = mean(c(day1_pos, day2_pos)), y = y_pos + 0.5, label = p_val_label, size = 3)
}

# Display the plot with annotations
print(p)

## logistic regression: overall
# Basic logistic regression with a single predictor
# binomial family tomodel the regression
# Convert the response variable to a factor
combined_data <- combined_data %>%
  mutate(apptslotstatus = factor(apptslotstatus, levels = c("Completed", "No Show")))
# day of week
logistic_overall_days <- glm(apptslotstatus ~ apptday, data = combined_data, family = binomial)
summary(logistic_overall_days)
# start time
logistic_overall_time <- glm(apptslotstatus ~ apptstarttime, data = combined_data, family = binomial)
summary(logistic_overall_time) # not significant
# month
logistic_overall_months <- glm(apptslotstatus ~ apptmnth, data = combined_data, family = binomial)
summary(logistic_overall_months) # significant in quadratic
# specialty
logistic_overall_specialty <- glm(apptslotstatus ~ appt.schdlng.prvdr.spclty, data = combined_data, family = binomial)
summary(logistic_overall_specialty) # significant for most

# plots: predicted probability, effect plot, odds ratio plots
library(ggeffects)  # For generating predictions with ggplot-compatible output
# day of week
# Generate predicted probabilities by appointment day
predicted_probs_by_day <- ggpredict(logistic_overall_days, terms = "apptday")
plot(predicted_probs_by_day)
# Plot predicted probabilities
ggplot(predicted_probs_by_day, aes(x = x, y = predicted)) +
  geom_point(size = 3) +
  geom_line(group = 1) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # Add confidence intervals
  
  # Convert y-axis to percentage format
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  
  # Adjust title, axis labels, and spacing
  labs(
    title = "Predicted Probability of No-Show by Day of the Week, 2023-2024",
    x = "Appointment Day",
    y = "Predicted Probability of No-Show (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center and enlarge title
    axis.title.x = element_text(margin = margin(t = 10)),  # Add space above x-axis title
    axis.title.y = element_text(margin = margin(r = 10)),   # Add space to the right of y-axis title
    
    # Make axes solid lines
    # panel.grid = element_blank(),  # Remove background grid lines
    panel.border = element_rect(color = "grey", fill = NA, linewidth = 1)  # Add solid border
  )

# month
predicted_probs_by_month <- ggpredict(logistic_overall_months, terms = "apptmnth")
plot(predicted_probs_by_month)

ggplot(predicted_probs_by_month, aes(x = x, y = predicted)) +
  geom_point(size = 3) +
  geom_line(group = 1) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # Add confidence intervals
  
  # Convert y-axis to percentage format
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  
  # Adjust title, axis labels, and spacing
  labs(
    title = "Predicted Probability of No-Show by Month, 2023-2024",
    x = "Appointment Month",
    y = "Predicted Probability of No-Show (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center and enlarge title
    axis.title.x = element_text(margin = margin(t = 10)),  # Add space above x-axis title
    axis.title.y = element_text(margin = margin(r = 10)),   # Add space to the right of y-axis title
    
    # Make axes solid lines
    # panel.grid = element_blank(),  # Remove background grid lines
    panel.border = element_rect(color = "grey", fill = NA, linewidth = 1)  # Add solid border
  )

# and specialty
predicted_probs_by_specialty <- ggpredict(logistic_overall_specialty, terms = "appt.schdlng.prvdr.spclty")
plot(predicted_probs_by_specialty)
ggplot(predicted_probs_by_specialty, aes(x = x, y = predicted)) +
  geom_point(size = 3) +
  # geom_line(group = 1) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # Add confidence intervals
  
  # Convert y-axis to percentage format
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  
  # Adjust title, axis labels, and spacing
  labs(
    title = "Predicted Probability of No-Show by Specialty, 2023-2024",
    x = "Appointment Specialty",
    y = "Predicted Probability of No-Show (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center and enlarge title
    axis.title.x = element_text(margin = margin(t = 10)),  # Add space above x-axis title
    axis.title.y = element_text(margin = margin(r = 10)),   # Add space to the right of y-axis title
    
    # Rotate x-axis labels for readability
    axis.text.x = element_text(angle = 60, hjust = 1),  # Rotate labels to 45 degrees
    
    # Make axes solid lines
    # panel.grid = element_blank(),  # Remove background grid lines
    panel.border = element_rect(color = "grey", fill = NA, linewidth = 1)  # Add solid border
  )


# effect plot -- marginal effect of each predictor on the probability of no show
library(carData)
library(effects)
# basically showing how each catefory of apptday affects the no show prob holding other factors constant
# create multivar logistic regression
logistic_model <- glm(apptslotstatus ~ apptday + apptmnth + appt.schdlng.prvdr.spclty + apptstarttime, 
                      data = combined_data, family = binomial)
# Generate effect plot for the logistic regression model
plot(allEffects(logistic_model),
     axes = list(x = list(rot = 45, cex = 0.8),   # Rotate x-axis labels 45 degrees, reduce font size
                 y = list(cex = 0.8)),             # Reduce font size for y-axis labels
     main.title = list(cex = 1.2),                 # Title font size
     lattice = list(strip = list(par.strip.text = list(cex = 1)))  # Panel label size
    )
# based on what this above plot looks like, i chose fixed effects = day and month, random effect = specialty and start time
# this is done later in the code

# odds ratios with confidence intervals
library(broom)
# Extract odds ratios and confidence intervals
odds_ratios <- tidy(logistic_model, exponentiate = TRUE, conf.int = TRUE)
odds_ratios <- odds_ratios %>%
  filter(!is.na(estimate) & !is.na(conf.low) & !is.na(conf.high))

# Plot odds ratios
ggplot(odds_ratios, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Odds Ratios for No-Show, multivar regression",
       x = "Predictor", y = "Odds Ratio (95% CI)") +
  theme_minimal()

# # facet plot of pred probs across multiple predictors
# # Generate predicted probabilities across day and month
# predicted_data_facet <- ggpredict(logistic_model, terms = c("apptday", "apptmnth", "appt.schdlng.prvdr.spclty", "apptstarttime"))
# 
# # Facet plot of predicted probabilities
# ggplot(predicted_data_facet, aes(x = x, y = predicted, color = group)) +
#   geom_line() +
#   facet_wrap(~facet, scales = "free") +
#   labs(title = "Predicted Probability of No-Show by Day/Month/Specialty/Start Time",
#        x = "Appointment Day", y = "Predicted Probability of No-Show") +
#   theme_minimal()

# partial dependence plot
# If you are using more complex models or want to isolate the effect of one predictor, 
# partial dependence plots can show the effect of a predictor on the response variable, 
# averaging over all other predictors.
# TODO bootstrap some confidence intervals on deez hoes
library(pdp)
# Partial dependence plot for appointment day
partial_plot_day <- partial(logistic_model, pred.var = "apptday", prob = TRUE)
# Plot
autoplot(partial_plot_day) +
  labs(title = "Partial Dependence Plot for Appointment Day",
       x = "Appointment Day", y = "Predicted Probability of No-Show") +
  theme_minimal()
# for month
partial_plot_month <- partial(logistic_model, pred.var = "apptmnth", prob = TRUE)
autoplot(partial_plot_month) +
  labs(title = "Partial Dependence Plot for Appointment Month",
       x = "Appointment Month", y = "Predicted Probability of No-Show") +
  theme_minimal()
# start time
partial_plot_time <- partial(logistic_model, pred.var = "apptstarttime", prob = TRUE)
autoplot(partial_plot_time) +
  labs(title = "Partial Dependence Plot for Appointment Start Time",
       x = "Appointment Start Time", y = "Predicted Probability of No-Show") +
  theme_minimal()
# specialty
partial_plot_specialty <- partial(logistic_model, pred.var = "appt.schdlng.prvdr.spclty", prob = TRUE)
autoplot(partial_plot_specialty) +
  labs(title = "Partial Dependence Plot for Appointment Specialty",
       x = "Appointment Specialty", y = "Predicted Probability of No-Show") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center and enlarge title
    axis.title.x = element_text(margin = margin(t = 10)),  # Add space above x-axis title
    axis.title.y = element_text(margin = margin(r = 10)),   # Add space to the right of y-axis title
    
    # Rotate x-axis labels for readability
    axis.text.x = element_text(angle = 60, hjust = 1),  # Rotate labels to 45 degrees
    
    # Make axes solid lines
    # panel.grid = element_blank(),  # Remove background grid lines
    panel.border = element_rect(color = "grey", fill = NA, linewidth = 1)  # Add solid border
  )

# and some post-hoc tests TODO make figures maybe
library(emmeans)
emmeans_results_days <- emmeans(logistic_model, pairwise ~ apptday, adjust = "bonferroni")
summary(emmeans_results_days$contrasts)
emmeans_results_months <- emmeans(logistic_model, pairwise ~ apptmnth, adjust = "bonferroni")
summary(emmeans_results_months$contrasts)
emmeans_results_time <- emmeans(logistic_model, pairwise ~ apptstarttime, adjust = "bonferroni")
summary(emmeans_results_time$contrasts)
emmeans_results_specialty <- emmeans(logistic_model, pairwise ~ appt.schdlng.prvdr.spclty, adjust = "bonferroni")
summary(emmeans_results_specialty$contrasts)


## mixed effects logistic regression
# based on effects plot from multivar reg, i chose fixed effects = day and month, random effect = specialty and start time
# account for variability of specialty and appt time, analyze no show rates capturing fixed patterns across days and months
library(lme4)
# Constructing the mixed-effects logistic regression model
mixed_model <- glmer(
  apptslotstatus == "No Show" ~ 
    apptday + apptmnth +          # Fixed effects
    (1 | appt.schdlng.prvdr.spclty) +  # Random effect for specialty
    (1 | apptstarttime),          # Random effect for start time
  data = combined_data,
  family = binomial
)
summary(mixed_model)
plot(allEffects(mixed_model)) # to visualize fixed effects

# plots: marginal effects, predicted probabilities







### TODO: now just 2023, 2024 (u of a started in jan, mayo in feb)
# TODO remove rad onc first
# # Remove rows where appt.schdlng.prvdr.spclty is "Radiation Oncology"
# combined_data <- combined_data %>%
#   filter(appt.schdlng.prvdr.spclty != "Radiation Oncology")


# TODO: anova (or kruskal wallis) from the predicted probabilities of the logistic regressions?

### compare no show rates within primary care only, weekdays vs saturdays
### and between student run clinics (using kruskal wallis)
unique((combined_data %>%
         filter(apptday == "Saturday"))$appt.schdlng.prvdr)

saturdays_all <- combined_data %>%
  filter(apptday == "Saturday")
unique(saturdays_all$appt.schdlng.prvdr)

provider_counts <- saturdays_all %>%
  count(appt.schdlng.prvdr, name = "count") %>%
  arrange(desc(count))

provider_specialty_counts <- saturdays_all %>%
  group_by(appt.schdlng.prvdr, appt.schdlng.prvdr.spclty) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

saturdays_creighton <- combined_data %>%
  filter(apptday == "Saturday" & 
           (appt.schdlng.prvdr == "CREIGHTON_IM" | appt.schdlng.prvdr == "CREIGHTON_FM" | appt.schdlng.prvdr == "CREIGHTON_PEDS" | appt.schdlng.prvdr == "HAZIN_M"))
creighton_no_show_prop <- sum(saturdays_creighton$apptslotstatus == "No Show") / nrow(saturdays_creighton)

saturdays_mayo <- combined_data %>%
  filter(apptday == "Saturday" & 
           (appt.schdlng.prvdr == "MAYO_IM" | appt.schdlng.prvdr == "MAYO_FM" | appt.schdlng.prvdr == "MAYO_GYNE"))
mayo_no_show_prop <- sum(saturdays_mayo$apptslotstatus == "No Show") / nrow(saturdays_mayo)

saturdays_uofa <- combined_data %>%
  filter(apptday == "Saturday" & appt.schdlng.prvdr == "UOFA_STUDENT_CLINIC")
uofa_no_show_prop <- sum(saturdays_uofa$apptslotstatus == "No Show") / nrow(saturdays_uofa)

weekday_primary_care <- combined_data %>%
  filter(apptday != "Saturday" & appt.schdlng.prvdr.spclty == "Primary Care")
pc_no_show_prop <- sum(weekday_primary_care$apptslotstatus == "No Show") / nrow(weekday_primary_care)

provider_vector <- c("Weekday Primary Care", "Creighton Saturday Clinic", "Mayo Saturday Clinic", "U of A Saturday Clinic")
no_show_props_vector <- c(pc_no_show_prop, creighton_no_show_prop, mayo_no_show_prop, uofa_no_show_prop)

ns_df <- data.frame(Column1 = provider_vector, Column2 = no_show_props_vector)
colnames(ns_df) <- c("Provider", "No-Show Proportion")
ns_df$`No-Show Count` <- c(sum(weekday_primary_care$apptslotstatus == "No Show"),
                           sum(saturdays_creighton$apptslotstatus == "No Show"),
                           sum(saturdays_mayo$apptslotstatus == "No Show"),
                           sum(saturdays_uofa$apptslotstatus == "No Show"))
ns_df$`Total Appointment Count` <- c(nrow(weekday_primary_care),
                                     nrow(saturdays_creighton),
                                     nrow(saturdays_mayo),
                                     nrow(saturdays_uofa))

## checking normality
# library(ggpubr)
# ns_df <- ns_df %>%
#   dplyr::rename(No_Show_Proportion = `No-Show Proportion`)
# ns_df <- ns_df %>%
#   dplyr::rename(No_Show_Count = `No-Show Count`)
# ns_df <- ns_df %>%
#   dplyr::rename(Total_Count = `Total Appointment Count`)
# ggqqplot(ns_df, x = "No_Show_Proportion", facet.by = "Provider")


## Fisher's exact test, since there are some groups with < 5 no shows
ns_df$No_Show_Proportion <- NULL

library(rstatix)
library(tidyr)

# Step 0: Add No-Show Proportion Column
# ns_df <- ns_df %>%
#   mutate(No_Show_Proportion = No_Show_Count / Total_Count)

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

# # Step 3: Adjust p-values using Bonferroni correction
# pairwise_results <- pairwise_results %>%
#   mutate(p_adj = p.adjust(p_value, method = "bonferroni")) %>%
#   # filter(p_adj < 0.05) %>%  # Only keep significant comparisons
#   mutate(label = paste("p =", format(p_adj, digits = 3)))

# Assuming `pairwise_results` has columns `Provider1`, `Provider2`, and `p_adj`
# Ensure that `pairwise_results` has these columns from the pairwise test results
pairwise_results <- pairwise_results %>%
  mutate(
    Provider1 = str_extract(comparison, "^[^ ]+.*?(?= vs)"),    # Extract first provider
    Provider2 = str_extract(comparison, "(?<=vs ).*$"),  # Extract second provider
    p_adj = p.adjust(p_value, method = "BH"),
    label = paste("p =", format(p_adj, digits = 3))
  )

# # Plotting with all p-values
# ggplot(ns_df, aes(x = Provider, y = No_Show_Proportion, fill = Provider)) +
#   geom_bar(stat = "identity") +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   labs(
#     title = "No-Show Proportion by Provider",
#     x = "Provider",
#     y = "No-Show Proportion (%)"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "none") +
#   
#   # Add Pairwise Comparison Annotations
#   geom_text(
#     data = pairwise_results,
#     aes(x = 2.5, y = max(ns_df$No_Show_Proportion) + 0.02, label = label),  # Adjust x/y as needed
#     color = "red",
#     inherit.aes = FALSE,
#     size = 3
#   )

# # Define provider positions for plotting
# provider_positions <- setNames(1:length(unique(ns_df$Provider)), unique(ns_df$Provider))
# 
# # Bar plot with significant pairwise comparisons annotated
# ggplot(ns_df, aes(x = Provider, y = No_Show_Proportion, fill = Provider)) +
#   geom_bar(stat = "identity") +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   labs(
#     title = "No-Show Proportion by Provider",
#     x = "Provider",
#     y = "No-Show Proportion (%)"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "none") +
#   
#   # Add Pairwise Comparison Annotations
#   geom_text(
#     data = pairwise_results, #%>% filter(p_adj < 0.05),  # Only display significant comparisons
#     aes(
#       x = (provider_positions[Provider1] + provider_positions[Provider2]) / 2,  # Midpoint between providers
#       y = max(ns_df$No_Show_Proportion) + 0.02 * seq_along(p_adj),  # Staggered y-position to avoid overlap
#       label = paste("p =", format(p_adj, digits = 3))
#     ),
#     color = "red",
#     inherit.aes = FALSE,
#     size = 3
#   )

# Plot the bar chart
# p <- ggplot(ns_df, aes(x = Provider, y = No_Show_Proportion, fill = Provider)) +
#   geom_bar(stat = "identity") +
#   labs(title = "No-Show Proportion by Provider", x = "Provider", y = "No-Show Proportion (%)") +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   )

# Set the order of providers in the desired order
ns_df$Provider <- factor(ns_df$Provider, levels = c("Weekday Primary Care", "Creighton Saturday Clinic", "Mayo Saturday Clinic", "U of A Saturday Clinic"))

p <- ggplot(ns_df, aes(x = Provider, y = No_Show_Proportion * 100, fill = Provider)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = scales::percent(No_Show_Proportion, accuracy = 0.1)), 
            vjust = -0.7, size = 4) +  # Add proportion labels
  labs(title = "No-Show Proportion by Primary Care Provider", 
       x = "Provider", 
       y = "No-Show Proportion (%)") +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove the legend
    plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  scale_y_continuous(limits = c(0, 30), expand = c(0, 0))  # Adjust y-axis limits if needed

# Add p-value annotations
position_offset <- 3  # Adjust this value for better spacing above bars

# Add annotations 
# for (i in 1:nrow(pairwise_results)) {
#   provider1 <- pairwise_results$Provider1[i]
#   provider2 <- pairwise_results$Provider2[i]
#   p_value <- pairwise_results$p_value[i]
#   p_label <- paste("p =", format(p_value, digits = 3))
#   
#   # Get x-axis positions of the providers
#   x_pos1 <- which(ns_df$Provider == provider1)
#   x_pos2 <- which(ns_df$Provider == provider2)
#   
#   # Get the y-axis (No_Show_Proportion) values for each provider
#   y_pos1 <- ns_df$No_Show_Proportion[ns_df$Provider == provider1]
#   y_pos2 <- ns_df$No_Show_Proportion[ns_df$Provider == provider2]
#   
#   # Set the max y-position above the two bars, with incremental adjustment for each comparison
#   max_y <- max(y_pos1, y_pos2) + (i * position_offset)
#   
#   # Add the line and p-value annotation to the plot
#   p <- p +
#     annotate("segment", x = x_pos1, xend = x_pos2, y = max_y, yend = max_y, color = "black", linewidth = 1.2) +
#     annotate("text", x = mean(c(x_pos1, x_pos2)), y = max_y + 0.01, label = p_label, size = 3, fontface = "bold")
# }

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





###############
### Figures ###
###############
ggplot(combined_data, aes(x = appt.schdlng.prvdr.spclty, y = ..prop.., group = 1)) +
  geom_bar() +
  labs(title = "Proportion of Each Specialty (Grouped), Overall", x = "Specialty", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))

ggplot(data_2024, aes(x = appt.schdlng.prvdr.spclty, y = ..prop.., group = 1)) +
  geom_bar() +
  labs(title = "Proportion of Each Specialty", x = "Specialty", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

