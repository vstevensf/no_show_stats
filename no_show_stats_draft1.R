# V. Stevens 10/9/24
# Data: 2023 and 2024 completed appts and no shows, Mon-Sat
# 2024 data until 10/8/24
# This script completes prelim analysis of no show rates

###############
### Imports ###
###############
# NOTE: run reformatting.R to acquire prior imports and reformatted datasets
# install.packages("tableone") # for summary stats
install.packages("gtsummary") # for summary stats
# library(tableone)
library(gtsummary)

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
    apptstarttime < as_hms("08:00:00") ~ "Early AM",
    apptstarttime >= as_hms("08:00:00") & apptstarttime < as_hms("12:00:00") ~ "AM",
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
                             levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                             ordered = TRUE)
# start time = ordinal
data_2023$apptstarttime <- factor(data_2023$apptstarttime, 
                             levels = c("Early AM", "AM", "PM"),
                             ordered = TRUE)
data_2024$apptstarttime <- factor(data_2024$apptstarttime, 
                                  levels = c("Early AM", "AM", "PM"),
                                  ordered = TRUE)

cols <- colnames(data_2023)
str(data_2023)
str(data_2024)

## Custom function to determine if Fisher's or Chi-squared was used (in CreateTableOne)
check_test_type <- function(var1, var2, data) {
  # Create a contingency table
  tbl <- table(data[[var1]], data[[var2]])
  
  # Perform Chi-squared test to extract expected counts
  chi_sq_test <- chisq.test(tbl)
  
  # Check if any expected count is less than 5
  if (any(chi_sq_test$expected < 5)) {
    return("Fisher's Exact Test")
  } else {
    return("Chi-squared Test")
  }
}

## now for summary stats
strat_vars <- c("appt.schdlng.prvdr.spclty",  "apptstarttime", "apptday", "apptmnth")
for (col in strat_vars) {
  print(table(data_2023[[col]]))
} # high freqs for most categories, prolly will run chi sq over fisher
for (col in strat_vars) {
  print(table(data_2024[[col]]))
} # high freqs for most categories, prolly will run chi sq over fisher
sapply(strat_vars, function(var) check_test_type(var, "apptslotstatus", data_2023)) # some specialties Fishers
sapply(strat_vars, function(var) check_test_type(var, "apptslotstatus", data_2024)) # some specialties Fishers


# compute descriptive stats for 2023
# Compute descriptive stats for 2023
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


###############
### Figures ###
###############
install.packages("ggplot2")
library(ggplot2)
# Check for missing data
summary(data_2023)
sum(is.na(data_2023))

# Visualize the distribution of variables
# TODO: convert to proportional bar plots
ggplot(data_2023, aes(x = apptday, fill = apptslotstatus)) +
  geom_bar(position = "dodge") +
  labs(title = "Appointment Status by Day", x = "Day", y = "Count", fill = "Status")
ggplot(data_2023, aes(x = apptday, fill = apptslotstatus)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +  # Convert y-axis to percentage
  labs(
    title = "Appointment Status Proportions by Day",
    x = "Day of the Week",
    y = "Proportion",
    fill = "Appointment Status"
  ) +
  theme_minimal()  # Optional: Apply a cleaner theme


ggplot(data_2023, aes(x = apptmnth, fill = apptslotstatus)) +
  geom_bar(position = "dodge") +
  labs(title = "Appointment Status by Month", x = "Month", y = "Count", fill = "Status")
ggplot(data_2023, aes(x = apptmnth, fill = apptslotstatus)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +  # Convert y-axis to percentage
  labs(
    title = "Appointment Status Proportions by Month",
    x = "Month",
    y = "Proportion",
    fill = "Appointment Status"
  ) +
  theme_minimal()

ggplot(data_2023, aes(x = appt.schdlng.prvdr.spclty, fill = apptslotstatus)) +
  geom_bar(position = "dodge") +
  labs(title = "Appointment Status by Specialty", x = "Specialty", y = "Count", fill = "Status") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(data_2023, aes(x = appt.schdlng.prvdr.spclty, fill = apptslotstatus)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +  # Convert y-axis to percentage
  labs(
    title = "Appointment Status Proportions by Specialty",
    x = "Specialty",
    y = "Proportion",
    fill = "Appointment Status"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data_2023, aes(x = apptstarttime, fill = apptslotstatus)) +
  geom_bar(position = "dodge") +
  labs(title = "Appointment Status by Start Time", x = "Start Time", y = "Count", fill = "Status")
ggplot(data_2023, aes(x = apptstarttime, fill = apptslotstatus)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +  # Convert y-axis to percentage
  labs(
    title = "Appointment Status Proportions by Start Time",
    x = "Start Time",
    y = "Proportion",
    fill = "Appointment Status"
  ) +
  theme_minimal()





###################
### Regressions ###
###################
# TODO: ignore the following for now, just playing around with regressions


# Load the necessary package
install.packages("lme4")
install.packages("broom")
install.packages("performance")
library(lme4) # hierarchical logistic regression
library(broom) # cleaning model outputs
library(performance) # model diagnostics

# Drop columns 'apptdate' and 'appt.schdlng.prvdr' before analysis
data_2023_clean <- data_2023 %>%
  select(-apptdate, -appt.schdlng.prvdr)

# Create binary outcome for no show vs completed
# Recode 'apptslotstatus' as a binary outcome: 1 = "No Show", 0 = "Completed"
data_2023 <- data_2023 %>%
  mutate(no_show = ifelse(apptslotstatus == "No Show", 1, 0))

# Standard Logistic Regression Model
logistic_model <- glm(
  no_show ~ apptday + apptmnth + apptstarttime + appt.schdlng.prvdr.spclty, 
  data = data_2023_clean, 
  family = binomial
)

summary(logistic_model)

exp(coef(logistic_model))

# Predict the probability of no-show
data_2023_clean$predicted_prob <- predict(logistic_model, type = "response")

# Set a threshold to classify as "No Show" or "Completed"
data_2023_clean$predicted_class <- ifelse(data_2023_clean$predicted_prob > 0.5, 1, 0)

# Confusion matrix
table(Predicted = data_2023_clean$predicted_class, Actual = data_2023_clean$no_show)

# Calculate accuracy
accuracy <- mean(data_2023_clean$predicted_class == data_2023_clean$no_show)
accuracy

# Pseudo R-squared
install.packages("pscl")
library(pscl)
pR2(logistic_model)

# Hosmer-Lemeshow Test
install.packages("ResourceSelection")
library(ResourceSelection)
hoslem.test(data_2023_clean$no_show, fitted(logistic_model))

# Plot predicted probabilities
ggplot(data_2023_clean, aes(x = apptmnth, y = predicted_prob)) +
  geom_boxplot() +
  facet_wrap(~ apptday) +
  labs(title = "Predicted No-Show Probability by Appointment Month and Day", 
       y = "Predicted Probability", x = "Appointment Month")


