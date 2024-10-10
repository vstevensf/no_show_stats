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

## Compute descriptive stats for 2023
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


###############
### Figures ###
###############
ggplot(data_2024, aes(x = appt.schdlng.prvdr.spclty, y = ..prop.., group = 1)) +
  geom_bar() +
  labs(title = "Proportion of Each Specialty", x = "Specialty", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

