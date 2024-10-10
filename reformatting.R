# V. Stevens 10/8/24
# Data: 2023 total no shows, 2023 sat no shows, 2024 total no shows, 2024 sat no shows
# 2024 data until 10/8/24
# This script reformats data for downstream analysis of no show rates

###############
### Imports ###
###############
setwd("C:\\Users\\m296398\\Desktop\\no_show_stats")
install.packages("openxlsx") # for reading/writing excel files
install.packages("dplyr") # for data manipulation
install.packages("hms") # for time-only datatype
install.packages("lubridate") # for more flexible parsing
install.packages("purrr") # for mapping function
install.packages("stringr") # for trimming strings
library(openxlsx)
library(dplyr)
library(hms)
library(lubridate)
library(purrr)
library(stringr)

##################
## Reformatting ##
##################

## Read in files
weekdays_2023 <- read.csv("all_weekday_2023.csv") # 15238 x 10
weekdays_2024 <- read.csv("all_weekday_2024.csv") # 10598 x 10
sats_2023 <- read.csv("all_sats_2023.csv") # 1018 x 10
sats_2024 <- read.csv("all_sats_2024.csv") # 786 x 10

cols <- colnames(weekdays_2023)

## "" --> NA
weekdays_2023 <- weekdays_2023 %>%
  mutate_all(~ na_if(., ""))
weekdays_2024 <- weekdays_2024 %>%
  mutate_all(~ na_if(., ""))
sats_2023 <- sats_2023 %>%
  mutate_all(~ na_if(., ""))
sats_2024 <- sats_2024 %>%
  mutate_all(~ na_if(., ""))

## Count NAs in multiple columns -- run this manually between steps to make sure no errors
sapply(weekdays_2023, function(x) sum(is.na(x)))
sapply(weekdays_2024, function(x) sum(is.na(x)))
sapply(sats_2023, function(x) sum(is.na(x)))
sapply(sats_2024, function(x) sum(is.na(x)))

## Checked that it's all primary and specialty care appts only (no family wellness)
unique(weekdays_2023$svc.dprtmnt)
unique(weekdays_2024$svc.dprtmnt)
unique(sats_2023$svc.dprtmnt)
unique(sats_2024$svc.dprtmnt)
# and delete that col (now 9 cols for all)
weekdays_2023 <- weekdays_2023 %>% select(-svc.dprtmnt)
weekdays_2024 <- weekdays_2024 %>% select(-svc.dprtmnt)
sats_2023 <- sats_2023 %>% select(-svc.dprtmnt)
sats_2024 <- sats_2024 %>% select(-svc.dprtmnt)

## Order by date and time
# Function to order dataframe by date and time columns
order_by_date_time <- function(df, date_col, time_col) {
  
  df_ordered <- df %>%
    mutate(
      # Convert the date column using lubridate's mdy() for month/day/year format
      {{date_col}} := mdy({{date_col}}),
      
      # Convert the time column using lubridate's parse_date_time for flexible parsing
      parsed_time = parse_date_time({{time_col}}, orders = c("%I:%M %p"), quiet = TRUE),
      
      # Convert parsed time to hms for time-only format
      {{time_col}} := as_hms(format(parsed_time, "%H:%M:%S"))
    ) %>%
    # Order by the date and time columns
    arrange({{date_col}}, {{time_col}})
  
  # Return the ordered dataframe
  return(df_ordered)
}
# now run on all dataframes
weekdays_2023 <- order_by_date_time(weekdays_2023, apptdate, apptstarttime)
weekdays_2024 <- order_by_date_time(weekdays_2024, apptdate, apptstarttime)
sats_2023 <- order_by_date_time(sats_2023, apptdate, apptstarttime)
sats_2024 <- order_by_date_time(sats_2024, apptdate, apptstarttime)
# and delete the extra parsing time col
weekdays_2023 <- weekdays_2023 %>% select(-parsed_time)
weekdays_2024 <- weekdays_2024 %>% select(-parsed_time)
sats_2023 <- sats_2023 %>% select(-parsed_time)
sats_2024 <- sats_2024 %>% select(-parsed_time)


## get rid of all open slots (= unscheduled)
weekdays_2023 <- weekdays_2023 %>% filter(apptslotstatus != "o - Open Slot") # 10649
weekdays_2024 <- weekdays_2024 %>% filter(apptslotstatus != "o - Open Slot") # 7804
sats_2023 <- sats_2023 %>% filter(apptslotstatus != "o - Open Slot") # 776
sats_2024 <- sats_2024 %>% filter(apptslotstatus != "o - Open Slot") # 634


## see the types of visits (providers and their specialties) in this data
scheduling_providers_1 <- weekdays_2023 %>% select(appt.schdlng.prvdr, appt.schdlng.prvdr.spclty)
scheduling_providers_2 <- weekdays_2024 %>% select(appt.schdlng.prvdr, appt.schdlng.prvdr.spclty)
scheduling_providers_3 <- sats_2023 %>% select(appt.schdlng.prvdr, appt.schdlng.prvdr.spclty)
scheduling_providers_4 <- sats_2024 %>% select(appt.schdlng.prvdr, appt.schdlng.prvdr.spclty)

scheduling_providers <- bind_rows(scheduling_providers_1,
                                  scheduling_providers_2,
                                  scheduling_providers_3,
                                  scheduling_providers_4)
scheduling_providers <- scheduling_providers %>%
  distinct(appt.schdlng.prvdr, .keep_all = TRUE) # 147 total!
# alphabetize
scheduling_providers <- scheduling_providers %>%
  arrange(appt.schdlng.prvdr)


# and read into excel (function)
save_unique_values_to_excel <- function(unique_values, file_name) {
  # Convert the unique values into a dataframe
  unique_df <- data.frame(UniqueValues = unique_values)
  
  # Write the dataframe to an Excel file
  write.xlsx(unique_df, file = file_name)
}

save_unique_values_to_excel(scheduling_providers, "scheduling_providers.xlsx")

## Reformat providers and their specialties, and filter
# Remove the following: EIGENMANN_K, ERTL_J, ESTRADA_Y, GERTEN_T, GOLDTHORPE_R, 
# JIMENEZ_BRISHEIDA, CREIGHTON_RN, ELIGIBILITY, TBI, ~ENG_CLASS_1, ~NURSE_WALKIN, 
# OHARA_L, OLAYA_F, OCONNOR_E, RICHARDSON_R
# either too varied, specialty indiscernible, or care management (transition of care/medication reconciliation)
to_remove <- c("EIGENMANN_K", "ERTL_J", "ESTRADA_Y", "GERTEN_T", "GOLDTHORPE_R", 
               "JIMENEZ_BRISHEIDA", "CREIGHTON_RN", "ELIGIBILITY", "TBI", 
               "~ENG_CLASS_1", "~NURSE_WALKIN", "OHARA_L", "OLAYA_F", "OCONNOR_E",
               "RICHARDSON_R")
weekdays_2023 <- weekdays_2023 %>%
  filter(!appt.schdlng.prvdr %in% to_remove)
weekdays_2024 <- weekdays_2024 %>%
  filter(!appt.schdlng.prvdr %in% to_remove)
sats_2023 <- sats_2023 %>%
  filter(!appt.schdlng.prvdr %in% to_remove)
sats_2024 <- sats_2024 %>%
  filter(!appt.schdlng.prvdr %in% to_remove)
# now rename some general specialties
# OB/GYN --> Gynecology
# Pulmonary Disease --> Pulmonary Medicine
# Emergency Medicine --> Internal Medicine
# Ophthalmology --> Ophthalmology/Optometry
# Optometry --> Ophthalmology/Optometry
# Otolaryngology --> Otolaryngology/Audiology
# Psychiatry --> Psychiatry/Psychology
# Psychology --> Psychiatry/Psychology
weekdays_2023 <- weekdays_2023 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    appt.schdlng.prvdr.spclty == "OB/GYN" ~ "Gynecology",
    appt.schdlng.prvdr.spclty == "Pulmonary Disease" ~ "Pulmonary Medicine",
    appt.schdlng.prvdr.spclty == "Emergency Medicine" ~ "Internal Medicine",
    appt.schdlng.prvdr.spclty == "Ophthalmology" ~ "Ophthalmology/Optometry",
    appt.schdlng.prvdr.spclty == "Optometry" ~ "Ophthalmology/Optometry",
    appt.schdlng.prvdr.spclty == "Otolaryngology" ~ "Otolaryngology/Audiology",
    appt.schdlng.prvdr.spclty == "Psychiatry" ~ "Psychiatry/Psychology",
    appt.schdlng.prvdr.spclty == "Psychology" ~ "Psychiatry/Psychology",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep all other values unchanged
  ))
weekdays_2024 <- weekdays_2024 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    appt.schdlng.prvdr.spclty == "OB/GYN" ~ "Gynecology",
    appt.schdlng.prvdr.spclty == "Pulmonary Disease" ~ "Pulmonary Medicine",
    appt.schdlng.prvdr.spclty == "Emergency Medicine" ~ "Internal Medicine",
    appt.schdlng.prvdr.spclty == "Ophthalmology" ~ "Ophthalmology/Optometry",
    appt.schdlng.prvdr.spclty == "Optometry" ~ "Ophthalmology/Optometry",
    appt.schdlng.prvdr.spclty == "Otolaryngology" ~ "Otolaryngology/Audiology",
    appt.schdlng.prvdr.spclty == "Psychiatry" ~ "Psychiatry/Psychology",
    appt.schdlng.prvdr.spclty == "Psychology" ~ "Psychiatry/Psychology",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep all other values unchanged
  ))
sats_2023 <- sats_2023 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    appt.schdlng.prvdr.spclty == "OB/GYN" ~ "Gynecology",
    appt.schdlng.prvdr.spclty == "Pulmonary Disease" ~ "Pulmonary Medicine",
    appt.schdlng.prvdr.spclty == "Emergency Medicine" ~ "Internal Medicine",
    appt.schdlng.prvdr.spclty == "Ophthalmology" ~ "Ophthalmology/Optometry",
    appt.schdlng.prvdr.spclty == "Optometry" ~ "Ophthalmology/Optometry",
    appt.schdlng.prvdr.spclty == "Otolaryngology" ~ "Otolaryngology/Audiology",
    appt.schdlng.prvdr.spclty == "Psychiatry" ~ "Psychiatry/Psychology",
    appt.schdlng.prvdr.spclty == "Psychology" ~ "Psychiatry/Psychology",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep all other values unchanged
  ))
sats_2024 <- sats_2024 %>%
  mutate(appt.schdlng.prvdr.spclty = case_when(
    appt.schdlng.prvdr.spclty == "OB/GYN" ~ "Gynecology",
    appt.schdlng.prvdr.spclty == "Pulmonary Disease" ~ "Pulmonary Medicine",
    appt.schdlng.prvdr.spclty == "Emergency Medicine" ~ "Internal Medicine",
    appt.schdlng.prvdr.spclty == "Ophthalmology" ~ "Ophthalmology/Optometry",
    appt.schdlng.prvdr.spclty == "Optometry" ~ "Ophthalmology/Optometry",
    appt.schdlng.prvdr.spclty == "Otolaryngology" ~ "Otolaryngology/Audiology",
    appt.schdlng.prvdr.spclty == "Psychiatry" ~ "Psychiatry/Psychology",
    appt.schdlng.prvdr.spclty == "Psychology" ~ "Psychiatry/Psychology",
    TRUE ~ appt.schdlng.prvdr.spclty  # Keep all other values unchanged
  ))


# more one by one renaming of specialties
# DINOWITZ_M = Orthopedic Surgery
# HOOKER_E = Nutrition
# JORDAN_H = Podiatry
# KASSMAN_S = Orthopedic Surgery
# LEIS_B = Internal Medicine
# RINNE_H = Wound Care
# THARALSON_E = Wound Care
# CREIGHTON_GI = Gastroenterology
# CREIGHTON_PCP3 = Internal Medicine
# CREIGHTON_PEDS_PT = Pediatric Physical Therapy
# DHHI = General Surgery
# MAYO_FM = Family Medicine
# MAYO_GYNE = Gynecology
# MAYO_IM = Internal Medicine
# PIHMA_CLINIC = Acupuncture/Herbal Medicine
# UOFA_STUDENT_CLINIC = Internal Medicine
# AUDIOLOGY_CLINIC = Otolaryngology/Audiology
# CARDIOLOGY = Cardiology
# CPAP_CLINIC = Pulmonary Medicine
# ECHO = Laboratory Medicine/Diagnostics
# EMG = Laboratory Medicine/Diagnostics
# GYNECOLOGY = Gynecology
# HOLTER = Laboratory Medicine/Diagnostics
# MAMMO = Diagnostic Radiology
# MNT = Nutrition
# PSYCH = Psychiatry/Psychology
# RETINAL_SCAN = Ophthalmology/Optometry
# VISUAL_FIELD = Ophthalmology/Optometry
# ~LAB = Laboratory Medicine/Diagnostics
# ~TB = Laboratory Medicine/Diagnostics
# ~Ultrasound = Diagnostic Radiology
# ~VACCINE = Laboratory Medicine/Diagnostics
# ~WOMENS_HEALTH = Gynecology
# ~WOUND = Wound Care
# Function to update the 'specialty' column based on 'provider'
rename_specialty_again <- function(df) {
  df %>%
    mutate(appt.schdlng.prvdr.spclty = case_when(
      appt.schdlng.prvdr == "BAYLESS_P" ~ "Internal Medicine",
      appt.schdlng.prvdr == "DINOWITZ_M" ~ "Orthopedic Surgery",
      appt.schdlng.prvdr == "HOOKER_E" ~ "Nutrition",
      appt.schdlng.prvdr == "JORDAN_H" ~ "Podiatry",
      appt.schdlng.prvdr == "KASSMAN_S" ~ "Orthopedic Surgery",
      appt.schdlng.prvdr == "LEIS_B" ~ "Internal Medicine",
      appt.schdlng.prvdr == "RINNE_H" ~ "Wound Care",
      appt.schdlng.prvdr == "THARALSON_E" ~ "Wound Care",
      appt.schdlng.prvdr == "CREIGHTON_GI" ~ "Gastroenterology",
      appt.schdlng.prvdr == "CREIGHTON_PCP3" ~ "Internal Medicine",
      appt.schdlng.prvdr == "CREIGHTON_PEDS_PT" ~ "Pediatric Physical Therapy",
      appt.schdlng.prvdr == "DHHI" ~ "General Surgery",
      appt.schdlng.prvdr == "MAYO_FM" ~ "Family Medicine",
      appt.schdlng.prvdr == "MAYO_GYNE" ~ "Gynecology",
      appt.schdlng.prvdr == "MAYO_IM" ~ "Internal Medicine",
      appt.schdlng.prvdr == "PIHMA_CLINIC" ~ "Acupuncture/Herbal Medicine",
      appt.schdlng.prvdr == "UOFA_STUDENT_CLINIC" ~ "Internal Medicine",
      appt.schdlng.prvdr == "AUDIOLOGY_CLINIC" ~ "Otolaryngology/Audiology",
      appt.schdlng.prvdr == "CARDIOLOGY" ~ "Cardiology",
      appt.schdlng.prvdr == "CPAP_CLINIC" ~ "Pulmonary Medicine",
      appt.schdlng.prvdr == "ECHO" ~ "Laboratory Medicine/Diagnostics",
      appt.schdlng.prvdr == "EMG" ~ "Laboratory Medicine/Diagnostics",
      appt.schdlng.prvdr == "GYNECOLOGY" ~ "Gynecology",
      appt.schdlng.prvdr == "HOLTER" ~ "Laboratory Medicine/Diagnostics",
      appt.schdlng.prvdr == "MAMMO" ~ "Diagnostic Radiology",
      appt.schdlng.prvdr == "MNT" ~ "Nutrition",
      appt.schdlng.prvdr == "PSYCH" ~ "Psychiatry/Psychology",
      appt.schdlng.prvdr == "RETINAL_SCAN" ~ "Ophthalmology/Optometry",
      appt.schdlng.prvdr == "VISUAL_FIELD" ~ "Ophthalmology/Optometry",
      appt.schdlng.prvdr == "~LAB" ~ "Laboratory Medicine/Diagnostics",
      appt.schdlng.prvdr == "~TB" ~ "Laboratory Medicine/Diagnostics",
      appt.schdlng.prvdr == "~Ultrasound" ~ "Diagnostic Radiology",
      appt.schdlng.prvdr == "~VACCINE" ~ "Laboratory Medicine/Diagnostics",
      appt.schdlng.prvdr == "~WOMENS_HEALTH" ~ "Gynecology",
      appt.schdlng.prvdr == "~WOUND" ~ "Wound Care",
      TRUE ~ appt.schdlng.prvdr.spclty  # Keep other values unchanged
    ))
}
dfs <- list(weekdays_2023, weekdays_2024, sats_2023, sats_2024)
updated_dfs_again <- map(dfs, rename_specialty_again)
weekdays_2023 <- updated_dfs_again[[1]]
weekdays_2024 <- updated_dfs_again[[2]]
sats_2023 <- updated_dfs_again[[3]]
sats_2024 <- updated_dfs_again[[4]]

# extracting total providers again
scheduling_providers_1 <- weekdays_2023 %>% select(appt.schdlng.prvdr, appt.schdlng.prvdr.spclty)
scheduling_providers_2 <- weekdays_2024 %>% select(appt.schdlng.prvdr, appt.schdlng.prvdr.spclty)
scheduling_providers_3 <- sats_2023 %>% select(appt.schdlng.prvdr, appt.schdlng.prvdr.spclty)
scheduling_providers_4 <- sats_2024 %>% select(appt.schdlng.prvdr, appt.schdlng.prvdr.spclty)

scheduling_providers <- bind_rows(scheduling_providers_1,
                                  scheduling_providers_2,
                                  scheduling_providers_3,
                                  scheduling_providers_4)
scheduling_providers <- scheduling_providers %>%
  distinct(appt.schdlng.prvdr, .keep_all = TRUE) # 147 total!
# alphabetize by specialty
scheduling_providers <- scheduling_providers %>%
  arrange(appt.schdlng.prvdr.spclty)
# save to excel for our purposes
save_unique_values_to_excel(scheduling_providers, "scheduling_providers_updated.xlsx")

## Include weekday times during business hours (exclude Thursday nights, 4pm on)
sort(unique(weekdays_2023$apptstarttime))
weekdays_2023 <- weekdays_2023 %>%
  filter(apptstarttime < as_hms("16:00:00"))
sort(unique(weekdays_2023$apptstarttime))

sort(unique(weekdays_2024$apptstarttime))
weekdays_2024 <- weekdays_2024 %>%
  filter(apptstarttime < as_hms("16:00:00"))
sort(unique(weekdays_2024$apptstarttime))

## Clean by appointment status
# for 2023
unique(weekdays_2023$apptslotstatus)
completed_appts_2023 <- weekdays_2023 %>%
  filter(apptslotstatus %in% c("3 - Checked Out", "4 - Charge Entered")) # 6154 completed appts in 2023 (weekdays)
sapply(completed_appts_2023, function(x) sum(is.na(x))) # check that apptcheckintime = 0, apptcancelreason = 6154
incomplete_appts_2023 <- weekdays_2023 %>%
  filter(apptslotstatus == "x - Cancelled") # 3281 incompleted appts in 2023 (weekdays)
unique(incomplete_appts_2023$apptcancelreason)
noshows_2023 <- incomplete_appts_2023 %>%
  filter(apptcancelreason == "NO-SHOW") # 995 no shows in 2023 (weekdays)
sapply(noshows_2023, function(x) sum(is.na(x))) # check that apptcancelreason = 0

unique(sats_2023$apptslotstatus)
completed_sats_2023 <- sats_2023 %>%
  filter(apptslotstatus %in% c("3 - Checked Out", "4 - Charge Entered")) # 519 completed sat appts in 2023
sapply(completed_sats_2023, function(x) sum(is.na(x))) # check that apptcheckintime = 0, apptcancelreason = 519
incomplete_sats_2023 <- sats_2023 %>%
  filter(apptslotstatus == "x - Cancelled") # 225 incompleted appts in 2023 (weekdays)
unique(incomplete_sats_2023$apptcancelreason)
noshows_sats_2023 <- incomplete_sats_2023 %>%
  filter(apptcancelreason == "NO-SHOW") # 71 no shows on saturdays in 2023
sapply(noshows_sats_2023, function(x) sum(is.na(x))) # check that apptcancelreason = 0


# for 2024
unique(weekdays_2024$apptslotstatus)
completed_appts_2024 <- weekdays_2024 %>%
  filter(apptslotstatus %in% c("3 - Checked Out", "4 - Charge Entered")) # 4488 completed appts in 2024 (weekdays)
sapply(completed_appts_2024, function(x) sum(is.na(x))) # check that apptcheckintime = 0, apptcancelreason = 4488
incomplete_appts_2024 <- weekdays_2024 %>%
  filter(apptslotstatus == "x - Cancelled") # 2256 incompleted appts in 2023 (weekdays)
unique(incomplete_appts_2024$apptcancelreason)
noshows_2024 <- incomplete_appts_2024 %>%
  filter(apptcancelreason == "NO-SHOW") # 722 no shows in 2024 (weekdays)
sapply(noshows_2024, function(x) sum(is.na(x))) # check that apptcancelreason = 0

unique(sats_2024$apptslotstatus)
completed_sats_2024 <- sats_2024 %>%
  filter(apptslotstatus %in% c("3 - Checked Out", "4 - Charge Entered")) # 462 completed sat appts in 2024
sapply(completed_sats_2024, function(x) sum(is.na(x))) # check that apptcheckintime = 0, apptcancelreason = 462
incomplete_sats_2024 <- sats_2024 %>%
  filter(apptslotstatus == "x - Cancelled") # 158 incompleted appts 2024 saturdays
unique(incomplete_sats_2024$apptcancelreason)
noshows_sats_2024 <- incomplete_sats_2024 %>%
  filter(apptcancelreason == "NO-SHOW") # 40 no shows on saturdays in 2024
sapply(noshows_sats_2024, function(x) sum(is.na(x))) # check that apptcancelreason = 0

# TODO rogue check in times for no show appts but i think that's user entry error


## 2023 weekday providers
prov_week_2023_1 <- completed_appts_2023 %>% select(appt.schdlng.prvdr, appt.schdlng.prvdr.spclty)
prov_week_2023_2 <- noshows_2023 %>% select(appt.schdlng.prvdr, appt.schdlng.prvdr.spclty)
prov_week_2023 <- bind_rows(prov_week_2023_1,
                            prov_week_2023_2)
prov_week_2023 <- prov_week_2023 %>%
  distinct(appt.schdlng.prvdr, .keep_all = TRUE) # 90 total
# alphabetize by specialty
prov_week_2023 <- prov_week_2023 %>%
  arrange(appt.schdlng.prvdr.spclty)
# save to excel for our purposes
save_unique_values_to_excel(prov_week_2023, "2023_weekday_providers.xlsx")

## 2023 saturday providers
prov_sats_2023_1 <- completed_sats_2023 %>% select(appt.schdlng.prvdr, appt.schdlng.prvdr.spclty)
prov_sats_2023_2 <- noshows_sats_2023 %>% select(appt.schdlng.prvdr, appt.schdlng.prvdr.spclty)
prov_sats_2023 <- bind_rows(prov_sats_2023_1,
                            prov_sats_2023_2)
prov_sats_2023 <- prov_sats_2023 %>%
  distinct(appt.schdlng.prvdr, .keep_all = TRUE) # 24 total
prov_sats_2023 <- prov_sats_2023 %>%
  arrange(appt.schdlng.prvdr.spclty)
save_unique_values_to_excel(prov_sats_2023, "2023_sats_providers.xlsx")

## 2024 weekday providers
prov_week_2024_1 <- completed_appts_2024 %>% select(appt.schdlng.prvdr, appt.schdlng.prvdr.spclty)
prov_week_2024_2 <- noshows_2024 %>% select(appt.schdlng.prvdr, appt.schdlng.prvdr.spclty)
prov_week_2024 <- bind_rows(prov_week_2024_1,
                            prov_week_2024_2)
prov_week_2024 <- prov_week_2024 %>%
  distinct(appt.schdlng.prvdr, .keep_all = TRUE) # 87 total
prov_week_2024 <- prov_week_2024 %>%
  arrange(appt.schdlng.prvdr.spclty)
save_unique_values_to_excel(prov_week_2024, "2024_weekday_providers.xlsx")

## 2024 saturday providers
prov_sats_2024_1 <- completed_sats_2024 %>% select(appt.schdlng.prvdr, appt.schdlng.prvdr.spclty)
prov_sats_2024_2 <- noshows_sats_2024 %>% select(appt.schdlng.prvdr, appt.schdlng.prvdr.spclty)
prov_sats_2024 <- bind_rows(prov_sats_2024_1,
                            prov_sats_2024_2)
prov_sats_2024 <- prov_sats_2024 %>%
  distinct(appt.schdlng.prvdr, .keep_all = TRUE) # 19 total
prov_sats_2024 <- prov_sats_2024 %>%
  arrange(appt.schdlng.prvdr.spclty)
save_unique_values_to_excel(prov_sats_2024, "2024_sats_providers.xlsx")


# final check for empty slots of necessary data (visual inspection)
sapply(completed_appts_2023, function(x) sum(is.na(x)))
sapply(completed_appts_2024, function(x) sum(is.na(x)))
sapply(completed_sats_2023, function(x) sum(is.na(x)))
sapply(completed_sats_2024, function(x) sum(is.na(x)))
sapply(noshows_2023, function(x) sum(is.na(x)))
sapply(noshows_2024, function(x) sum(is.na(x)))
sapply(noshows_sats_2023, function(x) sum(is.na(x)))
sapply(noshows_sats_2024, function(x) sum(is.na(x)))
# good to go :)

# limitation: can't tell if new patient, # distinct patients, demographics, transition of care, in person or telehealth, etc.
# also appts with np, rn , etc. not differentiated for primary and specialty care in this analysis

# Notes for research project purposes:
# selected primary and specialty care only (no family wellness)
# took out providers that didn't have discernible specialty, or focused mostly on transition/meds reconciliation
