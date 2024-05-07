
# Load packages 

if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse",
               "lubridate",
               "OlinkAnalyze",
               "rio",
               "data.table",
               "gtsummary",
               "readxl",
               "openxlsx")



# import biobank data and select variables of interest
OPDC_biobank_serum_RAW<- read.csv("~/Documents/R/OPDC/OPDC data files/OPDCBiobank_DATA_2024-04-16_1324_SERUM_ALL.csv") %>%
        mutate(across(where(is.character), ~ na_if(.,"")))

# Extract patient DOB
DOB_gender <- OPDC_biobank_serum_RAW %>%
        select(subjid, dob, gender) %>% # 0 = male, 1 = female 
        mutate(dob = as.Date(dob)) %>%
        filter(!is.na(dob)) %>%
        mutate(gender = ifelse(gender == 1,"female", "male"))

OPDC_biobank_serum_Metabolon <- OPDC_biobank_serum_RAW %>%
        select(subjid, redcap_event_name, bb_serum_date, bb_s_1_status, bb_s_1_used, bb_s_1_freezethaw, bb_s_2_status, bb_s_2_used, bb_s_2_freezethaw, bb_s_3_status, bb_s_3_used, bb_s_3_freezethaw, bb_s_4_status, bb_s_4_used, bb_s_4_freezethaw,bb_s_5_status, bb_s_5_used, bb_s_5_freezethaw) %>% 
        rename(visit = redcap_event_name, serum_date = bb_serum_date) %>%
        mutate(serum_date = as.Date(serum_date)) %>%
        filter(!str_detect(visit, "skin|lumber|plasma")) %>%
        mutate(arm = ifelse(str_detect(visit, "arm_1"), "PD", visit)) %>%
        mutate(arm = ifelse(str_detect(visit, "arm_2"), "Control", arm)) %>% 
        mutate(arm = ifelse(str_detect(visit, "arm_3"), "RBD", arm)) %>%
        mutate(arm = ifelse(str_detect(visit, "arm_4"), "Relative", arm)) %>%
        mutate(visit = substring(visit, 7,7)) %>% 
        mutate(visit = ifelse(visit == "_", "additional_visit", visit)) %>%
        mutate(status = bb_s_1_status + bb_s_2_status + bb_s_3_status + bb_s_4_status + bb_s_5_status) %>%
        mutate(status = ifelse(status == 10, "consumed", "active")) %>% # 2 = consumed, active = 1
        left_join(DOB_gender, by = "subjid") %>%
        mutate(age_at_sampling = round(as.numeric(difftime(serum_date, dob, units = "days"))/365.25)) %>% 
        mutate(sampling_site = substring(subjid, 5,6)) %>%
        select(subjid, dob, gender, arm, visit, serum_date, age_at_sampling, sampling_site, status) %>%
        filter(status == "active" | is.na(status)) %>%
        filter(!subjid %in% OPDC_PD_alternate$subjid) %>% # removes IDs of participants in the PD arm that turned out to have an alternate diagnosis
        filter(!subjid %in% opted_out_total) %>% # removes participants that have not agreed to data sharing 
        select(-c(status, dob, serum_date))

# Create the table
OPDC_biobank_serum_Metabolon_table <- OPDC_biobank_serum_Metabolon %>%
        tbl_summary(by = visit,
                    include = arm,
                    label = arm ~ "Study Arm",
                    statistic = list(all_categorical() ~ "{n}")) %>%  # Set empty string to remove summary statistics
        add_overall() %>%
        modify_header(label ~ "**Visit**") %>%
        modify_footnote(everything() ~ NA) 

# Print the modified table
print(OPDC_biobank_serum_Metabolon_table)

# Write data frame to Excel spreadsheet
write_xlsx(OPDC_biobank_serum_Metabolon, "~/Documents/R/OPDC/Metabolon_serum_list.xlsx")
