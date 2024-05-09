

# Shahzad's data request for TMEM analysis
Shahzad_data_request_TMEM <- OPDC_clinical_data_ALL_additional_variables %>%
        filter(study_arm == "PD" | study_arm == "RBD" | study_arm == "Control" | study_arm == "PPMI Control" | study_arm == "Relative") %>%
        select(all_of(Fixed_data),
               all_of(UPDRS_2_instrument),
               all_of(UPDRS_3_instrument),
               all_of(MOCA_TMB_instrument),
               Neurochip) %>%
        rename(date_onset = x_1_onest_date, 
               date_diagnosis = x_2_diag_date,
               ethnicity = a_1_ethnic_origin,
               handedness = a_4_handedness,
               TMB = o_trail_making_partb,
               MOCA_total = o_14_total_v1) %>%
        mutate(MOCA_corrected = ifelse(education_years <= 12, MOCA_total + 1, MOCA_total)) %>%
        filter(visit == 1 | visit == 2 | visit == 3 | visit == 4 | visit == 5 | visit == 6 | visit == 7 | visit == 8 | visit == 9) %>%
        filter(visit_type == "F2F" | visit_type == "video" | visit_type == "subject") %>%
        filter(!is.na(visit_date)) %>%
        filter(!subjid %in% OPDC_PD_alternate$subjid) %>%
        filter(!subjid %in% OPDC_PD_pure_not_sure$subjid) %>%
        filter(!is.na(Neurochip)) %>%
        mutate(age_at_onset = as.numeric(difftime(date_onset, dob, units = "days")) / 365.25) %>%
        mutate(age_at_diagnosis = as.numeric(difftime(date_diagnosis, dob, units = "days")) / 365.25) %>%
        select(-c(dob, date_onset, date_diagnosis, visit_date)) %>%
        relocate(age_at_onset, age_at_diagnosis, .after = gender)
        

# Shahzad's data request for demographics of Neurochip data 
Shahzad_data_request_demographics <- OPDC_clinical_data_ALL_additional_variables %>%
        filter(study_arm == "PD" | study_arm == "RBD" | study_arm == "Control") %>%
        filter(!subjid %in% OPDC_PD_alternate$subjid) %>%
        filter(!subjid %in% OPDC_PD_pure_not_sure$subjid) %>%
        filter(visit == 1) %>%
        filter(!is.na(Neurochip)) %>%
        select(subjid, study_arm, visit, age_at_visit, gender, Neurochip, Olink_CSF, Olink_serum, cluster) %>%
        distinct(subjid, .keep_all = TRUE)




# summary table of Neurochip data
Shahzad_data_request_demographics_table <- Shahzad_data_request_demographics %>%
        tbl_summary(by = study_arm,
                    include = c(gender, age_at_visit, Olink_CSF, Olink_serum, cluster),
                    missing_text = "Missing")

# Print the summary table
print(Shahzad_data_request_demographics_table)


# Write to .csv file 
write_csv(Shahzad_data_request, "TMEM_data_request.csv")
