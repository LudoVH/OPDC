

# Shahzad's data request
Shahzad_data_request <- OPDC_clinical_data_ALL_additional_variables %>%
        select(subjid,
               gender,
               study_arm,
               visit,
               age_at_visit,
               cluster,
               Olink_CSF,
               Olink_serum,
               Neurochip) %>%
        filter(visit == 1) %>%
        distinct(subjid, .keep_all = TRUE)


# summary table of Neurochip data
Shahzad_data_request_table <- Shahzad_data_request %>%
        tbl_summary(by = study_arm,
                    include = c(Olink_CSF, Olink_serum, cluster),
                    missing_text = "Missing")

# Print the summary table
print(Shahzad_data_request_table)