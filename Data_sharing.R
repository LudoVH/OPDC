

#################### OPTED-OUT OF DATA SHARING ######################

opted_out <- OPDC_clinical_data_RAW %>%
        select(subjid, optout) %>%
        filter(optout == 1)

# Generates list of IDs that have opted out of data sharing 
opted_out_IDs <- unique(opted_out$subjid)

OPDC_clinical_data_commercial_consent<- OPDC_clinical_data_RAW %>%
        select(subjid,
               redcap_event_name,
               consent_q17_v10,
               consent_q10_v9,
               consent_q10_v8,
               consent_q7_v7,
               consent_q7_v6,
               d1_consent_v5_q6,
               optout
        ) %>%
        filter(str_detect(redcap_event_name, "consen")) 

# identify participants that did not provide consent to share data with commercial partners 
commercial_consent_denied <- function(data_frame, value) {
        # Initialize an empty list to store rows for each column
        rows_list <- list()
        
        # Loop through each column of the data frame
        for (col in colnames(data_frame)) {
                # Identify rows with the specified value in the column
                rows_with_value <- which(data_frame[[col]] == value)
                
                # If there are rows with the value, store them in the list
                if (length(rows_with_value) > 0) {
                        rows_list[[col]] <- data_frame[rows_with_value, ]
                }
        }
        
        # Combine the rows into a single data frame
        extracted_rows <- do.call(rbind, rows_list)
        
        # Return the extracted data frame
        return(extracted_rows)
}

# Call the function to extract rows with the value 1 in each column
commercial_sharing_denied <- commercial_consent_denied(OPDC_clinical_data_commercial_consent, 0)
rownames(commercial_sharing_denied) <- 1:nrow(commercial_sharing_denied)

IDs_of_commercial_sharing_denied <- unique(commercial_sharing_denied$subjid)
# 29 participants that have not agreed for data to be shared commercially 


# Total list of participants that have either opted out of data sharing according to the question in "Patient Status" 
# or have disagreed to data sharing in their latest consent form
opted_out_total <- unique(c(IDs_of_commercial_sharing_denied, opted_out_IDs))

signed_to_V4 <- OPDC_clinical_data_RAW %>%
        filter(d1_consent_v1_q1 == 1 | d1_consent_v4_q1 == 1) %>% # all participants that at least signed version 1-4
        distinct(subjid, .keep_all = TRUE)

signed_V5_onwards <- OPDC_clinical_data_RAW %>%
        filter(d1_consent_v5_q1 == 1 | consent_q1_v6 == 1 | consent_q1_v7 == 1 | consent_q1_v8 == 1 | consent_q1_v9 == 1 | consent_q1_v10 == 1) %>% # all participants that signed version 1-6
        distinct(subjid, .keep_all = TRUE)


did_not_sign_V5_onwards <- signed_to_V4 %>% 
        filter(!subjid %in% signed_V5_onwards$subjid)

opted_out_anyway <- did_not_sign_V5_onwards %>%
        filter(subjid %in% opted_out_total)


opted_not_out <- OPDC_clinical_data_RAW %>%
        select(subjid, optout) %>%
        filter(optout == 0)

opted_in_by_default <- did_not_sign_V5_onwards %>%
        filter(!subjid %in% opted_out_total) %>%
        filter(subjid %in% opted_not_out$subjid)

never_asked <- did_not_sign_V5_onwards %>%
        filter(!subjid %in% opted_out_total) %>%
        filter(!subjid %in% opted_not_out$subjid) 