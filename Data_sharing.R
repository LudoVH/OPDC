

#################### OPTED-OUT OF DATA SHARING ######################

# Step 1: Extract all participants that signed a consent form that including the request to share data with commercial companies  
OPDC_clinical_data_commercial_consent_requested <- OPDC_clinical_data_RAW %>%
        select(subjid,
               redcap_event_name,
               consent_q11_v10, #I consent for my personal data and images to be shared with researchers in other academic centres and commercial companies
               consent_q17_v10, #I consent for my DNA, serum and plasma to be shared with researchers in other academic centres and commercial companies
               consent_q10_v9, #I consent for my personal data and images to be shared with researchers in other academic centres and commercial companies
               consent_q15_v9, #I consent for my DNA, serum and plasma to be shared with researchers in other academic centres and commercial companies
               consent_q10_v8, #I consent for my personal data and images to be shared with researchers in other academic centres and commercial companies
               consent_q15_v8, #I consent for my DNA, serum and plasma to be shared with researchers in other academic centres and commercial companies
               consent_q7_v7, #I consent for my personal data and images to be shared with researchers in other academic centres and commercial companies
               consent_q12_v7, #I consent for my DNA, serum and plasma to be shared with researchers in other academic centres and commercial companies 
               consent_q7_v6, #I consent for my personal data and images to be shared with researchers in other academic centres and commercial companies
               consent_q11_v6, #I consent for my DNA, serum and plasma to be shared with researchers in other academic centres and commercial companies
               d1_consent_v5_q8, #I consent for my DNA, serum and plasma to be shared with researchers in other academic centres and commercial companies
               d1_consent_v5_q6, #I consent for my personal data and images to be shared with researchers in other academic centres and commercial companies
               consent_q8, #I consent for my personal data and images to be shared with researchers in other academic centres and commercial companies
               consent_q12, #I consent for my DNA, serum and plasma to be shared with researchers in other academic centres and commercial companies
               add_proc_consent_q7_v6, #I consent for my anonymised brain scans to be shared with researchers in other academic centres and commercial companies
               add_proc_consent_q13_v6, #I consent to the anonymised stem cells being retained and used in future research into Parkinson's and related degenerative conditions. I agree to the cells being shared with researchers in other academic centres and commercial companies
               add_proc_consent_q15_v6, #I consent for my anonymised samples to be shared with researchers in other academic centres and commercial companies
               mri_consent_q6_v2, #I understand that my anonymised MRI brain scans and related clinical information may be shared with researchers in other academic centres and commercial companies
               ) %>%
        filter(str_detect(redcap_event_name, "cons"))



# Step 2: Identify participants that expressly did not consent to data sharing with a commercial company (asked in consent form V5 - V10)
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
commercial_sharing_denied <- commercial_consent_denied(OPDC_clinical_data_commercial_consent_requested, 0)
rownames(commercial_sharing_denied) <- 1:nrow(commercial_sharing_denied)

# List of IDs that have denied commercial data sharing of either clinical data/DNA/biofluids/stem cells/brain scans (70)
IDs_of_commercial_sharing_denied <- unique(commercial_sharing_denied$subjid)
IDs_of_commercial_sharing_denied_GSK <- commercial_sharing_denied %>%
        filter(subjid %in% GSK_IDs$subjid) %>%
        distinct(subjid)

# List of IDs from GSK ID list that have denied data sharing of clinical data OR DNA with commercial company (63)
Clinical_DNA_consent_denied <- commercial_sharing_denied %>%
                        filter(
                                consent_q11_v10 == 0 |
                                consent_q17_v10 == 0 |
                                consent_q10_v9 == 0 |
                                consent_q15_v9 == 0 |    
                                consent_q10_v8 == 0 |
                                consent_q15_v8 == 0 |
                                consent_q7_v7 == 0 |
                                consent_q12_v7 == 0 |
                                consent_q7_v6 == 0 |
                                consent_q11_v6 == 0 |
                                d1_consent_v5_q8 == 0 |
                                d1_consent_v5_q6 == 0 |
                                consent_q8 == 0 |
                                consent_q12 == 0
                                ) %>%
                        filter(subjid %in% GSK_IDs$subjid) %>%
                        distinct(subjid)


## STEP 3: Participants in the GSK ID list that have opted out of data sharing according to the patient's status on RedCap (12)
opted_out <- OPDC_clinical_data_RAW %>%
        select(subjid, optout) %>%
        filter(optout == 1) %>%
        filter(subjid %in% GSK_IDs$subjid) %>%
        distinct(subjid)


## STEP 4: List of participants from the GSK ID list that have either opted out or denied consent for data sharing of clinical data/DNA (75)
Clinical_DNA_consent_denied_opted_out <- unique(c(Clinical_DNA_consent_denied$subjid, opted_out$subjid))


## STEP 5: Calculating the number of participants that never signed consent form version 5 or above 

signed_at_least_V1_to_V4 <- OPDC_clinical_data_RAW %>%
        filter(d1_consent_v1_q1 == 1 | d1_consent_v4_q1 == 1) %>% # all participants that at least signed version 1-4 (question 1 used as a surrogate of whole consent form)
        distinct(subjid, .keep_all = TRUE) 

signed_V5_onwards <- OPDC_clinical_data_RAW %>%
        filter(d1_consent_v5_q1 == 1 | consent_q1_v6 == 1 | consent_q1_v7 == 1 | consent_q1_v8 == 1 | consent_q1_v9 == 1 | consent_q1_v10 == 1) %>% # all participants that signed version 1-6 (question 1 used as a surrogate of whole consent form)
        distinct(subjid, .keep_all = TRUE)

only_signed_up_to_V4 <- signed_at_least_V1_to_V4 %>% 
        filter(!subjid %in% signed_V5_onwards$subjid) %>%
        distinct(subjid, .keep_all = TRUE) 

# Participants that only signed up to V4 in the GSK ID list (435)
only_signed_up_to_V4_GSK <- only_signed_up_to_V4 %>%
        filter(subjid %in% GSK_IDs$subjid) %>%
        distinct(subjid, .keep_all = TRUE) %>%
        filter(!subjid %in% opted_out)


## STEP 6



opted_not_out <- OPDC_clinical_data_RAW %>%
        select(subjid, optout) %>%
        filter(optout == 0)

opted_in_by_default <- did_not_sign_V5_onwards %>%
        filter(!subjid %in% opted_out_total) %>%
        filter(subjid %in% opted_not_out$subjid)

never_asked <- did_not_sign_V5_onwards %>%
        filter(!subjid %in% opted_out_total) %>%
        filter(!subjid %in% opted_not_out$subjid) 




# Step 1: Import list of participant IDs shared with GSK 
GSK_IDs <- read.csv("~/Documents/R/OPDC/OPDC data files/info_opdc_data_shared_with_gsk_2024-04-29(in).csv") %>%
        select(OPDC_ID) %>%
        rename(subjid = "OPDC_ID")