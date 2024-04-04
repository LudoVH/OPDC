
# Load packages 

if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse",
               "lubridate",
               "OlinkAnalyze",
               "rio",
               "data.table",
               "gtsummary",
               "readxl")

OPDC_clinical_data_RAW <- read.csv("~/Documents/R/OPDC/OPDC RedCap data/OPDCDiscoveryCohort_DATA_2024-01-29_1345.csv") %>%
                        mutate(across(where(is.character), ~ na_if(.,"")))

# generate a data frame of fixed variables (dob, gender, onset, diagnosis)
fixed_variables_OPDC <- OPDC_clinical_data_RAW[!is.na(OPDC_clinical_data_RAW$dob), ] %>%
        select(subjid, dob, gender, x_1_onest_date, x_2_diag_date)

# generate a data frame of ethnicity and handedness
ethnicity_handedness <- OPDC_clinical_data_RAW[!is.na(OPDC_clinical_data_RAW$a_1_ethnic_origin), ] %>%
        select(subjid, a_1_ethnic_origin, a_4_handedness)


# Patient education years calculated 
OPDC_participant_education <- OPDC_clinical_data_RAW %>%
        filter(!is.na(i_1_school_leaving_age)) %>%
        select(subjid, i_1_school_leaving_age, i_2_1_further_ed_years) %>%
        replace(is.na(.), 0) %>%
        mutate(education_years = (i_1_school_leaving_age + i_2_1_further_ed_years -  5))


# Data frame of all data in OPDC 
OPDC_clinical_data_ALL <- OPDC_clinical_data_RAW %>% 
        filter(str_detect(subjid, "PDS")) %>%
        select(-c(dob, gender, x_1_onest_date, x_2_diag_date, redcap_data_access_group, redcap_survey_identifier, i_1_school_leaving_age, i_2_1_further_ed_years, a_1_ethnic_origin, a_4_handedness)) %>%
        rename(visit = "redcap_event_name") %>%
        mutate(study_arm = ifelse(str_detect(visit, "arm_1"), "PD", visit)) %>%
        mutate(study_arm = ifelse(str_detect(visit, "arm_2"), "Control", study_arm)) %>%
        mutate(study_arm = ifelse(str_detect(visit, "arm_3"), "RBD", study_arm)) %>%
        mutate(study_arm = ifelse(str_detect(visit, "arm_4"), "Relative", study_arm)) %>%
        mutate(study_arm = ifelse(str_detect(visit, "arm_5"), "PPMI Control", study_arm)) %>%
        mutate(visit_type = ifelse(str_detect(visit, "doctor|nurse"), "F2F", visit)) %>%
        mutate(visit_type = ifelse(str_detect(visit, "telephone"), "telephone", visit_type)) %>%
        mutate(visit_type = ifelse(str_detect(visit, "video"), "video", visit_type)) %>%
        mutate(visit_type = ifelse(str_detect(visit, "subject"), "subject", visit_type)) %>%
        mutate(visit = ifelse(str_detect(visit, "doctor|nurse|subject|video|telephone"), substring(visit, 7,7), visit)) %>%
        mutate(visit = ifelse(str_detect(visit, "clinic_visit_2"), 2, visit)) %>%
        left_join(fixed_variables_OPDC, by = "subjid") %>%
        left_join(OPDC_participant_education, by = "subjid") %>%
        left_join(ethnicity_handedness, by = "subjid") %>%
        relocate(visit_date, visit_type, dob, gender, education_years, .after = visit) %>%
        relocate(study_arm, .after = subjid) %>%
        mutate(dob = as.Date(dob), 
               visit_date = as.Date(visit_date), 
               x_1_onest_date = as.Date(x_1_onest_date), 
               x_2_diag_date = as.Date(x_2_diag_date)) %>%
        mutate(age_at_visit = round(time_length(difftime(visit_date, dob), "years"))) %>%
        relocate(age_at_visit, .after = visit_date)

# Summary table of participants in each (RAW)
OPDC_clinical_data_ALL_table <- OPDC_clinical_data_ALL %>%
                                distinct(subjid, .keep_all = TRUE) %>%
                                tbl_summary(include = study_arm)
print(OPDC_clinical_data_ALL_table)

################## SELECTING VARIABLES OF INTEREST ##################### testing 
Fixed_data <- c("subjid",
                "study_arm",
                "x_1_onest_date",
                "x_2_diag_date",
                "dob",
                "gender",
                "a_1_ethnic_origin",
                "a_4_handedness",
                "education_years",
                "visit",
                "visit_date",
                "age_at_visit",
                "visit_type")

Diagnostic_status <- names(OPDC_clinical_data_ALL[, grep("^conver|^alternate", names(OPDC_clinical_data_ALL))])

Likelihood_PD_diagnosis <- names(OPDC_clinical_data_ALL[, grep("^ak_", names(OPDC_clinical_data_ALL))])

Dementia_diagnosis <- c(names(OPDC_clinical_data_ALL[, grep("^as_", names(OPDC_clinical_data_ALL))]), "res_care_home","res_care_home_date", "nurse_care_home", "nurse_care_home_date")
 
# OPDC instruments 
UPDRS_instrument <- names(OPDC_clinical_data_ALL[, grep("^af", names(OPDC_clinical_data_ALL))])
TGUG_FT_instrment <- names(OPDC_clinical_data_ALL[, grep("^s_", names(OPDC_clinical_data_ALL))])

MOCA_TMB_instrument <- names(OPDC_clinical_data_ALL[, grep("^o_", names(OPDC_clinical_data_ALL))])
MMSE_instrument <- names(OPDC_clinical_data_ALL[, grep("^n_", names(OPDC_clinical_data_ALL))])
Telephone_MOCA_instrument <- names(OPDC_clinical_data_ALL[, grep("^bm", names(OPDC_clinical_data_ALL))])
Phonetic_semantic_fluency_instrument <- names(OPDC_clinical_data_ALL[, grep("^p1|^p2|^q1|^q2", names(OPDC_clinical_data_ALL))])
NART_instrument <- names(OPDC_clinical_data_ALL[, grep("^fsiq|^viq|^piq", names(OPDC_clinical_data_ALL))])
IQCODE_instrument <- ("ap_iqcodes_total")
FAQ_instrument <- ("faq_total")


############# IMPORT OTHER OPDC DATA FILES #####################

# import PD cluster assignment 
OPDC_PD_clusters <- read_excel("~/Documents/R/OPDC/OPDC data files/Discovery_PD_clusters_03_2024.xlsx") %>% 
        rename (subjid = "SubjID", cluster = "clusters")       


# import list of CSF samples already tested on Olink 
Olink_CSF_manifest <- read.csv("~/Documents/R/OPDC/OPDC data files/olink_csf_PD_samples_QC_variables.csv") %>%
        rename(subjid = "SubjectID", diagnosis = "ParticipantGroup") %>%
        mutate(cohort = ifelse(str_detect(subjid, "PDS"), "PD", "ALS")) %>%
        filter(cohort == "PD") %>%
        mutate(CSF = "Yes") %>%
        select(subjid, CSF) %>%
        distinct(subjid, .keep_all = TRUE) # 3 longitudinal samples that are duplicated need to be removed 

# import list of serum samples already tested on Olink 
Olink_serum_manifest <- read_xlsx("~/Documents/R/OPDC/Serum Manifests/Entire_serum_Olink_manifest.xlsx") %>%
        select(ID, diagnosis) %>%
        rename(subjid = "ID") %>%
        mutate(Serum = "Yes") %>%
        select(subjid, Serum)

# Import list of participants that have Neurochip data 
Neurochip_IDs <- read.csv("~/Documents/R/OPDC/OPDC data files/opdc_subjid_request_age_gender_diagnosis.csv") %>%
        rename(subjid = "OPDC_SubjID")


##############################


# identify IDs of PD patients with an alternative diagnosis (1 = Yes, patient has alternate diagnosis) and remove them 
OPDC_PD_alternate <- OPDC_clinical_data_ALL %>%
        filter(study_arm == "PD") %>%
        filter(alternate_diag == 1 | converted_to_other == 1 | converted_to_msa == 1 | converted_to_dementia == 1) %>%
        filter(!(subjid== "PDS/JR/329" | subjid == "PDS/MK/020" | subjid == "PDS/NH/100")) %>% # removes patients labeled with PD Dementia
        select(subjid, study_arm, one_of(Diagnostic_status))



# To find PD participants where the likelihood of PD is considered low, select all participants rated with a likelihood of PD <80%
OPDC_PD_pure_not_sure <- OPDC_clinical_data_ALL %>% 
        filter(study_arm == "PD") %>%
        filter(!subjid %in% OPDC_PD_alternate$subjid) %>%
        filter(!is.na(ak_1_has_pd)) %>%
        group_by(subjid) %>% 
        top_n(1, visit_date) %>%
        subset(ak_1_has_pd < 80) %>%
        select(subjid, study_arm, ak_1_has_pd)


###########################


OPDC_clinical_data_ALL_processed <- OPDC_clinical_data_ALL %>%
                                        filter(!subjid %in% OPDC_PD_alternate$subjid) %>%
                                        filter(!subjid %in% OPDC_PD_pure_not_sure) %>%
                                        left_join(Olink_CSF_manifest, by = "subjid") %>%
                                        left_join(Olink_serum_manifest, by = "subjid") %>%
                                        left_join(OPDC_PD_clusters, by = "subjid") %>%
                                        mutate(CSF = ifelse(!is.na(CSF), "Yes", "No")) %>%
                                        mutate(Serum = ifelse(!is.na(Serum), "Yes", "No"))        


                                   

# selects all relevant data 
OPDC_clinical_data_relevant <- OPDC_clinical_data_ALL %>%
        filter(study_arm == "PD" | study_arm == "RBD" | study_arm == "Control" | study_arm == "PPMI Control" | study_arm == "Relative") %>%
        select(one_of(Fixed_data),
               one_of(Diagnostic_status),
               one_of(Likelihood_PD_diagnosis),
               one_of(Dementia_diagnosis),
               one_of(UPDRS_instrument),
               one_of(TGUG_FT_instrment),
               one_of(MOCA_TMB_instrument),
               one_of(Phonetic_semantic_fluency_instrument),
               one_of(Telephone_MOCA_instrument),
               one_of(MMSE_instrument),
               one_of(NART_instrument),
               one_of(IQCODE_instrument),
               one_of(FAQ_instrument)) %>%
        rename(date_onset = x_1_onest_date, 
               date_diagnosis = x_2_diag_date,
               ethnicity = a_1_ethnic_origin,
               handedness = a_4_handedness,
               likelihood_PD = ak_1_has_pd, 
               meets_UKBB = ak_2_meets_criteria, 
               old_subjid = converted_subjid_2,
               FSIQ = fsiq_nart,
               VIQ = viq_nart,
               PIQ = piq_nart,
               TMB = o_trail_making_partb,
               phonemic_fluency_score_total = p2_1_total_raw,
               semantic_fluency_score_total = q2_1_total_raw,
               telephone_moca_score = bm_moca_total,
               MMSE_total = n_12_score,
               MOCA_total = o_14_total_v1,
               Telephone_MOCA_total = bm_moca_total,
               IQCODE_total = ap_iqcodes_total,
               FAQ_total = faq_total,
               dementia_diagnosis = as_diagno_dementia,
               TGUG_average = s_10_getgo_average,
               FT_time = s_8_flam_time) %>%
        mutate(MOCA_corrected = ifelse(education_years <= 12, MOCA_total + 1, MOCA_total))

 







# Ayan's data request
OPDC_clinical_data_relevant_CSF_serum_IDs <- OPDC_clinical_data_relevant_CSF_serum_cluster %>%
        select(subjid,
               gender,
               study_arm,
               visit,
               age_at_visit,
               cluster,
               CSF,
               Serum) %>%
        filter(visit == 1) %>%
        distinct(subjid, .keep_all = TRUE)


        
Shahzad_data_request <- Neurochip_IDs %>%
        left_join(OPDC_clinical_data_relevant_CSF_serum_IDs, by = "subjid")

export(Shahzad_data_request,"Shazhad_data_request.csv")



# summary table of Neurochip data
Shahzad_data_request_table <- Shahzad_data_request %>%
        tbl_summary(by = study_arm,
                    include = c(CSF, Serum, cluster),
                    missing_text = "Missing")

# Print the summary table
print(Shahzad_data_request_table)


missing <- Shahzad_data_request %>%
        filter(study_arm == "PD") %>%
        filter(is.na(Serum)) %>% 
        filter(!subjid %in% test$subjid) %>%
        filter(!subjid %in% OPDC_PD_alternate$subjid) %>%
        filter(!subjid %in% OPDC_PD_not_sure)
        


test <- Neurochip_IDs %>%
        filter(!subjid %in% OPDC_clinical_data_relevant_CSF_serum_cluster$subjid)
export(test, "Odd_IDs.xlsx")


# identify IDs of PD patients with an alternative diagnosis (1 = Yes, patient has alternate diagnosis) and remove them 
OPDC_PD_alternate <- OPDC_clinical_data_relevant %>%
        filter(alternate_diag == 1 | converted_to_other == 1) %>%
        filter(!(subjid == "PDS/JR/329" | subjid == "PDS/MK/020" | subjid == "PDS/NH/100")) # removes patients labeled with PD Dementia 
        
## Alternate diagnoses list 
# 1 MSA (Multiple system atrophy)
# 2 PSP (Progressive supranuclear palsy)
# 3 MND (Motor neuron disease)
# 4 PAF (Pure autonomic failure)
# 5 DLB (Dementia with Lewy bodies)
# 6 AD (Alzheimer's disease)
# 7 Vascular dementia
# 8 Vascular PD
# 9 Cerebellar ataxia
# 10 Essential tremor
# 11 Dystonic tremor
# 12 Atypical Tremor
# 13 Other (Specify below)


# Select all rows (participant visits) that at SOME point had a likelihood score recorded
# Then select the latest visit date for each likelihood score and then select the participants that have a likelihood score of >= 85%
OPDC_PD_pure_sure <- OPDC_clinical_data_relevant[!is.na(OPDC_clinical_data_relevant$likelihood_PD), ] %>%
        group_by(subjid) %>% 
        top_n(1, visit_date) %>%
        subset(likelihood_PD >= 80)

OPDC_PD_not_sure <- OPDC_clinical_data_relevant %>% 
        filter(study_arm == "PD") %>%
        filter(!subjid %in% OPDC_PD_pure_sure$subjid)


# Select all sure PD, RBD and controls 
OPDC_clinical_data_PD_RBD_Control <- OPDC_clinical_data_relevant %>%
        filter(!subjid %in% OPDC_PD_alternate$subjid) %>%
        filter(!subjid %in% OPDC_PD_not_sure$subjid)
        












# COGNITION DATA
# select instruments of interest





OPDC_dementia_participants <- OPDC_clinical_data_ALL %>%
        select(subjid, 
               dementia,
               dementia_date,
               converted_to_other,
               converted_to_other_diag,
               converted_to_dementia,
               converted_to_other_specify,
               conversion_diag_date,
               as_diagno_dementia,
               as_pd_dementia,
               as_alzheimers_dementia,
               as_vascular_dementia,
               as_other_dementia, 
               as_independant_or_clinic,
               as_independant_or_clinic_date,
               as_opdc_clinic,
               as_opdc_clinic_date,
               as_other_clinic,
               as_other_clinic_date,
               res_care_home,
               res_care_home_date,
               nurse_care_home,
               nurse_care_home_date) %>%
        filter(!dementia == 0 | !as_diagno_dementia == 0)



OPDC_cognition_data <- OPDC_clinical_data_ALL %>%
        select(subjid, 
               visit_date, 
               fsiq_nart, viq_nart, piq_nart, 
               ap_iqcodes_total,
               faq_total, 
               o_trail_making_partb, 
               p2_1_total_raw, 
               q2_1_total_raw, 
               bm_moca_total, 
               n_12_score, 
               o_14_total_v1) %>%
        left_join(OPDC_participant_demographics, by = "subjid") %>%
        relocate(dob, gender, .after = subjid) %>%
        left_join(OPDC_participant_education, by = "subjid") %>%
        relocate(i_1_school_leaving_age, i_2_1_further_ed_years, .after = gender) %>%
        rename(school_leaving_age = i_1_school_leaving_age,
               further_education_years = i_2_1_further_ed_years,
               FSIQ = fsiq_nart,
               VIQ = viq_nart,
               PIQ = piq_nart,
               TMB = o_trail_making_partb,
               phonemic_fluency_score = p2_1_total_raw,
               semantic_fluency_score = q2_1_total_raw,
               telephone_moca_score = bm_moca_total,
               MMSE = n_12_score,
               MOCA = o_14_total_v1,
               IQCODE = ap_iqcodes_total,
               FAQ = faq_total,
               dementia_diagnosis = as_diagno_dementia) %>%
        mutate(further_education_years = ifelse(is.na(further_education_years), 0, further_education_years)) %>%
        mutate(dob = as.Date(dob), visit_date = as.Date(visit_date)) %>%
        filter(!is.na(MOCA) & !is.na(MMSE))



group_by(winner) %>% summarise(years = paste(year, collapse=", "))



