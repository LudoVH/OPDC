
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

OPDC_clinical_data_RAW <- read.csv("~/Documents/R/OPDC/OPDC RedCap data/OPDCDiscoveryCohort_DATA_2024-05-07_2100.csv") %>%
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
        rename(visit = "redcap_event_name", MRI_done = "mri_history") %>%
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
        mutate(age_at_visit = time_length(difftime(visit_date, dob), "years"))

# Summary table of participants in each (RAW)
OPDC_clinical_data_ALL_table <- OPDC_clinical_data_ALL %>%
                                distinct(subjid, .keep_all = TRUE) %>%
                                tbl_summary(include = study_arm)
print(OPDC_clinical_data_ALL_table)



################## INSTRUMENT BUNDLES ##################### 

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
UPDRS_2_instrument <- names(OPDC_clinical_data_ALL)[grep("^h_._|^h_.._", names(OPDC_clinical_data_ALL))]
UPDRS_3_instrument <- names(OPDC_clinical_data_ALL[, grep("^af", names(OPDC_clinical_data_ALL))])
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
        mutate(Olink_CSF = "Yes") %>%
        select(subjid, Olink_CSF) %>%
        distinct(subjid, .keep_all = TRUE) # 3 longitudinal samples that are duplicated need to be removed 

# import list of serum samples already tested on Olink 
Olink_serum_manifest <- read_xlsx("~/Documents/R/OPDC/Serum Manifests/Entire_serum_Olink_manifest.xlsx") %>%
        select(ID, diagnosis) %>%
        rename(subjid = "ID") %>%
        mutate(Olink_serum = "Yes") %>%
        select(subjid, Olink_serum)

# Import list of participants that have Neurochip data 
Neurochip_list <- read.csv("~/Documents/R/OPDC/OPDC data files/opdc_subjid_request_age_gender_diagnosis.csv") %>%
        rename(subjid = "OPDC_SubjID") %>%
        mutate(Neurochip = "Yes") %>%
        select(subjid, Neurochip)

# import old genetic data 
old_genetic_data <- read_xlsx("~/Documents/R/OPDC/OPDC data files/OPDC_genetic_data.xlsx") %>% 
        rename(subjid = "ID") %>%
        mutate(confirmed_mutation = ifelse(!is.na(mutation),"Yes", NA)) %>%
        select(subjid, confirmed_mutation) 

# import fibroblast list 
Fibroblast_list <- read_xlsx("~/Documents/R/OPDC/Fibroblasts/Fibroblast_list.xlsx")  %>% 
        rename(subjid = "ID")

#import TMEM variant list 
TMEM_variant_list <- read.csv("~/Documents/R/OPDC/OPDC data files/20240411-opdc_tmem_variant_info.csv") %>%
        select(subjid, chr4.958159.C, chr4.958159.C.carriers, chr4.950422.C, chr4.950422.C.Carrier)

# import Michele's convertor list 
RBD_convertors_Michele <- read_xlsx("~/Documents/R/OPDC/OPDC data files/Michele_convertors.xlsx")

# add all the additional variables
OPDC_clinical_data_ALL_additional_variables <- OPDC_clinical_data_ALL %>%
        left_join(Olink_CSF_manifest, by = "subjid") %>%
        left_join(Olink_serum_manifest, by = "subjid") %>%
        left_join(OPDC_PD_clusters, by = "subjid") %>%
        left_join(Neurochip_list, by = "subjid") %>%
        left_join(TMEM_variant_list, by = "subjid") %>% 
        left_join(Fibroblast_list, by = "subjid") %>%
        left_join(old_genetic_data, by = "subjid") 


########### LIST OF TESTS DONE ##############

# List of participant IDs and what has been tested
OPDC_tested_list <- OPDC_clinical_data_ALL_additional_variables %>%
        select(subjid, study_arm, Olink_serum, Olink_CSF, Neurochip, Fibroblast, confirmed_mutation, MRI_done) %>%
        mutate(MRI_done = ifelse(MRI_done == 1, "Yes", NA)) %>%
        filter(Olink_serum == "Yes" | Olink_CSF == "Yes" | Neurochip == "Yes" | Fibroblast == "Yes" | confirmed_mutation == "Yes" | MRI_done == "Yes") %>%
        distinct(subjid, .keep_all = TRUE)

write.xlsx(OPDC_tested_list, "~/Documents/R/OPDC/OPDC_tested_list.xlsx")

# Summary table of participants in each (ALL + additional variables)
OPDC_tested_list_table <- OPDC_tested_list %>%
        tbl_summary(by = study_arm,
                include = c(Olink_serum, Olink_CSF, MRI_done, Neurochip, Fibroblast, confirmed_mutation),
                missing_text = "No",
                statistic = all_categorical() ~ "{n}") %>%
        add_overall() %>%
        modify_footnote()
print(OPDC_tested_list_table)



############################## ALTERNATIVE DIAGNOSIS #############################


# identify IDs of PD patients with an alternative diagnosis (1 = Yes, patient has alternate diagnosis) and remove them 
OPDC_PD_alternate <- OPDC_clinical_data_ALL %>%
        filter(study_arm == "PD") %>%
        filter(alternate_diag == 1 | converted_to_other == 1 | converted_to_msa == 1 | converted_to_dementia == 1) %>%
        filter(!(subjid== "PDS/JR/329" | subjid == "PDS/MK/020" | subjid == "PDS/NH/100" | subjid == "PDS/JR/189")) %>% # removes patients labeled with PD Dementia
        select(subjid, study_arm, one_of(Diagnostic_status))

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

# To find PD participants where the likelihood of PD is considered low, select all participants rated with a likelihood of PD <90%
OPDC_PD_pure_not_sure <- OPDC_clinical_data_ALL %>% 
        filter(study_arm == "PD") %>%
        filter(!subjid %in% OPDC_PD_alternate$subjid) %>%
        filter(!is.na(ak_1_has_pd)) %>%
        group_by(subjid) %>% 
        top_n(1, visit_date) %>%
        subset(ak_1_has_pd < 90) %>%
        select(subjid, study_arm, ak_1_has_pd)


########################### RBD PHENOCONVERTORS ###############################

RBD_convertors <- OPDC_clinical_data_ALL_additional_variables %>%
        filter(study_arm == "RBD") %>%
        filter(converted_to_pd == 1 | converted_to_other == 1) %>%
        select(subjid, converted_to_pd, converted_to_other, converted_to_other_diag, converted_to_dementia, converted_to_other_specify)

setdiff(RBD_convertors$subjid, RBD_convertors_Michele$subjid)

# Total RBD participants
total_RBD <- OPDC_clinical_data_ALL_additional_variables %>%
        filter(study_arm == "RBD") %>%
        filter(!(subjid == "PDS/PA/162" | subjid == "PDS/WP/052" | subjid == "PDS/JR/543")) %>% # Not truly RBD (as already PD/DLB at visit 1) 
        distinct(subjid)


############################ SELECTING SPECIFIC INSTRUMENTS ############################

# selects all relevant data 
OPDC_clinical_data_relevant <- OPDC_clinical_data_ALL_additional_variables %>%
        filter(study_arm == "PD" | study_arm == "RBD" | study_arm == "Control" | study_arm == "PPMI Control" | study_arm == "Relative") %>%
        select(all_of(Fixed_data),
               all_of(Diagnostic_status),
               all_of(Likelihood_PD_diagnosis),
               all_of(Dementia_diagnosis),
               all_of(UPDRS_3_instrument),
               all_of(TGUG_FT_instrment),
               all_of(MOCA_TMB_instrument),
               all_of(Phonetic_semantic_fluency_instrument),
               all_of(Telephone_MOCA_instrument),
               all_of(MMSE_instrument),
               all_of(NART_instrument),
               all_of(IQCODE_instrument),
               all_of(FAQ_instrument)) %>%
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



x <- Olink_CSF_manifest %>%
        filter(subjid %in% OPDC_PD_clusters$subjid)

new_cluster_info <- c("PDS/AH/018", "PDS/HH/043", "PDS/HH/057", "PDS/JR/261", "PDS/JR/558", 
        "PDS/JR/562","PDS/JR/563", "PDS/JR/564", "PDS/JR/570", "PDS/JR/576", "PDS/MK/024")

y <- Olink_CSF_manifest %>%
        filter(subjid %in% new_cluster_info)

z <- OPDC_PD_clusters %>%
        filter(subjid %in% new_cluster_info)