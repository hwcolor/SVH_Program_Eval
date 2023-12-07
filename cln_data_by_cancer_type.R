library(ggplot2)
library(tidyr)
library(dplyr)
library(expss)
library(readxl)
library(reshape2)
library(forcats)
library(rmarkdown)
library(googlesheets4)
library(stringi)
library(tidytext)
library(data.table)
library(batman)
library(devtools)

## analysis for preliminary report: 
### Owner: Hannah Williams
### Date: XX/XX/XX
##download lab data sets from google drive

##download demographics/data of all eligible pop

##download demographics/data of users enrolled in program
summary(raw_dat_demographics) 


##download healthprofile data from TALL table


## ADD CANCER SPECIFIC TABLES 
summary(raw_dat_cancer_LUNG) 

summary(raw_dat_cancer_BREAST) 

summary(raw_dat_cancer_CERVICAL) 

summary(raw_dat_cancer_CRC) 

summary(raw_dat_cancer_CRC) 

summary(raw_dat_cancer_PROSTATE) 


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
##/////### LUNG CANCER TABLE CLEAN UP ##/////###

## calculate pack years 
raw_dat_cancer_LUNG$hp_smoking_number_years <- as.numeric(raw_dat_cancer_LUNG$hp_smoking_number_years)
raw_dat_cancer_LUNG$hp_smoking_years_quit <- as.numeric(raw_dat_cancer_LUNG$hp_smoking_years_quit)
raw_dat_cancer_LUNG$hp_smoking_cigs_per_day <- as.numeric(raw_dat_cancer_LUNG$hp_smoking_cigs_per_day)

raw_dat_cancer_LUNG$smoking_pack_yrs <- ((raw_dat_cancer_LUNG$hp_smoking_cigs_per_day/20)*raw_dat_cancer_LUNG$hp_smoking_number_years)
cro(raw_dat_cancer_LUNG$smoking_pack_yrs)

raw_dat_cancer_LUNG <- raw_dat_cancer_LUNG %>%
  mutate(hp_lung_cancer_symptom_coughing_blood_tf = case_when(
    hp_lung_cancer_symptom_coughing_blood_yn == "Y" ~ 1,
    hp_lung_cancer_symptom_coughing_blood_yn == "N" ~ 0
  ))

raw_dat_cancer_LUNG <- raw_dat_cancer_LUNG %>%
  mutate(hp_lung_cancer_symptom_feeling_very_tired_all_the_time_tf = case_when(
    hp_lung_cancer_symptom_feeling_very_tired_all_the_time_yn == "Y" ~ 1,
    hp_lung_cancer_symptom_feeling_very_tired_all_the_time_yn == "N" ~ 0
  ))

raw_dat_cancer_LUNG <- raw_dat_cancer_LUNG %>% 
  mutate(hp_lung_cancer_symptom_persistent_cough_tf = case_when(
    hp_lung_cancer_symptom_persistent_cough_yn == "Y" ~ 1,
    hp_lung_cancer_symptom_persistent_cough_yn == "N" ~ 0
  ))

raw_dat_cancer_LUNG <- raw_dat_cancer_LUNG %>% 
  mutate(hp_lung_cancer_symptom_unexplained_weight_loss_tf = case_when(
    hp_lung_cancer_symptom_unexplained_weight_loss_yn == "Y" ~ 1,
    hp_lung_cancer_symptom_unexplained_weight_loss_yn == "N" ~ 0
  ))


cro(raw_dat_cancer_LUNG$hp_lung_cancer_symptom_coughing_blood_yn) 
cro(raw_dat_cancer_LUNG$hp_lung_cancer_symptom_feeling_very_tired_all_the_time_yn) 
cro(raw_dat_cancer_LUNG$hp_lung_cancer_symptom_persistent_cough_yn) 
cro(raw_dat_cancer_LUNG$hp_lung_cancer_symptom_unexplained_weight_loss_yn) 


raw_dat_cancer_LUNG$count_lung_symptoms <- (raw_dat_cancer_LUNG$hp_lung_cancer_symptom_coughing_blood_tf +
                                         raw_dat_cancer_LUNG$hp_lung_cancer_symptom_feeling_very_tired_all_the_time_tf +
                                         raw_dat_cancer_LUNG$hp_lung_cancer_symptom_persistent_cough_tf +
                                         raw_dat_cancer_LUNG$hp_lung_cancer_symptom_unexplained_weight_loss_tf)


cro(raw_dat_cancer_LUNG$count_lung_symptoms) 

### create has symptoms category
raw_dat_cancer_LUNG <- raw_dat_cancer_LUNG %>% 
  mutate(has_LUNG_symptoms_tf = case_when(
    count_lung_symptoms >= 1 ~ TRUE,
    count_lung_symptoms < 1 ~ FALSE))

cro(raw_dat_cancer_LUNG$has_LUNG_symptoms_tf) 

## create data frame 
colnames(raw_dat_cancer_LUNG)

cln_cancer_LUNG <- subset(raw_dat_cancer_LUNG, select = c("coloruser_id", "health_profile_id", "hp_smoking_status", "Care_Program_Participants_Phi__count_current_smoker",
                                                     "hp_smoking_cigs_per_day", "hp_smoking_years_quit", "hp_smoking_number_years", "smoking_pack_yrs",
                                                     "hp_lung_cancer_risk_stratification_level", "hp_lung_cancer_screening_adherence", "hp_lung_cancer_time_since_low_dose_ct_scan",
                                                     "hp_lung_cancer_yn", "count_lung_symptoms", "has_LUNG_symptoms_tf"))


cln_cancer_LUNG <- left_join(cln_cancer_LUNG, cln_ai_LUNG, by = "coloruser_id")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
##/////### BREAST CANCER TABLE CLEAN UP ##/////###
raw_dat_cancer_BREAST <- raw_dat_cancer_BREAST %>%
  mutate(hp_breast_cancer_symptom_irritation_or_dimpling_tf = case_when(
    hp_breast_cancer_symptom_irritation_or_dimpling_of__741e3637 == "Y" ~ 1,
    hp_breast_cancer_symptom_irritation_or_dimpling_of__741e3637 == "N" ~ 0
  ))

raw_dat_cancer_BREAST <- raw_dat_cancer_BREAST %>%
  mutate(hp_breast_cancer_symptom_new_lump_in_breast_or_underarm_tf = case_when(
    hp_breast_cancer_symptom_new_lump_in_breast_or_underarm_yn == "Y" ~ 1,
    hp_breast_cancer_symptom_new_lump_in_breast_or_underarm_yn == "N" ~ 0
  ))

raw_dat_cancer_BREAST <- raw_dat_cancer_BREAST %>% 
  mutate(hp_breast_cancer_symptom_nipple_discharge_tf = case_when(
    hp_breast_cancer_symptom_nipple_discharge_yn == "Y" ~ 1,
    hp_breast_cancer_symptom_nipple_discharge_yn == "N" ~ 0
  ))

raw_dat_cancer_BREAST <- raw_dat_cancer_BREAST %>% 
  mutate(hp_breast_cancer_symptom_pulling_or_pain_in_nipple_tf = case_when(
    hp_breast_cancer_symptom_pulling_or_pain_in_nipple_yn == "Y" ~ 1,
    hp_breast_cancer_symptom_pulling_or_pain_in_nipple_yn == "N" ~ 0
  ))

raw_dat_cancer_BREAST <- raw_dat_cancer_BREAST %>% 
  mutate(hp_breast_cancer_symptom_redness_or_flaky_skin_in_n_tf = case_when(
    hp_breast_cancer_symptom_redness_or_flaky_skin_in_n_08c0e613 == "Y" ~ 1,
    hp_breast_cancer_symptom_redness_or_flaky_skin_in_n_08c0e613 == "N" ~ 0
  ))

raw_dat_cancer_BREAST <- raw_dat_cancer_BREAST %>% 
  mutate(hp_breast_cancer_symptom_thickening_or_swelling_of_breast_tf = case_when(
    hp_breast_cancer_symptom_thickening_or_swelling_of_breast_yn == "Y" ~ 1,
    hp_breast_cancer_symptom_thickening_or_swelling_of_breast_yn == "N" ~ 0
  ))


cro(raw_dat_cancer_BREAST$hp_breast_cancer_symptom_irritation_or_dimpling_tf) 
cro(raw_dat_cancer_BREAST$hp_breast_cancer_symptom_new_lump_in_breast_or_underarm_tf) 
cro(raw_dat_cancer_BREAST$hp_breast_cancer_symptom_nipple_discharge_tf) 
cro(raw_dat_cancer_BREAST$hp_breast_cancer_symptom_redness_or_flaky_skin_in_n_tf) 
cro(raw_dat_cancer_BREAST$hp_breast_cancer_symptom_thickening_or_swelling_of_breast_tf) 
cro(raw_dat_cancer_BREAST$hp_breast_cancer_symptom_pulling_or_pain_in_nipple_tf) 


raw_dat_cancer_BREAST$count_breast_symptoms <- (raw_dat_cancer_BREAST$hp_breast_cancer_symptom_irritation_or_dimpling_tf 
                                           + raw_dat_cancer_BREAST$hp_breast_cancer_symptom_new_lump_in_breast_or_underarm_tf
                                           + raw_dat_cancer_BREAST$hp_breast_cancer_symptom_nipple_discharge_tf
                                           + raw_dat_cancer_BREAST$hp_breast_cancer_symptom_redness_or_flaky_skin_in_n_tf
                                           + raw_dat_cancer_BREAST$hp_breast_cancer_symptom_thickening_or_swelling_of_breast_tf 
                                           + raw_dat_cancer_BREAST$hp_breast_cancer_symptom_pulling_or_pain_in_nipple_tf) 


cro(raw_dat_cancer_BREAST$count_breast_symptoms) 

### create has symptoms category
raw_dat_cancer_BREAST <- raw_dat_cancer_BREAST %>% 
  mutate(has_BREAST_symptoms_tf = case_when(
    count_breast_symptoms >= 1 ~ TRUE,
    count_breast_symptoms < 1 ~ FALSE))

cro(raw_dat_cancer_BREAST$has_BREAST_symptoms_tf) 

## create data frame 
colnames(raw_dat_cancer_BREAST)

cln_cancer_BREAST <- subset(raw_dat_cancer_BREAST, select = c("coloruser_id", "health_profile_id", "hp_breast_cancer_yn", "hp_mastectomy_yn", 
                                                         "hp_breast_cancer_risk_stratification_level", "hp_breast_cancer_screening_adherence", 
                                                         "hp_breast_cancer_time_since_last_mammogram", "count_breast_symptoms", "has_BREAST_symptoms_tf"))


cln_cancer_BREAST <- left_join(cln_cancer_BREAST, cln_ai_BREAST, by = "coloruser_id")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
##/////### CERVICAL CANCER TABLE CLEAN UP ##/////###
colnames(raw_dat_cancer_CERVICAL)
raw_dat_cancer_CERVICAL <- raw_dat_cancer_CERVICAL %>%
  mutate(hp_cervical_cancer_symptom_bleeding_after_intercour_tf = case_when(
    hp_cervical_cancer_symptom_bleeding_after_intercour_910a8836 == "Y" ~ 1,
    hp_cervical_cancer_symptom_bleeding_after_intercour_910a8836 == "N" ~ 0
  ))

raw_dat_cancer_CERVICAL <- raw_dat_cancer_CERVICAL %>%
  mutate(hp_cervical_cancer_symptom_heavy_vaginal_bleeding_tf = case_when(
    hp_cervical_cancer_symptom_heavy_vaginal_bleeding_yn == "Y" ~ 1,
    hp_cervical_cancer_symptom_heavy_vaginal_bleeding_yn == "N" ~ 0
  ))

raw_dat_cancer_CERVICAL <- raw_dat_cancer_CERVICAL %>% 
  mutate(hp_cervical_cancer_symptom_bleeding_after_menopause_tf = case_when(
    hp_cervical_cancer_symptom_bleeding_after_menopause_yn == "Y" ~ 1,
    hp_cervical_cancer_symptom_bleeding_after_menopause_yn == "N" ~ 0
  ))

raw_dat_cancer_CERVICAL <- raw_dat_cancer_CERVICAL %>% 
  mutate(hp_cervical_cancer_symptom_unexplained_persistent_tf = case_when(
    hp_cervical_cancer_symptom_unexplained_persistent_p_88591c86 == "Y" ~ 1,
    hp_cervical_cancer_symptom_unexplained_persistent_p_88591c86 == "N" ~ 0
  ))


cro(raw_dat_cancer_CERVICAL$hp_cervical_cancer_symptom_unexplained_persistent_tf) 
cro(raw_dat_cancer_CERVICAL$hp_cervical_cancer_symptom_bleeding_after_menopause_tf) 
cro(raw_dat_cancer_CERVICAL$hp_cervical_cancer_symptom_heavy_vaginal_bleeding_tf) 
cro(raw_dat_cancer_CERVICAL$hp_cervical_cancer_symptom_bleeding_after_intercour_tf) 



raw_dat_cancer_CERVICAL$count_CERVICAL_symptoms <- (raw_dat_cancer_CERVICAL$hp_cervical_cancer_symptom_unexplained_persistent_tf 
                                               + raw_dat_cancer_CERVICAL$hp_cervical_cancer_symptom_bleeding_after_menopause_tf 
                                               + raw_dat_cancer_CERVICAL$hp_cervical_cancer_symptom_heavy_vaginal_bleeding_tf
                                               + raw_dat_cancer_CERVICAL$hp_cervical_cancer_symptom_bleeding_after_intercour_tf) 



cro(raw_dat_cancer_CERVICAL$count_CERVICAL_symptoms) 

### create has symptoms category
raw_dat_cancer_CERVICAL <- raw_dat_cancer_CERVICAL %>% 
  mutate(has_CERVICAL_symptoms_tf = case_when(
    count_CERVICAL_symptoms >= 1 ~ TRUE,
    count_CERVICAL_symptoms < 1 ~ FALSE))

cro(raw_dat_cancer_CERVICAL$has_CERVICAL_symptoms_tf) 

## create data frame 
colnames(raw_dat_cancer_CERVICAL)

cln_cancer_CERVICAL <- subset(raw_dat_cancer_CERVICAL, select = c("coloruser_id", "health_profile_id", "hp_cervical_cancer_yn", "hp_procedure_hysterectomy_yn", 
                                                             "hp_cervical_cancer_risk_stratification_level", "hp_cervical_cancer_time_since_last_screening", 
                                                             "hp_cervical_cancer_screening_adherence", "hp_cervical_cancer_screening_result", "count_CERVICAL_symptoms", 
                                                             "has_CERVICAL_symptoms_tf"))


cln_cancer_CERVICAL <- left_join(cln_cancer_CERVICAL, cln_ai_CERVICAL, by = "coloruser_id")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
##/////### COLORECTAL CANCER TABLE CLEAN UP ##/////###

            
raw_dat_cancer_CRC <- raw_dat_cancer_CRC %>%
  mutate(hp_colorectal_cancer_symptom_blood_in_stool_tf = case_when(
    hp_colorectal_cancer_symptom_blood_in_stool_yn == "Y" ~ 1,
    hp_colorectal_cancer_symptom_blood_in_stool_yn == "N" ~ 0
  ))

raw_dat_cancer_CRC <- raw_dat_cancer_CRC %>%
  mutate(hp_colorectal_cancer_symptom_change_in_bowel_movements_tf = case_when(
    hp_colorectal_cancer_symptom_change_in_bowel_movements_yn == "Y" ~ 1,
    hp_colorectal_cancer_symptom_change_in_bowel_movements_yn == "N" ~ 0
  ))

raw_dat_cancer_CRC <- raw_dat_cancer_CRC %>% 
  mutate(hp_colorectal_cancer_symptom_rectal_bleeding_tf = case_when(
    hp_colorectal_cancer_symptom_rectal_bleeding_yn == "Y" ~ 1,
    hp_colorectal_cancer_symptom_rectal_bleeding_yn == "N" ~ 0
  ))

raw_dat_cancer_CRC <- raw_dat_cancer_CRC %>% 
  mutate(hp_colorectal_cancer_symptom_persistent_abdominal_tf = case_when(
    hp_colorectal_cancer_symptom_persistent_abdominal_o_97837a45 == "Y" ~ 1,
    hp_colorectal_cancer_symptom_persistent_abdominal_o_97837a45 == "N" ~ 0
  ))
#cro(raw_dat_cancer_CRC$hp_colorectal_cancer_symptom_persistent_abdominal_o_97837a45)

#cro(raw_dat_cancer_CRC$hp_colorectal_cancer_symptom_blood_in_stool_tf) 
cro(raw_dat_cancer_CRC$hp_colorectal_cancer_symptom_rectal_bleeding_tf) 
cro(raw_dat_cancer_CRC$hp_colorectal_cancer_symptom_change_in_bowel_movements_tf) 
#cro(raw_dat_cancer_CRC$hp_colorectal_cancer_symptom_persistent_abdominal_tf) 


raw_dat_cancer_CRC$count_CRC_symptoms <- (raw_dat_cancer_CRC$hp_colorectal_cancer_symptom_rectal_bleeding_tf 
                                               + raw_dat_cancer_CRC$hp_colorectal_cancer_symptom_change_in_bowel_movements_tf) 



cro(raw_dat_cancer_CRC$count_CRC_symptoms) 

### create has symptoms category
raw_dat_cancer_CRC <- raw_dat_cancer_CRC %>% 
  mutate(has_CRC_symptoms_tf = case_when(
    count_CRC_symptoms >= 1 ~ TRUE,
    count_CRC_symptoms < 1 ~ FALSE))

cro(raw_dat_cancer_CRC$has_CRC_symptoms_tf) 

## create data frame 
colnames(raw_dat_cancer_CRC)

cln_cancer_CRC <- subset(raw_dat_cancer_CRC, select = c("coloruser_id", "health_profile_id", "hp_colorectal_cancer_yn", "hp_polyps_diagnosis_yn", "hp_had_10_or_more_polyps_yn",
                                                   "hp_cystic_fibrosis_diagnosis_yn", "hp_diagnosed_cystic_fibrosis_yn", "hp_crohns_disease_diagnosis_yn", "hp_diagnosed_crohns_disease_yn",
                                                   "hp_colorectal_cancer_screening_adherence", "hp_colorectal_cancer_risk_stratification_level", "hp_colorectal_cancer_time_since_last_colonoscopy", "hp_colorectal_cancer_result_last_colonoscopy",         
                                                    "hp_colorectal_cancer_time_since_last_partial_colonoscopy", "hp_colorectal_cancer_result_last_partial_colonoscopy", "hp_colorectal_cancer_time_since_last_stool_blood_test",
                                                    "hp_colorectal_cancer_result_last_stool_blood_test",  "hp_colorectal_cancer_time_since_last_stool_dna_test", "hp_colorectal_cancer_result_last_stool_dna_test",
                                                    "hp_colorectal_cancer_time_since_last_virtual_colonoscopy", "hp_colorectal_cancer_result_last_virtual_colonoscopy", 
                                                     "count_CRC_symptoms", "has_CRC_symptoms_tf"))


cln_cancer_CRC <- left_join(cln_cancer_CRC, cln_ai_CRC, by = "coloruser_id")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
##/////### PROSTATE CANCER TABLE CLEAN UP ##/////###

raw_dat_cancer_PROSTATE <- raw_dat_cancer_PROSTATE %>%
  mutate(hp_prostate_cancer_symptom_blood_in_urine_or_semen_tf = case_when(
    hp_prostate_cancer_symptom_blood_in_urine_or_semen_yn == "Y" ~ 1,
    hp_prostate_cancer_symptom_blood_in_urine_or_semen_yn == "N" ~ 0
  ))

raw_dat_cancer_PROSTATE <- raw_dat_cancer_PROSTATE %>%
  mutate(hp_prostate_cancer_symptom_changes_in_size_firmness_tf = case_when(
    hp_prostate_cancer_symptom_changes_in_size_firmness_68b3b624 == "Y" ~ 1,
    hp_prostate_cancer_symptom_changes_in_size_firmness_68b3b624 == "N" ~ 0
  ))

raw_dat_cancer_PROSTATE <- raw_dat_cancer_PROSTATE %>% 
  mutate(hp_prostate_cancer_symptom_difficulty_urinating_tf = case_when(
    hp_prostate_cancer_symptom_difficulty_urinating_yn == "Y" ~ 1,
    hp_prostate_cancer_symptom_difficulty_urinating_yn == "N" ~ 0
  ))

raw_dat_cancer_PROSTATE <- raw_dat_cancer_PROSTATE %>% 
  mutate(hp_prostate_cancer_symptom_frequent_urges_to_urinate_tf = case_when(
    hp_prostate_cancer_symptom_frequent_urges_to_urinate_yn == "Y" ~ 1,
    hp_prostate_cancer_symptom_frequent_urges_to_urinate_yn == "N" ~ 0
  ))


raw_dat_cancer_PROSTATE <- raw_dat_cancer_PROSTATE %>% 
  mutate(hp_prostate_cancer_symptom_pain_or_burning_during_u_tf = case_when(
    hp_prostate_cancer_symptom_pain_or_burning_during_u_26b5809d == "Y" ~ 1,
    hp_prostate_cancer_symptom_pain_or_burning_during_u_26b5809d == "N" ~ 0
  ))
#cro(raw_dat_cancer_PROSTATE$hp_prostate_cancer_symptom_frequent_pain_in_lower_tf)

raw_dat_cancer_PROSTATE <- raw_dat_cancer_PROSTATE %>% 
  mutate(hp_prostate_cancer_symptom_frequent_pain_in_lower_tf = case_when(
    hp_prostate_cancer_symptom_frequent_pain_in_lower_b_32f42917 == "Y" ~ 1,
    hp_prostate_cancer_symptom_frequent_pain_in_lower_b_32f42917 == "N" ~ 0
  ))

raw_dat_cancer_PROSTATE <- raw_dat_cancer_PROSTATE %>% 
  mutate(hp_prostate_cancer_symptom_pain_when_touching_the_p_tf = case_when(
    hp_prostate_cancer_symptom_pain_when_touching_the_p_8985644e == "Y" ~ 1,
    hp_prostate_cancer_symptom_pain_when_touching_the_p_8985644e == "N" ~ 0
  ))



cro(raw_dat_cancer_PROSTATE$hp_prostate_cancer_symptom_pain_or_burning_during_u_tf)
#cro(raw_dat_cancer_PROSTATE$hp_prostate_cancer_symptom_pain_when_touching_the_p_tf) 
#cro(raw_dat_cancer_PROSTATE$hp_prostate_cancer_symptom_blood_in_urine_or_semen_tf) 
cro(raw_dat_cancer_PROSTATE$hp_prostate_cancer_symptom_difficulty_urinating_tf) 
cro(raw_dat_cancer_PROSTATE$hp_prostate_cancer_symptom_changes_in_size_firmness_tf) 
cro(raw_dat_cancer_PROSTATE$hp_prostate_cancer_symptom_frequent_urges_to_urinate_tf) 
#cro(raw_dat_cancer_PROSTATE$hp_prostate_cancer_symptom_frequent_pain_in_lower_tf) 




raw_dat_cancer_PROSTATE$count_PROSTATE_symptoms <- (raw_dat_cancer_PROSTATE$hp_prostate_cancer_symptom_frequent_urges_to_urinate_tf
                                     + raw_dat_cancer_PROSTATE$hp_prostate_cancer_symptom_difficulty_urinating_tf) 



cro(raw_dat_cancer_PROSTATE$count_PROSTATE_symptoms) 

### create has symptoms category
raw_dat_cancer_PROSTATE <- raw_dat_cancer_PROSTATE %>% 
  mutate(has_PROSTATE_symptoms_tf = case_when(
    count_PROSTATE_symptoms >= 1 ~ TRUE,
    count_PROSTATE_symptoms < 1 ~ FALSE))

cro(raw_dat_cancer_PROSTATE$has_PROSTATE_symptoms_tf) 

## create data frame 
colnames(raw_dat_cancer_PROSTATE)

cln_cancer_PROSTATE <- subset(raw_dat_cancer_PROSTATE, select = c("coloruser_id", "health_profile_id", "hp_prostate_cancer_yn",
                                                             "hp_prostate_cancer_risk_stratification_level", "hp_prostate_cancer_time_since_psa_screening",
                                                             "hp_prostate_cancer_screening_adherence", "hp_prostate_cancer_psa_test_result", 
                                                             "count_PROSTATE_symptoms", "has_PROSTATE_symptoms_tf"))

cln_cancer_PROSTATE <- left_join(cln_cancer_PROSTATE, cln_ai_PROSTATE, by = "coloruser_id")

