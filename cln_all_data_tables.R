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

## join demographics w/ cancer data
cln_data_all <- left_join(cln_demographics, cln_cancer_LUNG, by = "coloruser_id")

cln_data_all <- left_join(cln_data_all, cln_cancer_BREAST, by = "coloruser_id")

cln_data_all <- left_join(cln_data_all, cln_cancer_CERVICAL, by = "coloruser_id")

cln_data_all <- left_join(cln_data_all, cln_cancer_CRC, by = "coloruser_id")

cln_data_all <- left_join(cln_data_all, cln_cancer_PROSTATE, by = "coloruser_id")

cln_data_all <- left_join(cln_data_all, cln_all_tests, by = "coloruser_id")

##calculate age categories

colnames(cln_data_all)

## CREATE AGE GROUP CATEGORIES - categories are based on screening guidelines/recommendations

##age at registration
cln_data_all <- cln_data_all %>% 
  mutate(age_at_registration_cat = case_when(
    Care_Program_Participants_Phi__age_at_registration < 40.0 ~ "Less than 40 years old",
    Care_Program_Participants_Phi__age_at_registration >= 40.0 & Care_Program_Participants_Phi__age_at_registration < 45.0 ~ "40 to 44 years old",
    Care_Program_Participants_Phi__age_at_registration >= 45.0 & Care_Program_Participants_Phi__age_at_registration < 50.0 ~ "45 to 49 years old",
    Care_Program_Participants_Phi__age_at_registration >= 50.0 & Care_Program_Participants_Phi__age_at_registration < 55.0 ~ "50 to 54 years old",
    Care_Program_Participants_Phi__age_at_registration >= 55.0 & Care_Program_Participants_Phi__age_at_registration < 60.0 ~ "55 to 59 years old",
    Care_Program_Participants_Phi__age_at_registration >= 60.0 ~ "60 years and older"
  ))

cro(cln_data_all$age_at_registration_cat)

##age NOW
cln_data_all <- cln_data_all %>% 
  mutate(age_now_cat = case_when(
    Care_Program_Participants_Phi__age_now < 40.0 ~ "Less than 40 years old",
    Care_Program_Participants_Phi__age_now >= 40.0 & Care_Program_Participants_Phi__age_now < 45.0 ~ "40 to 44 years old",
    Care_Program_Participants_Phi__age_now >= 45.0 & Care_Program_Participants_Phi__age_now < 50.0 ~ "45 to 49 years old",
    Care_Program_Participants_Phi__age_now >= 50.0 & Care_Program_Participants_Phi__age_now < 55.0 ~ "50 to 54 years old",
    Care_Program_Participants_Phi__age_now >= 55.0 & Care_Program_Participants_Phi__age_now < 60.0 ~ "55 to 59 years old",
    Care_Program_Participants_Phi__age_now >= 60.0 ~ "60 years and older"
  ))

cro(cln_data_all$age_now_cat)

###-------### ###-------### ###-------### ###-------### ###-------### ###-------### ###-------### ###-------### ###-------### 
##SYMPTOMS DATA

colnames(cln_data_all)

dfcount_ANY_symptoms <-subset(cln_data_all, select = c("coloruser_id", "count_PROSTATE_symptoms", "count_CRC_symptoms", "count_CERVICAL_symptoms",
                                                     "count_breast_symptoms", "count_lung_symptoms"))
                                                     
dfcount_ANY_symptoms$count_ANY_symptoms <- rowSums(dfcount_ANY_symptoms [,c("count_PROSTATE_symptoms", "count_CRC_symptoms", "count_CERVICAL_symptoms",
                                                  "count_breast_symptoms", "count_lung_symptoms")], na.rm=TRUE)

cro(dfcount_ANY_symptoms$count_ANY_symptoms)

dfcount_ANY_symptoms <- dfcount_ANY_symptoms %>% 
  mutate(report_ANY_symptoms = case_when(
    count_ANY_symptoms >= 1 ~ TRUE,
    count_ANY_symptoms < 1 ~ FALSE
  ))

cro(dfcount_ANY_symptoms$report_ANY_symptoms)

dfcount_ANY_symptoms <- dfcount_ANY_symptoms %>%
  select(coloruser_id, report_ANY_symptoms, count_ANY_symptoms)

cln_data_all <- left_join(cln_data_all, dfcount_ANY_symptoms, by = "coloruser_id")

#cln_data_all <- left_join(cln_data_all, dfcount_ANY_symptoms, by = "coloruser_id", select = c("coloruser_id", "count_ANY_symptoms"))

cro(cln_data_all$count_ANY_symptoms)
cro(cln_data_all$report_ANY_symptoms)

###-------### ###-------### ###-------### ###-------### ###-------### ###-------### ###-------### 

### ADHERENCE DATA

dfcount_ANY_adherence <-subset(cln_data_all, select = c("coloruser_id", "hp_cervical_cancer_screening_adherence", "hp_colorectal_cancer_screening_adherence", "hp_breast_cancer_screening_adherence",
                                                       "hp_prostate_cancer_screening_adherence", "hp_lung_cancer_screening_adherence"))

cln_data_all <- cln_data_all %>% 
  mutate(not_adherent_any = case_when(
    hp_cervical_cancer_screening_adherence == "not_up_to_date" | hp_colorectal_cancer_screening_adherence == "not_up_to_date" |
      hp_breast_cancer_screening_adherence == "not_up_to_date" | hp_prostate_cancer_screening_adherence == "not_up_to_date" |
        hp_lung_cancer_screening_adherence == "not_up_to_date" ~ TRUE,
    hp_cervical_cancer_screening_adherence != "not_up_to_date" | is.na(hp_cervical_cancer_screening_adherence) & 
      hp_colorectal_cancer_screening_adherence != "not_up_to_date" &
      hp_breast_cancer_screening_adherence != "not_up_to_date" | is.na(hp_breast_cancer_screening_adherence) &
      hp_prostate_cancer_screening_adherence != "not_up_to_date" | is.na(hp_prostate_cancer_screening_adherence) &
      hp_lung_cancer_screening_adherence != "not_up_to_date"| is.na(hp_lung_cancer_screening_adherence) ~ FALSE)
  )

###-------### ###-------### ###-------### ###-------### ###-------### ###-------### ###-------### ###-------### ###-------### 
###-------###  TEST KITS ###-------###  
colnames(cln_data_all)

dfcount_ANY_test <-subset(cln_data_all, select = c("coloruser_id", "FIT_kit_count", "HPV_kit_count", "H30_kit_count",
                                                       "C30_kit_count", "PGx_kit_count", "PSA_kit_count"))

dfcount_ANY_test$count_ANY_test <- rowSums(dfcount_ANY_test [,c("FIT_kit_count", "HPV_kit_count", "H30_kit_count",
                                                                            "C30_kit_count", "PGx_kit_count", "PSA_kit_count")], na.rm=TRUE)

cro(dfcount_ANY_test$count_ANY_test)

dfcount_ANY_test <- dfcount_ANY_test %>% 
  mutate(report_ANY_test = case_when(
    count_ANY_test >= 1 ~ TRUE,
    count_ANY_test < 1 ~ FALSE
  ))

cro(dfcount_ANY_test$report_ANY_test)

##keep old DF so you can see which users have multiple tests of the same type
dfcount_ANY_test.x <- dfcount_ANY_test %>%
  select(coloruser_id, report_ANY_test, count_ANY_test)

cln_data_all <- left_join(cln_data_all, dfcount_ANY_test.x, by = "coloruser_id")

###-------### ###-------### ###-------### ###-------### ###-------### ###-------### ###-------### ###-------### ###-------### 
###-------###  RISK DATA ###-------### 

colnames(cln_data_all)

#CRC
cln_data_all <- cln_data_all %>% 
  mutate(CRC_high_risk_ct = case_when(
    hp_colorectal_cancer_risk_stratification_level == "high" ~ 1,
    hp_colorectal_cancer_risk_stratification_level == "average" | hp_colorectal_cancer_risk_stratification_level == "unknown" ~ 0
  ))

cro(cln_data_all$CRC_high_risk_ct)

#BREAST
cln_data_all <- cln_data_all %>% 
  mutate(breast_high_risk_ct = case_when(
    hp_breast_cancer_risk_stratification_level == "high" ~ 1,
    hp_breast_cancer_risk_stratification_level == "average" | hp_breast_cancer_risk_stratification_level == "unknown" ~ 0
  ))

cro(cln_data_all$breast_high_risk_ct)

#CERVICAL
cln_data_all <- cln_data_all %>% 
  mutate(cervical_high_risk_ct = case_when(
    hp_cervical_cancer_risk_stratification_level == "high" ~ 1,
    hp_cervical_cancer_risk_stratification_level == "average" | hp_cervical_cancer_risk_stratification_level == "unknown" ~ 0
  ))

cro(cln_data_all$cervical_high_risk_ct)

#PROSTATE
cln_data_all <- cln_data_all %>% 
  mutate(prostate_high_risk_ct = case_when(
    hp_prostate_cancer_risk_stratification_level == "high" ~ 1,
    hp_prostate_cancer_risk_stratification_level == "average" | hp_prostate_cancer_risk_stratification_level == "unknown" ~ 0
  ))

cro(cln_data_all$prostate_high_risk_ct)

#LUNG
cln_data_all <- cln_data_all %>% 
  mutate(lung_high_risk_ct = case_when(
    hp_lung_cancer_risk_stratification_level == "high" ~ 1,
    hp_lung_cancer_risk_stratification_level == "average" | hp_lung_cancer_risk_stratification_level == "unknown" ~ 0
  ))

cro(cln_data_all$lung_high_risk_ct)

###-------### ADD ANY RISK TO CLN TABLE
dfcount_ANY_risk <-subset(cln_data_all, select = c("coloruser_id", "lung_high_risk_ct", "CRC_high_risk_ct", "breast_high_risk_ct",
                                                       "cervical_high_risk_ct", "prostate_high_risk_ct"))

dfcount_ANY_risk$count_ANY_risk <- rowSums(dfcount_ANY_risk [,c("lung_high_risk_ct", "CRC_high_risk_ct", "breast_high_risk_ct",
                                                                "cervical_high_risk_ct", "prostate_high_risk_ct")], na.rm=TRUE)
cro(dfcount_ANY_risk$count_ANY_risk)

dfcount_ANY_risk <- dfcount_ANY_risk %>% 
  mutate(report_ANY_risk = case_when(
    count_ANY_risk >= 1 ~ TRUE,
    count_ANY_risk < 1 ~ FALSE
  ))

cro(dfcount_ANY_risk$report_ANY_risk)

dfcount_ANY_risk <- dfcount_ANY_risk %>%
  select(coloruser_id, report_ANY_risk, count_ANY_risk)

cln_data_all <- left_join(cln_data_all, dfcount_ANY_risk, by = "coloruser_id")


###-------### ###-------### ###-------### ###-------### ###-------### ###-------### ###-------### ###-------### ###-------### 
###-------### ###-------### ###-------### ###-------### ###-------### ###-------### ###-------### ###-------### ###-------### 
###-------### ###-------### ###-------### ###-------### ###-------### ###-------### ###-------### ###-------### ###-------### 


#### TABLES ####
#### TABLES ####
#### TABLES #### DELETE ->> MOVE THIS TO R MARKDOWN
#### TABLES ####


##TABLES
library(arsenal)
my_controls <- tableby.control(
  test = T,
  total = T,
  numeric.test = "kwt", cat.test = "chisq",
  numeric.stats = c("meansd", "medianq1q3", "range", "Nmiss2"),
  cat.stats = c("countpct", "Nmiss2"),
  stats.labels = list(
    meansd = "Mean (SD)",
    medianq1q3 = "Median (Q1, Q3)",
    range = "Min - Max",
    Nmiss2 = "Missing"
  ),
  digits = 2L, digits.count = 0L, digits.pct = 1L, digits.p = 3L,
)

colnames(cln_data_all)
my_labels2 <- list(gender = "Gender", 
                   sex_assigned_at_birth = "Sex assigned at birth",
                   Care_Program_Participants_Phi__age_at_registration = "Age at registration", 
                   address_state = "State of residence",
                   census_ethnicity_groups = "Race and Ethnicity",
                   hp_smoking_status = "Smoking status",
                   smoking_pack_yrs = "Pack Years",
                   hp_lung_cancer_screening_adherence = "Lung Cancer Screening Adherence",
                   has_LUNG_symptoms_tf = "Has lung cancer symptoms",
                   hp_breast_cancer_screening_adherence = "Breast Cancer Screening Recommendation",
                   has_BREAST_symptoms_tf = "Has breast cancer symptoms",
                   hp_cervical_cancer_screening_adherence = "Cervical Cancer Screening Recommendation",
                   has_cervical_symptoms_tf = "Has cervical cancer symptoms",
                   hp_colorectal_cancer_screening_adherence = "Colorectal Cancer Screening Recommendation",
                   has_CRC_symptoms_tf = "Has colorectal cancer symptoms",
                   hp_prostate_cancer_screening_adherence = "Prostate Cancer Screening Recommendation",
                   has_prostate_symptoms_tf = "Has prostate cancer symptoms",
                   report_ANY_risk = "At increased risk for 1 or more cancers",
                   report_ANY_symptoms = "Reports 1 or more symptoms",
                   not_adherent_any = "Due for 1 or more cancer screenings",
                   
               
                   has_cancer_history = "Has cancer hx",
                   has_no_cancer_history = "Has no cancer hx",
                   has_active_cancer_or_chemotherapy = "Current cancer dx or tx",
                   
                   ## Breast Cancer LABELS
                   has_breast_cancer_history = "Has breast cancer hx",
                   has_family_history_breast_cancer = "Has family hx breast cancer",
                   breast_changes_status = "Has had recent changes to breast",
                   mammogram_in_last_two_years_status = "Had mammogram w/in last 2yrs",
                   has_increased_risk_breast_cancer = "Is at increased risk of breast cancer",
                   is_average_risk_and_up_to_date_screening_breast_cancer = "Avg risk of breast cancer + up-to-date on screening",
                   is_average_risk_and_may_be_due_for_screening_breast_cancer = "Avg risk of breast cancer + may be due for screening",
                   
                   ## CRC Cancer LABELS
                   has_colon_cancer_history = "Has CRC cancer hx",
                   colon_cancer_screening_in_last_ten_years_status = "Had colon cancer screening w/in last 10 yrs",
                   colon_cancer_screening_type = "Type of colon cancer screening",
                   has_colon_cancer_risk_ibs = "Has IBS dx",
                   has_colon_cancer_risk_symptoms = "Has CRC symptoms",
                   has_no_colon_cancer_risk_factors = "Has CRC risk factors",
                   has_colon_cancer_risk_polyps = "Has hx of colon polyps",
                   has_family_history_colon_cancer = "Has family hx of CRC",
                   has_increased_risk_colon_cancer = "Is at increased risk of CRC cancer",
                   is_average_risk_and_up_to_date_screening_colon_cancer = "Avg risk of CRC + up-to-date on screening",
                   is_average_risk_and_may_be_due_for_screening_colon_cancer = "Avg risk of CRC + may be due for screening",
                   
                   ## CERVICAL CANCER LABELS
                   has_cervical_cancer_history = "Has cervical cancer hx",
                   abnormal_pap_smear_status = "Had abnormal pap test",
                   cervical_cancer_screening_in_last_three_years_status = "Had cervical cancer screening w/in last 3 yrs",
                   has_increased_risk_cervical_cancer = "Is at increased risk of cervical cancer",
                   is_average_risk_and_up_to_date_screening_cervical_cancer = "Avg risk of cervical + up-to-date on screening",
                   is_average_risk_and_may_be_due_for_screening_cervical_cancer = "Avg risk of cervical + may be due for screening",
                   
                   ##PROSTATE CANCER
                   has_prostate_cancer_history = "Has prostate cancer hx",
                   relative_diagnosed_with_prostate_cancer_under_65_status = "Has family hx of prostate cancer (w/ dx < 65 yrs)",
                   has_family_history_prostate_cancer = "Has family hx of prostate cancer",
                   has_increased_risk_prostate_cancer = "Is at increased risk of prostate cancer",
                   
                   ##LUNG CANCER
                   has_lung_cancer_history = "Has lung cancer hx",
                   #is_eligible_screening_lung_cancer = "Is eligible for lung cancer screening"
                   
                   ## KIT METRICS
                   kitorder_replacement_reason = "Kit Order Replacement Type",
                   time_from_kitorder_order_to_participant_delivery_hours = "Time from kit order to delivery (hrs)",
                   time_from_participant_delivery_to_kit_activation_hours = "Time from kit delivery to activation (hrs)",
                   time_from_kit_activation_to_shipment_to_lab_hours = "Time from kit activation to shipment to lab (hrs)",
                   time_from_kitorder_shipment_to_lab_to_accession_hours = "Time from shipment to lab to lab accessioning (hrs)",
                   time_from_accession_to_report_release_hours = "Time from accessioning to report release (hrs)",
                   time_from_kitorder_order_to_lab_to_report_release_hours = "Time from kit order to report release (hrs)",
                   time_from_report_opened_to_consult_creation_hours = "Time from report opened to consult scheudled (hrs)",
                   time_from_report_release_to_report_open_hours = "Time from report released to report opened (hrs)",
                   sample_sequence_run_rejection_note = "Reason for kit failure (i.e., sample rejection)",
                   fulfillment_batch_distribution_type = "Kit fullfilment type",
                   issue_QNS = "Kit Failure - QNS",
                   issue_hemolyzed = "Kit Failure - Hemolyzed",
                   issue_fullfilment = "Kit Failure - Fullfilment issue"
                   
                   
)



#### TABLES ####
#### TABLES ####
#### TABLES #### DELETE ->> MOVE THIS TO R MARKDOWN
#### TABLES ####
##Table of population characteristics
colnames(cln_data_all)

# delete dup columns
#cln_data_all = select(cln_data_all, -report_ANY_risk.x, -count_ANY_risk.x, -report_ANY_risk.y, -count_ANY_risk.y)


table_one <- tableby( ~ Care_Program_Participants_Phi__age_at_registration + age_at_registration_cat + Care_Program_Participants_Phi__age_now + age_now_cat + sex_assigned_at_birth + gender + census_ethnicity_groups + address_state +
                        Care_Program_Participants_Phi__is_dependent + hp_smoking_status + report_ANY_symptoms + 
                        not_adherent_any + report_ANY_risk, data = cln_data_all, control = my_controls)
summary(table_one, 
        labelTranslations = my_labels2,
        title = "Table 1 - Characteristics of enrolled participants with care plans "
)


table_two <- tableby( ~ hp_breast_cancer_risk_stratification_level + hp_lung_cancer_risk_stratification_level + hp_cervical_cancer_risk_stratification_level +
                      hp_prostate_cancer_risk_stratification_level + hp_colorectal_cancer_risk_stratification_level, data = cln_data_all, control = my_controls)
  

## females only
cln_AFAB_data <- subset(cln_data_all, cln_data_all$sex_assigned_at_birth == "F")

## BREAST CA
colnames(cln_cancer_BREAST)


table_AFAB_breast <- tableby(hp_breast_cancer_risk_stratification_level ~ Care_Program_Participants_Phi__age_at_registration + census_ethnicity_groups +
                        Care_Program_Participants_Phi__is_dependent + hp_smoking_status + hp_breast_cancer_screening_adherence + hp_breast_cancer_time_since_last_mammogram + has_BREAST_symptoms_tf + report_ANY_symptoms + 
                        not_adherent_any, data = cln_AFAB_data, control = my_controls)


summary(table_AFAB_breast, 
        labelTranslations = my_labels2,
        title = "Table 3a - Characteristics of female participants by breast cancer risk"
       )

## CERVICAL CA
colnames(cln_cancer_CERVICAL)
table_AFAB_cervical <- tableby(hp_cervical_cancer_risk_stratification_level ~ Care_Program_Participants_Phi__age_at_registration + census_ethnicity_groups +
                                 Care_Program_Participants_Phi__is_dependent + hp_smoking_status + 
                                 hp_cervical_cancer_screening_adherence + hp_cervical_cancer_time_since_last_screening + 
                                 hp_cervical_cancer_screening_result + has_CERVICAL_symptoms_tf + report_ANY_symptoms + 
                                 not_adherent_any, data = cln_AFAB_data, control = my_controls)

summary(table_AFAB_cervical, 
        labelTranslations = my_labels2,
        title = "Table 3b - Characteristics of female participants by cervical cancer risk"
)

## MALES ONLY
cln_AMAB_data <- subset(cln_data_all, cln_data_all$sex_assigned_at_birth == "M")

## Prostate CA
colnames(cln_cancer_PROSTATE)
table_AMAB_prostate <- tableby(hp_prostate_cancer_risk_stratification_level ~ Care_Program_Participants_Phi__age_at_registration + census_ethnicity_groups +
                                 Care_Program_Participants_Phi__is_dependent + hp_smoking_status + hp_prostate_cancer_yn + 
                                 hp_prostate_cancer_screening_adherence + hp_prostate_cancer_time_since_psa_screening + 
                                 hp_prostate_cancer_psa_test_result + has_PROSTATE_symptoms_tf + report_ANY_symptoms + 
                                 not_adherent_any, data = cln_AMAB_data, control = my_controls)

summary(table_AMAB_prostate, 
        labelTranslations = my_labels2,
        title = "Table 3c - Characteristics of male participants by prostate cancer risk"
)


## SMOKERS ONLY
cln_smokers_data <- subset(cln_data_all, (cln_data_all$hp_smoking_status == "current_smoker" | cln_data_all$hp_smoking_status == "former_smoker"))

## Lung CA
colnames(cln_cancer_LUNG)

table_LUNG_risk <- tableby(hp_lung_cancer_risk_stratification_level ~ Care_Program_Participants_Phi__age_at_registration + sex_assigned_at_birth + 
                        gender + census_ethnicity_groups + Care_Program_Participants_Phi__is_dependent + hp_smoking_status + hp_smoking_cigs_per_day + 
                        hp_smoking_years_quit + hp_smoking_number_years + smoking_pack_yrs + hp_lung_cancer_screening_adherence +
                        hp_lung_cancer_time_since_low_dose_ct_scan + has_LUNG_symptoms_tf + report_ANY_symptoms + 
                                 not_adherent_any, data = cln_smokers_data, control = my_controls)

summary(table_LUNG_risk, 
        labelTranslations = my_labels2,
        title = "Table 3d - Characteristics of current and former smokers by lung cancer risk"
)

# by smoking status
table_LUNG_status <- tableby(hp_smoking_status ~ Care_Program_Participants_Phi__age_at_registration + sex_assigned_at_birth + 
                        gender + census_ethnicity_groups + Care_Program_Participants_Phi__is_dependent formula  + 
                        hp_lung_cancer_risk_stratification_level + hp_smoking_cigs_per_day + 
                        hp_smoking_years_quit + hp_smoking_number_years + smoking_pack_yrs + hp_lung_cancer_screening_adherence +
                        hp_lung_cancer_time_since_low_dose_ct_scan + has_LUNG_symptoms_tf + report_ANY_symptoms + 
                        not_adherent_any, data = cln_smokers_data, control = my_controls)

summary(table_LUNG_status, 
        labelTranslations = my_labels2,
        title = "Table 3d - Characteristics of current and former smokers by lung cancer risk"
)



## Colorectal CA
colnames(cln_cancer_CRC)


##missing IBS + UC FIELD <<<<--!!!!
table_CRC_risk <- tableby(hp_colorectal_cancer_risk_stratification_level ~ Care_Program_Participants_Phi__age_at_registration + sex_assigned_at_birth + 
                             gender + census_ethnicity_groups + Care_Program_Participants_Phi__is_dependent + hp_smoking_status + hp_polyps_diagnosis_yn + 
                            hp_had_10_or_more_polyps_yn + hp_cystic_fibrosis_diagnosis_yn + hp_crohns_disease_diagnosis_yn + hp_colorectal_cancer_screening_adherence +
                            hp_colorectal_cancer_time_since_last_colonoscopy + hp_colorectal_cancer_result_last_colonoscopy + hp_colorectal_cancer_time_since_last_partial_colonoscopy +
                            hp_colorectal_cancer_result_last_partial_colonoscopy + hp_colorectal_cancer_time_since_last_stool_blood_test + hp_colorectal_cancer_result_last_stool_blood_test +
                            hp_colorectal_cancer_time_since_last_stool_dna_test + hp_colorectal_cancer_result_last_stool_dna_test + hp_colorectal_cancer_time_since_last_virtual_colonoscopy +
                            hp_colorectal_cancer_result_last_virtual_colonoscopy + has_CRC_symptoms_tf + report_ANY_symptoms + 
                            not_adherent_any, data = cln_data_all, control = my_controls)

summary(table_CRC_risk, 
        labelTranslations = my_labels2,
        title = "Table 3e - Characteristics of participants by CRC risk"
)



