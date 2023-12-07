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
summary(raw_dat_all_population)
c <- unique(raw_dat_all_population$coloruser_id)
c1 <- unique(raw_dat_all_population$allow_list_entry_id)


##download demographics/data of users enrolled in program
summary(raw_dat_demographics)
a <- unique(raw_dat_demographics$coloruser_id)

colnames(raw_dat_demographics)

cln_demographics <- subset(raw_dat_demographics, select = c("coloruser_id", "health_profile_id", "Care_Program_Participants_Phi__is_dependent", "Care_Program_Participants_Phi__cancer_diagnosis_status",
                                                       "Care_Program_Participants_Phi__has_known_mutation_history", "Care_Program_Participants_Phi__is_cancer_caregiver", "hp_ancestry_ashkenazi_jewish",
                                                       "address_state", "address_zip_code", "sex_assigned_at_birth", "gender", "ethnicities", "us_census_ethnicity", "birthday",
                                                       "Care_Program_Participants_Phi__age_now", "Care_Program_Participants_Phi__age_at_registration", "Care_Program_population_membership_created_at", 
                                                       "Care_Program_Participants_Phi__has_action_items", "Care_Program_Participants_Phi__has_completed_action_items",
                                                       "Care_Program_Participants_Phi__count_with_action_items", "Care_Program_Participants_Phi__count_with_completed"))



##download at-home test/lab order data
summary(raw_dat_tests)
b <- unique(raw_dat_tests$coloruser_id)
b_kits <- unique(raw_dat_tests$Test_Orders_Phi__kitorder_id)

##download healtprofile data from TALL table
#db_healthprofile <- read_sheet("https://docs.google.com/spreadsheets/d/1Aykasa-z0dNzyz2U5cLertk34JUbfMKl5tOgjQ3XjnI/edit?usp=sharing")
#summary(db_healthprofile)


# # # # # 
# # # # # # # # # # 
# create ethnicity groups
cro(cln_demographics$ethnicities)
cln_demographics$ethnicity_asian <- grepl("Asian", cln_demographics$ethnicities, ignore.case = T) | grepl("Filipino", cln_demographics$ethnicities, ignore.case = T) | grepl("Japanese", cln_demographics$ethnicities, ignore.case = T) | grepl("Chinese", cln_demographics$ethnicities, ignore.case = T) 
cro(cln_demographics$ethnicity_asian)

cln_demographics$ethnicity_aa <- grepl("African American / Black", cln_demographics$ethnicities, ignore.case = T)
cro(cln_demographics$ethnicity_aa)

cln_demographics$ethnicity_white <- grepl("White", cln_demographics$ethnicities, ignore.case = T)
cro(cln_demographics$ethnicity_white)

cln_demographics$ethnicity_latino <- grepl("Latino/a", cln_demographics$ethnicities, ignore.case = T)
cro(cln_demographics$ethnicity_latino)

cln_demographics$ethnicity_aian <- grepl("Native American", cln_demographics$ethnicities, ignore.case = T)
cro(cln_demographics$ethnicity_aian)

cln_demographics$ethnicity_other_cat <- grepl("Other", cln_demographics$ethnicities, ignore.case = T)
cro(cln_demographics$ethnicity_other_cat)

#ethnicity count

cln_demographics$ethnicity_count <- (cln_demographics$ethnicity_aian + cln_demographics$ethnicity_asian + cln_demographics$ethnicity_aa
                                + cln_demographics$ethnicity_latino + cln_demographics$ethnicity_white
                                + cln_demographics$ethnicity_other_cat)

cro(cln_demographics$ethnicity_count)

### create multiple ethnicity category
cln_demographics <- cln_demographics %>% 
  mutate(ethnicity_multiple = case_when(
    ethnicity_count >= 2 ~ TRUE,
    ethnicity_count < 2 ~ FALSE))

summary(cln_demographics)
##add census ethnicity groups
cln_demographics <- cln_demographics %>%
  mutate(census_ethnicity_groups = case_when(
    ethnicity_multiple == TRUE & ethnicity_latino == FALSE ~ 'Multiethnic',
    ethnicity_multiple == FALSE & ethnicity_asian == TRUE & ethnicity_latino == FALSE ~ 'AAPI',
    ethnicity_multiple == FALSE & ethnicity_white == TRUE & ethnicity_latino == FALSE ~ 'White',
    ethnicity_multiple == FALSE & ethnicity_other_cat == TRUE & ethnicity_latino == FALSE ~ 'Other',
    ethnicity_multiple == FALSE & ethnicity_aian == TRUE & ethnicity_latino == FALSE ~ 'Native American or Alaskan',
    ethnicity_multiple == FALSE & ethnicity_aa == TRUE & ethnicity_latino == FALSE ~ 'Black or African American',
    ethnicity_latino == TRUE ~ 'Hispanic or Latino of any race',
    ethnicity_count == 0 ~ "Unknown"
  ))
cro(cln_demographics$census_ethnicity_groups)
cro(cln_demographics$us_census_ethnicity)

##########################
##########################
##########################
########################## STOP
##########################
##########################


## CERVICAL CANCER + MAMOGRAMS
cro(all_data$has_breast_cancer_history)
cro(all_data$mammogram_in_last_two_years_status)

cro(all_data$is_average_risk_and_up_to_date_screening_breast_cancer, all_data$age_at_kit_order)
all_data <- all_data %>% 
  mutate(compliant_mammogram_screening = case_when(
    sex == "F" &  age_at_kit_order >= 40 & mammogram_in_last_two_years_status == "No" ~ "Not compliant",
    sex == "F" &  age_at_kit_order >= 40 & mammogram_in_last_two_years_status == "Yes" ~ "Compliant (last mammogram < 2yrs)",
    sex == "F" &  age_at_kit_order < 40 & mammogram_in_last_two_years_status == "No" ~ "Not applicable (under 40yrs)",
    sex == "M" ~ "Not applicable"
  ))
cro(all_data$compliant_mammogram_screening)

all_data <- all_data %>% 
  mutate(compliant_bc_screening_tf = case_when(
    compliant_mammogram_screening == "Compliant (last mammogram < 2yrs)" ~ TRUE,
    compliant_mammogram_screening == "Not compliant" ~ FALSE
  ))
cro(all_data$compliant_bc_screening_tf)

## COLORECTAL CANCER 
cro(all_data$has_colon_cancer_history)
cro(all_data$has_colon_cancer_risk_ibs)
cro(all_data$has_colon_cancer_risk_polyps)
#cro(all_data$has_colon_cancer_risk_symptoms)
cro(all_data$colon_cancer_screening_in_last_ten_years_status)

## CRC Screening
all_data <- all_data %>% 
  mutate(compliant_CRC_screening = case_when(
    age_at_kit_order >= 45 & colon_cancer_screening_in_last_ten_years_status == "No" ~ "Not compliant",
    age_at_kit_order >= 45 & colon_cancer_screening_in_last_ten_years_status == "Yes" ~ "Compliant (last screening < 10 yrs)",
    age_at_kit_order < 45 ~ "Not applicable (under 45yrs)"
  ))
cro(all_data$compliant_CRC_screening)

all_data <- all_data %>% 
  mutate(compliant_CRC_screening_tf = case_when(
    compliant_CRC_screening == "Not compliant" ~ FALSE,
    compliant_CRC_screening == "Compliant (last screening < 10 yrs)" ~ TRUE
  ))

cro(all_data$compliant_CRC_screening_tf)

cro(all_data$compliant_CRC_screening)
cro(all_data$is_average_risk_and_may_be_due_for_screening_colon_cancer)
cro(all_data$is_average_risk_and_up_to_date_screening_colon_cancer)
cro(all_data$has_increased_risk_colon_cancer)

## PROSTATE CANCER 
cro(all_data$has_prostate_cancer_history)
cro(all_data$relative_diagnosed_with_prostate_cancer_under_65_status)
cro(all_data$has_family_history_prostate_cancer)

#50 is cut off for ACS, recommends shared decision making, tbd if we should include other cut offs for race/ethnicity and/or family hx
all_data <- all_data %>% 
  mutate(eligible_PR_screening = case_when(
    sex == "M" & age_at_kit_order >= 50 ~ "Eligible for Shared Decision Making",
    sex == "M" & age_at_kit_order < 50 ~ "Not applicable (AGE)",
    sex == "F" ~ "Not applicable (Female)"
  ))

cro(all_data$eligible_PR_screening)

all_data <- all_data %>% 
  mutate(eligible_PR_screening_tf = case_when(
    eligible_PR_screening == "Eligible for Shared Decision Making" ~ TRUE,
    eligible_PR_screening == "Not applicable (AGE)" ~ FALSE
  ))

cro(all_data$eligible_PR_screening_tf)
cro(all_data$eligible_PR_screening, all_data$sex)

## CERVICAL CANCER##
cro(all_data$cervical_cancer_screening_in_last_three_years_status)
cro(all_data$has_cervical_cancer_history)
cro(all_data$abnormal_pap_smear_status)

##HPV + PAP testing
all_data <- all_data %>% 
  mutate(eligible_cervical_cancer_screening = case_when(
    sex == "F" & age_at_kit_order >= 21 & age_at_kit_order < 30 & cervical_cancer_screening_in_last_three_years_status == "No" ~ "Pap testing recommended",
    sex == "F" & age_at_kit_order >= 30 & age_at_kit_order < 65 & cervical_cancer_screening_in_last_three_years_status == "No" ~ "Pap test and HPV test may be recommended",
    sex == "F" & age_at_kit_order >= 21 & age_at_kit_order < 65 & cervical_cancer_screening_in_last_three_years_status == "Yes" ~ "Compliant (last screening 3yrs)",
    sex == "F" & age_at_kit_order < 21 | age_at_kit_order >= 65 ~ "Not applicable (AGE)",
    sex == "M" ~ "Not applicable"
  ))

cro(all_data$eligible_cervical_cancer_screening)


all_data <- all_data %>% 
  mutate(eligible_cervical_cancer_screening_tf = case_when(
    eligible_cervical_cancer_screening == "Compliant (last screening 3yrs)" ~ FALSE,
    eligible_cervical_cancer_screening == "Pap testing recommended" | eligible_cervical_cancer_screening == "Pap test and HPV test may be recommended"~ TRUE
  ))

cro(all_data$eligible_cervical_cancer_screening_tf)


## LUNG CANCER ##
cro(all_data$has_lung_cancer_history)
cro(all_data$is_eligible_screening_lung_cancer)
cro(all_data$cigarettes_per_day)
cro(all_data$years_smoked)
cro(all_data$smoking_status)
cro(all_data$has_smoked_in_last_fifteen_years)

cro(all_data$cigarettes_per_day, all_data$years_smoked)

## calculate pack-years
all_data$smoking_pack_yrs = ((all_data$cigarettes_per_day/20)*all_data$years_smoked)

cro(all_data$smoking_pack_yrs)

all_data <- all_data %>% 
  mutate(eligible_lung_cancer_screening = case_when(
    age_at_kit_order >= 50 & smoking_pack_yrs >= 20 & (has_smoked_in_last_fifteen_years == TRUE | smoking_status == "Current") ~ "Eligible for Lung Cancer Screening",
    age_at_kit_order >= 50 & smoking_pack_yrs < 20 | has_smoked_in_last_fifteen_years == FALSE & smoking_status != "Current" ~ "Not Applicable (Former Smoker)",
    age_at_kit_order < 50 ~ "Not applicable (AGE)"
  ))

cro(all_data$eligible_lung_cancer_screening)
cro(all_data$is_eligible_screening_lung_cancer)
cro(all_data$smoking_pack_yrs)

all_data <- all_data %>% 
  mutate(eligible_lung_cancer_screening_tf = case_when(
    eligible_lung_cancer_screening == "Eligible for Lung Cancer Screening" ~ TRUE,
    eligible_lung_cancer_screening == "Not Applicable (Former Smoker)" ~ FALSE
  ))

cro(all_data$eligible_lung_cancer_screening_tf)
cro(all_data$eligible_lung_cancer_screening_tf, all_data$smoking_status)


#all_data$count_eligbile_screening <- with(all_data, (compliant_bc_screening_tf + compliant_CRC_screening_tf + eligible_cervical_cancer_screening_tf + eligible_lung_cancer_screening_tf + eligible_PR_screening_tf))
#cro(all_data$count_eligbile_screening)

### common DM medications: https://dtc.ucsf.edu/types-of-diabetes/type2/treatment-of-type-2-diabetes/medications-and-therapies/type-2-non-insulin-therapies/table-of-medications/

##find insulin rx
all_data$insulin_rx <- grepl("insulin", all_data$current_medication_list, ignore.case = T) | grepl("humalog", all_data$current_medication_list, ignore.case = T) | 
  grepl("novolin", all_data$current_medication_list, ignore.case = T) | grepl("humulin", all_data$current_medication_list, ignore.case = T) | grepl("lantus", all_data$current_medication_list, ignore.case = T) | 
  grepl("toujeo", all_data$current_medication_list, ignore.case = T) | grepl("tresiba", all_data$current_medication_list, ignore.case = T) | grepl("novolog", all_data$current_medication_list, ignore.case = T) 

cro(all_data$insulin_rx)

##find non-insulin rx
all_data$non_insulin_dm_rx <- grepl("metformin", all_data$current_medication_list, ignore.case = T) | grepl("glitazone", all_data$current_medication_list, ignore.case = T) | grepl("lipizide", all_data$current_medication_list, ignore.case = T) | 
  grepl("lyburide", all_data$current_medication_list, ignore.case = T) | grepl("glutide", all_data$current_medication_list, ignore.case = T) | grepl("acarbose", all_data$current_medication_list, ignore.case = T) | 
  grepl("glinide", all_data$current_medication_list, ignore.case = T) |  grepl("exenatide", all_data$current_medication_list, ignore.case = T) | grepl("gliptin", all_data$current_medication_list, ignore.case = T) | 
  grepl("gliflozin", all_data$current_medication_list, ignore.case = T) | grepl("glucophage", all_data$current_medication_list, ignore.case = T) | grepl("trulicity", all_data$current_medication_list, ignore.case = T) | 
  grepl("sulfonylureas", all_data$current_medication_list, ignore.case = T) | grepl("januvia", all_data$current_medication_list, ignore.case = T) | grepl("nesina", all_data$current_medication_list, ignore.case = T) |
  grepl("fortamet", all_data$current_medication_list, ignore.case = T) | grepl("glumetza", all_data$current_medication_list, ignore.case = T) | grepl("wegovy", all_data$current_medication_list)

cro(all_data$non_insulin_dm_rx)

##find any DM rx

all_data$any_dm_rx <- grepl("insulin", all_data$current_medication_list, ignore.case = T) | grepl("humalog", all_data$current_medication_list, ignore.case = T) | 
  grepl("novolin", all_data$current_medication_list, ignore.case = T) | grepl("humulin", all_data$current_medication_list, ignore.case = T) | grepl("lantus", all_data$current_medication_list, ignore.case = T) | 
  grepl("toujeo", all_data$current_medication_list, ignore.case = T) | grepl("tresiba", all_data$current_medication_list, ignore.case = T) | grepl("novolog", all_data$current_medication_list, ignore.case = T) |
  grepl("metformin", all_data$current_medication_list, ignore.case = T) | grepl("glitazone", all_data$current_medication_list, ignore.case = T) | grepl("lipizide", all_data$current_medication_list, ignore.case = T) | 
  grepl("lyburide", all_data$current_medication_list, ignore.case = T) | grepl("glutide", all_data$current_medication_list, ignore.case = T) | grepl("acarbose", all_data$current_medication_list, ignore.case = T) | 
  grepl("glinide", all_data$current_medication_list, ignore.case = T) |  grepl("exenatide", all_data$current_medication_list, ignore.case = T) | grepl("gliptin", all_data$current_medication_list, ignore.case = T) | 
  grepl("gliflozin", all_data$current_medication_list, ignore.case = T) | grepl("glucophage", all_data$current_medication_list, ignore.case = T) | grepl("trulicity", all_data$current_medication_list, ignore.case = T) | 
  grepl("sulfonylureas", all_data$current_medication_list, ignore.case = T) | grepl("januvia", all_data$current_medication_list, ignore.case = T) | grepl("nesina", all_data$current_medication_list, ignore.case = T) |
  grepl("fortamet", all_data$current_medication_list, ignore.case = T) | grepl("glumetza", all_data$current_medication_list, ignore.case = T) | grepl("diabetes", all_data$current_medication_list) |
  grepl("wegovy", all_data$current_medication_list)

cro(all_data$any_dm_rx)

## data frame w/ dm pts and rx
#dm_rx_df <- subset(all_data, all_data$has_any_dm == TRUE, select = c("coloruser_id", "kitorder_id", "sex", "dm_rx", "diabetes", "current_medication_list", "any_dm_rx", "non_insulin_dm_rx", "insulin_rx", "dm_dx_type", "Hemoglobin_A1C"))

###CALCULATE # OF INDIVIDUALS W/ ACTIONABLE DIABETES RESULTS
all_data <- all_data %>% 
  mutate(actionable_DM = case_when(
    has_any_dm == FALSE & Hemoglobin_A1C < 5.7 ~ FALSE,
    has_any_dm == FALSE & Hemoglobin_A1C >= 5.7 ~ TRUE,
    has_any_dm == TRUE & Hemoglobin_A1C < 7.0 ~ FALSE,
    has_any_dm == TRUE & Hemoglobin_A1C >= 7.0 ~ TRUE
  ))

cro(all_data$actionable_DM)



###CALCULATE # OF INDIVIDUALS W/ ACTIONABLE HTN RESULTS
all_data <- all_data %>% 
  mutate(actionable_HTN = case_when(
    has_high_blood_pressure == FALSE & vitals_systolic_blood_pressure < 130.0 & vitals_diastolic_blood_pressure < 80.0 ~ FALSE,
    has_high_blood_pressure == FALSE & vitals_systolic_blood_pressure >= 130.0 | vitals_diastolic_blood_pressure >= 80.0 ~ TRUE,
    has_high_blood_pressure == TRUE & vitals_systolic_blood_pressure < 130.0 & vitals_diastolic_blood_pressure < 80.0 ~ FALSE,
    has_high_blood_pressure == TRUE & vitals_systolic_blood_pressure >= 130.0 | vitals_diastolic_blood_pressure >= 80.0 ~ TRUE
  ))

cro(all_data$actionable_HTN)
cro(all_data$BP_goal)


all_data <- all_data %>%
  mutate(cv_race_grp = case_when(
    ethnicity_aa == TRUE ~ 'aa',
    ethnicity_white == TRUE ~ 'white',
    ethnicity_white == FALSE &  ethnicity_aa == FALSE ~ 'other'
  ))

cro(all_data$cv_race_grp)

all_data <- all_data %>% 
  mutate(cv_sex = case_when(
    sex == "F" ~ 0,
    sex == "M" ~ 1
  ))
cro(all_data$cv_sex)

all_data <- all_data %>% 
  mutate(cv_smoke = case_when(
    smoking_status == "Current" ~ 1,
    smoking_status == "Former" | smoking_status == "Never" ~ 0
  ))

cro(all_data$smoking_status)
cro(all_data$cv_smoke)

#all_data$dm_rx <- grepl("diabetes", all_data$medication_for_conditions, ignore.case = F) | grepl("pre_diabetes", all_data$medication_for_conditions, ignore.case = F) 
#all_data$bp_rx <- grepl("high_blood_pressure", all_data$medication_for_conditions, ignore.case = F) 

#all_data <- all_data %>% 
#  mutate(cv_bp = case_when(
#    bp_rx == TRUE ~ 1,
#    bp_rx == FALSE ~ 0
#  ))
#cro(all_data$cv_bp)

# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# STOP It looks like we cut questions for "are you taking an rx for any of these conditions - need to follow-up.
# w/o this information we cannot calculate the ADA score of ASCVD score
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------


all_data <- all_data %>% 
  mutate(cv_dm = case_when(
    has_diabetes == TRUE ~ 1,
    has_diabetes == FALSE ~ 0
  ))

cro(all_data$cv_dm)

### STATIN MEDICATION
all_data$any_statin_rx <- grepl("statin", all_data$current_medication_list, ignore.case = T) | grepl("Lipitor", all_data$current_medication_list, ignore.case = T) | grepl("Crestor", all_data$current_medication_list, ignore.case = T) | grepl("Zocor", all_data$current_medication_list, ignore.case = T) | grepl("Lescol", all_data$current_medication_list, ignore.case = T) | grepl("Pravachol", all_data$current_medication_list, ignore.case = T) | 
  grepl("Pravachol", all_data$current_medication_list, ignore.case = T) | grepl("Livalo", all_data$current_medication_list, ignore.case = T) | grepl("Altoprev", all_data$current_medication_list, ignore.case = T) | grepl("Vytorin", all_data$current_medication_list, ignore.case = T) | grepl("Caduet", all_data$current_medication_list) | grepl("cholesterol", all_data$current_medication_list)
cro(all_data$any_statin_rx)


#In patients 40 to 75 years of age with diabetes mellitus and LDL-C ≥70 mg/dL (≥1.8 mmol/L), start moderate-intensity statin therapy without calculating 10-year ASCVD risk.
#In patients with severe primary hypercholesterolemia (LDL-C level ≥190 mg/dL [≥4.9 mmol/L]), without calculating 10-year ASCVD risk, begin high-intensity statin therapy.
all_data <- all_data %>% 
  mutate(should_start_statin = case_when(
    LDL_Cholesterol >= 190 |
      (LDL_Cholesterol >= 70 & has_diabetes == TRUE & age_at_kit_order >= 40.0 & age_at_kit_order <= 75.0) ~ TRUE
  ))

cro(all_data$should_start_statin)

all_data <- all_data %>% 
  mutate(should_start_statin.tf = case_when(
    should_start_statin == TRUE ~ TRUE,
    is.na(should_start_statin) ~ FALSE
  ))


cro(all_data$should_start_statin)
cro(all_data$should_start_statin, all_data$any_statin_rx)
cro(all_data$has_high_cholesterol, all_data$any_statin_rx)


#LDL goals
all_data <- all_data %>% 
  mutate(ldl_goal = case_when(
    LDL_Cholesterol < 100 ~ "Optimal",
    LDL_Cholesterol >= 100 & LDL_Cholesterol < 130 ~ "Near optimal",
    LDL_Cholesterol >= 130 & LDL_Cholesterol < 160 ~ "Borderline high",
    LDL_Cholesterol >= 160 & LDL_Cholesterol < 190 ~ "High",
    LDL_Cholesterol >= 190 ~ "Very high"
  ))
cro(all_data$ldl_goal)
cro(all_data$has_high_cholesterol, all_data$ldl_goal)


## TIME SINCE LAST CHECK-UP
cro(all_data$time_since_last_checkup)

all_data <- all_data %>% 
  mutate(last_checkup_one_yr = case_when(
    time_since_last_checkup == "1 year ago or less" ~ TRUE,
    time_since_last_checkup == "More than 1 but less than 2 years ago" | time_since_last_checkup == "More than 2 years ago" | time_since_last_checkup == "Never" ~ FALSE
  ))

cro(all_data$last_checkup_one_yr)

## Hba1C DM results category - 

all_data <- all_data %>% 
  mutate(hba1c_DM_tf = case_when(
    hba1c_cat == "Normal" ~ FALSE,
    hba1c_cat == "Diabetes" | hba1c_cat == "Pre-Diabetes" ~ TRUE
  ))

cro(all_data$hba1c_DM_tf)

##### ~~~~~~ ###### ~~~~~~ #####
##### ~~~~~~ ###### ~~~~~~ ##### ~~~~~~ ######
##### ~~~~~~ ###### ~~~~~~ ##### ~~~~~~ ###### ~~~~~~ ##### ACVSD 10 YR Risk ##### ~~~~~~ ###### ~~~~~~ ##### ~~~~~~ ###### ~~~~~~ ##### 
##### ~~~~~~ ###### ~~~~~~ ##### ~~~~~~ ###### ~~~~~~ ##### ACVSD 10 YR Risk  ##### ~~~~~~ ###### ~~~~~~ ##### ~~~~~~ ###### ~~~~~~ ##### 
##### ~~~~~~ ###### ~~~~~~ ##### ~~~~~~ ######
##### ~~~~~~ ###### ~~~~~~ #####

library(PooledCohort)
library(dplyr, warn.conflicts = FALSE)
## 2013 Goff PCE is intended for patients with LDL-C < 190 mg/dL (4.92 mmol/L), without ASCVD, not on LDL-C lowering therapy


all_data <- all_data %>%
  mutate(race = case_when(
    ethnicity_aa == TRUE ~ 'black',
    ethnicity_white == TRUE ~ 'white',
    ethnicity_white == FALSE &  ethnicity_aa == FALSE ~ 'white'
  ))

cro(all_data$race)

all_data <- all_data %>% 
  mutate(sex = case_when(
    sex == "F" ~ "female",
    sex == "M" ~ "male"
  ))
cro(all_data$sex)

all_data <- all_data %>% 
  mutate(smoke_current = case_when(
    smoking_status == "current_smoker" ~ "yes",
    smoking_status != "current_smoker" ~ "no"
  ))

cro(all_data$smoking_status, all_data$smoke_current)

all_data <- all_data %>% 
  mutate(bp_meds = case_when(
    any_htn_rx == TRUE ~ "yes",
    any_htn_rx == FALSE ~ "no"
  ))

cro(all_data$bp_meds)

all_data <- all_data %>% 
  mutate(diabetes = case_when(
    has_diabetes == TRUE ~ "yes",
    has_diabetes == FALSE ~ "no"
  ))

cro(all_data$diabetes)

#create data frame w/ inputs
db_acsvd_risk <- subset(all_data, select = c("coloruser_id.x", "kitorder_id", "sex", "age_at_kit_order", "bp_meds", "diabetes", "smoke_current", "race", "Total_Cholesterol",
                                             "vitals_systolic_blood_pressure", "HDL_Cholesterol"))
## rename columns for pooled cohorts
db_acsvd_risk <- db_acsvd_risk %>% 
  rename("age_years" = "age_at_kit_order",
         "chol_total_mgdl"= "Total_Cholesterol",
         "bp_sys_mmhg" = "vitals_systolic_blood_pressure",
         "chol_hdl_mgdl" = "HDL_Cholesterol")


db_acsvd_risk <- db_acsvd_risk %>% 
  mutate(goff_2013_risk = predict_10yr_ascvd_risk(
    sex = sex,
    race = race,
    age_years = age_years,
    chol_total_mgdl = chol_total_mgdl,
    chol_hdl_mgdl = chol_hdl_mgdl,
    bp_sys_mmhg = bp_sys_mmhg,
    bp_meds = bp_meds,
    smoke_current = smoke_current,
    diabetes = diabetes,
    override_boundary_errors = TRUE
  ))

db_acsvd_risk$goff_2013_risk_pct <- round((db_acsvd_risk$goff_2013_risk*100), digits = 2)


db_acsvd_risk <- db_acsvd_risk %>% 
  mutate(yad_2018_risk = predict_10yr_ascvd_risk(
    sex = sex,
    race = race,
    age_years = age_years,
    chol_total_mgdl = chol_total_mgdl,
    chol_hdl_mgdl = chol_hdl_mgdl,
    bp_sys_mmhg = bp_sys_mmhg,
    bp_meds = bp_meds,
    smoke_current = smoke_current,
    diabetes = diabetes,
    equation_version = "Yadlowsky_2018",
    override_boundary_errors = TRUE
  ))

db_acsvd_risk$yad_2018_risk_pct <- round((db_acsvd_risk$yad_2018_risk*100), digits = 2)

##### ~~~~~~ ###### ~~~~~~ #####
##join ACSVD risk scores to all data set
##### ~~~~~~ ###### ~~~~~~ #####

db_acsvd_risk_scores <- subset(db_acsvd_risk, select = c("kitorder_id", "yad_2018_risk", "yad_2018_risk_pct", "goff_2013_risk", "goff_2013_risk_pct"))
all_data <- full_join(all_data, db_acsvd_risk_scores, by = "kitorder_id")


##### ~~~~~~ ###### ~~~~~~ #####
##### ~~~~~~ ###### ~~~~~~ ##### ~~~~~~ ######
#Cat for ACSVD 10 yr risks = Low-risk (<5%); Borderline risk (5% to 7.4%);  Intermediate risk (7.5% to 19.9%); High risk (≥20%)

all_data <- all_data %>% 
  mutate(yad_2018_cat = case_when(
    yad_2018_risk_pct < 5.0 ~ "Low-risk",
    yad_2018_risk_pct >= 5.0 & yad_2018_risk_pct < 7.5 ~ "Borderline risk",
    yad_2018_risk_pct >= 7.5 & yad_2018_risk_pct < 20.0 ~ "Intermediate risk",
    yad_2018_risk_pct >= 20.0 ~ "High risk",
    is.na(yad_2018_risk_pct) ~ "Missing"
  ))
cro(all_data$yad_2018_cat)

all_data <- all_data %>% 
  mutate(goff_2013_cat = case_when(
    goff_2013_risk_pct < 5.0 ~ "Low-risk",
    goff_2013_risk_pct >= 5.0 & goff_2013_risk_pct < 7.5 ~ "Borderline risk",
    goff_2013_risk_pct >= 7.5 & goff_2013_risk_pct < 20.0 ~ "Intermediate risk",
    goff_2013_risk_pct >= 20.0 ~ "High risk",
    is.na(goff_2013_risk_pct) ~ "Missing"
  ))
cro(all_data$goff_2013_cat)


### algorithm for recommendations
all_data <- all_data %>% 
  mutate(statin_rec_2022 = case_when(
    LDL_Cholesterol >= 190.0 ~ "Start High Intensity Statin",
    age_at_kit_order < 20.0 & LDL_Cholesterol < 190.0 ~ "Lifestyle management",
    (age_at_kit_order >= 20.0 & age_at_kit_order < 40.0) & LDL_Cholesterol < 160.0 ~ "Lifestyle management",
    (age_at_kit_order >= 20.0 & age_at_kit_order < 40.0) & (LDL_Cholesterol >= 160.0 & LDL_Cholesterol < 190.0) ~ "Consider Statin Rx",
    (age_at_kit_order >= 40.0 & age_at_kit_order < 75.0) & cv_dm == TRUE & LDL_Cholesterol < 190.0 ~ "Start Moderate Intensity Statin",
    (age_at_kit_order >= 40.0 & age_at_kit_order < 75.0) & cv_dm == FALSE & LDL_Cholesterol < 70.0 ~ "Lifestyle management",
    (age_at_kit_order >= 40.0 & age_at_kit_order < 75.0) & cv_dm == FALSE & (LDL_Cholesterol >= 70.0 & LDL_Cholesterol < 190.0) & yad_2018_cat == "Low-risk" ~ "Lifestyle management",
    (age_at_kit_order >= 40.0 & age_at_kit_order < 75.0) & cv_dm == FALSE & (LDL_Cholesterol >= 70.0 & LDL_Cholesterol < 190.0) & yad_2018_cat == "Borderline risk" ~ "Consider Statin Rx",
    (age_at_kit_order >= 40.0 & age_at_kit_order < 75.0) & cv_dm == FALSE & (LDL_Cholesterol >= 70.0 & LDL_Cholesterol < 190.0) & (yad_2018_cat == "Intermediate risk" | yad_2018_cat == "High risk") ~ "Start Statin Rx",
    age_at_kit_order >= 75.0 & LDL_Cholesterol < 190.0  ~ "Clinical assessment and risk discussion"
  ))
cro(all_data$statin_rec_2022)


### eligbility for AJ dyslyp program
all_data <- all_data %>% 
  mutate(eligbile_ldl_program = case_when(
    ((age_at_kit_order >= 26.0 & age_at_kit_order < 80.0) & LDL_Cholesterol >= 190.0) |
      ((age_at_kit_order >= 40.0 & age_at_kit_order <= 75.0) & (Hemoglobin_A1C >= 6.5 | has_diabetes == TRUE) & LDL_Cholesterol > 70.0) |
      ((age_at_kit_order >= 40.0 & age_at_kit_order <= 75.0) & yad_2018_risk_pct >= 7.5) ~ TRUE
  ))

all_data <- all_data %>% 
  mutate(eligbile_ldl_program = case_when(
    eligbile_ldl_program == TRUE ~ TRUE,
    is.na(eligbile_ldl_program) ~ FALSE
  ))

cro(all_data$eligbile_ldl_program)

###
all_data <- all_data %>% 
  mutate(eligbile_ldl_program_cat = case_when(
    ((age_at_kit_order >= 26.0 & age_at_kit_order < 80.0) & LDL_Cholesterol >= 190.0) ~ "26-80 y/o with LDL >= 190 mg/dL",
    ((age_at_kit_order >= 40.0 & age_at_kit_order <= 75.0) & (Hemoglobin_A1C >= 6.5 | has_diabetes == TRUE) & LDL_Cholesterol > 70.0) ~ "40-75 y/o with LDL >= 70 mg/dL and HbA1c >= 6.5% or prior DM dx",
    ((age_at_kit_order >= 40.0 & age_at_kit_order <= 75.0) & yad_2018_risk_pct >= 7.5) ~ "40-75 y/o with LDL >= 100 mg/dL and >= 7.5% ACSVD-10 yr risk" 
  ))
cro(all_data$eligbile_ldl_program_cat)

cro(all_data$has_high_cholesterol)

cro(all_data$eligbile_ldl_program_cat)

# eligilty for AJ HTN program
all_data <- all_data %>% 
  mutate(eligbile_HTN_program = case_when(
    vitals_systolic_blood_pressure < 130.0 & vitals_diastolic_blood_pressure < 80.0 ~ FALSE,
    vitals_systolic_blood_pressure >= 130.0 | vitals_diastolic_blood_pressure >= 80.0 ~ TRUE
  ))

cro(all_data$eligbile_HTN_program)
cro(all_data$eligbile_HTN_program, all_data$eligbile_ldl_program)

### all program eligible
all_data <- all_data %>% 
  mutate(eligbile_any_program = case_when(
    eligbile_HTN_program == TRUE & eligbile_ldl_program == TRUE ~ "Both LDL + HTN",
    eligbile_HTN_program == FALSE & eligbile_ldl_program == TRUE ~ "LDL only",
    eligbile_HTN_program == TRUE & eligbile_ldl_program == FALSE ~ "HTN only",
    eligbile_HTN_program == FALSE & eligbile_ldl_program == FALSE ~ "None"
  ))
cro(all_data$eligbile_any_program)

cro(all_data$has_high_blood_pressure, all_data$eligbile_HTN_program)
cro_cpct(all_data$has_high_blood_pressure, all_data$eligbile_HTN_program)

cro(all_data$BP_cat, all_data$eligbile_HTN_program)

cro(all_data$has_high_cholesterol, all_data$eligbile_ldl_program)
cro_cpct(all_data$has_high_cholesterol, all_data$eligbile_ldl_program)

##### DATA FRAME FOR INDIVIDUALS MISSING STATIN REC ##### ~~~~~~ ###### ~~~~~~ ##### ~~~~~~ ###### ~~~~~~ ##### 
##### DATA FRAME FOR INDIVIDUALS MISSING STATIN REC ##### ~~~~~~ ###### ~~~~~~ ##### ~~~~~~ ###### ~~~~~~ ##### 

df_missing_statin_rec <- subset(all_data, is.na(all_data$statin_rec_2022), select = c("coloruser_id.x", "kitorder_id", "statin_rec_2022", "sex", "age_at_kit_order", "bp_meds", "diabetes", "smoke_current", "race", "Total_Cholesterol",
                                                                                      "vitals_systolic_blood_pressure","LDL_Cholesterol", "HDL_Cholesterol", "actionable_HTN", "has_high_blood_pressure", "has_high_cholesterol", "any_statin_rx", "yad_2018_cat",
                                                                                      "yad_2018_risk_pct", "goff_2013_cat", "goff_2013_risk_pct"))


##### ~~~~~~ ###### ~~~~~~ ##### ~~~~~~ ###### ~~~~~~ ##### TABLES ##### ~~~~~~ ###### ~~~~~~ ##### ~~~~~~ ###### ~~~~~~ ##### 
##### ~~~~~~ ###### ~~~~~~ ##### ~~~~~~ ###### ~~~~~~ ##### TABLES ##### ~~~~~~ ###### ~~~~~~ ##### ~~~~~~ ###### ~~~~~~ ##### 
##### ~~~~~~ ###### ~~~~~~ ##### ~~~~~~ ######
##### ~~~~~~ ###### ~~~~~~ #####


###CALCULATE # OF INDIVIDUALS W/ ACTIONABLE LDL RESULTS
all_data <- all_data %>% 
  mutate(actionable_ldl = case_when(
    any_statin_rx == FALSE & (statin_rec_2022 == "Start Moderate Intensity Statin" | statin_rec_2022 == "Start Statin Rx" | statin_rec_2022 == "Start High Intensity Statin") ~ TRUE,
    any_statin_rx == TRUE & (statin_rec_2022 == "Start Moderate Intensity Statin" | statin_rec_2022 == "Start Statin Rx" | statin_rec_2022 == "Start High Intensity Statin") ~ TRUE,
    any_statin_rx == TRUE | (statin_rec_2022 != "Start Moderate Intensity Statin" & statin_rec_2022 != "Start Statin Rx" & statin_rec_2022 != "Start High Intensity Statin") ~ FALSE
  ))

cro(all_data$actionable_ldl)
##Calculate co-morbs
all_data$cvd_comorb_ct <- (all_data$has_any_dm + all_data$has_high_cholesterol + all_data$has_high_blood_pressure)
cro(all_data$cvd_comorb_ct)


##Calculate any actionable result
all_data$any_actionable_cvd_ct <- (all_data$actionable_DM + all_data$actionable_HTN + all_data$actionable_ldl)
cro(all_data$any_actionable_cvd_ct)

all_data <- all_data %>% 
  mutate(any_actionable_cvd_tf = case_when(
    any_actionable_cvd_ct == 0 ~ FALSE,
    any_actionable_cvd_ct >= 1 ~ TRUE
  ))

cro(all_data$any_actionable_cvd_tf)



#####
all_data <- all_data %>% 
  mutate(actionable_ldl_result_type = case_when(
    actionable_ldl == TRUE & has_high_cholesterol == TRUE ~ "Disease Management (LDL)",
    actionable_ldl == TRUE & has_high_cholesterol == FALSE ~ "New Dx (LDL)"
  ))

cro(all_data$actionable_ldl_result_type)

all_data <- all_data %>% 
  mutate(actionable_HTN_result_type = case_when(    
    actionable_HTN == TRUE & has_high_blood_pressure == TRUE & any_htn_rx == TRUE ~ "Disease Management (HTN - taking rx)",
    actionable_HTN == TRUE & has_high_blood_pressure == TRUE & any_htn_rx == FALSE ~ "Disease Management (HTN - no rx)",
    actionable_HTN == TRUE & has_high_blood_pressure == FALSE & any_htn_rx == TRUE ~ "New Dx (HTN - taking rx)",
    actionable_HTN == TRUE & has_high_blood_pressure == FALSE & any_htn_rx == FALSE ~ "New Dx (HTN - no rx)"
  ))
cro(all_data$actionable_HTN_result_type)


all_data <- all_data %>% 
  mutate(actionable_DM_result_type = case_when(
    actionable_DM == TRUE & has_prediabetes == TRUE ~ "Disease Management (Prediabetes)",
    actionable_DM == TRUE & has_diabetes == TRUE ~ "Disease Management (Diabetes)",
    actionable_DM == TRUE & hba1c_cat == "Pre-Diabetes" & has_prediabetes == FALSE & has_diabetes == FALSE ~ "New Dx (Prediabetes)",
    actionable_DM == TRUE & hba1c_cat == "Diabetes" & has_diabetes == FALSE ~ "New Dx (Diabetes)"
  ))   

cro(all_data$actionable_DM_result_type)
cro(all_data$consult_status)

### CREATED CONSULT Y/N
all_data <- all_data %>% 
  mutate(consult_created_tf = case_when(
    consult_status == "finished" | consult_status == "amendment" | consult_status == "scheduled" ~ TRUE,
    consult_status == "canceled" | is.na(consult_status) ~ FALSE
  ))   
cro(all_data$consult_created_tf)

##### ~~~~~~ ###### ~~~~~~ ##### ~~~~~~ ###### ~~~~~~ ###### ~~~~~~ ##### ~~~~~~ ###### 
##### ~~~~~~ ###### ~~~~~~ ##### ~~~~~~ ###### ~~~~~~ ###### ~~~~~~ ##### ~~~~~~ ######
##characteristics tables

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

my_labels2 <- list(participant_gender_identity = "Gender", 
                   sex = "Sex assigned at birth",
                   age_at_kit_order = "Age", 
                   census_ethnicity_groups = "Race and Ethnicity",
                   smoking_status = "Smoking status",
                   smoking_pack_yrs = "Pack Years",
                   has_diabetes = "Has Diabetes",
                   has_high_blood_pressure = "Has high blood pressure",
                   has_high_cholesterol = "Has high cholesterol",
                   ada_dm_risk_cat = "ADA Risk Score",
                   days_per_week_physically_active = "Number of days active per week (30 min or more)",
                   cat_BMI = "BMI",
                   yad_2018_cat = "ACSVD-10 yr Risk Category (Yadlowsky 2018)",
                   yad_2018_risk_pct = "ACSVD-10 yr Risk % (Yadlowsky 2018)",
                   statin_rec_2022 = "Primary Prevention of ACSVD Recommendation (2022)",
                   dm_dx_type = "Diabetes Dx",
                   time_since_last_checkup = "Time since last PCP visit",
                   BP_cat = "Blood Pressure (at-home)",
                   AST_cat = "Aspartate Transferase (AST) DBS Test (U/L)",
                   ALT_cat = "Alanine Transaminase (ALT) DBS Test (U/L)",
                   hba1c_cat = "HbA1c DBS Test - Diabetes Screening",
                   total_chol_cat = "Total Cholesterol (mg/dL)",
                   HDL_chol_cat = "HDL Cholesterol (mg/dL)",
                   LDL_chol_cat = "LDL Cholesterol (mg/dL)",
                   triglycerides_cat = "Triglycerides (mg/dL)",
                   anxiety_GAD2_cat = "GAD-2 Results",
                   depression_PHQ2_cat = "PHQ-2 Results",
                   compliant_mammogram_screening = "Breast Cancer Screening Recommendation",
                   compliant_CRC_screening = "CRC Screening Recommendation",
                   eligible_cervical_cancer_screening = "Cervical Cancer Screening Recommendation",
                   eligible_PR_screening = "Prostate Cancer Screening Recommendation",
                   eligible_lung_cancer_screening = "Lung Cancer Screening Recommendation",
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

cro(cln_demographics$participant_us_census_ethnicity)

#### TABLES ####
#### TABLES ####
#### TABLES ####
#### TABLES ####
##Table of population characteristics
table_one <- tableby( ~ age_at_kit_order + sex + participant_gender_identity + participant_us_census_ethnicity + 
                        fulfillment_batch_distribution_type + time_from_participant_delivery_to_kit_activation_hours
                      + time_from_kitorder_order_to_lab_to_report_release_hours + issue_fullfilment + issue_hemolyzed
                      + issue_QNS, data = merged_data, control = my_controls)
summary(table_one, 
        labelTranslations = my_labels2,
        title = "Table 1.a - Characteristics of requested kits"
)



##Table of population characteristics
table_one.a <- tableby( ~ age_at_kit_order + sex + participant_gender_identity + census_ethnicity_groups
                        + days_per_week_physically_active + last_checkup_one_yr + has_pcp + cat_BMI 
                        + smoking_status + smoking_pack_yrs 
                        + dm_dx_type + has_high_cholesterol + has_high_blood_pressure + has_cancer_history
                        + anxiety_GAD2_cat + depression_PHQ2_cat
                        + BP_cat + AST_cat + ALT_cat + hba1c_cat + total_chol_cat + HDL_chol_cat + LDL_chol_cat + triglycerides_cat
                        + ada_dm_risk_cat + yad_2018_cat + statin_rec_2022, data = all_data, control = my_controls)
summary(table_one.a, 
        labelTranslations = my_labels2,
        title = "Table 1.a - Characteristics of participants who recieved results"
)

table_one.b <- tableby(consult_created_tf ~  time_since_report_released_hours + time_from_report_release_to_quiz_start_hours + time_from_report_release_to_consult_creation_hours
                       + time_from_kitorder_to_consult_creation_hours + time_from_report_release_to_report_open_hours + time_from_report_opened_to_consult_creation_hours + consult_outcome, data = all_data, control = my_controls)
summary(table_one.b, 
        labelTranslations = my_labels2,
        title = "Table 1.b - Characteristics of participants who recieved consult vs those who did not"
)





## table of characteristics by diabetes type 
table_two <- tableby(dm_dx_type ~ ada_dm_risk_cat + hba1c_cat + dm_hba1c_ctrl + any_dm_rx + actionable_DM, data = all_data, control = my_controls)
summary(table_two, 
        labelTranslations = my_labels2,
        title = "Table 2 - Characteristics of participants with Diabetes Dx"
)

cro(all_data$time_since_last_checkup)


chisq.test(all_data$last_checkup_one_yr, all_data$ada_dm_risk_cat)
chisq.test(all_data$last_checkup_one_yr, all_data$hba1c_tf)

cro(all_data$last_checkup_one_yr)

#####
##### STOPPED HERE
#####


## characterstics of those w/ any type of DM dx (preDM or DM)
table_two.b <- tableby(~ ada_dm_risk_cat + hba1c_cat + dm_hba1c_ctrl, subset = has_any_dm == TRUE, data = all_data, control = my_controls)
summary(table_two.b, 
        labelTranslations = my_labels2,
        title = "Table 2.b - Characteristics of participants with Diabetes dx by DM type"
)
cro(all_data$hba1c_cat, all_data$has_diabetes)



table_three <- tableby(has_high_blood_pressure ~ BP_cat + any_htn_rx + actionable_HTN, data = all_data, control = my_controls)
summary(table_three, 
        labelTranslations = my_labels2,
        title = "Table 3 - Characteristics of participants with HTN Dx"
)

table_three.b <- tableby(last_checkup_one_yr ~ BP_cat + any_htn_rx, subset = has_high_blood_pressure == FALSE, data = all_data, control = my_controls)
summary(table_three.b, 
        labelTranslations = my_labels2,
        title = "Table 3.b - Time since last check-up (w/in 1 yr) among participants with a prior HTN dx"
)

cro(all_data$has_high_blood_pressure)

table_three.c <- tableby(has_high_blood_pressure ~ BP_cat + any_htn_rx, subset = has_high_blood_pressure == TRUE, data = all_data, control = my_controls)
summary(table_three.c, 
        labelTranslations = my_labels2,
        title = "Table 3.c - At-home blood pressure readings and HTN medication use among those w/ a prior HTN dx"
)


table_four.a <- tableby(any_statin_rx ~ has_high_cholesterol + yad_2018_cat + statin_rec_2022, data = all_data, control = my_controls)
summary(table_four.a, 
        labelTranslations = my_labels2,
        title = "Table 4.a - Characteristics of participants who report taking a statin"
)

cro(all_data$should_start_statin.tf, all_data$any_statin_rx)

table_four <- tableby(has_high_cholesterol ~ should_start_statin.tf + any_statin_rx + ldl_goal, data = all_data, control = my_controls)
summary(table_four, 
        labelTranslations = my_labels2,
        title = "Table 4 - Characteristics of participants w/ high cholesterol dx"
)



table_four.b <- tableby(last_checkup_one_yr  ~ should_start_statin.tf, subset =has_high_cholesterol == FALSE, data = all_data, control = my_controls)
summary(table_four.b, 
        labelTranslations = my_labels2,
        title = "Table 4.b - Time since last check-up (w/in 1 yr) among participants with a prior high cholesterol dx"
)



table_four.c <- tableby(should_start_statin.tf ~ any_statin_rx + ldl_goal, subset = has_high_cholesterol == TRUE, data = all_data, control = my_controls)
summary(table_four.c, 
        labelTranslations = my_labels2,
        title =  "Table 4.c - Participants with high cholesterol dx who should be taking a statin"
)


table_four.d <- tableby(actionable_ldl ~ has_high_cholesterol + any_statin_rx + ldl_goal, data = all_data, control = my_controls)
summary(table_four.d, 
        labelTranslations = my_labels2,
        title = "Table 4.d - Participants with an actionable LDL result"
)

cro(all_data$actionable_HTN, all_data$any_htn_rx)

table_five <- tableby( ~ actionable_ldl + actionable_DM + actionable_HTN, data = all_data, control = my_controls)
summary(table_five, 
        labelTranslations = my_labels2,
        title = "Participants with actionable lab results by cardiometabolic area"
)



### NEW TABLES FOR CANCER RISK + SCREENING ###

table_six <- tableby(last_checkup_one_yr ~ compliant_mammogram_screening + compliant_CRC_screening + eligible_cervical_cancer_screening + eligible_cervical_cancer_screening 
                     + eligible_lung_cancer_screening, data = all_data, control = my_controls)
summary(table_six, 
        labelTranslations = my_labels2,
        title = "Table 6. Participant with actionable cancer screening recommendations" 
)

cro(all_data$depression_PHQ2_cat, all_data$anxiety_GAD2_cat)
cro(all_data$anxiety_and_depression_risk_score)

cro(all_data$age_at_kit_order, all_data$compliant_mammogram_screening)

#####
#####
#####

library(sjPlot)
library(sjmisc)
library(sjlabelled)

all_data_no_dm <- subset(all_data, all_data$has_any_dm == FALSE)
all_data_has_dm <- subset(all_data, all_data$has_any_dm == TRUE)

all_data_no_dm$last_checkup_one_yr <- factor(all_data_no_dm$last_checkup_one_yr, levels = c("FALSE", "TRUE"))
all_data_no_dm$ada_dm_risk_cat <- factor(all_data_no_dm$ada_dm_risk_cat, levels = c("Elevated Risk", "Normal Risk"))
all_data_no_dm$hba1c_tf <- factor(all_data_no_dm$hba1c_tf, levels = c("TRUE", "FALSE"))



#########################################################################################################
############################## ~~~ FREE TEXT RX ANALYSIS ~~~~ #############################################################
#########################################################################################################
##############################################################################################
# FREE TEXT RX
##########################################################################################
# Sentiment analysis w/ tidy text ~ https://www.tidytextmining.com/sentiment.html
## 1) Install textdata package to download sentiment lexicons
install.packages("textdata")
library(textdata)
library(tidyr)
library(tidytext) 

rx_tidy <- data.frame(all_data$kitorder_id, all_data$current_medication_list) 
colnames(rx_tidy) <- c('surveyid', 'sentence')
rx_tidy$sentence <- as.character(rx_tidy$sentence)

rx_tidy <- na.omit(rx_tidy)


rx_tidy_df <- rx_tidy %>%
  unnest_tokens(word, sentence)


# Remove stop words from our dataset with an anti_join()
rx_tidy_df_df1 <- rx_tidy_df %>%
  anti_join(stop_words, by = "word")
#######
rx_reviews <- data.frame(txt = rx_tidy$sentence,
                         stringsAsFactors = FALSE)

# split text corput based on word token
rx_reviews_1 <- rx_reviews %>% 
  unnest_tokens(output = word, input = txt) %>% 
  count(word, sort = TRUE) 


## rx stop words

rx_stop_words <- c("a", "and", "i", "my", "the", "to", "was", "were", "had", "this", "they",
                   "at", "for", "me", "so", "didn't", "in", "it", "but", "that", "would", "of", "is",
                   "once", "daily", "mg", "be", "taking", "twice", "years", "needed", "day", "1", "10", 
                   "10mg", "2", "20", "5", "tablet", "since", "20mg", "tablet", "as", "per", "mcg",
                   "3", "d", "1x", "months", "50", "been")
df_rx <- data.frame(rx_stop_words)

rx_reviews %>% 
  unnest_tokens(word, txt, token = "ngrams", n = 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% df_rx$rx_stop_words) %>%
  filter(!word2 %in% df_rx$rx_stop_words) %>%
  filter(word1 != 'NA') %>%
  unite(word,word1, word2, sep = " ") %>% 
  count(word, sort = TRUE) %>% 
  slice(1:15) %>% 
  ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "#211D4F") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Top Bigrams from Other Symptoms question",
       caption = "MDPH Antivirals Preliminary Analysis")

table_rx = rx_reviews_1 %>%
  count(word, sort = TRUE) %>%
  filter(word != 1 & word != 25 & word != "2x" & word != "5mg" & word != 5
         & word != 4 & word != 6 & word != 7 & word != 8 & word != 9 & word != 100
         & word != "d3" & word != "year" & word != "every" & word != "40mg" & word != "50mg" &
           word != 40 & word != "100mg" & word != "have")  %>%
  mutate(word = reorder(word, n))



### most common rx for ppl who are eligbile for HTN program 

rx__htn_tidy <- subset(all_data, all_data$eligbile_HTN_program == TRUE, select = c("kitorder_id", "current_medication_list")) 
colnames(rx__htn_tidy) <- c('surveyid', 'sentence')
rx__htn_tidy$sentence <- as.character(rx__htn_tidy$sentence)

rx__htn_tidy <- na.omit(rx__htn_tidy)


rx__htn_tidy_df <- rx__htn_tidy %>%
  unnest_tokens(word, sentence)

# Remove stop words from our dataset with an anti_join()
rx__htn_tidy_df1 <- rx__htn_tidy_df  %>%
  anti_join(stop_words, by = "word")

####### 
rx_reviews_htn <- data.frame(txt = rx__htn_tidy$sentence,
                             stringsAsFactors = FALSE)

rx_reviews_htn %>% 
  unnest_tokens(word, txt, token = "ngrams", n = 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% df_rx$rx_stop_words) %>%
  filter(!word2 %in% df_rx$rx_stop_words) %>%
  filter(word1 != 'NA') %>%
  unite(word,word1, word2, sep = " ") %>% 
  count(word, sort = TRUE) %>% 
  slice(1:15) %>% 
  ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "#211D4F") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Rx for those eligble for HTN program")


# split text corput based on word token
rx_reviews_htn_1 <- rx_reviews_htn %>% 
  unnest_tokens(output = word, input = txt) %>% 
  count(word, sort = TRUE) 

table_rx_htn = rx__htn_tidy_df1 %>%
  count(word, sort = TRUE) %>%
  filter(word != 1 & word != 25 & word != "2x" & word != "mg" & word != 5 & word != "daily" & word != "day"
         & word != "multi" & word != 50 & word != 20 & word != "1x" & word != "20mg"
         & word != 4 & word != 6 & word != 7 & word != 8 & word != 9 & word != 100
         & word != "d3" & word != "year" & word != "every" & word != "40mg" & word != "50mg" &
           word != 40 & word != "100mg" & word != "have")  %>%
  mutate(word = reorder(word, n))

cro(all_data$bp_rx, all_data$has_high_blood_pressure)




### most common rx for ppl who are eligbile for LDL program 
rx_ldl_tidy <- subset(all_data, all_data$eligbile_ldl_program == TRUE, select = c("kitorder_id", "current_medication_list")) 
colnames(rx_ldl_tidy ) <- c('surveyid', 'sentence')
rx_ldl_tidy$sentence <- as.character(rx_ldl_tidy$sentence)

rx_ldl_tidy <- na.omit(rx_ldl_tidy)


rx_ldl_tidy_df <- rx_ldl_tidy %>%
  unnest_tokens(word, sentence)

# Remove stop words from our dataset with an anti_join()
rx_ldl_tidy_df1 <- rx_ldl_tidy_df  %>%
  anti_join(stop_words, by = "word")

####### 
rx_reviews_ldl <- data.frame(txt = rx_ldl_tidy$sentence,
                             stringsAsFactors = FALSE)

rx_reviews_ldl %>% 
  unnest_tokens(word, txt, token = "ngrams", n = 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% df_rx$rx_stop_words) %>%
  filter(!word2 %in% df_rx$rx_stop_words) %>%
  filter(word1 != 'NA') %>%
  unite(word,word1, word2, sep = " ") %>% 
  count(word, sort = TRUE) %>% 
  slice(1:15) %>% 
  ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "#211D4F") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Rx for those eligble for LDL program")


# split text corput based on word token
rx_reviews_ldl_1 <- rx_reviews_ldl %>% 
  unnest_tokens(output = word, input = txt) %>% 
  count(word, sort = TRUE) 

table_rx_ldl = rx_ldl_tidy_df1 %>%
  count(word, sort = TRUE) %>%
  filter(word != 1 & word != 25 & word != "2x" & word != "mg" & word != 5 & word != "daily" & word != "day"
         & word != "multi" & word != 50 & word != 20 & word != "1x" & word != "20mg" & word != "10mg" & word != 10 & word != "5mg"
         & word != 4 & word != 6 & word != 7 & word != 8 & word != 9 & word != 100
         & word != "d3" & word != "year" & word != "every" & word != "40mg" & word != "50mg" &
           word != 40 & word != "100mg" & word != "have")  %>%
  mutate(word = reorder(word, n))

#########################################################################################################
############################## ~~~ END ~~~~ #############################################################
#########################################################################################################