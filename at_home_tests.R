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

##download at-home test/lab order data
summary(raw_dat_tests)



## dataframe by test type UNIQUE only
#df_users_tests <- subset(raw_dat_tests, select = c("coloruser_id"))
df_users_tests <- distinct(raw_dat_tests, coloruser_id)


## H30 table and rename columns 
db_h30_tests <- subset(raw_dat_tests, raw_dat_tests$Test_Orders_Phi__test_type_detailed == "hereditary 30")
summary(db_h30_tests)

c <- unique(db_h30_tests$coloruser_id)

count_h30_kit <- db_h30_tests %>%
  group_by(coloruser_id) %>% 
  summarise(H30_kit_count = n())
            
unique_h30_tests <- db_h30_tests %>%
  group_by(coloruser_id) %>% 
  slice_max(Test_Orders_Phi__test_requested_at)
         
unique_h30_tests <- left_join(unique_h30_tests, count_h30_kit, by = "coloruser_id")


df_h30 <- subset(unique_h30_tests, select = c("coloruser_id", 
                                          "Test_Orders_Phi__test_subtype", 
                                          "Test_Orders_Phi__kitorder_id",
                                          "Test_Orders_Phi__is_refulfilled",
                                          "Test_Orders_Phi__test_order_status", 
                                          "Test_Orders_Phi__lab_order_consult_status",
                                          "Test_Orders_Phi__result_interpretation", 
                                          "Test_Orders_Phi__has_report_opened_by_participant", 
                                          "Test_Orders_Phi__test_order_lifecycle_days",
                                          "H30_kit_count"))
summary(df_h30)

db_h30_tests  <- db_h30_tests %>%
  rename( "H30_kitorder_id" = "Test_Orders_Phi__kitorder_id",
          "H30_test_order_status" = "Test_Orders_Phi__test_order_status",
          "has_H30_test" = "Test_Orders_Phi__test_subtype",
          "H30_lab_order_consult_status" = "Test_Orders_Phi__lab_order_consult_status",
          "H30_result_interpretation" = "Test_Orders_Phi__result_interpretation",
          "H30_has_report_opened_by_participant" = "Test_Orders_Phi__has_report_opened_by_participant",
          "H30_test_order_lifecycle_days" = "Test_Orders_Phi__test_order_lifecycle_days",
          "H30_tat_test_requested_to_approved_day" =  "Test_Orders_Phi__tat_test_requested_to_approved_days",
          "H30_tat_test_requested_to_activated_days" = "Test_Orders_Phi__tat_test_requested_to_activated_days",
          "H30_tat_test_requested_to_accessioned_days" =  "Test_Orders_Phi__tat_test_requested_to_accessioned_days",
          "H30_tat_test_accessioned_to_report_rel" = "Test_Orders_Phi__tat_test_accessioned_to_report_rel_953b2a48"
  )

df_h30  <- df_h30 %>%
  rename( "H30_kitorder_id" = "Test_Orders_Phi__kitorder_id",
          "H30_test_order_status" = "Test_Orders_Phi__test_order_status",
          "has_H30_test" = "Test_Orders_Phi__test_subtype",
          "H30_lab_order_consult_status" = "Test_Orders_Phi__lab_order_consult_status",
          "H30_result_interpretation" = "Test_Orders_Phi__result_interpretation",
          "H30_has_report_opened_by_participant" = "Test_Orders_Phi__has_report_opened_by_participant",
          "H30_test_order_lifecycle_days" = "Test_Orders_Phi__test_order_lifecycle_days"
  )

## C30 table and rename columns 
db_c30_tests <- subset(raw_dat_tests, raw_dat_tests$Test_Orders_Phi__test_type_detailed == "cardio 30")

d <- unique(db_c30_tests$coloruser_id)

count_c30_kit <- db_c30_tests %>%
  group_by(coloruser_id) %>% 
  summarise(C30_kit_count = n())

unique_c30_tests <- db_c30_tests %>%
  group_by(coloruser_id) %>% 
  slice_max(Test_Orders_Phi__test_requested_at)

unique_c30_tests <- left_join(unique_c30_tests, count_c30_kit, by = "coloruser_id")


df_c30 <- subset(unique_c30_tests, select = c("coloruser_id", 
                                          "Test_Orders_Phi__test_subtype", 
                                          "Test_Orders_Phi__kitorder_id", 
                                          "Test_Orders_Phi__is_refulfilled",
                                          "Test_Orders_Phi__test_order_status", 
                                          "Test_Orders_Phi__lab_order_consult_status",
                                          "Test_Orders_Phi__result_interpretation", 
                                          "Test_Orders_Phi__has_report_opened_by_participant", 
                                          "Test_Orders_Phi__test_order_lifecycle_days",
                                          "C30_kit_count"))

db_c30_tests  <- db_c30_tests %>%
  rename( "C30_kitorder_id" = "Test_Orders_Phi__kitorder_id",
          "C30_test_order_status" = "Test_Orders_Phi__test_order_status",
          "C30_test_order_lifecycle_days" = "Test_Orders_Phi__test_order_lifecycle_days",
          "C30_tat_test_requested_to_approved_day" =  "Test_Orders_Phi__tat_test_requested_to_approved_days",
          "C30_tat_test_requested_to_activated_days" = "Test_Orders_Phi__tat_test_requested_to_activated_days",
          "C30_tat_test_requested_to_accessioned_days" =  "Test_Orders_Phi__tat_test_requested_to_accessioned_days",
          "C30_tat_test_accessioned_to_report_rel" = "Test_Orders_Phi__tat_test_accessioned_to_report_rel_953b2a48",
          "has_C30_test" = "Test_Orders_Phi__test_subtype",
          "C30_lab_order_consult_status" = "Test_Orders_Phi__lab_order_consult_status",
          "C30_result_interpretation" = "Test_Orders_Phi__result_interpretation",
          "C30_has_report_opened_by_participant" = "Test_Orders_Phi__has_report_opened_by_participant"
  )

df_c30  <- df_c30 %>%
  rename( "C30_kitorder_id" = "Test_Orders_Phi__kitorder_id",
          "C30_test_order_status" = "Test_Orders_Phi__test_order_status",
          "has_C30_test" = "Test_Orders_Phi__test_subtype",
          "C30_lab_order_consult_status" = "Test_Orders_Phi__lab_order_consult_status",
          "C30_result_interpretation" = "Test_Orders_Phi__result_interpretation",
          "C30_has_report_opened_by_participant" = "Test_Orders_Phi__has_report_opened_by_participant",
          "C30_test_order_lifecycle_days" = "Test_Orders_Phi__test_order_lifecycle_days"
  )

### PGX table and rename columns 
db_pgx_tests <- subset(raw_dat_tests, raw_dat_tests$Test_Orders_Phi__test_type_detailed == "pgx v1")

e <- unique(db_pgx_tests$coloruser_id)

count_pgx_kit <- db_pgx_tests %>%
  group_by(coloruser_id) %>% 
  summarise(PGx_kit_count = n())

unique_pgx_tests <- db_pgx_tests %>%
  group_by(coloruser_id) %>% 
  slice_max(Test_Orders_Phi__test_requested_at)

unique_pgx_tests <- left_join(unique_pgx_tests, count_pgx_kit, by = "coloruser_id")

df_pgx <- subset(unique_pgx_tests, select = c("coloruser_id", 
                                              "Test_Orders_Phi__test_subtype", 
                                              "Test_Orders_Phi__kitorder_id", 
                                              "Test_Orders_Phi__is_refulfilled",
                                              "Test_Orders_Phi__test_order_status", 
                                              "Test_Orders_Phi__lab_order_consult_status",
                                              "Test_Orders_Phi__result_interpretation", 
                                              "Test_Orders_Phi__has_report_opened_by_participant", 
                                              "Test_Orders_Phi__test_order_lifecycle_days",
                                              "PGx_kit_count"))

db_pgx_tests  <- db_pgx_tests %>%
  rename( "PGx_kitorder_id" = "Test_Orders_Phi__kitorder_id",
          "PGx_test_order_status" = "Test_Orders_Phi__test_order_status",
          "PGx_test_order_lifecycle_days" = "Test_Orders_Phi__test_order_lifecycle_days",
          "PGx_tat_test_requested_to_approved_day" =  "Test_Orders_Phi__tat_test_requested_to_approved_days",
          "PGx_tat_test_requested_to_activated_days" = "Test_Orders_Phi__tat_test_requested_to_activated_days",
          "PGx_tat_test_requested_to_accessioned_days" =  "Test_Orders_Phi__tat_test_requested_to_accessioned_days",
          "PGx_tat_test_accessioned_to_report_rel" = "Test_Orders_Phi__tat_test_accessioned_to_report_rel_953b2a48",
          "has_PGx_test" = "Test_Orders_Phi__test_subtype",
          "PGx_lab_order_consult_status" = "Test_Orders_Phi__lab_order_consult_status",
          "PGx_result_interpretation" = "Test_Orders_Phi__result_interpretation",
          "PGx_has_report_opened_by_participant" = "Test_Orders_Phi__has_report_opened_by_participant"
  )

df_pgx  <- df_pgx %>%
  rename( "PGx_kitorder_id" = "Test_Orders_Phi__kitorder_id",
          "PGx_test_order_status" = "Test_Orders_Phi__test_order_status",
          "has_PGx_test" = "Test_Orders_Phi__test_subtype",
          "PGx_lab_order_consult_status" = "Test_Orders_Phi__lab_order_consult_status",
          "PGx_result_interpretation" = "Test_Orders_Phi__result_interpretation",
          "PGx_has_report_opened_by_participant" = "Test_Orders_Phi__has_report_opened_by_participant",
          "PGx_test_order_lifecycle_days" = "Test_Orders_Phi__test_order_lifecycle_days"
  )

## FIT table and rename columns 
db_FIT_tests <- subset(raw_dat_tests, raw_dat_tests$Test_Orders_Phi__test_type_detailed == "fit")

f <- unique(db_FIT_tests$coloruser_id)

count_FIT_kit <- db_FIT_tests %>%
  group_by(coloruser_id) %>% 
  summarise(FIT_kit_count = n())

unique_FIT_tests <- db_FIT_tests %>%
  group_by(coloruser_id) %>% 
  slice_max(Test_Orders_Phi__test_requested_at)

unique_FIT_tests <- left_join(unique_FIT_tests, count_FIT_kit, by = "coloruser_id")

df_FIT <- subset(unique_FIT_tests, select = c("coloruser_id", 
                                              "Test_Orders_Phi__test_subtype", 
                                              "Test_Orders_Phi__kitorder_id", 
                                              "Test_Orders_Phi__is_refulfilled",
                                              "Test_Orders_Phi__test_order_status", 
                                              "Test_Orders_Phi__lab_order_consult_status",
                                              "Test_Orders_Phi__result_interpretation", 
                                              "Test_Orders_Phi__has_report_opened_by_participant", 
                                              "Test_Orders_Phi__test_order_lifecycle_days",
                                              "FIT_kit_count"))

db_FIT_tests  <- db_FIT_tests %>%
  rename( "FIT_kitorder_id" = "Test_Orders_Phi__kitorder_id",
          "FIT_test_order_status" = "Test_Orders_Phi__test_order_status",
          "FIT_test_order_lifecycle_days" = "Test_Orders_Phi__test_order_lifecycle_days",
          "FIT_tat_test_requested_to_approved_day" =  "Test_Orders_Phi__tat_test_requested_to_approved_days",
          "FIT_tat_test_requested_to_activated_days" = "Test_Orders_Phi__tat_test_requested_to_activated_days",
          "FIT_tat_test_requested_to_accessioned_days" =  "Test_Orders_Phi__tat_test_requested_to_accessioned_days",
          "FIT_tat_test_accessioned_to_report_rel" = "Test_Orders_Phi__tat_test_accessioned_to_report_rel_953b2a48",
          "has_FIT_test" = "Test_Orders_Phi__test_subtype",
          "FIT_lab_order_consult_status" = "Test_Orders_Phi__lab_order_consult_status",
          "FIT_result_interpretation" = "Test_Orders_Phi__result_interpretation",
          "FIT_has_report_opened_by_participant" = "Test_Orders_Phi__has_report_opened_by_participant"
  )

df_FIT  <- df_FIT %>%
  rename( "FIT_kitorder_id" = "Test_Orders_Phi__kitorder_id",
          "FIT_test_order_status" = "Test_Orders_Phi__test_order_status",
          "has_FIT_test" = "Test_Orders_Phi__test_subtype",
          "FIT_lab_order_consult_status" = "Test_Orders_Phi__lab_order_consult_status",
          "FIT_result_interpretation" = "Test_Orders_Phi__result_interpretation",
          "FIT_has_report_opened_by_participant" = "Test_Orders_Phi__has_report_opened_by_participant",
          "FIT_test_order_lifecycle_days" = "Test_Orders_Phi__test_order_lifecycle_days"
  )


## HPV table and rename columns 
db_HPV_tests <- subset(raw_dat_tests, raw_dat_tests$Test_Orders_Phi__test_type_detailed == "hpv - first void urine")


g <- unique(db_HPV_tests$coloruser_id)

count_HPV_kit <- db_HPV_tests %>%
  group_by(coloruser_id) %>% 
  summarise(HPV_kit_count = n())

unique_HPV_tests <- db_HPV_tests %>%
  group_by(coloruser_id) %>% 
  slice_max(Test_Orders_Phi__test_requested_at)

unique_HPV_tests <- left_join(unique_HPV_tests, count_HPV_kit, by = "coloruser_id")

df_HPV <- subset(unique_HPV_tests, select = c("coloruser_id", 
                                              "Test_Orders_Phi__test_subtype", 
                                              "Test_Orders_Phi__kitorder_id", 
                                              "Test_Orders_Phi__is_refulfilled",
                                              "Test_Orders_Phi__test_order_status", 
                                              "Test_Orders_Phi__lab_order_consult_status",
                                              "Test_Orders_Phi__result_interpretation", 
                                              "Test_Orders_Phi__has_report_opened_by_participant", 
                                              "Test_Orders_Phi__test_order_lifecycle_days",
                                              "HPV_kit_count"))

db_HPV_tests  <- db_HPV_tests %>%
  rename( "HPV_kitorder_id" = "Test_Orders_Phi__kitorder_id",
          "HPV_test_order_status" = "Test_Orders_Phi__test_order_status",
          "HPV_test_order_lifecycle_days" = "Test_Orders_Phi__test_order_lifecycle_days",
          "HPV_tat_test_requested_to_approved_day" =  "Test_Orders_Phi__tat_test_requested_to_approved_days",
          "HPV_tat_test_requested_to_activated_days" = "Test_Orders_Phi__tat_test_requested_to_activated_days",
          "HPV_tat_test_requested_to_accessioned_days" =  "Test_Orders_Phi__tat_test_requested_to_accessioned_days",
          "HPV_tat_test_accessioned_to_report_rel" = "Test_Orders_Phi__tat_test_accessioned_to_report_rel_953b2a48",
          "has_HPV_test" = "Test_Orders_Phi__test_subtype",
          "HPV_lab_order_consult_status" = "Test_Orders_Phi__lab_order_consult_status",
          "HPV_result_interpretation" = "Test_Orders_Phi__result_interpretation",
          "HPV_has_report_opened_by_participant" = "Test_Orders_Phi__has_report_opened_by_participant"
  )

df_HPV  <- df_HPV %>%
  rename( "HPV_kitorder_id" = "Test_Orders_Phi__kitorder_id",
          "HPV_test_order_status" = "Test_Orders_Phi__test_order_status",
          "has_HPV_test" = "Test_Orders_Phi__test_subtype",
          "HPV_lab_order_consult_status" = "Test_Orders_Phi__lab_order_consult_status",
          "HPV_result_interpretation" = "Test_Orders_Phi__result_interpretation",
          "HPV_has_report_opened_by_participant" = "Test_Orders_Phi__has_report_opened_by_participant",
          "HPV_test_order_lifecycle_days" = "Test_Orders_Phi__test_order_lifecycle_days"
  )

## PSA table and rename columns 
db_PSA_tests <- subset(raw_dat_tests, raw_dat_tests$Test_Orders_Phi__test_type_detailed == "psa - dried blood")

h <- unique(db_PSA_tests$coloruser_id)

count_PSA_kit <- db_PSA_tests %>%
  group_by(coloruser_id) %>% 
  summarise(PSA_kit_count = n())

unique_PSA_tests <- db_PSA_tests %>%
  group_by(coloruser_id) %>% 
  slice_max(Test_Orders_Phi__test_requested_at)

unique_PSA_tests <- left_join(unique_PSA_tests, count_PSA_kit, by = "coloruser_id")

df_PSA <- subset(unique_PSA_tests, select = c("coloruser_id", 
                                              "Test_Orders_Phi__test_subtype", 
                                              "Test_Orders_Phi__kitorder_id", 
                                              "Test_Orders_Phi__is_refulfilled",
                                              "Test_Orders_Phi__test_order_status", 
                                              "Test_Orders_Phi__lab_order_consult_status",
                                              "Test_Orders_Phi__result_interpretation", 
                                              "Test_Orders_Phi__has_report_opened_by_participant", 
                                              "Test_Orders_Phi__test_order_lifecycle_days",
                                              "PSA_kit_count"))


db_PSA_tests  <- db_PSA_tests %>%
  rename( "PSA_kitorder_id" = "Test_Orders_Phi__kitorder_id",
          "PSA_test_order_status" = "Test_Orders_Phi__test_order_status",
          "PSA_test_order_lifecycle_days" = "Test_Orders_Phi__test_order_lifecycle_days",
          "PSA_tat_test_requested_to_approved_day" =  "Test_Orders_Phi__tat_test_requested_to_approved_days",
          "PSA_tat_test_requested_to_activated_days" = "Test_Orders_Phi__tat_test_requested_to_activated_days",
          "PSA_tat_test_requested_to_accessioned_days" =  "Test_Orders_Phi__tat_test_requested_to_accessioned_days",
          "PSA_tat_test_accessioned_to_report_rel" = "Test_Orders_Phi__tat_test_accessioned_to_report_rel_953b2a48",
          "has_PSA_test" = "Test_Orders_Phi__test_subtype",
          "PSA_lab_order_consult_status" = "Test_Orders_Phi__lab_order_consult_status",
          "PSA_result_interpretation" = "Test_Orders_Phi__result_interpretation",
          "PSA_has_report_opened_by_participant" = "Test_Orders_Phi__has_report_opened_by_participant"
  )

df_PSA <- df_PSA %>%
  rename( "PSA_kitorder_id" = "Test_Orders_Phi__kitorder_id",
          "PSA_test_order_status" = "Test_Orders_Phi__test_order_status",
          "has_PSA_test" = "Test_Orders_Phi__test_subtype",
          "PSA_lab_order_consult_status" = "Test_Orders_Phi__lab_order_consult_status",
          "PSA_result_interpretation" = "Test_Orders_Phi__result_interpretation",
          "PSA_has_report_opened_by_participant" = "Test_Orders_Phi__has_report_opened_by_participant",
          "PSA_test_order_lifecycle_days" = "Test_Orders_Phi__test_order_lifecycle_days"
  )

merged_all_test <- left_join(df_users_tests, df_h30, by = "coloruser_id")

merged_all_test <- left_join(merged_all_test, df_c30, by = "coloruser_id")

merged_all_test <- left_join(merged_all_test, df_pgx, by = "coloruser_id")

merged_all_test <- left_join(merged_all_test, df_FIT, by = "coloruser_id")

merged_all_test <- left_join(merged_all_test, df_HPV, by = "coloruser_id")

merged_all_test <- left_join(merged_all_test, df_PSA, by = "coloruser_id")

##clean up test names to T/F for count
merged_all_test <- merged_all_test %>% 
  mutate(has_H30_test = case_when(
    has_H30_test == "hereditary 30" ~ TRUE,
    is.na(has_H30_test) ~ FALSE
  ))

merged_all_test <- merged_all_test %>% 
  mutate(has_C30_test = case_when(
    has_C30_test == "cardio 30" ~ TRUE,
    is.na(has_C30_test) ~ FALSE
  ))

merged_all_test <- merged_all_test %>% 
  mutate(has_PGx_test = case_when(
    has_PGx_test == "pgx v1" ~ TRUE,
    is.na(has_PGx_test) ~ FALSE
  ))

merged_all_test <- merged_all_test %>% 
  mutate(has_FIT_test = case_when(
    has_FIT_test == "fit" ~ TRUE,
    is.na(has_FIT_test) ~ FALSE
  ))

merged_all_test <- merged_all_test %>% 
  mutate(has_HPV_test = case_when(
    has_HPV_test == "hpv - first void urine" ~ TRUE,
    is.na(has_HPV_test) ~ FALSE
  ))

merged_all_test <- merged_all_test %>% 
  mutate(has_PSA_test = case_when(
    has_PSA_test == "psa - dried blood" ~ TRUE,
    is.na(has_PSA_test) ~ FALSE
  ))


colnames(merged_all_test)

cln_all_tests <- subset(merged_all_test, select = c("coloruser_id", "has_H30_test", "H30_test_order_status", "H30_result_interpretation",
                                                    "H30_kit_count", "has_C30_test", "C30_test_order_status", "C30_result_interpretation",
                                                    "C30_kit_count", "has_PGx_test", "PGx_test_order_status", "PGx_result_interpretation",
                                                    "PGx_kit_count", "has_FIT_test", "FIT_test_order_status", "FIT_result_interpretation",
                                                    "FIT_kit_count", "has_HPV_test", "HPV_test_order_status", "HPV_result_interpretation",
                                                    "HPV_kit_count", "has_PSA_test", "PSA_test_order_status", "PSA_result_interpretation",
                                                    "PSA_kit_count"))
                                              
 