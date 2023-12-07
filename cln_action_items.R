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

#check data
summary(raw_dat_action_items)

#get column names
colnames(raw_dat_action_items)

#get action item type names
cro(raw_dat_action_items$type)
cro(raw_dat_action_items$subtype)

# pivot the data to a wide format:
cln_ai <- subset(raw_dat_action_items, select = c("coloruser_id", "action_item_id", "condition_category", "type", "subtype", "created_at", "current_state_timestamp",
                                                     "_raw_status", "resolved_reason"))
  
# each individual gets a single row, each column corresponds to a question response or summary of their question responses
ai_pivot = cln_ai %>% select(-resolved_reason, -type, -created_at, -current_state_timestamp) %>%
  group_by(coloruser_id, action_item_id) %>%
  pivot_wider(names_from = "subtype", values_from = "_raw_status") %>%
  ungroup()

colnames(ai_pivot)

df_ai_genetics <- subset(ai_pivot, (ai_pivot$condition_category == "Genetic testing" | ai_pivot$condition_category == "Genetic discover"))
## delete NA rows
df_ai_genetics <- df_ai_genetics[, colSums(is.na(df_ai_genetics)) != nrow(df_ai_genetics)]
summary(df_ai_genetics)

cln_ai_genetics = df_ai_genetics %>% select(-action_item_id, -condition_category) %>%
  group_by(coloruser_id) %>%
  summarise_all(~na.omit(.)[1])



df_ai_CRC <- subset(ai_pivot, (ai_pivot$condition_category == "Colorectal cancer"))
## delete NA rows
df_ai_CRC <- df_ai_CRC[, colSums(is.na(df_ai_CRC)) != nrow(df_ai_CRC)]
summary(df_ai_CRC)

cln_ai_CRC = df_ai_CRC %>% select(-action_item_id, -condition_category) %>%
  group_by(coloruser_id) %>%
  summarise_all(~na.omit(.)[1])

summary(cln_ai_CRC)

df_ai_BREAST <- subset(ai_pivot, (ai_pivot$condition_category == "Breast cancer"))
## delete NA rows
df_ai_BREAST <- df_ai_BREAST[, colSums(is.na(df_ai_BREAST)) != nrow(df_ai_BREAST)]
summary(df_ai_BREAST)

cln_ai_BREAST = df_ai_BREAST %>% select(-action_item_id, -condition_category) %>%
  group_by(coloruser_id) %>%
  summarise_all(~na.omit(.)[1])

summary(cln_ai_BREAST)

df_ai_CERVICAL <- subset(ai_pivot, (ai_pivot$condition_category == "Cervical cancer"))
## delete NA rows
df_ai_CERVICAL <- df_ai_CERVICAL[, colSums(is.na(df_ai_CERVICAL)) != nrow(df_ai_CERVICAL)]
summary(df_ai_CERVICAL)

cln_ai_CERVICAL = df_ai_CERVICAL %>% select(-action_item_id, -condition_category) %>%
  group_by(coloruser_id) %>%
  summarise_all(~na.omit(.)[1])

summary(cln_ai_BREAST)


df_ai_LUNG <- subset(ai_pivot, (ai_pivot$condition_category == "Lung cancer"))
## delete NA rows
df_ai_LUNG <- df_ai_LUNG[, colSums(is.na(df_ai_LUNG)) != nrow(df_ai_LUNG)]
summary(df_ai_LUNG)
colnames(df_ai_LUNG)

cln_ai_LUNG = df_ai_LUNG %>% select(-action_item_id, -condition_category) %>%
  group_by(coloruser_id) %>%
  summarise_all(~na.omit(.)[1])
  
summary(cln_ai_LUNG)

df_ai_PROSTATE <- subset(ai_pivot, (ai_pivot$condition_category == "Prostate cancer"))
## delete NA rows
df_ai_PROSTATE <- df_ai_PROSTATE[, colSums(is.na(df_ai_PROSTATE)) != nrow(df_ai_PROSTATE)]
summary(df_ai_PROSTATE)

cln_ai_PROSTATE = df_ai_PROSTATE %>% select(-action_item_id, -condition_category) %>%
  group_by(coloruser_id) %>%
  summarise_all(~na.omit(.)[1])

summary(cln_ai_PROSTATE)





