#
# 
#             WBA SOCIAL BENCHMARK DATA SCORING & ANALYSIS
#             
#                     
#                         

# FIRST INSTRUCTION: Run line 10-136
## Loading necessary R packages for data cleaning and importing csv
# install.packages("tidyverse") #run this code if you don't have the package installed
# install.packages("readxl") #run this code if you don't have the package installed
# install.packages("readr") #run this code if you don't have the package installed
# install.packages("devtools") #run this code if you don't have the package installed
#  devtools::install_github("bergant/airtabler")

library(tidyverse)
library(readxl)
library(readr)
library(writexl)
library(openxlsx)
library(airtabler)

## Set a folder path name, change accordingly to your PC folder path name.
#user <- "C:/Users/lprat/OneDrive - World Benchmarking Alliance/Social Benchmark 2024/" 
#raw <- paste0(user, "Data analysis/Raw/")
#output <- paste0(user, "Data analysis/Output/")

## Retrieve SDG2000 Data and all CSI data from Airtable using API
Sys.setenv("AIRTABLE_API_KEY" = "patSeFJxXRTCkCJb9.175246b8d869e81719918b35023682bf794b94be2b04eecac6d0e1fe3f82746e")


#Social Benchmark database
Social_base <- airtable(
  base = "appNpVtwIMT8dpfVb", #API for Social Benchmark Base 
  tables = "tblkAOzLx3NDneD5N") #API for Social Benchmark Assessment Table
SocialBM_data <- Social_base$`tblkAOzLx3NDneD5N`$select_all() %>%
  select(`assessment-ID`, `friendly-ID`, `company-name`, industry, `hq-country`, `hq-region`, `assessment-year`, `is-latest`, score, `external-comment`, evidence, source, `page-number`, link, bec) %>%
  separate(`assessment-ID`, into = c("WBA_ID", "GEC"), sep = "-") 

# Keep only the latest
SocialBM_data <- SocialBM_data %>% 
  filter(`is-latest` == "Yes" & !is.na(WBA_ID)) 

# Remove the older version of CSIs from 744 companies

# Import CSIs 2024 from 744 companies

# Append them together to create a new dataset of 2,000

# Remove incomplete CSI assessment from Gender
#checking number of assessment per company/assessment year (should be 46)
element_count <- SocialBM_data %>%
  group_by(WBA_ID) %>%
  summarize(
    Count = n(),
    across(c(`company-name`), first)
  ) %>%
  filter(Count != 46)  #Clean Harbors-PT_00437 (never used, should be excluded.

# Save the data frame
saveRDS(SocialBM_data, "All_SocialBM_18Sep.RDS")

########################################################################################################################
#     SCORING script - CSI old way of scoring
#     0 if NONE of element METs, 0.5 if SOME of elements METs, 1 if ALL element METs
#     All get equal weight (1) except CSI 04 - Assessing human rights risks and impacts & CSI 05 - Integrating and acting on human rights risks and impacts (both get 2)


## Generate count met, unmet, partially met, and total

# Generate indicator code and add element score
SocialBM_data <- SocialBM_data %>%
  mutate(CSI_indicator = substr(bec, 1, 6)) %>% 
  mutate(element_score = 0,
         element_score = ifelse(score == "Met", 1, element_score),
         element_score = ifelse(score == "Not Applicable", NA, element_score),
         element_score = ifelse(score == "Partially Met", 0.5, element_score)
         )

SocialBM_element_score <- SocialBM_data %>% 
  # Transpose element level to wide format
  pivot_wider(id_cols = c(WBA_ID, `company-name`, industry, `hq-country`, `hq-region`, `assessment-year`), names_from = bec, values_from = element_score)

SocialBM_element_score_text <- SocialBM_data %>% 
  # Transpose element level to wide format
  pivot_wider(id_cols = c(WBA_ID, `company-name`, industry, `hq-country`, `hq-region`, `assessment-year`), names_from = bec, values_from = score)


# Generate count of each score
CSI_scoring <- SocialBM_data %>%
  group_by(WBA_ID, CSI_indicator) %>%
  summarise(count_met = sum(score == "Met", na.rm = TRUE),
            count_unmet = sum(score == "Unmet", na.rm = TRUE),
            count_na = sum(score == "Not Applicable", na.rm = TRUE),
            count_partial = sum(score == "Partially Met", na.rm = TRUE),
            count_total = n())

# Double check if the count match
CSI_scoring_check <- CSI_scoring %>%
  mutate(count_total2 = count_met + count_unmet + count_na + count_partial) %>% 
  filter(count_total != count_total2) #0 obs, good.

# Generate RAW score by count of METs at indicator level
CSI_scoring <- CSI_scoring %>%
  mutate(raw_score = 0,
         raw_score = if_else(count_met > 0 & count_met < count_total, 0.5, raw_score),
         raw_score = if_else(count_partial > 0 & count_partial < count_total, 0.5, raw_score),
         raw_score = if_else(count_partial > 0 & count_partial == count_total, 0.5, raw_score),
         raw_score = if_else(count_met == count_total, 1, raw_score),
         raw_score = if_else(count_met == 1 & CSI_indicator == "CSI.04", 1, raw_score),
         raw_score = if_else(count_met == 1 & CSI_indicator == "CSI.05", 1, raw_score)) 

# Generate weighted score at indicator level
CSI_scoring <- CSI_scoring %>% 
  mutate(weight = 1,
         weight = if_else(CSI_indicator == "CSI.04", 2, weight),
         weight = if_else(CSI_indicator == "CSI.05", 2, weight)) %>% 
  mutate(weighted_score = raw_score * weight)

# Transpose the indicator score
CSI_scoring_fmt <- CSI_scoring %>%
  pivot_wider(id_cols = c(WBA_ID), names_from = CSI_indicator, values_from = weighted_score)

# Generate score by company
CSI_score_company <- CSI_scoring %>%
  group_by(WBA_ID) %>%
  summarise(CSIscore = sum(weighted_score))

# Now combine all together
SocialBM_score_ALL <- SocialBM_element_score_text %>% 
  left_join(CSI_score_company, by = c(WBA_ID = "WBA_ID")) %>% 
  left_join(CSI_scoring_fmt, by = c(WBA_ID = "WBA_ID")) %>% 
  mutate(MA_1 = `CSI.01` + `CSI.02` + `CSI.03` + `CSI.04` + `CSI.05` + `CSI.06` + `CSI.07` + `CSI.08`,
         MA_2 = `CSI.09` + `CSI.10` + `CSI.11` + `CSI.12` + `CSI.13` + `CSI.14`,
         MA_3 = `CSI.15` + `CSI.16` + `CSI.17` + `CSI.18`)
# Note! We don't have to calculate score at MA level.

SocialBM_score_ALL <- SocialBM_score_ALL %>% 
  mutate(`hq-country` = unlist(`hq-country`),
         `company-name` = unlist(`company-name`),
         #industry = unlist(industry),
         `hq-region` = unlist(`hq-region`)) 

# Save the data frame
saveRDS(SocialBM_score_ALL, "SocialBM_score_ALL_18Sep.RDS")

SocialBM_score_ALL <-  SocialBM_score_ALL %>% 
  select(WBA_ID, `company-name`, `industry`, `hq-country`, `hq-region`, `assessment-year`, CSIscore, `CSI.01`, `CSI.01.EA`, `CSI.02`, `CSI.02.EA`, `CSI.02.EB`, `CSI.03`, `CSI.03.EA`, `CSI.03.EB`, `CSI.04`, `CSI.04.EA`, `CSI.04.EB`, `CSI.05`, `CSI.05.EA`, `CSI.05.EB`, `CSI.06`, `CSI.06.EA`, `CSI.06.EB`, `CSI.07`, `CSI.07.EA`, `CSI.08`, `CSI.08.EA`, `CSI.09`, `CSI.09.EA`, `CSI.09.EB`, `CSI.09.EC`, `CSI.09.ED`, `CSI.10`, `CSI.10.EA`, `CSI.10.EB`, `CSI.10.EC`, `CSI.11`, `CSI.11.EA`, `CSI.11.EB`, `CSI.11.EC`, `CSI.12`, `CSI.12.EA`, `CSI.12.EB`, `CSI.13`, `CSI.13.EA`, `CSI.13.EB`, `CSI.13.EC`, `CSI.13.ED`, `CSI.14`, `CSI.14.EA`, `CSI.14.EB`, `CSI.14.EC`, `CSI.14.ED`, `CSI.15`, `CSI.15.EA`, `CSI.15.EB`, `CSI.16`,  `CSI.16.EA`, `CSI.16.EB`, `CSI.16.EC`, `CSI.17`,  `CSI.17.EA`, `CSI.17.EB`, `CSI.17.EC`, `CSI.17.ED`, `CSI.18`, `CSI.18.EA`, `CSI.18.EB`, `CSI.18.EC`, `CSI.18.ED`) %>% 
  arrange(WBA_ID)

write_xlsx(SocialBM_score_ALL, "C:/Users/lprat/OneDrive - World Benchmarking Alliance/Social Benchmark 2024/Data analysis/Script/SocialBM_score_forexternaldataset_18Sep2024.xlsx")


