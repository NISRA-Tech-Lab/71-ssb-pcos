library(here)
source(paste0(here(), "/code/config.R"))


# Check Trend folder exists
if (!dir.exists(paste0(data_folder, "Trend"))) {
  dir.create(paste0(data_folder, "Trend"))
}

# Check Trend folder 2021 exists
if (!dir.exists(paste0(data_folder, "Trend/2021"))) {
  dir.create(paste0(data_folder, "Trend/2021"))
}

ods_file_path <- paste0(data_folder, "Trend/PCOS Tables 2022 reduced.ODS")

# Awareness of NISRA ####

## Table 1: Awareness of NISRA by year ####

table_1_data <- read_ods(ods_file_path, sheet = "Awareness_of_NISRA", range = "A4:J8") %>%
  mutate(`2009 [Note 1]` = as.numeric(case_when(`2009 [Note 1]` == "No data" ~ NA,
                                                TRUE ~ `2009 [Note 1]`)),
         `2010 [Note 1]` = as.numeric(case_when(`2010 [Note 1]` == "No data" ~ NA,
                                                TRUE ~ `2010 [Note 1]`)))

saveRDS(table_1_data, paste0(data_folder, "Trend/2021/table_1_data.RDS"))

# Trust NISRA ####

## Table 23: Trust in NISRA by year ####

table_23_data <- read_ods(ods_file_path, sheet = "Trust_NISRA", range = "A4:F8")

saveRDS(table_23_data, paste0(data_folder, "Trend/2021/table_23_data.RDS"))

# Trust Civil Service ####

## Table 25: Trust in the Civil Service by year ####

table_25_data <- read_ods(ods_file_path, sheet = "Trust_Civil_Service", range = "A4:F8")

saveRDS(table_25_data, paste0(data_folder, "Trend/2021/table_25_data.RDS"))

## Table 26: Trust in the Northern Ireland Assembly by year ####

table_26_data <- read_ods(ods_file_path, sheet = "Trust_Civil_Service", range = "A10:F14")

saveRDS(table_26_data, paste0(data_folder, "Trend/2021/table_26_data.RDS"))

## Table 27: Trust in the Media by year ####

table_27_data <- read_ods(ods_file_path, sheet = "Trust_Civil_Service", range = "A17:F21")

saveRDS(table_27_data, paste0(data_folder, "Trend/2021/table_27_data.RDS"))

# Trust NISRA Statistics ####

## Table 28: Trust NISRA Statistics by year ####
 
table_28_data <- read_ods(ods_file_path, sheet = "Trust_NISRA_Statistics", range = "A4:F8")

saveRDS(table_28_data, paste0(data_folder, "Trend/2021/table_28_data.RDS"))

# Value ####

## Table 31: Statistics produced by NISRA are important to understand Northern Ireland by year ####

table_31_data <- read_ods(ods_file_path, sheet = "Value", range = "A4:E8")

saveRDS(table_31_data, paste0(data_folder, "Trend/2021/table_31_data.RDS"))

# Political Interference ####

## Table 34: Statistics produced by NISRA are free from political interference by year ####

table_34_data <- read_ods(ods_file_path, sheet = "Political_Interference", range = "A4:F8")

saveRDS(table_34_data, paste0(data_folder, "Trend/2021/table_34_data.RDS"))

# Confidence ####

## Table 36: Personal information provided to NISRA will be kept confidential by year ####

table_36_data <- read_ods(ods_file_path, sheet = "Confidentiality", range = "A4:F8")

saveRDS(table_36_data, paste0(data_folder, "Trend/2021/table_36_data.RDS"))
