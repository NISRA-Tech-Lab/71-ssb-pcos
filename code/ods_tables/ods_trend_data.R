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

correct_2016_data <- paste0(data_folder, "Trend/Correct 2016 data.XLSX")

# Awareness of NISRA ####

## Table 1a: Awareness of NISRA by year ####

table_1a_data <- read_ods(ods_file_path, sheet = "Awareness_of_NISRA", range = "A4:J8") %>%
  mutate(
    `2009 [Note 1]` = as.numeric(case_when(
      `2009 [Note 1]` == "No data" ~ NA,
      TRUE ~ `2009 [Note 1]`
    )),
    `2010 [Note 1]` = as.numeric(case_when(
      `2010 [Note 1]` == "No data" ~ NA,
      TRUE ~ `2010 [Note 1]`
    ))
  )

saveRDS(table_1a_data, paste0(data_folder, "Trend/2021/table_1a_data.RDS"))

## Table 1c: Awareness of ONS by year ####

table_1c_data <- data.frame(Response = c("Yes", "No", "Don't Know", "Number of Respondents")) %>%
  mutate(
    `2014` = c(71, 27.4956998577179, 1.33128506479173, 1907),
    `2016` = c(71.2539450324221, 27.2537935894976, 1.49226137808037, 1966),
    `2018` = c(69, 29, 1, 1965)
  )

saveRDS(table_1c_data, paste0(data_folder, "Trend/2021/table_1c_data.RDS"))

# Trust NISRA ####

## Table 3.1a: Trust in NISRA by year ####

table_3.1a_data <- readxl::read_xlsx(correct_2016_data, sheet = "2016", range = "A4:F8")

saveRDS(table_3.1a_data, paste0(data_folder, "Trend/2021/table_3.1a_data.RDS"))

# Trust Civil Service ####

## Table 3.2a: Trust in the Civil Service by year ####

table_3.2a_data <- readxl::read_xlsx(correct_2016_data, sheet = "2016", range = "A11:F15")

saveRDS(table_3.2a_data, paste0(data_folder, "Trend/2021/table_3.2a_data.RDS"))

## Table 3.3a: Trust in the Northern Ireland Assembly by year ####

table_3.3a_data <- readxl::read_xlsx(correct_2016_data, sheet = "2016", range = "A18:F22")

names(table_3.3a_data) <- gsub("Note 5", "Note 2", names(table_3.3a_data))

saveRDS(table_3.3a_data, paste0(data_folder, "Trend/2021/table_3.3a_data.RDS"))

## Table 3.4a: Trust in the Media by year ####

table_3.4a_data <- readxl::read_xlsx(correct_2016_data, sheet = "2016", range = "A26:F30")

saveRDS(table_3.4a_data, paste0(data_folder, "Trend/2021/table_3.4a_data.RDS"))

# Trust NISRA Statistics ####

## Table 4a: Trust NISRA Statistics by year ####

table_4a_data <- readxl::read_xlsx(correct_2016_data, sheet = "2016", range = "A33:F37")

saveRDS(table_4a_data, paste0(data_folder, "Trend/2021/table_4a_data.RDS"))

## Table 4d: Trust ONS Statistics by year ####

table_4d_data <- data.frame(Response = c("Yes", "No", "Don't Know", "Number of Respondents")) %>%
  mutate(
    `2014` = c(66.40047, 15.74657, 17.85295, 1906),
    `2016` = c(69.54472, 12.5457, 17.90959, 1966),
    `2018` = c(69.0196674316804, 12.0987869494368, 18.8815456188828, 1965)
  )

saveRDS(table_4d_data, paste0(data_folder, "Trend/2021/table_4d_data.RDS"))

# Value ####

## Table 5a: Statistics produced by NISRA are important to understand Northern Ireland by year ####

table_5a_data <- readxl::read_xlsx(correct_2016_data, sheet = "2016", range = "A40:E44")

saveRDS(table_5a_data, paste0(data_folder, "Trend/2021/table_5a_data.RDS"))

# Political Interference ####

## Table 6a: Statistics produced by NISRA are free from political interference by year ####

table_6a_data <- readxl::read_xlsx(correct_2016_data, sheet = "2016", range = "A47:F51")

saveRDS(table_6a_data, paste0(data_folder, "Trend/2021/table_6a_data.RDS"))

# Confidence ####

## Table 7a: Personal information provided to NISRA will be kept confidential by year ####

table_7a_data <- readxl::read_xlsx(correct_2016_data, sheet = "2016", range = "A54:F58")

saveRDS(table_7a_data, paste0(data_folder, "Trend/2021/table_7a_data.RDS"))
