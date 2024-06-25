# Set folder path
library(here)

# Read config file
source(paste0(here(), "/code/config.R"))

chart_1_data <- read.xlsx(paste0(data_folder, "PCOS 2022 Charts.xlsx"), sheet = "Chart 1", startRow = 6) %>%
  t() %>% as.data.frame() %>%
  mutate(year = rownames(.)) %>%
  filter(year != "X1") %>%
  mutate(pct = round_half_up(as.numeric(`1`))) %>%
  select(year, pct) %>%
  filter(year != current_year)

saveRDS(chart_1_data, paste0(data_folder, "/Trend/chart_1_data.RDS"))

chart_2_data <- read.xlsx(paste0(data_folder, "PCOS 2022 Charts.xlsx"), sheet = "Chart 2", startRow = 12) %>%
  t() %>% as.data.frame() %>%
  mutate(year = rownames(.)) %>%
  filter(year != "X1") %>%
  mutate(NISRA = round_half_up(as.numeric(`1`))) %>%
  mutate(ONS = round_half_up(as.numeric(`2`))) %>%
  filter(year != current_year) %>%
  select(year, NISRA, ONS) 

saveRDS(chart_2_data, paste0(data_folder, "/Trend/chart_2_data.RDS"))

chart_6_data <- read.xlsx(paste0(data_folder, "PCOS 2022 Charts.xlsx"), sheet = "Chart6", startRow = 3) %>%
  t() %>% as.data.frame()  %>%
  row_to_names(row_number = 1) %>%
  mutate(year = rownames(.)) %>%
  filter(year != current_year) %>%
  mutate(across(1:3, as.numeric)) %>%
  mutate(across(1:3, round_half_up, 0))

saveRDS(chart_6_data, paste0(data_folder, "/Trend/chart_6_data.RDS"))

chart_10_data <- read.xlsx(paste0(data_folder, "PCOS 2022 Charts.xlsx"), sheet = "Chart 10", startRow = 3) %>%
  t() %>% as.data.frame()  %>%
  row_to_names(row_number = 1) %>%
  mutate(year = rownames(.)) %>%
  filter(year != current_year) %>%
  mutate(across(1:3, as.numeric)) %>%
  mutate(across(1:3, round_half_up))

saveRDS(chart_10_data, paste0(data_folder, "/Trend/chart_10_data.RDS"))

chart_13_data <- read.xlsx(paste0(data_folder, "PCOS 2022 Charts.xlsx"), sheet = "Chart 13", startRow = 3) %>%
  t() %>% as.data.frame()  %>%
  row_to_names(row_number = 1) %>%
  mutate(year = rownames(.)) %>%
  filter(year != current_year) %>%
  mutate(across(1:3, as.numeric)) %>%
  mutate(across(1:3, round_half_up))

saveRDS(chart_13_data, paste0(data_folder, "/Trend/chart_13_data.RDS"))

chart_15_data <- read.xlsx(paste0(data_folder, "PCOS 2022 Charts.xlsx"), sheet = "Chart 16", startRow = 3) %>%
  t() %>% 
  as.data.frame() %>%
  row_to_names(row_number = 1) %>%
  mutate(year = rownames(.)) %>%
  mutate(across(1:3, as.numeric)) %>%
  mutate(across(1:3, round_half_up)) %>%
  filter(year != current_year) 

saveRDS(chart_15_data, paste0(data_folder, "/Trend/chart_15_data.RDS"))

chart_17_data <- read.xlsx(paste0(data_folder, "PCOS 2022 Charts.xlsx"), sheet = "Chart 18", startRow = 3) %>%
  t() %>% as.data.frame()  %>%
  row_to_names(row_number = 1) %>%
  mutate(year = rownames(.)) %>%
  filter(year != current_year) %>%
  mutate(across(1:3, as.numeric)) %>%
  mutate(across(1:3, round_half_up, 1))

saveRDS(chart_17_data, paste0(data_folder, "/Trend/chart_17_data.RDS"))

