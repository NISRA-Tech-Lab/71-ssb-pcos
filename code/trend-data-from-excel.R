# Set folder path
library(here)

# Read config file
source(paste0(here(), "/code/config.R"))

# Check Trend folder exists
if (!exists(paste0(data_folder, "Trend"))) {
  dir.create(paste0(data_folder, "Trend"))
}

# Check Trend folder 2021 exists
if (!exists(paste0(data_folder, "Trend/2021"))) {
  dir.create(paste0(data_folder, "Trend/2021"))
}

# Chart 1: Awareness of NISRA (2009 - 2021) ####
chart_1_data <- read.xlsx(paste0(data_folder, "Trend/PCOS 2022 Charts.xlsx"), sheet = "Chart 1", startRow = 6) %>%
  t() %>% as.data.frame() %>%
  mutate(year = as.numeric(rownames(.))) %>%
  mutate(pct = as.numeric(`1`)) %>%
  select(year, pct) %>%
  filter(year != 2022 & !is.na(pct))

rownames(chart_1_data) <- 1:nrow(chart_1_data)

saveRDS(chart_1_data, paste0(data_folder, "Trend/2021/chart_1_data.RDS"))

# Chart 2: Awareness of NISRA (2014-2021) and ONS (2014-2021) ####
chart_2_data <- read.xlsx(paste0(data_folder, "Trend/PCOS 2022 Charts.xlsx"), sheet = "Chart 2", startRow = 12) %>%
  t() %>% as.data.frame() %>%
  mutate(year = as.numeric(rownames(.)),
         nisra = as.numeric(`1`),
         ons = as.numeric(`2`)) %>%
  filter(year != 2022) %>%
  select(year, nisra, ons)

rownames(chart_2_data) <- 1:nrow(chart_2_data)

saveRDS(chart_2_data, paste0(data_folder, "/Trend/2021/chart_2_data.RDS"))

# Chart 5: Trust in NISRA (2014-2021) ####
chart_5_data <- read.xlsx(paste0(data_folder, "Trend/PCOS 2022 Charts.xlsx"), sheet = "Chart6", startRow = 3) %>%
  t() %>% as.data.frame() %>%
  mutate(year = as.numeric(rownames(.)),
         trust = as.numeric(`3`),
         distrust = as.numeric(`2`),
         dunno = as.numeric(`1`)) %>%
  filter(year != 2022) %>%
  select(year:dunno)

rownames(chart_5_data) <- 1:nrow(chart_5_data)

saveRDS(chart_5_data, paste0(data_folder, "/Trend/2021/chart_5_data.RDS"))

# Chart 8: Trust in NISRA statistics (2014-2021) #### 

chart_8_data <- read.xlsx(paste0(data_folder, "Trend/PCOS 2022 Charts.xlsx"), sheet = "Chart 10", startRow = 3) %>%
  t() %>% as.data.frame() %>%
  mutate(year = as.numeric(rownames(.)),
         trust = as.numeric(`3`),
         distrust = as.numeric(`2`),
         dunno = as.numeric(`1`)) %>%
  filter(year != 2022) %>%
  select(year:dunno)

rownames(chart_8_data) <- 1:nrow(chart_8_data)

saveRDS(chart_8_data, paste0(data_folder, "/Trend/2021/chart_8_data.RDS"))

# Chart 10: NISRA statistics are important to understand Northern Ireland (2016-2021) ####

chart_10_data <- read.xlsx(paste0(data_folder, "Trend/PCOS 2022 Charts.xlsx"), sheet = "Chart 13", startRow = 3) %>%
  t() %>% as.data.frame()  %>%
  mutate(year = as.numeric(rownames(.)),
         agree = as.numeric(`3`),
         disagree = as.numeric(`2`),
         dunno = as.numeric(`1`)) %>%
  filter(year != 2022) %>%
  select(year:dunno)

rownames(chart_10_data) <- 1:nrow(chart_10_data)

saveRDS(chart_10_data, paste0(data_folder, "/Trend/2021/chart_10_data.RDS"))

# Chart 12: NISRA statistics are free from political interference (2014-2021) ####

chart_12_data <- read.xlsx(paste0(data_folder, "Trend/PCOS 2022 Charts.xlsx"), sheet = "Chart 16", startRow = 3) %>%
  t() %>% 
  as.data.frame() %>%
  mutate(year = as.numeric(rownames(.)),
         agree = as.numeric(`3`),
         disagree = as.numeric(`2`),
         dunno = as.numeric(`1`)) %>%
  filter(year != 2022) %>%
  select(year:dunno)

rownames(chart_12_data) <- 1:nrow(chart_12_data)

saveRDS(chart_12_data, paste0(data_folder, "/Trend/2021/chart_12_data.RDS"))

# Chart 14: Personal information provided to NISRA will be kept confidential (2014-2021) ####

chart_14_data <- read.xlsx(paste0(data_folder, "Trend/PCOS 2022 Charts.xlsx"), sheet = "Chart 18", startRow = 3) %>%
  t() %>%
  as.data.frame() %>%
  mutate(year = as.numeric(rownames(.)),
         agree = as.numeric(`3`),
         disagree = as.numeric(`2`),
         dunno = as.numeric(`1`)) %>%
  filter(year != 2022) %>%
  select(year:dunno)

rownames(chart_14_data) <- 1:nrow(chart_14_data)

saveRDS(chart_14_data, paste0(data_folder, "/Trend/2021/chart_14_data.RDS"))

