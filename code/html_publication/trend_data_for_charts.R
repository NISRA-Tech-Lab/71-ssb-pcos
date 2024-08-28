# Check Trend folder exists
if (!exists(paste0(data_folder, "Trend"))) {
  dir.create(paste0(data_folder, "Trend"))
}

# Check Trend folder 2021 exists
if (!exists(paste0(data_folder, "Trend/2021"))) {
  dir.create(paste0(data_folder, "Trend/2021"))
}

# Chart 1: Awareness of NISRA (2009 - 2021) ####
aware_nisra_data <- read.xlsx(paste0(data_folder, "Trend/PCOS 2022 Charts.xlsx"), sheet = "Chart 1", startRow = 6) %>%
  t() %>%
  as.data.frame() %>%
  mutate(year = as.numeric(rownames(.))) %>%
  mutate(pct = as.numeric(`1`)) %>%
  select(year, pct) %>%
  filter(year != 2022 & !is.na(pct))

rownames(aware_nisra_data) <- 1:nrow(aware_nisra_data)

saveRDS(aware_nisra_data, paste0(data_folder, "Trend/2021/aware_nisra_data.RDS"))

# Chart 2: Awareness of NISRA (2014-2021) and ONS (2014-2021) ####
aware_nisra_ons_data <- read.xlsx(paste0(data_folder, "Trend/PCOS 2022 Charts.xlsx"), sheet = "Chart 2", startRow = 12) %>%
  t() %>%
  as.data.frame() %>%
  mutate(
    year = as.numeric(rownames(.)),
    nisra = as.numeric(`1`),
    ons = as.numeric(`2`)
  ) %>%
  filter(year != 2022) %>%
  select(year, nisra, ons)

rownames(aware_nisra_ons_data) <- 1:nrow(aware_nisra_ons_data)

saveRDS(aware_nisra_ons_data, paste0(data_folder, "/Trend/2021/aware_nisra_ons_data.RDS"))

# Chart 5: Trust in NISRA (2014-2021) ####
trust_nisra_data <- read.xlsx(paste0(data_folder, "Trend/PCOS 2022 Charts.xlsx"), sheet = "Chart6", startRow = 3) %>%
  t() %>%
  as.data.frame() %>%
  mutate(
    year = as.numeric(rownames(.)),
    trust = as.numeric(`3`),
    distrust = as.numeric(`2`),
    dont_know = as.numeric(`1`)
  ) %>%
  filter(year != 2022) %>%
  select(year:dont_know)

rownames(trust_nisra_data) <- 1:nrow(trust_nisra_data)

saveRDS(trust_nisra_data, paste0(data_folder, "/Trend/2021/trust_nisra_data.RDS"))

# Chart 8: Trust in NISRA statistics (2014-2021) ####

trust_stats_data <- read.xlsx(paste0(data_folder, "Trend/PCOS 2022 Charts.xlsx"), sheet = "Chart 10", startRow = 3) %>%
  t() %>%
  as.data.frame() %>%
  mutate(
    year = as.numeric(rownames(.)),
    trust = as.numeric(`3`),
    distrust = as.numeric(`2`),
    dont_know = as.numeric(`1`)
  ) %>%
  filter(year != 2022) %>%
  select(year:dont_know)

rownames(trust_stats_data) <- 1:nrow(trust_stats_data)

saveRDS(trust_stats_data, paste0(data_folder, "/Trend/2021/trust_stats_data.RDS"))

# Chart 10: NISRA statistics are important to understand Northern Ireland (2016-2021) ####

stats_important_data <- read.xlsx(paste0(data_folder, "Trend/PCOS 2022 Charts.xlsx"), sheet = "Chart 13", startRow = 3) %>%
  t() %>%
  as.data.frame() %>%
  mutate(
    year = as.numeric(rownames(.)),
    agree = as.numeric(`3`),
    disagree = as.numeric(`2`),
    dont_know = as.numeric(`1`)
  ) %>%
  filter(year != 2022) %>%
  select(year:dont_know)

rownames(stats_important_data) <- 1:nrow(stats_important_data)

saveRDS(stats_important_data, paste0(data_folder, "/Trend/2021/stats_important_data.RDS"))

# Chart 12: NISRA statistics are free from political interference (2014-2021) ####

political_data <- read.xlsx(paste0(data_folder, "Trend/PCOS 2022 Charts.xlsx"), sheet = "Chart 16", startRow = 3) %>%
  t() %>%
  as.data.frame() %>%
  mutate(
    year = as.numeric(rownames(.)),
    agree = as.numeric(`3`),
    disagree = as.numeric(`2`),
    dont_know = as.numeric(`1`)
  ) %>%
  filter(year != 2022) %>%
  select(year:dont_know)

rownames(political_data) <- 1:nrow(political_data)

saveRDS(political_data, paste0(data_folder, "/Trend/2021/political_data.RDS"))

# Chart 14: Personal information provided to NISRA will be kept confidential (2014-2021) ####

confidential_data <- read.xlsx(paste0(data_folder, "Trend/PCOS 2022 Charts.xlsx"), sheet = "Chart 18", startRow = 3) %>%
  t() %>%
  as.data.frame() %>%
  mutate(
    year = as.numeric(rownames(.)),
    agree = as.numeric(`3`),
    disagree = as.numeric(`2`),
    dont_know = as.numeric(`1`)
  ) %>%
  filter(year != 2022) %>%
  select(year:dont_know)

rownames(confidential_data) <- 1:nrow(confidential_data)

saveRDS(confidential_data, paste0(data_folder, "/Trend/2021/confidential_data.RDS"))
