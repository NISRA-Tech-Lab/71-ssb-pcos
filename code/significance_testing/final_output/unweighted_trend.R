# Do not need to run
# This script is only called when needed via a condition in significance_testing.R

library(here)
source(paste0(here(), "/code/config.R"))

# First read in trend data from template ####

unweighted_trend_sheet <- paste0(data_folder, "Trend/unweighted trend data.xlsx")

sheets <- getSheetNames(unweighted_trend_sheet)

for (i in 1:length(sheets)) {
  if (i == 1) {
    trend_data <- readxl::read_xlsx(unweighted_trend_sheet, sheet = sheets[i]) %>%
      mutate(stat = paste(sheets[i], "-", stat))
  } else {
    trend_data <- trend_data %>%
      bind_rows(readxl::read_xlsx(unweighted_trend_sheet, sheet = sheets[i]) %>%
        mutate(stat = paste(sheets[i], "-", stat)))
  }
}

# Missing values from 2020 ####

data_2020 <- readRDS(paste0(data_folder, "Final/PCOS 2020 Final Dataset.RDS"))

## TrustNISRA2 ####

trust_2020 <- data_2020 %>%
  group_by(TrustNISRA2) %>%
  summarise(count = n()) %>%
  filter(!is.na(TrustNISRA2)) %>%
  mutate(pct = count / sum(count) * 100) %>%
  adorn_totals() %>%
  mutate(
    stat = c(
      "Trust in NISRA - % Yes",
      "Trust in NISRA - % No",
      "Trust in NISRA - % DK",
      "Trust in NISRA - Base"
    ),
    pct = case_when(
      grepl("Base", stat) ~ count,
      TRUE ~ pct
    )
  ) %>%
  select(stat, pct)

## TrustNISRAstats2 ####

trust_stats_2020 <- data_2020 %>%
  group_by(TrustNISRAstats2) %>%
  summarise(count = n()) %>%
  filter(!is.na(TrustNISRAstats2)) %>%
  mutate(pct = count / sum(count) * 100) %>%
  adorn_totals() %>%
  mutate(
    stat = c(
      "TrustNISRAStats - % Yes",
      "TrustNISRAStats - % No",
      "TrustNISRAStats - % DK",
      "TrustNISRAStats - Base"
    ),
    pct = case_when(
      grepl("Base", stat) ~ count,
      TRUE ~ pct
    )
  ) %>%
  select(stat, pct)

## NISRAstatsImp2 ####

value_2020 <- data_2020 %>%
  group_by(NISRAstatsImp2) %>%
  summarise(count = n()) %>%
  filter(!is.na(NISRAstatsImp2)) %>%
  mutate(pct = count / sum(count) * 100) %>%
  adorn_totals() %>%
  mutate(
    stat = c(
      "Value - % Yes",
      "Value - % No",
      "Value - % DK",
      "Value - Base"
    ),
    pct = case_when(
      grepl("Base", stat) ~ count,
      TRUE ~ pct
    )
  ) %>%
  select(stat, pct)

## Political2 ####

political_2020 <- data_2020 %>%
  group_by(Political2) %>%
  summarise(count = n()) %>%
  filter(!is.na(Political2)) %>%
  mutate(pct = count / sum(count) * 100) %>%
  adorn_totals() %>%
  mutate(
    stat = c(
      "Interference - % Yes",
      "Interference - % No",
      "Interference - % DK",
      "Interference - Base"
    ),
    pct = case_when(
      grepl("Base", stat) ~ count,
      TRUE ~ pct
    )
  ) %>%
  select(stat, pct)

## Confidential2 ####

confidential_2020 <- data_2020 %>%
  group_by(Confidential2) %>%
  summarise(count = n()) %>%
  filter(!is.na(Confidential2)) %>%
  mutate(pct = count / sum(count) * 100) %>%
  adorn_totals() %>%
  mutate(
    stat = c(
      "Confidentiality - % Yes",
      "Confidentiality - % No",
      "Confidentiality - % DK",
      "Confidentiality - Base"
    ),
    pct = case_when(
      grepl("Base", stat) ~ count,
      TRUE ~ pct
    )
  ) %>%
  select(stat, pct)

## Insert missing 2020 values into data frame ####

trend_2020 <- bind_rows(
  trust_2020,
  trust_stats_2020,
  value_2020,
  political_2020,
  confidential_2020
)

trend_data <- trend_data %>%
  left_join(trend_2020,
    by = "stat"
  ) %>%
  mutate(`2020` = case_when(
    !is.na(pct) ~ pct,
    TRUE ~ `2020`
  )) %>%
  select(-pct)

# Missing values from 2016 ####

data_2016 <- readRDS(paste0(data_folder, "Final/PCOS 2016 Final Dataset.RDS"))

## TrustNISRA2 ####

trust_2016 <- data_2016 %>%
  group_by(TrustNISRA2) %>%
  summarise(count = n()) %>%
  filter(!is.na(TrustNISRA2))

trust_2016_all <- trust_2016 %>%
  mutate(pct = count / sum(count) * 100) %>%
  adorn_totals() %>%
  mutate(
    stat = c(
      "Trust in NISRA - % Yes",
      "Trust in NISRA - % No",
      "Trust in NISRA - % DK",
      "Trust in NISRA - Base"
    ),
    pct = case_when(
      grepl("Base", stat) ~ count,
      TRUE ~ pct
    )
  ) %>%
  select(stat, pct)

### Excluding Don't knows ####

trust_2016_ex_dk <- trust_2016 %>%
  filter(TrustNISRA2 != "Don't Know") %>%
  mutate(pct = count / sum(count) * 100) %>%
  adorn_totals() %>%
  filter(TrustNISRA2 != "Tend to distrust/distrust greatly") %>%
  mutate(
    stat = c(
      "TruNISRAexcDK - % Yes",
      "TruNISRAexcDK - Base"
    ),
    pct = case_when(
      grepl("Base", stat) ~ count,
      TRUE ~ pct
    )
  ) %>%
  select(stat, pct)

## TrustNISRAstats2 ####

trust_stats_2016 <- data_2016 %>%
  group_by(TrustNISRAstats2) %>%
  summarise(count = n()) %>%
  filter(!is.na(TrustNISRAstats2))

trust_stats_2016_all <- trust_stats_2016 %>%
  mutate(pct = count / sum(count) * 100) %>%
  adorn_totals() %>%
  mutate(
    stat = c(
      "TrustNISRAStats - % Yes",
      "TrustNISRAStats - % No",
      "TrustNISRAStats - % DK",
      "TrustNISRAStats - Base"
    ),
    pct = case_when(
      grepl("Base", stat) ~ count,
      TRUE ~ pct
    )
  ) %>%
  select(stat, pct)

### Excluding Don't knows ####

trust_stats_2016_ex_dk <- trust_stats_2016 %>%
  filter(TrustNISRAstats2 != "Don't Know") %>%
  mutate(pct = count / sum(count) * 100) %>%
  adorn_totals() %>%
  filter(TrustNISRAstats2 != "Tend to distrust/distrust greatly") %>%
  mutate(
    stat = c(
      "TruNISRAStatsexcDK - % Yes",
      "TruNISRAStatsexcDK - Base"
    ),
    pct = case_when(
      grepl("Base", stat) ~ count,
      TRUE ~ pct
    )
  ) %>%
  select(stat, pct)

## NISRAstatsImp2 ####

value_2016 <- data_2016 %>%
  group_by(NISRAstatsImp2) %>%
  summarise(count = n()) %>%
  filter(!is.na(NISRAstatsImp2))

value_2016_all <- value_2016 %>%
  mutate(pct = count / sum(count) * 100) %>%
  adorn_totals() %>%
  mutate(
    stat = c(
      "Value - % Yes",
      "Value - % No",
      "Value - % DK",
      "Value - Base"
    ),
    pct = case_when(
      grepl("Base", stat) ~ count,
      TRUE ~ pct
    )
  ) %>%
  select(stat, pct)

### Excluding Don't knows ####

value_2016_ex_dk <- value_2016 %>%
  filter(NISRAstatsImp2 != "Don't Know") %>%
  mutate(pct = count / sum(count) * 100) %>%
  adorn_totals() %>%
  filter(NISRAstatsImp2 != "Tend to disagree/Strongly disagree") %>%
  mutate(
    stat = c(
      "ValuesExDK - % Yes",
      "ValuesExDK - Base"
    ),
    pct = case_when(
      grepl("Base", stat) ~ count,
      TRUE ~ pct
    )
  ) %>%
  select(stat, pct)

## Political2 ####

political_2016 <- data_2016 %>%
  group_by(Political2) %>%
  summarise(count = n()) %>%
  filter(!is.na(Political2))

political_2016_all <- political_2016 %>%
  mutate(pct = count / sum(count) * 100) %>%
  adorn_totals() %>%
  mutate(
    stat = c(
      "Interference - % Yes",
      "Interference - % No",
      "Interference - % DK",
      "Interference - Base"
    ),
    pct = case_when(
      grepl("Base", stat) ~ count,
      TRUE ~ pct
    )
  ) %>%
  select(stat, pct)

### Excluding Don't knows ####

political_2016_ex_dk <- political_2016 %>%
  filter(Political2 != "Don't Know") %>%
  mutate(pct = count / sum(count) * 100) %>%
  adorn_totals() %>%
  filter(Political2 != "Tend to disagree/Strongly disagree") %>%
  mutate(
    stat = c(
      "InterfExDK - % Yes",
      "InterfExDK - Base"
    ),
    pct = case_when(
      grepl("Base", stat) ~ count,
      TRUE ~ pct
    )
  ) %>%
  select(stat, pct)

## Confidential2 ####

confidential_2016 <- data_2016 %>%
  group_by(Confidential2) %>%
  summarise(count = n()) %>%
  filter(!is.na(Confidential2))

confidential_2016_all <- confidential_2016 %>%
  mutate(pct = count / sum(count) * 100) %>%
  adorn_totals() %>%
  mutate(
    stat = c(
      "Confidentiality - % Yes",
      "Confidentiality - % No",
      "Confidentiality - % DK",
      "Confidentiality - Base"
    ),
    pct = case_when(
      grepl("Base", stat) ~ count,
      TRUE ~ pct
    )
  ) %>%
  select(stat, pct)

### Excluding Don't knows ####

confidential_2016_ex_dk <- confidential_2016 %>%
  filter(Confidential2 != "Don't Know") %>%
  mutate(pct = count / sum(count) * 100) %>%
  adorn_totals() %>%
  filter(Confidential2 != "Tend to disagree/Strongly disagree") %>%
  mutate(
    stat = c(
      "ConfExDK - % Yes",
      "ConfExDK - Base"
    ),
    pct = case_when(
      grepl("Base", stat) ~ count,
      TRUE ~ pct
    )
  ) %>%
  select(stat, pct)

## Insert missing 2016 values into data frame ####

trend_2016 <- bind_rows(
  trust_2016_all,
  trust_2016_ex_dk,
  trust_stats_2016_all,
  trust_stats_2016_ex_dk,
  value_2016_all,
  value_2016_ex_dk,
  political_2016_all,
  political_2016_ex_dk,
  confidential_2016_all,
  confidential_2016_ex_dk
)

trend_data <- trend_data %>%
  left_join(trend_2016,
    by = "stat"
  ) %>%
  mutate(`2016` = case_when(
    !is.na(pct) ~ pct,
    TRUE ~ `2016`
  )) %>%
  select(-pct)

# Missing values from 2014 ####

data_2014 <- readRDS(paste0(data_folder, "Final/PCOS 2014 Final Dataset.RDS"))

## PCOS1 ####

aware_2014 <- data_2014 %>%
  group_by(PCOS1) %>%
  summarise(count = n()) %>%
  filter(!is.na(PCOS1)) %>%
  mutate(pct = count / sum(count) * 100) %>%
  adorn_totals() %>%
  filter(PCOS1 != "No") %>%
  mutate(
    stat = c(
      "Awareness - % Yes",
      "Awareness - Base"
    ),
    pct = case_when(
      grepl("Base", stat) ~ count,
      TRUE ~ pct
    )
  ) %>%
  select(stat, pct)

## TrustNISRA2 ####

trust_2014 <- data_2014 %>%
  group_by(TrustNISRA2) %>%
  summarise(count = n()) %>%
  filter(!is.na(TrustNISRA2))

trust_2014_all <- trust_2014 %>%
  mutate(pct = count / sum(count) * 100) %>%
  adorn_totals() %>%
  mutate(
    stat = c(
      "Trust in NISRA - % Yes",
      "Trust in NISRA - % No",
      "Trust in NISRA - % DK",
      "Trust in NISRA - Base"
    ),
    pct = case_when(
      grepl("Base", stat) ~ count,
      TRUE ~ pct
    )
  ) %>%
  select(stat, pct)

### Excluding Don't knows ####

trust_2014_ex_dk <- trust_2014 %>%
  filter(TrustNISRA2 != "Don't Know") %>%
  mutate(pct = count / sum(count) * 100) %>%
  adorn_totals() %>%
  filter(TrustNISRA2 != "Tend to distrust/distrust greatly") %>%
  mutate(
    stat = c(
      "TruNISRAexcDK - % Yes",
      "TruNISRAexcDK - Base"
    ),
    pct = case_when(
      grepl("Base", stat) ~ count,
      TRUE ~ pct
    )
  ) %>%
  select(stat, pct)

## TrustNISRAstats2 ####

trust_stats_2014 <- data_2014 %>%
  group_by(TrustNISRAstats2) %>%
  summarise(count = n()) %>%
  filter(!is.na(TrustNISRAstats2))

trust_stats_2014_all <- trust_stats_2014 %>%
  mutate(pct = count / sum(count) * 100) %>%
  adorn_totals() %>%
  mutate(
    stat = c(
      "TrustNISRAStats - % Yes",
      "TrustNISRAStats - % No",
      "TrustNISRAStats - % DK",
      "TrustNISRAStats - Base"
    ),
    pct = case_when(
      grepl("Base", stat) ~ count,
      TRUE ~ pct
    )
  ) %>%
  select(stat, pct)

### Excluding Don't knows ####

trust_stats_2014_ex_dk <- trust_stats_2014 %>%
  filter(TrustNISRAstats2 != "Don't Know") %>%
  mutate(pct = count / sum(count) * 100) %>%
  adorn_totals() %>%
  filter(TrustNISRAstats2 != "Tend to distrust/distrust greatly") %>%
  mutate(
    stat = c(
      "TruNISRAStatsexcDK - % Yes",
      "TruNISRAStatsexcDK - Base"
    ),
    pct = case_when(
      grepl("Base", stat) ~ count,
      TRUE ~ pct
    )
  ) %>%
  select(stat, pct)

## Political2 ####

political_2014 <- data_2014 %>%
  group_by(Political2) %>%
  summarise(count = n()) %>%
  filter(!is.na(Political2))

political_2014_all <- political_2014 %>%
  mutate(pct = count / sum(count) * 100) %>%
  adorn_totals() %>%
  mutate(
    stat = c(
      "Interference - % Yes",
      "Interference - % No",
      "Interference - % DK",
      "Interference - Base"
    ),
    pct = case_when(
      grepl("Base", stat) ~ count,
      TRUE ~ pct
    )
  ) %>%
  select(stat, pct)

### Excluding Don't knows ####

political_2014_ex_dk <- political_2014 %>%
  filter(Political2 != "Don't Know") %>%
  mutate(pct = count / sum(count) * 100) %>%
  adorn_totals() %>%
  filter(Political2 != "Tend to disagree/Strongly disagree") %>%
  mutate(
    stat = c(
      "InterfExDK - % Yes",
      "InterfExDK - Base"
    ),
    pct = case_when(
      grepl("Base", stat) ~ count,
      TRUE ~ pct
    )
  ) %>%
  select(stat, pct)

## Confidential2 ####

confidential_2014 <- data_2014 %>%
  group_by(Confidential2) %>%
  summarise(count = n()) %>%
  filter(!is.na(Confidential2))

confidential_2014_all <- confidential_2014 %>%
  mutate(pct = count / sum(count) * 100) %>%
  adorn_totals() %>%
  mutate(
    stat = c(
      "Confidentiality - % Yes",
      "Confidentiality - % No",
      "Confidentiality - % DK",
      "Confidentiality - Base"
    ),
    pct = case_when(
      grepl("Base", stat) ~ count,
      TRUE ~ pct
    )
  ) %>%
  select(stat, pct)

### Excluding Don't knows ####

confidential_2014_ex_dk <- confidential_2014 %>%
  filter(Confidential2 != "Don't Know") %>%
  mutate(pct = count / sum(count) * 100) %>%
  adorn_totals() %>%
  filter(Confidential2 != "Tend to disagree/Strongly disagree") %>%
  mutate(
    stat = c(
      "ConfExDK - % Yes",
      "ConfExDK - Base"
    ),
    pct = case_when(
      grepl("Base", stat) ~ count,
      TRUE ~ pct
    )
  ) %>%
  select(stat, pct)

## Insert missing 2014 values into data frame ####

trend_2014 <- bind_rows(
  aware_2014,
  trust_2014_all,
  trust_2014_ex_dk,
  trust_stats_2014_all,
  trust_stats_2014_ex_dk,
  political_2014_all,
  political_2014_ex_dk,
  confidential_2014_all,
  confidential_2014_ex_dk
)

trend_data <- trend_data %>%
  left_join(trend_2014,
    by = "stat"
  ) %>%
  mutate(`2014` = case_when(
    !is.na(pct) ~ pct,
    TRUE ~ `2014`
  )) %>%
  select(-pct)

# Missing values from 2012 ####

data_2012 <- readRDS(paste0(data_folder, "Final/PCOS 2012 Final Dataset.RDS"))

## PCOS4 ####

aware_2012 <- data_2012 %>%
  group_by(PCOS4) %>%
  summarise(count = n()) %>%
  filter(!is.na(PCOS4)) %>%
  mutate(pct = count / sum(count) * 100) %>%
  adorn_totals() %>%
  filter(!PCOS4 %in% c("No", "Don't Know")) %>%
  mutate(
    stat = c(
      "Awareness - % Yes",
      "Awareness - Base"
    ),
    pct = case_when(
      grepl("Base", stat) ~ count,
      TRUE ~ pct
    )
  ) %>%
  select(stat, pct)

## Insert missing 2012 values into data frame ####

names(aware_2012) <- c("stat", "2012")

trend_data <- trend_data %>%
  left_join(aware_2012,
            by = "stat")

# Remove 2022 from trend data (save out to 2021 folder) ####

# 2022 (and all future) values will be recalculated when
# significance_testing.R is run for new year

trend_data <- trend_data %>%
  select(-`2022`)

saveRDS(trend_data, paste0(data_folder, "Trend/2021/unweighted trend data.RDS"))
