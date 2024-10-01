# Script to create weighted trend data file up to 2021.
# One-off run

library(here)
source(paste0(here(), "/code/config.R"))

# Check trend folder for 2021 exists and run script to create it if not
trend_folder <- paste0(data_folder, "Trend/2021/")

if (!dir.exists(trend_folder)) {
  source(paste0(here(), "/code/html_publication/trend_data_for_charts.R"))
  source(paste0(here(), "/code/ods_tables/ods_trend_data.R"))
}

trend_years <- c(seq(2012, 2018, 2), 2019:2021)

for (year in trend_years) {
  if (!file.exists(paste0(data_folder, "Final/PCOS ", year, " Final Dataset.RDS"))) {
    source(paste0(here(), "/code/pfg_tables/Historic Data to R.R"))
  }
}

# Awareness of NISRA ####

aware_trend <- readRDS(paste0(trend_folder, "table_1a_data.RDS"))

names(aware_trend) <- gsub(" [Note 1]", "", names(aware_trend), fixed = TRUE)

aware_trend <- aware_trend %>%
  select(stat = `Response (%)`, rev(everything())) %>%
  filter(stat %in% c("Yes", "Number of Respondents")) %>%
  mutate(stat = c("Awareness - % Yes",
                  "Awareness - Base"))

# Trust in NISRA ####

trust_trend <- readRDS(paste0(trend_folder, "table_3.1a_data.RDS")) %>%
  rename(stat = `Response (%)`) %>%
  mutate(stat = c("Trust in NISRA - % Yes",
                  "Trust in NISRA - % No",
                  "Trust in NISRA - % DK",
                  "Trust in NISRA - Base"))

## Excluding Don't Knows ####

trend_years <- names(trust_trend)[names(trust_trend) != "stat"]

trust_trend_ex_dk <- data.frame(stat = c("TruNISRAexcDK - % Yes",
                                         "TruNISRAexcDK - Base"))

for (year in trend_years) {
  
  year_data <- readRDS(paste0(data_folder, "Final/PCOS ", year, " Final Dataset.RDS")) %>%
    filter(TrustNISRA2 != "Refusal")
  
  levels(year_data$TrustNISRA2)[levels(year_data$TrustNISRA2) == "Tend to trust/trust a great deal"] <- "Trust a great deal/Tend to trust"
  levels(year_data$TrustNISRA2)[levels(year_data$TrustNISRA2) == "Don't Know"] <- "Don't know"
  
  weight <- if (year == "2020") {
    "W4"
  } else if (as.numeric(year) %in% 2012:2016) {
    "weight"
  } else {
    "W3"
  }
  
  trust_trend_ex_dk$value <- c(f_return_p(year_data, "TrustNISRA2", "Trust a great deal/Tend to trust", weight = weight, dk = FALSE) * 100,
                               f_return_n(year_data$TrustNISRA2[year_data$TrustNISRA2 != "Don't know"]))
  
  names(trust_trend_ex_dk)[names(trust_trend_ex_dk) == "value"] <- year
  
}

# Trust NISRA Statistics ####

trust_stats_trend <- readRDS(paste0(trend_folder, "table_4a_data.RDS")) %>%
  rename(stat = `Response (%)`) %>%
  mutate(stat = c("TrustNISRAStats - % Yes",
                  "TrustNISRAStats - % No",
                  "TrustNISRAStats - % DK",
                  "TrustNISRAStats - Base"))

## Excluding Don't Knows ####

trend_years <- names(trust_stats_trend)[names(trust_stats_trend) != "stat"]

trust_stats_trend_ex_dk <- data.frame(stat = c("TruNISRAStatsexcDK - % Yes",
                                               "TruNISRAStatsexcDK - Base"))

for (year in trend_years) {
  
  year_data <- readRDS(paste0(data_folder, "Final/PCOS ", year, " Final Dataset.RDS")) %>%
    filter(TrustNISRAstats2 != "Refusal")
  
  levels(year_data$TrustNISRAstats2)[levels(year_data$TrustNISRAstats2) == "Tend to trust/trust a great deal"] <- "Trust a great deal/Tend to trust"
  levels(year_data$TrustNISRAstats2)[levels(year_data$TrustNISRAstats2) == "Don't Know"] <- "Don't know"
  
  weight <- if (year == "2020") {
    "W4"
  } else if (as.numeric(year) %in% 2012:2016) {
    "weight"
  } else {
    "W3"
  }
  
  trust_stats_trend_ex_dk$value <- c(f_return_p(year_data, "TrustNISRAstats2", "Trust a great deal/Tend to trust", weight = weight, dk = FALSE) * 100,
                                     f_return_n(year_data$TrustNISRAstats2[year_data$TrustNISRAstats2 != "Don't know"]))
  
  names(trust_stats_trend_ex_dk)[names(trust_stats_trend_ex_dk) == "value"] <- year
  
}

# Value ####

value_trend <- readRDS(paste0(trend_folder, "table_5a_data.RDS")) %>%
  rename(stat = `Response (%)`) %>%
  mutate(stat = c("Value - % Yes",
                  "Value - % No",
                  "Value - % DK",
                  "Value - Base"))

## Excluding Don't Knows ####

trend_years <- names(value_trend)[names(value_trend) != "stat"]

value_trend_ex_dk <- data.frame(stat = c("ValuesExDK - % Yes",
                                         "ValuesExDK - Base"))

for (year in trend_years) {
  
  year_data <- readRDS(paste0(data_folder, "Final/PCOS ", year, " Final Dataset.RDS")) %>%
    filter(NISRAstatsImp2 != "Refusal")
  
  levels(year_data$NISRAstatsImp2)[levels(year_data$NISRAstatsImp2) == "Strongly agree/Tend to agree"] <- "Strongly Agree/Tend to Agree"
  levels(year_data$NISRAstatsImp2)[levels(year_data$NISRAstatsImp2) == "Don't Know"] <- "Don't know"
  
  weight <- if (year == "2020") {
    "W4"
  } else if (as.numeric(year) %in% 2012:2016) {
    "weight"
  } else {
    "W3"
  }
  
  value_trend_ex_dk$value <- c(f_return_p(year_data, "NISRAstatsImp2", "Strongly Agree/Tend to Agree", weight = weight, dk = FALSE) * 100,
                               f_return_n(year_data$NISRAstatsImp2[year_data$NISRAstatsImp2 != "Don't know"]))
  
  names(value_trend_ex_dk)[names(value_trend_ex_dk) == "value"] <- year
  
}

# Political Interference ####

interference_trend <- readRDS(paste0(trend_folder, "table_6a_data.RDS")) %>%
  rename(stat = `Response (%)`) %>%
  mutate(stat = c("Interference - % Yes",
                  "Interference - % No",
                  "Interference - % DK",
                  "Interference - Base"))

## Excluding Don't Knows ####

trend_years <- names(interference_trend)[names(interference_trend) != "stat"]

interference_trend_ex_dk <- data.frame(stat = c("InterfExDK - % Yes",
                                                "InterfExDK - Base"))

for (year in trend_years) {
  
  year_data <- readRDS(paste0(data_folder, "Final/PCOS ", year, " Final Dataset.RDS")) %>%
    filter(Political2 != "Refusal")
  
  levels(year_data$Political2)[levels(year_data$Political2) == "Strongly agree/Tend to agree"] <- "Strongly Agree/Tend to Agree"
  levels(year_data$Political2)[levels(year_data$Political2) == "Don't Know"] <- "Don't know"
  
  weight <- if (year == "2020") {
    "W4"
  } else if (as.numeric(year) %in% 2012:2016) {
    "weight"
  } else {
    "W3"
  }
  
  interference_trend_ex_dk$value <- c(f_return_p(year_data, "Political2", "Strongly Agree/Tend to Agree", weight = weight, dk = FALSE) * 100,
                                      f_return_n(year_data$Political2[year_data$Political2 != "Don't know"]))
  
  names(interference_trend_ex_dk)[names(interference_trend_ex_dk) == "value"] <- year
  
}

# Confidentiality ####

confidential_trend <- readRDS(paste0(trend_folder, "table_7a_data.RDS")) %>%
  rename(stat = `Response (%)`) %>%
  mutate(stat = c("Confidentiality - % Yes",
                  "Confidentiality - % No",
                  "Confidentiality - % DK",
                  "Confidentiality - Base"))

## Excluding Don't Knows ####

trend_years <- names(confidential_trend)[names(confidential_trend) != "stat"]

confidential_trend_ex_dk <- data.frame(stat = c("ConfExDK - % Yes",
                                                "ConfExDK - Base"))

for (year in trend_years) {
  
  year_data <- readRDS(paste0(data_folder, "Final/PCOS ", year, " Final Dataset.RDS")) %>%
    filter(Confidential2 != "Refusal")
  
  levels(year_data$Confidential2)[levels(year_data$Confidential2) == "Strongly agree/Tend to agree"] <- "Strongly Agree/Tend to Agree"
  levels(year_data$Confidential2)[levels(year_data$Confidential2) == "Don't Know"] <- "Don't know"
  
  weight <- if (year == "2020") {
    "W4"
  } else if (as.numeric(year) %in% 2012:2016) {
    "weight"
  } else {
    "W3"
  }
  
  confidential_trend_ex_dk$value <- c(f_return_p(year_data, "Confidential2", "Strongly Agree/Tend to Agree", weight = weight, dk = FALSE) * 100,
                                      f_return_n(year_data$Confidential2[year_data$Confidential2 != "Don't know"]))
  
  names(confidential_trend_ex_dk)[names(confidential_trend_ex_dk) == "value"] <- year
  
}

# Trust NI Assembly ###

trend_years <- c(2014, 2016, 2019:2021)

assembly_trend <- data.frame(stat = c("Trust NI Assembly - % Yes",
                                      "Trust NI Assembly - % No",
                                      "Trust NI Assembly - % DK",
                                      "Trust NI Assembly - Base",
                                      "TruNIAssemExDK - % Yes",
                                      "TruNIAssemExDK - Base"))

for (year in trend_years) {
  
  year_data <- readRDS(paste0(data_folder, "Final/PCOS ", year, " Final Dataset.RDS")) %>%
    filter(TrustAssemblyElectedBody2 != "Refusal")
    
  levels(year_data$TrustAssemblyElectedBody2)[levels(year_data$TrustAssemblyElectedBody2) == "Tend to trust/trust a great deal"] <- "Trust a great deal/Tend to trust"
  levels(year_data$TrustAssemblyElectedBody2)[levels(year_data$TrustAssemblyElectedBody2) == "Tend to distrust/distrust greatly"] <- "Tend to distrust/Distrust greatly"
  levels(year_data$TrustAssemblyElectedBody2)[levels(year_data$TrustAssemblyElectedBody2) == "Don't Know"] <- "Don't know"

  
  weight <- if (year == 2020) {
    "W4"
  } else if (year %in% 2012:2016) {
    "weight"
  } else {
    "W3"
  }
  
  assembly_trend$value <- c(f_return_p(year_data, "TrustAssemblyElectedBody2", "Trust a great deal/Tend to trust", weight = weight) * 100,
                            f_return_p(year_data, "TrustAssemblyElectedBody2", "Tend to distrust/Distrust greatly", weight = weight) * 100,
                            f_return_p(year_data, "TrustAssemblyElectedBody2", "Don't know", weight = weight) * 100,
                            f_return_n(year_data$TrustAssemblyElectedBody2),
                            f_return_p(year_data, "TrustAssemblyElectedBody2", "Trust a great deal/Tend to trust", weight = weight, dk = FALSE) * 100,
                            f_return_n(year_data$TrustAssemblyElectedBody2[year_data$TrustAssemblyElectedBody2 != "Don't know"]))
  
  names(assembly_trend)[names(assembly_trend) == "value"] <- year
  
}

# Trust Media ###

trend_years <- c(2014, 2016, 2019:2021)

media_trend <- data.frame(stat = c("Trust in media - % Yes",
                                   "Trust in media - % No",
                                   "Trust in media - % DK",
                                   "Trust in media - Base",
                                   "TruMediaExDK - % Yes",
                                   "TruMediaExDK - Base"))

for (year in trend_years) {
  
  year_data <- readRDS(paste0(data_folder, "Final/PCOS ", year, " Final Dataset.RDS")) %>%
    filter(TrustMedia2 != "Refusal")
  
  levels(year_data$TrustMedia2)[levels(year_data$TrustMedia2) == "Tend to trust/trust a great deal"] <- "Trust a great deal/Tend to trust"
  levels(year_data$TrustMedia2)[levels(year_data$TrustMedia2) == "Tend to distrust/distrust greatly"] <- "Tend to distrust/Distrust greatly"
  levels(year_data$TrustMedia2)[levels(year_data$TrustMedia2) == "Don't Know"] <- "Don't know"
  
  
  weight <- if (year == 2020) {
    "W4"
  } else if (year %in% 2012:2016) {
    "weight"
  } else {
    "W3"
  }
  
  media_trend$value <- c(f_return_p(year_data, "TrustMedia2", "Trust a great deal/Tend to trust", weight = weight) * 100,
                         f_return_p(year_data, "TrustMedia2", "Tend to distrust/Distrust greatly", weight = weight) * 100,
                         f_return_p(year_data, "TrustMedia2", "Don't know", weight = weight) * 100,
                         f_return_n(year_data$TrustMedia2),
                         f_return_p(year_data, "TrustMedia2", "Trust a great deal/Tend to trust", weight = weight, dk = FALSE) * 100,
                         f_return_n(year_data$TrustMedia2[year_data$TrustMedia2 != "Don't know"]))
  
  names(media_trend)[names(media_trend) == "value"] <- year
  
}

# Trust Civil Service ###

trend_years <- c(2014, 2016, 2019:2021)

nics_trend <- data.frame(stat = c("Trust Civil Service - % Yes",
                                  "Trust Civil Service - % No",
                                  "Trust Civil Service - % DK",
                                  "Trust Civil Service - Base",
                                  "TruNICSExDK - % Yes",
                                  "TruNICSExDK - Base"))

for (year in trend_years) {
  
  year_data <- readRDS(paste0(data_folder, "Final/PCOS ", year, " Final Dataset.RDS")) %>%
    filter(TrustCivilService2 != "Refusal")
  
  levels(year_data$TrustCivilService2)[levels(year_data$TrustCivilService2) == "Tend to trust/trust a great deal"] <- "Trust a great deal/Tend to trust"
  levels(year_data$TrustCivilService2)[levels(year_data$TrustCivilService2) == "Tend to distrust/distrust greatly"] <- "Tend to distrust/Distrust greatly"
  levels(year_data$TrustCivilService2)[levels(year_data$TrustCivilService2) == "Don't Know"] <- "Don't know"
  
  
  weight <- if (year == 2020) {
    "W4"
  } else if (year %in% 2012:2016) {
    "weight"
  } else {
    "W3"
  }
  
  nics_trend$value <- c(f_return_p(year_data, "TrustCivilService2", "Trust a great deal/Tend to trust", weight = weight) * 100,
                        f_return_p(year_data, "TrustCivilService2", "Tend to distrust/Distrust greatly", weight = weight) * 100,
                        f_return_p(year_data, "TrustCivilService2", "Don't know", weight = weight) * 100,
                        f_return_n(year_data$TrustCivilService2),
                        f_return_p(year_data, "TrustCivilService2", "Trust a great deal/Tend to trust", weight = weight, dk = FALSE) * 100,
                        f_return_n(year_data$TrustCivilService2[year_data$TrustCivilService2 != "Don't know"]))
  
  names(nics_trend)[names(nics_trend) == "value"] <- year
  
}

# Bind data frame and save out RDS ####

weighted_trend <- bind_rows(aware_trend,
                            trust_trend,
                            trust_trend_ex_dk,
                            trust_stats_trend,
                            trust_stats_trend_ex_dk,
                            value_trend,
                            value_trend_ex_dk,
                            interference_trend,
                            interference_trend_ex_dk,
                            confidential_trend,
                            confidential_trend_ex_dk,
                            assembly_trend,
                            media_trend,
                            nics_trend)

saveRDS(weighted_trend, paste0(data_folder, "Trend/2021/weighted trend data.RDS"))
