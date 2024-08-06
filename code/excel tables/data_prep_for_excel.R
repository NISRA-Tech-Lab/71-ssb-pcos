library(here)
source(paste0(here(), "/code/data_prep.R"))

# Awareness of NISRA ####

## Table 1: Awareness of NISRA by year ####

table_1_data <- table_1_data %>%
  mutate(current_year = c(aware_nisra_ons_data$nisra[aware_nisra_ons_data$year == current_year],
                          sum(data_final$W3[data_final$PCOS1 == "No"], na.rm = TRUE)  / sum(data_final$W3[!is.na(data_final$PCOS1)], na.rm = TRUE) * 100,
                          sum(data_final$W3[data_final$PCOS1 == "DontKnow"], na.rm = TRUE)  / sum(data_final$W3[!is.na(data_final$PCOS1)], na.rm = TRUE) * 100,
                          nrow(data_final[!is.na(data_final$PCOS1), ])))

names(table_1_data)[names(table_1_data) == "current_year"] <- current_year

saveRDS(table_1_data, paste0(data_folder, "Trend/", current_year, "/table_1_data.RDS"))

## Table 2: Awareness of NISRA and ONS ####

table_2_data <- table_1_data[c("Response (%)", current_year)] %>%
  mutate(ONS = c(data_ons$Yes[data_ons$`Related Variable` == "PCOS1"],
                 data_ons$No[data_ons$`Related Variable` == "PCOS1"],
                 data_ons$`Don't know`[data_ons$`Related Variable` == "PCOS1"],
                 data_ons$`Unweighted base`[data_ons$`Related Variable` == "PCOS1"]))

names(table_2_data)[names(table_2_data) == current_year] <- "NISRA"

# Awareness of NISRA Statistics by those not previously aware of NISRA ####

## Table 3: Aware of NISRA statistics on the number of deaths in Northern Ireland ####
## Table 4: Aware of NISRA statistics on recorded levels of crime in Northern Ireland ####
## Table 5: Aware of NISRA statistics on the qualifications of school leavers in Northern Ireland ####
## Table 6: Aware of NISRA statistics on the number of people who live in Northern Ireland ####
## Table 7: Aware of NISRA statistics on hospital waiting times in Northern Ireland ####
## Table 8: Aware of NISRA statistics on the Northern Ireland Census every ten years ####
## Table 9: Aware of NISRA statistics on the unemployment rate in Northern Ireland ####
## Table 10: Aware of NISRA statistics on people living in poverty in Northern Ireland ####
## Table 11: Aware of NISRA statistics on percentage of journeys made by walking, cycling or public transport in Northern Ireland ####

for (i in 1:length(outputs)) {
  
  df_name <- paste0("table_", i + 2, "_data")
  
  df <- data.frame(response = c("Yes", "No", "Don't Know", "Number of Respondents"),
                   current_year = c(chart_3_data$yes[gsub("\n", "", chart_3_data$output) == outputs[i]],
                                    chart_3_data$no[gsub("\n", "", chart_3_data$output) == outputs[i]],
                                    chart_3_data$dont_know[gsub("\n", "", chart_3_data$output) == outputs[i]],
                                    nrow(data_final[!is.na(data_final[[paste0("PCOS1d", i)]]), ])))
  
  names(df) <- c("Response (%)", current_year)
  
  assign(df_name, df)
  
}

## Table 12: Number of selected NISRA statistics respondents had heard of (among those who were not previously aware of NISRA) ####

table_12_data <- data_final %>%
  filter(PCOS1 == "No") %>%
  group_by(not_heard_yes_count) %>%
  summarise(count = sum(W3)) %>%
  mutate(current_year = count / sum(count) * 100) %>%
  select(not_heard_yes_count, current_year) %>%
  rbind(data.frame(not_heard_yes_count = "Number of Respondents",
                   current_year = nrow(data_final[data_final$PCOS1 == "No", ])))

names(table_12_data) <- c("Number of NISRA statistics", paste(current_year, "(%)"))
         
# Awareness of NISRA Statistics by those previously aware of NISRA ####

## Table 13: Aware of NISRA statistics on the number of deaths in Northern Ireland ####
## Table 14: Aware of NISRA statistics on recorded levels of crime in Northern Ireland ####
## Table 15: Aware of NISRA statistics on the qualifications of school leavers in Northern Ireland ####
## Table 16: Aware of NISRA statistics on the number of people who live in Northern Ireland ####
## Table 17: Aware of NISRA statistics on hospital waiting times in Northern Ireland ####
## Table 18: Aware of NISRA statistics on the Northern Ireland Census every ten years ####
## Table 19: Aware of NISRA statistics on the unemployment rate in Northern Ireland ####
## Table 20: Aware of NISRA statistics on people living in poverty in Northern Ireland ####
## Table 21: Aware of NISRA statistics on percentage of journeys made by walking, cycling or public transport in Northern Ireland ####

for (i in 1:length(outputs)) {
  
  df_name <- paste0("table_", i + 12, "_data")
  
  df <- data.frame(response = c("Yes", "No", "Don't Know", "Number of Respondents"),
                   current_year = c(chart_4_data$yes[gsub("\n", "", chart_4_data$output) == outputs[i]],
                                    chart_4_data$no[gsub("\n", "", chart_4_data$output) == outputs[i]],
                                    chart_4_data$dont_know[gsub("\n", "", chart_4_data$output) == outputs[i]],
                                    nrow(data_final[!is.na(data_final[[paste0("PCOS1c", i)]]), ])))
  
  names(df) <- c("Response (%)", current_year)
  
  assign(df_name, df)
  
}
 
## Table 22: Number of selected NISRA statistics respondents had heard of (among those who were previously aware of NISRA) ####
 
table_22_data <- data_final %>%
  filter(PCOS1 == "Yes") %>%
  group_by(heard_yes_count) %>%
  summarise(count = sum(W3)) %>%
  mutate(current_year = count / sum(count) * 100) %>%
  select(heard_yes_count, current_year) %>%
  rbind(data.frame(heard_yes_count = "Number of Respondents",
                   current_year = nrow(data_final[data_final$PCOS1 == "Yes", ])))

names(table_22_data) <- c("Number of NISRA statistics", paste(current_year, "(%)"))

# Trust in NISRA ####

## Table 23: Trust in NISRA by year ####

table_23_data <- table_23_data %>%
  mutate(current_year = c(trust_nisra_data$trust[trust_nisra_data$year == current_year],
                          trust_nisra_data$distrust[trust_nisra_data$year == current_year],
                          trust_nisra_data$dont_know[trust_nisra_data$year == current_year],
                          sum(!is.na(data_final$TrustNISRA2))))

names(table_23_data)[names(table_23_data) == "current_year"] <- current_year

saveRDS(table_23_data, paste0(data_folder, "Trend/", current_year, "/table_23_data.RDS"))

## Table 24: Trust in NISRA and ONS
 
table_24_data <- table_23_data[c("Response (%)", current_year)] %>%
  mutate(ONS = c(chart_6_data$trust[grepl("ONS", chart_6_data$org)],
                 chart_6_data$distrust[grepl("ONS", chart_6_data$org)],
                 chart_6_data$dont_know[grepl("ONS", chart_6_data$org)],
                 ons_chart_6$`Unweighted base`))

names(table_24_data)[names(table_24_data) == current_year] <- "NISRA"

## Table 25: Trust in NISRA by respondent's awareness of NISRA ####

table_25_data <- table_24_data["Response (%)"] %>%
  mutate(`Had heard of NISRA` = c(sum(data_final$W3[data_final$TrustNISRA2 == "Trust a great deal/Tend to trust" & data_final$PCOS1 == "Yes"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "Yes"], na.rm = TRUE) * 100,
                                  sum(data_final$W3[data_final$TrustNISRA2 == "Tend to distrust/Distrust greatly" & data_final$PCOS1 == "Yes"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "Yes"], na.rm = TRUE) * 100,
                                  sum(data_final$W3[data_final$TrustNISRA2 == "Don't know" & data_final$PCOS1 == "Yes"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "Yes"], na.rm = TRUE) * 100,
                                  sum(!is.na(data_final$TrustNISRA2) & data_final$PCOS1 == "Yes")),
         `Had not heard of NISRA` = c(sum(data_final$W3[data_final$TrustNISRA2 == "Trust a great deal/Tend to trust" & data_final$PCOS1 == "No"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "No"], na.rm = TRUE) * 100,
                                      sum(data_final$W3[data_final$TrustNISRA2 == "Tend to distrust/Distrust greatly" & data_final$PCOS1 == "No"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "No"], na.rm = TRUE) * 100,
                                      sum(data_final$W3[data_final$TrustNISRA2 == "Don't know" & data_final$PCOS1 == "No"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "No"], na.rm = TRUE) * 100,
                                      sum(!is.na(data_final$TrustNISRA2) & data_final$PCOS1 == "No")))

# Trust Civil Service ####

## Table 26: Trust in Civil Service by year ####

table_26_data <- table_26_data %>%
  mutate(current_year = c(chart_7_data$trust[trimws(chart_7_data$org) == "The Civil Service"],
                          chart_7_data$distrust[trimws(chart_7_data$org) == "The Civil Service"],
                          chart_7_data$dont_know[trimws(chart_7_data$org) == "The Civil Service"],
                          sum(!is.na(data_final$TrustCivilService2))))

names(table_26_data)[names(table_26_data) == "current_year"] <- current_year

saveRDS(table_26_data, paste0(data_folder, "Trend/", current_year, "/table_26_data.RDS"))

## Table 27: Trust in Northern Ireland Assembly by year ####

table_27_data <- table_27_data %>%
  mutate(current_year = c(chart_7_data$trust[trimws(chart_7_data$org) == "The NI Assembly"],
                          chart_7_data$distrust[trimws(chart_7_data$org) == "The NI Assembly"],
                          chart_7_data$dont_know[trimws(chart_7_data$org) == "The NI Assembly"],
                          sum(!is.na(data_final$TrustNIAssembly2))))

names(table_27_data)[names(table_27_data) == "current_year"] <- current_year

saveRDS(table_27_data, paste0(data_folder, "Trend/", current_year, "/table_27_data.RDS"))

## Table 28: Trust in the Media by year ####

table_28_data <- table_28_data %>%
  mutate(current_year = c(chart_7_data$trust[trimws(chart_7_data$org) == "The media"],
                          chart_7_data$distrust[trimws(chart_7_data$org) == "The media"],
                          chart_7_data$dont_know[trimws(chart_7_data$org) == "The media"],
                          sum(!is.na(data_final$TrustMedia2))))

names(table_28_data)[names(table_28_data) == "current_year"] <- current_year

saveRDS(table_28_data, paste0(data_folder, "Trend/", current_year, "/table_28_data.RDS"))

# Trust NISRA Statistics ####

## Table 29: Trust in NISRA statistics by year ####

table_29_data <- table_29_data %>%
  mutate(current_year = c(trust_stats_data$trust[trust_stats_data$year == current_year],
                          trust_stats_data$distrust[trust_stats_data$year == current_year],
                          trust_stats_data$dont_know[trust_stats_data$year == current_year],
                          sum(!is.na(data_final$TrustNISRAstats2))))

names(table_29_data)[names(table_29_data) == "current_year"] <- current_year

saveRDS(table_29_data, paste0(data_folder, "Trend/", current_year, "/table_29_data.RDS"))

## Table 30: Trust in NISRA and ONS statistics ####
 
table_30_data <- table_29_data[c("Response (%)", current_year)] %>%
  mutate(ONS = c(chart_9_data$trust[grepl("ONS", chart_9_data$org)],
                 chart_9_data$distrust[grepl("ONS", chart_9_data$org)],
                 chart_9_data$dont_know[grepl("ONS", chart_9_data$org)],
                 ons_chart_9$`Unweighted base`))

names(table_30_data)[names(table_30_data) == current_year] <- "NISRA"

## Table 31: Trust in NISRA statistics by respondent's awareness of NISRA ####

table_31_data <- table_30_data["Response (%)"] %>%
  mutate(`Had heard of NISRA` = c(sum(data_final$W3[data_final$TrustNISRAstats2 == "Trust a great deal/Tend to trust" & data_final$PCOS1 == "Yes"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "Yes"], na.rm = TRUE) * 100,
                                  sum(data_final$W3[data_final$TrustNISRAstats2 == "Tend to distrust/Distrust greatly" & data_final$PCOS1 == "Yes"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "Yes"], na.rm = TRUE) * 100,
                                  sum(data_final$W3[data_final$TrustNISRAstats2 == "Don't know" & data_final$PCOS1 == "Yes"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "Yes"], na.rm = TRUE) * 100,
                                  sum(!is.na(data_final$TrustNISRAstats2) & data_final$PCOS1 == "Yes")),
         `Had not heard of NISRA` = c(sum(data_final$W3[data_final$TrustNISRAstats2 == "Trust a great deal/Tend to trust" & data_final$PCOS1 == "No"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "No"], na.rm = TRUE) * 100,
                                      sum(data_final$W3[data_final$TrustNISRAstats2 == "Tend to distrust/Distrust greatly" & data_final$PCOS1 == "No"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "No"], na.rm = TRUE) * 100,
                                      sum(data_final$W3[data_final$TrustNISRAstats2 == "Don't know" & data_final$PCOS1 == "No"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "No"], na.rm = TRUE) * 100,
                                      sum(!is.na(data_final$TrustNISRAstats2) & data_final$PCOS1 == "No")))

# Value ####

## Table 32: Statistics produced by NISRA are important to understand Northern Ireland by year ####

table_32_data <- table_32_data %>%
  mutate(current_year = c(stats_important_data$agree[stats_important_data$year == current_year],
                          stats_important_data$disagree[stats_important_data$year == current_year],
                          stats_important_data$dont_know[stats_important_data$year == current_year],
                          sum(!is.na(data_final$NISRAstatsImp2))))

names(table_32_data)[names(table_32_data) == "current_year"] <- current_year

saveRDS(table_32_data, paste0(data_folder, "Trend/", current_year, "/table_32_data.RDS"))

## Table 33: Statistics produced are important to understand our country (NISRA and ONS) ####

table_33_data <- table_32_data[c("Response (%)", current_year)] %>%
  mutate(ONS = c(chart_11_data$agree[grepl("ONS", chart_11_data$org)],
                 chart_11_data$disagree[grepl("ONS", chart_11_data$org)],
                 chart_11_data$dont_know[grepl("ONS", chart_11_data$org)],
                 ons_chart_11$`Unweighted base`))

names(table_33_data)[names(table_33_data) == current_year] <- "NISRA"

## Table 34: Statistics produced by NISRA are important to understand Northern Ireland, by whether or not the respondent had heard of NISRA ####

table_34_data <- table_33_data["Response (%)"] %>%
  mutate(`Had heard of NISRA` = c(sum(data_final$W3[data_final$NISRAstatsImp2 == "Strongly Agree/Tend to Agree" & data_final$PCOS1 == "Yes"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "Yes"], na.rm = TRUE) * 100,
                                  sum(data_final$W3[data_final$NISRAstatsImp2 == "Tend to disagree/Strongly disagree" & data_final$PCOS1 == "Yes"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "Yes"], na.rm = TRUE) * 100,
                                  sum(data_final$W3[data_final$NISRAstatsImp2 == "Don't know" & data_final$PCOS1 == "Yes"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "Yes"], na.rm = TRUE) * 100,
                                  sum(!is.na(data_final$NISRAstatsImp2) & data_final$PCOS1 == "Yes")),
         `Had not heard of NISRA` = c(sum(data_final$W3[data_final$NISRAstatsImp2 == "Strongly Agree/Tend to Agree" & data_final$PCOS1 == "No"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "No"], na.rm = TRUE) * 100,
                                      sum(data_final$W3[data_final$NISRAstatsImp2 == "Tend to disagree/Strongly disagree" & data_final$PCOS1 == "No"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "No"], na.rm = TRUE) * 100,
                                      sum(data_final$W3[data_final$NISRAstatsImp2 == "Don't know" & data_final$PCOS1 == "No"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "No"], na.rm = TRUE) * 100,
                                      sum(!is.na(data_final$NISRAstatsImp2) & data_final$PCOS1 == "No")))

# Political Interference ####

## Table 35: Statistics produced by NISRA are free from political interference by year ####

table_35_data <- table_35_data %>%
  mutate(current_year = c(political_data$agree[political_data$year == current_year],
                          political_data$disagree[political_data$year == current_year],
                          political_data$dont_know[political_data$year == current_year],
                          sum(!is.na(data_final$Political2))))

names(table_35_data)[names(table_35_data) == "current_year"] <- current_year

saveRDS(table_35_data, paste0(data_folder, "Trend/", current_year, "/table_35_data.RDS"))

## Table 36: Statistics produced are free from political interference (NISRA and ONS) ####

table_36_data <- table_35_data[c("Response (%)", current_year)] %>%
  mutate(ONS = c(chart_13_data$agree[grepl("ONS", chart_13_data$org)],
                 chart_13_data$disagree[grepl("ONS", chart_13_data$org)],
                 chart_13_data$dont_know[grepl("ONS", chart_13_data$org)],
                 ons_chart_13$`Unweighted base`))

names(table_36_data)[names(table_36_data) == current_year] <- "NISRA"

# Confidentiality ####

## Table 37: Personal information provided to NISRA will be kept confidential ####

table_37_data <- table_37_data %>%
  mutate(current_year = c(confidential_data$agree[confidential_data$year == current_year],
                          confidential_data$disagree[confidential_data$year == current_year],
                          confidential_data$dont_know[confidential_data$year == current_year],
                          sum(!is.na(data_final$Confidential2))))

names(table_37_data)[names(table_37_data) == "current_year"] <- current_year

saveRDS(table_37_data, paste0(data_folder, "Trend/", current_year, "/table_37_data.RDS"))

## Table 38: Personal information provided will be kept confidential (NISRA and ONS) ####

table_38_data <- table_37_data[c("Response (%)", current_year)] %>%
  mutate(ONS = c(chart_15_data$agree[grepl("ONS", chart_15_data$org)],
                 chart_15_data$disagree[grepl("ONS", chart_15_data$org)],
                 chart_15_data$dont_know[grepl("ONS", chart_15_data$org)],
                 ons_chart_15$`Unweighted base`))

