library(here)
source(paste0(here(), "/code/data_prep.R"))

# Awareness of NISRA ####

## Table 1a: Awareness of NISRA by year ####

table_1a_data <- table_1a_data %>%
  mutate(current_year = c(aware_nisra_ons_data$nisra[aware_nisra_ons_data$year == current_year],
                          sum(data_final$W3[data_final$PCOS1 == "No"], na.rm = TRUE)  / sum(data_final$W3[!is.na(data_final$PCOS1)], na.rm = TRUE) * 100,
                          sum(data_final$W3[data_final$PCOS1 == "DontKnow"], na.rm = TRUE)  / sum(data_final$W3[!is.na(data_final$PCOS1)], na.rm = TRUE) * 100,
                          nrow(data_final[!is.na(data_final$PCOS1), ])))

names(table_1a_data)[names(table_1a_data) == "current_year"] <- current_year

saveRDS(table_1a_data, paste0(data_folder, "Trend/", current_year, "/table_1a_data.RDS"))

## Table 1b: Awareness of NISRA and ONS ####

table_1b_data <- table_1a_data[c("Response (%)", current_year)] %>%
  mutate(ONS = c(data_ons$Yes[data_ons$`Related Variable` == "PCOS1"],
                 data_ons$No[data_ons$`Related Variable` == "PCOS1"],
                 data_ons$`Don't know`[data_ons$`Related Variable` == "PCOS1"],
                 data_ons$`Unweighted base`[data_ons$`Related Variable` == "PCOS1"]))

names(table_1b_data)[names(table_1b_data) == current_year] <- "NISRA"

## Table 1c: Awareness of ONS by year ####

if (!ons_year %in% names(table_1c_data)) {

  table_1c_data <- table_1c_data %>%
    mutate(ONS = table_1b_data$ONS)
  
  names(table_1c_data)[names(table_1c_data) == "ONS"] <- ons_year

}

saveRDS(table_1c_data, paste0(data_folder, "Trend/", current_year, "/table_1c_data.RDS"))

# Awareness of NISRA Statistics by those not previously aware of NISRA ####

## Table 2.1a: Aware of NISRA statistics on the number of deaths in Northern Ireland ####
## Table 2.1b: Aware of NISRA statistics on recorded levels of crime in Northern Ireland ####
## Table 2.1c: Aware of NISRA statistics on the qualifications of school leavers in Northern Ireland ####
## Table 2.1d: Aware of NISRA statistics on the number of people who live in Northern Ireland ####
## Table 2.1e: Aware of NISRA statistics on hospital waiting times in Northern Ireland ####
## Table 2.1f: Aware of NISRA statistics on the Northern Ireland Census every ten years ####
## Table 2.1g: Aware of NISRA statistics on the unemployment rate in Northern Ireland ####
## Table 2.1h: Aware of NISRA statistics on people living in poverty in Northern Ireland ####
## Table 2.1i: Aware of NISRA statistics on percentage of journeys made by walking, cycling or public transport in Northern Ireland ####

for (i in 1:length(outputs)) {
  
  df_name <- paste0("table_2.1", letters[i],"_data")
  
  df <- data.frame(response = c("Yes", "No", "Don't Know", "Number of Respondents"),
                   current_year = c(aware_stats_data$yes[gsub("\n", "", aware_stats_data$output) == outputs[i]],
                                    aware_stats_data$no[gsub("\n", "", aware_stats_data$output) == outputs[i]],
                                    aware_stats_data$dont_know[gsub("\n", "", aware_stats_data$output) == outputs[i]],
                                    nrow(data_final[!is.na(data_final[[paste0("PCOS1d", i)]]), ])))
  
  names(df) <- c("Response (%)", current_year)
  
  assign(df_name, df)
  
}

## Table 2.1j: Number of selected NISRA statistics respondents had heard of (among those who were not previously aware of NISRA) ####

table_2.1j_data <- data_final %>%
  filter(PCOS1 == "No") %>%
  group_by(not_heard_yes_count) %>%
  summarise(count = sum(W3)) %>%
  mutate(current_year = count / sum(count) * 100) %>%
  select(not_heard_yes_count, current_year) %>%
  rbind(data.frame(not_heard_yes_count = "Number of Respondents",
                   current_year = nrow(data_final[!is.na(data_final$PCOS1d1), ])))

names(table_2.1j_data) <- c("Number of NISRA statistics", paste(current_year, "(%)"))
         
# Awareness of NISRA Statistics by those previously aware of NISRA ####

## Table 2.2a: Aware that statistics on the number of deaths in Northern Ireland are produced by NISRA statisticians ####
## Table 2.2b: Aware that statistics on recorded levels of crime in Northern Ireland are produced by NISRA statisticians ####
## Table 2.2c: Aware that statistics on the qualifications of school leavers in Northern Ireland are produced by NISRA statisticians ####
## Table 2.2d: Aware that statistics on the number of people who live in Northern Ireland are produced by NISRA statisticians ####
## Table 2.2e: Aware that statistics on hospital waiting times in Northern Ireland are produced by NISRA statisticians ####
## Table 2.2f: Aware that statistics on the Northern Ireland Census every ten years are produced by NISRA statisticians ####
## Table 2.2g: Aware that statistics on the unemployment rate in Northern Ireland are produced by NISRA statisticians ####
## Table 2.2h: Aware that statistics on people living in poverty in Northern Ireland are produced by NISRA statisticians ####
## Table 2.2i: Aware that statistics on percentage of journeys made by walking, cycling or public transport in Northern Ireland are produced by NISRA statisticians ####

for (i in 1:length(outputs)) {
  
  df_name <- paste0("table_2.2", letters[i], "_data")
  
  df <- data.frame(response = c("Yes", "No", "Don't Know", "Number of Respondents"),
                   current_year = c(aware_stats_by_nisra_data$yes[gsub("\n", "", aware_stats_by_nisra_data$output) == outputs[i]],
                                    aware_stats_by_nisra_data$no[gsub("\n", "", aware_stats_by_nisra_data$output) == outputs[i]],
                                    aware_stats_by_nisra_data$dont_know[gsub("\n", "", aware_stats_by_nisra_data$output) == outputs[i]],
                                    nrow(data_final[!is.na(data_final[[paste0("PCOS1c", i)]]), ])))
  
  names(df) <- c("Response (%)", current_year)
  
  assign(df_name, df)
  
}
 
## Table 2.2j: Number of selected NISRA statistics respondents had heard of (among those who were previously aware of NISRA) ####
 
table_2.2j_data <- data_final %>%
  filter(PCOS1 == "Yes") %>%
  group_by(heard_yes_count) %>%
  summarise(count = sum(W3)) %>%
  mutate(current_year = count / sum(count) * 100) %>%
  select(heard_yes_count, current_year) %>%
  rbind(data.frame(heard_yes_count = "Number of Respondents",
                   current_year = nrow(data_final[!is.na(data_final$PCOS1c1), ])))

names(table_2.2j_data) <- c("Number of NISRA statistics", paste(current_year, "(%)"))

# Trust in NISRA ####

## Table 3.1a: Trust in NISRA by year ####

table_3.1a_data <- table_3.1a_data %>%
  mutate(current_year = c(trust_nisra_data$trust[trust_nisra_data$year == current_year],
                          trust_nisra_data$distrust[trust_nisra_data$year == current_year],
                          trust_nisra_data$dont_know[trust_nisra_data$year == current_year],
                          sum(!is.na(data_final$TrustNISRA2))))

names(table_3.1a_data)[names(table_3.1a_data) == "current_year"] <- current_year

saveRDS(table_3.1a_data, paste0(data_folder, "Trend/", current_year, "/table_3.1a_data.RDS"))

## Table 3.1b: Trust in NISRA and ONS
 
table_3.1b_data <- table_3.1a_data[c("Response (%)", current_year)] %>%
  mutate(ONS = c(trust_nisra_ons_data$trust[grepl("ONS", trust_nisra_ons_data$org)],
                 trust_nisra_ons_data$distrust[grepl("ONS", trust_nisra_ons_data$org)],
                 trust_nisra_ons_data$dont_know[grepl("ONS", trust_nisra_ons_data$org)],
                 trust_ons_data$`Unweighted base`))

names(table_3.1b_data)[names(table_3.1b_data) == current_year] <- "NISRA"

## Table 3.1c: Trust in NISRA by respondent's awareness of NISRA ####

table_3.1c_data <- table_3.1b_data["Response (%)"] %>%
  mutate(`Had heard of NISRA` = c(sum(data_final$W3[data_final$TrustNISRA2 == "Trust a great deal/Tend to trust" & data_final$PCOS1 == "Yes"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "Yes"], na.rm = TRUE) * 100,
                                  sum(data_final$W3[data_final$TrustNISRA2 == "Tend to distrust/Distrust greatly" & data_final$PCOS1 == "Yes"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "Yes"], na.rm = TRUE) * 100,
                                  sum(data_final$W3[data_final$TrustNISRA2 == "Don't know" & data_final$PCOS1 == "Yes"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "Yes"], na.rm = TRUE) * 100,
                                  sum(!is.na(data_final$TrustNISRA2) & data_final$PCOS1 == "Yes")),
         `Had not heard of NISRA` = c(sum(data_final$W3[data_final$TrustNISRA2 == "Trust a great deal/Tend to trust" & data_final$PCOS1 == "No"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "No"], na.rm = TRUE) * 100,
                                      sum(data_final$W3[data_final$TrustNISRA2 == "Tend to distrust/Distrust greatly" & data_final$PCOS1 == "No"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "No"], na.rm = TRUE) * 100,
                                      sum(data_final$W3[data_final$TrustNISRA2 == "Don't know" & data_final$PCOS1 == "No"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "No"], na.rm = TRUE) * 100,
                                      sum(!is.na(data_final$TrustNISRA2) & data_final$PCOS1 == "No")))

# Trust Civil Service ####

## Table 3.2: Trust in Civil Service by year ####

table_3.2_data <- table_3.2_data %>%
  mutate(current_year = c(trust_institutions_data$trust[trimws(trust_institutions_data$org) == "The Civil Service"],
                          trust_institutions_data$distrust[trimws(trust_institutions_data$org) == "The Civil Service"],
                          trust_institutions_data$dont_know[trimws(trust_institutions_data$org) == "The Civil Service"],
                          sum(!is.na(data_final$TrustCivilService2))))

names(table_3.2_data)[names(table_3.2_data) == "current_year"] <- current_year

saveRDS(table_3.2_data, paste0(data_folder, "Trend/", current_year, "/table_3.2_data.RDS"))

## Table 3.3: Trust in Northern Ireland Assembly by year ####

table_3.3_data <- table_3.3_data %>%
  mutate(current_year = c(trust_institutions_data$trust[trimws(trust_institutions_data$org) == "The NI Assembly"],
                          trust_institutions_data$distrust[trimws(trust_institutions_data$org) == "The NI Assembly"],
                          trust_institutions_data$dont_know[trimws(trust_institutions_data$org) == "The NI Assembly"],
                          sum(!is.na(data_final$TrustNIAssembly2))))

names(table_3.3_data)[names(table_3.3_data) == "current_year"] <- current_year

saveRDS(table_3.3_data, paste0(data_folder, "Trend/", current_year, "/table_3.3_data.RDS"))

## Table 3.4: Trust in the Media by year ####

table_3.4_data <- table_3.4_data %>%
  mutate(current_year = c(trust_institutions_data$trust[trimws(trust_institutions_data$org) == "The media"],
                          trust_institutions_data$distrust[trimws(trust_institutions_data$org) == "The media"],
                          trust_institutions_data$dont_know[trimws(trust_institutions_data$org) == "The media"],
                          sum(!is.na(data_final$TrustMedia2))))

names(table_3.4_data)[names(table_3.4_data) == "current_year"] <- current_year

saveRDS(table_3.4_data, paste0(data_folder, "Trend/", current_year, "/table_3.4_data.RDS"))

# Trust NISRA Statistics ####

## Table 4a: Trust in NISRA statistics by year ####

table_4a_data <- table_4a_data %>%
  mutate(current_year = c(trust_stats_data$trust[trust_stats_data$year == current_year],
                          trust_stats_data$distrust[trust_stats_data$year == current_year],
                          trust_stats_data$dont_know[trust_stats_data$year == current_year],
                          sum(!is.na(data_final$TrustNISRAstats2))))

names(table_4a_data)[names(table_4a_data) == "current_year"] <- current_year

saveRDS(table_4a_data, paste0(data_folder, "Trend/", current_year, "/table_4a_data.RDS"))

## Table 4b: Trust in NISRA and ONS statistics ####
 
table_4b_data <- table_4a_data[c("Response (%)", current_year)] %>%
  mutate(ONS = c(trust_stats_nisra_ons_data$trust[grepl("ONS", trust_stats_nisra_ons_data$org)],
                 trust_stats_nisra_ons_data$distrust[grepl("ONS", trust_stats_nisra_ons_data$org)],
                 trust_stats_nisra_ons_data$dont_know[grepl("ONS", trust_stats_nisra_ons_data$org)],
                 trust_ons_stats_data$`Unweighted base`))

names(table_4b_data)[names(table_4b_data) == current_year] <- "NISRA"

## Table 31: Trust in NISRA statistics by respondent's awareness of NISRA ####

table_4c_data <- table_4b_data["Response (%)"] %>%
  mutate(`Had heard of NISRA` = c(sum(data_final$W3[data_final$TrustNISRAstats2 == "Trust a great deal/Tend to trust" & data_final$PCOS1 == "Yes"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "Yes"], na.rm = TRUE) * 100,
                                  sum(data_final$W3[data_final$TrustNISRAstats2 == "Tend to distrust/Distrust greatly" & data_final$PCOS1 == "Yes"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "Yes"], na.rm = TRUE) * 100,
                                  sum(data_final$W3[data_final$TrustNISRAstats2 == "Don't know" & data_final$PCOS1 == "Yes"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "Yes"], na.rm = TRUE) * 100,
                                  sum(!is.na(data_final$TrustNISRAstats2) & data_final$PCOS1 == "Yes")),
         `Had not heard of NISRA` = c(sum(data_final$W3[data_final$TrustNISRAstats2 == "Trust a great deal/Tend to trust" & data_final$PCOS1 == "No"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "No"], na.rm = TRUE) * 100,
                                      sum(data_final$W3[data_final$TrustNISRAstats2 == "Tend to distrust/Distrust greatly" & data_final$PCOS1 == "No"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "No"], na.rm = TRUE) * 100,
                                      sum(data_final$W3[data_final$TrustNISRAstats2 == "Don't know" & data_final$PCOS1 == "No"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "No"], na.rm = TRUE) * 100,
                                      sum(!is.na(data_final$TrustNISRAstats2) & data_final$PCOS1 == "No")))

## Table 4d: Trust ONS Statistics by year ####

if (!ons_year %in% names(table_4d_data)) {
  
  table_4d_data <- table_4d_data %>%
    mutate(ONS = table_4b_data$ONS)
  
  names(table_4d_data)[names(table_4d_data) == "ONS"] <- ons_year
  
}

saveRDS(table_4d_data, paste0(data_folder, "Trend/", current_year, "/table_4d_data.RDS"))

# Value ####

## Table 32: Statistics produced by NISRA are important to understand Northern Ireland by year ####

table_5a_data <- table_5a_data %>%
  mutate(current_year = c(stats_important_data$agree[stats_important_data$year == current_year],
                          stats_important_data$disagree[stats_important_data$year == current_year],
                          stats_important_data$dont_know[stats_important_data$year == current_year],
                          sum(!is.na(data_final$NISRAstatsImp2))))

names(table_5a_data)[names(table_5a_data) == "current_year"] <- current_year

saveRDS(table_5a_data, paste0(data_folder, "Trend/", current_year, "/table_5a_data.RDS"))

## Table 33: Statistics produced are important to understand our country (NISRA and ONS) ####

table_33_data <- table_5a_data[c("Response (%)", current_year)] %>%
  mutate(ONS = c(stats_important_nisra_ons_data$agree[grepl("ONS", stats_important_nisra_ons_data$org)],
                 stats_important_nisra_ons_data$disagree[grepl("ONS", stats_important_nisra_ons_data$org)],
                 stats_important_nisra_ons_data$dont_know[grepl("ONS", stats_important_nisra_ons_data$org)],
                 stats_important_ons_data$`Unweighted base`))

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

table_6a_data <- table_6a_data %>%
  mutate(current_year = c(political_data$agree[political_data$year == current_year],
                          political_data$disagree[political_data$year == current_year],
                          political_data$dont_know[political_data$year == current_year],
                          sum(!is.na(data_final$Political2))))

names(table_6a_data)[names(table_6a_data) == "current_year"] <- current_year

saveRDS(table_6a_data, paste0(data_folder, "Trend/", current_year, "/table_6a_data.RDS"))

## Table 36: Statistics produced are free from political interference (NISRA and ONS) ####

table_36_data <- table_6a_data[c("Response (%)", current_year)] %>%
  mutate(ONS = c(political_nisra_ons_data$agree[grepl("ONS", political_nisra_ons_data$org)],
                 political_nisra_ons_data$disagree[grepl("ONS", political_nisra_ons_data$org)],
                 political_nisra_ons_data$dont_know[grepl("ONS", political_nisra_ons_data$org)],
                 political_ons_data$`Unweighted base`))

names(table_36_data)[names(table_36_data) == current_year] <- "NISRA"

# Confidentiality ####

## Table 37: Personal information provided to NISRA will be kept confidential ####

table_7a_data <- table_7a_data %>%
  mutate(current_year = c(confidential_data$agree[confidential_data$year == current_year],
                          confidential_data$disagree[confidential_data$year == current_year],
                          confidential_data$dont_know[confidential_data$year == current_year],
                          sum(!is.na(data_final$Confidential2))))

names(table_7a_data)[names(table_7a_data) == "current_year"] <- current_year

saveRDS(table_7a_data, paste0(data_folder, "Trend/", current_year, "/table_7a_data.RDS"))

## Table 38: Personal information provided will be kept confidential (NISRA and ONS) ####

table_38_data <- table_7a_data[c("Response (%)", current_year)] %>%
  mutate(ONS = c(condifential_nisra_ons_data$agree[grepl("ONS", condifential_nisra_ons_data$org)],
                 condifential_nisra_ons_data$disagree[grepl("ONS", condifential_nisra_ons_data$org)],
                 condifential_nisra_ons_data$dont_know[grepl("ONS", condifential_nisra_ons_data$org)],
                 condifential_ons_data$`Unweighted base`))

