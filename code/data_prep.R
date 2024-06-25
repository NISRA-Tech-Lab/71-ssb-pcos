# Set folder path
library(here)

# Read config file
source(paste0(here(), "/code/config.R"))

#### Read data in ####
data_final_from_spss <- readspss::read.spss(paste0(data_folder, "Final/PCOS 2022 Final Dataset (14 July 2023).sav"), pass = password)
data_raw <- readspss::read.spss(paste0(data_folder, "Raw/2223_PCOS_FINAL_WEIGHTED_PASSWORDED.sav"), pass = raw_password)

#### Recode variables #####

# Recode age variable and DERHI
data_final <- data_raw %>% 
  mutate(AGE2 = as.factor(case_when(AGE <= 24 ~ "16-24",
                                    AGE <= 34 ~ "25-34",
                                    AGE <= 44 ~ "35-44",
                                    AGE <= 54 ~ "45-54",
                                    AGE <= 64 ~ "55-64",
                                    AGE <= 74 ~ "65-74",
                                    TRUE ~ "75 and over")),
         AGE2a = as.factor(case_when(AGE <= 15 ~ "0-15",
                                     AGE <= 29 ~ "20-29",
                                     AGE <= 39 ~ "30-39",
                                     AGE <= 49 ~ "40-49",
                                     AGE <= 59 ~ "59",
                                     TRUE ~ "60+")),
         DERHIanalysis = case_when(DERHI == "Other qualifications" ~ NA,
                                   TRUE ~ DERHI),
         remove = FALSE) %>%  ## Added for later
  relocate("AGE2a", .after = "Age1") %>%
  relocate("AGE2", .after = "W3")

# Loop to recode all "Trust" based answers
trust_q_old <- c("PCOS2a", "PCOS2b", "PCOS2c", "PCOS2d", "PCOS3")
trust_q_new <- c("TrustCivilService2", "TrustNIAssembly2", "TrustMedia2", "TrustNISRA2", "TrustNISRAstats2")

for (i in 1:length(trust_q_old)) {
  
  data_final[[trust_q_new[i]]] <- case_when(data_final[[trust_q_old[i]]] %in% c("Trust a great deal", "Tend to trust", "Trust them greatly", "Tend not to trust them") ~ "Trust a great deal/Tend to trust",
                                            data_final[[trust_q_old[i]]] %in% c("Tend to distrust", "Distrust greatly", "Tend not to trust them", "Distrust them greatly") ~ "Tend to distrust/Distrust greatly",
                                            TRUE ~ data_final[[trust_q_old[i]]]) %>%
    as.factor()
  
}

# Loop to recode all "agree" based answers

agree_q_old <- c("PCOS4", "PCOS5", "PCOS6")
agree_q_new <- c("NISRAstatsImp2", "Political2" , "Confidential2")

for (i in 1:length(agree_q_old)) {
  
  data_final[[agree_q_new[i]]] <- case_when(data_final[[agree_q_old[i]]] %in% c("Strongly agree", "Tend to agree") ~ "Strongly Agree/Tend to Agree ",
                                            data_final[[agree_q_old[i]]] %in% c("Tend to disagree", "Strongly disagree") ~ "Tend to disagree/Strongly disagree",
                                            TRUE ~ data_final[[agree_q_old[i]]]) %>%
    as.factor()

}

# Recode Refusals to Missing

vars_to_recode_to_missing <- names(data_final)[c(which(names(data_final) == "PCOS1"):which(names(data_final) == "PCOS1d9"),
                                                 which(names(data_final) == "TrustCivilService2"):which(names(data_final) == "Confidential2"))]

for (i in 1:length(vars_to_recode_to_missing)) {
  
  data_final[[vars_to_recode_to_missing[i]]] <- case_when(data_final[[vars_to_recode_to_missing[i]]] == "Refusal" ~ NA,
                                                          TRUE ~ data_final[[vars_to_recode_to_missing[i]]])
  
}

# Check all above variables for NA across all Q's

for (i in 1:nrow(data_final)) {
  for (j in 1:length(vars_to_recode_to_missing)) {
    if (is.na(data_final[[vars_to_recode_to_missing[j]]][i])) {
      data_final$remove[i] <- TRUE
    } else {
      data_final$remove[i] <- FALSE
      break
    }
  }
}

# Tidy up data
data_final <- data_final %>%
  filter(!remove) %>%
  select(-remove) %>%
  relocate("DERHIanalysis", .after = "Confidential2")

saveRDS(data_final, paste0(data_folder, "Final/PCOS ", current_year," Final Dataset.RDS"))

# Add dfs for tables and charts ####

## Read in all last year's trend data ####
for (file in list.files((paste0(data_folder, "Trend/", current_year - 1)))) {
  assign(sub(".RDS", "", file),
         readRDS(paste0(data_folder, "Trend/", current_year - 1, "/", file)))
}

# Check Trend folder for new exists
if (!exists(paste0(data_folder, "Trend/", current_year))) {
  dir.create(paste0(data_folder, "Trend/", current_year))
}

## Chart 1: Awareness of NISRA by year ####

chart_1_data <- chart_1_data %>%
  rbind(data.frame(year = current_year,
                   pct = sum(data_final$W3[data_final$PCOS1 == "Yes"]) / sum(data_final$W3) * 100))

saveRDS(chart_1_data, paste0(data_folder, "Trend/", current_year, "/chart_1_data.RDS"))

## Chart 2: Awareness of NISRA and ONS by year ####

chart_2_data <- chart_2_data %>%
  rbind(data.frame(year = current_year,
                   nisra = sum(data_final$W3[data_final$PCOS1 == "Yes"]) / sum(data_final$W3) * 100,
                   ons = NA))

saveRDS(chart_2_data, paste0(data_folder, "Trend/", current_year, "/chart_2_data.RDS"))

## Chart 3: Awareness of specific NISRA statistics for respondents who were not aware of NISRA ####

outputs <- c("The number of deaths in NI",
             "Recorded levels of crime in NI",
             "Qualifications of school leavers in NI",
             "The number of people who live in NI",
             "Statistics on hospital waiting times in NI",
             "The NI Census every ten years",
             "The unemployment rate in NI",
             "People living in poverty in NI",
             "Percentage of journeys by walking, cycling, public transport in NI")

PCOS1d_vars <- names(data_final)[grepl("PCOS1d", names(data_final)) & names(data_final) != "PCOS1d"]

chart_3_data <- data.frame(output = character(),
                           yes = numeric(),
                           no = numeric(),
                           dunno = numeric())

for (i in 1:length(outputs)) {
  chart_3_data <- chart_3_data %>%
    rbind(data.frame(output = outputs[i],
                     yes = sum(data_final$W3[data_final[[PCOS1d_vars[i]]] == "Yes"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final[[PCOS1d_vars[i]]])]) * 100,
                     no = sum(data_final$W3[data_final[[PCOS1d_vars[i]]] == "No"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final[[PCOS1d_vars[i]]])]) * 100,
                     dunno = sum(data_final$W3[data_final[[PCOS1d_vars[i]]] == "DontKnow"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final[[PCOS1d_vars[i]]])]) * 100))
}

chart_3_data <- chart_3_data %>%
  arrange(yes)

sort_order <- chart_3_data %>%
  mutate(order = as.numeric(rownames(.))) %>%
  select(output, order)

## Chart 4: Awareness of specific NISRA statistics for respondents who were aware of NISRA ####

PCOS1c_vars <- names(data_final)[grepl("PCOS1c", names(data_final)) & names(data_final) != "PCOS1c"]

chart_4_data <- data.frame(output = character(),
                           yes = numeric(),
                           no = numeric(),
                           dunno = numeric())

for (i in 1:length(outputs)) {
  chart_4_data <- chart_4_data %>%
    rbind(data.frame(output = outputs[i],
                     yes = sum(data_final$W3[data_final[[PCOS1c_vars[i]]] == "Yes"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final[[PCOS1c_vars[i]]])]) * 100,
                     no = sum(data_final$W3[data_final[[PCOS1c_vars[i]]] == "No"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final[[PCOS1c_vars[i]]])]) * 100,
                     dunno = sum(data_final$W3[data_final[[PCOS1c_vars[i]]] == "DontKnow"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final[[PCOS1c_vars[i]]])]) * 100))
}

chart_4_data <- chart_4_data %>%
  left_join(sort_order) %>%
  arrange(order) %>%
  select(-order)

## Chart 5: Trust in NISRA by year

chart_5_data <- chart_5_data %>%
  rbind(data.frame(year = current_year,
                   trust = sum(data_final$W3[data_final$TrustNISRA2 == "Trust a great deal/Tend to trust"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$TrustNISRA2)]),
                   distrust = sum(data_final$W3[data_final$TrustNISRA2 == "Tend to distrust/Distrust greatly"], na.rm = TRUE)  / sum(data_final$W3[!is.na(data_final$TrustNISRA2)]),
                   dunno = sum(data_final$W3[data_final$TrustNISRA2 == "DontKnow"], na.rm = TRUE)  / sum(data_final$W3[!is.na(data_final$TrustNISRA2)])))

saveRDS(chart_5_data, paste0(data_folder, "Trend/", current_year, "/chart_5_data.RDS"))

## Chart 6: Trust in NISRA and ONS as institutions

 
