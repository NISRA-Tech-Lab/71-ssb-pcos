# Set folder path
library(here)

# Read config file
source(paste0(here(), "/code/config.R"))

# Old and new names for all "trust" based answers
trust_q_old <- c("PCOS2a", "PCOS2b", "PCOS2c", "PCOS2d", "PCOS3")
trust_q_new <- c("TrustCivilService2", "TrustNIAssembly2", "TrustMedia2", "TrustNISRA2", "TrustNISRAstats2")

# Old and new names for all "agree" based answers
agree_q_old <- c("PCOS4", "PCOS5", "PCOS6")
agree_q_new <- c("NISRAstatsImp2", "Political2" , "Confidential2")

# Read data in from SPSS ####

data_raw <- readspss::read.spss(paste0(data_folder, "Raw/", data_filename),
                                pass = password,
                                use.missings = FALSE)

source(paste0(here(), "/code/check_raw_variables.R"))

# Read in ONS data from Excel ####
data_ons_raw <- read.xlsx(paste0(data_folder, "ONS/", ons_filename), sheet = "weighted_pct") %>%
  filter(Year == ons_year)

names(data_ons_raw) <- gsub(".", " ", names(data_ons_raw), fixed = TRUE)

unweighted_ons <- read.xlsx(paste0(data_folder, "ONS/", ons_filename), sheet = "unweighted_n") %>%
  filter(Year == ons_year)

names(unweighted_ons) <- gsub(".", " ", names(unweighted_ons), fixed = TRUE)

unweighted_ons_base <- unweighted_ons %>%
  mutate(`Unweighted base` = `Unweighted base` - `Prefer not to answer`) %>%
  select(`Related Variable`, `Unweighted base`)

data_ons <- data_ons_raw %>%
  mutate(`Weighted base` = 100 - `Prefer not to answer`,
         across(.cols = `Don't know`:`Strongly disagree`, ~ .x / `Weighted base` * 100)) %>%
  select(-`Prefer not to answer`, -`Weighted base`) %>%
  left_join(unweighted_ons_base, by = "Related Variable")

# Recode variables #####

## Recode age variable and DERHI ####
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
         remove = FALSE,
         AwareNISRA2 = PCOS1) %>%  ## Added for later
  relocate("AGE2a", .after = "Age1") %>%
  relocate("AGE2", .after = "W3")

## Loop to recode all "Trust" based answers ####

for (i in 1:length(trust_q_old)) {
  
  data_final[[trust_q_new[i]]] <- case_when(data_final[[trust_q_old[i]]] %in% c("Trust a great deal", "Tend to trust", "Trust them greatly", "Tend to trust them") ~ "Trust a great deal/Tend to trust",
                                            data_final[[trust_q_old[i]]] %in% c("Tend to distrust", "Distrust greatly", "Tend not to trust them", "Distrust them greatly") ~ "Tend to distrust/Distrust greatly",
                                            data_final[[trust_q_old[i]]] == "DontKnow" ~ "Don't know",
                                            TRUE ~ data_final[[trust_q_old[i]]]) %>%
    as.factor()
  
}

## Loop to recode all "agree" based answers ####

for (i in 1:length(agree_q_old)) {
  
  data_final[[agree_q_new[i]]] <- case_when(data_final[[agree_q_old[i]]] %in% c("Strongly agree", "Tend to agree") ~ "Strongly Agree/Tend to Agree",
                                            data_final[[agree_q_old[i]]] %in% c("Tend to disagree", "Strongly disagree") ~ "Tend to disagree/Strongly disagree",
                                            data_final[[agree_q_old[i]]] == "DontKnow" ~ "Don't know",
                                            TRUE ~ data_final[[agree_q_old[i]]]) %>%
    as.factor()

}

## Recode Refusals to Missing ####

vars_to_recode_to_missing <- names(data_final)[c(which(names(data_final) == "PCOS1"):which(names(data_final) == "PCOS1d9"),
                                                 which(names(data_final) == "TrustCivilService2"):which(names(data_final) == "Confidential2"))]

for (i in 1:length(vars_to_recode_to_missing)) {
  
  data_final[[vars_to_recode_to_missing[i]]] <- case_when(data_final[[vars_to_recode_to_missing[i]]] == "Refusal" ~ NA,
                                                          TRUE ~ data_final[[vars_to_recode_to_missing[i]]])
  
}

## Check all above variables for Missing across all Q's ####

for (i in 1:nrow(data_final)) {
  for (j in 1:length(vars_to_recode_to_missing)) {
    if (is.na(data_final[[vars_to_recode_to_missing[j]]][i])) {
      data_final$remove[i] <- TRUE
    } else {
      if (data_final[[vars_to_recode_to_missing[j]]][i] %in% c("DontKnow", "Don't know")) {
        data_final$remove[i] <- TRUE
      } else {
        data_final$remove[i] <- FALSE
        break
      }
    }
  }
}
  
## Tidy up data ####
data_final <- data_final %>%
  filter(!remove) %>%
  select(-remove) %>%
  relocate("DERHIanalysis", .after = "Confidential2")

saveRDS(data_final, paste0(data_folder, "Final/PCOS ", current_year," Final Dataset.RDS"))

## Check created variables against originals ####
source(paste0(here(), "/code/check_created_variables.R"))

# Create data frames for charts ####

# Check Trend folder for 2021 exists
if (!dir.exists(paste0(data_folder, "Trend/2021"))) {
  dir.create(paste0(data_folder, "Trend/2021"))
  source(paste0(here(), "/code/trend_data_for_charts.R"))
  source(paste0(here(), "/code/excel tables/excel_trend_data.R"))
}

## Read in all last year's trend data ####
for (file in list.files((paste0(data_folder, "Trend/", current_year - 1)))) {
  assign(sub(".RDS", "", file),
         readRDS(paste0(data_folder, "Trend/", current_year - 1, "/", file)))
}

# Check Trend folder for new year exists
if (!dir.exists(paste0(data_folder, "Trend/", current_year))) {
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
                   ons = if (current_year == ons_year) data_ons$Yes[data_ons$`Related Variable` == "PCOS1"] else NA))

saveRDS(chart_2_data, paste0(data_folder, "Trend/", current_year, "/chart_2_data.RDS"))

chart_2_data <- chart_2_data %>%
  mutate(year = as.character(year))

## Chart 3: Awareness of specific NISRA statistics for respondents who were not aware of NISRA ####

# List of all PCOS1d variables
PCOS1d_vars <- names(data_final)[grepl("PCOS1d", names(data_final)) & names(data_final) != "PCOS1d"]

# Use variable label to extract output name
outputs <- sub("\\..*", "", attributes(data_final)$var.label[grepl("Heard", attributes(data_final)$var.label)]) %>%
  sub("transport in NI", "transport", .) %>%
  trimws()

chart_3_data <- data.frame(output = character(),
                           yes = numeric(),
                           no = numeric(),
                           dont_know = numeric())

for (i in 1:length(outputs)) {
  chart_3_data <- chart_3_data %>%
    rbind(data.frame(output = f_wrap_labels(outputs[i], 38),
                     yes = sum(data_final$W3[data_final[[PCOS1d_vars[i]]] == "Yes"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final[[PCOS1d_vars[i]]])]) * 100,
                     no = sum(data_final$W3[data_final[[PCOS1d_vars[i]]] == "No"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final[[PCOS1d_vars[i]]])]) * 100,
                     dont_know = sum(data_final$W3[data_final[[PCOS1d_vars[i]]] == "DontKnow"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final[[PCOS1d_vars[i]]])]) * 100))
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
                           dont_know = numeric())

for (i in 1:length(outputs)) {
  chart_4_data <- chart_4_data %>%
    rbind(data.frame(output = f_wrap_labels(outputs[i], 38),
                     yes = sum(data_final$W3[data_final[[PCOS1c_vars[i]]] == "Yes"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final[[PCOS1c_vars[i]]])]) * 100,
                     no = sum(data_final$W3[data_final[[PCOS1c_vars[i]]] == "No"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final[[PCOS1c_vars[i]]])]) * 100,
                     dont_know = sum(data_final$W3[data_final[[PCOS1c_vars[i]]] == "DontKnow"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final[[PCOS1c_vars[i]]])]) * 100))
}

chart_4_data <- chart_4_data %>%
  left_join(sort_order) %>%
  arrange(order) %>%
  select(-order)

## Chart 5: Trust in NISRA by year ####

chart_5_data <- chart_5_data %>%
  rbind(data.frame(year = current_year,
                   trust = sum(data_final$W3[data_final$TrustNISRA2 == "Trust a great deal/Tend to trust"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$TrustNISRA2)]) * 100,
                   distrust = sum(data_final$W3[data_final$TrustNISRA2 == "Tend to distrust/Distrust greatly"], na.rm = TRUE)  / sum(data_final$W3[!is.na(data_final$TrustNISRA2)]) * 100,
                   dont_know = sum(data_final$W3[data_final$TrustNISRA2 == "Don't know"], na.rm = TRUE)  / sum(data_final$W3[!is.na(data_final$TrustNISRA2)]) * 100))

saveRDS(chart_5_data, paste0(data_folder, "Trend/", current_year, "/chart_5_data.RDS"))

chart_5_data <- chart_5_data %>%
  mutate(year = as.character(year))

## Chart 6: Trust in NISRA and ONS as institutions ####

ons_chart_6 <- data_ons %>%
  filter(`Related Variable` == "TrustNISRA2")

chart_6_data <- chart_5_data %>%
  filter(year == current_year) %>%
  mutate(org = paste0("NISRA (", current_year, ")")) %>%
  select(org, trust:dont_know) %>%
  rbind(data.frame(org = paste0("ONS (", ons_year, ")"),
                   trust = ons_chart_6$`Trust a great deal` + ons_chart_6$`Tend to trust`,
                   distrust = ons_chart_6$`Tend to distrust` + ons_chart_6$`Distrust greatly`,
                   dont_know = ons_chart_6$`Don't know`))


## Chart 7: Trust in institutions ####
 
chart_7_data <- 
  rbind(data.frame(org = c("The NI Assembly", "The Civil Service", "The media"),
                   trust = c(sum(data_final$W3[data_final$TrustNIAssembly2 == "Trust a great deal/Tend to trust"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$TrustNIAssembly2)]) * 100,
                             sum(data_final$W3[data_final$TrustCivilService2 == "Trust a great deal/Tend to trust"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$TrustCivilService2)]) * 100,
                             sum(data_final$W3[data_final$TrustMedia2 == "Trust a great deal/Tend to trust"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$TrustMedia2)]) * 100),
                   distrust = c(sum(data_final$W3[data_final$TrustNIAssembly2 == "Tend to distrust/Distrust greatly"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$TrustNIAssembly2)]) * 100,
                                sum(data_final$W3[data_final$TrustCivilService2 == "Tend to distrust/Distrust greatly"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$TrustCivilService2)]) * 100,
                                sum(data_final$W3[data_final$TrustMedia2 == "Tend to distrust/Distrust greatly"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$TrustMedia2)]) * 100),
                   dont_know = c(sum(data_final$W3[data_final$TrustNIAssembly2 == "Don't know"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$TrustNIAssembly2)]) * 100,
                             sum(data_final$W3[data_final$TrustCivilService2 == "Don't know"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$TrustCivilService2)]) * 100,
                             sum(data_final$W3[data_final$TrustMedia2 == "Don't know"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$TrustMedia2)]) * 100)),
        chart_5_data %>%
          filter(year == current_year) %>%
          mutate(org = "NISRA") %>%
          select(org, trust:dont_know)) %>%
  mutate(org = paste0(org, " "))

## Chart 8: Trust in NISRA statistics by year ####

chart_8_data <- chart_8_data %>%
  rbind(data.frame(year = current_year,
                   trust = sum(data_final$W3[data_final$TrustNISRAstats2 == "Trust a great deal/Tend to trust"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$TrustNISRAstats2)]) * 100,
                   distrust = sum(data_final$W3[data_final$TrustNISRAstats2 == "Tend to distrust/Distrust greatly"], na.rm = TRUE)  / sum(data_final$W3[!is.na(data_final$TrustNISRAstats2)]) * 100,
                   dont_know = sum(data_final$W3[data_final$TrustNISRAstats2 == "Don't know"], na.rm = TRUE)  / sum(data_final$W3[!is.na(data_final$TrustNISRAstats2)]) * 100))

saveRDS(chart_8_data, paste0(data_folder, "Trend/", current_year, "/chart_8_data.RDS"))

chart_8_data <- chart_8_data %>%
  mutate(year = as.character(year))

## Chart 9: Trust in statistics produced by NISRA  and ONS ####

ons_chart_9 <- data_ons %>%
  filter(`Related Variable` == "TrustNISRAstats2")

chart_9_data <- chart_8_data %>%
  filter(year == current_year) %>%
  mutate(org = paste0("NISRA (", current_year, ")")) %>%
  select(org, trust:dont_know) %>%
  rbind(data.frame(org = paste0("ONS (", ons_year, ")"),
                   trust = ons_chart_9$`Trust a great deal` + ons_chart_9$`Tend to trust`,
                   distrust = ons_chart_9$`Tend to distrust` + ons_chart_9$`Distrust greatly`,
                   dont_know = ons_chart_9$`Don't know`))


## Chart 10: NISRA statistics are important to understand Northern Ireland by year ####
 
chart_10_data <- chart_10_data %>%
  rbind(data.frame(year = current_year,
                   agree = sum(data_final$W3[data_final$NISRAstatsImp2 == "Strongly Agree/Tend to Agree"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$NISRAstatsImp2)]) * 100,
                   disagree = sum(data_final$W3[data_final$NISRAstatsImp2 == "Tend to disagree/Strongly disagree"], na.rm = TRUE)  / sum(data_final$W3[!is.na(data_final$NISRAstatsImp2)]) * 100,
                   dont_know = sum(data_final$W3[data_final$NISRAstatsImp2 == "Don't know"], na.rm = TRUE)  / sum(data_final$W3[!is.na(data_final$NISRAstatsImp2)]) * 100))

saveRDS(chart_10_data, paste0(data_folder, "Trend/", current_year, "/chart_10_data.RDS"))

chart_10_data <- chart_10_data %>%
  mutate(year = as.character(year))

## Chart 11: Statistics produced are important to understand our country, NISRA and ONS ####

ons_chart_11 <- data_ons %>%
  filter(`Related Variable` == "NISRAstatsImp2")

chart_11_data <- chart_10_data %>%
  filter(year == current_year) %>%
  mutate(org = paste0("NISRA (", current_year, ")")) %>%
  select(org, agree:dont_know) %>%
  rbind(data.frame(org = paste0("ONS (", ons_year, ")"),
                   agree = ons_chart_11$`Strongly agree` + ons_chart_11$`Tend to agree`,
                   disagree = ons_chart_11$`Strongly disagree` + ons_chart_11$`Tend to disagree`,
                   dont_know = ons_chart_11$`Don't know`))

## Chart 12: NISRA statistics are free from political interference by year ####

chart_12_data <- chart_12_data %>%
  rbind(data.frame(year = current_year,
                   agree = sum(data_final$W3[data_final$Political2 == "Strongly Agree/Tend to Agree"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$Political2)]) * 100,
                   disagree = sum(data_final$W3[data_final$Political2 == "Tend to disagree/Strongly disagree"], na.rm = TRUE)  / sum(data_final$W3[!is.na(data_final$Political2)]) * 100,
                   dont_know = sum(data_final$W3[data_final$Political2 == "Don't know"], na.rm = TRUE)  / sum(data_final$W3[!is.na(data_final$Political2)]) * 100))

saveRDS(chart_12_data, paste0(data_folder, "Trend/", current_year, "/chart_12_data.RDS"))

chart_12_data <- chart_12_data %>%
  mutate(year = as.character(year))

## Chart 13: Statistics produced are free from political interference, NISRA and ONS ####
 
ons_chart_13 <- data_ons %>%
  filter(`Related Variable` == "Political2")

chart_13_data <- chart_12_data %>%
  filter(year == current_year) %>%
  mutate(org = paste0("NISRA (", current_year, ")")) %>%
  select(org, agree:dont_know) %>%
  rbind(data.frame(org = paste0("ONS (", ons_year, ")"),
                   agree = ons_chart_13$`Strongly agree` + ons_chart_13$`Tend to agree`,
                   disagree = ons_chart_13$`Strongly disagree` + ons_chart_13$`Tend to disagree`,
                   dont_know = ons_chart_13$`Don't know`))

## Chart 14: Personal information provided to NISRA will be kept confidential by year ####

chart_14_data <- chart_14_data %>%
  rbind(data.frame(year = current_year,
                   agree = sum(data_final$W3[data_final$Confidential2 == "Strongly Agree/Tend to Agree"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$Confidential2)]) * 100,
                   disagree = sum(data_final$W3[data_final$Confidential2 == "Tend to disagree/Strongly disagree"], na.rm = TRUE)  / sum(data_final$W3[!is.na(data_final$Confidential2)]) * 100,
                   dont_know = sum(data_final$W3[data_final$Confidential2 == "Don't know"], na.rm = TRUE)  / sum(data_final$W3[!is.na(data_final$Confidential2)]) * 100))

saveRDS(chart_14_data, paste0(data_folder, "Trend/", current_year, "/chart_14_data.RDS"))

chart_14_data <- chart_14_data %>%
  mutate(year = as.character(year))

## Chart 15: Belief that personal information provided will be kept confidential, NISRA and ONS ####

ons_chart_15 <- data_ons %>%
  filter(`Related Variable` == "Confidential2")

chart_15_data <- chart_14_data %>%
  filter(year == current_year) %>%
  mutate(org = paste0("NISRA (", current_year, ")")) %>%
  select(org, agree:dont_know) %>%
  rbind(data.frame(org = paste0("ONS (", ons_year, ")"),
                   agree = ons_chart_15$`Strongly agree` + ons_chart_15$`Tend to agree`,
                   disagree = ons_chart_15$`Strongly disagree` + ons_chart_15$`Tend to disagree`,
                   dont_know = ons_chart_15$`Don't know`))

# Figures for commentary ####

## Introduction ####

sample_size <- prettyNum(nrow(data_final), big.mark = ",")
heard_of_nisra <- round_half_up(chart_1_data$pct[chart_1_data$year == current_year])
trust_in_nisra <- round_half_up(chart_5_data$trust[chart_5_data$year == current_year])
trust_in_nisra_stats <- round_half_up(chart_8_data$trust[chart_8_data$year == current_year])
importance_of_stats <- round_half_up(chart_10_data$agree[chart_10_data$year == current_year])
free_from_interference <- round_half_up(chart_12_data$agree[chart_12_data$year == current_year])

## Awareness of NISRA ####

aware_of_ons <- round_half_up(chart_2_data$ons[chart_2_data$year == ons_year])

## Awareness of NISRA Statistics ####

### Not heard of NISRA and aware of all / none of the statistics ####

data_final <- data_final %>%
  mutate(not_heard_yes_count = NA,
         heard_yes_count = NA)

for (i in 1:nrow(data_final)) {
  if (data_final$PCOS1[i] == "No") {
    data_final$not_heard_yes_count[i] <- 0
    for (j in 1:length(PCOS1d_vars)) {
      if (!is.na(data_final[[PCOS1d_vars[j]]][i])) {
        if(data_final[[PCOS1d_vars[j]]][i] == "Yes") {
          data_final$not_heard_yes_count[i] <- data_final$not_heard_yes_count[i] + 1
        }
      }
    }
  } else {
    data_final$heard_yes_count[i] <- 0
    for (j in 1:length(PCOS1c_vars)) {
      if (!is.na(data_final[[PCOS1c_vars[j]]][i])) {
        if(data_final[[PCOS1c_vars[j]]][i] == "Yes") {
          data_final$heard_yes_count[i] <- data_final$heard_yes_count[i] + 1
        }
      }
    }
  }
}

data_final <- data_final %>%
  mutate(not_heard_aware_none = case_when(not_heard_yes_count == 0 ~ TRUE,
                                          TRUE ~ FALSE),
         not_heard_aware_all = case_when(not_heard_yes_count == length(PCOS1c_vars) ~ TRUE,
                                         TRUE ~ FALSE),
         heard_aware_none = case_when(heard_yes_count == 0 ~ TRUE,
                                          TRUE ~ FALSE),
         heard_aware_all = case_when(heard_yes_count == length(PCOS1c_vars) ~ TRUE,
                                         TRUE ~ FALSE))


not_heard_aware_none <- round_half_up(sum(data_final$W3[data_final$not_heard_aware_none]) / sum(data_final$W3[data_final$PCOS1 == "No"]) * 100)
not_heard_aware_all <- round_half_up(sum(data_final$W3[data_final$not_heard_aware_all]) / sum(data_final$W3[data_final$PCOS1 == "No"]) * 100)

### Not heard of NISRA but aware of outputs ####

not_heard_aware_census <- round_half_up(chart_3_data$yes[grepl("NI Census", chart_3_data$output)])
not_heard_aware_unemployment <- round_half_up(chart_3_data$yes[grepl("unemployment", chart_3_data$output)])
not_heard_aware_hospital <- round_half_up(chart_3_data$yes[grepl("hospital", chart_3_data$output)])
not_heard_aware_people <- round_half_up(chart_3_data$yes[grepl("number of people", chart_3_data$output)])
not_heard_aware_cycling <- round_half_up(chart_3_data$yes[grepl("cycling", chart_3_data$output)])

### Heard of NISRA and aware of all / none of the statistics ####

heard_aware_none <- round_half_up(sum(data_final$W3[data_final$heard_aware_none]) / sum(data_final$W3[data_final$PCOS1 == "Yes"]) * 100)
heard_aware_all <- round_half_up(sum(data_final$W3[data_final$heard_aware_all]) / sum(data_final$W3[data_final$PCOS1 == "Yes"]) * 100)

heard_aware_census <- round_half_up(chart_4_data$yes[grepl("NI Census", chart_4_data$output)])
heard_aware_people <- round_half_up(chart_4_data$yes[grepl("number of people", chart_4_data$output)])
heard_aware_deaths <- round_half_up(chart_4_data$yes[grepl("deaths", chart_4_data$output)])
heard_of_aware_qualifications  <- round_half_up(chart_4_data$yes[grepl("Qualifications", chart_4_data$output)])
heard_of_aware_poverty  <- round_half_up(chart_4_data$yes[grepl("poverty", chart_4_data$output)])
heard_of_aware_cycling  <- round_half_up(chart_4_data$yes[grepl("cycling", chart_4_data$output)])

## Trust in NISRA ####

dont_know_trust_nisra <- round_half_up(chart_5_data$dont_know[chart_5_data$year == current_year])
distrust_nisra <- round_half_up(chart_5_data$distrust[chart_5_data$year == current_year])

heard_of_and_trust_nisra <- round_half_up(sum(data_final$W3[data_final$PCOS1 == "Yes" & data_final$TrustNISRA2 == "Trust a great deal/Tend to trust"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "Yes"]) * 100)

trust_in_ons <- round_half_up(chart_6_data$trust[grepl("ONS", chart_6_data$org)])
trust_in_media <- round_half_up(chart_7_data$trust[grepl("The media", chart_7_data$org)])
trust_in_assembly <- round_half_up(chart_7_data$trust[grepl("Assembly", chart_7_data$org)])
trust_in_nics <- round_half_up(chart_7_data$trust[grepl("Civil Service", chart_7_data$org)])


## Trust in NISRA Statistics ####

distrust_nisra_stats <- round_half_up(chart_8_data$distrust[chart_8_data == current_year])
dont_know_trust_nisra_stats <- round_half_up(chart_8_data$dont_know[chart_8_data == current_year])

heard_of_and_trust_nisra_stats <- round_half_up(sum(data_final$W3[data_final$PCOS1 == "Yes" & data_final$TrustNISRAstats2 == "Trust a great deal/Tend to trust"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "Yes"]) * 100)

trust_in_ons_stats <- round_half_up(chart_9_data$trust[grepl("ONS", chart_9_data$org)])
distrust_ons_stats <- round_half_up(chart_9_data$distrust[grepl("ONS", chart_9_data$org)])
dont_know_trust_ons_stats  <- round_half_up(chart_9_data$dont_know[grepl("ONS", chart_9_data$org)])

## Value ####

disagree_importance <- round_half_up(chart_10_data$disagree[chart_10_data$year == current_year])
dont_know_importance <- round_half_up(chart_10_data$dont_know[chart_10_data$year == current_year])

importance_of_ons <- round_half_up(chart_11_data$agree[grepl("ONS", chart_11_data$org)])
disagre_importance_ons  <- round_half_up(chart_11_data$disagree[grepl("ONS", chart_11_data$org)])
dont_know_importance_ons  <- round_half_up(chart_11_data$dont_know[grepl("ONS", chart_11_data$org)])

## Political Interference ####

disagree_interference <- round_half_up(chart_12_data$disagree[chart_12_data == current_year])
dont_know_interference <- round_half_up(chart_12_data$dont_know[chart_12_data == current_year])

free_from_interference_ons <- round_half_up(chart_13_data$agree[grepl("ONS", chart_13_data$org)])

## Confidentiality ####

agree_confidential <- round_half_up(chart_14_data$agree[chart_14_data$year == current_year]) 
dont_know_confidential <- round_half_up(chart_14_data$dont_know[chart_14_data$year == current_year]) 
disagree_confidential <- round_half_up(chart_14_data$disagree[chart_14_data$year == current_year])

agree_confidential_ons <- round_half_up(chart_15_data$agree[grepl("ONS", chart_15_data$org)])
