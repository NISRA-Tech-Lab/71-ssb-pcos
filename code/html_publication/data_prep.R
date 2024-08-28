# Set folder path
library(here)

# Read config file
source(paste0(here(), "/code/config.R"))

# Old and new names for all "trust" based answers
# trust_body_var is defined in config.R
trust_q_old <- c("PCOS2a", "PCOS2b", "PCOS2b", "PCOS2c", "PCOS2d", "PCOS3")
trust_q_new <- c("TrustCivilService2", trust_body_var, "TrustAssemblyElectedBody2", "TrustMedia2", "TrustNISRA2", "TrustNISRAstats2")

# TrustElectedBodies2

# Old and new names for all "agree" based answers
agree_q_old <- c("PCOS4", "PCOS5", "PCOS6")
agree_q_new <- c("NISRAstatsImp2", "Political2", "Confidential2")

# Read data in from SPSS ####

data_raw <- readspss::read.spss(paste0(data_folder, "Raw/", data_filename),
  pass = password,
  use.missings = FALSE
)

## Raw variable check ran and output to Excel workbook in Outputs folder ####
source(paste0(here(), "/code/html_publication/check_raw_variables.R"))

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
  mutate(
    `Weighted base` = 100 - `Prefer not to answer`,
    across(.cols = `Don't know`:`Strongly disagree`, ~ .x / `Weighted base` * 100)
  ) %>%
  select(-`Prefer not to answer`, -`Weighted base`) %>%
  left_join(unweighted_ons_base, by = "Related Variable")

# Recode variables #####

## Recode age variable and DERHI ####
data_final <- data_raw %>%
  mutate(
    AGE2 = as.factor(case_when(
      AGE <= 24 ~ "16-24",
      AGE <= 34 ~ "25-34",
      AGE <= 44 ~ "35-44",
      AGE <= 54 ~ "45-54",
      AGE <= 64 ~ "55-64",
      AGE <= 74 ~ "65-74",
      TRUE ~ "75 and over"
    )),
    AGE2a = as.factor(case_when(
      AGE <= 15 ~ "0-15",
      AGE <= 29 ~ "20-29",
      AGE <= 39 ~ "30-39",
      AGE <= 49 ~ "40-49",
      AGE <= 59 ~ "59",
      TRUE ~ "60+"
    )),
    DERHIanalysis = case_when(
      DERHI == "Other qualifications" ~ NA,
      TRUE ~ DERHI
    ),
    DERHIanalysis = factor(DERHIanalysis,
      levels = c(
        "Degree, or Degree equivalent and above",
        "Other higher education below degree level",
        "A levels, vocational level 3 and equivalents",
        "GCSE/O level grade A*-C. vocational level 2 and equivalents",
        "Qualifications at level 1 and below",
        "No qualification"
      )
    ),
    EMPST2 = factor(EMPST2, levels = c("In paid employment", "Not in paid employment")),
    remove = FALSE,
    AwareNISRA2 = PCOS1
  ) %>% ## Added for later
  relocate("AGE2a", .after = "Age1") %>%
  relocate("AGE2", .after = "W3")

## Loop to recode all "Trust" based answers ####

for (i in 1:length(trust_q_old)) {
  data_final[[trust_q_new[i]]] <- case_when(
    data_final[[trust_q_old[i]]] %in% c("Trust a great deal", "Tend to trust", "Trust them greatly", "Tend to trust them") ~ "Trust a great deal/Tend to trust",
    data_final[[trust_q_old[i]]] %in% c("Tend to distrust", "Distrust greatly", "Tend not to trust them", "Distrust them greatly") ~ "Tend to distrust/Distrust greatly",
    data_final[[trust_q_old[i]]] == "DontKnow" ~ "Don't know",
    TRUE ~ data_final[[trust_q_old[i]]]
  ) %>%
    factor(levels = c("Trust a great deal/Tend to trust", "Tend to distrust/Distrust greatly", "Don't know"))
}

## Loop to recode all "agree" based answers ####

for (i in 1:length(agree_q_old)) {
  data_final[[agree_q_new[i]]] <- case_when(
    data_final[[agree_q_old[i]]] %in% c("Strongly agree", "Tend to agree") ~ "Strongly Agree/Tend to Agree",
    data_final[[agree_q_old[i]]] %in% c("Tend to disagree", "Strongly disagree") ~ "Tend to disagree/Strongly disagree",
    data_final[[agree_q_old[i]]] == "DontKnow" ~ "Don't know",
    TRUE ~ data_final[[agree_q_old[i]]]
  ) %>%
    factor(levels = c("Strongly Agree/Tend to Agree", "Tend to disagree/Strongly disagree", "Don't know"))
}

## Recode Refusals to Missing ####

vars_to_recode_to_missing <- names(data_final)[c(
  which(names(data_final) == "PCOS1"):which(names(data_final) == "PCOS1d9"),
  which(names(data_final) == "TrustCivilService2"):which(names(data_final) == "Confidential2")
)]

for (i in 1:length(vars_to_recode_to_missing)) {
  data_final[[vars_to_recode_to_missing[i]]] <- case_when(
    data_final[[vars_to_recode_to_missing[i]]] == "Refusal" ~ NA,
    TRUE ~ data_final[[vars_to_recode_to_missing[i]]]
  )
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

saveRDS(data_final, paste0(data_folder, "Final/PCOS ", current_year, " Final Dataset.RDS"))

## Check created variables against originals (see outputs folder) ####
source(paste0(here(), "/code/html_publication/check_created_variables.R"))

## Supplementary tables output to outputs folder ####
source(paste0(here(), "/code/html_publication/supplementary_tables.R"))

# Create data frames for charts ####

# Check Trend folder for 2021 exists
if (!dir.exists(paste0(data_folder, "Trend/2021"))) {
  dir.create(paste0(data_folder, "Trend/2021"))
  source(paste0(here(), "/code/html_publication/trend_data_for_charts.R"))
  source(paste0(here(), "/code/ods_tables/ods_trend_data.R"))
}

## Read in all last year's trend data ####
for (file in list.files((paste0(data_folder, "Trend/", current_year - 1)))) {
  assign(
    sub(".RDS", "", file),
    readRDS(paste0(data_folder, "Trend/", current_year - 1, "/", file))
  )
}

# Check Trend folder for new year exists
if (!dir.exists(paste0(data_folder, "Trend/", current_year))) {
  dir.create(paste0(data_folder, "Trend/", current_year))
}

## Chart 1: Awareness of NISRA by year ####

aware_nisra_data <- aware_nisra_data %>%
  rbind(data.frame(
    year = current_year,
    pct = sum(data_final$W3[data_final$PCOS1 == "Yes"]) / sum(data_final$W3) * 100
  ))

saveRDS(aware_nisra_data, paste0(data_folder, "Trend/", current_year, "/aware_nisra_data.RDS"))

## Chart 2: Awareness of NISRA and ONS by year ####

aware_nisra_ons_data <- aware_nisra_ons_data %>%
  rbind(data.frame(
    year = current_year,
    nisra = sum(data_final$W3[data_final$PCOS1 == "Yes"]) / sum(data_final$W3) * 100,
    ons = if (current_year == ons_year) data_ons$Yes[data_ons$`Related Variable` == "PCOS1"] else NA
  ))

saveRDS(aware_nisra_ons_data, paste0(data_folder, "Trend/", current_year, "/aware_nisra_ons_data.RDS"))

aware_nisra_ons_data <- aware_nisra_ons_data %>%
  mutate(year = as.character(year))

## Chart 3: Awareness of specific NISRA statistics for respondents who were not aware of NISRA ####

# Use variable label to extract output name
outputs <- sub("\\..*", "", attributes(data_final)$var.label[grepl("Heard", attributes(data_final)$var.label)]) %>%
  trimws()

aware_stats_data <- data.frame(
  output = character(),
  yes = numeric(),
  no = numeric(),
  dont_know = numeric()
)

for (i in 1:length(outputs)) {
  aware_stats_data <- aware_stats_data %>%
    rbind(data.frame(
      output = f_wrap_labels(outputs[i], 47),
      yes = sum(data_final$W3[data_final[[PCOS1d_vars[i]]] == "Yes"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final[[PCOS1d_vars[i]]])]) * 100,
      no = sum(data_final$W3[data_final[[PCOS1d_vars[i]]] == "No"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final[[PCOS1d_vars[i]]])]) * 100,
      dont_know = sum(data_final$W3[data_final[[PCOS1d_vars[i]]] == "DontKnow"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final[[PCOS1d_vars[i]]])]) * 100
    ))
}

aware_stats_data <- aware_stats_data %>%
  arrange(yes)

sort_order <- aware_stats_data %>%
  mutate(order = as.numeric(rownames(.))) %>%
  select(output, order)

## Chart 4: Awareness of specific NISRA statistics for respondents who were aware of NISRA ####

aware_stats_by_nisra_data <- data.frame(
  output = character(),
  yes = numeric(),
  no = numeric(),
  dont_know = numeric()
)

for (i in 1:length(outputs)) {
  aware_stats_by_nisra_data <- aware_stats_by_nisra_data %>%
    rbind(data.frame(
      output = f_wrap_labels(outputs[i], 47),
      yes = sum(data_final$W3[data_final[[PCOS1c_vars[i]]] == "Yes"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final[[PCOS1c_vars[i]]])]) * 100,
      no = sum(data_final$W3[data_final[[PCOS1c_vars[i]]] == "No"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final[[PCOS1c_vars[i]]])]) * 100,
      dont_know = sum(data_final$W3[data_final[[PCOS1c_vars[i]]] == "DontKnow"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final[[PCOS1c_vars[i]]])]) * 100
    ))
}

aware_stats_by_nisra_data <- aware_stats_by_nisra_data %>%
  left_join(sort_order) %>%
  arrange(order) %>%
  select(-order)

## Chart 5: Trust in NISRA by year ####

trust_nisra_data <- trust_nisra_data %>%
  rbind(data.frame(
    year = current_year,
    trust = sum(data_final$W3[data_final$TrustNISRA2 == "Trust a great deal/Tend to trust"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$TrustNISRA2)]) * 100,
    distrust = sum(data_final$W3[data_final$TrustNISRA2 == "Tend to distrust/Distrust greatly"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$TrustNISRA2)]) * 100,
    dont_know = sum(data_final$W3[data_final$TrustNISRA2 == "Don't know"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$TrustNISRA2)]) * 100
  ))

saveRDS(trust_nisra_data, paste0(data_folder, "Trend/", current_year, "/trust_nisra_data.RDS"))

trust_nisra_data <- trust_nisra_data %>%
  mutate(year = as.character(year))

## Chart 6: Trust in NISRA and ONS as institutions ####

trust_ons_data <- data_ons %>%
  filter(`Related Variable` == "TrustNISRA2")

trust_nisra_ons_data <- trust_nisra_data %>%
  filter(year == current_year) %>%
  mutate(org = paste0("NISRA (", current_year, ")")) %>%
  select(org, trust:dont_know) %>%
  rbind(data.frame(
    org = paste0("ONS (", ons_year, ")"),
    trust = trust_ons_data$`Trust a great deal` + trust_ons_data$`Tend to trust`,
    distrust = trust_ons_data$`Tend to distrust` + trust_ons_data$`Distrust greatly`,
    dont_know = trust_ons_data$`Don't know`
  ))


## Chart 7: Trust in institutions ####

trust_institutions_data <-
  rbind(
    data.frame(
      org = c("The NI Assembly", "The Civil Service", "The media"),
      trust = c(
        sum(data_final$W3[data_final$TrustAssemblyElectedBody2 == "Trust a great deal/Tend to trust"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$TrustAssemblyElectedBody2)]) * 100,
        sum(data_final$W3[data_final$TrustCivilService2 == "Trust a great deal/Tend to trust"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$TrustCivilService2)]) * 100,
        sum(data_final$W3[data_final$TrustMedia2 == "Trust a great deal/Tend to trust"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$TrustMedia2)]) * 100
      ),
      distrust = c(
        sum(data_final$W3[data_final$TrustAssemblyElectedBody2 == "Tend to distrust/Distrust greatly"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$TrustAssemblyElectedBody2)]) * 100,
        sum(data_final$W3[data_final$TrustCivilService2 == "Tend to distrust/Distrust greatly"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$TrustCivilService2)]) * 100,
        sum(data_final$W3[data_final$TrustMedia2 == "Tend to distrust/Distrust greatly"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$TrustMedia2)]) * 100
      ),
      dont_know = c(
        sum(data_final$W3[data_final$TrustAssemblyElectedBody2 == "Don't know"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$TrustAssemblyElectedBody2)]) * 100,
        sum(data_final$W3[data_final$TrustCivilService2 == "Don't know"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$TrustCivilService2)]) * 100,
        sum(data_final$W3[data_final$TrustMedia2 == "Don't know"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$TrustMedia2)]) * 100
      )
    ),
    trust_nisra_data %>%
      filter(year == current_year) %>%
      mutate(org = "NISRA") %>%
      select(org, trust:dont_know)
  ) %>%
  mutate(org = paste0(org, " "))

## Chart 8: Trust in NISRA statistics by year ####

trust_stats_data <- trust_stats_data %>%
  rbind(data.frame(
    year = current_year,
    trust = sum(data_final$W3[data_final$TrustNISRAstats2 == "Trust a great deal/Tend to trust"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$TrustNISRAstats2)]) * 100,
    distrust = sum(data_final$W3[data_final$TrustNISRAstats2 == "Tend to distrust/Distrust greatly"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$TrustNISRAstats2)]) * 100,
    dont_know = sum(data_final$W3[data_final$TrustNISRAstats2 == "Don't know"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$TrustNISRAstats2)]) * 100
  ))

saveRDS(trust_stats_data, paste0(data_folder, "Trend/", current_year, "/trust_stats_data.RDS"))

trust_stats_data <- trust_stats_data %>%
  mutate(year = as.character(year))

## Chart 9: Trust in statistics produced by NISRA  and ONS ####

trust_ons_stats_data <- data_ons %>%
  filter(`Related Variable` == "TrustNISRAstats2")

trust_stats_nisra_ons_data <- trust_stats_data %>%
  filter(year == current_year) %>%
  mutate(org = paste0("NISRA (", current_year, ")")) %>%
  select(org, trust:dont_know) %>%
  rbind(data.frame(
    org = paste0("ONS (", ons_year, ")"),
    trust = trust_ons_stats_data$`Trust a great deal` + trust_ons_stats_data$`Tend to trust`,
    distrust = trust_ons_stats_data$`Tend to distrust` + trust_ons_stats_data$`Distrust greatly`,
    dont_know = trust_ons_stats_data$`Don't know`
  ))


## Chart 10: NISRA statistics are important to understand Northern Ireland by year ####

stats_important_data <- stats_important_data %>%
  rbind(data.frame(
    year = current_year,
    agree = sum(data_final$W3[data_final$NISRAstatsImp2 == "Strongly Agree/Tend to Agree"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$NISRAstatsImp2)]) * 100,
    disagree = sum(data_final$W3[data_final$NISRAstatsImp2 == "Tend to disagree/Strongly disagree"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$NISRAstatsImp2)]) * 100,
    dont_know = sum(data_final$W3[data_final$NISRAstatsImp2 == "Don't know"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$NISRAstatsImp2)]) * 100
  ))

saveRDS(stats_important_data, paste0(data_folder, "Trend/", current_year, "/stats_important_data.RDS"))

stats_important_data <- stats_important_data %>%
  mutate(year = as.character(year))

## Chart 11: Statistics produced are important to understand our country, NISRA and ONS ####

stats_important_ons_data <- data_ons %>%
  filter(`Related Variable` == "NISRAstatsImp2")

stats_important_nisra_ons_data <- stats_important_data %>%
  filter(year == current_year) %>%
  mutate(org = paste0("NISRA (", current_year, ")")) %>%
  select(org, agree:dont_know) %>%
  rbind(data.frame(
    org = paste0("ONS (", ons_year, ")"),
    agree = stats_important_ons_data$`Strongly agree` + stats_important_ons_data$`Tend to agree`,
    disagree = stats_important_ons_data$`Strongly disagree` + stats_important_ons_data$`Tend to disagree`,
    dont_know = stats_important_ons_data$`Don't know`
  ))

## Chart 12: NISRA statistics are free from political interference by year ####

political_data <- political_data %>%
  rbind(data.frame(
    year = current_year,
    agree = sum(data_final$W3[data_final$Political2 == "Strongly Agree/Tend to Agree"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$Political2)]) * 100,
    disagree = sum(data_final$W3[data_final$Political2 == "Tend to disagree/Strongly disagree"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$Political2)]) * 100,
    dont_know = sum(data_final$W3[data_final$Political2 == "Don't know"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$Political2)]) * 100
  ))

saveRDS(political_data, paste0(data_folder, "Trend/", current_year, "/political_data.RDS"))

political_data <- political_data %>%
  mutate(year = as.character(year))

## Chart 13: Statistics produced are free from political interference, NISRA and ONS ####

political_ons_data <- data_ons %>%
  filter(`Related Variable` == "Political2")

political_nisra_ons_data <- political_data %>%
  filter(year == current_year) %>%
  mutate(org = paste0("NISRA (", current_year, ")")) %>%
  select(org, agree:dont_know) %>%
  rbind(data.frame(
    org = paste0("ONS (", ons_year, ")"),
    agree = political_ons_data$`Strongly agree` + political_ons_data$`Tend to agree`,
    disagree = political_ons_data$`Strongly disagree` + political_ons_data$`Tend to disagree`,
    dont_know = political_ons_data$`Don't know`
  ))

## Chart 14: Personal information provided to NISRA will be kept confidential by year ####

confidential_data <- confidential_data %>%
  rbind(data.frame(
    year = current_year,
    agree = sum(data_final$W3[data_final$Confidential2 == "Strongly Agree/Tend to Agree"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$Confidential2)]) * 100,
    disagree = sum(data_final$W3[data_final$Confidential2 == "Tend to disagree/Strongly disagree"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$Confidential2)]) * 100,
    dont_know = sum(data_final$W3[data_final$Confidential2 == "Don't know"], na.rm = TRUE) / sum(data_final$W3[!is.na(data_final$Confidential2)]) * 100
  ))

saveRDS(confidential_data, paste0(data_folder, "Trend/", current_year, "/confidential_data.RDS"))

confidential_data <- confidential_data %>%
  mutate(year = as.character(year))

## Chart 15: Belief that personal information provided will be kept confidential, NISRA and ONS ####

condifential_ons_data <- data_ons %>%
  filter(`Related Variable` == "Confidential2")

condifential_nisra_ons_data <- confidential_data %>%
  filter(year == current_year) %>%
  mutate(org = paste0("NISRA (", current_year, ")")) %>%
  select(org, agree:dont_know) %>%
  rbind(data.frame(
    org = paste0("ONS (", ons_year, ")"),
    agree = condifential_ons_data$`Strongly agree` + condifential_ons_data$`Tend to agree`,
    disagree = condifential_ons_data$`Strongly disagree` + condifential_ons_data$`Tend to disagree`,
    dont_know = condifential_ons_data$`Don't know`
  ))

# Figures for commentary ####

## Introduction ####

sample_size <- prettyNum(nrow(data_final), big.mark = ",")
heard_of_nisra <- round_half_up(aware_nisra_data$pct[aware_nisra_data$year == current_year])
trust_in_nisra <- round_half_up(trust_nisra_data$trust[trust_nisra_data$year == current_year])
trust_in_nisra_stats <- round_half_up(trust_stats_data$trust[trust_stats_data$year == current_year])
importance_of_stats <- round_half_up(stats_important_data$agree[stats_important_data$year == current_year])
free_from_interference <- round_half_up(political_data$agree[political_data$year == current_year])

## Awareness of NISRA ####

aware_of_ons <- round_half_up(aware_nisra_ons_data$ons[aware_nisra_ons_data$year == ons_year])

## Awareness of NISRA Statistics ####

### Not heard of NISRA and aware of all / none of the statistics ####

data_final <- data_final %>%
  mutate(
    not_heard_yes_count = NA,
    heard_yes_count = NA
  )

for (i in 1:nrow(data_final)) {
  if (data_final$PCOS1[i] == "No") {
    data_final$not_heard_yes_count[i] <- 0
    for (j in 1:length(PCOS1d_vars)) {
      if (!is.na(data_final[[PCOS1d_vars[j]]][i])) {
        if (data_final[[PCOS1d_vars[j]]][i] == "Yes") {
          data_final$not_heard_yes_count[i] <- data_final$not_heard_yes_count[i] + 1
        }
      }
    }
  } else {
    data_final$heard_yes_count[i] <- 0
    for (j in 1:length(PCOS1c_vars)) {
      if (!is.na(data_final[[PCOS1c_vars[j]]][i])) {
        if (data_final[[PCOS1c_vars[j]]][i] == "Yes") {
          data_final$heard_yes_count[i] <- data_final$heard_yes_count[i] + 1
        }
      }
    }
  }
}

data_final <- data_final %>%
  mutate(
    not_heard_aware_none = case_when(
      not_heard_yes_count == 0 ~ TRUE,
      TRUE ~ FALSE
    ),
    not_heard_aware_all = case_when(
      not_heard_yes_count == length(PCOS1c_vars) ~ TRUE,
      TRUE ~ FALSE
    ),
    heard_aware_none = case_when(
      heard_yes_count == 0 ~ TRUE,
      TRUE ~ FALSE
    ),
    heard_aware_all = case_when(
      heard_yes_count == length(PCOS1c_vars) ~ TRUE,
      TRUE ~ FALSE
    )
  )


not_heard_aware_none <- round_half_up(sum(data_final$W3[data_final$not_heard_aware_none]) / sum(data_final$W3[data_final$PCOS1 == "No"]) * 100)
not_heard_aware_all <- round_half_up(sum(data_final$W3[data_final$not_heard_aware_all]) / sum(data_final$W3[data_final$PCOS1 == "No"]) * 100)

### Not heard of NISRA but aware of outputs ####

not_heard_aware_census <- round_half_up(aware_stats_data$yes[grepl("NI Census", aware_stats_data$output)])
not_heard_aware_unemployment <- round_half_up(aware_stats_data$yes[grepl("unemployment", aware_stats_data$output)])
not_heard_aware_hospital <- round_half_up(aware_stats_data$yes[grepl("hospital", aware_stats_data$output)])
not_heard_aware_people <- round_half_up(aware_stats_data$yes[grepl("number of people", aware_stats_data$output)])
not_heard_aware_cycling <- round_half_up(aware_stats_data$yes[grepl("cycling", aware_stats_data$output)])

### Heard of NISRA and aware of all / none of the statistics ####

heard_aware_none <- round_half_up(sum(data_final$W3[data_final$heard_aware_none]) / sum(data_final$W3[data_final$PCOS1 == "Yes"]) * 100)
heard_aware_all <- round_half_up(sum(data_final$W3[data_final$heard_aware_all]) / sum(data_final$W3[data_final$PCOS1 == "Yes"]) * 100)

heard_aware_census <- round_half_up(aware_stats_by_nisra_data$yes[grepl("NI Census", aware_stats_by_nisra_data$output)])
heard_aware_people <- round_half_up(aware_stats_by_nisra_data$yes[grepl("number of people", aware_stats_by_nisra_data$output)])
heard_aware_deaths <- round_half_up(aware_stats_by_nisra_data$yes[grepl("deaths", aware_stats_by_nisra_data$output)])
heard_of_aware_qualifications <- round_half_up(aware_stats_by_nisra_data$yes[grepl("Qualifications", aware_stats_by_nisra_data$output)])
heard_of_aware_poverty <- round_half_up(aware_stats_by_nisra_data$yes[grepl("poverty", aware_stats_by_nisra_data$output)])
heard_of_aware_cycling <- round_half_up(aware_stats_by_nisra_data$yes[grepl("cycling", aware_stats_by_nisra_data$output)])

## Trust in NISRA ####

dont_know_trust_nisra <- round_half_up(trust_nisra_data$dont_know[trust_nisra_data$year == current_year])
distrust_nisra <- round_half_up(trust_nisra_data$distrust[trust_nisra_data$year == current_year])

heard_of_and_trust_nisra <- round_half_up(sum(data_final$W3[data_final$PCOS1 == "Yes" & data_final$TrustNISRA2 == "Trust a great deal/Tend to trust"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "Yes"]) * 100)

trust_in_ons <- round_half_up(trust_nisra_ons_data$trust[grepl("ONS", trust_nisra_ons_data$org)])
trust_in_media <- round_half_up(trust_institutions_data$trust[grepl("The media", trust_institutions_data$org)])
trust_in_assembly <- round_half_up(trust_institutions_data$trust[grepl("Assembly", trust_institutions_data$org)])
trust_in_nics <- round_half_up(trust_institutions_data$trust[grepl("Civil Service", trust_institutions_data$org)])


## Trust in NISRA Statistics ####

distrust_nisra_stats <- round_half_up(trust_stats_data$distrust[trust_stats_data == current_year])
dont_know_trust_nisra_stats <- round_half_up(trust_stats_data$dont_know[trust_stats_data == current_year])

heard_of_and_trust_nisra_stats <- round_half_up(sum(data_final$W3[data_final$PCOS1 == "Yes" & data_final$TrustNISRAstats2 == "Trust a great deal/Tend to trust"], na.rm = TRUE) / sum(data_final$W3[data_final$PCOS1 == "Yes"]) * 100)

trust_in_ons_stats <- round_half_up(trust_stats_nisra_ons_data$trust[grepl("ONS", trust_stats_nisra_ons_data$org)])
distrust_ons_stats <- round_half_up(trust_stats_nisra_ons_data$distrust[grepl("ONS", trust_stats_nisra_ons_data$org)])
dont_know_trust_ons_stats <- round_half_up(trust_stats_nisra_ons_data$dont_know[grepl("ONS", trust_stats_nisra_ons_data$org)])

## Value ####

disagree_importance <- round_half_up(stats_important_data$disagree[stats_important_data$year == current_year])
dont_know_importance <- round_half_up(stats_important_data$dont_know[stats_important_data$year == current_year])

importance_of_ons <- round_half_up(stats_important_nisra_ons_data$agree[grepl("ONS", stats_important_nisra_ons_data$org)])
disagre_importance_ons <- round_half_up(stats_important_nisra_ons_data$disagree[grepl("ONS", stats_important_nisra_ons_data$org)])
dont_know_importance_ons <- round_half_up(stats_important_nisra_ons_data$dont_know[grepl("ONS", stats_important_nisra_ons_data$org)])

## Political Interference ####

disagree_interference <- round_half_up(political_data$disagree[political_data == current_year])
dont_know_interference <- round_half_up(political_data$dont_know[political_data == current_year])

free_from_interference_ons <- round_half_up(political_nisra_ons_data$agree[grepl("ONS", political_nisra_ons_data$org)])

## Confidentiality ####

agree_confidential <- round_half_up(confidential_data$agree[confidential_data$year == current_year])
dont_know_confidential <- round_half_up(confidential_data$dont_know[confidential_data$year == current_year])
disagree_confidential <- round_half_up(confidential_data$disagree[confidential_data$year == current_year])

agree_confidential_ons <- round_half_up(condifential_nisra_ons_data$agree[grepl("ONS", condifential_nisra_ons_data$org)])
