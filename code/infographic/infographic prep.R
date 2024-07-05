library(here)
source(paste0(here(), "/code/excel tables/data_prep_for_excel.R"))

# Overview ####

## Respondents' awareness of NISRA ####

awareness_of_nisra <- data.frame(year = current_year,
                                 aware_of_nisra = chart_1_data$pct[chart_1_data$year == current_year],
                                 not_aware_of_nisra = 100 - chart_1_data$pct[chart_1_data$year == current_year])

## Trust in NISRA statistics ####

trust_nisra_stats <- chart_8_data %>%
  tail(5) %>%
  mutate(year = as.numeric(year))

names(trust_nisra_stats)[2:ncol(trust_nisra_stats)] <- paste0(names(trust_nisra_stats)[2:ncol(trust_nisra_stats)], "_nisra_stats")

## Personal Information provided to NISRA will be kept confidential ####

confidentiality <- chart_14_data %>%
  tail(5)  %>%
  mutate(year = as.numeric(year))

names(confidentiality)[2:ncol(confidentiality)] <- paste0(names(confidentiality)[2:ncol(confidentiality)], "_confidentiality")

## Personal Information provided to NISRA will be kept confidential ####

importance <- chart_10_data %>%
  tail(5)  %>%
  mutate(year = as.numeric(year))

names(importance)[2:ncol(importance)] <- paste0(names(importance)[2:ncol(importance)], "_importance")

## NISRA compared to other institutions ####

institutions <- data.frame(year = current_year,
                           trust_nisra = chart_7_data$trust[grepl("NISRA", chart_7_data)],
                           distrust_nisra = chart_7_data$distrust[grepl("NISRA", chart_7_data)],
                           dont_know_nisra = chart_7_data$dont_know[grepl("NISRA", chart_7_data)],
                           trust_media = chart_7_data$trust[grepl("media", chart_7_data)],
                           distrust_media = chart_7_data$distrust[grepl("media", chart_7_data)],
                           dont_know_media = chart_7_data$dont_know[grepl("media", chart_7_data)],
                           trust_nics = chart_7_data$trust[grepl("Civil Service", chart_7_data)],
                           distrust_nics = chart_7_data$distrust[grepl("Civil Service", chart_7_data)],
                           dont_know_nics = chart_7_data$dont_know[grepl("Civil Service", chart_7_data)],
                           trust_assembly = chart_7_data$trust[grepl("Assembly", chart_7_data)],
                           distrust_assembly = chart_7_data$distrust[grepl("Assembly", chart_7_data)],
                           dont_know_assembly = chart_7_data$dont_know[grepl("Assembly", chart_7_data)])

# Join all data ####
 
infographic_data <- full_join(awareness_of_nisra,
                              trust_nisra_stats,
                              by = "year") %>%
  full_join(confidentiality,
            by = "year") %>%
  full_join(importance,
            by = "year") %>%
  full_join(institutions, 
            by = "year") %>%
  arrange(year)

write.csv(infographic_data,
          paste0(here(), "/outputs/infographic_data_", current_year, ".csv"),
          row.names = FALSE,
          na = "")
