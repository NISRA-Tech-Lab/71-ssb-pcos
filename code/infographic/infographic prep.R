library(here)
source(paste0(here(), "/code/data_prep.R"))

# Overview ####

# Overview Infographic
## Respondents' awareness of NISRA ####
awareness_of_nisra <- data.frame(year = current_year,
                                 aware_of_nisra = chart_1_data$pct[chart_1_data$year == current_year],
                                 not_aware_of_nisra = 100 - chart_1_data$pct[chart_1_data$year == current_year])
awareness_of_nisra <- awareness_of_nisra[,2:3]
awareness_of_nisra <- transpose(awareness_of_nisra)
awareness_of_nisra$Answer <- c("No", "Yes")
names(awareness_of_nisra)[names(awareness_of_nisra) == 'V1'] <- 'Percentage'
awareness_of_nisra$Percentage <- round_half_up(awareness_of_nisra$Percentage)
donut_chart_df <- awareness_of_nisra

## Trust in NISRA statistics ####
trust_nisra_stats <- chart_8_data %>%
  tail(5) %>%
  mutate(year = as.numeric(year))
new_names <- c("Year",
               "Tend to trust/trust a great deal",
               "Tend to distrust/distrust a great deal",
               "Don't know")
trust_nisra_stats <- trust_nisra_stats %>% 
  set_names(new_names)
trust_df <- gather(trust_nisra_stats, Category, Percentage, -Year)
trust_df$Percentage <- round_half_up(trust_df$Percentage) 
trust_df$Year <- as.character(trust_df$Year)

# Confidentiality
confidentiality <- chart_14_data %>%
  tail(5)  %>%
  mutate(year = as.numeric(year))
confidentiality <- confidentiality %>% 
  set_names(new_names)
confidentiality <- gather(confidentiality, Category, Percentage, -Year)
confidentiality$Percentage <- round_half_up(confidentiality$Percentage) 
confidentiality$Year <- as.character(confidentiality$Year)

## Personal Information provided to NISRA will be kept confidential ####
importance <- chart_10_data %>%
  tail(5)  %>%
  mutate(year = as.numeric(year))
importance <- importance %>% 
  set_names(new_names)
important_df <- gather(importance, Category, Percentage, -Year)
important_df$Percentage <- round_half_up(important_df$Percentage) 
important_df$Year <- as.character(important_df$Year)

## NISRA compared to other institutions ####
new_importance_names <- c("Institution",
                          "Tend to trust/trust a great deal",
                          "Tend to distrust/distrust a great deal",
                          "Don't know")
institutions <- chart_7_data
institutions <- institutions %>% 
  set_names(new_importance_names)
institutions_df <- gather(institutions, Category, Percentage, -Institution)
institutions_df$Percentage <- round_half_up(institutions_df$Percentage) 
trust_compared_df <- institutions_df

# Trust Infographic
# Chart 1
trust_info_data1 <- chart_9_data[chart_9_data$org %like% "NISRA", ]
new_trust_names <- c("Org", "Yes", "No", "Don't know")
trust_info_data1 <- trust_info_data1 %>% 
  set_names(new_trust_names)
trust_info_data1 <- gather(trust_info_data1, class, prop, -Org)
trust_info_data1$prop <- round_half_up(trust_info_data1$prop) 

# Chart 2
trust_info_data2 <- readRDS(paste0(data_folder, "Trend/", current_year,"/table_29_data.RDS"))
trust_info_data2 <- trust_info_data2[trust_info_data2$`Response (%)` %like% "Tend to trust/trust a great deal", ]
trust_info_data2 <- gather(trust_info_data2, Year, Percentage, -`Response (%)`)
trust_info_data2$Percentage <- round_half_up(trust_info_data2$Percentage) 
new_info_names <- c("Category", "Year", "Percentage")
trust_info_data2 <- trust_info_data2 %>% 
  set_names(new_info_names)
trust_info_data2$Year <- as.numeric(trust_info_data2$Year)
line_chart_df <- trust_info_data2

# Chart 3
trust_info_data3 <- readRDS(paste0(data_folder, "Trend/", current_year,"/table_35_data.RDS"))
trust_info_data3 <- trust_info_data3[trust_info_data3$`Response (%)` %like% "Strongly agree/Tend to agree", ]
trust_info_data3 <- gather(trust_info_data3, Year, Percentage, -`Response (%)`)
trust_info_data3$Percentage <- round_half_up(trust_info_data3$Percentage) 
new_info_names <- c("Category", "Year", "Percentage")
trust_info_data3 <- trust_info_data3 %>% 
  set_names(new_info_names)
trust_info_data3$Year <- as.character(trust_info_data3$Year)
line_chart_df <- trust_info_data3

# Awareness Infographic
# Chart 2
awareness_info_data1 <- readRDS(paste0(data_folder, "Trend/", current_year,"/table_1_data.RDS"))

awareness_info_data1 <- awareness_info_data1[awareness_info_data1$`Response (%)` %like% "Yes", ]
awareness_info_data1 <- gather(awareness_info_data1, Year, Percentage, -`Response (%)`)
awareness_info_data1$Percentage <- round_half_up(awareness_info_data1$Percentage) 
new_info_names <- c("Category", "Year", "Percentage")
awareness_info_data1 <- awareness_info_data1 %>% 
  set_names(new_info_names)
awareness_info_data1$Year <- sub('Note 1', '', awareness_info_data1$Year)
awareness_info_data1$Year <- gsub("\\[|\\]", "", awareness_info_data1$Year)
awareness_info_data1$Year <- gsub("[[:space:]]", "", awareness_info_data1$Year)
awareness_info_data1$Year <- as.numeric(awareness_info_data1$Year)
awareness_info_data1 <- subset(awareness_info_data1, Year >= 2017 & Year <= current_year)
line_chart_df <- awareness_info_data1

# Chart 3
awareness_info_data2 <- readRDS(paste0(data_folder, "Trend/", current_year,"/chart_2_data.RDS"))
awareness_info_data2$year <- as.character(awareness_info_data2$year)
awareness_info_data2 <- subset(awareness_info_data2 , awareness_info_data2$year == '2016'|
                                                      awareness_info_data2$year == '2018'|
                                                      awareness_info_data2$year == '2021'|
                                                      awareness_info_data2$year == '2022')
awareness_info_data2 <- gather(awareness_info_data2, Group, Percentage, -`year`)
awareness_info_data2$Percentage <- round_half_up(awareness_info_data2$Percentage) 
awareness_info_data2$Group <- toupper(awareness_info_data2$Group)

# Chart 4
awareness_info_data3 <- gather(chart_4_data, Answer, Percentage, -`output`)
awareness_info_data3$Percentage <- round_half_up(awareness_info_data3$Percentage, 1) 
colnames(awareness_info_data3[1]) <- 'Group'
colnames(awareness_info_data3)[1] <- c("Group")

# Join all data ####
infographic_data <- full_join(awareness_of_nisra,
                              trust_nisra_stats,
                              by = "year") %>%
  full_join(confidentiality,
            by = "year") %>%
  full_join(importance,
            by = "year") %>%
  bind_rows(institutions) %>%
  arrange(year)

write.csv(infographic_data,
          paste0(here(), "/outputs/infographic_data_", current_year, ".csv"),
          row.names = FALSE,
          na = "")

