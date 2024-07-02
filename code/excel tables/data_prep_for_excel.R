library(here)
source(paste0(here(), "/code/data_prep.R"))

# Awareness of NISRA ####

## Table 1: Awareness of NISRA by year ####

table_1_data <- table_1_data %>%
  mutate(current_year = c(chart_2_data$nisra[chart_2_data$year == current_year],
                          sum(data_final$W3[data_final$PCOS1 == "No"], na.rm = TRUE)  / sum(data_final$W3[!is.na(data_final$PCOS1)], na.rm = TRUE) * 100,
                          sum(data_final$W3[data_final$PCOS1 == "DontKnow"], na.rm = TRUE)  / sum(data_final$W3[!is.na(data_final$PCOS1)], na.rm = TRUE) * 100,
                          nrow(data_final[!is.na(data_final$PCOS1), ])))

names(table_1_data)[names(table_1_data) == "current_year"] <- current_year

saveRDS(table_1_data, paste0(data_folder, "Trend/", current_year, "/table_1_data.RDS"))

## Table 2: Awareness of NISRA and ONS ####

table_2_data <- table_1_data[c("Response (%)", current_year)] %>%
  mutate(ONS = read_xlsx(ons_xl, sheet = "Awareness of ONS", range = "D4:D7", col_names = FALSE) %>%
           pull(`...1`))

names(table_2_data)[names(table_1_data) == current_year] <- "NISRA"

# Awareness of NISRA Statistics by those not previously aware of NISRA ####

## Table 3: Aware of NISRA statistics on the number of deaths in Northern Ireland ####
## Table 4: Aware of NISRA statistics on recorded levels of crime in Northern Ireland ####
## Table 5: Aware of NISRA statistics on recorded levels of crime in Northern Ireland ####
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
## Table 15: Aware of NISRA statistics on recorded levels of crime in Northern Ireland ####
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
 
## Table 22: Number of selected NISRA statistics respondents had heard of (among those who were not previously aware of NISRA) ####
 
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

## Table 23: Trust in NISRA by year

table_23_data <- table_23_data %>%
  mutate(current_year = c(chart_5_data$trust[chart_5_data$year == current_year],
                          chart_5_data$distrust[chart_5_data$year == current_year],
                          chart_5_data$dont_know[chart_5_data$year == current_year],
                          sum(!is.na(data_final$TrustNISRA2))))

names(table_23_data)[names(table_23_data) == "current_year"] <- current_year

saveRDS(table_23_data, paste0(data_folder, "Trend/", current_year, "/table_23_data.RDS"))
