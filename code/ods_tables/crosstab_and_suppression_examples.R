library(here)
source(paste0(here(), "/code/html_publication/data_prep.R"))

# Example 1 ####
# Use this example to include a new table by co-variate if not expecting any suppression

## Aware NISRA by AgeGroup without suppression ####

# Swap AwareNISRA2 and AGE2 throughout this example to create table for any question by any co-var
# Replace "example_1_table" with more appropriate name for new table
 
# After testing code creates table in this script paste all code for it at bottom of data_prep_for_ods.R
 
# Open run_ods_tables.R and take the name of the resulting data frame (in this case "example_1_table") and place it inside
# the f_worksheet() function relating to the sheet you want to add it to

responses <- levels(data_final$AwareNISRA2)

groups <- levels(data_final$AGE2)

example_1_table <- data.frame(response = c(responses, "Number of Respondents"))

for (group in groups) {
  for (i in 1:length(responses)) {
    example_1_table[[group]][i] <- f_return_p_group(data_final, "AwareNISRA2", responses[i], "AGE2", group) * 100
  }
  example_1_table[[group]][length(responses) + 1] <- f_return_n_group(data_final$AwareNISRA2, data_final$AGE2, group)
}

names(example_1_table)[names(example_1_table) == "response"] <- "Response (%)"

# Example 2 ####
# This builds on Example 1 by including an "if" step in the logic to remove Don't knows from the table
# Use this example to include a new table by co-variate if wanting to suppress below a certain number of "don't know" responses
 
## Aware NISRA by AgeGroup with suppression for 3 or fewer "Don't know" responses ####

# Swap AwareNISRA2 and AGE2 throughout this example to create table for any question by any co-var
# Replace "example_1_table" with more appropriate name for new table
# Change the value in the line "if (dont_know_count <= 3) {" to change the tolerance

# After testing code creates table in this script paste all code for it at bottom of data_prep_for_ods.R
 
# Open run_ods_tables.R and take the name of the resulting data frame (in this case "example_2_table") and place it inside
# the f_worksheet() function relating to the sheet you want to add it to

responses <- levels(data_final$AwareNISRA2)

groups <- levels(data_final$AGE2)

dont_know_count <- length(which(data_final$AwareNISRA2 == "Don't know"))

if (dont_know_count <= 3) {
  responses <- setdiff(responses, "Don't know")
  data_final_f <- data_final %>%
    filter(AwareNISRA2 != "Don't know")
} else {
  data_final_f <- data_final
}

example_2_table <- data.frame(response = c(responses, "Number of Respondents"))

for (group in groups) {
  for (i in 1:length(responses)) {
    example_2_table[[group]][i] <- f_return_p_group(data_final_f, "AwareNISRA2", responses[i], "AGE2", group) * 100
  }
  example_2_table[[group]][length(responses) + 1] <- f_return_n_group(data_final_f$AwareNISRA2, data_final_f$AGE2, group)
}

names(example_2_table)[names(example_2_table) == "response"] <- "Response (%)"