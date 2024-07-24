old_q <- c(trust_q_old, agree_q_old)
new_q <- c(trust_q_new, agree_q_new)

wb <- loadWorkbook(paste0(here(), "/outputs/Variable checks ", current_year, ".xlsx"))

addWorksheet(wb, "Recoded Variables") 

r <- 1

for (i in 1:length(old_q)) {
  
  old <- data_final %>%
    group_by(var = .[[old_q[i]]]) %>%
    summarise(n = n()) %>%
    mutate(var = case_when(is.na(var) ~ "Missing",
                           TRUE ~ var))
  
  names(old) <- c(old_q[i], "n")
  
  new <- data_final %>%
    group_by(var = .[[new_q[i]]]) %>%
    summarise(n = n()) %>%
    mutate(var = case_when(is.na(var) ~ "Missing",
                           TRUE ~ var))
  
  names(new) <- c(new_q[i], "n")
  
  writeDataTable(wb, "Recoded Variables",
                 old,
                 tableStyle = "none",
                 headerStyle = ch,
                 withFilter = FALSE,
                 startRow = r)
  
  addStyle(wb, "Recoded Variables",
           hs2,
           rows = r,
           cols = 1)
  
  writeDataTable(wb, "Recoded Variables",
                 new,
                 tableStyle = "none",
                 headerStyle = ch,
                 withFilter = FALSE,
                 startRow = r,
                 startCol = ncol(old) + 2)
  
  addStyle(wb, "Recoded Variables",
           hs2,
           rows = r,
           cols = ncol(old) + 2)
  
  r <- r + nrow(old) + 2
  
}

setColWidths(wb, "Recoded Variables",
             cols = 1:5,
             widths = c(17, 11, 5, 24.5, 11))

saveWorkbook(wb,
             paste0(here(), "/outputs/Variable checks ", current_year, ".xlsx"),
             overwrite = TRUE)