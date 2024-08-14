
# Load checks workbook ####

wb <- loadWorkbook(paste0(here(), "/outputs/Variable checks ", current_year, ".xlsx"))

# Check Recoded vars vs Recoded previous year ####

addWorksheet(wb, "Recoded Variables")

data_last <- readRDS(paste0(data_folder, "Final/PCOS ", current_year - 1, " Final Dataset.RDS"))

r <- 1

recoded_vars <- c(new_q, "DERHIanalysis", "DERHI", "AGE2")

for (i in 1:length(recoded_vars)) {
  
  frequency_table <- data_last %>%
    group_by(var = .[[recoded_vars[i]]]) %>%
    summarise(unweighted_last = n(),
              weighted_last = sum(W3)) %>%
    full_join(data_final %>%
                group_by(var = .[[recoded_vars[i]]]) %>%
                summarise(unweighted_current = n(),
                          weighted_current = sum(W3)),
              by = "var") %>%
    mutate(var = case_when(is.na(var) ~ "Missing",
                           TRUE ~var),
           unweighted_last = case_when(is.na(unweighted_last) ~ 0,
                                       TRUE ~ unweighted_last),
           weighted_last = case_when(is.na(weighted_last) ~ 0,
                                     TRUE ~ weighted_last),
           unweighted_current = case_when(is.na(unweighted_current) ~ 0,
                                          TRUE ~ unweighted_current),
           weighted_current = case_when(is.na(weighted_current) ~ 0,
                                        TRUE ~ weighted_current),
           unweighted_last_pct = unweighted_last / sum(unweighted_last) * 100,
           weighted_last_pct = weighted_last / sum(weighted_last) * 100,
           unweighted_current_pct = unweighted_current / sum(unweighted_current) * 100,
           weighted_current_pct = weighted_current / sum(weighted_current) * 100) %>%
    select(var, weighted_last_pct, weighted_current_pct, unweighted_current_pct, unweighted_last, unweighted_current, unweighted_last_pct,
           weighted_last, weighted_current) %>%
    adorn_totals()
  
  names(frequency_table) <- c(recoded_vars[i],
                              paste("Weighted %\n", current_year - 1),
                              paste("Weighted %\n", current_year),
                              paste("Unweighted %\n", current_year),
                              paste("Unweighted n\n", current_year - 1),
                              paste("Unweighted n\n", current_year),
                              paste("Unweighted %\n", current_year - 1),
                              paste("Weighted n\n", current_year - 1),
                              paste("Weighted n\n", current_year))
  
  writeDataTable(wb, "Recoded Variables",
                 frequency_table[1:3],
                 tableStyle = "none",
                 headerStyle = ch,
                 withFilter = FALSE,
                 startRow = r)
  
  writeDataTable(wb, "Recoded Variables",
                 frequency_table[c(1, 4, 3)],
                 tableStyle = "none",
                 headerStyle = ch,
                 withFilter = FALSE,
                 startRow = r,
                 startCol = 5)
  
  writeDataTable(wb, "Recoded Variables",
                 frequency_table[c(7, 2, 5, 6, 8, 9)],
                 tableStyle = "none",
                 headerStyle = ch,
                 withFilter = FALSE,
                 startRow = r,
                 startCol = 9)
  
  addStyle(wb, "Recoded Variables",
           hs2,
           rows = r,
           cols = c(1, 5, 9))
  
  addStyle(wb, "Recoded Variables",
           ns3d,
           rows = (r + 1):(r + nrow(frequency_table) - 1),
           cols = c(2, 3, 6, 7, 9:14),
           gridExpand = TRUE)
  
  r <- r + nrow(frequency_table) + 2
  
}

setColWidths(wb, "Recoded Variables",
             cols = 1:14,
             widths = c(24, 12.67, 12.67, 8, 24, 12.67, 12.67, 8, rep(12.67, 6)))

freezePane(wb, "Recoded Variables",
           firstCol = TRUE)

# Crosstabs ####
 
addWorksheet(wb, "Crosstabs")

r <- 1

old_q <- c(old_q, "DERHI", "AGE")
new_q <- c(new_q, "DERHIanalysis", "AGE2")

for (i in 1:length(new_q)) {
  
  if (old_q[i] == "AGE") {
    old_q_options <- as.character(sort(unique(data_final[[old_q[i]]])))
  } else {
    old_q_options <- levels(data_final[[old_q[i]]])
  }
  
  new_q_options <- levels(data_final[[new_q[i]]])
  
  if (new_q[i] == "DERHIanalysis") {
    new_q_options <- c(new_q_options, "Missing")
  }
  
  crosstab <- data.frame(new = new_q_options)
  
  for (j in 1:length(new_q_options)) {
    for (k in 1:length(old_q_options)) {
      
      if (new_q_options[j] == "Missing") {
        
        crosstab[[old_q_options[k]]][j] <- data_final %>%
          filter(is.na(.[[new_q[i]]]) &
                   .[[old_q[i]]] == old_q_options[k]) %>%
          nrow()
        
      } else {
        
        crosstab[[old_q_options[k]]][j] <- data_final %>%
          filter(.[[new_q[i]]] == new_q_options[j] &
                   .[[old_q[i]]] == old_q_options[k]) %>%
          nrow()
        
      }
      
      
    }
  }
  
  names(crosstab)[names(crosstab) == "new"] <- new_q[i]
  
  writeData(wb, "Crosstabs",
            old_q[i],
            startRow = r,
            startCol = 2)
  
  mergeCells(wb, "Crosstabs",
             cols = 2:ncol(crosstab),
             rows = r)
  
  addStyle(wb, "Crosstabs",
           ch2,
           rows = r,
           cols = 2)
  
  r <- r + 1
  
  writeDataTable(wb, "Crosstabs",
                 crosstab,
                 tableStyle = "none",
                 headerStyle = ch,
                 withFilter = FALSE,
                 startRow = r)
  
  addStyle(wb, "Crosstabs",
           hs2,
           rows = r:(r + nrow(crosstab)),
           cols = 1)
  
  r <- r + nrow(crosstab) + 2
  
}

setColWidths(wb, "Crosstabs",
             cols = 1:length(unique(data_final$AGE)),
             widths = c(52, rep(13, length(unique(data_final$AGE)) - 1)))

freezePane(wb, "Crosstabs",
           firstCol = TRUE)

saveWorkbook(wb,
             paste0(here(), "/outputs/Variable checks ", current_year, ".xlsx"),
             overwrite = TRUE)