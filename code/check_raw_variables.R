saveRDS(data_raw, paste0(data_folder, "Raw/PCOS ", current_year, " Dataset.RDS"))

PCOS_vars <- c(names(data_raw)[grepl("PCOS", names(data_raw))], "DERHI", "EMPST2", "AGE")
PCOS1c_vars <- names(data_raw)[grepl("PCOS1c", names(data_raw)) & names(data_raw) != "PCOS1c"]
PCOS1d_vars <- names(data_raw)[grepl("PCOS1d", names(data_raw)) & names(data_raw) != "PCOS1d"]

data_last <- readRDS(paste0(data_folder, "Raw/PCOS ", current_year - 1, " Dataset.RDS"))

first_label <- which(grepl("Before being contacted", attributes(data_raw)$var.label))

old_q <- c("PCOS1", trust_q_old, agree_q_old)
new_q <- c("AwareNISRA2", trust_q_new, agree_q_new)

wb <- createWorkbook()
modifyBaseFont(wb, fontSize = 12, fontName = "Arial")

addWorksheet(wb, "Raw Variables")

r <- 1

for (i in 1:length(PCOS_vars)) {
  
  if (PCOS_vars[i] %in% PCOS1c_vars) {
    
    data_raw_f <- data_raw %>%
      filter(PCOS1 == "Yes")
    
    data_last_f <- data_last %>%
      filter(PCOS1 == "Yes")
    
  } else if (PCOS_vars[i] %in% PCOS1d_vars) {
    
    data_raw_f <- data_raw %>%
      filter(PCOS1 == "No")
    
    data_last_f <- data_last %>%
      filter(PCOS1 == "No")
    
  } else {
    
    data_raw_f <- data_raw
    
    data_last_f <- data_last
    
  }

  frequency_table <- data_last_f %>%
    group_by(var = .[[PCOS_vars[i]]]) %>%
    summarise(unweighted_last = n(),
              weighted_last = sum(W3)) %>%
    full_join(data_raw_f %>%
                group_by(var = .[[PCOS_vars[i]]]) %>%
                summarise(unweighted_current = n(),
                          weighted_current = sum(W3)),
              by = "var")
  
  if (class(frequency_table$var) != "numeric") {
    frequency_table <- frequency_table %>%
      mutate(var = case_when(is.na(var) ~ "Missing",
                             TRUE ~ var))
  }
  
  frequency_table <- frequency_table %>%
    mutate(unweighted_last = case_when(is.na(unweighted_last) ~ 0,
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
  
  names(frequency_table) <- c(if (PCOS_vars[i] %in% old_q) paste0(gsub("2", "", new_q[which(old_q == PCOS_vars[i])]), " (", PCOS_vars[i], ")") else PCOS_vars[i],
                              paste("Weighted %\n", current_year - 1),
                              paste("Weighted %\n", current_year),
                              paste("Unweighted %\n", current_year),
                              paste("Unweighted n\n", current_year - 1),
                              paste("Unweighted n\n", current_year),
                              paste("Unweighted %\n", current_year - 1),
                              paste("Weighted n\n", current_year - 1),
                              paste("Weighted n\n", current_year))
  
  writeData(wb, "Raw Variables",
            if (PCOS_vars[i] %in% c("DERHI", "EMPST2", "AGE")) paste("Covariate:", PCOS_vars[i]) else attributes(data_raw)$var.label[first_label + i - 1],
            startRow = r)
  
  addStyle(wb, "Raw Variables",
           pt2,
           rows = r,
           cols = 1)
  
  r <- r + 1
  
  writeDataTable(wb, "Raw Variables",
                 frequency_table[1:3],
                 tableStyle = "none",
                 headerStyle = ch,
                 withFilter = FALSE,
                 startRow = r)
  
  writeDataTable(wb, "Raw Variables",
                 frequency_table[c(1, 4, 3)],
                 tableStyle = "none",
                 headerStyle = ch,
                 withFilter = FALSE,
                 startRow = r,
                 startCol = 5)
  
  writeDataTable(wb, "Raw Variables",
                 frequency_table[c(7, 2, 5, 6, 8, 9)],
                 tableStyle = "none",
                 headerStyle = ch,
                 withFilter = FALSE,
                 startRow = r,
                 startCol = 9)
  
  addStyle(wb, "Raw Variables",
           hs2,
           rows = r,
           cols = c(1, 5, 9))
  
  addStyle(wb, "Raw Variables",
           ns3d,
           rows = (r + 1):(r + nrow(frequency_table) - 1),
           cols = c(2, 3, 6, 7, 9:14),
           gridExpand = TRUE)
  
  r <- r + nrow(frequency_table) + 2

}

setColWidths(wb, "Raw Variables",
             cols = 1:14,
             widths = c(24, 12.67, 12.67, 8, 24, 12.67, 12.67, 8, rep(12.67, 6)))

freezePane(wb, "Raw Variables",
           firstCol = TRUE)

# PCOS 1c Crosstabs ####

addWorksheet(wb, "PCOS1c Raw")

r <- 1

for (i in 1:length(PCOS1c_vars)) {
  
  responses <- levels(data_raw[[PCOS1c_vars[i]]])
  
  crosstab <- data.frame(PCOS1 = c("Yes", "No", "Don't know"))
  
  for (j in 1:length(responses)) {
    
    crosstab[[responses[j]]] <- c(data_raw %>%
                                    filter(PCOS1 == "Yes" & .[[PCOS1c_vars[i]]] == responses[j]) %>%
                                    nrow(),
                                  data_raw %>%
                                    filter(PCOS1 == "No" & .[[PCOS1c_vars[i]]] == responses[j]) %>%
                                    nrow(),
                                  data_raw %>%
                                    filter(PCOS1 == "Don't know" & .[[PCOS1c_vars[i]]] == responses[j]) %>%
                                    nrow())
    
  }
  
  writeData(wb, "PCOS1c Raw",
            x = paste0(PCOS1c_vars[i], " by PCOS1"),
            startRow = r)
  
  addStyle(wb, "PCOS1c Raw",
           pt2,
           rows = r,
           cols = 1)
  
  r <- r + 1
  
  writeData(wb, "PCOS1c Raw",
            x = PCOS1c_vars[i],
            startRow = r,
            startCol = 2)
  
  mergeCells(wb, "PCOS1c Raw",
             cols = 2:ncol(crosstab),
             rows = r)
  
  addStyle(wb, "PCOS1c Raw",
           ch2,
           rows = r,
           cols = 2)
  
  r <- r + 1
  
  writeDataTable(wb, "PCOS1c Raw",
                 crosstab,
                 tableStyle = "none",
                 headerStyle = ch,
                 withFilter = FALSE,
                 startRow = r)
  
  r <- r + nrow(crosstab) + 2
  
  
  
}

# PCOS 1d Crosstabs ####

addWorksheet(wb, "PCOS1d Raw")

r <- 1

for (i in 1:length(PCOS1d_vars)) {
  
  responses <- levels(data_raw[[PCOS1d_vars[i]]])
  
  crosstab <- data.frame(PCOS1 = c("Yes", "No", "Don't know"))
  
  for (j in 1:length(responses)) {
    
    crosstab[[responses[j]]] <- c(data_raw %>%
                                    filter(PCOS1 == "Yes" & .[[PCOS1d_vars[i]]] == responses[j]) %>%
                                    nrow(),
                                  data_raw %>%
                                    filter(PCOS1 == "No" & .[[PCOS1d_vars[i]]] == responses[j]) %>%
                                    nrow(),
                                  data_raw %>%
                                    filter(PCOS1 == "Don't know" & .[[PCOS1d_vars[i]]] == responses[j]) %>%
                                    nrow())
    
  }
  
  writeData(wb, "PCOS1d Raw",
            x = paste0(PCOS1d_vars[i], " by PCOS1"),
            startRow = r)
  
  addStyle(wb, "PCOS1d Raw",
           pt2,
           rows = r,
           cols = 1)
  
  r <- r + 1
  
  writeData(wb, "PCOS1d Raw",
            x = PCOS1d_vars[i],
            startRow = r,
            startCol = 2)
  
  mergeCells(wb, "PCOS1d Raw",
             cols = 2:ncol(crosstab),
             rows = r)
  
  addStyle(wb, "PCOS1d Raw",
           ch2,
           rows = r,
           cols = 2)
  
  r <- r + 1
  
  writeDataTable(wb, "PCOS1d Raw",
                 crosstab,
                 tableStyle = "none",
                 headerStyle = ch,
                 withFilter = FALSE,
                 startRow = r)
  
  r <- r + nrow(crosstab) + 2
  
  
}

saveWorkbook(wb,
             paste0(here(), "/outputs/Variable checks ", current_year, ".xlsx"),
             overwrite = TRUE)