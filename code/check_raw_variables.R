saveRDS(data_raw, paste0(data_folder, "Raw/PCOS ", current_year, " Dataset.RDS"))

PCOS_vars <- names(data_raw)[grepl("PCOS", names(data_raw))]

data_last <- readRDS(paste0(data_folder, "Raw/PCOS ", current_year - 1, " Dataset.RDS"))

wb <- createWorkbook()
modifyBaseFont(wb, fontSize = 12, fontName = "Arial")

addWorksheet(wb, "Raw Variables")

r <- 1

for (var in PCOS_vars) {

  frequency_table <- data_last %>%
    group_by(var = .[[var]]) %>%
    summarise(unweighted_last = n(),
              weighted_last = sum(W3)) %>%
    left_join(data_raw %>%
                group_by(var = .[[var]]) %>%
                summarise(unweighted_current = n(),
                          weighted_current = sum(W3)),
              by = "var") %>%
    mutate(var = case_when(is.na(var) ~ "Missing",
                                TRUE ~ var),
           unweighted_last_pct = round_half_up(unweighted_last / sum(unweighted_last) * 100, 3),
           weighted_last_pct = round_half_up(weighted_last / sum(weighted_last) * 100, 3),
           unweighted_current_pct = round_half_up(unweighted_current / sum(unweighted_current) * 100, 3),
           weighted_current_pct = round_half_up(weighted_current / sum(weighted_current) * 100, 3)) %>%
    select(var, unweighted_last, unweighted_current, unweighted_last_pct, unweighted_current_pct,
           weighted_last, weighted_current, weighted_last_pct, weighted_current_pct) %>%
    adorn_totals()
  
  names(frequency_table) <- c(var,
                              paste("Unweighted n\n", current_year - 1),
                              paste("Unweighted n\n", current_year),
                              paste("Unweighted %\n", current_year - 1),
                              paste("Unweighted %\n", current_year),
                              paste("Weighted n\n", current_year - 1),
                              paste("Weighted n\n", current_year),
                              paste("Weighted %\n", current_year - 1),
                              paste("Weighted %\n", current_year))
  
  writeDataTable(wb, "Raw Variables",
                 frequency_table,
                 tableStyle = "none",
                 headerStyle = ch,
                 withFilter = FALSE,
                 startRow = r)
  
  addStyle(wb, "Raw Variables",
           hs2,
           rows = r,
           cols = 1)
  
  r <- r + nrow(frequency_table) + 2

}

setColWidths(wb, "Raw Variables",
             cols = 1:ncol(frequency_table),
             widths = c(17, rep(12.67, ncol(frequency_table) - 1)))

saveWorkbook(wb,
             paste0(here(), "/outputs/Variable checks ", current_year, ".xlsx"),
             overwrite = TRUE)