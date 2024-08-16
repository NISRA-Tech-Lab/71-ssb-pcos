# Set questions and co-variates ####

sup_q <- c("PCOS1", trust_q_new, agree_q_new)
sup_covar <- c("AGE2", "DERHIanalysis", "EMPST2")

wb <- createWorkbook()

modifyBaseFont(wb, fontSize = 12, fontName = "Arial")

for (i in 1:length(sup_q)) {
  
  sheet_name <- sup_q[i]
  
  responses <- levels(data_final[[sup_q[i]]])
  
  addWorksheet(wb, sheet_name)
  
  r <- 1
  
  for (j in 1:length(sup_covar)) {
    
    weight <- if (grepl("AGE", sup_covar[j])) {
      "W1"
    } else if (sup_covar[j] == "SEX") {
      "W2"
    } else {
      "W3"
    }
    
    crosstab <- data.frame(var = responses)
    
    names(crosstab) <- sup_q[i]
    
    unweighted <- crosstab
    
    headings <- levels(data_final[[sup_covar[j]]])
    
    for (k in 1:length(headings)) {
      
      for (l in 1:length(responses)) {
        
        crosstab[[headings[k]]][l] <- data_final %>%
          filter(.[[sup_covar[j]]] == headings[k] & .[[sup_q[i]]] == responses[l]) %>%
          pull(weight) %>%
          sum(na.rm = TRUE) / data_final %>%
          filter(.[[sup_covar[j]]] == headings[k] & !is.na(.[[sup_q[i]]])) %>%
          pull(weight) %>%
          sum(na.rm = TRUE) * 100
        
        unweighted[[headings[k]]][l] <- data_final %>%
          filter(.[[sup_covar[j]]] == headings[k] & .[[sup_q[i]]] == responses[l]) %>%
          nrow()
        
      }
      
    }
    
    unweighted <- unweighted %>%
      adorn_totals()
    
    writeData(wb, sheet_name,
              x = paste0("Weighted % of ", sup_q[i], " by ", sup_covar[j]),
              startRow = r)
    
    writeData(wb, sheet_name,
              x = paste0("Unweighted N of ", sup_q[i], " by ", sup_covar[j]),
              startRow = r,
              startCol = 10)
    
    addStyle(wb, sheet_name,
             rows = r,
             cols = c(1, 10),
             style = ts,
             gridExpand = TRUE)
    
    r <- r + 1
    
    writeData(wb, sheet_name,
              x = sup_covar[j],
              startRow = r,
              startCol = 2)
    
    mergeCells(wb, sheet_name,
               rows = r,
               cols = 2:ncol(crosstab))
    
    writeData(wb, sheet_name,
              x = sup_covar[j],
              startRow = r,
              startCol = 11)
    
    mergeCells(wb, sheet_name,
               rows = r,
               cols = 11:(ncol(unweighted) + 9))
    
    addStyle(wb, sheet_name,
             ch2,
             rows = r,
             cols = c(2, 11),
             gridExpand = TRUE)
    
    r <- r + 1
    
    writeDataTable(wb, sheet_name,
                   x = crosstab,
                   startRow = r,
                   tableStyle = "none",
                   headerStyle = hs,
                   withFilter = FALSE)
    
    writeDataTable(wb, sheet_name,
                   x = unweighted,
                   startRow = r,
                   startCol = 10,
                   tableStyle = "none",
                   headerStyle = hs,
                   withFilter = FALSE)
    
    addStyle(wb, sheet_name,
             style = hs2,
             rows = r,
             cols = c(1, 10),
             gridExpand = TRUE)
    
    addStyle(wb, sheet_name,
             style = ns1d,
             rows = (r+1):(r + nrow(crosstab)),
             cols = 2:ncol(crosstab),
             gridExpand = TRUE)
    
    r <- r + nrow(unweighted) + 2
    
  }
  
  setColWidths(wb, sheet_name, cols = c(1, 10), widths = 27)
  
}

saveWorkbook(wb,
             paste0(here(), "/outputs/Supplementary tables ", current_year, ".xlsx"),
             overwrite = TRUE)