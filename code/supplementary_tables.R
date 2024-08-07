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
    
    crosstab <- data.frame(var = responses)
    
    names(crosstab) <- sup_q[i]
    
    headings <- levels(data_final[[sup_covar[j]]])
    
    for (k in 1:length(headings)) {
      
      for (l in 1:length(responses)) {
        
        crosstab[[headings[k]]][l] <- data_final %>%
          filter(.[[sup_covar[j]]] == headings[k] & .[[sup_q[i]]] == responses[l]) %>%
          pull("W3") %>%
          sum(na.rm = TRUE) / data_final %>%
          filter(.[[sup_covar[j]]] == headings[k]) %>%
          pull("W3") %>%
          sum(na.rm = TRUE) * 100
        
      }
      
    }
    
    writeData(wb, sheet_name,
              x = paste0("Crosstab: ", sup_q[i], " by ", sup_covar[j]),
              startRow = r)
    
    addStyle(wb, sheet_name,
             rows = r,
             cols = 1,
             style = ts)
    
    r <- r + 1
    
    writeData(wb, sheet_name,
              x = sup_covar[j],
              startRow = r,
              startCol = 2)
    
    mergeCells(wb, sheet_name,
               rows = r,
               cols = 2:ncol(crosstab))
    
    addStyle(wb, sheet_name,
             ch2,
             rows = r,
             cols = 2)
    
    r <- r + 1
    
    writeDataTable(wb, sheet_name,
                   x = crosstab,
                   startRow = r,
                   tableStyle = "none",
                   headerStyle = hs,
                   withFilter = FALSE)
    
    addStyle(wb, sheet_name,
             style = hs2,
             rows = r,
             cols = 1)
    
    r <- r + nrow(crosstab) + 2
    
  }
  
  setColWidths(wb, sheet_name, cols = 1, widths = 27)
  
}

saveWorkbook(wb,
             paste0(here(), "/outputs/Supplementary tables ", current_year, ".xlsx"),
             overwrite = TRUE)