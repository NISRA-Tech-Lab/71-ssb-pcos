f_supplementary_tables <- function (data, year, trust_q, agree_q, co_var, age_weight, sex_weight, weight) {
  
  # Set questions and co-variates ####
  sup_q <- c("PCOS1", trust_q, agree_q)
  
  wb <- createWorkbook()
  
  modifyBaseFont(wb, fontSize = 12, fontName = "Arial")
  
  num_levels <- c()
  
  for (i in 1:length(co_var)) {
    num_levels <- c(num_levels, length(levels(data[[co_var[i]]])))
  }
  
  max_levels <- max(num_levels)
  
  for (i in 1:length(sup_q)) {
    sheet_name <- sup_q[i]
    
    responses <- levels(data[[sup_q[i]]])
    
    addWorksheet(wb, sheet_name, zoom = 75)
    
    r <- 1
    
    for (j in 1:length(co_var)) {
      
      weight <- if (grepl("AGE", co_var[j])) {
        age_weight
      } else if (co_var[j] == "SEX") {
        sex_weight
      } else {
        weight
      }
      
      crosstab <- data.frame(var = responses)
      
      names(crosstab) <- sup_q[i]
      
      unweighted <- crosstab
      
      headings <- levels(data[[co_var[j]]])
      
      for (k in 1:length(headings)) {
        for (l in 1:length(responses)) {
          crosstab[[headings[k]]][l] <- data %>%
            filter(.[[co_var[j]]] == headings[k] & .[[sup_q[i]]] == responses[l]) %>%
            pull(weight) %>%
            sum(na.rm = TRUE) / data %>%
            filter(.[[co_var[j]]] == headings[k] & !is.na(.[[sup_q[i]]])) %>%
            pull(weight) %>%
            sum(na.rm = TRUE) * 100
          
          unweighted[[headings[k]]][l] <- data %>%
            filter(.[[co_var[j]]] == headings[k] & .[[sup_q[i]]] == responses[l]) %>%
            nrow()
        }
      }
      
      unweighted <- unweighted %>%
        adorn_totals()
      
      writeData(wb, sheet_name,
                x = paste0("Weighted % of ", sup_q[i], " by ", co_var[j]),
                startRow = r
      )
      
      writeData(wb, sheet_name,
                x = paste0("Unweighted N of ", sup_q[i], " by ", co_var[j]),
                startRow = r,
                startCol = max_levels + 3
      )
      
      addStyle(wb, sheet_name,
               rows = r,
               cols = c(1, max_levels + 3),
               style = ts,
               gridExpand = TRUE
      )
      
      r <- r + 1
      
      writeData(wb, sheet_name,
                x = co_var[j],
                startRow = r,
                startCol = 2
      )
      
      mergeCells(wb, sheet_name,
                 rows = r,
                 cols = 2:ncol(crosstab)
      )
      
      writeData(wb, sheet_name,
                x = co_var[j],
                startRow = r,
                startCol = max_levels + 4
      )
      
      mergeCells(wb, sheet_name,
                 rows = r,
                 cols = (max_levels + 4):(ncol(unweighted) + max_levels + 2)
      )
      
      addStyle(wb, sheet_name,
               ch2,
               rows = r,
               cols = c(2, max_levels + 4),
               gridExpand = TRUE
      )
      
      r <- r + 1
      
      writeDataTable(wb, sheet_name,
                     x = crosstab,
                     startRow = r,
                     tableStyle = "none",
                     headerStyle = hs,
                     withFilter = FALSE
      )
      
      writeDataTable(wb, sheet_name,
                     x = unweighted,
                     startRow = r,
                     startCol = max_levels + 3,
                     tableStyle = "none",
                     headerStyle = hs,
                     withFilter = FALSE
      )
      
      addStyle(wb, sheet_name,
               style = hs2,
               rows = r,
               cols = c(1, max_levels + 3),
               gridExpand = TRUE
      )
      
      addStyle(wb, sheet_name,
               style = ns1d,
               rows = (r + 1):(r + nrow(crosstab)),
               cols = 2:ncol(crosstab),
               gridExpand = TRUE
      )
      
      r <- r + nrow(unweighted) + 2
    }
    
    setColWidths(wb, sheet_name, cols = 1:(max_levels * 2 + 3), widths = 12)
    setColWidths(wb, sheet_name, cols = c(1, max_levels + 3), widths = 29)
  }
  
  saveWorkbook(wb,
               paste0(here(), "/outputs/Supplementary tables ", year, ".xlsx"),
               overwrite = TRUE
  )
  
}