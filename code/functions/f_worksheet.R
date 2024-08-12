f_worksheet <- function(wb,
                        sheet_name,
                        contents,
                        title,
                        outlining,
                        tables) {
  
  addWorksheet(wb, sheetName = sheet_name)
  
  r <- 1
  
  writeData(wb, sheet_name,
            x = title,
            startRow = r)
  
  addStyle(wb, sheet_name,
           rows = r:length(title),
           cols = 1,
           style = pt,
           gridExpand = TRUE)
  
  setRowHeights(wb, sheet_name, rows = r, heights = 30)
  
  r <- r + length(title)
  
  if (length(tables) == 1) {
    writeData(wb, sheet_name,
              x = paste("This worksheet contains one table, outlining", outlining),
              startRow = r)
  } else {
    writeData(wb, sheet_name,
              x = paste("This worksheet contains", english(length(tables)), "tables, presented vertically with one blank row in between, outlining", outlining),
              startRow = r)
  }
  
  r <- r + 1
  
  writeData(wb, "Contents",
            contents,
            startRow = cr)
  
  addStyle(wb, "Contents",
           style = pt2,
           rows = cr,
           cols = 1)
  
  setRowHeights(wb, "Contents", rows = cr, heights = 30)
  
  cr <<- cr + 1
  
  for (i in 1:length(tables)) {
    
    table_name <- sub(":.*", "", tables[[i]]$title) %>%
      sub(" ", "_", .) %>%
      tolower(.) 
    
    writeData(wb, sheet_name,
              tables[[i]]$title,
              startRow = r)
    
    addStyle(wb, sheet_name,
             rows = r,
             cols = 1,
             style = pt2)
    
    if (i == 1) {
      setRowHeights(wb, sheet_name, rows = r, heights = 30)
    }
    
    r <- r + 1
    
    if ("note" %in% names(tables[[i]])) {
      
      writeData(wb, sheet_name,
                tables[[i]]$note,
                startRow = r)
      
      r <- r + 1
      
    }
    
    writeDataTable(wb, sheet_name,
                   x = tables[[i]]$data,
                   startRow = r,
                   headerStyle = ch_lined,
                   tableStyle = "none",
                   withFilter = FALSE,
                   tableName = table_name)
    
    addStyle(wb, sheet_name,
             rows = r,
             cols = 1,
             style = ch_lined_left)
    
    addStyle(wb, sheet_name,
             rows = (r + 1):(r + nrow(tables[[i]]$data) - 1),
             cols = 2:(ncol(tables[[i]]$data)),
             style = ns,
             gridExpand = TRUE)
    
    addStyle(wb, sheet_name,
             rows = (r + nrow(tables[[i]]$data)),
             cols = 2:(ncol(tables[[i]]$data)),
             style = ns_italic,
             gridExpand = TRUE)
    
    addStyle(wb, sheet_name,
             rows = (r + nrow(tables[[i]]$data)),
             cols = 1,
             style = num_resp)
    
    addStyle(wb, sheet_name,
             rows = (r + 1):(r + nrow(tables[[i]]$data) - 1),
             cols = 1,
             style = pt2)
    
    
    
    writeFormula(wb, "Contents",
                 x = makeHyperlinkString(sheet = sheet_name, row = r - 1, col = 1, text = tables[[i]]$title),
                 startRow = cr)
    
    cr <<- cr + 1
    
    r <- r + nrow(tables[[i]]$data) + 2
    
  }
  
  setColWidths(wb,
               sheet_name,
               cols = 1:ncol(tables[[1]]$data),
               widths = c(27, rep(12, ncol(tables[[1]]$data) - 1)))
  
}