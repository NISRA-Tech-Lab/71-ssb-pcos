f_trust_instiutions <- function (title, tables) {
  
  # Create new workbook ####
  wb <- createWorkbook(creator = "NISRA Statistical Support Branch",
                       title = paste("Public Awareness of and Trust in Official Statistics Northern Ireland,", current_year),
                       subject = "Public Awareness and Trust of Statistics",
                       category = "Government")
  
  ## Set formatting ####
  modifyBaseFont(wb, fontSize = 12, fontName = "Arial")
  
  
  sheet_name <- "Trust_in_institutions"
  
  addWorksheet(wb, sheet_name)
  
  r <- 1
  
  writeData(wb, sheet_name,
            x = "Qu 3: For each institution, please indicate whether you tend to trust it or tend not to trust it",
            startRow = r)
  
  addStyle(wb, sheet_name,
           rows = r,
           cols = 1,
           style = pt,
           gridExpand = TRUE)
  
  setRowHeights(wb, sheet_name, rows = r, heights = 30)
  
  r <- r + 1
  
  if (length(tables) == 1) {
    writeData(wb, sheet_name,
              x = paste("This worksheet contains one table, outlining trust in public institutions."),
              startRow = r)
  } else {
    writeData(wb, sheet_name,
              x = paste("This worksheet contains", english(length(tables)), "tables, presented vertically with one blank row in between, outling trust in public institutions."),
              startRow = r)
  }
  
  r <- r + 1
  
  setRowHeights(wb, sheet_name, rows = r, heights = 30)
  
  for (table in tables) {
    
    table_name <- sub(":.*", "", table$title) %>%
      sub(" ", "_", .) %>%
      tolower(.)
    
    writeData(wb, sheet_name,
              x = table$title,
              startRow = r)
    
    addStyle(wb, sheet_name,
             rows = r,
             cols = 1,
             style = pt2)
    
    r <- r + 1
    
    if ("note" %in% names(table)) {
      
      writeData(wb, sheet_name,
                x = table$note,
                startRow = r)
      
      r <- r + 1
      
    }
    
    writeDataTable(wb, sheet_name,
                   x = table$data,
                   startRow = r,
                   headerStyle = ch_lined,
                   tableStyle = "none",
                   withFilter = FALSE,
                   tableName = table_name,
                   keepNA = TRUE,
                   na.string = "No data")
    
    addStyle(wb, sheet_name,
             rows = r,
             cols = 1,
             style = ch_lined_left)
    
    addStyle(wb, sheet_name,
             rows = (r + 1):(r + nrow(table$data) - 1),
             cols = 2:(ncol(table$data)),
             style = ns,
             gridExpand = TRUE)
    
    addStyle(wb, sheet_name,
             rows = (r + nrow(table$data)),
             cols = 2:(ncol(table$data)),
             style = ns_italic,
             gridExpand = TRUE)
    
    addStyle(wb, sheet_name,
             rows = (r + nrow(table$data)),
             cols = 1,
             style = num_resp)
    
    addStyle(wb, sheet_name,
             rows = (r + 1):(r + nrow(table$data) - 1),
             cols = 1,
             style = pt2)
    
    r <- r + nrow(table$data) + 2
    
  }
  
  setColWidths(wb,
               sheet_name,
               cols = 1:ncol(tables[[1]]$data),
               widths = c(27, rep(12, ncol(tables[[1]]$data) - 1)))
  
  xl_name <- paste0(here(), "/outputs/table_data/Trust_in_institutions_", current_year, ".xlsx")
  ods_name <- gsub(".xlsx", ".ods", xl_name)
  
  saveWorkbook(wb, xl_name, overwrite = TRUE)
  f_convert_to_ods(xl_name)
  unlink(xl_name)
  
  ods_size <- paste0(
    round_half_up(file.size(ods_name) / 1000),
    "kB"
  )
  
  div(style = "margin-top: -20px; margin-bottom: 20px;",
      div(class = "download-button", embed_file(ods_name, text = "Download data")),
      span(class = "download-text", paste0(" - ", title, " (.ODS format, ", ods_size, ")")))
  
  
}

