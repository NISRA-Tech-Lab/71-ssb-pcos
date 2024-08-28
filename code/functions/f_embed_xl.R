f_embed_xl <- function(df, sheet_title, tab_name) {
  if (!dir.exists(paste0(here(), "/outputs/table_data"))) {
    dir.create(paste0(here(), "/outputs/table_data"))
  }


  wb <- createWorkbook(
    creator = "NISRA Statistical Support Branch",
    title = sheet_title,
    subject = "Public Awareness and Trust of Statistics",
    category = "Government"
  )

  modifyBaseFont(wb, fontSize = 12, fontName = "Arial")

  addWorksheet(wb, tab_name)

  writeData(
    wb, tab_name,
    c(sheet_title, "This worksheet contains one table.")
  )

  addStyle(wb, tab_name,
    style = pt,
    rows = 1,
    cols = 1
  )

  writeDataTable(wb, tab_name,
    df,
    startRow = 3,
    tableStyle = "none",
    withFilter = FALSE,
    tableName = tab_name,
    headerStyle = hs
  )

  addStyle(wb, tab_name,
    style = hs2,
    rows = 3,
    cols = 1
  )

  addStyle(wb, tab_name,
    style = ns,
    rows = 4:(nrow(df) + 3),
    cols = 2:ncol(df),
    gridExpand = TRUE
  )

  setColWidths(wb, tab_name,
    cols = 1,
    widths = 23
  )

  saveWorkbook(wb, paste0(here(), "/outputs/table_data/", tab_name, "_", current_year, ".xlsx"), overwrite = TRUE)

  xl_size <- paste0(
    round_half_up(file.size(paste0(here(), "/outputs/table_data/", tab_name, "_", current_year, ".xlsx")) / 1000),
    "kB"
  )

  write.csv(df, paste0(here(), "/outputs/table_data/", tab_name, "_", current_year, ".csv"), row.names = FALSE)

  csv_size <- round_half_up(file.size(paste0(here(), "/outputs/table_data/", tab_name, "_", current_year, ".csv")) / 1000)

  csv_size <- if (csv_size == 0) {
    "1kB"
  } else {
    paste0(csv_size, "kB")
  }

  paste(
    sheet_title, embed_file(paste0(here(), "/outputs/table_data/", tab_name, "_", current_year, ".xlsx"),
      text = paste0(" (.XLSX format; ", xl_size, ")")
    ),
    embed_file(paste0(here(), "/outputs/table_data/", tab_name, "_", current_year, ".csv"),
      text = paste0("(.CSV format; ", csv_size, ")")
    )
  )
}
