f_embed_ods <- function(df, sheet_title, tab_name, app_b = FALSE) {
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

  xl_name <- paste0(here(), "/outputs/table_data/", tab_name, "_", current_year, ".xlsx")
  ods_name <- gsub(".xlsx", ".ods", xl_name)

  saveWorkbook(wb, xl_name, overwrite = TRUE)

  f_convert_to_ods(xl_name)

  ods_size <- paste0(
    round_half_up(file.size(ods_name) / 1000),
    "kB"
  )

  unlink(xl_name)

  if (app_b) {
    paste0(sheet_title, " (", embed_file(ods_name, text = ".ODS format"), "; ", ods_size, ")")
  } else {
    div(
      style = "margin-top: -20px; margin-bottom: 20px;",
      div(class = "download-button", embed_file(ods_name, text = "Download data")),
      span(class = "download-text", paste0(" - ", sheet_title, " (.ODS format; ", ods_size, ")"))
    )
  }
}
