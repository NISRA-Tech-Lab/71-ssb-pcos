f_extract_sheet <- function(path, sheet_name, sheet_title) {
  
  wb_in <- loadWorkbook(path)
  
  sheet_names <- getSheetNames(path)
  
  wb_out <- wb_in
  
  for (sheet in sheet_names) {
    
    if (sheet != sheet_name) {
      removeWorksheet(wb_out, sheet)
    }
    
  }
  
  xl_out_name <- paste0(here(), "/outputs/table_data/", sheet_name, "_", current_year, ".xlsx")
  ods_out_name <- sub(".xlsx", ".ods", xl_out_name)
  
  saveWorkbook(wb_out, xl_out_name, overwrite = TRUE)
  f_convert_to_ods(xl_out_name)
  unlink(xl_out_name)
  
  ods_size <- paste0(
    round_half_up(file.size(ods_out_name) / 1000),
    "kB"
  )
  
  div(style = "margin-top: -20px; margin-bottom: 20px;",
      div(class = "download-button", embed_file(ods_out_name, text = "Download data")),
      span(class = "download-text", paste0(" - ", sheet_title, " (.ODS format, ", ods_size, ")")))
  
}