unweighted_trend_sheet <- paste0(data_folder, "Trend/unweighted trend data.xlsx")

sheets <- getSheetNames(unweighted_trend_sheet)

for (i in 1:length(sheets)) {
  
  if (i == 1) {
    trend_data <- readxl::read_xlsx(unweighted_trend_sheet, sheet = sheets[i]) %>%
      mutate(stat = paste(sheets[i], "-", stat))
  } else {
    trend_data <- trend_data %>%
      bind_rows(readxl::read_xlsx(unweighted_trend_sheet, sheet = sheets[i]) %>%
                  mutate(stat = paste(sheets[i], "-", stat)))
  }
  
}

trend_data <- trend_data %>%
  select(-`2022`)

saveRDS(trend_data, paste0(data_folder, "Trend/2021/unweighted trend data.RDS"))
