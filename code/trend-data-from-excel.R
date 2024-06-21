# Set folder path
library(here)

# Read config file
source(paste0(here(), "/code/config.R"))

chart_1_data <- read.xlsx(paste0(data_folder, "PCOS 2022 Charts.xlsx"), sheet = "Chart 1", startRow = 6) %>%
  t() %>% as.data.frame() %>%
  mutate(year = rownames(.)) %>%
  filter(year != "X1") %>%
  mutate(pct = round_half_up(as.numeric(`1`))) %>%
  select(year, pct) %>%
  filter(year != current_year)

saveRDS(chart_1_data, paste0(data_folder, "/Trend/chart_1_data.RDS"))
