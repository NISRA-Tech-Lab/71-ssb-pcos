# Demo specific configuration

library(here)

source(paste0(here(), "/code/config.R"))

#### INPUTS AND SETTINGS ####
##### SETTINGS #####

report_final <- FALSE

ifelse(!dir.exists(paste0(here(), "/code/demo/demo_outputs/")),
  dir.create(paste0(here(), "/code/demo/demo_outputs/")),
  "Outputs folder exists already"
)

# folder for download button csv and excel files
ifelse(!dir.exists(paste0(here(), "/code/demo/demo_outputs/", "figdata/")),
  dir.create(paste0(here(), "/code/demo/demo_outputs/", "figdata/")),
  "demo figdata folder exists already"
)

# check if demo excel tables exist. Used in demo_report to embed files
demo_excel_ready <- file.exists(paste0(
  here(), "/code/demo/demo_outputs/",
  "RAP_demo_tables.xlsx"
))
