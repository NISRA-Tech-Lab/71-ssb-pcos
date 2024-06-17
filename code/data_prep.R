# Set folder path
library(here)

# Read config file
source(paste0(here(), "/code/config.R"))

#### Read data in ####

data22 <- readspss::read.spss(paste0(data_folder, "PCOS 2022 Final Dataset (14 July 2023).sav"), pass = password)

#### Recode variables #####


#### Add dfs for tables and charts ####
