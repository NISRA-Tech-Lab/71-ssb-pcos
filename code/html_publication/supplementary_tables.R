library(here)
source(paste0(here(), "/code/config.R"))

# To run for older year (2021 and before) check values sup_year, sup_trust_q, sup_agree_q
# and sup_covar below and then press Ctrl + Alt + R to run.
# Tables will appear in outputs folder.

# Set year
sup_year <- 2021

# Date read in based on year value
sup_data <- readRDS(paste0(data_folder, "Final/PCOS ", sup_year, " Final Dataset.RDS"))

# "Trust" type Questions to analyse
sup_trust_q <- c("TrustCivilService2", "TrustAssemblyElectedBody2", "TrustMedia2", "TrustNISRA2", "TrustNISRAstats2")

# "Agree/disagree" type Questions to analyse
sup_agree_q <- c("NISRAstatsImp2", "Political2", "Confidential2")

# Co-variates
sup_covar <- c("AGE2", "DERHIanalysis", "EMPST2")

# Call function. Set weights below if different
f_supplementary_tables(data = sup_data,
                       year = sup_year,
                       trust_q = sup_trust_q,
                       agree_q = sup_agree_q,
                       co_var = sup_covar,
                       age_weight = "W1",
                       sex_weight = "W2",
                       weight = "W3")