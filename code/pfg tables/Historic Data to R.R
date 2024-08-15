library(here)
source(paste0(here(), "/code/config.R"))

# 2021 Data ####

data_2021 <- readspss::read.sav(paste0(data_folder, "Final/PCOS 2021 FINAL (JULY 2022).sav"), use.missings = TRUE) %>%
  mutate(TrustAssemblyElectedBody2 = TrustNIAssembly2)

levels(data_2021$AGE2)[levels(data_2021$AGE2) == "74 and over"] <- "75 and over"

saveRDS(data_2021, paste0(data_folder, "Final/PCOS 2021 Final Dataset.RDS"))

# 2020 Data ####

data_2020 <- readspss::read.sav(paste0(data_folder, "Final/DATA  PCOS 2020 - Sept 21 (PCOS1 DK set to valid).sav"), use.missings = TRUE) %>%
  mutate(TrustAssemblyElectedBody2 = TrustNIAssembly2)

saveRDS(data_2020, paste0(data_folder, "Final/PCOS 2020 Final Dataset.RDS"))

# 2019 Data ####

data_2019 <- readspss::read.sav(paste0(data_folder, "Final/FOR REPORT PCOS_2019 - Sept 20 (Conf corrected, no refusals in recode var).sav"), use.missings = TRUE) %>%
  mutate(TrustAssemblyElectedBody2 = TrustElectedRep2)

saveRDS(data_2019, paste0(data_folder, "Final/PCOS 2019 Final Dataset.RDS"))

# 2018 Data ####

data_2018 <- readspss::read.sav(paste0(data_folder, "Final/Norma - PCOS 2018 weighted (Sep 19) - Working Copy.sav"), use.missings = TRUE) %>%
  mutate(DERHIanalysis = derhiRECODE)

saveRDS(data_2018, paste0(data_folder, "Final/PCOS 2018 Final Dataset.RDS"))

# 2016 Data ####

data_2016 <- readspss::read.sav(paste0(data_folder, "Final/omoctPCOS_2016_partial_recoded.sav"), use.missings = TRUE) %>%
  mutate(TrustAssemblyElectedBody2 = TrustNIAssembly2,
         AGE2 = pcosage,
         SEX = persex,
         EMPST2 = factor(empst2,
                         levels = levels(empst2),
                         labels = c("In paid employment", "Not in paid employment", "Refusal", "DontKnow")),
         DERHIanalysis = factor(highqual,
                                levels = levels(highqual),
                                labels = c("Degree, or Degree equivalent and above",
                                           "Other higher education below degree level",                  
                                           "A levels, vocational level 3 and equivalents",
                                           "GCSE/O level grade A*-C. vocational level 2 and equivalents",
                                           "Qualifications at level 1 and below",
                                           "No qualification",
                                           "Refusal",                                                    
                                           "DontKnow")))

saveRDS(data_2016, paste0(data_folder, "Final/PCOS 2016 Final Dataset.RDS"))

# 2014 Data ####

data_2014 <- readspss::read.sav(paste0(data_folder, "Final/OMSEP14PCOS_partial_recoded.sav"), use.missings = TRUE) %>%
  mutate(TrustAssemblyElectedBody2 = TrustNIAssembly2,
         AGE2 = pcosage,
         SEX = persex,
         EMPST2 = factor(empst2,
                         levels = levels(empst2),
                         labels = c("In paid employment", "Not in paid employment", "Refusal", "DontKnow")),
         DERHIanalysis = factor(highqual,
                                levels = levels(highqual),
                                labels = c("Degree, or Degree equivalent and above",
                                           "Other higher education below degree level",                  
                                           "A levels, vocational level 3 and equivalents",
                                           "GCSE/O level grade A*-C. vocational level 2 and equivalents",
                                           "Qualifications at level 1 and below",
                                           "No qualification",
                                           "Refusal",                                                    
                                           "DontKnow")))

saveRDS(data_2014, paste0(data_folder, "Final/PCOS 2014 Final Dataset.RDS"))

# 2012 Data #### 
 
data_2012 <- readspss::read.sav(paste0(data_folder, "Final/PCOS 2012 (PCOS4 Refusals set to missing).sav"), use.missings = TRUE) %>%
  mutate(SEX = persex,
         EMPST2 = factor(empst2,
                         levels = levels(empst2),
                         labels = c("In paid employment", "Not in paid employment", "Refusal", "DontKnow")),
         DERHIanalysis = factor(highqual,
                                levels = levels(highqual),
                                labels = c("Degree, or Degree equivalent and above",
                                           "Other higher education below degree level",                  
                                           "A levels, vocational level 3 and equivalents",
                                           "GCSE/O level grade A*-C. vocational level 2 and equivalents",
                                           "Qualifications at level 1 and below",
                                           "No qualification",
                                           "Refusal",                                                    
                                           "DontKnow")))

saveRDS(data_2012, paste0(data_folder, "Final/PCOS 2012 Final Dataset.RDS"))