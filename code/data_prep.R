# Set folder path
library(here)

# Read config file
source(paste0(here(), "/code/config.R"))

#### Read data in ####
data2223 <- readspss::read.spss(paste0(data_folder, "Final/PCOS 2022 Final Dataset (14 July 2023).sav"), pass = password)
data22 <- readspss::read.spss(paste0(data_folder, "Raw/2223_PCOS_FINAL_WEIGHTED_PASSWORDED.sav"), pass = raw_password)

#### Recode variables #####
data22 <- data22 %>% rename("TrustCivilService2" = "PCOS2a", 
                            "TrustNIAssembly2" = "PCOS2b",  
                            "TrustMedia2" = "PCOS2c", 
                            "TrustNISRA2" = "PCOS2d",
                            "TrustNISRAstats2" = "PCOS3", 
                            "NISRAstatsImp2" = "PCOS4", 
                            "Political2" = "PCOS5",  
                            "Confidential2" = "PCOS6")

data22 <- data22 %>%
  filter(!is.na(TrustCivilService2)|
         !is.na(TrustNIAssembly2)|
         !is.na(TrustMedia2)|   
         !is.na(TrustNISRA2)|
         !is.na(TrustNISRAstats2)|
         !is.na(NISRAstatsImp2)|
         !is.na(Political2)|
         !is.na(Confidential2))

data22 <- data22 %>%
  filter(TrustNIAssembly2 != "Don't know"& TrustNIAssembly2 != "Refusal" |
           TrustMedia2 != "Don't know" & TrustMedia2 != "Refusal" |
           TrustNISRA2 != "Don't know" & TrustNISRA2 != "Refusal" |
           TrustNISRAstats2 != "Don't know" & TrustNISRAstats2 != "Refusal" |
           NISRAstatsImp2 != "Don't know" & NISRAstatsImp2 != "Refusal" |
           Political2 != "Don't know" & Political2 != "Refusal" |
           Confidential2 != "Don't know" & Confidential2 != "Refusal"
  )

data22 <- data22 %>%
  filter(TrustNIAssembly2 != "Don't know"& TrustNIAssembly2 != "Refusal" |
           TrustMedia2 != "Don't know" & TrustMedia2 != "Refusal" |
           TrustNISRA2 != "Don't know" & TrustNISRA2 != "Refusal" |
           TrustNISRAstats2 != "Don't know" & TrustNISRAstats2 != "Refusal" |
           NISRAstatsImp2 != "Don't know" & NISRAstatsImp2 != "Refusal" |
           Political2 != "Don't know" & Political2 != "Refusal" |
           Confidential2 != "Don't know" & Confidential2 != "Refusal"
  )

data22 <- data22 %>%
  filter(TrustNIAssembly2 == "Don't know"| TrustNIAssembly2 == "Refusal" &
           TrustMedia2 == "Don't know"| TrustMedia2 == "Refusal" &
           TrustNISRA2 == "Don't know"| TrustNISRA2 == "Refusal" &
           TrustNISRAstats2 == "Don't know"| TrustNISRAstats2 == "Refusal" &
           NISRAstatsImp2 == "Don't know"| NISRAstatsImp2 == "Refusal" &
           Political2 == "Don't know"| Political2 == "Refusal" &
           Confidential2 == "Don't know"| Confidential2 == "Refusal"
  )

# Merging factor levels
data22$TrustCivilService2 <- fct_collapse(data22$TrustCivilService2, 
                                          "Trust a great deal/Tend to trust" = c("Trust a great deal","Tend to trust"), 
                                          "Tend to distrust/Distrust greatly" = c("Tend to distrust", "Distrust greatly"))
table(data22$TrustCivilService2)

data22$TrustCivilService2 <- fct_collapse(data22$TrustCivilService2, 
                                          "Trust a great deal/Tend to trust" = c("Trust a great deal","Tend to trust"), 
                                          "Tend to distrust/Distrust greatly" = c("Tend to distrust", "Distrust greatly"))

data22$TrustNIAssembly2 <- fct_collapse(data22$TrustNIAssembly2, 
                                        "Trust a great deal/Tend to trust" = c("Trust a great deal","Tend to trust"), 
                                        "Tend to distrust/Distrust greatly" = c("Tend to distrust", "Distrust greatly"))

data22$TrustMedia2 <- fct_collapse(data22$TrustMedia2, 
                                   "Trust a great deal/Tend to trust" = c("Trust a great deal","Tend to trust"), 
                                   "Tend to distrust/Distrust greatly" = c("Tend to distrust", "Distrust greatly"))

data22$TrustNISRA2 <- fct_collapse(data22$TrustNISRA2, 
                                   "Trust a great deal/Tend to trust" = c("Trust a great deal","Tend to trust"), 
                                   "Tend to distrust/Distrust greatly" = c("Tend to distrust", "Distrust greatly"))

data22$TrustNISRAstats2 <- fct_collapse(data22$TrustNISRAstats2, 
                                        "Trust a great deal/Tend to trust" = c("Trust a great deal","Tend to trust"), 
                                        "Tend to distrust/Distrust greatly" = c("Tend to distrust", "Distrust greatly"))

data22$NISRAstatsImp2 <- fct_collapse(data22$NISRAstatsImp2, 
                                      "Trust a great deal/Tend to trust" = c("Trust a great deal","Tend to trust"), 
                                      "Tend to distrust/Distrust greatly" = c("Tend to distrust", "Distrust greatly"))

data22$Political2 <- fct_collapse(data22$Political2, 
                                  "Trust a great deal/Tend to trust" = c("Trust a great deal","Tend to trust"), 
                                  "Tend to distrust/Distrust greatly" = c("Tend to distrust", "Distrust greatly"))

data22$Confidential2 <- fct_collapse(data22$Confidential2, 
                                     "Trust a great deal/Tend to trust" = c("Trust a great deal","Tend to trust"), 
                                     "Tend to distrust/Distrust greatly" = c("Tend to distrust", "Distrust greatly"))

data22 <- data22 %>% 
  mutate(AGE2 = case_when(AGE >= 16 & AGE <= 24 ~ "16-24",
                          AGE >= 25 & AGE <= 34 ~ "25-34",
                          AGE >= 35 & AGE <= 44 ~ "35-44",
                          AGE >= 45 & AGE <= 54 ~ "45-54",
                          AGE >= 55 & AGE <= 64 ~ "55-64",
                          AGE >= 65 & AGE <= 74 ~ "65-74",
                          AGE >= 75 ~ "75 and over",
                          TRUE ~ NA_character_))
data22$AGE2 <- as.factor(data22$AGE2)

data22[c("DERHI")][data22[c("DERHI")] == "Other qualifications"] <- NA
table(data22$DERHI)

data22 <- data22 %>%
  rename("DERHIanalysis" = "DERHI")
  
data22[c("PCOS1", "TrustCivilService2", "TrustNIAssembly2", "TrustMedia2", "TrustNISRA2", "TrustNISRAstats2", 
         "NISRAstatsImp2", "Political2", "Confidential2",  "PCOS1c1", "PCOS1c2", "PCOS1c3", "PCOS1c4", "PCOS1c5", 
         "PCOS1c6", "PCOS1c7", "PCOS1c8", "PCOS1c9", "PCOS1d1", "PCOS1d2", "PCOS1d3", "PCOS1d4", "PCOS1d5", "PCOS1d6", 
         "PCOS1d7", "PCOS1d8")][data22[c("PCOS1", "TrustCivilService2", "TrustNIAssembly2", "TrustMedia2", "TrustNISRA2", 
                                         "TrustNISRAstats2", "NISRAstatsImp2", "Political2", "Confidential2",  "PCOS1c1", 
                                         "PCOS1c2", "PCOS1c3", "PCOS1c4", "PCOS1c5", "PCOS1c6", "PCOS1c7", "PCOS1c8", "PCOS1c9", 
                                         "PCOS1d1", "PCOS1d2", "PCOS1d3", "PCOS1d4", "PCOS1d5", "PCOS1d6", "PCOS1d7", 
                                         "PCOS1d8")] == "Refusal"] <- NA

#### Add dfs for tables and charts ####
