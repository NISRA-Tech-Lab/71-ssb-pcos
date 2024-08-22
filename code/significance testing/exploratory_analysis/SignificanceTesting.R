library(here)

# R Scripts containing bespoke functions for significance testing
source(paste0(here(), "/code/config.R"))
source(paste0(here(), "/code/significance testing/exploratory_analysis/functions/SignificanceTestingFunctions.R"))

### **** DATA **** ####
data_last <- readRDS(paste0(data_folder, "Final/PCOS 2021 Final Dataset.RDS"))
data_current <- readRDS(paste0(data_folder, "Final/PCOS 2022 Final Dataset.RDS"))

data_last$PCOS2a <- fct_collapse(data_last$PCOS2a, 
                                "yes" = c("Trust a great deal","Tend to trust"), 
                                "no" = c("Tend to distrust", "Distrust greatly"), 
                                "dont_know" = "DontKnow")

data_last$PCOS2b <- fct_collapse(data_last$PCOS2b, 
                                "yes" = c("Trust a great deal","Tend to trust"), 
                                "no" = c("Tend to distrust", "Distrust greatly"),
                                         "dont_know" = "DontKnow")

data_last$PCOS2c <- fct_collapse(data_last$PCOS2c, 
                               "yes" = c("Trust a great deal","Tend to trust"), 
                               "no" = c("Tend to distrust", "Distrust greatly"),
                                        "dont_know" = "DontKnow")

data_last$PCOS2d <- fct_collapse(data_last$PCOS2d, 
                               "yes" = c("Trust a great deal","Tend to trust"), 
                               "no" = c("Tend to distrust", "Distrust greatly"),
                                        "dont_know" = "DontKnow")

data_last$PCOS3 <- fct_collapse(data_last$PCOS3, 
                               "yes" = c("Tend to trust them", "Trust them greatly"), 
                               "no" = c("Tend not to trust them", "Distrust them greatly"), 
                               "dont_know" = "DontKnow")

data_last$PCOS4 <- fct_collapse(data_last$PCOS4, 
                               "yes" = c("Strongly agree","Tend to agree"), 
                               "no" = c("Tend to disagree", "Strongly disagree"), 
                               "dont_know" = "DontKnow")

data_last$PCOS5 <- fct_collapse(data_last$PCOS5, 
                               "yes" = c("Strongly agree","Tend to agree"), 
                               "no" = c("Tend to disagree", "Strongly disagree"), 
                               "dont_know" = "DontKnow")

data_last$PCOS6 <- fct_collapse(data_last$PCOS6, 
                               "yes" = c("Strongly agree","Tend to agree"), 
                               "no" = c("Tend to disagree", "Strongly disagree"), 
                               "dont_know" = "DontKnow")

data_current$PCOS2a <- fct_collapse(data_current$PCOS2a, 
                                            "yes" = c("Trust a great deal","Tend to trust"), 
                                            "no" = c("Tend to distrust", "Distrust greatly"), 
                                    "dont_know" = "DontKnow")

data_current$PCOS2b <- fct_collapse(data_current$PCOS2b, 
                                "yes" = c("Trust a great deal","Tend to trust"), 
                                "no" = c("Tend to distrust", "Distrust greatly"), 
                                "dont_know" = "DontKnow")

data_current$PCOS2c <- fct_collapse(data_current$PCOS2c, 
                                "yes" = c("Trust a great deal","Tend to trust"), 
                                "no" = c("Tend to distrust", "Distrust greatly"), 
                                "dont_know" = "DontKnow")

data_current$PCOS2d <- fct_collapse(data_current$PCOS2d, 
                                "yes" = c("Trust a great deal","Tend to trust"), 
                                "no" = c("Tend to distrust", "Distrust greatly"), 
                                "dont_know" = "DontKnow")

data_current$PCOS3 <- fct_collapse(data_current$PCOS3, 
                               "yes" = c("Tend to trust them", "Trust them greatly"), 
                               "no" = c("Tend not to trust them", "Distrust them greatly"), 
                               "dont_know" = "DontKnow")

data_current$PCOS4 <- fct_collapse(data_current$PCOS4, 
                               "yes" = c("Strongly agree","Tend to agree"), 
                               "no" = c("Tend to disagree", "Strongly disagree"), 
                               "dont_know" = "DontKnow")

data_current$PCOS5 <- fct_collapse(data_current$PCOS5, 
                               "yes" = c("Strongly agree","Tend to agree"), 
                               "no" = c("Tend to disagree", "Strongly disagree"), 
                               "dont_know" = "DontKnow")

data_current$PCOS6 <- fct_collapse(data_current$PCOS6, 
                               "yes" = c("Strongly agree","Tend to agree"), 
                               "no" = c("Tend to disagree", "Strongly disagree"), 
                               "dont_know" = "DontKnow")

data_current$TrustNISRA2 <- fct_collapse(data_current$PCOS6, 
                                         "yes" = c("Trust a great deal/Tend to trust"), 
                                         "no" = c("Tend to distrust/Distrust greatly"), 
                                         "dont_know" = "DontKnow")

data_current$AwareNISRA2 <- fct_collapse(data_current$PCOS6, 
                                      "yes" = c("Yes"), 
                                      "no" = c("No"), 
                                      "dont_know" = "DontKnow")

data_current$TrustCivilService2 <- fct_collapse(data_current$TrustCivilService2, 
                                         "yes" = c("Trust a great deal/Tend to trust"), 
                                         "no" = c("Tend to distrust/Distrust greatly"), 
                                         "dont_know" = "DontKnow")

data_current$TrustNIAssembly2 <- fct_collapse(data_current$TrustNIAssembly2, 
                                                "yes" = c("Trust a great deal/Tend to trust"), 
                                                "no" = c("Tend to distrust/Distrust greatly"), 
                                              "dont_know" = "DontKnow")

data_current$TrustMedia2 <- fct_collapse(data_current$TrustMedia2, 
                                              "yes" = c("Trust a great deal/Tend to trust"), 
                                              "no" = c("Tend to distrust/Distrust greatly"), 
                                         "dont_know" = "DontKnow")

data_current$TrustNISRA2 <- fct_collapse(data_current$TrustNISRA2, 
                                              "yes" = c("Trust a great deal/Tend to trust"), 
                                              "no" = c("Tend to distrust/Distrust greatly"), 
                                         "dont_know" = "DontKnow")

data_current$TrustNISRAstats2 <- fct_collapse(data_current$TrustNISRAstats2, 
                                              "yes" = c("Trust a great deal/Tend to trust"), 
                                              "no" = c("Tend to distrust/Distrust greatly"), 
                                              "dont_know" = "DontKnow")

data_current$Political2 <- fct_collapse(data_current$Political2, 
                                              "yes" = c("Strongly Agree/Tend to Agree"), 
                                              "no" = c("Tend to disagree/Strongly disagree"), 
                                        "dont_know" = "DontKnow")

data_current$Confidential2 <- fct_collapse(data_current$Confidential2, 
                                           "yes" = c("Strongly Agree/Tend to Agree"), 
                                           "no" = c("Tend to disagree/Strongly disagree"), 
                                           "dont_know" = "DontKnow")

data_current$NISRAstatsImp2 <- fct_collapse(data_current$Confidential2, 
                                            "yes" = c("Strongly Agree/Tend to Agree"), 
                                            "no" = c("Tend to disagree/Strongly disagree"), 
                                            "dont_know" = "DontKnow")

data_last$TrustNISRA2 <- fct_collapse(data_last$PCOS6, 
                                         "yes" = c("Trust a great deal/Tend to trust"), 
                                         "no" = c("Tend to distrust/Distrust greatly"), 
                                      "dont_know" = "DontKnow")

data_last$AwareNISRA2 <- fct_collapse(data_last$PCOS6, 
                                         "yes" = c("Yes"), 
                                         "no" = c("No"), 
                                      "dont_know" = "DontKnow")

data_last$TrustCivilService2 <- fct_collapse(data_last$TrustCivilService2, 
                                                "yes" = c("Trust a great deal/Tend to trust"), 
                                                "no" = c("Tend to distrust/Distrust greatly"), 
                                             "dont_know" = "DontKnow")

data_last$TrustNIAssembly2 <- fct_collapse(data_last$TrustNIAssembly2, 
                                              "yes" = c("Trust a great deal/Tend to trust"), 
                                              "no" = c("Tend to distrust/Distrust greatly"), 
                                           "dont_know" = "DontKnow")

data_last$TrustMedia2 <- fct_collapse(data_last$TrustMedia2, 
                                         "yes" = c("Trust a great deal/Tend to trust"), 
                                         "no" = c("Tend to distrust/Distrust greatly"), 
                                      "dont_know" = "DontKnow")

data_last$TrustNISRA2 <- fct_collapse(data_last$TrustNISRA2, 
                                         "yes" = c("Trust a great deal/Tend to trust"), 
                                         "no" = c("Tend to distrust/Distrust greatly"), 
                                      "dont_know" = "DontKnow")

data_last$TrustNISRAstats2 <- fct_collapse(data_last$TrustNISRAstats2, 
                                              "yes" = c("Trust a great deal/Tend to trust"), 
                                              "no" = c("Tend to distrust/Distrust greatly"), 
                                           "dont_know" = "DontKnow")

data_last$Political2 <- fct_collapse(data_last$Political2, 
                                        "yes" = c("Strongly Agree/Tend to Agree"), 
                                        "no" = c("Tend to disagree/Strongly disagree"), 
                                     "dont_know" = "DontKnow")

data_last$Confidential2 <- fct_collapse(data_last$Confidential2, 
                                           "yes" = c("Strongly Agree/Tend to Agree"), 
                                           "no" = c("Tend to disagree/Strongly disagree"), 
                                        "dont_know" = "DontKnow")

data_last$NISRAstatsImp2 <- fct_collapse(data_last$Confidential2, 
                                            "yes" = c("Strongly Agree/Tend to Agree"), 
                                            "no" = c("Tend to disagree/Strongly disagree"), 
                                         "dont_know" = "DontKnow")

### **** CURRENT YEAR **** ####
currentYear = "data_current"

# Re-labelling some fields
data_current$SEX <- recode(data_current$SEX, "M" = "Male", "F" = "Female")

### **** INPUTS **** ####
vars <- read.csv(paste0(here(), "/code/significance testing/exploratory_analysis/inputs/var.csv"))
groupings <- read.csv(paste0(here(), "/code/significance testing/exploratory_analysis/inputs/grouping.csv"))

### **** CODE **** ####
# Build dataframe for testing from vars and groupings inputs 
vardf <- stCombinations(vars, groupings, currentYear)

# flatten file to extract all variable info needed for significance testing
stVars <- rbindlist(split.default(as.data.table(vardf), c(0, sequence(ncol(vardf)-1) %/% 4)), use.names = FALSE) 
stVars <- unique(stVars) # remove duplicated entries
stVars <- stVars[rep(seq_len(nrow(stVars)), each = 3), ]
number_rows_stvars <- nrow(stVars)/3
answer <- c("yes", "no", "dont_know")
stVars$Answer <- rep(answer, number_rows_stvars)

#Extract question text
question<- apply(stVars, 1, function(y) extractQuestionText(y['year1'],y['var1']))
stVars$question <- "A"
stVars<- cbind(stVars, question) # add question text into stVars dataframe

#apply varAnalysis to extract the p and n values for each case in stVars
pnlist<- apply(stVars, 1, function(y) varAnalysis(y['year1'],y['group1'],y['grouping1'],y['var1'],y['Answer']))
allVars<- cbind(stVars, t(pnlist)) # add p and n values to stVars and rename to allVars
colnames(allVars) <- c('year', 'var','group','grouping', 'Answer', 'question','n','p') # rename columns for next stage

#Join the p, n and question text data to the relevant variables and groupings in vardf
vardf <- stCombinations(vars, groupings, currentYear)
vardf <- vardf[rep(seq_len(nrow(vardf)), each = 3), ]
number_rows_vardf <- nrow(vardf)/3
answer <- c("yes", "no", "dont_know")
vardf$Answer <- rep(answer, number_rows_vardf)
vardf <- vardf %>% left_join(allVars, by= c("var1"= "var", "group1" = "group", 
                                            "grouping1" = "grouping", "year1" = "year",
                                            "Answer" = "Answer"))#
colnames(vardf) <-  c( "year1", "var1","group1","grouping1", "year2","var2" ,"group2",
                       "grouping2", "Answer", "question1", "n", "p")

vardf <- vardf%>%  left_join(allVars, by= c("var2"= "var", "group2" = "group",
                                            "grouping2" = "grouping", "year2" = "year",
                                            "Answer" = "Answer")) 
colnames(vardf) <-  c("year1", "var1","group1","grouping1", "year2","var2" ,"group2",
                       "grouping2", "answer", "question1",
                       "n1","p1", "question2", "n2", "p2")

vardf$p1[is.na(vardf$p1)] <- 0
vardf$p2[is.na(vardf$p2)] <- 0

#apply function SignificanceTest to extract significance and directional info
for(i in 1:nrow(vardf)) {
  x  <- significanceTest(vardf[i,'p1'],vardf[i,'n1'],vardf[i,'p2'],vardf[i,'n2'])
  vardf[i,'significance'] = x['significance']
  vardf[i,'direction'] = x['direction']
  vardf[i,'score'] = x['score']
}

#structure the data in a more friendly way for presenting and order so that significant changes are first
vardf <- vardf %>% 
  select(year1, group1, grouping1, var1, question1, n1, p1, year2, group2, grouping2, var2, answer, n2, p2,significance, direction, score) %>%
  arrange(desc(significance))

outputfile <- paste0('outputs/significanceTestingAll2022-', Sys.Date(), '.csv') 

write.csv(vardf, outputfile) #a csv file with the results will be written to the outputs folder.

wb2 <- createWorkbook()
modifyBaseFont(wb2, fontSize = 12, fontName = "Arial")
addWorksheet(wb2, "TrustCivilService2")
addWorksheet(wb2, "TrustNIAssembly2")
addWorksheet(wb2, "TrustMedia2")
addWorksheet(wb2, "TrustNISRA2")
addWorksheet(wb2, "TrustNISRAstats2")
addWorksheet(wb2, "NISRAstatsImp2")
addWorksheet(wb2, "Political2")
addWorksheet(wb2, "Confidential2")
addWorksheet(wb2, "PCOS1c1")
addWorksheet(wb2, "PCOS1c2")
addWorksheet(wb2, "PCOS1c3")
addWorksheet(wb2, "PCOS1c4")
addWorksheet(wb2, "PCOS1c5")
addWorksheet(wb2, "PCOS1c6")
addWorksheet(wb2, "PCOS1c7")
addWorksheet(wb2, "PCOS1c8")
addWorksheet(wb2, "PCOS1c9")
addWorksheet(wb2, "PCOS1d1")
addWorksheet(wb2, "PCOS1d2")
addWorksheet(wb2, "PCOS1d3")
addWorksheet(wb2, "PCOS1d4")
addWorksheet(wb2, "PCOS1d5")
addWorksheet(wb2, "PCOS1d6")
addWorksheet(wb2, "PCOS1d7")
addWorksheet(wb2, "PCOS1d8")
addWorksheet(wb2, "PCOS1d9")

excel_df <- vardf %>%
  mutate(year1 = case_when(year1 == "data_current" ~ current_year,
                           year1 == "data_last" ~ current_year - 1),
         year2 = case_when(year2 == "data_current" ~ current_year,
                           year2 == "data_last" ~ current_year - 1),
         score = as.numeric(score)) %>%
  select(year1, grouping1, var1, year2, grouping2, var2, answer, score) %>%
  rename(`Grouping 1` = grouping1, `Grouping 2` = grouping2, `z Score` = score) %>%
  arrange(var1)
  
# Sorting columns
grouping_1_order <- c("Male", "16-24", "25-34", "35-44", "45-54", "55-64",
                      "65-74", 
                      "All", "A levels, vocational level 3 and equivalents",
                      "Degree, or Degree equivalent and above",
                      "GCSE/O level grade A*-C. vocational level 2 and equivalents",
                      "Other higher education below degree level",
                      "Qualifications at level 1 and below", "In paid employment")

grouping_2_order <- c("Female", 
                      "25-34", "35-44", "45-54", "55-64", "65-74", "75 and over",
                      "All", "A levels, vocational level 3 and equivalents", 
                      "GCSE/O level grade A*-C. vocational level 2 and equivalents",
                      "No qualification",
                      "Other higher education below degree level",
                      "Qualifications at level 1 and below", 
                      "Not in paid employment")

TrustMedia2_df <- subset(excel_df, var1 == "TrustMedia2") %>%
  select(-var1, -var2) %>%
  mutate(`Grouping 1` = factor(`Grouping 1`, levels = c(grouping_1_order))) %>%
  mutate(`Grouping 2` = factor(`Grouping 2`, levels = c(grouping_2_order))) %>%
  arrange(`Grouping 1`, `Grouping 2`)

TrustNISRA2_df <- subset(excel_df, var1 == "TrustNISRA2") %>%
  select(-var1, -var2) %>%
  mutate(`Grouping 1` = factor(`Grouping 1`, levels = c(grouping_1_order))) %>%
  mutate(`Grouping 2` = factor(`Grouping 2`, levels = c(grouping_2_order))) %>%
  arrange(`Grouping 1`, `Grouping 2`)

TrustNISRAstats2_df <- subset(excel_df, var1 == "TrustNISRAstats2") %>%
  select(-var1, -var2) %>%
  mutate(`Grouping 1` = factor(`Grouping 1`, levels = c(grouping_1_order))) %>%
  mutate(`Grouping 2` = factor(`Grouping 2`, levels = c(grouping_2_order))) %>%
  arrange(`Grouping 1`, `Grouping 2`)

NISRAstatsImp2_df <- subset(excel_df, var1 == "NISRAstatsImp2") %>%
  select(-var1, -var2) %>%
  mutate(`Grouping 1` = factor(`Grouping 1`, levels = c(grouping_1_order))) %>%
  mutate(`Grouping 2` = factor(`Grouping 2`, levels = c(grouping_2_order))) %>%
  arrange(`Grouping 1`, `Grouping 2`)

Political2_df <- subset(excel_df, var1 == "Political2") %>%
  select(-var1, -var2) %>%
  mutate(`Grouping 1` = factor(`Grouping 1`, levels = c(grouping_1_order))) %>%
  mutate(`Grouping 2` = factor(`Grouping 2`, levels = c(grouping_2_order))) %>%
  arrange(`Grouping 1`, `Grouping 2`)

Confidential2_df <- subset(excel_df, var1 == "Confidential2") %>%
  select(-var1, -var2) %>%
  mutate(`Grouping 1` = factor(`Grouping 1`, levels = c(grouping_1_order))) %>%
  mutate(`Grouping 2` = factor(`Grouping 2`, levels = c(grouping_2_order))) %>%
  arrange(`Grouping 1`, `Grouping 2`)

PCOS1c1_df <- subset(excel_df, var1 == "PCOS1c1") %>%
  select(-var1, -var2) %>%
  mutate(`Grouping 1` = factor(`Grouping 1`, levels = c(grouping_1_order))) %>%
  mutate(`Grouping 2` = factor(`Grouping 2`, levels = c(grouping_2_order))) %>%
  arrange(`Grouping 1`, `Grouping 2`)

PCOS1c2_df <- subset(excel_df, var1 == "PCOS1c2") %>%
  select(-var1, -var2) %>%
  mutate(`Grouping 1` = factor(`Grouping 1`, levels = c(grouping_1_order))) %>%
  mutate(`Grouping 2` = factor(`Grouping 2`, levels = c(grouping_2_order))) %>%
  arrange(`Grouping 1`, `Grouping 2`)

PCOS1c3_df <- subset(excel_df, var1 == "PCOS1c3") %>%
  select(-var1, -var2) %>%
  mutate(`Grouping 1` = factor(`Grouping 1`, levels = c(grouping_1_order))) %>%
  mutate(`Grouping 2` = factor(`Grouping 2`, levels = c(grouping_2_order))) %>%
  arrange(`Grouping 1`, `Grouping 2`)

PCOS1c4_df <- subset(excel_df, var1 == "PCOS1c4") %>%
  select(-var1, -var2) %>%
  mutate(`Grouping 1` = factor(`Grouping 1`, levels = c(grouping_1_order))) %>%
  mutate(`Grouping 2` = factor(`Grouping 2`, levels = c(grouping_2_order))) %>%
  arrange(`Grouping 1`, `Grouping 2`)

PCOS1c5_df <- subset(excel_df, var1 == "PCOS1c5") %>%
  select(-var1, -var2) %>%
  mutate(`Grouping 1` = factor(`Grouping 1`, levels = c(grouping_1_order))) %>%
  mutate(`Grouping 2` = factor(`Grouping 2`, levels = c(grouping_2_order))) %>%
  arrange(`Grouping 1`, `Grouping 2`)

PCOS1c6_df <- subset(excel_df, var1 == "PCOS1c6") %>%
  select(-var1, -var2) %>%
  mutate(`Grouping 1` = factor(`Grouping 1`, levels = c(grouping_1_order))) %>%
  mutate(`Grouping 2` = factor(`Grouping 2`, levels = c(grouping_2_order))) %>%
  arrange(`Grouping 1`, `Grouping 2`)

PCOS1c7_df <- subset(excel_df, var1 == "PCOS1c7") %>%
  select(-var1, -var2) %>%
  mutate(`Grouping 1` = factor(`Grouping 1`, levels = c(grouping_1_order))) %>%
  mutate(`Grouping 2` = factor(`Grouping 2`, levels = c(grouping_2_order))) %>%
  arrange(`Grouping 1`, `Grouping 2`)

PCOS1c8_df <- subset(excel_df, var1 == "PCOS1c8") %>%
  select(-var1, -var2) %>%
  mutate(`Grouping 1` = factor(`Grouping 1`, levels = c(grouping_1_order))) %>%
  mutate(`Grouping 2` = factor(`Grouping 2`, levels = c(grouping_2_order))) %>%
  arrange(`Grouping 1`, `Grouping 2`)

PCOS1c9_df <- subset(excel_df, var1 == "PCOS1c9") %>%
  select(-var1, -var2) %>%
  mutate(`Grouping 1` = factor(`Grouping 1`, levels = c(grouping_1_order))) %>%
  mutate(`Grouping 2` = factor(`Grouping 2`, levels = c(grouping_2_order))) %>%
  arrange(`Grouping 1`, `Grouping 2`)

PCOS1d1_df <- subset(excel_df, var1 == "PCOS1d1") %>%
  select(-var1, -var2) %>%
  mutate(`Grouping 1` = factor(`Grouping 1`, levels = c(grouping_1_order))) %>%
  mutate(`Grouping 2` = factor(`Grouping 2`, levels = c(grouping_2_order))) %>%
  arrange(`Grouping 1`, `Grouping 2`)

PCOS1d2_df <- subset(excel_df, var1 == "PCOS1d2") %>%
  select(-var1, -var2) %>%
  mutate(`Grouping 1` = factor(`Grouping 1`, levels = c(grouping_1_order))) %>%
  mutate(`Grouping 2` = factor(`Grouping 2`, levels = c(grouping_2_order))) %>%
  arrange(`Grouping 1`, `Grouping 2`)

PCOS1d3_df <- subset(excel_df, var1 == "PCOS1d3") %>%
  select(-var1, -var2) %>%
  mutate(`Grouping 1` = factor(`Grouping 1`, levels = c(grouping_1_order))) %>%
  mutate(`Grouping 2` = factor(`Grouping 2`, levels = c(grouping_2_order))) %>%
  arrange(`Grouping 1`, `Grouping 2`)

PCOS1d4_df <- subset(excel_df, var1 == "PCOS1d4") %>%
  select(-var1, -var2) %>%
  mutate(`Grouping 1` = factor(`Grouping 1`, levels = c(grouping_1_order))) %>%
  mutate(`Grouping 2` = factor(`Grouping 2`, levels = c(grouping_2_order))) %>%
  arrange(`Grouping 1`, `Grouping 2`)

PCOS1d5_df <- subset(excel_df, var1 == "PCOS1d5") %>%
  select(-var1, -var2) %>%
  mutate(`Grouping 1` = factor(`Grouping 1`, levels = c(grouping_1_order))) %>%
  mutate(`Grouping 2` = factor(`Grouping 2`, levels = c(grouping_2_order))) %>%
  arrange(`Grouping 1`, `Grouping 2`)

PCOS1d6_df <- subset(excel_df, var1 == "PCOS1d6") %>%
  select(-var1, -var2) %>%
  mutate(`Grouping 1` = factor(`Grouping 1`, levels = c(grouping_1_order))) %>%
  mutate(`Grouping 2` = factor(`Grouping 2`, levels = c(grouping_2_order))) %>%
  arrange(`Grouping 1`, `Grouping 2`)

PCOS1d7_df <- subset(excel_df, var1 == "PCOS1d7") %>%
  select(-var1, -var2) %>%
  mutate(`Grouping 1` = factor(`Grouping 1`, levels = c(grouping_1_order))) %>%
  mutate(`Grouping 2` = factor(`Grouping 2`, levels = c(grouping_2_order))) %>%
  arrange(`Grouping 1`, `Grouping 2`)

PCOS1d8_df <- subset(excel_df, var1 == "PCOS1d8") %>%
  select(-var1, -var2) %>%
  mutate(`Grouping 1` = factor(`Grouping 1`, levels = c(grouping_1_order))) %>%
  mutate(`Grouping 2` = factor(`Grouping 2`, levels = c(grouping_2_order))) %>%
  arrange(`Grouping 1`, `Grouping 2`)

PCOS1d9_df <- subset(excel_df, var1 == "PCOS1d9") %>%
  select(-var1, -var2) %>%
  mutate(`Grouping 1` = factor(`Grouping 1`, levels = c(grouping_1_order))) %>%
  mutate(`Grouping 2` = factor(`Grouping 2`, levels = c(grouping_2_order))) %>%
  arrange(`Grouping 1`, `Grouping 2`)

TrustCivilService2_df <- subset(excel_df, var1 == "TrustCivilService2") %>%
  select(-var1, -var2) %>%
  mutate(`Grouping 1` = factor(`Grouping 1`, levels = c(grouping_1_order))) %>%
  mutate(`Grouping 2` = factor(`Grouping 2`, levels = c(grouping_2_order))) %>%
  arrange(`Grouping 1`, `Grouping 2`)

TrustNIAssembly2_df <- subset(excel_df, var1 == "TrustNIAssembly2") %>%
  select(-var1, -var2) %>%
  mutate(`Grouping 1` = factor(`Grouping 1`, levels = c(grouping_1_order))) %>%
  mutate(`Grouping 2` = factor(`Grouping 2`, levels = c(grouping_2_order))) %>%
  arrange(`Grouping 1`, `Grouping 2`)

sig_df <- data.frame(Table_Name = c("TrustCivilService2", "TrustNIAssembly2",
                                    "TrustMedia2", "TrustNISRA2", "TrustNISRAstats2",
                                    "NISRAstatsImp2", "Political2", "Confidential2", 
                                    "PCOS1c1", "PCOS1c2", "PCOS1c3", "PCOS1c4",
                                    "PCOS1c5", "PCOS1c6", "PCOS1c7", "PCOS1c8",
                                    "PCOS1c9", "PCOS1d1", "PCOS1d2", "PCOS1d3", 
                                    "PCOS1d4", "PCOS1d5", "PCOS1d6", "PCOS1d7",
                                    "PCOS1d8", "PCOS1d9"), 
                     Sheet = c("TrustCivilService2", "TrustNIAssembly2",
                               "TrustMedia2", "TrustNISRA2", "TrustNISRAstats2",
                               "NISRAstatsImp2", "Political2", "Confidential2", 
                               "PCOS1c1", "PCOS1c2", "PCOS1c3", "PCOS1c4",
                               "PCOS1c5", "PCOS1c6", "PCOS1c7", "PCOS1c8",
                               "PCOS1c9", "PCOS1d1", "PCOS1d2", "PCOS1d3", 
                               "PCOS1d4", "PCOS1d5", "PCOS1d6", "PCOS1d7",
                               "PCOS1d8", "PCOS1d9"))

r <- 1
r <- r + 1

for (i in 1:nrow(sig_df)) {
  
  df <- sig_df[i,]
  
  writeDataTable(wb2, 
                 sheet = paste0(df$Sheet),
                 x = get(paste0(df$Table_Name, "_df")),
                 startRow = r,
                 startCol = 1,
                 colNames = TRUE,
                 tableStyle = "none",
                 tableName = df$Table_Name,
                 withFilter = FALSE,
                 bandedRows = FALSE)
  
}

for (i in 1:nrow(TrustCivilService2_df)) {
  if (!is.na(TrustCivilService2_df[i, 6])) {
    if (abs(TrustCivilService2_df[i, 5]) > qnorm(0.975)) {
      addStyle(wb2, "TrustCivilService2",
               style = sig,
               rows = r + i,
               cols = 6)
    } else {
      addStyle(wb2, "TrustCivilService2",
               style = not_sig,
               rows = r + i,
               cols = 6)
    }
  }
}

for (i in 1:nrow(TrustNIAssembly2_df)) {
  if (!is.na(TrustNIAssembly2_df[i, 6])) {
    if (abs(TrustNIAssembly2_df[i, 6]) > qnorm(0.975)) {
      addStyle(wb2, "TrustNIAssembly2",
               style = sig,
               rows = r + i,
               cols = 6)
    } else {
      addStyle(wb2, "TrustNIAssembly2",
               style = not_sig,
               rows = r + i,
               cols = 6)
    }
  }
}

for (i in 1:nrow(TrustMedia2_df)) {
  if (!is.na(TrustMedia2_df[i, 6])) {
    if (abs(TrustMedia2_df[i, 6]) > qnorm(0.975)) {
      addStyle(wb2, "TrustMedia2",
               style = sig,
               rows = r + i,
               cols = 6)
    } else {
      addStyle(wb2, "TrustMedia2",
               style = not_sig,
               rows = r + i,
               cols = 6)
    }
  }
}


for (i in 1:nrow(TrustNISRA2_df)) {
  if (!is.na(TrustNISRA2_df[i, 6])) {
    if (abs(TrustNISRA2_df[i, 6]) > qnorm(0.975)) {
      addStyle(wb2, "TrustNISRA2",
               style = sig,
               rows = r + i,
               cols = 6)
    } else {
      addStyle(wb2, "TrustNISRA2",
               style = not_sig,
               rows = r + i,
               cols = 6)
    }
  }
}

for (i in 1:nrow(TrustNISRAstats2_df)) {
  if (!is.na(TrustNISRAstats2_df[i, 6])) {
    if (abs(TrustNISRAstats2_df[i, 6]) > qnorm(0.975)) {
      addStyle(wb2, "TrustNISRAstats2",
               style = sig,
               rows = r + i,
               cols = 6)
    } else {
      addStyle(wb2, "TrustNISRAstats2",
               style = not_sig,
               rows = r + i,
               cols = 6)
    }
  }
}

for (i in 1:nrow(NISRAstatsImp2_df)) {
  if (!is.na(NISRAstatsImp2_df[i, 6])) {
    if (abs(NISRAstatsImp2_df[i, 6]) > qnorm(0.975)) {
      addStyle(wb2, "NISRAstatsImp2",
               style = sig,
               rows = r + i,
               cols = 6)
    } else {
      addStyle(wb2, "NISRAstatsImp2",
               style = not_sig,
               rows = r + i,
               cols = 6)
    }
  }
}

for (i in 1:nrow(Political2_df)) {
  if (!is.na(Political2_df[i, 6])) {
    if (abs(Political2_df[i, 6]) > qnorm(0.975)) {
      addStyle(wb2, "Political2",
               style = sig,
               rows = r + i,
               cols = 6)
    } else {
      addStyle(wb2, "Political2",
               style = not_sig,
               rows = r + i,
               cols = 6)
    }
  }
}

for (i in 1:nrow(Confidential2_df)) {
  if (!is.na(Confidential2_df[i, 6])) {
    if (abs(Confidential2_df[i, 6]) > qnorm(0.975)) {
      addStyle(wb2, "Confidential2",
               style = sig,
               rows = r + i,
               cols = 6)
    } else {
      addStyle(wb2, "Confidential2",
               style = not_sig,
               rows = r + i,
               cols = 6)
    }
  }
}

for (i in 1:nrow(PCOS1c1_df)) {
  if (!is.na(PCOS1c1_df[i, 6])) {
    if (abs(PCOS1c1_df[i, 6]) > qnorm(0.975)) {
      addStyle(wb2, "PCOS1c1",
               style = sig,
               rows = r + i,
               cols = 6)
    } else {
      addStyle(wb2, "PCOS1c1",
               style = not_sig,
               rows = r + i,
               cols = 6)
    }
  }
}

for (i in 1:nrow(PCOS1c2_df)) {
  if (!is.na(PCOS1c2_df[i, 6])) {
    if (abs(PCOS1c2_df[i, 6]) > qnorm(0.975)) {
      addStyle(wb2, "PCOS1c2",
               style = sig,
               rows = r + i,
               cols = 6)
    } else {
      addStyle(wb2, "PCOS1c2",
               style = not_sig,
               rows = r + i,
               cols = 6)
    }
  }
}

for (i in 1:nrow(PCOS1c3_df)) {
  if (!is.na(PCOS1c3_df[i, 6])) {
    if (abs(PCOS1c3_df[i, 6]) > qnorm(0.975)) {
      addStyle(wb2, "PCOS1c3",
               style = sig,
               rows = r + i,
               cols = 6)
    } else {
      addStyle(wb2, "PCOS1c3",
               style = not_sig,
               rows = r + i,
               cols = 6)
    }
  }
}

for (i in 1:nrow(PCOS1c4_df)) {
  if (!is.na(PCOS1c4_df[i, 6])) {
    if (abs(PCOS1c4_df[i, 6]) > qnorm(0.975)) {
      addStyle(wb2, "PCOS1c4",
               style = sig,
               rows = r + i,
               cols = 6)
    } else {
      addStyle(wb2, "PCOS1c4",
               style = not_sig,
               rows = r + i,
               cols = 6)
    }
  }
}

for (i in 1:nrow(PCOS1c5_df)) {
  if (!is.na(PCOS1c5_df[i, 6])) {
    if (abs(PCOS1c5_df[i, 6]) > qnorm(0.975)) {
      addStyle(wb2, "PCOS1c5",
               style = sig,
               rows = r + i,
               cols = 6)
    } else {
      addStyle(wb2, "PCOS1c5",
               style = not_sig,
               rows = r + i,
               cols = 6)
    }
  }
}

for (i in 1:nrow(PCOS1c6_df)) {
  if (!is.na(PCOS1c6_df[i, 6])) {
    if (abs(PCOS1c6_df[i, 6]) > qnorm(0.975)) {
      addStyle(wb2, "PCOS1c6",
               style = sig,
               rows = r + i,
               cols = 6)
    } else {
      addStyle(wb2, "PCOS1c6",
               style = not_sig,
               rows = r + i,
               cols = 6)
    }
  }
}

for (i in 1:nrow(PCOS1c7_df)) {
  if (!is.na(PCOS1c7_df[i, 6])) {
    if (abs(PCOS1c7_df[i, 6]) > qnorm(0.975)) {
      addStyle(wb2, "PCOS1c7",
               style = sig,
               rows = r + i,
               cols = 6)
    } else {
      addStyle(wb2, "PCOS1c7",
               style = not_sig,
               rows = r + i,
               cols = 6)
    }
  }
}

for (i in 1:nrow(PCOS1c8_df)) {
  if (!is.na(PCOS1c8_df[i, 6])) {
    if (abs(PCOS1c8_df[i, 6]) > qnorm(0.975)) {
      addStyle(wb2, "PCOS1c8",
               style = sig,
               rows = r + i,
               cols = 6)
    } else {
      addStyle(wb2, "PCOS1c8",
               style = not_sig,
               rows = r + i,
               cols = 6)
    }
  }
}

for (i in 1:nrow(PCOS1c9_df)) {
  if (!is.na(PCOS1c9_df[i, 6])) {
    if (abs(PCOS1c9_df[i, 6]) > qnorm(0.975)) {
      addStyle(wb2, "PCOS1c9",
               style = sig,
               rows = r + i,
               cols = 6)
    } else {
      addStyle(wb2, "PCOS1c9",
               style = not_sig,
               rows = r + i,
               cols = 6)
    }
  }
}

for (i in 1:nrow(PCOS1d1_df)) {
  if (!is.na(PCOS1d1_df[i, 6])) {
    if (abs(PCOS1d1_df[i, 6]) > qnorm(0.975)) {
      addStyle(wb2, "PCOS1d1",
               style = sig,
               rows = r + i,
               cols = 6)
    } else {
      addStyle(wb2, "PCOS1d1",
               style = not_sig,
               rows = r + i,
               cols = 6)
    }
  }
}

for (i in 1:nrow(PCOS1d2_df)) {
  if (!is.na(PCOS1d2_df[i, 6])) {
    if (abs(PCOS1d2_df[i, 6]) > qnorm(0.975)) {
      addStyle(wb2, "PCOS1d2",
               style = sig,
               rows = r + i,
               cols = 6)
    } else {
      addStyle(wb2, "PCOS1d2",
               style = not_sig,
               rows = r + i,
               cols = 6)
    }
  }
}

for (i in 1:nrow(PCOS1d3_df)) {
  if (!is.na(PCOS1d3_df[i, 6])) {
    if (abs(PCOS1d3_df[i, 6]) > qnorm(0.975)) {
      addStyle(wb2, "PCOS1d3",
               style = sig,
               rows = r + i,
               cols = 6)
    } else {
      addStyle(wb2, "PCOS1d3",
               style = not_sig,
               rows = r + i,
               cols = 6)
    }
  }
}

for (i in 1:nrow(PCOS1d4_df)) {
  if (!is.na(PCOS1d4_df[i, 6])) {
    if (abs(PCOS1d4_df[i, 6]) > qnorm(0.975)) {
      addStyle(wb2, "PCOS1d4",
               style = sig,
               rows = r + i,
               cols = 6)
    } else {
      addStyle(wb2, "PCOS1d4",
               style = not_sig,
               rows = r + i,
               cols = 6)
    }
  }
}

for (i in 1:nrow(PCOS1d5_df)) {
  if (!is.na(PCOS1d5_df[i, 6])) {
    if (abs(PCOS1d5_df[i, 6]) > qnorm(0.975)) {
      addStyle(wb2, "PCOS1d5",
               style = sig,
               rows = r + i,
               cols = 6)
    } else {
      addStyle(wb2, "PCOS1d5",
               style = not_sig,
               rows = r + i,
               cols = 6)
    }
  }
}

for (i in 1:nrow(PCOS1d6_df)) {
  if (!is.na(PCOS1d6_df[i, 6])) {
    if (abs(PCOS1d6_df[i, 6]) > qnorm(0.975)) {
      addStyle(wb2, "PCOS1d6",
               style = sig,
               rows = r + i,
               cols = 6)
    } else {
      addStyle(wb2, "PCOS1d6",
               style = not_sig,
               rows = r + i,
               cols = 6)
    }
  }
}

for (i in 1:nrow(PCOS1d7_df)) {
  if (!is.na(PCOS1d7_df[i, 6])) {
    if (abs(PCOS1d7_df[i, 6]) > qnorm(0.975)) {
      addStyle(wb2, "PCOS1d7",
               style = sig,
               rows = r + i,
               cols = 6)
    } else {
      addStyle(wb2, "PCOS1d7",
               style = not_sig,
               rows = r + i,
               cols = 6)
    }
  }
}

for (i in 1:nrow(PCOS1d8_df)) {
  if (!is.na(PCOS1d8_df[i, 6])) {
    if (abs(PCOS1d8_df[i, 6]) > qnorm(0.975)) {
      addStyle(wb2, "PCOS1d8",
               style = sig,
               rows = r + i,
               cols = 6)
    } else {
      addStyle(wb2, "PCOS1d8",
               style = not_sig,
               rows = r + i,
               cols = 6)
    }
  }
}

for (i in 1:nrow(PCOS1d9_df)) {
  if (!is.na(PCOS1d9_df[i, 6])) {
    if (abs(PCOS1d9_df[i, 6]) > qnorm(0.975)) {
      addStyle(wb2, "PCOS1d9",
               style = sig,
               rows = r + i,
               cols = 6)
    } else {
      addStyle(wb2, "PCOS1d9",
               style = not_sig,
               rows = r + i,
               cols = 6)
    }
  }
}

saveWorkbook(wb2,
             paste0(here(), "/outputs/significance outputs/exploratory significance output ", current_year, ".xlsx"),
             overwrite = TRUE)

