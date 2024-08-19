library(here)

# R Scripts containing bespoke functions for significance testing
source(paste0(here(), "/code/config.R"))
source(paste0(here(), "/code/significance testing/exploratory_analysis/functions/SignificanceTestingFunctions.R"))

### **** DATA **** ####
data_last <- readRDS(paste0(data_folder, "Final/PCOS 2021 Final Dataset.RDS"))
data_current <- readRDS(paste0(data_folder, "Final/PCOS 2022 Final Dataset.RDS"))

data_last$PCOS2a <- fct_collapse(data_last$PCOS2a, 
                                "yes" = c("Trust a great deal","Tend to trust"), 
                                "no" = c("Tend to distrust", "Distrust greatly"))

data_last$PCOS2b <- fct_collapse(data_last$PCOS2b, 
                                "yes" = c("Trust a great deal","Tend to trust"), 
                                "no" = c("Tend to distrust", "Distrust greatly"))

data_last$PCOS2c <- fct_collapse(data_last$PCOS2c, 
                               "yes" = c("Trust a great deal","Tend to trust"), 
                               "no" = c("Tend to distrust", "Distrust greatly"))

data_last$PCOS2d <- fct_collapse(data_last$PCOS2d, 
                               "yes" = c("Trust a great deal","Tend to trust"), 
                               "no" = c("Tend to distrust", "Distrust greatly"))

data_last$PCOS3 <- fct_collapse(data_last$PCOS3, 
                               "yes" = c("Tend to trust them", "Trust them greatly"), 
                               "no" = c("Tend not to trust them", "Distrust them greatly"))

data_last$PCOS4 <- fct_collapse(data_last$PCOS4, 
                               "yes" = c("Strongly agree","Tend to agree"), 
                               "no" = c("Tend to disagree", "Strongly disagree"))

data_last$PCOS5 <- fct_collapse(data_last$PCOS5, 
                               "yes" = c("Strongly agree","Tend to agree"), 
                               "no" = c("Tend to disagree", "Strongly disagree"))

data_last$PCOS6 <- fct_collapse(data_last$PCOS6, 
                               "yes" = c("Strongly agree","Tend to agree"), 
                               "no" = c("Tend to disagree", "Strongly disagree"))

data_current$PCOS2a <- fct_collapse(data_current$PCOS2a, 
                                            "yes" = c("Trust a great deal","Tend to trust"), 
                                            "no" = c("Tend to distrust", "Distrust greatly"))

data_current$PCOS2b <- fct_collapse(data_current$PCOS2b, 
                                "yes" = c("Trust a great deal","Tend to trust"), 
                                "no" = c("Tend to distrust", "Distrust greatly"))

data_current$PCOS2c <- fct_collapse(data_current$PCOS2c, 
                                "yes" = c("Trust a great deal","Tend to trust"), 
                                "no" = c("Tend to distrust", "Distrust greatly"))

data_current$PCOS2d <- fct_collapse(data_current$PCOS2d, 
                                "yes" = c("Trust a great deal","Tend to trust"), 
                                "no" = c("Tend to distrust", "Distrust greatly"))

data_current$PCOS3 <- fct_collapse(data_current$PCOS3, 
                               "yes" = c("Tend to trust them", "Trust them greatly"), 
                               "no" = c("Tend not to trust them", "Distrust them greatly"))

data_current$PCOS4 <- fct_collapse(data_current$PCOS4, 
                               "yes" = c("Strongly agree","Tend to agree"), 
                               "no" = c("Tend to disagree", "Strongly disagree"))

data_current$PCOS5 <- fct_collapse(data_current$PCOS5, 
                               "yes" = c("Strongly agree","Tend to agree"), 
                               "no" = c("Tend to disagree", "Strongly disagree"))

data_current$PCOS6 <- fct_collapse(data_current$PCOS6, 
                               "yes" = c("Strongly agree","Tend to agree"), 
                               "no" = c("Tend to disagree", "Strongly disagree"))

data_current$TrustNISRA2 <- fct_collapse(data_current$PCOS6, 
                                         "yes" = c("Trust a great deal/Tend to trust"), 
                                         "no" = c("Tend to distrust/Distrust greatly"))

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

#Extract question text
question<- apply(stVars, 1, function(y) extractQuestionText(y['year1'],y['var1']))
stVars$question <- "A"
stVars<- cbind(stVars, question) # add question text into stVars dataframe

#apply varAnalysis to extract the p and n values for each case in stVars
pnlist<- apply(stVars, 1, function(y) varAnalysis(y['year1'],y['group1'],y['grouping1'],y['var1']))
allVars<- cbind(stVars, t(pnlist)) # add p and n values to stVars and rename to allVars
colnames(allVars) <- c('year', 'var','group','grouping', 'question','n','p') # rename columns for next stage

#Join the p, n and question text data to the relevant variables and groupings in vardf
vardf <- vardf %>% left_join(allVars, by= c("var1"= "var", "group1" = "group", "grouping1" = "grouping", "year1" = "year"))#
colnames(vardf) <-  c( "year1", "var1","group1","grouping1", "year2","var2" ,"group2","grouping2","question1", "n1","p1" )
vardf <- vardf%>%  left_join(allVars, by= c("var2"= "var", "group2" = "group", "grouping2" = "grouping", "year2" = "year")) 
colnames(vardf) <-  c( "year1", "var1","group1","grouping1", "year2","var2" ,"group2","grouping2","question1", "n1","p1", "question2", "n2", "p2" )

#apply function SignificanceTest to extract significance and directional info
for(i in 1:nrow(vardf)) {
  x  <- significanceTest(vardf[i,'p1'],vardf[i,'n1'],vardf[i,'p2'],vardf[i,'n2'])
  vardf[i,'significance'] = x['significance']
  vardf[i,'direction'] = x['direction']
  vardf[i,'score'] = x['score']
}

#structure the data in a more friendly way for presenting and order so that significant changes are first
vardf <- vardf %>% 
  select(year1, group1, grouping1, var1, question1, n1, p1, year2, group2, grouping2, var2, question2, n2, p2,significance, direction, score) %>%
  arrange(desc(significance))

outputfile <- paste0('outputs/significanceTestingAll2022-', Sys.Date(), '.csv') 

write.csv(vardf, outputfile) #a csv file with the results will be written to the outputs folder.


wb2 <- createWorkbook()
modifyBaseFont(wb2, fontSize = 12, fontName = "Arial")
addWorksheet(wb2, "PCOS1")
addWorksheet(wb2, "PCOS1c")
addWorksheet(wb2, "PCOS1d")
addWorksheet(wb2, "PCOS2a")
addWorksheet(wb2, "PCOS2b")
addWorksheet(wb2, "PCOS2c")
addWorksheet(wb2, "PCOS2d")
addWorksheet(wb2, "PCOS3")
addWorksheet(wb2, "PCOS4")
addWorksheet(wb2, "PCOS5")
addWorksheet(wb2, "PCOS6")

excel_df <- vardf %>%
  mutate(year1 = case_when(year1 == "data_current" ~ current_year,
                           year1 == "data_last" ~ current_year - 1),
         year2 = case_when(year2 == "data_current" ~ current_year,
                           year2 == "data_last" ~ current_year - 1),
         score = as.numeric(score)) %>%
  select(year1, grouping1, var1, year2, grouping2, var2, score) %>%
  rename(`Grouping 1` = grouping1, `Grouping 2` = grouping2, `z Score` = score) %>%
  arrange(var1)
  

PCOS1_df <- subset(excel_df, var1 == "PCOS1") %>%
  select(-var1, -var2) %>%
  arrange(`Grouping 1`)

PCOS1c_df <- excel_df %>%
  filter(str_detect(var1, "PCOS1c")) %>%
  arrange(`Grouping 1`)

PCOS1d_df <- excel_df %>%
  filter(str_detect(var1, "PCOS1d")) %>%
  select(-var1, -var2) %>%
  arrange(`Grouping 1`)

PCOS2a_df <- excel_df %>%
  filter(str_detect(var1, "PCOS2a")) %>%
  select(-var1, -var2) %>%
  arrange(`Grouping 1`)

PCOS2b_df <- excel_df %>%
  filter(str_detect(var1, "PCOS2b")) %>%
  select(-var1, -var2) %>%
  arrange(`Grouping 1`)

PCOS2c_df <- excel_df %>%
  filter(str_detect(var1, "PCOS2c")) %>%
  select(-var1, -var2) %>%
  arrange(`Grouping 1`)

PCOS2d_df <- excel_df %>%
  filter(str_detect(var1, "PCOS2d")) %>%
  select(-var1, -var2) %>%
  arrange(`Grouping 1`)

PCOS3_df <- excel_df %>%
  filter(str_detect(var1, "PCOS3")) %>%
  select(-var1, -var2) %>%
  arrange(`Grouping 1`)
PCOS4_df <- excel_df %>%
  filter(str_detect(var1, "PCOS4")) %>%
  select(-var1, -var2) %>%
  arrange(`Grouping 1`)
PCOS5_df <- excel_df %>%
  filter(str_detect(var1, "PCOS5")) %>%
  select(-var1, -var2) %>%
  arrange(`Grouping 1`)
PCOS6_df <- excel_df %>%
  filter(str_detect(var1, "PCOS6")) %>%
  select(-var1, -var2) %>%
  arrange(`Grouping 1`)

sig_df <- data.frame(Table_Name = c("PCOS1_df", "PCOS1c_df",  "PCOS1d_df", "PCOS2a_df", "PCOS2b_df", 
                                    "PCOS2c_df", "PCOS2d_df", "PCOS3_df", "PCOS4_df", "PCOS5_df", "PCOS6_df"), 
                     Sheet = c("PCOS1", "PCOS1c", "PCOS1d", "PCOS2a", "PCOS2b", "PCOS2c", "PCOS2d", "PCOS3",
                               "PCOS4", "PCOS5", "PCOS6"))
r <- 1
r <- r + 1

for (i in 1:nrow(sig_df)) {
  
  df <- sig_df[i,]
  
  writeDataTable(wb2, 
                 sheet = paste0(df$Sheet),
                 x = get(df$Table_Name),
                 startRow = r,
                 startCol = 1,
                 colNames = TRUE,
                 tableStyle = "none",
                 tableName = df$Table_Name,
                 withFilter = FALSE,
                 bandedRows = FALSE)
  
}

for (i in 1:nrow(PCOS1_df)) {
  if (!is.na(PCOS1_df[i, 5])) {
    if (abs(PCOS1_df[i, 5]) > 1.96) {
      addStyle(wb2, "PCOS1",
               style = sig,
               rows = r + i,
               cols = 5)
    } else {
      addStyle(wb2, "PCOS1",
               style = not_sig,
               rows = r + i,
               cols = 5)
    }
  }
}

for (i in 1:nrow(PCOS1c_df)) {
  if (!is.na(PCOS1c_df[i, 7])) {
    if (abs(PCOS1c_df[i, 7]) > 1.96) {
      addStyle(wb2, "PCOS1c",
               style = sig,
               rows = r + i,
               cols = 7)
    } else {
      addStyle(wb2, "PCOS1c",
               style = not_sig,
               rows = r + i,
               cols = 7)
    }
  }
}

for (i in 1:nrow(PCOS1d_df)) {
  if (!is.na(PCOS1d_df[i, 5])) {
    if (abs(PCOS1d_df[i, 5]) > 1.96) {
      addStyle(wb2, "PCOS1d",
               style = sig,
               rows = r + i,
               cols = 5)
    } else {
      addStyle(wb2, "PCOS1d",
               style = not_sig,
               rows = r + i,
               cols = 5)
    }
  }
}

for (i in 1:nrow(PCOS2a_df)) {
  if (!is.na(PCOS2a_df[i, 5])) {
    if (abs(PCOS2a_df[i, 5]) > 1.96) {
      addStyle(wb2, "PCOS2a",
               style = sig,
               rows = r + i,
               cols = 5)
    } else {
      addStyle(wb2, "PCOS2a",
               style = not_sig,
               rows = r + i,
               cols = 5)
    }
  }
}

for (i in 1:nrow(PCOS2b_df)) {
  if (!is.na(PCOS2b_df[i, 5])) {
    if (abs(PCOS2b_df[i, 5]) > 1.96) {
      addStyle(wb2, "PCOS2b",
               style = sig,
               rows = r + i,
               cols = 5)
    } else {
      addStyle(wb2, "PCOS2b",
               style = not_sig,
               rows = r + i,
               cols = 5)
    }
  }
}

for (i in 1:nrow(PCOS2c_df)) {
  if (!is.na(PCOS2c_df[i, 5])) {
    if (abs(PCOS2c_df[i, 5]) > 1.96) {
      addStyle(wb2, "PCOS2c",
               style = sig,
               rows = r + i,
               cols = 5)
    } else {
      addStyle(wb2, "PCOS2c",
               style = not_sig,
               rows = r + i,
               cols = 5)
    }
  }
}

for (i in 1:nrow(PCOS2d_df)) {
  if (!is.na(PCOS2d_df[i, 5])) {
    if (abs(PCOS2d_df[i, 5]) > 1.96) {
      addStyle(wb2, "PCOS2d",
               style = sig,
               rows = r + i,
               cols = 5)
    } else {
      addStyle(wb2, "PCOS2d",
               style = not_sig,
               rows = r + i,
               cols = 5)
    }
  }
}

for (i in 1:nrow(PCOS3_df)) {
  if (!is.na(PCOS3_df[i, 5])) {
    if (abs(PCOS3_df[i, 5]) > 1.96) {
      addStyle(wb2, "PCOS3",
               style = sig,
               rows = r + i,
               cols = 5)
    } else {
      addStyle(wb2, "PCOS3",
               style = not_sig,
               rows = r + i,
               cols = 5)
    }
  }
}

for (i in 1:nrow(PCOS4_df)) {
  if (!is.na(PCOS4_df[i, 5])) {
    if (abs(PCOS4_df[i, 5]) > 1.96) {
      addStyle(wb2, "PCOS4",
               style = sig,
               rows = r + i,
               cols = 5)
    } else {
      addStyle(wb2, "PCOS4",
               style = not_sig,
               rows = r + i,
               cols = 5)
    }
  }
}

for (i in 1:nrow(PCOS5_df)) {
  if (!is.na(PCOS5_df[i, 5])) {
    if (abs(PCOS5_df[i, 5]) > 1.96) {
      addStyle(wb2, "PCOS5",
               style = sig,
               rows = r + i,
               cols = 5)
    } else {
      addStyle(wb2, "PCOS5",
               style = not_sig,
               rows = r + i,
               cols = 5)
    }
  }
}

for (i in 1:nrow(PCOS6_df)) {
  if (!is.na(PCOS6_df[i, 5])) {
    if (abs(PCOS6_df[i, 5]) > 1.96) {
      addStyle(wb2, "PCOS6",
               style = sig,
               rows = r + i,
               cols = 5)
    } else {
      addStyle(wb2, "PCOS6",
               style = not_sig,
               rows = r + i,
               cols = 5)
    }
  }
}

saveWorkbook(wb2,
             paste0(here(), "/outputs/significance outputs/exploratory significance output ", current_year, ".xlsx"),
             overwrite = TRUE)

