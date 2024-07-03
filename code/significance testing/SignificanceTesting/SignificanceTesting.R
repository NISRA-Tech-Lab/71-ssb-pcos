library(dplyr)
library(data.table)
library(tidyr)
library(forcats)

# R Scripts containing bespoke functions for significance testing
source("code/significance testing/SignificanceTesting/functions/SignificanceTestingFunctions.R")

### **** DATA **** ####
data2021 <- readRDS("T:/Projects/71 - SSB PCOS/Data/Final/PCOS 2021 Final Dataset.RDS")
data2022 <- readRDS("T:/Projects/71 - SSB PCOS/Data/Final/PCOS 2022 Final Dataset.RDS")

data2021$PCOS2a <- fct_collapse(data2021$PCOS2a, 
                                "yes" = c("Trust a great deal","Tend to trust"), 
                                "no" = c("Tend to distrust", "Distrust greatly"))

data2021$PCOS2b <- fct_collapse(data2021$PCOS2b, 
                                "yes" = c("Trust a great deal","Tend to trust"), 
                                "no" = c("Tend to distrust", "Distrust greatly"))

data2021$PCOS2c <- fct_collapse(data2021$PCOS2c, 
                               "yes" = c("Trust a great deal","Tend to trust"), 
                               "no" = c("Tend to distrust", "Distrust greatly"))

data2021$PCOS2d <- fct_collapse(data2021$PCOS2d, 
                               "yes" = c("Trust a great deal","Tend to trust"), 
                               "no" = c("Tend to distrust", "Distrust greatly"))

data2021$PCOS3 <- fct_collapse(data2021$PCOS3, 
                               "yes" = c("Tend to trust them", "Trust them greatly"), 
                               "no" = c("Tend not to trust them", "Distrust them greatly"))

data2021$PCOS4 <- fct_collapse(data2021$PCOS4, 
                               "yes" = c("Strongly agree","Tend to agree"), 
                               "no" = c("Tend to disagree", "Strongly disagree"))

data2021$PCOS5 <- fct_collapse(data2021$PCOS5, 
                               "yes" = c("Strongly agree","Tend to agree"), 
                               "no" = c("Tend to disagree", "Strongly disagree"))

data2021$PCOS6 <- fct_collapse(data2021$PCOS6, 
                               "yes" = c("Strongly agree","Tend to agree"), 
                               "no" = c("Tend to disagree", "Strongly disagree"))

data2022$PCOS2a <- fct_collapse(data2022$PCOS2a, 
                                            "yes" = c("Trust a great deal","Tend to trust"), 
                                            "no" = c("Tend to distrust", "Distrust greatly"))

data2022$PCOS2b <- fct_collapse(data2022$PCOS2b, 
                                "yes" = c("Trust a great deal","Tend to trust"), 
                                "no" = c("Tend to distrust", "Distrust greatly"))

data2022$PCOS2c <- fct_collapse(data2022$PCOS2c, 
                                "yes" = c("Trust a great deal","Tend to trust"), 
                                "no" = c("Tend to distrust", "Distrust greatly"))

data2022$PCOS2d <- fct_collapse(data2022$PCOS2d, 
                                "yes" = c("Trust a great deal","Tend to trust"), 
                                "no" = c("Tend to distrust", "Distrust greatly"))

data2022$PCOS3 <- fct_collapse(data2022$PCOS3, 
                               "yes" = c("Tend to trust them", "Trust them greatly"), 
                               "no" = c("Tend not to trust them", "Distrust them greatly"))

data2022$PCOS4 <- fct_collapse(data2022$PCOS4, 
                               "yes" = c("Strongly agree","Tend to agree"), 
                               "no" = c("Tend to disagree", "Strongly disagree"))

data2022$PCOS5 <- fct_collapse(data2022$PCOS5, 
                               "yes" = c("Strongly agree","Tend to agree"), 
                               "no" = c("Tend to disagree", "Strongly disagree"))

data2022$PCOS6 <- fct_collapse(data2022$PCOS6, 
                               "yes" = c("Strongly agree","Tend to agree"), 
                               "no" = c("Tend to disagree", "Strongly disagree"))

### **** CURRENT YEAR **** ####
currentYear = "data2022"

# Re-labelling some fields
data2022$SEX <- recode(data2022$SEX, "M" = "Male", "F" = "Female")

### **** INPUTS **** ####
vars <- read.csv("code/significance testing/SignificanceTesting/inputs/var.csv") 
groupings <- read.csv("code/significance testing/SignificanceTesting/inputs/grouping.csv")

### **** CODE **** ####
# Build dataframe for testing from vars and groupings inputs 
vardf<- stCombinations(vars, groupings, currentYear)

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

outputfile <- paste0('outputs/significanceTestingAll1920-', Sys.Date(), '.csv') 

write.csv(vardf,outputfile) #a csv file with the results will be written to the outputs folder.

