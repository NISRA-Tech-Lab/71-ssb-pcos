

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

### **** CURRENT YEAR **** ####
currentYear = "data_current"

# Re-labelling some fields
data_current$SEX <- recode(data_current$SEX, "M" = "Male", "F" = "Female")

### **** INPUTS **** ####
vars <- read.csv(paste0(here(), "/code/significance testing/exploratory_analysis/inputs/var.csv"))
groupings <- read.csv(paste0(here(), "/code/significance testing/exploratory_analysis/inputs/grouping.csv"))

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

outputfile <- paste0('outputs/significanceTestingAll2022-', Sys.Date(), '.csv') 

write.csv(vardf,outputfile) #a csv file with the results will be written to the outputs folder.

