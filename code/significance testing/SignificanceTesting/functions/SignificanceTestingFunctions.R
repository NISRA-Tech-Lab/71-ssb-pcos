significanceTest <- function(p1,n1,p2,n2) {
  # This function follows the method used in the spreadsheet exactly. It outputs 
  # whether the difference is significant or not and where significant, it indicates 
  # whether p1 or p2 is larger.
  s1 <- (1/n1)+(1/n2)
  s2 <- p1*n1
  s3 <- p2*n2
  s4 <- s2/n1
  s5 <- s3/n2
  s6 <- s4-s5
  s7 <- (s2+s3)/(n1+n2)
  s8 <- 1-s7
  s9 <- sqrt(s7*s8*s1)
  z <- s6/s9
  
  if (abs(z)>1.96){ 
    significance <- "Significant" 
    if (z<0){
      direction = "p1 < p2"
    }
    else {direction = "p1 > p2"}
  } else {significance <- "Not Significant"
    direction = NA
  }
  return(c(significance = significance, direction = direction, score = z))
}

confidenceInterval <- function(p,n){
  # This fucntion returns the upper and lower confidence limits 
  # **not used in this code currently**
  ciCalc <- 1.96 * sqrt((p*(1-p))/n)
  return(c(confIn = ciCalc, lowerCL = p-ciCalc, upperCL = p+ciCalc))
}

varAnalysis <- function(df1, group1, grouping1, var1){
  # This function establishes the p and n values for a given variable and grouping.
  df <- get(df1)
  #  group1 <- get(group1, df)
  #  grouping1 <- grouping1
  var <- as.name(var1)
  if (group1 == "All"){
    df <- df %>% select(var)
  }
  else {
    group1 <- get(group1, df)
    df <- df %>% filter(group1 == grouping1) %>% select(var)
    #print(group1)
  }
  
  colnames(df) <- "var1"
  df<- df %>% filter(!is.na(var1))%>% 
    mutate(var1 = case_when(var1 == "Yes" ~ "yes",
                            var1 == "No" ~ "no",
                            var1 == "yes" ~ "yes",
                            var1 == "no" ~ "no",
                            var1 == "Never" ~ "no",
                            TRUE ~ "yes")) 
    
    n <- nrow(df)
    p <- prop.table(table(df$var1))["yes"]
    return(c(n = n, p = p))
}

extractQuestionText <- function(df, var){
  # This function extracts the question text as stored in the SPSS file for a specific variable
  df <- get(df)
  #print(var)
  qText <- attributes(df)$variable.labels[var]
  
  
  return (q = qText)
  
  
}

extractVar2 <- function(vars, y1, v1, y2){
  # This function extracts the corresponding variable for a specified year 
  # given the year and variable to compare to
  year1<- get(y1, vars)
  year2 <- y2 
  v2 <- vars %>% filter(year1 == v1) 
  v2 <- v2 %>% select(year2)
  return (v2$year2)
  
}


stCombinations <- function(vars, groupings, currentYear){
  # Build dataframe for testing from vars and groupings inputs 
  v2<-vars %>% pivot_longer(cols = colnames(vars), names_to = "year", values_to = "var") %>% drop_na()  # extract all the different years of data
  yearlyCombinations <- crossing(year1=colnames(vars), year2=colnames(vars)) # work out all the possible combinations of years for comparison
  yearlyCombinations <- yearlyCombinations[ !duplicated(apply(yearlyCombinations, 1, sort), MARGIN = 2), ] #remove duplicated combinations
  yearlyCombinations <- yearlyCombinations %>% filter (year1 == currentYear | year2==currentYear) # only retain years that include the current year
  
  x<- yearlyCombinations %>% left_join(v2, by = c("year1"= "year")) %>% mutate(var1 = var) %>% select(-var) # Add in the variables for all year1 cases
  var2<- apply(x, 1, function(y) extractVar2(vars,y['year1'],y['var1'],y['year2'])) # identify the corresponding year2 variables
  varYears<- cbind(x, var2) #add in corresponding year2 variables
  
  vardf <- data.frame() # create emply dataframe
  z=1 # initialise z
  
  # loop around all variables and groupings adding in to the dataframe
  for(i in 1:nrow(varYears)) {
    if (varYears[i,'year1']== currentYear | varYears[i,'year2'] == currentYear){
      if (varYears[i,'year1']== varYears[i,'year2']){
        for(j in 1:nrow(groupings)) {
          vardf[z,'year1'] = varYears[i,'year1']
          vardf[z,'var1'] = varYears[i,'var1']
          vardf[z,'year2'] = varYears[i,'year2']
          vardf[z,'var2'] = varYears[i,'var2']
          vardf[z,'group1'] = groupings[j,'group1']
          vardf[z,'grouping1'] = groupings[j,'grouping1']
          vardf[z,'group2'] = groupings[j,'group2']
          vardf[z,'grouping2'] = groupings[j,'grouping2']
          z=z+1
        }
      }
      
      vardf[z,'year1'] = varYears[i,'year1']
      vardf[z,'var1'] = varYears[i,'var1']
      vardf[z,'year2'] = varYears[i,'year2']
      vardf[z,'var2'] = varYears[i,'var2']
      vardf[z,'group1'] = "All"
      vardf[z,'grouping1'] = "All"
      vardf[z,'group2'] = "All"
      vardf[z,'grouping2'] = "All"
      z = z+1
    }
    
  }
  vardf <- vardf %>% select(year1,var1,group1,grouping1,year2,var2,group2,grouping2) # select only the variables required
  exdf <- vardf %>% filter(year1 == year2 & var1==var2 & grouping1== grouping2 & group1== group2)
  vardf <- vardf %>% anti_join(exdf)
  return (vardf )
  
}
