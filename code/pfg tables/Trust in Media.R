library(here)
source(paste0(here(), "/code/config.R"))

# Define all available years based on current_year value from config #### 

data_years <- c(seq(2012, 2018, 2), 2019:current_year)

# Lookup list for EQUALGROUPS labels (taken from PfG documentation) ####
eq_labels <- list("Sex - Male" = "1",
                  "Sex - Female" = "2",
                  "16-24" = "3",
                  "25-34" = "4", 
                  "35-44" = "69",
                  "45-54" = "70",
                  "55-64" = "71",
                  "65-74" = "72",
                  "75+" = "8")

# Create template of data frame ####

trust_media <- data.frame(STATISTIC = character()) %>%
  mutate(`TLIST(A1)` = numeric(),
         EQUALGROUPS = character(),
         `Variable name` = character(),
         `Lower limit` = numeric(),
         VALUE = numeric(),
         `Upper limit` = numeric())

# Loop through all available years ####

for (year in data_years) {

  ## Read data from Remote location ####
  data_year <- readRDS(paste0(data_folder, "Final/PCOS ", year, " Final Dataset.RDS"))
  
  ### assign data frame to global environemnt ####  
  assign(paste0("data_", year),
         data_year)
  
  ## Run Analysis only if TrustMedia2 is present in data ####
  
  if ("TrustMedia2" %in% names(data_year)) {
   
    ### Which weight variable to use depending on year ####
    
    weight <- if (year == 2020) {
      "W4"
    } else if (year %in% 2012:2016) {
      "weight"
    } else {
      "W3"
    }
    
    ### Calculated weighted value for NI ####
    
    ni_value <- data_year %>%
      filter(!is.na(TrustMedia2) & TrustMedia2 %in% c("Trust a great deal/Tend to trust", "Tend to trust/trust a great deal")) %>%
      pull(weight) %>%
      sum() / data_year %>%
        filter(!is.na(TrustMedia2)) %>%
        pull(weight) %>%
        sum() * 100 
    
    ### Unweighted n for NI for year ####
    
    ni_n <- data_year %>%
      filter(!is.na(TrustMedia2)) %>%
      nrow()
    
    ### Unweighted p for NI for year ####
    
    ni_p <- data_year %>%
      filter(!is.na(TrustMedia2) & TrustMedia2 %in% c("Trust a great deal/Tend to trust", "Tend to trust/trust a great deal")) %>%
      nrow() / ni_n
    
    ### Confidence intervals for NI for year ####
    
    ni_ci <- f_confidence_interval(p = ni_p,
                                   n = ni_n)
    
    ### Write NI row for year as data frame ####
    
    ni_data <- data.frame(STATISTIC = character(1)) %>%
      mutate(`TLIST(A1)` = year,
      EQUALGROUPS = "N92000002",
      `Variable name` = "Northern Ireland",
      `Lower limit` = ni_ci[["lower_cl"]] * 100,
      VALUE = ni_value,
      `Upper limit` = ni_ci[["upper_cl"]] * 100)
    
    ### Append this row to trust_media data frame ####
    
    trust_media <- trust_media %>%
      bind_rows(ni_data)
    
    ## By Age Group ####
    
    if ("AGE2" %in% names(data_year)) {
      
      ### Reword value labels for age groups ####
      levels(data_year$AGE2) <- gsub(" ", "", levels(data_year$AGE2), fixed = TRUE) %>%
        gsub("andover", "+", .)
      
      ### Which weighting variable to use for age analysis based on year ####
      age_weight <- if (year %in% 2012:2016) {
        "weight"
      } else {
        "W2"
      }
      
      ### Age groups to loop through ####
      ages <- levels(data_year$AGE2)
      
      for (age in ages) {
        
        ### Calculate weighted age value for each age in each year ####
        age_value <- data_year %>%
          filter(!is.na(TrustMedia2) & AGE2 == age & TrustMedia2 %in% c("Trust a great deal/Tend to trust", "Tend to trust/trust a great deal")) %>%
          pull(age_weight) %>%
          sum() / data_year %>%
          filter(!is.na(TrustMedia2) & AGE2 == age) %>%
          pull(age_weight) %>%
          sum() * 100
        
        ### Unweighted age n for each year ####
        age_n <- data_year %>%
          filter(!is.na(TrustMedia2) & AGE2 == age) %>%
          nrow()
        
        ### Unweighted age p value for each year ####
        age_p <- data_year %>%
          filter(!is.na(TrustMedia2) & AGE2 == age & TrustMedia2 %in% c("Trust a great deal/Tend to trust", "Tend to trust/trust a great deal")) %>%
          nrow() / age_n
        
        ### Confidence intervals for age group ####
        age_ci <- f_confidence_interval(p = age_p,
                                        n = age_n)
        
        ### Write new row for each age group for each year as data frame ####
        
        age_data <- data.frame(STATISTIC = character(1)) %>%
          mutate(`TLIST(A1)` = year,
                 EQUALGROUPS = eq_labels[[age]],
                 `Variable name` = paste("Age", age),
                 `Lower limit` = age_ci[["lower_cl"]] * 100,
                 VALUE = age_value,
                 `Upper limit` = age_ci[["upper_cl"]] * 100)
        
        ### Append to trust_media data frame ####
        
        trust_media <- trust_media %>%
          bind_rows(age_data)
        
      }
    
    }
    
    ## Sex ####
    
    if ("SEX" %in% names(data_year)) {
      
      data_year <- data_year %>%
        mutate(SEX = factor(SEX,
                            levels = levels(SEX),
                            labels = c("Sex - Male", "Sex - Female", "Refusal", "Don't Know")))
      
      sexes <- c("Sex - Male", "Sex - Female")
      
      sex_weight <- if (year %in% 2012:2016) {
        "weight"
      } else if (year == 2020) {
        "W1a"
      } else {
        "W1"
      }
      
      for (sex in sexes) {
        
        ### Calculate weighted sex value for each sex in each year ####
        sex_value <- data_year %>%
          filter(!is.na(TrustMedia2) & SEX == sex & TrustMedia2 %in% c("Trust a great deal/Tend to trust", "Tend to trust/trust a great deal")) %>%
          pull(sex_weight) %>%
          sum() / data_year %>%
          filter(!is.na(TrustMedia2) & SEX == sex) %>%
          pull(sex_weight) %>%
          sum() * 100
        
        ### Unweighted sex n for each year ####
        sex_n <- data_year %>%
          filter(!is.na(TrustMedia2) & SEX == sex) %>%
          nrow()
        
        ### Unweighted sex p value for each year ####
        sex_p <- data_year %>%
          filter(!is.na(TrustMedia2) & SEX == sex & TrustMedia2 %in% c("Trust a great deal/Tend to trust", "Tend to trust/trust a great deal")) %>%
          nrow() / sex_n
        
        ### Confidence intervals for sex group ####
        sex_ci <- f_confidence_interval(p = sex_p,
                                        n = sex_n)
        
        ### Write sex row for each sex group for each year as data frame ####
        
        sex_data <- data.frame(STATISTIC = character(1)) %>%
          mutate(`TLIST(A1)` = year,
                 EQUALGROUPS = eq_labels[[sex]],
                 `Variable name` = sex,
                 `Lower limit` = sex_ci[["lower_cl"]] * 100,
                 VALUE = sex_value,
                 `Upper limit` = sex_ci[["upper_cl"]] * 100)
        
        ### Append to trust_media data frame ####
        
        trust_media <- trust_media %>%
          bind_rows(sex_data)
        
      }
      
    }
    
    ## Employment Status ####
    
    if ("EMPST2" %in% names(data_year)) {
      
      emps <- c("In paid employment", "Not in paid employment")
      
      for (emp in emps) {
        
        ### Calculate weighted Employment Status value for each sex in each year ####
        emp_value <- data_year %>%
          filter(!is.na(TrustMedia2) & EMPST2 == emp & TrustMedia2 %in% c("Trust a great deal/Tend to trust", "Tend to trust/trust a great deal")) %>%
          pull(weight) %>%
          sum() / data_year %>%
          filter(!is.na(TrustMedia2) & EMPST2 == emp) %>%
          pull(weight) %>%
          sum() * 100
        
        ### Unweighted Emplpoyment Status n for each year ####
        emp_n <- data_year %>%
          filter(!is.na(TrustMedia2) & EMPST2 == emp) %>%
          nrow()
        
        ### Unweighted Employment Status p value for each year ####
        emp_p <- data_year %>%
          filter(!is.na(TrustMedia2) & EMPST2 == emp & TrustMedia2 %in% c("Trust a great deal/Tend to trust", "Tend to trust/trust a great deal")) %>%
          nrow() / emp_n
        
        ### Confidence intervals for sex group ####
        emp_ci <- f_confidence_interval(p = emp_p,
                                        n = emp_n)
        
        ### Write new row for each employment status for each year as data frame ####
        
        emp_data <- data.frame(STATISTIC = character(1)) %>%
          mutate(`TLIST(A1)` = year,
                 EQUALGROUPS = "",
                 `Variable name` = paste0("Employment Status - ", emp),
                 `Lower limit` = emp_ci[["lower_cl"]] * 100,
                 VALUE = emp_value,
                 `Upper limit` = emp_ci[["upper_cl"]] * 100)
        
        ### Append to trust_media data frame ####
        
        trust_media <- trust_media %>%
          bind_rows(emp_data)
        
      }
      
    }
    
    ## Highest Qualification achieved #### 
    
    if ("DERHIanalysis" %in% names(data_year)) {
      
      quals <- c("Degree, or Degree equivalent and above",
                 "Other higher education below degree level",
                 "A levels, vocational level 3 and equivalents",             
                 "GCSE/O level grade A*-C. vocational level 2 and equivalents",
                 "Qualifications at level 1 and below",
                 "No qualification")
      
      for (qual in quals) {
        
        ### Calculate weighted qualification value for each qualification group in each year ####
        qual_value <- data_year %>%
          filter(!is.na(TrustMedia2) & DERHIanalysis == qual & TrustMedia2 %in% c("Trust a great deal/Tend to trust", "Tend to trust/trust a great deal")) %>%
          pull(weight) %>%
          sum() / data_year %>%
          filter(!is.na(TrustMedia2) & DERHIanalysis == qual) %>%
          pull(weight) %>%
          sum() * 100
        
        ### Unweighted n for each qualification group for each year ####
        qual_n <- data_year %>%
          filter(!is.na(TrustMedia2) & DERHIanalysis == qual) %>%
          nrow()
        
        ### Unweighted Highest Qualification p value for each year ####
        qual_p <- data_year %>%
          filter(!is.na(TrustMedia2) & DERHIanalysis == qual & TrustMedia2 %in% c("Trust a great deal/Tend to trust", "Tend to trust/trust a great deal")) %>%
          nrow() / qual_n
        
        ### Confidence intervals for Highest Qualification ####
        qual_ci <- f_confidence_interval(p = qual_p,
                                         n = qual_n)
        
        ### Write new row for each Highest Qualification for each year as data frame ####
        
        qual_data <- data.frame(STATISTIC = character(1)) %>%
          mutate(`TLIST(A1)` = year,
                 EQUALGROUPS = "",
                 `Variable name` = paste0("Highest Qualification - ", qual),
                 `Lower limit` = qual_ci[["lower_cl"]] * 100,
                 VALUE = qual_value,
                 `Upper limit` = qual_ci[["upper_cl"]] * 100)
        
        ### Append to trust_media data frame ####
        
        trust_media <- trust_media %>%
          bind_rows(qual_data)
        
      }
      
    }
    
  }
  
}

# Sort final data frame ####

sort_order <- c("Northern Ireland", sort(unique(trust_media$`Variable name`[trust_media$`Variable name` != "Northern Ireland"])))

trust_media <- trust_media %>%
  mutate(`Variable name` = factor(`Variable name`,
                              levels = sort_order)) %>%
  arrange(`Variable name`, `TLIST(A1)`)

wb <- createWorkbook()

addWorksheet(wb, "TrustMedia")

writeDataTable(wb, "TrustMedia",
               x = trust_media,
               tableStyle = "none",
               withFilter = FALSE)

addStyle(wb, "TrustMedia",
         style = ns_pfg,
         rows = 2:(nrow(trust_media) + 1),
         cols = 5:7,
         gridExpand = TRUE)

setColWidths(wb, "TrustMedia",
             cols = 1:7,
             widths = c(22.86, 14.14, 12.86, 52.43, 10.14, 10.14, 10.71))

xl_filename <- paste0(here(), "/outputs/PfG - Equality Groups Data ", current_year, ".xlsx")

saveWorkbook(wb, xl_filename, overwrite = TRUE)

openXL(xl_filename)