library(here)
source(paste0(here(), "/code/significance testing/final_output/significance_testing.R"))

if (!exists (paste0(here(), "/outputs/significance outputs"))) {
  dir.create(paste0(here(), "/outputs/significance outputs"))
}

# Create Workbook ####

wb <- createWorkbook()
modifyBaseFont(wb, fontSize = 12, fontName = "Arial")

# Awareness of NISRA ####
 
addWorksheet(wb, "Awareness")

r <- 1

writeData(wb, "Awareness",
          x = "Awareness of NISRA",
          startRow = r)

addStyle(wb, "Awareness",
         style = pt,
         rows = r,
         cols = 1)

r <- r + 2

## Current Year vs Previous Year ####
 
f_insert_sig_table(df = awareness_year,
                   sheet = "Awareness",
                   title = paste0("Awareness of NISRA - ", current_year - 1, " vs ", current_year))

## In Work vs Not in work ####

f_insert_sig_table(df = work_status,
                   sheet = "Awareness",
                   title = paste0("Awareness of NISRA - In Work vs Not in Work - ", current_year))

## Age groups ####
 
f_insert_z_table(df = age_z_scores,
                 sheet = "Awareness",
                 title = paste0("Awareness of NISRA by Age Group - ", current_year))

## Qualifications ####

f_insert_z_table(df = qual_z_scores,
                 sheet = "Awareness",
                 title = paste0("Awareness of NISRA by Highest Qualification Achieved - ", current_year))

setColWidths(wb, "Awareness",
             cols = 1:ncol(age_z_scores),
             widths = c(47, rep(12, ncol(age_z_scores) - 1)))

# Products ####
 
addWorksheet(wb, "Products")

r <- 1

writeData(wb, "Products",
          x = "Products",
          startRow = r)

addStyle(wb, "Products",
         style = pt,
         rows = r,
         cols = 1)

r <- r + 2

## Heard of NISRA vs Had not heard of NISRA ####
 
writeData(wb, "Products",
          x = paste0("Heard of NISRA vs Not heard of NISRA - ", current_year),
          startRow = r)

addStyle(wb, "Products",
         style = pt2,
         rows = r,
         cols = 1)

r <- r + 1

writeDataTable(wb, "Products",
               x = products_stats,
               startRow = r,
               tableStyle = "none",
               headerStyle = ch,
               withFilter = FALSE)

addStyle(wb, "Products",
         style = ns3d,
         rows = (r + 1):(r + nrow(products_stats) - 1),
         cols = 2:ncol(products_stats),
         gridExpand = TRUE)

for (i in 1:nrow(products_stats)) {
  
  if (!is.na(products_stats[i, "Z"])) {
    if (abs(products_stats[i, "Z"]) > 1.96) {
      addStyle(wb, "Products",
               style = sig,
               rows = r + i,
               cols = which(names(products_stats) == "Z"))
    } else {
      addStyle(wb, "Products",
               style = not_sig,
               rows = r + i,
               cols = which(names(products_stats) == "Z"))
    }
  }
  
}

r <- r + nrow(products_stats) + 2

## Had heard of NISRA: This year vs previous year ####
 
writeData(wb, "Products",
          x = paste0("Heard of NISRA: ", current_year - 1, " vs ", current_year),
          startRow = r)

addStyle(wb, "Products",
         style = pt2,
         rows = r,
         cols = 1)

r <- r + 1

writeDataTable(wb, "Products",
               x = heard_stats,
               startRow = r,
               tableStyle = "none",
               headerStyle = ch,
               withFilter = FALSE)

addStyle(wb, "Products",
         style = ns3d,
         rows = (r + 1):(r + nrow(heard_stats) - 1),
         cols = 2:ncol(heard_stats),
         gridExpand = TRUE)

for (i in 1:nrow(heard_stats)) {
  
  if (!is.na(heard_stats[i, "Z"])) {
    if (abs(heard_stats[i, "Z"]) > 1.96) {
      addStyle(wb, "Products",
               style = sig,
               rows = r + i,
               cols = which(names(heard_stats) == "Z"))
    } else {
      addStyle(wb, "Products",
               style = not_sig,
               rows = r + i,
               cols = which(names(heard_stats) == "Z"))
    }
  }
  
}

r <- r + nrow(heard_stats) + 2

## Had not heard of NISRA: This year vs previous year ####
 
writeData(wb, "Products",
          x = paste0("Had not heard of NISRA: ", current_year - 1, " vs ", current_year),
          startRow = r)

addStyle(wb, "Products",
         style = pt2,
         rows = r,
         cols = 1)

r <- r + 1

writeDataTable(wb, "Products",
               x = not_heard_stats,
               startRow = r,
               tableStyle = "none",
               headerStyle = ch,
               withFilter = FALSE)

addStyle(wb, "Products",
         style = ns3d,
         rows = (r + 1):(r + nrow(not_heard_stats) - 1),
         cols = 2:ncol(not_heard_stats),
         gridExpand = TRUE)

for (i in 1:nrow(not_heard_stats)) {
  
  if (!is.na(not_heard_stats[i, "Z"])) {
    if (abs(not_heard_stats[i, "Z"]) > 1.96) {
      addStyle(wb, "Products",
               style = sig,
               rows = r + i,
               cols = which(names(not_heard_stats) == "Z"))
    } else {
      addStyle(wb, "Products",
               style = not_sig,
               rows = r + i,
               cols = which(names(not_heard_stats) == "Z"))
    }
  }
  
}

r <- r + nrow(not_heard_stats) + 2

setColWidths(wb, "Products",
             cols = 1:ncol(products_stats),
             widths = c(55, rep(12, ncol(products_stats) - 1)))

# Trust in NISRA ####
 
addWorksheet(wb, "Trust in NISRA")

r <- 1

writeData(wb, "Trust in NISRA",
          x = "Trust in NISRA",
          startRow = r)

addStyle(wb, "Trust in NISRA",
         style = pt,
         rows = r,
         cols = 1)

r <- r + 2

## Trust in NISRA: This year vs previous year ####

f_insert_sig_table(df = trust_year,
                   sheet = "Trust in NISRA",
                   title = paste0("Trust in NISRA - ", current_year, " vs ", current_year - 1))

## Distrust in NISRA: This year vs previous year ####

f_insert_sig_table(df = distrust_year,
                   sheet = "Trust in NISRA",
                   title = paste0("Distrust in NISRA - ", current_year, " vs ", current_year - 1))

## Don't Know trust in NISRA: This year vs previous year ####

f_insert_sig_table(df = dont_know_trust,
                   sheet = "Trust in NISRA",
                   title = paste0("Don't Know responses - ", current_year, " vs ", current_year - 1))

## Trust in NISRA by Age group compare ####

f_insert_z_table(df = trust_age_z_scores, 
                 sheet = "Trust in NISRA",
                 title = paste0("Trust in NISRA by Age Group - ", current_year))

## Distrust in NISRA by Age group compare ####
 
f_insert_z_table(df = distrust_age_z_scores, 
                 sheet = "Trust in NISRA",
                 title = paste0("Distrust in NISRA by Age Group - ", current_year))

## Don't know trust in NISRA by Age group compare ####
 
f_insert_z_table(df = dont_know_trust_age_z_scores, 
                 sheet = "Trust in NISRA",
                 title = paste0("Don't know trust in NISRA by Age Group - ", current_year))

# ValueIntConfYears ####
 
addWorksheet(wb, "ValueIntConfYears")

r <- 1

writeData(wb, "ValueIntConfYears",
          x = "Years",
          startRow = r)

addStyle(wb, "ValueIntConfYears",
         style = pt,
         rows = r,
         cols = 1)

r <- r + 2

f_insert_sig_table(df = value_year,
                   sheet = "ValueIntConfYears",
                   title = paste0("NISRA stats are important - ", current_year , " vs ", current_year - 1))

f_insert_sig_table(df = interference_year,
                   sheet = "ValueIntConfYears",
                   title = paste0("NISRA stats are free from interference - ", current_year , " vs ", current_year - 1))

f_insert_sig_table(df = confidential_year,
                   sheet = "ValueIntConfYears",
                   title = paste0("NISRA will keep my information confidential - ", current_year , " vs ", current_year - 1))

# Save Workbook ####

saveWorkbook(wb,
             paste0(here(), "/outputs/significance outputs/significance output ", current_year, ".xlsx"),
             overwrite = TRUE)
