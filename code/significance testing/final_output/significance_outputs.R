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

writeData(wb, "Awareness",
          x = paste0("Awareness of NISRA - ", current_year - 1, " vs ", current_year),
          startRow = r)

addStyle(wb, "Awareness",
         style = pt2,
         rows = r,
         cols = 1)

r <- r + 1

writeDataTable(wb, "Awareness",
               x = awareness_year,
               startRow = r,
               tableStyle = "none",
               headerStyle = ch,
               withFilter = FALSE)

addStyle(wb, "Awareness",
         style = ns3d,
         rows = r + 1,
         cols = 2:ncol(awareness_year),
         gridExpand = TRUE)

addStyle(wb, "Awareness",
         style = ns_comma,
         rows = r + 2,
         cols = 2:ncol(awareness_year),
         gridExpand = TRUE)

if (abs(awareness_year$`Z Score`[1]) > 1.96) {
  addStyle(wb, "Awareness",
           style = sig,
           rows = r + 1,
           cols = ncol(awareness_year))
} else {
  addStyle(wb, "Awareness",
           style = not_sig,
           rows = r + 1,
           cols = ncol(awareness_year))
}

r <- r + nrow(awareness_year) + 2

## In Work vs Not in work ####

writeData(wb, "Awareness",
x = paste0("Awareness of NISRA - In Work vs Not in Work - ", current_year),
startRow = r)

addStyle(wb, "Awareness",
         style = pt2,
         rows = r,
         cols = 1)

r <- r + 1

writeDataTable(wb, "Awareness",
               x = work_status,
               startRow = r,
               tableStyle = "none",
               headerStyle = ch,
               withFilter = FALSE)

addStyle(wb, "Awareness",
         style = ns3d,
         rows = r + 1,
         cols = 2:ncol(work_status),
         gridExpand = TRUE)

addStyle(wb, "Awareness",
         style = ns_comma,
         rows = r + 2,
         cols = 2:ncol(work_status),
         gridExpand = TRUE)

if (abs(work_status$`Z Score`[1]) > 1.96) {
  addStyle(wb, "Awareness",
           style = sig,
           rows = r + 1,
           cols = ncol(work_status))
} else {
  addStyle(wb, "Awareness",
           style = not_sig,
           rows = r + 1,
           cols = ncol(work_status))
}

r <- r + nrow(work_status) + 2


## Age groups ####

writeData(wb, "Awareness",
          x = paste0("Awareness of NISRA by Age Group - ", current_year),
          startRow = r)

addStyle(wb, "Awareness",
         style = pt2,
         rows = r,
         cols = 1)

r <- r + 1

writeDataTable(wb, "Awareness",
               x = age_z_scores,
               startRow = r,
               tableStyle = "none",
               headerStyle = ch,
               withFilter = FALSE)

addStyle(wb, "Awareness",
         style = ns3d,
         rows = (r + 1):(r + nrow(age_z_scores)),
         cols = 2:ncol(age_z_scores),
         gridExpand = TRUE)

for (i in 1:nrow(age_z_scores)) {
  for (j in 2:ncol(age_z_scores)) {
    if (!is.na(age_z_scores[i, j])) {
      if (abs(age_z_scores[i, j]) > 1.96) {
        addStyle(wb, "Awareness",
                 style = sig,
                 rows = r + i,
                 cols = j)
      } else {
        addStyle(wb, "Awareness",
                 style = not_sig,
                 rows = r + i,
                 cols = j)
      }
    }
  }
}

for (i in 1:nrow(age_z_scores)) {
  addStyle(wb, "Awareness",
           style = grey,
           rows = r + i,
           cols = 1 + i)
}

r <- r + nrow(age_z_scores) + 2

## Qualifications ####
 
writeData(wb, "Awareness",
          x = paste0("Awareness of NISRA by Highest Qualification Achieved - ", current_year),
          startRow = r)

addStyle(wb, "Awareness",
         style = pt2,
         rows = r,
         cols = 1)

r <- r + 1

writeDataTable(wb, "Awareness",
               x = qual_z_scores,
               startRow = r,
               tableStyle = "none",
               headerStyle = ch,
               withFilter = FALSE)

addStyle(wb, "Awareness",
         style = ns3d,
         rows = (r + 1):(r + nrow(qual_z_scores)),
         cols = 2:ncol(qual_z_scores),
         gridExpand = TRUE)

for (i in 1:nrow(qual_z_scores)) {
  for (j in 2:ncol(qual_z_scores)) {
    if (!is.na(qual_z_scores[i, j])) {
      if (abs(qual_z_scores[i, j]) > 1.96) {
        addStyle(wb, "Awareness",
                 style = sig,
                 rows = r + i,
                 cols = j)
      } else {
        addStyle(wb, "Awareness",
                 style = not_sig,
                 rows = r + i,
                 cols = j)
      }
    }
  }
}

for (i in 1:nrow(qual_z_scores)) {
  addStyle(wb, "Awareness",
           style = grey,
           rows = r + i,
           cols = 1 + i)
}

r <- r + nrow(qual_z_scores) + 2

setColWidths(wb, "Awareness",
             cols = 1:ncol(age_z_scores),
             widths = c(47, rep(12, ncol(qual_z_scores) - 1)))

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
 
writeData(wb, "Trust in NISRA",
          x = paste0("Trust in NISRA - ", current_year, " vs ", current_year - 1),
          startRow = r)

addStyle(wb, "Trust in NISRA",
         style = pt2,
         rows = r,
         cols = 1)

r <- r + 1

writeDataTable(wb, "Trust in NISRA",
               x = trust_year,
               startRow = r,
               tableStyle = "none",
               headerStyle = ch,
               withFilter = FALSE)

addStyle(wb, "Trust in NISRA",
         style = ns3d,
         rows = r + 1,
         cols = 2:ncol(trust_year),
         gridExpand = TRUE)

addStyle(wb, "Trust in NISRA",
         style = ns_comma,
         rows = r + 2,
         cols = 2:ncol(trust_year),
         gridExpand = TRUE)

if (abs(trust_year$`Z Score`[1]) > 1.96) {
  addStyle(wb, "Trust in NISRA",
           style = sig,
           rows = r + 1,
           cols = ncol(trust_year))
} else {
  addStyle(wb, "Trust in NISRA",
           style = not_sig,
           rows = r + 1,
           cols = ncol(trust_year))
}

r <- r + nrow(trust_year) + 2

## Distrust in NISRA: This year vs previous year ####

writeData(wb, "Trust in NISRA",
          x = paste0("Disrust in NISRA - ", current_year, " vs ", current_year - 1),
          startRow = r)

addStyle(wb, "Trust in NISRA",
         style = pt2,
         rows = r,
         cols = 1)

r <- r + 1

writeDataTable(wb, "Trust in NISRA",
               x = distrust_year,
               startRow = r,
               tableStyle = "none",
               headerStyle = ch,
               withFilter = FALSE)

addStyle(wb, "Trust in NISRA",
         style = ns3d,
         rows = r + 1,
         cols = 2:ncol(distrust_year),
         gridExpand = TRUE)

addStyle(wb, "Trust in NISRA",
         style = ns_comma,
         rows = r + 2,
         cols = 2:ncol(distrust_year),
         gridExpand = TRUE)

if (abs(trust_year$`Z Score`[1]) > 1.96) {
  addStyle(wb, "Trust in NISRA",
           style = sig,
           rows = r + 1,
           cols = ncol(distrust_year))
} else {
  addStyle(wb, "Trust in NISRA",
           style = not_sig,
           rows = r + 1,
           cols = ncol(distrust_year))
}

r <- r + nrow(distrust_year) + 2

## Don't Know trust in NISRA: This year vs previous year ####

writeData(wb, "Trust in NISRA",
          x = paste0("Don't Know responses - ", current_year, " vs ", current_year - 1),
          startRow = r)

addStyle(wb, "Trust in NISRA",
         style = pt2,
         rows = r,
         cols = 1)

r <- r + 1

writeDataTable(wb, "Trust in NISRA",
               x = dont_know_trust,
               startRow = r,
               tableStyle = "none",
               headerStyle = ch,
               withFilter = FALSE)

addStyle(wb, "Trust in NISRA",
         style = ns3d,
         rows = r + 1,
         cols = 2:ncol(dont_know_trust),
         gridExpand = TRUE)

addStyle(wb, "Trust in NISRA",
         style = ns_comma,
         rows = r + 2,
         cols = 2:ncol(dont_know_trust),
         gridExpand = TRUE)

if (abs(trust_year$`Z Score`[1]) > 1.96) {
  addStyle(wb, "Trust in NISRA",
           style = sig,
           rows = r + 1,
           cols = ncol(dont_know_trust))
} else {
  addStyle(wb, "Trust in NISRA",
           style = not_sig,
           rows = r + 1,
           cols = ncol(dont_know_trust))
}

r <- r + nrow(dont_know_trust) + 2

## Trust in NISRA by Age group compare ####
 
writeData(wb, "Trust in NISRA",
          x = paste0("Trust in NISRA by Age Group - ", current_year),
          startRow = r)

addStyle(wb, "Trust in NISRA",
         style = pt2,
         rows = r,
         cols = 1)

r <- r + 1

writeDataTable(wb, "Trust in NISRA",
               x = trust_age_z_scores,
               startRow = r,
               tableStyle = "none",
               headerStyle = ch,
               withFilter = FALSE)

addStyle(wb, "Trust in NISRA",
         style = ns3d,
         rows = (r + 1):(r + nrow(trust_age_z_scores)),
         cols = 2:ncol(trust_age_z_scores),
         gridExpand = TRUE)

for (i in 1:nrow(trust_age_z_scores)) {
  for (j in 2:ncol(trust_age_z_scores)) {
    if (!is.na(trust_age_z_scores[i, j])) {
      if (abs(trust_age_z_scores[i, j]) > 1.96) {
        addStyle(wb, "Trust in NISRA",
                 style = sig,
                 rows = r + i,
                 cols = j)
      } else {
        addStyle(wb, "Trust in NISRA",
                 style = not_sig,
                 rows = r + i,
                 cols = j)
      }
    }
  }
}

for (i in 1:nrow(trust_age_z_scores)) {
  addStyle(wb, "Trust in NISRA",
           style = grey,
           rows = r + i,
           cols = 1 + i)
}

r <- r + nrow(trust_age_z_scores) + 2

# Save Workbook ####

saveWorkbook(wb,
             paste0(here(), "/outputs/significance outputs/significance output ", current_year, ".xlsx"),
             overwrite = TRUE)
