library(here)
source(paste0(here(), "/code/significance testing/final_output/significance_testing.R"))

if (!exists (paste0(here(), "/outputs/significance outputs"))) {
  dir.create(paste0(here(), "/outputs/significance outputs"))
}

# Create Workbook ####

wb <- createWorkbook()
modifyBaseFont(wb, fontSize = 12, fontName = "Arial")

# This year vs last year with DKs ####

addWorksheet(wb, paste0(current_year, "vs", current_year - 1, "withDKs"))

setColWidths(wb,  paste0(current_year, "vs", current_year - 1, "withDKs"),
             cols = c(5, 10),
             widths = 5)

r <- 1

writeData(wb, paste0(current_year, "vs", current_year - 1, "withDKs"),
          x = paste0(current_year, " vs ", current_year - 1, " with Don't Know responses (UNWEIGHTED)"),
          startRow = r)

addStyle(wb, paste0(current_year, "vs", current_year - 1, "withDKs"),
         style = pt,
         rows = r,
         cols = 1)

r <- r + 2

## Awareness of NISRA ####

f_insert_sig_table(df = awareness_year,
                   sheet = paste0(current_year, "vs", current_year - 1, "withDKs"),
                   title = paste0("Awareness of NISRA - ", current_year - 1, " vs ", current_year))

## Trust NISRA ####

f_insert_sig_table(df = trust_year,
                   sheet = paste0(current_year, "vs", current_year - 1, "withDKs"),
                   title = paste0("Trust in NISRA - ", current_year, " vs ", current_year - 1))

r <- r - nrow(trust_year) - 3

f_insert_sig_table(df = distrust_year,
                   sheet = paste0(current_year, "vs", current_year - 1, "withDKs"),
                   title = paste0("Distrust in NISRA - ", current_year, " vs ", current_year - 1),
                   c = ncol(trust_year) + 2)

r <- r - nrow(distrust_year) - 3

f_insert_sig_table(df = dont_know_trust,
                   sheet = paste0(current_year, "vs", current_year - 1, "withDKs"),
                   title = paste0("Don't know trust in NISRA - ", current_year, " vs ", current_year - 1),
                   c = ncol(trust_year) + ncol(distrust_year) + 3)

## Trust NISRA statistics ####

f_insert_sig_table(df = trust_stats_year,
                   sheet = paste0(current_year, "vs", current_year - 1, "withDKs"),
                   title = paste0("Trust in NISRA Statistics - ", current_year, " vs ", current_year - 1))

r <- r - nrow(trust_stats_year) - 3

f_insert_sig_table(df = distrust_stats_year,
                   sheet = paste0(current_year, "vs", current_year - 1, "withDKs"),
                   title = paste0("Distrust in NISRA Statistics - ", current_year, " vs ", current_year - 1),
                   c = ncol(trust_stats_year) + 2)

r <- r - nrow(distrust_stats_year) - 3

f_insert_sig_table(df = dont_know_trust_stats_year,
                   sheet = paste0(current_year, "vs", current_year - 1, "withDKs"),
                   title = paste0("Don't know trust in NISRA Statistics - ", current_year, " vs ", current_year - 1),
                   c = ncol(trust_stats_year) + ncol(distrust_stats_year) + 3)

## NISRA Stats Importance ####

f_insert_sig_table(df = value_year,
                   sheet = paste0(current_year, "vs", current_year - 1, "withDKs"),
                   title = paste0("NISRA Stats are important - ", current_year, " vs ", current_year - 1))

r <- r - nrow(value_year) - 3

f_insert_sig_table(df = no_value_year,
                   sheet = paste0(current_year, "vs", current_year - 1, "withDKs"),
                   title = paste0("NISRA stats are not important - ", current_year, " vs ", current_year - 1),
                   c = ncol(value_year) + 2)

r <- r - nrow(no_value_year) - 3

f_insert_sig_table(df = dont_know_value_year,
                   sheet = paste0(current_year, "vs", current_year - 1, "withDKs"),
                   title = paste0("Don't know if NISRA stats are important - ", current_year, " vs ", current_year - 1),
                   c = ncol(value_year) + ncol(no_value_year) + 3)

## Interference ####

f_insert_sig_table(df = interference_year,
                   sheet = paste0(current_year, "vs", current_year - 1, "withDKs"),
                   title = paste0("NISRA Stats are free from political interference - ", current_year, " vs ", current_year - 1))

r <- r - nrow(interference_year) - 3

f_insert_sig_table(df = no_interference_year,
                   sheet = paste0(current_year, "vs", current_year - 1, "withDKs"),
                   title = paste0("NISRA stats are not free from political interference - ", current_year, " vs ", current_year - 1),
                   c = ncol(value_year) + 2)

r <- r - nrow(no_interference_year) - 3

f_insert_sig_table(df = dont_know_interference_year,
                   sheet = paste0(current_year, "vs", current_year - 1, "withDKs"),
                   title = paste0("Don't know if NISRA stats are free from political interference - ", current_year, " vs ", current_year - 1),
                   c = ncol(value_year) + ncol(no_value_year) + 3)

## Confidential ####

f_insert_sig_table(df = confidential_year,
                   sheet = paste0(current_year, "vs", current_year - 1, "withDKs"),
                   title = paste0("NISRA keeps my information confidential - ", current_year, " vs ", current_year - 1))

r <- r - nrow(confidential_year) - 3

f_insert_sig_table(df = no_confidential_year,
                   sheet = paste0(current_year, "vs", current_year - 1, "withDKs"),
                   title = paste0("NISRA does not keep my information confidential - ", current_year, " vs ", current_year - 1),
                   c = ncol(confidential_year) + 2)

r <- r - nrow(no_confidential_year) - 3

f_insert_sig_table(df = dont_know_confidential_year,
                   sheet = paste0(current_year, "vs", current_year - 1, "withDKs"),
                   title = paste0("Don't know if NISRA keeps my information confidential - ", current_year, " vs ", current_year - 1),
                   c = ncol(confidential_year) + ncol(no_confidential_year) + 3)

## Trust in Civil Service ####

f_insert_sig_table(df = trust_nics_year,
                   sheet = paste0(current_year, "vs", current_year - 1, "withDKs"),
                   title = paste0("Trust in the Civil Service - ", current_year, " vs ", current_year - 1))

r <- r - nrow(trust_nics_year) - 3

f_insert_sig_table(df = distrust_nics_year,
                   sheet = paste0(current_year, "vs", current_year - 1, "withDKs"),
                   title = paste0("Distrust in the Civil Service - ", current_year, " vs ", current_year - 1),
                   c = ncol(trust_nics_year) + 2)

r <- r - nrow(distrust_nics_year) - 3

f_insert_sig_table(df = dont_know_trust_nics,
                   sheet = paste0(current_year, "vs", current_year - 1, "withDKs"),
                   title = paste0("Don't know if trust in the Civil Service - ", current_year, " vs ", current_year - 1),
                   c = ncol(trust_nics_year) + ncol(distrust_nics_year) + 3)

## Trust in NI Assembly ####

f_insert_sig_table(df = trust_assembly_year,
                   sheet = paste0(current_year, "vs", current_year - 1, "withDKs"),
                   title = paste0("Trust in the Assembly - ", current_year, " vs ", current_year - 1))

r <- r - nrow(trust_assembly_year) - 3

f_insert_sig_table(df = distrust_assembly_year,
                   sheet = paste0(current_year, "vs", current_year - 1, "withDKs"),
                   title = paste0("Distrust in the Assembly - ", current_year, " vs ", current_year - 1),
                   c = ncol(trust_assembly_year) + 2)

r <- r - nrow(distrust_assembly_year) - 3

f_insert_sig_table(df = dont_know_trust_assembly,
                   sheet = paste0(current_year, "vs", current_year - 1, "withDKs"),
                   title = paste0("Don't know if trust in the Assembly - ", current_year, " vs ", current_year - 1),
                   c = ncol(trust_assembly_year) + ncol(distrust_assembly_year) + 3)

## Trust in the Media ####

f_insert_sig_table(df = trust_media_year,
                   sheet = paste0(current_year, "vs", current_year - 1, "withDKs"),
                   title = paste0("Trust in the Media - ", current_year, " vs ", current_year - 1))

r <- r - nrow(trust_media_year) - 3

f_insert_sig_table(df = distrust_media_year,
                   sheet = paste0(current_year, "vs", current_year - 1, "withDKs"),
                   title = paste0("Distrust in the Media - ", current_year, " vs ", current_year - 1),
                   c = ncol(trust_media_year) + 2)

r <- r - nrow(distrust_media_year) - 3

f_insert_sig_table(df = dont_know_trust_media,
                   sheet = paste0(current_year, "vs", current_year - 1, "withDKs"),
                   title = paste0("Don't know if trust in the Media - ", current_year, " vs ", current_year - 1),
                   c = ncol(trust_media_year) + ncol(distrust_media_year) + 3)

# This year vs last year excl DKs ####

addWorksheet(wb, paste0(current_year, "vs", current_year - 1, "exclDKs"))

setColWidths(wb,  paste0(current_year, "vs", current_year - 1, "exclDKs"),
             cols = c(5, 10),
             widths = 5)

r <- 1

writeData(wb, paste0(current_year, "vs", current_year - 1, "exclDKs"),
          x = paste0(current_year, " vs ", current_year - 1, " excl Don't Know responses (UNWEIGHTED)"),
          startRow = r)

addStyle(wb, paste0(current_year, "vs", current_year - 1, "exclDKs"),
         style = pt,
         rows = r,
         cols = 1)

r <- r + 2

## Trust in NISRA ####

f_insert_sig_table(trust_year_ex_dk,
                   sheet = paste0(current_year, "vs", current_year - 1, "exclDKs"),
                   title = paste0("Trust in NISRA - ", current_year, " vs ", current_year - 1))

## Trust in NISRA Statistics ####

f_insert_sig_table(trust_stats_year_ex_dk,
                   sheet = paste0(current_year, "vs", current_year - 1, "exclDKs"),
                   title = paste0("Trust in NISRA Statistics - ", current_year, " vs ", current_year - 1))

## NISRA Stats Importance ####

f_insert_sig_table(value_year_ex_dk,
                   sheet = paste0(current_year, "vs", current_year - 1, "exclDKs"),
                   title = paste0("NISRA Stats are important - ", current_year, " vs ", current_year - 1))

## Interference ####

f_insert_sig_table(interference_year_ex_dk,
                   sheet = paste0(current_year, "vs", current_year - 1, "exclDKs"),
                   title = paste0("NISRA Stats are free from political interference - ", current_year, " vs ", current_year - 1))

## Confidential ####

f_insert_sig_table(confidential_year_ex_dk,
                   sheet = paste0(current_year, "vs", current_year - 1, "exclDKs"),
                   title = paste0("NISRA keeps my information confidential - ", current_year, " vs ", current_year - 1))

## Trust in the Civil Service ####

f_insert_sig_table(trust_nics_year_ex_dk,
                   sheet = paste0(current_year, "vs", current_year - 1, "exclDKs"),
                   title = paste0("Trust in the Civil Service - ", current_year, " vs ", current_year - 1))

## Trust in the Assembly ####

f_insert_sig_table(trust_assembly_year_ex_dk,
                   sheet = paste0(current_year, "vs", current_year - 1, "exclDKs"),
                   title = paste0("Trust in the Assembly - ", current_year, " vs ", current_year - 1))

## Trust in the Media ####

f_insert_sig_table(trust_media_year_ex_dk,
                   sheet = paste0(current_year, "vs", current_year - 1, "exclDKs"),
                   title = paste0("Trust in the Media - ", current_year, " vs ", current_year - 1))

# ONS vs NISRA ####

addWorksheet(wb, "ONSvsNISRA")

r <- 1

writeData(wb, "ONSvsNISRA",
          x = "NISRA vs ONS Comparisons (UNWEIGHTED)",
          startRow = r)

addStyle(wb, "ONSvsNISRA",
         style = pt,
         rows = r,
         cols = 1)

r <- r + 2

## Heard of NISRA vs heard of ONS ####

f_insert_sig_table(df = heard_nisra_ons,
                   sheet = "ONSvsNISRA",
                   title = "Awareness of NISRA vs Awareness of ONS")

## Trust NISRA vs Trust ONS ####

f_insert_sig_table(df = trust_nisra_ons,
                   sheet = "ONSvsNISRA",
                   title = "Trust in NISRA vs Trust in ONS")

## Trust NISRA stats vs Trust ONS stats ####

f_insert_sig_table(df = trust_stats_nisra_ons,
                   sheet = "ONSvsNISRA",
                   title = "Trust NISRA stats vs Trust ONS stats")

## Stats are important: NISRA vs ONS ####

f_insert_sig_table(df = value_nisra_ons,
                   sheet = "ONSvsNISRA",
                   title = "Stats are important: NISRA vs ONS")

## Stats are free from political interference: NISRA vs ONS ####

f_insert_sig_table(df = interference_nisra_ons,
                   sheet = "ONSvsNISRA",
                   title = "Stats are free from political interference: NISRA vs ONS")

## Information will be kept confidential: NISRA vs ONS ####

f_insert_sig_table(df = confidential_nisra_ons,
                   sheet = "ONSvsNISRA",
                   title = "Information will be kept confidential: NISRA vs ONS")

# ONSvNISRAexcDKs ####

addWorksheet(wb, "ONSvNISRAexcDKs")

r <- 1

writeData(wb, "ONSvNISRAexcDKs",
          x = "NISRA vs ONS (exc DKs) (UNWEIGHTED)",
          startRow = r)

addStyle(wb, "ONSvNISRAexcDKs",
         style = pt,
         rows = r,
         cols = 1)

r <- r + 2

## Heard of NISRA vs heard of ONS (exc DKs) ####

f_insert_sig_table(df = nisra_ons_heard_ex_dk,
                   sheet = "ONSvNISRAexcDKs",
                   title = "Awareness of NISRA vs awareness of ONS (exc DKs)")

## Trust in NISRA vs Trust in ONS (exc DKs) ####

f_insert_sig_table(df = nisra_ons_trust_ex_dk,
                   sheet = "ONSvNISRAexcDKs",
                   title = "Trust in NISRA vs Trust in ONS (exc DKs)")

## Trust in NISRA stats vs Trust in ONS stats (exc DKs) ####

f_insert_sig_table(df = nisra_ons_trust_stats_ex_dk,
                   sheet = "ONSvNISRAexcDKs",
                   title = "Trust in NISRA stats vs Trust in ONS stats (exc DKs)")

## NISRA stats are important vs ONS stats are important (exc DKs) ####

f_insert_sig_table(df = nisra_ons_important_ex_dk,
                   sheet = "ONSvNISRAexcDKs",
                   title = "NISRA stats are important vs ONS stats are important (exc DKs)")

## NISRA stats are free from political interference vs ONS stats are free from political interference (exc DKs) ####

f_insert_sig_table(df = nisra_ons_political_ex_dk,
                   sheet = "ONSvNISRAexcDKs",
                   title = "Free from political interference: NISRA stats vs ONS stats (exc DKs)")

## NISRA will keep my information confidential vs ONS will keep my information confidential (exc DKs) ####

f_insert_sig_table(df = nisra_ons_confidential_ex_dk,
                   sheet = "ONSvNISRAexcDKs",
                   title = "Will keep my information confidential: NISRA vs ONS (exc DKs)")



# Awareness of NISRA ####
 
addWorksheet(wb, "Awareness")

r <- 1

writeData(wb, "Awareness",
          x = "Awareness of NISRA (UNWEIGHTED)",
          startRow = r)

addStyle(wb, "Awareness",
         style = pt,
         rows = r,
         cols = 1)

r <- r + 2

## Trend ####

f_insert_sig_table(df = aware_trend,
                   sheet = "Awareness",
                   title = paste0("Awareness of NISRA - 2014 to ", current_year))

f_insert_z_table(df = aware_trend_z_scores,
                 sheet = "Awareness",
                 title = paste0("Awareness of NISRA - 2014 to ", current_year))

## In Work vs Not in work ####

f_insert_sig_table(df = work_status,
                   sheet = "Awareness",
                   title = paste0("Awareness of NISRA - In Work vs Not in Work - ", current_year))

## Age groups ####
 
f_insert_sig_table(df = age_stats,
                   sheet = "Awareness",
                   title = paste0("Awareness of NISRA - by Age Group - ", current_year))
 
f_insert_z_table(df = age_z_scores,
                 sheet = "Awareness",
                 title = paste0("Awareness of NISRA by Age Group - ", current_year))

## Qualifications ####

f_insert_sig_table(df = qual_stats,
                   sheet = "Awareness",
                   title = paste0("Awareness of NISRA - by Highest Qualification Achieved - ", current_year))

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
          x = "Products (UNWEIGHTED)",
          startRow = r)

addStyle(wb, "Products",
         style = pt,
         rows = r,
         cols = 1)

r <- r + 2

## Had heard of NISRA: This year vs previous year ####
 
writeData(wb, "Products",
          x = paste0("Aware produced by NISRA (Heard of NISRA) - ", current_year - 1, " vs ", current_year),
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
          x = paste0("Aware of statistics (Had not heard of NISRA) - ", current_year - 1, " vs ", current_year),
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
             cols = 1:ncol(heard_stats),
             widths = c(55, rep(12, ncol(heard_stats) - 1)))

# Trust in NISRA ####
 
addWorksheet(wb, "Trust in NISRA")

r <- 1

writeData(wb, "Trust in NISRA",
          x = "Trust in NISRA (UNWEIGHTED)",
          startRow = r)

addStyle(wb, "Trust in NISRA",
         style = pt,
         rows = r,
         cols = 1)

r <- r + 2

## Trend ####

f_insert_sig_table(df = trust_trend,
                   sheet = "Trust in NISRA",
                   title = paste0("Trust in NISRA - 2014 to ", current_year))

f_insert_z_table(df = trust_trend_z_scores_yes,
                 sheet = "Trust in NISRA",
                 title = paste0("Yes - Trust in NISRA - 2014 to ", current_year))

f_insert_z_table(df = trust_trend_z_scores_no,
                 sheet = "Trust in NISRA",
                 title = paste0("No - Trust in NISRA - 2014 to ", current_year))

f_insert_z_table(df = trust_trend_z_scores_dk,
                 sheet = "Trust in NISRA",
                 title = paste0("Don't know - Trust in NISRA - 2014 to ", current_year))

## Trust in NISRA by Work Status ####

f_insert_sig_table(df = trust_work_status,
                   sheet = "Trust in NISRA",
                   title = paste0("Trust in NISRA - In work vs Not in work - ", current_year))


## Trust in NISRA by Age group compare ####

f_insert_sig_table(df = trust_age_stats,
                   sheet = "Trust in NISRA",
                   title = paste0("Trust in NISRA - by Age Group - ", current_year))

f_insert_z_table(df = trust_age_z_scores, 
                 sheet = "Trust in NISRA",
                 title = paste0("Yes - Trust in NISRA by Age Group - ", current_year))

## Distrust in NISRA by Age group compare ####
 
f_insert_z_table(df = distrust_age_z_scores, 
                 sheet = "Trust in NISRA",
                 title = paste0("No - Trust in NISRA by Age Group - ", current_year))

## Don't know trust in NISRA by Age group compare ####
 
f_insert_z_table(df = dont_know_trust_age_z_scores, 
                 sheet = "Trust in NISRA",
                 title = paste0("Don't know - Trust in NISRA by Age Group - ", current_year))

## Trust in NISRA by Qualification compare ####

f_insert_sig_table(df = trust_qual_stats,
                   sheet = "Trust in NISRA",
                   title = paste0("Trust in NISRA - by highest qualification achieved - ", current_year))

f_insert_z_table(df = trust_qual_z_scores, 
                 sheet = "Trust in NISRA",
                 title = paste0("Yes - Trust in NISRA by highest qualification achieved - ", current_year))

## Distrust in NISRA by Qualification compare ####

f_insert_z_table(df = distrust_qual_z_scores, 
                 sheet = "Trust in NISRA",
                 title = paste0("No - Trust in NISRA by highest qualification achieved - ", current_year))

## Don't know trust in NISRA by Qualification compare ####

f_insert_z_table(df = dont_know_qual_age_z_scores, 
                 sheet = "Trust in NISRA",
                 title = paste0("Don't know - Trust in NISRA by highest qualification achieved - ", current_year))

# Value ##

addWorksheet(wb, "Value")

setColWidths(wb, "Value",
             cols = 1:ncol(value_age_z_scores),
             widths = c(47, rep(12, ncol(value_age_z_scores) - 1)))

r <- 1

writeData(wb, "Value",
          x = "NISRA Statistics are important to understand Northern Ireland",
          startRow = r)

addStyle(wb, "Value",
         style = pt,
         rows = r,
         cols = 1)

r <- r + 2

f_insert_sig_table(df = value_work_stats,
                   sheet = "Value",
                   title = paste("NISRA stats are important - In work vs Not in work -", current_year))

f_insert_sig_table(df = value_age_stats,
                   sheet = "Value",
                   title = paste("NISRA stats are important - In work vs Not in work -", current_year))

f_insert_z_table(df = value_age_z_scores,
                 sheet = "Value",
                 title = paste0("NISRA stats are important by Age Group - ", current_year))

f_insert_z_table(df = value_disagree_age_z_scores,
                 sheet = "Value",
                 title = paste0("NISRA stats are not important by Age Group - ", current_year))

f_insert_z_table(df = value_dont_know_age_z_scores,
                 sheet = "Value",
                 title = paste0("Don't know if NISRA stats are important by Age Group - ", current_year))

f_insert_sig_table(df = value_qual_stats,
                   sheet = "Value",
                   title = paste("NISRA stats are important - By highest qualification achieved -", current_year))

f_insert_z_table(df = value_qual_z_scores,
                 sheet = "Value",
                 title = paste0("NISRA stats are important by highest qualification achieved - ", current_year))

f_insert_z_table(df = value_disagree_qual_z_scores,
                 sheet = "Value",
                 title = paste0("NISRA stats are not important by highest qualification achieved - ", current_year))

f_insert_z_table(df = value_dont_know_qual_z_scores,
                 sheet = "Value",
                 title = paste0("Don't know if NISRA stats are important by highest qualification achieved - ", current_year))

# Trust in NISRA (exc DK) ####

addWorksheet(wb, "TruNISRAexcDK")

setColWidths(wb, "TruNISRAexcDK",
             cols = 1:ncol(trust_nisra_age_z_scores_ex_dk),
             widths = c(47, rep(12, ncol(trust_nisra_age_z_scores_ex_dk) - 1)))

r <- 1

writeData(wb, "TruNISRAexcDK",
          x = "Trust in NISRA (excluding Don't knows)",
          startRow = r)

addStyle(wb, "TruNISRAexcDK",
         style = pt,
         rows = r,
         cols = 1)

r <- r + 2

## In work vs not in work ####

f_insert_sig_table(df = trust_nisra_work_ex_dk,
                   sheet = "TruNISRAexcDK",
                   title = "Trust in NISRA: in work vs not in work (exc DKs)")

## By age ####

f_insert_sig_table(df = trust_nisra_age_ex_dk,
                   sheet = "TruNISRAexcDK",
                   title = "Trust in NISRA: by Age Group (exc DKs)")

## Age comparison ####

f_insert_z_table(df = trust_nisra_age_z_scores_ex_dk,
                 sheet = "TruNISRAexcDK",
                 title = "Trust in NISRA: by Age Group (exc DKs)")

## By qualification ####

f_insert_sig_table(df = trust_nisra_qual_ex_dk,
                   sheet = "TruNISRAexcDK",
                   title = "Trust in NISRA: by Highest Qualification Achieved (exc DKs)")

## Qualification comparison ####

f_insert_z_table(df = trust_nisra_qual_z_scores_ex_dk,
                 sheet = "TruNISRAexcDK",
                 title = "Trust in NISRA: by Age Group (exc DKs)")

# Interference ####

addWorksheet(wb, "Interference")

setColWidths(wb, "Interference",
             cols = 1:ncol(interference_age_z_scores),
             widths = c(47, rep(12, ncol(interference_age_z_scores) - 1)))

r <- 1

writeData(wb, "Interference",
          x = "NISRA Statistics are free from political interference",
          startRow = r)

addStyle(wb, "Interference",
         style = pt,
         rows = r,
         cols = 1)

r <- r + 2

f_insert_sig_table(df = interference_work_stats,
                   sheet = "Interference",
                   title = paste("NISRA stats are free from political interference - In work vs Not in work -", current_year))

f_insert_sig_table(df = interference_age_stats,
                   sheet = "Interference",
                   title = paste("NISRA stats are free from political interference - by Age Group -", current_year))

f_insert_z_table(df = interference_age_z_scores,
                 sheet = "Interference",
                 title = paste0("NISRA stats are free from political interference by Age Group - ", current_year))

f_insert_z_table(df = interference_disagree_age_z_scores,
                 sheet = "Interference",
                 title = paste0("NISRA stats are not free from political interference by Age Group - ", current_year))

f_insert_z_table(df = interference_dont_know_age_z_scores,
                 sheet = "Interference",
                 title = paste0("Don't know if NISRA stats are free from political interference by Age Group - ", current_year))

f_insert_sig_table(df = interference_qual_stats,
                   sheet = "Interference",
                   title = paste("NISRA stats are free from political interference - By highest qualification achieved -", current_year))

f_insert_z_table(df = interference_qual_z_scores,
                 sheet = "Interference",
                 title = paste0("NISRA stats are free from political interference by highest qualification achieved - ", current_year))

f_insert_z_table(df = interference_disagree_qual_z_scores,
                 sheet = "Interference",
                 title = paste0("NISRA stats are not free from political interference by highest qualification achieved - ", current_year))

f_insert_z_table(df = interference_dont_know_qual_z_scores,
                 sheet = "Interference",
                 title = paste0("Don't know if NISRA stats are free from political interference by highest qualification achieved - ", current_year))

# Confidentiality ####

addWorksheet(wb, "Confidentiality")

setColWidths(wb, "Confidentiality",
             cols = 1:ncol(confidential_age_z_scores),
             widths = c(47, rep(12, ncol(confidential_age_z_scores) - 1)))

r <- 1

writeData(wb, "Confidentiality",
          x = "NISRA will keep my information confidential",
          startRow = r)

addStyle(wb, "Confidentiality",
         style = pt,
         rows = r,
         cols = 1)

r <- r + 2

f_insert_sig_table(df = confidential_work_stats,
                   sheet = "Confidentiality",
                   title = paste("NISRA will keep my information confidential - In work vs Not in work -", current_year))

f_insert_sig_table(df = confidential_age_stats,
                   sheet = "Confidentiality",
                   title = paste("NISRA will keep my information confidential - by Age Group -", current_year))

f_insert_z_table(df = confidential_age_z_scores,
                 sheet = "Confidentiality",
                 title = paste0("NISRA will keep my information confidential by Age Group - ", current_year))

f_insert_z_table(df = confidential_disagree_age_z_scores,
                 sheet = "Confidentiality",
                 title = paste0("NISRA will not keep my information confidential by Age Group - ", current_year))

f_insert_z_table(df = confidential_dont_know_age_z_scores,
                 sheet = "Confidentiality",
                 title = paste0("Don't know if NISRA will keep my information confidential by Age Group - ", current_year))

f_insert_sig_table(df = confidential_qual_stats,
                   sheet = "Confidentiality",
                   title = paste("NISRA will keep my information confidential - By highest qualification achieved -", current_year))

f_insert_z_table(df = confidential_qual_z_scores,
                 sheet = "Confidentiality",
                 title = paste0("NISRA will keep my information confidential by highest qualification achieved - ", current_year))

f_insert_z_table(df = confidential_disagree_qual_z_scores,
                 sheet = "Confidentiality",
                 title = paste0("NISRA will not keep my information confidential by highest qualification achieved - ", current_year))

f_insert_z_table(df = confidential_dont_know_qual_z_scores,
                 sheet = "Confidentiality",
                 title = paste0("Don't know if NISRA will keep my information confidential by highest qualification achieved - ", current_year))

# Trust NISRA Statistics ####

addWorksheet(wb, "TrustNISRAStats")

setColWidths(wb, "TrustNISRAStats",
             cols = 1:ncol(trust_stats_age_z_scores),
             widths = c(47, rep(12, ncol(trust_stats_age_z_scores) - 1)))

r <- 1

writeData(wb, "TrustNISRAStats",
          x = "Trust in NISRA Statistics",
          startRow = r)

addStyle(wb, "TrustNISRAStats",
         style = pt,
         rows = r,
         cols = 1)

r <- r + 2

f_insert_sig_table(df = trust_stats_work_stats,
                   sheet = "TrustNISRAStats",
                   title = paste("Trust in NISRA Statistics - In work vs Not in work -", current_year))

f_insert_sig_table(df = trust_stats_age_stats,
                   sheet = "TrustNISRAStats",
                   title = paste("Trust in NISRA Statistics - by Age Group -", current_year))

f_insert_z_table(df = trust_stats_age_z_scores,
                 sheet = "TrustNISRAStats",
                 title = paste0("Trust in NISRA Statistics by Age Group - ", current_year))

f_insert_z_table(df = trust_stats_disagree_age_z_scores,
                 sheet = "TrustNISRAStats",
                 title = paste0("Distrust in NISRA Statistics by Age Group - ", current_year))

f_insert_z_table(df = trust_stats_dont_know_age_z_scores,
                 sheet = "TrustNISRAStats",
                 title = paste0("Don't know if Trust in NISRA Statistics by Age Group - ", current_year))

f_insert_sig_table(df = trust_stats_qual_stats,
                   sheet = "TrustNISRAStats",
                   title = paste("Trust in NISRA Statistics - By highest qualification achieved -", current_year))

f_insert_z_table(df = trust_stats_qual_z_scores,
                 sheet = "TrustNISRAStats",
                 title = paste0("Trust in NISRA Statistics by highest qualification achieved - ", current_year))

f_insert_z_table(df = trust_stats_disagree_qual_z_scores,
                 sheet = "TrustNISRAStats",
                 title = paste0("Distrust in NISRA Statistics by highest qualification achieved - ", current_year))

f_insert_z_table(df = trust_stats_dont_know_qual_z_scores,
                 sheet = "TrustNISRAStats",
                 title = paste0("Don't know if Trust in NISRA Statistics by highest qualification achieved - ", current_year))


# Trust NISRA stats (exc DK) ####

addWorksheet(wb, "TruNISRAStatsexcDK")

setColWidths(wb, "TruNISRAStatsexcDK",
             cols = 1:ncol(trust_stats_age_z_scores_ex_dk),
             widths = c(47, rep(12, ncol(trust_stats_age_z_scores_ex_dk) - 1)))

r <- 1

writeData(wb, "TruNISRAStatsexcDK",
          x = "Trust NISRA stats (excluding Don't knows)",
          startRow = r)

addStyle(wb, "TruNISRAStatsexcDK",
         style = pt,
         rows = r,
         cols = 1)

r <- r + 2

## In work vs not in work ####

f_insert_sig_table(df = trust_stats_work_ex_dk,
                   sheet = "TruNISRAStatsexcDK",
                   title = "Trust NISRA stats: in work vs not in work (exc DKs)")

## By age ####

f_insert_sig_table(df = trust_stats_age_ex_dk,
                   sheet = "TruNISRAStatsexcDK",
                   title = "Trust NISRA stats: by Age Group (exc DKs)")

## Age comparison ####

f_insert_z_table(df = trust_stats_age_z_scores_ex_dk,
                 sheet = "TruNISRAStatsexcDK",
                 title = "Trust NISRA stats: by Age Group (exc DKs)")

## By qualification ####

f_insert_sig_table(df = trust_stats_qual_ex_dk,
                   sheet = "TruNISRAStatsexcDK",
                   title = "Trust NISRA stats: by Highest Qualification Achieved (exc DKs)")

## Qualification comparison ####

f_insert_z_table(df = trust_stats_qual_z_scores_ex_dk,
                 sheet = "TruNISRAStatsexcDK",
                 title = "Trust NISRA stats: by Age Group (exc DKs)")

# Value NISRA stats (exc DK) ####

addWorksheet(wb, "ValuesExDK")

setColWidths(wb, "ValuesExDK",
             cols = 1:ncol(value_age_z_scores_ex_dk),
             widths = c(47, rep(12, ncol(value_age_z_scores_ex_dk) - 1)))

r <- 1

writeData(wb, "ValuesExDK",
          x = "Value NISRA stats (excluding Don't knows)",
          startRow = r)

addStyle(wb, "ValuesExDK",
         style = pt,
         rows = r,
         cols = 1)

r <- r + 2

## In work vs not in work ####

f_insert_sig_table(df = value_work_ex_dk,
                   sheet = "ValuesExDK",
                   title = "Value NISRA stats: in work vs not in work (exc DKs)")

## By age ####

f_insert_sig_table(df = value_age_ex_dk,
                   sheet = "ValuesExDK",
                   title = "Value NISRA stats: by Age Group (exc DKs)")

## Age comparison ####

f_insert_z_table(df = value_age_z_scores_ex_dk,
                 sheet = "ValuesExDK",
                 title = "Value NISRA stats: by Age Group (exc DKs)")

## By qualification ####

f_insert_sig_table(df = value_qual_ex_dk,
                   sheet = "ValuesExDK",
                   title = "Value NISRA stats: by Highest Qualification Achieved (exc DKs)")

## Qualification comparison ####

f_insert_z_table(df = value_qual_z_scores_ex_dk,
                 sheet = "ValuesExDK",
                 title = "Value NISRA stats: by Age Group (exc DKs)")

# NISRA stats free from interference (exc DK) ####
 
addWorksheet(wb, "InterfExDK")

setColWidths(wb, "InterfExDK",
             cols = 1:ncol(interference_age_z_scores_ex_dk),
             widths = c(47, rep(12, ncol(interference_age_z_scores_ex_dk) - 1)))

r <- 1

writeData(wb, "InterfExDK",
          x = "NISRA stats are free from political interference (excluding Don't knows)",
          startRow = r)

addStyle(wb, "InterfExDK",
         style = pt,
         rows = r,
         cols = 1)

r <- r + 2

## In work vs not in work ####

f_insert_sig_table(df = interference_work_ex_dk,
                   sheet = "InterfExDK",
                   title = "NISRA stats are free from political interference: in work vs not in work (exc DKs)")

## By age ####

f_insert_sig_table(df = interference_age_ex_dk,
                   sheet = "InterfExDK",
                   title = "NISRA stats are free from political interference: by Age Group (exc DKs)")

## Age comparison ####

f_insert_z_table(df = interference_age_z_scores_ex_dk,
                 sheet = "InterfExDK",
                 title = "NISRA stats are free from political interference: by Age Group (exc DKs)")

## By qualification ####

f_insert_sig_table(df = interference_qual_ex_dk,
                   sheet = "InterfExDK",
                   title = "NISRA stats are free from political interference: by Highest Qualification Achieved (exc DKs)")

## Qualification comparison ####

f_insert_z_table(df = interference_qual_z_scores_ex_dk,
                 sheet = "InterfExDK",
                 title = "NISRA stats are free from political interference: by Age Group (exc DKs)")

# NISRA will keep my information confidential (exc DK) ####

addWorksheet(wb, "ConfExDK")

setColWidths(wb, "ConfExDK",
             cols = 1:ncol(confidential_age_z_scores_ex_dk),
             widths = c(47, rep(12, ncol(confidential_age_z_scores_ex_dk) - 1)))

r <- 1

writeData(wb, "ConfExDK",
          x = "NISRA will keep my information confidential (excluding Don't knows)",
          startRow = r)

addStyle(wb, "ConfExDK",
         style = pt,
         rows = r,
         cols = 1)

r <- r + 2

## In work vs not in work ####

f_insert_sig_table(df = confidential_work_ex_dk,
                   sheet = "ConfExDK",
                   title = "NISRA will keep my information confidential: in work vs not in work (exc DKs)")

## By age ####

f_insert_sig_table(df = confidential_age_ex_dk,
                   sheet = "ConfExDK",
                   title = "NISRA will keep my information confidential: by Age Group (exc DKs)")

## Age comparison ####

f_insert_z_table(df = confidential_age_z_scores_ex_dk,
                 sheet = "ConfExDK",
                 title = "NISRA will keep my information confidential: by Age Group (exc DKs)")

## By qualification ####

f_insert_sig_table(df = confidential_qual_ex_dk,
                   sheet = "ConfExDK",
                   title = "NISRA will keep my information confidential: by Highest Qualification Achieved (exc DKs)")

## Qualification comparison ####

f_insert_z_table(df = confidential_qual_z_scores_ex_dk,
                 sheet = "ConfExDK",
                 title = "NISRA will keep my information confidential: by Age Group (exc DKs)")

# Save Workbook ####

saveWorkbook(wb,
             paste0(here(), "/outputs/significance outputs/significance output ", current_year, ".xlsx"),
             overwrite = TRUE)

openXL(paste0(here(), "/outputs/significance outputs/significance output ", current_year, ".xlsx"))
