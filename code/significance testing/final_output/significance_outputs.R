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


# Save Workbook ####

saveWorkbook(wb,
             paste0(here(), "/outputs/significance outputs/significance output ", current_year, ".xlsx"),
             overwrite = TRUE)


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
  select(grouping1, var1, grouping2, var2, score) %>%
  rename(`Grouping 1` = grouping1, `Grouping 2` = grouping2, `z Score` = score) %>%
  arrange(var1)
excel_df$`z Score` <- as.numeric(excel_df$`z Score`)
library(stringr)

PCOS1_df <- subset(excel_df, var1 == "PCOS1") %>%
  select(-var1, -var2) %>%
  arrange(`Grouping 1`)
PCOS1c_df <- excel_df %>%
  filter(str_detect(var1, "PCOS1c")) %>%
  select(-var1, -var2) %>%
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
  if (!is.na(PCOS1_df[i, 3])) {
    if (abs(PCOS1_df[i, 3]) > 1.96) {
      addStyle(wb2, "PCOS1",
               style = sig,
               rows = r + i,
               cols = 3)
    } else {
      addStyle(wb2, "PCOS1",
               style = not_sig,
               rows = r + i,
               cols = 3)
    }
  }
}

for (i in 1:nrow(PCOS1c_df)) {
  if (!is.na(PCOS1c_df[i, 3])) {
    if (abs(PCOS1c_df[i, 3]) > 1.96) {
      addStyle(wb2, "PCOS1c",
               style = sig,
               rows = r + i,
               cols = 3)
    } else {
      addStyle(wb2, "PCOS1c",
               style = not_sig,
               rows = r + i,
               cols = 3)
    }
  }
}

for (i in 1:nrow(PCOS1d_df)) {
  if (!is.na(PCOS1d_df[i, 3])) {
    if (abs(PCOS1d_df[i, 3]) > 1.96) {
      addStyle(wb2, "PCOS1d",
               style = sig,
               rows = r + i,
               cols = 3)
    } else {
      addStyle(wb2, "PCOS1d",
               style = not_sig,
               rows = r + i,
               cols = 3)
    }
  }
}

for (i in 1:nrow(PCOS2a_df)) {
  if (!is.na(PCOS2a_df[i, 3])) {
    if (abs(PCOS2a_df[i, 3]) > 1.96) {
      addStyle(wb2, "PCOS2a",
               style = sig,
               rows = r + i,
               cols = 3)
    } else {
      addStyle(wb2, "PCOS2a",
               style = not_sig,
               rows = r + i,
               cols = 3)
    }
  }
}

for (i in 1:nrow(PCOS2b_df)) {
  if (!is.na(PCOS2b_df[i, 3])) {
    if (abs(PCOS2b_df[i, 3]) > 1.96) {
      addStyle(wb2, "PCOS2b",
               style = sig,
               rows = r + i,
               cols = 3)
    } else {
      addStyle(wb2, "PCOS2b",
               style = not_sig,
               rows = r + i,
               cols = 3)
    }
  }
}

for (i in 1:nrow(PCOS2c_df)) {
  if (!is.na(PCOS2c_df[i, 3])) {
    if (abs(PCOS2c_df[i, 3]) > 1.96) {
      addStyle(wb2, "PCOS2c",
               style = sig,
               rows = r + i,
               cols = 3)
    } else {
      addStyle(wb2, "PCOS2c",
               style = not_sig,
               rows = r + i,
               cols = 3)
    }
  }
}

for (i in 1:nrow(PCOS2d_df)) {
  if (!is.na(PCOS2d_df[i, 3])) {
    if (abs(PCOS2d_df[i, 3]) > 1.96) {
      addStyle(wb2, "PCOS2d",
               style = sig,
               rows = r + i,
               cols = 3)
    } else {
      addStyle(wb2, "PCOS2d",
               style = not_sig,
               rows = r + i,
               cols = 3)
    }
  }
}

for (i in 1:nrow(PCOS3_df)) {
  if (!is.na(PCOS3_df[i, 3])) {
    if (abs(PCOS3_df[i, 3]) > 1.96) {
      addStyle(wb2, "PCOS3",
               style = sig,
               rows = r + i,
               cols = 3)
    } else {
      addStyle(wb2, "PCOS3",
               style = not_sig,
               rows = r + i,
               cols = 3)
    }
  }
}

for (i in 1:nrow(PCOS4_df)) {
  if (!is.na(PCOS4_df[i, 3])) {
    if (abs(PCOS4_df[i, 3]) > 1.96) {
      addStyle(wb2, "PCOS4",
               style = sig,
               rows = r + i,
               cols = 3)
    } else {
      addStyle(wb2, "PCOS4",
               style = not_sig,
               rows = r + i,
               cols = 3)
    }
  }
}

for (i in 1:nrow(PCOS5_df)) {
  if (!is.na(PCOS5_df[i, 3])) {
    if (abs(PCOS5_df[i, 3]) > 1.96) {
      addStyle(wb2, "PCOS5",
               style = sig,
               rows = r + i,
               cols = 3)
    } else {
      addStyle(wb2, "PCOS5",
               style = not_sig,
               rows = r + i,
               cols = 3)
    }
  }
}

for (i in 1:nrow(PCOS6_df)) {
  if (!is.na(PCOS6_df[i, 3])) {
    if (abs(PCOS6_df[i, 3]) > 1.96) {
      addStyle(wb2, "PCOS6",
               style = sig,
               rows = r + i,
               cols = 3)
    } else {
      addStyle(wb2, "PCOS6",
               style = not_sig,
               rows = r + i,
               cols = 3)
    }
  }
}

r <- r + nrow(age_z_scores) + 2

saveWorkbook(wb2,
             paste0(here(), "/outputs/significance outputs/dfi significance output ", current_year, ".xlsx"),
             overwrite = TRUE)
