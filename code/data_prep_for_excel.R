# Placeholder for Excel table
library(here)

# Creating spreadsheets
source(paste0(here(),"/code/config.R"))
source(paste0(here(),"/code/data_prep.R"))
library(readxl)

# set basic formatting
modifyBaseFont(new_workbook, fontSize = 10, fontName = "Arial")

# Pub date
pub_date <- format(Sys.time(), "%d-%m-%Y")

text <- read_excel("C:/Users/2352605/Documents/71-ssb-pcos/code/Excel Outputs/Text.xlsx", sheet = "Text")
num_rows <- nrow(text)

#### create new workbook ####
# update meta data (creator and title)
new_workbook <- createWorkbook(creator = "Dept name",
                               title = "Add theme here")

# naming output file
output_file <- paste0("outputs/excel_tables.xlsx")

#### Add worksheets ####
addWorksheet(new_workbook, sheetName = "Introduction")
addWorksheet(new_workbook, sheetName = "Contents")
addWorksheet(new_workbook, sheetName = "Awareness_of_NISRA")
addWorksheet(new_workbook, sheetName = "Awareness_NISRA_Statistics")
addWorksheet(new_workbook, sheetName = "Aware_Statistics_by_NISRA")
addWorksheet(new_workbook, sheetName = "Trust_NISRA")
addWorksheet(new_workbook, sheetName = "Trust_Civil_Service")
addWorksheet(new_workbook, sheetName = "Trust_NI_Assembly")
addWorksheet(new_workbook, sheetName = "Trust_Media")
addWorksheet(new_workbook, sheetName = "Trust_NISRA_Statistics")
addWorksheet(new_workbook, sheetName = "Value")
addWorksheet(new_workbook, sheetName = "Political_Interference")
addWorksheet(new_workbook, sheetName = "Confidentiality")

for (i in 1:num_rows) {
  
  df <- text[i,]
  
  writeData(new_workbook,
            sheet = paste0(df$Sheet),
            x = df$Text,
            startRow = df$Number,
            colNames = TRUE)
}

# Contents Page
writeFormula(new_workbook, "Awareness_of_NISRA",x = '=HYPERLINK(#Table_1!A4","Table_1")',startCol = 1, startRow = 4)
writeFormula(new_workbook, "Awareness_of_NISRA",x = '=HYPERLINK(#Table_2!A11","Table_2")',startCol = 1, startRow = 5)
writeFormula(new_workbook, "Awareness_of_NISRA_Statistics",x = '=HYPERLINK(#Table_6!A6","Table_6")',startCol = 1, startRow = 7)
writeFormula(new_workbook, "Awareness_of_NISRA_Statistics",x = '=HYPERLINK(#Table_7!A11","Table_7")',startCol = 1, startRow = 8)
writeFormula(new_workbook, "Awareness_of_NISRA_Statistics",x = '=HYPERLINK(#Table_8!A17","Table_8")',startCol = 1, startRow = 9)
writeFormula(new_workbook, "Awareness_of_NISRA_Statistics",x = '=HYPERLINK(#Table_9!A23","Table_9")',startCol = 1, startRow = 10)
writeFormula(new_workbook, "Awareness_of_NISRA_Statistics",x = '=HYPERLINK(#Table_10!A29","Table_10")',startCol = 1, startRow = 11)
writeFormula(new_workbook, "Awareness_of_NISRA_Statistics",x = '=HYPERLINK(#Table_11!A35","Table_11")',startCol = 1, startRow = 12)
writeFormula(new_workbook, "Awareness_of_NISRA_Statistics",x = '=HYPERLINK(#Table_12!A41","Table_12")',startCol = 1, startRow = 13)
writeFormula(new_workbook, "Awareness_of_NISRA_Statistics",x = '=HYPERLINK(#Table_13!A47","Table_13")',startCol = 1, startRow = 14)
writeFormula(new_workbook, "Awareness_of_NISRA_Statistics",x = '=HYPERLINK(#Table_14!A53","Table_14")',startCol = 1, startRow = 15)
writeFormula(new_workbook, "Awareness_of_NISRA_Statistics",x = '=HYPERLINK(#Table_15!A59","Table_15")',startCol = 1, startRow = 16)
writeFormula(new_workbook, "Aware_Statistics_by_NISRA",x = '=HYPERLINK(#Table_16!A5","Table_16")',startCol = 1, startRow = 18)
writeFormula(new_workbook, "Aware_Statistics_by_NISRA",x = '=HYPERLINK(#Table_17!A11","Table_17")',startCol = 1, startRow = 19)
writeFormula(new_workbook, "Aware_Statistics_by_NISRA",x = '=HYPERLINK(#Table_18!A17","Table_18")',startCol = 1, startRow = 20)
writeFormula(new_workbook, "Aware_Statistics_by_NISRA",x = '=HYPERLINK(#Table_19!A23","Table_19")',startCol = 1, startRow = 21)
writeFormula(new_workbook, "Aware_Statistics_by_NISRA",x = '=HYPERLINK(#Table_20!A29","Table_20")',startCol = 1, startRow = 22)
writeFormula(new_workbook, "Aware_Statistics_by_NISRA",x = '=HYPERLINK(#Table_21!A35","Table_21")',startCol = 1, startRow = 23)
writeFormula(new_workbook, "Aware_Statistics_by_NISRA",x = '=HYPERLINK(#Table_22!A41","Table_22")',startCol = 1, startRow = 24)
writeFormula(new_workbook, "Aware_Statistics_by_NISRA",x = '=HYPERLINK(#Table_23!A47","Table_23")',startCol = 1, startRow = 25)
writeFormula(new_workbook, "Aware_Statistics_by_NISRA",x = '=HYPERLINK(#Table_24!A53","Table_24")',startCol = 1, startRow = 26)
writeFormula(new_workbook, "Aware_Statistics_by_NISRA",x = '=HYPERLINK(#Table_25!A59","Table_25")',startCol = 1, startRow = 27)
writeFormula(new_workbook, "Trust_NISRA",x = '=HYPERLINK(#Table_26!A4","Table_26")',startCol = 1, startRow = 29)
writeFormula(new_workbook, "Trust_NISRA",x = '=HYPERLINK(#Table_27!A10","Table_27")',startCol = 1, startRow = 30)
writeFormula(new_workbook, "Trust_NISRA",x = '=HYPERLINK(#Table_31!A5","Table_31")',startCol = 1, startRow = 31)
writeFormula(new_workbook, "Trust_Civil_Service",x = '=HYPERLINK(#Table_32!A10","Table_32")',startCol = 1, startRow = 33)
writeFormula(new_workbook, "Trust_Civil_Service",x = '=HYPERLINK(#Table_36!A16","Table_36")',startCol = 1, startRow = 35)
writeFormula(new_workbook, "Trust_Civil_Service",x = '=HYPERLINK(#Table_40!A23","Table_40")',startCol = 1, startRow = 37)
writeFormula(new_workbook, "Trust_NISRA_Statistics",x = '=HYPERLINK(#Table_44!A4","Table_44")',startCol = 1, startRow = 39)
writeFormula(new_workbook, "Trust_NISRA_Statistics",x = '=HYPERLINK(#Table_45!A10","Table_45")',startCol = 1, startRow = 40)
writeFormula(new_workbook, "Trust_NISRA_Statistics",x = '=HYPERLINK(#Table_49!A16","Table_49")',startCol = 1, startRow = 41)
writeFormula(new_workbook, "Value",x = '=HYPERLINK(#Table_50!A4","Table_50")',startCol = 1, startRow = 43)
writeFormula(new_workbook, "Value",x = '=HYPERLINK(#Table_51!A10","Table_51")',startCol = 1, startRow = 44)
writeFormula(new_workbook, "Value",x = '=HYPERLINK(#Table_55!A16","Table_55")',startCol = 1, startRow = 45)
writeFormula(new_workbook, "Political_Interference",x = '=HYPERLINK(#Table_56!A4","Table_56")',startCol = 1, startRow = 47)
writeFormula(new_workbook, "Political_Interference",x = '=HYPERLINK(#Table_57!A10","Table_57")',startCol = 1, startRow = 48)
writeFormula(new_workbook, "Confidentiality",x = '=HYPERLINK(#Table_61!A4","Table_61")',startCol = 1, startRow = 50)
writeFormula(new_workbook, "Confidentiality",x = '=HYPERLINK(#Table_62!A10","Table_62")',startCol = 1, startRow = 51)

# style headings etc
header1 <- createStyle(textDecoration = "Bold", fontSize = 12)
header2 <- createStyle(textDecoration = "Bold", fontSize = 10)
header3 <- createStyle(textDecoration = "Bold", fontSize = 11)
header4 <- createStyle(textDecoration = "Bold", fontSize = 11, fontColour = "blue")

addStyle(new_workbook, sheet = "Introduction",
         style = header1, rows = 1, cols = 1)

addStyle(new_workbook, sheet = "Contents",
         style = header1, rows = 1, cols = 1)

addStyle(new_workbook, sheet = "Introduction",
         style = header2, rows = c(3, 5, 7, 8, 11, 13, 15, 17), cols = 1)

addStyle(new_workbook, sheet = "Contents",
         style = header2, rows = c(2, 3, 6, 17, 28, 32, 34, 36, 38, 42, 46, 49), cols = 1)

addStyle(new_workbook, sheet = "Awareness_of_NISRA",
         style = header2, rows = 1, cols = 1)

addStyle(new_workbook, sheet = "Awareness_of_NISRA",
         style = header3, rows = c(3:8, 10:15), cols = 1)

table_1_data_ncols <- ncol(table_1_data) - 1

addStyle(new_workbook, sheet = "Awareness_of_NISRA",
         style = header3, rows = c(4), cols = c(2:table_1_data_ncols))

addStyle(new_workbook, sheet = "Awareness_of_NISRA",
         style = header3, rows = c(11), cols = c(2, 3))

addStyle(new_workbook, sheet = "Awareness_NISRA_Statistics",
         style = header4, rows = c(1), cols = c(1))

addStyle(new_workbook, sheet = "Awareness_NISRA_Statistics",
         style = header3, rows = c(2), cols = c(1))

addStyle(new_workbook, sheet = "Awareness_NISRA_Statistics",
         style = header3, rows = c(3:70), cols = c(1))

addStyle(new_workbook, sheet = "Awareness_NISRA_Statistics",
         style = header3, rows = c(5, 11, 17, 23, 29, 35, 41, 47, 53, 59), cols = c(2))

addStyle(new_workbook, sheet = "Aware_Statistics_by_NISRA",
         style = header4, rows = c(1), cols = c(1))

addStyle(new_workbook, sheet = "Aware_Statistics_by_NISRA",
         style = header2, rows = c(4:70), cols = c(1))

addStyle(new_workbook, sheet = "Aware_Statistics_by_NISRA",
         style = header2, rows = c(5, 11, 17, 23, 29, 35, 41, 47, 53, 59), cols = c(2))

addStyle(new_workbook, sheet = "Trust_NISRA",
         style = header2, rows = c(1), cols = c(1))

addStyle(new_workbook, sheet = "Trust_NISRA",
         style = header4, rows = c(3:14), cols = c(1))

table_26_data_ncols <- ncol(table_26_data) - 1

addStyle(new_workbook, sheet = "Trust_NISRA",
         style = header4, rows = c(4), cols = c(2:table_26_data_ncols))

addStyle(new_workbook, sheet = "Trust_NISRA",
         style = header2, rows = c(10), cols = c(2, 3))

table_26_data_ncols <- ncol(table_26_data) - 1

addStyle(new_workbook, sheet = "Trust_NISRA",
         style = header2, rows = c(4), cols = c(2, table_26_data_ncols))

addStyle(new_workbook, sheet = "Trust_Civil_Service",
         style = header3, rows = c(1), cols = c(1))

addStyle(new_workbook, sheet = "Trust_Civil_Service",
         style = header2, rows = c(3:8), cols = c(1))

table_32_data_ncols <- ncol(table_32_data) - 1

addStyle(new_workbook, sheet = "Trust_Civil_Service",
         style = header3, rows = c(4), cols = c(2:table_32_data_ncols))

addStyle(new_workbook, sheet = "Trust_NI_Assembly",
         style = header3, rows = c(1), cols = c(1))

addStyle(new_workbook, sheet = "Trust_NI_Assembly",
          style = header2, rows = c(3:8), cols = c(1))

table_36_data_ncols <- ncol(table_36_data) - 1

addStyle(new_workbook, sheet = "Trust_NI_Assembly",
         style = header2, rows = c(4), cols = c(2:table_36_data_ncols))

addStyle(new_workbook, sheet = "Trust_Media",
         style = header3, rows = c(1), cols = c(1))

addStyle(new_workbook, sheet = "Trust_Media",
         style = header3, rows = c(1), cols = c(1))

addStyle(new_workbook, sheet = "Trust_Media",
         style = header2, rows = c(2:7), cols = c(1))

table_40_data_ncols <- ncol(table_40_data) - 1

addStyle(new_workbook, sheet = "Trust_NI_Assembly",
         style = header2, rows = c(4), cols = c(2:table_40_data_ncols))

addStyle(new_workbook, sheet = "Trust_NISRA_Statistics",
         style = header3, rows = c(1), cols = c(1))

addStyle(new_workbook, sheet = "Trust_NISRA_Statistics",
         style = header2, rows = c(3:20), cols = c(1))

table_44_data_ncols <- ncol(table_44_data) - 1

addStyle(new_workbook, sheet = "Trust_NISRA_Statistics",
         style = header2, rows = c(4), cols = c(2:table_44_data_ncols))

addStyle(new_workbook, sheet = "Trust_NISRA_Statistics",
         style = header2, rows = c(10), cols = c(2, 3))

addStyle(new_workbook, sheet = "Trust_NISRA_Statistics",
         style = header2, rows = c(16), cols = c(2, 3))

addStyle(new_workbook, sheet = "Value",
         style = header3, rows = c(1), cols = c(1))

addStyle(new_workbook, sheet = "Value",
         style = header2, rows = c(16), cols = c(3:20))

table_50_data_ncols <- ncol(table_50_data) - 1

addStyle(new_workbook, sheet = "Value",
         style = header2, rows = c(4), cols = c(2:table_50_data))

addStyle(new_workbook, sheet = "Value",
         style = header2, rows = c(10), cols = c(2, 3))

addStyle(new_workbook, sheet = "Value",
         style = header2, rows = c(16), cols = c(2, 3))

addStyle(new_workbook, sheet = "Political_Interference",
         style = header3, rows = c(1), cols = c(1))

addStyle(new_workbook, sheet = "Political_Interference",
         style = header2, rows = c(3:14), cols = c(1))

table_56_data_ncols <- ncol(table_56_data) - 1

addStyle(new_workbook, sheet = "Political_Interference",
         style = header2, rows = c(4), cols = c(2:table_56_data_ncols))

addStyle(new_workbook, sheet = "Confidentiality",
         style = header3, rows = c(1), cols = c(1))

addStyle(new_workbook, sheet = "Confidentiality",
         style = header2, rows = c(3:14), cols = c(1))

table_61_data_ncols <- ncol(table_61_data) - 1

addStyle(new_workbook, sheet = "Confidentiality",
         style = header2, rows = c(4), cols = c(2:table_61_data_ncols))

addStyle(new_workbook, sheet = "Confidentiality",
         style = header2, rows = c(10), cols = c(2, 3))

#### Save workbook ####
# naming output file
output_file <- paste0("excel_tables.xlsx")
saveWorkbook(new_workbook, "excel_output_tables.xlsx", overwrite = TRUE)
