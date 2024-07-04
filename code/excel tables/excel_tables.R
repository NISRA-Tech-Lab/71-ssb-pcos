# Placeholder for Excel table
library(here)

# Creating spreadsheets
source(paste0(here(),"/code/excel tables/data_prep_for_excel.R"))

bold15 <- createStyle(textDecoration = "bold",
                      fontSize = 11)

# Rounding
df.list <- list(table_1_data,  table_10_data, table_11_data, table_12_data, table_13_data,
                table_14_data, table_15_data, table_16_data, table_17_data, table_18_data,
                table_19_data, table_2_data, table_20_data, table_21_data, table_22_data,
                table_23_data, table_24_data, table_25_data, table_26_data, table_27_data,
                table_28_data, table_29_data, table_3_data,  table_30_data, table_31_data,
                table_32_data, table_33_data, table_34_data, table_35_data, table_36_data,
                table_37_data, table_4_data,  table_5_data,  table_6_data,  table_7_data,
                table_8_data,  table_9_data)

text <- read_excel("C:/Users/2352605/Documents/71-ssb-pcos/code/excel tables/Text.xlsx", sheet = "Text")
num_rows <- nrow(text)

#### create new workbook ####
# update meta data (creator and title)
new_workbook <- createWorkbook(creator = "Dept name",
                               title = "Add theme here")

# set basic formatting
modifyBaseFont(new_workbook, fontSize = 10, fontName = "Arial")

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

# Add tables
tables_dataframe <- data.frame(Table_Name = c("table_1_data", "table_2_data", "table_3_data", "table_4_data",  "table_5_data",
                                              "table_6_data", "table_7_data", "table_8_data", "table_9_data", "table_10_data", 
                                              "table_11_data", "table_12_data", "table_13_data", "table_14_data", "table_15_data",
                                              "table_16_data", "table_17_data", "table_18_data", "table_19_data", "table_20_data", 
                                              "table_21_data", "table_22_data", "table_23_data", "table_24_data", "table_25_data",
                                              "table_26_data", "table_27_data", "table_28_data", "table_29_data", "table_30_data", 
                                              "table_31_data", "table_32_data", "table_33_data", "table_34_data", "table_35_data",
                                              "table_36_data", "table_37_data"),
                               table_name_col = c("Table 1: Awareness of NISRA, 2009-2022",
                                                  "Table 2: Awareness of NISRA (2022) and ONS (2021)",
                                                  "Table 3: Aware of NISRA statistics on the number of deaths in Northern Ireland, 2022",
                                                  "Table 4: Aware of NISRA statistics on recorded levels of crime in Northern Ireland, 2022",
                                                  "Table 5: Aware of NISRA statistics on the qualifications of school leavers in Northern Ireland, 2022",
                                                  "Table 6: Aware of NISRA statistics on the number of people who live in Northern Ireland, 2022",
                                                  "Table 7: Aware of NISRA statistics on hospital waiting times in Northern Ireland, 2022",
                                                  "Table 8: Aware of NISRA statistics on the Northern Ireland Census every ten years, 2022",
                                                  "Table 9: Aware of NISRA statistics on the unemployment rate in Northern Ireland, 2022",
                                                  "Table 10: Aware of NISRA statistics on people living in poverty in Northern Ireland, 2022",
                                                  "Table 11: Aware of NISRA statistics on percentage of journeys made by walking, cycling or public transport in Northern Ireland, 2022",
                                                  "Table 12: Number of selected NISRA statistics respondents had heard of (among those who were not previously aware of NISRA), 2022",
                                                  "Table 13: Aware that statistics on the number of deaths in Northern Ireland are produced by NISRA statisticians, 2022",
                                                  "Table 14: Aware that statistics on recorded levels of crime in Northern Ireland are produced by NISRA statisticians, 2022",
                                                  "Table 15: Aware that statistics on the qualifications of school leavers in Northern Ireland are produced by NISRA statisticians, 2022",
                                                  "Table 16: Aware that statistics on the number of people who live in Northern Ireland are produced by NISRA statisticians, 2022",
                                                  "Table 17: Aware that statistics on hospital waiting times in Northern Ireland are produced by NISRA statisticians, 2022",
                                                  "Table 18: Aware that statistics on the Northern Ireland Census every ten years are produced by NISRA statisticians, 2022",
                                                  "Table 19: Aware that statistics on the unemployment rate in Northern Ireland are produced by NISRA statisticians, 2022",
                                                  "Table 20: Aware that statistics on people living in poverty in Northern Ireland are produced by NISRA statisticians, 2022",
                                                  "Table 21: Aware that statistics on the percentage of journeys made by walking, cycling or public transport in Northern Ireland are produced by NISRA statisticians, 2022",
                                                  "Table 22: Number of specified statistics respondents are aware are produced by NISRA statisticians (among those who had previously heard of NISRA), 2022",
                                                  "Table 23: Trust in NISRA, 2014-2022",
                                                  "Table 24: Trust in NISRA (2022) and ONS (2021)",
                                                  "Table 25: Trust in the Civil Service, 2014-2022",
                                                  "Table 26: Trust in the Northern Ireland Assembly, 2014-2022",
                                                  "Table 27: Trust in the Media, 2014-2022",
                                                  "Table 28: Trust in NISRA Statistics, 2014-2022",
                                                  "Table 29: Trust in NISRA (2022) and ONS (2021) statistics",
                                                  "Table 30: Trust in NISRA statistics by respondent's awareness of NISRA, 2022",
                                                  "Table 31: Statistics produced by NISRA are important to understand Northern Ireland, 2016-2022",
                                                  "Table 32: Statistics produced are important to understand our country (NISRA 2022 and ONS 2021) ",
                                                  "Table 33: Statistics produced by NISRA are important to understand Northern Ireland, by whether or not the respondent had heard of NISRA, 2022",
                                                  "Table 34: Statistics produced by NISRA are free from political interference, 2014-2022",
                                                  "Table 35: Statistics produced are free from political interference (NISRA 2022 and ONS 2021) ",
                                                  "Table 36: Personal information provided to NISRA will be kept confidential, 2014-2022",
                                                  "Table 37: Personal information provided will be kept confidential (NISRA 2022 and ONS 2021)"),
                               Sheet = c("Awareness_of_NISRA", "Awareness_of_NISRA",
                                         "Awareness_NISRA_Statistics", "Awareness_NISRA_Statistics", "Awareness_NISRA_Statistics", 
                                         "Awareness_NISRA_Statistics",
                                         "Awareness_NISRA_Statistics", "Awareness_NISRA_Statistics", "Awareness_NISRA_Statistics",
                                         "Awareness_NISRA_Statistics", "Awareness_NISRA_Statistics", "Awareness_NISRA_Statistics",
                                         "Aware_Statistics_by_NISRA", "Aware_Statistics_by_NISRA", "Aware_Statistics_by_NISRA",
                                         "Aware_Statistics_by_NISRA", "Aware_Statistics_by_NISRA", "Aware_Statistics_by_NISRA",
                                         "Aware_Statistics_by_NISRA", "Aware_Statistics_by_NISRA", "Aware_Statistics_by_NISRA",
                                         "Aware_Statistics_by_NISRA", "Trust_NISRA",
                                         "Trust_NISRA", "Trust_Civil_Service", "Trust_NI_Assembly", "Trust_Media",
                                         "Trust_NISRA_Statistics", "Trust_NISRA_Statistics", "Trust_NISRA_Statistics",
                                         "Value", "Value", "Value", "Political_Interference", "Political_Interference",
                                         "Confidentiality", "Confidentiality"),
                               Start_Row = c(4, 11, 5, 11, 17, 23, 29, 35, 41, 47, 53, 59, 5, 11, 17, 23, 29, 35, 41,
                                             47, 53, 59, 4, 10, 5, 4, 3, 4, 10, 16, 4, 10, 16, 4, 10, 4, 10))

for (i in 1:nrow(tables_dataframe)) {
  df <- tables_dataframe[i,]

  writeDataTable(new_workbook, 
                 sheet = paste0(df$Sheet),
                 x = get(df$Table_Name),
                 startRow = df$Start_Row,
                 startCol = 1,
                 colNames = TRUE,
                 tableStyle = "none",
                 tableName = df$Table_Name,
                 withFilter = FALSE,
                 bandedRows = FALSE,
                 headerStyle = pt)
  
  writeData(new_workbook,
            sheet = paste0(df$Sheet),
            x = df$table_name_col,
            startRow = df$Start_Row-1,
            colNames = TRUE)
}

worksheet_name_col <- c("Awareness_of_NISRA", "Awareness_of_NISRA",
          "Awareness_NISRA_Statistics", "Awareness_NISRA_Statistics", "Awareness_NISRA_Statistics", 
          "Awareness_NISRA_Statistics",
          "Awareness_NISRA_Statistics", "Awareness_NISRA_Statistics", "Awareness_NISRA_Statistics",
          "Awareness_NISRA_Statistics", "Awareness_NISRA_Statistics", "Awareness_NISRA_Statistics",
          "Aware_Statistics_by_NISRA", "Aware_Statistics_by_NISRA", "Aware_Statistics_by_NISRA",
          "Aware_Statistics_by_NISRA", "Aware_Statistics_by_NISRA", "Aware_Statistics_by_NISRA",
          "Aware_Statistics_by_NISRA", "Aware_Statistics_by_NISRA", "Aware_Statistics_by_NISRA",
          "Aware_Statistics_by_NISRA", "Trust_NISRA",
          "Trust_NISRA", "Trust_Civil_Service", "Trust_NI_Assembly", "Trust_Media",
          "Trust_NISRA_Statistics", "Trust_NISRA_Statistics", "Trust_NISRA_Statistics",
          "Value", "Value", "Value", "Political_Interference", "Political_Interference",
          "Confidentiality", "Confidentiality")

Start_Row = c(4, 11, 5, 11, 17, 23, 29, 35, 41, 47, 53, 59, 5, 11, 17, 23, 29, 35, 41,
              47, 53, 59, 4, 10, 5, 4, 3, 4, 10, 16, 4, 10, 16, 4, 10, 4, 10)

# Contents Page
worksheet_name_df <- data.frame(worksheet_name = worksheet_name_col,
                                row_number = 2:38, 
                                Start_Row, 
                                tables_dataframe$table_name_col)

contents_row_number <- nrow(worksheet_name_df)

for (i in 1:contents_row_number) {
  
  df <- worksheet_name_df[i,]

  writeFormula(new_workbook, "Contents",
               startRow = df$row_number,
               x = makeHyperlinkString(
                 sheet = paste0(df$worksheet_name), row = df$Start_Row, col = 1,
                 text = paste0(df$table_name_col)
               )
  )

}

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

addStyle(new_workbook, sheet = "Awareness_NISRA_Statistics",
         style = header4, rows = c(1), cols = c(1))

addStyle(new_workbook, sheet = "Awareness_NISRA_Statistics",
         style = header3, rows = c(2), cols = c(1))

addStyle(new_workbook, sheet = "Awareness_NISRA_Statistics",
         style = header3, rows = c(3:70), cols = c(1))

addStyle(new_workbook, sheet = "Aware_Statistics_by_NISRA",
         style = header4, rows = c(1), cols = c(1))

addStyle(new_workbook, sheet = "Aware_Statistics_by_NISRA",
         style = header2, rows = c(4:70), cols = c(1))

addStyle(new_workbook, sheet = "Trust_NISRA",
         style = header2, rows = c(1), cols = c(1))

addStyle(new_workbook, sheet = "Trust_NISRA",
         style = header2, rows = c(3:14), cols = c(1))

addStyle(new_workbook, sheet = "Trust_Civil_Service",
         style = header3, rows = c(1), cols = c(1))

addStyle(new_workbook, sheet = "Trust_Civil_Service",
         style = header2, rows = c(3:8), cols = c(1))

addStyle(new_workbook, sheet = "Trust_NI_Assembly",
         style = header3, rows = c(1), cols = c(1))

addStyle(new_workbook, sheet = "Trust_NI_Assembly",
          style = header2, rows = c(3:8), cols = c(1))

addStyle(new_workbook, sheet = "Trust_Media",
         style = header3, rows = c(1), cols = c(1))

addStyle(new_workbook, sheet = "Trust_Media",
         style = header3, rows = c(1), cols = c(1))

addStyle(new_workbook, sheet = "Trust_Media",
         style = header2, rows = c(2:7), cols = c(1))

addStyle(new_workbook, sheet = "Trust_NISRA_Statistics",
         style = header3, rows = c(1), cols = c(1))

addStyle(new_workbook, sheet = "Trust_NISRA_Statistics",
         style = header2, rows = c(3:20), cols = c(1))

addStyle(new_workbook, sheet = "Value",
         style = header3, rows = c(1), cols = c(1))

addStyle(new_workbook, sheet = "Political_Interference",
         style = header3, rows = c(1), cols = c(1))

addStyle(new_workbook, sheet = "Political_Interference",
         style = header2, rows = c(3:14), cols = c(1))

addStyle(new_workbook, sheet = "Confidentiality",
         style = header3, rows = c(1), cols = c(1))

addStyle(new_workbook, sheet = "Confidentiality",
         style = header2, rows = c(3:14), cols = c(1))

# Column and row widths
setColWidths(new_workbook, sheet = "Introduction", cols = c(1), widths = c(109))
setColWidths(new_workbook, sheet = "Contents", cols = c(1), widths = c(139))
table_1_cols <- ncol(table_1_data) - 1  
setColWidths(new_workbook, sheet = "Awareness_of_NISRA", cols = c(1), widths = c(46))
setColWidths(new_workbook, sheet = "Awareness_of_NISRA", cols = c(2:table_1_cols), widths = c(22))
setColWidths(new_workbook, sheet = "Awareness_NISRA_Statistics", cols = c(1, 2), widths = c(46, 22))
setColWidths(new_workbook, sheet = "Aware_Statistics_by_NISRA", cols = c(1, 2), widths = c(46, 22))
table_23_cols <- ncol(table_23_data) - 1  
setColWidths(new_workbook, sheet = "Trust_NISRA", cols = c(1), widths = c(46))
setColWidths(new_workbook, sheet = "Trust_NISRA", cols = c(2:table_23_cols), widths = c(22))
table_25_cols <- ncol(table_25_data) - 1  
setColWidths(new_workbook, sheet = "Trust_Civil_Service", cols = c(1), widths = c(46))
setColWidths(new_workbook, sheet = "Trust_Civil_Service", cols = c(2:table_25_cols), widths = c(22))
table_26_cols <- ncol(table_26_data) - 1  
setColWidths(new_workbook, sheet = "Trust_NI_Assembly", cols = c(1), widths = c(46))
setColWidths(new_workbook, sheet = "Trust_NI_Assembly", cols = c(2:table_26_cols), widths = c(22))
table_27_cols <- ncol(table_27_data) - 1  
setColWidths(new_workbook, sheet = "Trust_Media", cols = c(1), widths = c(46))
setColWidths(new_workbook, sheet = "Trust_Media", cols = c(2:table_27_cols), widths = c(22))
table_28_cols <- ncol(table_28_data) - 1  
setColWidths(new_workbook, sheet = "Trust_NISRA_Statistics", cols = c(1), widths = c(46))
setColWidths(new_workbook, sheet = "Trust_NISRA_Statistics", cols = c(2:table_28_cols), widths = c(22))
table_31_cols <- ncol(table_31_data) - 1  
setColWidths(new_workbook, sheet = "Value", cols = c(1), widths = c(46))
setColWidths(new_workbook, sheet = "Value", cols = c(2:table_31_cols), widths = c(22))
table_34_cols <- ncol(table_34_data) - 1  
setColWidths(new_workbook, sheet = "Political_Interference", cols = c(1), widths = c(46))
setColWidths(new_workbook, sheet = "Political_Interference", cols = c(2:table_34_cols), widths = c(22))
table_36_cols <- ncol(table_36_data) - 1  
setColWidths(new_workbook, sheet = "Confidentiality", cols = c(1), widths = c(46))
setColWidths(new_workbook, sheet = "Confidentiality", cols = c(2:table_36_cols), widths = c(22))

#### Save workbook ####
# naming output file
saveWorkbook(new_workbook, "code/excel tables/excel_output_tables.xlsx", overwrite = TRUE)

