# Read in the data prep script ####
library(here)
source(paste0(here(),"/code/excel tables/data_prep_for_excel.R"))

# Create new workbook ####
wb <- createWorkbook(creator = "NISRA Statistical Support Branch",
                     title = paste("Public Awareness of and Trust in Official Statistics Northern Ireland,", current_year),
                     subject = "Public Awareness and Trust of Statistics",
                     category = "Government")

## Set formatting ####
modifyBaseFont(wb, fontSize = 12, fontName = "Arial")

# Introduction Page ####
addWorksheet(wb, sheetName = "Introduction")

intro_text <- c(paste("Public Awareness of and Trust in Official Statistics, Northern Ireland", current_year, " (Detailed Tables)"),
                "These tables present the responses to the Public Awareness of the Northern Ireland Statistics and Research Agency (NISRA) and Trust in Northern Ireland official statistics questions in the Northern Ireland Continuous Household Survey (CHS). Breakdowns by age group employment status and highest educational qualification are included, as well as comparisons over time and with the Office for National Statistics (ONS), where appropriate. The tables accompany the Public Awareness of and Trust in Official Statistics, Northern Ireland report which includes further explanations and related commentary. A list of tables is provided in the contents sheet.",
                "Linked report (and infographics):",
                "PUBLICATION LINK",
                "Topics covered:",
                "Awareness of NISRA, Awareness of NISRA statistics, Trust in NISRA, Trust in the Civil Service, Trust in the NI Assembly, Trust in the Media, Trust in NISRA statistics, Value of NISRA Statistics, Political Interference, Confidentiality.",
                "Source:",
                paste0("Northern Ireland Continuous Household Survey (CHS): September ", current_year, " to December ", current_year, "."),
                "Published:",
                pub_date_words_dmy,
                "Limitations:",
                paste0("The figures presented in these tables were obtained from a sample of the population (", sample_size, " persons) and are therefore estimates, not precise figures. This means that they have a margin of error. Significance tests can be carried out to determine if the observed differences in the percentages for different response groups are likely to be real differences or due to sampling error."),
                "Weighting:",
                "The percentages in the tables are weighted to adjust for non-response bias. The total number of respondents is unweighted. Further information on the weighting and methodology can be found in Appendix A of the report.",
                "Quality Information:",
                "BQR LINK",
                "Contact Information:",
                "Norma Broomfield\nStatistical Support Branch, NISRA\nColby House\nStranmillis Court\nBelfast, BT9 5RR\n\nTelephone: 028 9038 8481\nE-mail: Norma.Broomfield@nisra.gov.uk")

writeData(wb, "Introduction",
          x = intro_text)

addStyle(wb, "Introduction",
         rows = 1,
         cols = 1,
         style = pt)

addStyle(wb, "Introduction",
         rows = c(2, 6, 12, 14, 18),
         cols = 1,
         style = tw,
         gridExpand = TRUE)

addStyle(wb, "Introduction",
         rows = c(3, 5, 7, 9, 11, 13, 15, 17),
         cols = 1,
         style = pt2,
         gridExpand = TRUE)


pub_link <- paste0("https://www.nisra.gov.uk/publications/public-awareness-and-trust-official-statistics-", current_year)
names(pub_link) <- paste("Public Awareness of and Trust in Official Statistics, Northern Ireland", current_year)
class(pub_link) <- "hyperlink"

writeData(wb, sheet = "Introduction",
          x = pub_link,
          startRow = which(intro_text == "PUBLICATION LINK"),
          colNames = FALSE)

bqr_link <- "https://www.nisra.gov.uk/publications/background-quality-report-public-awareness-and-trust-official-statistics"
names(bqr_link) <- "Background Quality Report: Public Awareness and Trust in Official Statistics"
class(bqr_link) <- "hyperlink"

writeData(wb, sheet = "Introduction",
          x = bqr_link,
          startRow = which(intro_text == "BQR LINK"),
          colNames = FALSE)

setColWidths(wb, "Introduction", widths = 112, cols = 1)


# Contents Page ####

addWorksheet(wb, sheetName = "Contents")


writeData(wb, "Contents",
          x = c(paste("Public Awareness of and Trust in Official Statistics, Northern Ireland", current_year),
                "Table of Contents"))

addStyle(wb, "Contents",
         style = pt,
         rows = 1,
         cols = 1)

addStyle(wb, "Contents",
         style = pt2,
         rows = 2,
         cols = 1)

cr <- 3

setColWidths(wb, "Contents", widths = 114, cols = 1)

# Add rest of worksheets ####

## Awareness of NISRA ####

f_worksheet(wb,
            sheet_name = "Awareness_of_NISRA",
            contents = "Awareness of the Northern Ireland Statistics and Research Agency (NISRA)",
            title = "Q: Before being contacted about this survey had you heard of NISRA, the Northern Ireland Statistics and Research Agency?",
            outlining = "awareness of NISRA.",
            tables = list(
              list(
                data = table_1_data,
                title = paste0("Table 1: Awareness of NISRA, 2009-", current_year),
                note = "Note 1: The percentages for 2009 and 2010 are rounded figures"
              ),
              list(
                data = table_2_data,
                title = paste0("Table 2: Awareness of NISRA (", current_year, ") and ONS (", ons_year, ")")
              )
            )
)

## Awareness of NISRA Statistics ####

f_worksheet(wb,
            sheet_name = "Awareness_NISRA_Statistics",
            contents = "Awareness of NISRA statistics among those who had not previously heard of NISRA",
            title = c("Question asked of those who were not previously aware of NISRA before being contacted about the survey:",
                      "Q: NISRA produces official statistics for Northern Ireland on a wide range of issues. Have you heard of these?"),
            outlining = "awareness of specified statistics produced by NISRA, among those who had not previously heard of NISRA.",
            tables = list(
              list(data = table_3_data,
                   title = paste("Table 3: Aware of NISRA statistics on the number of deaths in Northern Ireland,", current_year)
              ),
              list(data = table_4_data,
                   title = paste("Table 4: Aware of NISRA statistics on recorded levels of crime in Northern Ireland,", current_year)
              ),
              list(data = table_5_data,
                   title = paste("Table 5: Aware of NISRA statistics on the qualifications of school leavers in Northern Ireland,", current_year)
              ),
              list(data = table_6_data,
                   title = paste("Table 6: Aware of NISRA statistics on the number of people who live in Northern Ireland,", current_year)
              ),
              list(data = table_7_data,
                   title = paste("Table 7: Aware of NISRA statistics on hospital waiting times in Northern Ireland,", current_year)
              ),
              list(data = table_8_data,
                   title = paste("Table 8: Aware of NISRA statistics on the Northern Ireland Census every ten years,", current_year)
              ),
              list(data = table_9_data,
                   title = paste("Table 9: Aware of NISRA statistics on the unemployment rate in Northern Ireland,", current_year)
              ),
              list(data = table_10_data,
                   title = paste("Table 10: Aware of NISRA statistics on people living in poverty in Northern Ireland,", current_year)
              ),
              list(data = table_11_data,
                   title = paste("Table 11: Aware of NISRA statistics on percentage of journeys made by walking, cycling or public transport in Northern Ireland,", current_year)
              ),
              list(data = table_12_data,
                   title = paste("Table 12: Number of selected NISRA statistics respondents had heard of (among those who were not previously aware of NISRA),", current_year)
              )
            )
)

## Aware Statistics by NISRA ####

f_worksheet(wb,
            sheet_name = "Aware_Statistics_by_NISRA",
            contents = "Awareness that specified statistics are produced by NISRA statisticians among those who were aware of NISRA",
            title = c("Question asked of those who were aware of NISRA before being contacted about the survey:",
                      "Q: NISRA produces official statistics for Northern Ireland on a wide range of issues. Were you aware that this information was produced by NISRA statisticians?"),
            outlining = "awareness that specified statistics are produced by NISRA statisticians, among those who had heard of NISRA.",
            tables = list(
              list(data = table_13_data,
                   title = paste("Table 13: Aware of NISRA statistics on the number of deaths in Northern Ireland,", current_year)
              ),
              list(data = table_14_data,
                   title = paste("Table 14: Aware of NISRA statistics on recorded levels of crime in Northern Ireland,", current_year)
              ),
              list(data = table_15_data,
                   title = paste("Table 15: Aware of NISRA statistics on the qualifications of school leavers in Northern Ireland,", current_year)
              ),
              list(data = table_16_data,
                   title = paste("Table 16: Aware of NISRA statistics on the number of people who live in Northern Ireland,", current_year)
              ),
              list(data = table_17_data,
                   title = paste("Table 17: Aware of NISRA statistics on hospital waiting times in Northern Ireland,", current_year)
              ),
              list(data = table_18_data,
                   title = paste("Table 18: Aware of NISRA statistics on the Northern Ireland Census every ten years,", current_year)
              ),
              list(data = table_19_data,
                   title = paste("Table 19: Aware of NISRA statistics on the unemployment rate in Northern Ireland,", current_year)
              ),
              list(data = table_20_data,
                   title = paste("Table 20: Aware of NISRA statistics on people living in poverty in Northern Ireland,", current_year)
              ),
              list(data = table_21_data,
                   title = paste("Table 21: Aware of NISRA statistics on percentage of journeys made by walking, cycling or public transport in Northern Ireland,", current_year)
              ),
              list(data = table_22_data,
                   title = paste("Table 22: Number of selected NISRA statistics respondents had heard of (among those who had previously heard of NISRA),", current_year)
              )
            )
)

## Trust in NISRA ####

f_worksheet(wb,
            sheet_name = "Trust_NISRA",
            contents = "Trust in NISRA",
            title = "Q: For each institution, please indicate whether you tend to trust it or tend not to trust it - NISRA?",
            outlining = "trust in NISRA as an institution.",
            tables = list(
              list(
                data = table_23_data,
                title = paste0("Table 23: Trust in NISRA, 2014-", current_year)
              ),
              list(
                data = table_24_data,
                title = paste0("Table 24: Trust in NISRA (", current_year, ") and ONS (", ons_year, ")")
              ),
              list(
                data = table_25_data,
                title = paste0("Table 25: Trust in NISRA by respondent's awareness of NISRA, ", current_year)
              )
            )
)

## Trust in the Civil Service ####

f_worksheet(wb,
            sheet_name = "Trust_Civil_Service",
            contents = "Trust in the Civil Service",
            title = "Q: For each institution, please indicate whether you tend to trust it or tend not to trust it - The Civil Service?",
            outlining = "trust in the Civil Service.",
            tables = list(
              list(
                data = table_26_data,
                title = paste0("Table 26: Trust in the Civil Service, 2014-", current_year)
              )
            )
)

## Trust in the Northern Ireland Assembly ####

f_worksheet(wb,
            sheet_name = "Trust_NI_Assembly",
            contents = "Trust in the Northern Ireland Assembly", 
            title = "Q: For each institution, please indicate whether you tend to trust it or tend not to trust it - The Civil Service?",
            outlining = "trust in the Northern Ireland Assembly",
            tables = list(
              list(
                data = table_27_data,
                title = paste0("Table 27: Trust in the Civil Service, 2014-", current_year, " [Note 2]"),
                note = "Note 2: In 2019, respondents were asked whether they trusted elected bodies, such as the NI Assembly or the UK Government as the NI Assembly was suspended at this time."
              )              
            )
)

## Trust in the Media ####

f_worksheet(wb,
            sheet_name = "Trust_Media",
            contents = "Trust in the Media",
            title = "Q: For each institution, please indicate whether you tend to trust it or tend not to trust it - The Media?",
            outlining = "trust in the media",
            tables = list(
              list(
                data = table_28_data,
                title = paste0("Table 28: Trust in the Media, 2014-", current_year)
              )              
            )
)

## Trust in the statistics produced by NISRA ####

f_worksheet(wb,
            sheet_name = "Trust_NISRA_Statistics",
            contents = "Trust in the statistics produced by NISRA",
            title = "Q: Personally, how much trust do you have in the statistics produced by NISRA?",
            outlining = "trust in the statistics produced by NISRA",
            tables = list(
              list(
                data = table_29_data,
                title = paste0("Table 29: Trust in NISRA statistics, 2014-", current_year)
              ),
              list(
                data = table_30_data,
                title = paste0("Table 30: Trust in NISRA (", current_year, ") and ONS (", ons_year, ")")
              ),
              list(
                data = table_31_data,
                title = paste0("Table 31: Trust in NISRA statistics by respondent's awareness of NISRA, ", current_year)
              )  
            )
)

## Value ####
f_worksheet(wb,
            sheet_name = "Value",
            contents = "Value",
            title = "Q: Statistics produced by NISRA are important to understand Northern Ireland",
            outlining = "the value in NISRA statistics.",
            tables = list(
              list(
                data = table_32_data,
                title = paste0("Table 32: Statistics produced by NISRA are important to understand Northern Ireland, 2016-", current_year)
              ),
              list(
                data = table_33_data,
                title = paste0("Table 33: Statistics produced are important to understand our country (", current_year, ") and ONS (", ons_year, ")")
              ),
              list(
                data = table_34_data,
                title = paste0("Table 34: Statistics produced by NISRA are important to understand Northern Ireland, by whether or not the respondent had heard of NISRA, ", current_year)
              )  
            )
)

## Political Interference ####

f_worksheet(wb,
            sheet_name = "Political_Interference",
            contents = "Political Interference",
            title = "Q: Statistics produced by NISRA are free from political interference",
            outlining = "agreement with the belief that statistics produced by NISRA are free from political interference.",
            tables = list(
              list(
                data = table_35_data,
                title = paste0("Table 35: Statistics produced by NISRA are free from political interference, 2014-", current_year)
              ),
              list(
                data = table_36_data,
                title = paste0("Table 36: Statistics produced are free from political interference (", current_year, ") and ONS (", ons_year, ")")
              ) 
            )
)

## Confidentiality ####

f_worksheet(wb,
            sheet_name = "Confidentiality",
            contents = "Confidentiality",
            title = "Q: I believe that personal information provided to NISRA will be kept confidential",
            outlining = "agreement with the belief that statistics produced by NISRA will be kept confidential.",
            tables = list(
              list(
                data = table_37_data,
                title = paste0("Table 37: Personal information provided to NISRA will be kept confidential, 2014-", current_year)
              ),
              list(
                data = table_38_data,
                title = paste0("Table 38: Personal information provided will be kept confidential (", current_year, ") and ONS (", ons_year, ")")
              ) 
            )
)



# Save workbook ####
saveWorkbook(wb, paste0(here(), "/outputs/Public-Awareness-of-and-Trust-in-Official-Statistics-Northern-Ireland-", current_year, "-tables.xlsx"), overwrite = TRUE)
