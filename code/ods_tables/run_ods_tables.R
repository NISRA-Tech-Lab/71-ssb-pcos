# Read in the data prep script ####
library(here)
source(paste0(here(), "/code/ods_tables/data_prep_for_ods.R"))

# Create new workbook ####
wb <- createWorkbook(
  creator = "NISRA Statistical Support Branch",
  title = paste("Public Awareness of and Trust in Official Statistics Northern Ireland,", current_year),
  subject = "Public Awareness and Trust of Statistics",
  category = "Government"
)

## Set formatting ####
modifyBaseFont(wb, fontSize = 12, fontName = "Arial")

# Introduction Page ####
addWorksheet(wb, sheetName = "Introduction")

intro_text <- c(
  paste("Public Awareness of and Trust in Official Statistics, Northern Ireland", current_year, "(Detailed Tables)"),
  "These tables present the responses to the Public Awareness of the Northern Ireland Statistics and Research Agency (NISRA) and Trust in Northern Ireland official statistics questions in the Northern Ireland Continuous Household Survey (CHS). Comparisons over time and with the Office for National Statistics (ONS) have been included, where appropriate. The tables accompany the Public Awareness of and Trust in Official Statistics, Northern Ireland report which includes further explanations and related commentary. A list of tables is provided in the contents sheet.",
  "Linked report (and infographics):",
  "PUBLICATION LINK",
  "Topics covered:",
  "Awareness of NISRA, Awareness of NISRA statistics, Trust in NISRA, Trust in the Civil Service, Trust in the NI Assembly, Trust in the Media, Trust in NISRA statistics, Value of NISRA Statistics, Political Interference, Confidentiality.",
  "Published:",
  pub_date_words_dmy,
  "Data sources:",
  paste0("2018 to ", current_year, " data: Northern Ireland Continuous Household Survey (CHS)"),
  "2006 to 2016 data: Northern Ireland Omnibus Survey (April 2009, April 2010, June 2012, September 2014, October 2016)",
  paste0("ONS data (2018 to ", ons_year, "): Public Confidence in Official Statistics (PCOS) survey"),
  "ONS data (2014 to 2016): British Social Attitudes survey (BSA)",
  "Limitations:",
  "The figures presented in these tables were obtained from a sample of the population and are therefore estimates, not precise figures. This means that they have a margin of error. Significance tests can be carried out to determine if the observed differences in the percentages for different response groups are likely to be real differences or due to sampling error.",
  "Weighting:",
  "The percentages in the tables are weighted to adjust for non-response bias. The total number of respondents is unweighted. Further information on the weighting and methodology can be found in Appendix A of the report.",
  "Quality Information:",
  "BQR LINK",
  "Contact Information:",
  "Norma Broomfield\nStatistical Support Branch, NISRA\nColby House\nStranmillis Court\nBelfast, BT9 5RR\n\nTelephone: 028 9038 8481\nE-mail: Norma.Broomfield@nisra.gov.uk"
)

writeData(wb, "Introduction",
  x = intro_text
)

addStyle(wb, "Introduction",
  rows = 1,
  cols = 1,
  style = pt
)

addStyle(wb, "Introduction",
  rows = 2:(length(intro_text)),
  cols = 1,
  style = tw,
  gridExpand = TRUE
)

heading_rows <- which(intro_text %in% c(
  "Linked report (and infographics):",
  "Topics covered:",
  "Published:",
  "Data sources:",
  "Limitations:",
  "Weighting:",
  "Quality Information:",
  "Contact Information:"
))

addStyle(wb, "Introduction",
  rows = heading_rows,
  cols = 1,
  style = pt2,
  gridExpand = TRUE
)


pub_link <- paste0("https://www.nisra.gov.uk/publications/public-awareness-and-trust-official-statistics-", current_year)
names(pub_link) <- paste("Public Awareness of and Trust in Official Statistics, Northern Ireland", current_year)
class(pub_link) <- "hyperlink"

writeData(wb,
  sheet = "Introduction",
  x = pub_link,
  startRow = which(intro_text == "PUBLICATION LINK"),
  colNames = FALSE
)

bqr_link <- "https://www.nisra.gov.uk/publications/background-quality-report-public-awareness-and-trust-official-statistics"
names(bqr_link) <- "Background Quality Report: Public Awareness and Trust in Official Statistics"
class(bqr_link) <- "hyperlink"

writeData(wb,
  sheet = "Introduction",
  x = bqr_link,
  startRow = which(intro_text == "BQR LINK"),
  colNames = FALSE
)

setColWidths(wb, "Introduction", widths = 112, cols = 1)
setRowHeights(wb, "Introduction", rows = c(1, heading_rows), heights = 30)
setRowHeights(wb, "Introduction", rows = 2, heights = 60)

# Contents Page ####

addWorksheet(wb, sheetName = "Contents")


writeData(wb, "Contents",
  x = c(
    paste("Public Awareness of and Trust in Official Statistics, Northern Ireland", current_year),
    "Table of Contents"
  )
)

addStyle(wb, "Contents",
  style = pt,
  rows = 1,
  cols = 1
)

addStyle(wb, "Contents",
  style = pt2,
  rows = 2,
  cols = 1
)

setRowHeights(wb, "Contents", rows = 2, heights = 30)

# cr is a row counter for the contents page see f_worksheet()
cr <- 3

setColWidths(wb, "Contents", widths = 114, cols = 1)

# Add rest of worksheets ####

## Awareness of NISRA ####

f_worksheet(wb,
  sheet_name = "Awareness_of_NISRA",
  contents = "Awareness of the Northern Ireland Statistics and Research Agency (NISRA)",
  title = "Qu 1: Before being contacted about this survey had you heard of NISRA, the Northern Ireland Statistics and Research Agency?",
  outlining = "outlining awareness of NISRA.",
  tables = list(
    list(
      data = table_1a_data,
      title = paste0("Table 1a: Awareness of NISRA, 2009 to ", current_year),
      note = "Note 1: The percentages for 2009 and 2010 are rounded figures"
    ),
    list(
      data = table_1b_data,
      title = if (current_year == ons_year) {
        paste0("Table 1b: Awareness of NISRA and ONS, ", current_year)
      } else {
        paste0("Table 1b: Awareness of NISRA (", current_year, ") and ONS (", ons_year, ")")
      }
    ),
    list(
      data = table_1c_data,
      title = paste0("Table 1c: Awareness of ONS, 2014 to ", ons_year)
    )
  )
)

## Awareness of NISRA Statistics ####

f_worksheet(wb,
  sheet_name = "Awareness_NISRA_Statistics",
  contents = "Awareness of NISRA statistics among those who had not previously heard of NISRA",
  title = c(
    "Question asked of those who were not previously aware of NISRA before being contacted about the survey:",
    "Qu 2.1: NISRA produces official statistics for Northern Ireland on a wide range of issues. Have you heard of these?"
  ),
  outlining = "outlining awareness of specified statistics produced by NISRA, among those who had not previously heard of NISRA.",
  tables = list(
    list(
      data = table_2.1a_data,
      title = paste("Table 2.1a: Aware of NISRA statistics on the number of deaths in Northern Ireland,", current_year)
    ),
    list(
      data = table_2.1b_data,
      title = paste("Table 2.1b: Aware of NISRA statistics on recorded levels of crime in Northern Ireland,", current_year)
    ),
    list(
      data = table_2.1c_data,
      title = paste("Table 2.1c: Aware of NISRA statistics on the qualifications of school leavers in Northern Ireland,", current_year)
    ),
    list(
      data = table_2.1d_data,
      title = paste("Table 2.1d: Aware of NISRA statistics on the number of people who live in Northern Ireland,", current_year)
    ),
    list(
      data = table_2.1e_data,
      title = paste("Table 2.1e: Aware of NISRA statistics on hospital waiting times in Northern Ireland,", current_year)
    ),
    list(
      data = table_2.1f_data,
      title = paste("Table 2.1f: Aware of NISRA statistics on the Northern Ireland Census every ten years,", current_year)
    ),
    list(
      data = table_2.1g_data,
      title = paste("Table 2.1g: Aware of NISRA statistics on the unemployment rate in Northern Ireland,", current_year)
    ),
    list(
      data = table_2.1h_data,
      title = paste("Table 2.1h: Aware of NISRA statistics on people living in poverty in Northern Ireland,", current_year)
    ),
    list(
      data = table_2.1i_data,
      title = paste("Table 2.1i: Aware of NISRA statistics on percentage of journeys made by walking, cycling or public transport in Northern Ireland,", current_year)
    ),
    list(
      data = table_2.1j_data,
      title = paste("Table 2.1j: Number of selected NISRA statistics respondents had heard of (among those who were not previously aware of NISRA),", current_year)
    )
  )
)

## Aware Statistics by NISRA ####

f_worksheet(wb,
  sheet_name = "Aware_Statistics_by_NISRA",
  contents = "Awareness that specified statistics are produced by NISRA statisticians among those who were aware of NISRA",
  title = c(
    "Question asked of those who were aware of NISRA before being contacted about the survey:",
    "Qu 2.2: NISRA produces official statistics for Northern Ireland on a wide range of issues. Were you aware that this information was produced by NISRA statisticians?"
  ),
  outlining = "outlining awareness that specified statistics are produced by NISRA statisticians, among those who had heard of NISRA.",
  tables = list(
    list(
      data = table_2.2a_data,
      title = paste("Table 2.2a: Aware that statistics on the number of deaths in Northern Ireland are produced by NISRA statisticians,", current_year)
    ),
    list(
      data = table_2.2b_data,
      title = paste("Table 2.2b: Aware that statistics on recorded levels of crime in Northern Ireland are produced by NISRA statisticians,", current_year)
    ),
    list(
      data = table_2.2c_data,
      title = paste("Table 2.2c: Aware that statistics on the qualifications of school leavers in Northern Ireland are produced by NISRA statisticians,", current_year)
    ),
    list(
      data = table_2.2d_data,
      title = paste("Table 2.2d: Aware that statistics on the number of people who live in Northern Ireland are produced by NISRA statisticians,", current_year)
    ),
    list(
      data = table_2.2e_data,
      title = paste("Table 2.2e: Aware that statistics on hospital waiting times in Northern Ireland are produced by NISRA statisticians,", current_year)
    ),
    list(
      data = table_2.2f_data,
      title = paste("Table 2.2f: Aware that statistics on the Northern Ireland Census every ten years are produced by NISRA statisticians,", current_year)
    ),
    list(
      data = table_2.2g_data,
      title = paste("Table 2.2g: Aware that statistics on the unemployment rate in Northern Ireland are produced by NISRA statisticians,", current_year)
    ),
    list(
      data = table_2.2h_data,
      title = paste("Table 2.2h: Aware that statistics on people living in poverty in Northern Ireland are produced by NISRA statisticians,", current_year)
    ),
    list(
      data = table_2.2i_data,
      title = paste("Table 2.2i: Aware that statistics on the percentage of journeys made by walking, cycling or public transport in Northern Ireland are produced by NISRA statisticians,", current_year)
    ),
    list(
      data = table_2.2j_data,
      title = paste("Table 2.2j: Number of specified statistics respondents are aware are produced by NISRA statisticians (among those who had previously heard of NISRA),", current_year)
    )
  )
)

## Trust in NISRA ####

f_worksheet(wb,
  sheet_name = "Trust_NISRA",
  contents = "Trust in NISRA",
  title = "Qu 3.1: For each institution, please indicate whether you tend to trust it or tend not to trust it - NISRA?",
  outlining = "outlining trust in NISRA as an institution.",
  tables = list(
    list(
      data = table_3.1a_data,
      title = paste0("Table 3.1a: Trust in NISRA, 2014 to ", current_year)
    ),
    list(
      data = table_3.1b_data,
      title = if (current_year == ons_year) {
        paste0("Table 3.1b: Trust in NISRA and ONS, ", current_year)
      } else {
        paste0("Table 3.1b: Trust in NISRA (", current_year, ") and ONS (", ons_year, ")")
      }
    ),
    list(
      data = table_3.1c_data,
      title = paste0("Table 3.1c: Trust in NISRA by respondent's awareness of NISRA, ", current_year)
    )
  )
)

## Trust in the Civil Service ####

f_worksheet(wb,
  sheet_name = "Trust_Civil_Service",
  contents = "Trust in the Civil Service",
  title = "Qu 3.2: For each institution, please indicate whether you tend to trust it or tend not to trust it - The Civil Service?",
  outlining = "outlining trust in the Civil Service.",
  tables = list(
    list(
      data = table_3.2a_data,
      title = paste0("Table 3.2a: Trust in the Civil Service, 2014 to ", current_year)
    )
  )
)

## Trust in the Northern Ireland Assembly ####

# Note: If this is using the TrustElectedRep2 variable the "Note 2" text will automatically be applied to variable label.
#       However, the "note" text below will need updated manually.

f_worksheet(wb,
  sheet_name = "Trust_NI_Assembly",
  contents = "Trust in the Northern Ireland Assembly",
  title = "Qu 3.3: For each institution, please indicate whether you tend to trust it or tend not to trust it - The NI Assembly?",
  outlining = "outlining trust in the Northern Ireland Assembly",
  tables = list(
    list(
      data = table_3.3a_data,
      title = paste0("Table 3.3a: Trust in the Northern Ireland Assembly, 2014 to ", current_year, " [Note 2]"),
      note = "Note 2: In 2019, respondents were asked whether they trusted elected bodies, such as the NI Assembly or the UK Government as the NI Assembly was suspended at this time."
    )
  )
)

## Trust in the Media ####

f_worksheet(wb,
  sheet_name = "Trust_Media",
  contents = "Trust in the Media",
  title = "Qu 3.4: For each institution, please indicate whether you tend to trust it or tend not to trust it - The Media?",
  outlining = "outlining trust in the media",
  tables = list(
    list(
      data = table_3.4a_data,
      title = paste0("Table 3.4a: Trust in the Media, 2014 to ", current_year)
    )
  )
)

## Trust in the statistics produced by NISRA ####

f_worksheet(wb,
  sheet_name = "Trust_NISRA_Statistics",
  contents = "Trust in the statistics produced by NISRA",
  title = "Qu 4: Personally, how much trust do you have in the statistics produced by NISRA?",
  outlining = "outlining trust in the statistics produced by NISRA",
  tables = list(
    list(
      data = table_4a_data,
      title = paste0("Table 4a: Trust in NISRA statistics, 2014 to ", current_year)
    ),
    list(
      data = table_4b_data,
      title = if (current_year == ons_year) {
        paste0("Table 4b: Trust in NISRA statistics and ONS statistics, ", current_year, ")")
      } else {
        paste0("Table 4b: Trust in NISRA statistics (", current_year, ") and ONS statistics (", ons_year, ")")
      }
    ),
    list(
      data = table_4c_data,
      title = paste0("Table 4c: Trust in NISRA statistics by respondent's awareness of NISRA, ", current_year)
    ),
    list(
      data = table_4d_data,
      title = paste0("Table 4d: Trust in ONS statistics, 2014 to ", ons_year)
    )
  )
)

## Value ####

f_worksheet(wb,
  sheet_name = "Value",
  contents = "Value",
  title = "Qu 5: Statistics produced by NISRA are important to understand Northern Ireland",
  outlining = "which concern the value of NISRA statistics.",
  tables = list(
    list(
      data = table_5a_data,
      title = paste0("Table 5a: Statistics produced by NISRA are important to understand Northern Ireland, 2016 to ", current_year)
    ),
    list(
      data = table_5b_data,
      title = if (current_year == ons_year) {
        paste0("Table 5b: Statistics produced are important to understand our country (NISRA and ONS), ", current_year)
      } else {
        paste0("Table 5b: Statistics produced are important to understand our country (NISRA ", current_year, " and ONS ", ons_year, ")")
      }
    ),
    list(
      data = table_5c_data,
      title = paste0("Table 5c: Statistics produced by NISRA are important to understand Northern Ireland, by whether or not the respondent had heard of NISRA, ", current_year)
    )
  )
)

## Political Interference ####

f_worksheet(wb,
  sheet_name = "Political_Interference",
  contents = "Political Interference",
  title = "Qu 6: Statistics produced by NISRA are free from political interference",
  outlining = "which outline agreement with the belief that statistics produced by NISRA are free from political interference.",
  tables = list(
    list(
      data = table_6a_data,
      title = paste0("Table 6a: Statistics produced by NISRA are free from political interference, 2014 to ", current_year)
    ),
    list(
      data = table_6b_data,
      title = if (current_year == ons_year) {
        paste0("Table 6b: Statistics produced are free from political interference (NISRA and ONS) , ", current_year)
      } else {
        paste0("Table 6b: Statistics produced are free from political interference (NISRA ", current_year, " and ONS ", ons_year, ")")
      }
    )
  )
)

## Confidentiality ####

f_worksheet(wb,
  sheet_name = "Confidentiality",
  contents = "Confidentiality",
  title = "Qu 7: I believe that personal information provided to NISRA will be kept confidential",
  outlining = "which concern belief that statistics provided to NISRA will be kept confidential.",
  tables = list(
    list(
      data = table_7a_data,
      title = paste0("Table 7a: Personal information provided to NISRA will be kept confidential, 2014 to ", current_year)
    ),
    list(
      data = table_7b_data,
      title = if (current_year == ons_year) {
        paste0("Table 7b: Personal information provided will be kept confidential (NISRA and ONS), ", current_year)
      } else {
        paste0("Table 7b: Personal information provided will be kept confidential (NISRA ", current_year, " and ONS ", ons_year, ")")
      }
    )
  )
)



# Save workbook ####

xl_filename <- paste0(here(), "/outputs/Public-Awareness-of-and-Trust-in-Official-Statistics-Northern-Ireland-", current_year, "-tables.xlsx")
ods_filename <- sub(".xlsx", ".ods", xl_filename)

saveWorkbook(wb, xl_filename, overwrite = TRUE)
f_convert_to_ods(xl_filename)

ods_filesize <- paste0(
  round_half_up(file.size(ods_filename) / 1000),
  "kB"
)
