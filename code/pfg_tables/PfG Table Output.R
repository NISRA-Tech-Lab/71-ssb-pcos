library(here)
source(paste0(here(), "/code/config.R"))
source(paste0(here(), "/code/pfg_tables/Historic Data to R.R"))
wb <- createWorkbook()

# Define all available years based on current_year value from config ####
data_years <- c(seq(2012, 2018, 2), 2019:current_year)

# Questions to analyse ####
questions <- c("TrustMedia2", "TrustAssemblyElectedBody2")

# Co-variates to include ####
co_vars <- c("AGE2", "SEX", "EMPST2", "DERHIanalysis", "OwnRelig2", "URBH")

# Lookup table for EQUALGROUPS labels (taken from PfG documentation) ####
eq_labels <- read.xlsx(
  xlsxFile = paste0(here(), "/code/pfg_tables/Classifications_Equality Groups - Template.xlsx"),
  sheet = "Classifications_EQ - Template"
)

# Loop through questions ####

for (question in questions) {
  ## Create template of data frame ####
  question_data <- data.frame(STATISTIC = character()) %>%
    mutate(
      `TLIST(A1)` = numeric(),
      EQUALGROUPS = character(),
      `Variable name` = character(),
      `Lower limit` = numeric(),
      VALUE = numeric(),
      `Upper limit` = numeric()
    )

  for (year in data_years) {
    ## Read data from Remote location ####
    data_year <- readRDS(paste0(data_folder, "Final/PCOS ", year, " Final Dataset.RDS"))

    ### assign data frame to global environment ####
    assign(
      paste0("data_", year),
      data_year
    )

    ## Run Analysis only if question is present in data ####

    if (question %in% names(data_year)) {
      ### Which weight variables to use depending on year ####

      # Set Refusals to NA
      data_year[[question]][data_year[[question]] == "Refusal"] <- NA
      
      #### NI level / Most co-vars ####
      ni_weight <- if (year == 2020) {
        "W4"
      } else if (year %in% 2012:2016) {
        "weight"
      } else {
        "W3"
      }

      #### Age co-vars ####
      age_weight <- if (year %in% 2012:2016) {
        "weight"
      } else if (year == 2020) {
        "W1a"
      } else {
        "W1"
      }

      #### Sex co-vars ####
      sex_weight <- if (year %in% 2012:2016) {
        "weight"
      } else {
        "W2"
      }

      ### Calculated weighted value for NI ####

      ni_value <- data_year %>%
        filter(!is.na(.[[question]]) & .[[question]] %in% c("Trust a great deal/Tend to trust", "Tend to trust/trust a great deal")) %>%
        pull(ni_weight) %>%
        sum() / data_year %>%
          filter(!is.na(.[[question]])) %>%
          pull(ni_weight) %>%
          sum() * 100

      ### Unweighted n for NI for year ####

      ni_n <- data_year %>%
        filter(!is.na(.[[question]])) %>%
        nrow()

      ### Confidence intervals for NI for year ####

      ni_ci <- f_confidence_interval(
        p = ni_value / 100,
        n = ni_n
      )

      ### Write NI row for year as data frame ####

      ni_data <- data.frame(STATISTIC = character(1)) %>%
        mutate(
          `TLIST(A1)` = year,
          EQUALGROUPS = "N92000002",
          `Variable name` = "Northern Ireland",
          `Lower limit` = ni_ci[["lower_cl"]] * 100,
          VALUE = ni_value,
          `Upper limit` = ni_ci[["upper_cl"]] * 100
        )

      ### Append this row to question_data data frame ####

      question_data <- question_data %>%
        bind_rows(ni_data)

      ### Loop through co-variates ####

      for (var in co_vars) {
        if (var %in% names(data_year)) {
          if (grepl("AGE", var)) {
            #### Reword value labels for age groups ####
            levels(data_year[[var]]) <- gsub(" ", "", levels(data_year[[var]]), fixed = TRUE) %>%
              gsub("andover", "+", .) %>%
              paste("Age", .)

            weight <- age_weight
          } else if (grepl("SEX", var)) {
            data_year <- data_year %>%
              mutate(SEX = factor(SEX,
                levels = levels(SEX),
                labels = c("Sex - Male", "Sex - Female", "Refusal", "Don't Know")
              )) %>%
              filter(SEX %in% c("Sex - Male", "Sex - Female")) %>%
              mutate(SEX = factor(SEX,
                levels = c("Sex - Male", "Sex - Female")
              ))

            weight <- sex_weight
          } else {
            weight <- ni_weight
          }

          if (var == "EMPST2") {
            new_levels <- levels(data_year$EMPST2)[!levels(data_year$EMPST2) %in% c("Refusal", "DontKnow")]

            new_labels <- paste("Employment Status -", new_levels)

            data_year <- data_year %>%
              filter(!EMPST2 %in% c("Refusal", "DontKnow")) %>%
              mutate(EMPST2 = factor(EMPST2,
                levels = new_levels,
                labels = new_labels
              ))
          }

          if (var == "DERHIanalysis") {
            new_levels <- levels(data_year$DERHIanalysis)[!levels(data_year$DERHIanalysis) %in% c("Other qualifications", "Refusal", "DontKnow")]

            new_labels <- paste("Highest Qualification -", new_levels)

            data_year <- data_year %>%
              filter(!DERHIanalysis %in% c("Other qualifications", "Refusal", "DontKnow")) %>%
              mutate(DERHIanalysis = factor(DERHIanalysis,
                levels = new_levels,
                labels = new_labels
              ))
          }

          if (var == "OwnRelig2") {
            new_levels <- levels(data_year$OwnRelig2)[!levels(data_year$OwnRelig2) %in% c("Refusal", "Dont know")]

            new_labels <- paste("Religion -", new_levels) %>%
              gsub("No Religion", "no religion", .)

            data_year <- data_year %>%
              filter(!OwnRelig2 %in% c("Refusal", "Dont know")) %>%
              mutate(OwnRelig2 = factor(OwnRelig2,
                levels = new_levels,
                labels = new_labels
              ))
          }

          if (var == "URBH") {
            data_year <- data_year %>%
              mutate(URBH = factor(URBH,
                levels = c("URBAN", "RURAL"),
                labels = c("Urban Rural - Urban", "Urban Rural - Rural")
              ))
          }

          #### Values to loop through ####

          co_vals <- levels(data_year[[var]])

          for (co_val in co_vals) {
            ##### Weighted p ####
            p_weighted <- data_year %>%
              filter(!is.na(.[[question]]) & .[[var]] == co_val & .[[question]] %in% c("Trust a great deal/Tend to trust", "Tend to trust/trust a great deal")) %>%
              pull(weight) %>%
              sum() / data_year %>%
                filter(!is.na(.[[question]]) & .[[var]] == co_val) %>%
                pull(weight) %>%
                sum() * 100


            ##### Unweighted n #####
            n_value <- data_year %>%
              filter(!is.na(.[[question]]) & .[[var]] == co_val) %>%
              nrow()

            ##### Confidence interval ####
            ci <- f_confidence_interval(
              p = p_weighted / 100,
              n = n_value
            )

            ##### EQUALGROUPS lookup ####

            EQUALGROUP <- if (co_val %in% eq_labels$VALUE) {
              eq_labels$CODE[eq_labels$VALUE == co_val]
            } else {
              ""
            }

            new_row <- data.frame(STATISTIC = character(1)) %>%
              mutate(
                `TLIST(A1)` = year,
                EQUALGROUPS = EQUALGROUP,
                `Variable name` = co_val,
                `Lower limit` = ci[["lower_cl"]] * 100,
                VALUE = p_weighted,
                `Upper limit` = ci[["upper_cl"]] * 100
              )

            question_data <- question_data %>%
              bind_rows(new_row)
          }
        }
      }
    }
  }

  ## Sort final data frame ####

  sort_order <- c("Northern Ireland")
  
  for (var in co_vars) {
    
    sort_order <- c(sort_order, levels(data_year[[var]]))
    
  }

  question_data <- question_data %>%
    mutate(`Variable name` = factor(`Variable name`,
      levels = sort_order
    )) %>%
    arrange(`Variable name`, `TLIST(A1)`)
  
  question_data_rounded <- question_data %>%
    mutate(`Lower limit` = round_half_up(`Lower limit`, 1),
           VALUE = round_half_up(VALUE, 1),
           `Upper limit` = round_half_up(`Upper limit`, 1))


  ## Write to Excel ####

  addWorksheet(wb, question, tabColour = "#00205B")

  writeDataTable(wb, question,
    x = question_data_rounded,
    tableStyle = "none",
    withFilter = FALSE
  )

  addStyle(wb, question,
    style = ns_pfg,
    rows = 2:(nrow(question_data_rounded) + 1),
    cols = 5:7,
    gridExpand = TRUE
  )

  setColWidths(wb, question,
    cols = 1:7,
    widths = c(22.86, 14.14, 12.86, 52.43, 10.14, 10.14, 10.71)
  )
  
  unrounded_sheet <- paste(substr(question, 1, 19), "(UNROUNDED)")
  
  addWorksheet(wb, unrounded_sheet, tabColour = "#3878C5")
  
  writeDataTable(wb, unrounded_sheet,
                 x = question_data,
                 tableStyle = "none",
                 withFilter = FALSE
  )
  
  addStyle(wb, unrounded_sheet,
           style = ns_pfg,
           rows = 2:(nrow(question_data) + 1),
           cols = 5:7,
           gridExpand = TRUE
  )
  
  setColWidths(wb, unrounded_sheet,
               cols = 1:7,
               widths = c(22.86, 14.14, 12.86, 52.43, 10.14, 10.14, 10.71)
  )
}

xl_filename <- paste0(here(), "/outputs/PfG - Equality Groups Data ", current_year, ".xlsx")

saveWorkbook(wb, xl_filename, overwrite = TRUE)

openXL(xl_filename)
