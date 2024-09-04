library(here)

# R Scripts containing bespoke functions for significance testing
source(paste0(here(), "/code/config.R"))

## INPUTS ####
vars <- read.csv(paste0(here(), "/code/significance_testing/exploratory_analysis/inputs/var.csv"))
groupings <- read.csv(paste0(here(), "/code/significance_testing/exploratory_analysis/inputs/grouping.csv"))

time_series_vars <- readxl::read_xlsx(paste0(here(), "/code/significance_testing/exploratory_analysis/inputs/var read across - time series.XLSX"),
                                      sheet = "var read across ")

analysis_year <- 2022
comparison_year <- 2021

## DATA ####
data_last <- readRDS(paste0(data_folder, "Final/PCOS ", comparison_year, " Final Dataset.RDS"))
data_current <- readRDS(paste0(data_folder, "Final/PCOS ", analysis_year, " Final Dataset.RDS"))

for (i in 1:nrow(vars)) {
  data_last[[vars$data_last[i]]] <- fct_collapse(data_last[[vars$data_last[i]]],
    "yes" = c(
      "Yes",
      "Trust a great deal",
      "Tend to trust",
      "Tend to trust them",
      "Trust them greatly",
      "Strongly agree",
      "Tend to agree",
      "Trust a great deal/Tend to trust",
      "Strongly Agree/Tend to Agree"
    ),
    "no" = c(
      "No",
      "Tend to distrust",
      "Distrust greatly",
      "Tend not to trust them",
      "Distrust them greatly",
      "Tend to disagree",
      "Strongly disagree",
      "Tend to distrust/Distrust greatly",
      "Tend to disagree/Strongly disagree"
    ),
    "dont_know" = c(
      "DontKnow",
      "Don't know"
    )
  )

  data_current[[vars$data_current[i]]] <- fct_collapse(data_current[[vars$data_current[i]]],
    "yes" = c(
      "Yes",
      "Trust a great deal",
      "Tend to trust",
      "Tend to trust them",
      "Trust them greatly",
      "Strongly agree",
      "Tend to agree",
      "Trust a great deal/Tend to trust",
      "Strongly Agree/Tend to Agree"
    ),
    "no" = c(
      "No",
      "Tend to distrust",
      "Distrust greatly",
      "Tend not to trust them",
      "Distrust them greatly",
      "Tend to disagree",
      "Strongly disagree",
      "Tend to distrust/Distrust greatly",
      "Tend to disagree/Strongly disagree"
    ),
    "dont_know" = c(
      "DontKnow",
      "Don't know"
    )
  )
}



### **** CURRENT YEAR **** ####
currentYear <- "data_current"

# Re-labelling some fields
data_current$SEX <- recode(data_current$SEX, "M" = "Male", "F" = "Female")

# Remove values from PCOS1c1, c2 etc. if not Yes for PCOS1

for (i in 1:9) {
  data_current[[paste0("PCOS1c", i)]][data_current$PCOS1 == "No"] <- NA
  data_current[[paste0("PCOS1d", i)]][data_current$PCOS1 == "Yes"] <- NA

  data_last[[paste0("PCOS1c", i)]][data_last$PCOS1 == "No"] <- NA
  data_last[[paste0("PCOS1d", i)]][data_last$PCOS1 == "Yes"] <- NA
}

# Data excluding don't knows
data_current_excl_dk <- data_current
data_current_excl_dk[] <- lapply(data_current_excl_dk, function(x) gsub("dont_know", NA, x))

data_last[] <- lapply(data_last, function(x) gsub("Don't know", "dont_know", x))
data_last_excl_dk <- data_last
data_last_excl_dk[] <- lapply(data_last_excl_dk, function(x) gsub("dont_know", NA, x))

### **** CODE **** ####
# Build dataframe for testing from vars and groupings inputs
co_vars <- unique(groupings$group1)

grouping_order <- c()

for (i in 1:length(co_vars)) {
  grouping_order <- c(grouping_order, levels(data_current[[co_vars[i]]]))
  
  if (co_vars[i] == "AGE2") {
    grouping_order <- c(grouping_order, "All")
  }
}

grouping_order <- base::setdiff(grouping_order, c("Refusal", "DontKnow"))

a <- c("SEX", "SEX", "AGE2", "AGE2", "AGE2", "AGE2", "AGE2", "AGE2", "AGE2", 
       "All", "DERHIanalysis", "DERHIanalysis", "DERHIanalysis", "DERHIanalysis", 
       "DERHIanalysis", "DERHIanalysis", "EMPST2", "EMPST2")

b <- data.frame()
for (i in 1:nrow(vars)) {
  data_last_question <- c(rep(vars[i, "data_last"], 18))
  data_last_question <- as.data.frame(data_last_question)
  b <- rbind(b, data_last_question)
}

c <- data.frame()
for (i in 1:nrow(vars)) {
  data_current_question <- c(rep(vars[i, "data_current"], 18))
  data_current_question <- as.data.frame(data_current_question)
  c <- rbind(c, data_current_question)
}

previous_year_comp_df <- cbind(grouping_order, a, b, c)

vardf <- stCombinations(vars, groupings, currentYear)
vardf <- vardf[!duplicated(vardf), ]

# flatten file tvardf# flatten file to extract all variable info needed for significance testing
stVars <- rbindlist(split.default(as.data.table(vardf), c(0, sequence(ncol(vardf) - 1) %/% 4)), use.names = FALSE)
stVars <- unique(stVars) # remove duplicated entries
stVars <- stVars[rep(seq_len(nrow(stVars)), each = 3), ]
number_rows_stvars <- nrow(stVars) / 3
answer <- c("yes", "no", "dont_know")
stVars$Answer <- rep(answer, number_rows_stvars)

# Extract question text
question <- apply(stVars, 1, function(y) extractQuestionText(y["year1"], y["var1"]))
stVars$question <- "A"
stVars <- cbind(stVars, question) # add question text into stVars dataframe

# apply varAnalysis to extract the p and n values for each case in stVars
pnlist <- apply(stVars, 1, function(y) varAnalysis(y["year1"], y["group1"], y["grouping1"], y["var1"], y["Answer"]))
allVars <- cbind(stVars, t(pnlist)) # add p and n values to stVars and rename to allVars
colnames(allVars) <- c("year", "var", "group", "grouping", "Answer", "question", "n", "p") # rename columns for next stage

stVars_excl_dk <- stVars %>%
  filter(Answer != "dont_know")

stVars_excl_dk$year1 <- sub("data_current", "data_current_excl_dk", stVars_excl_dk$year1)
stVars_excl_dk$year1 <- sub("data_last", "data_last_excl_dk", stVars_excl_dk$year1)

pnlist_excl_dk <- apply(stVars_excl_dk, 1, function(y) varAnalysis(y["year1"], y["group1"], y["grouping1"], y["var1"], y["Answer"]))
allVars_excl_dk <- cbind(stVars_excl_dk, t(pnlist_excl_dk)) # add p and n values to stVars and rename to allVars
colnames(allVars_excl_dk) <- c("year", "var", "group", "grouping", "Answer", "question", "n", "p") # rename columns for next stage

# Join the p, n and question text data to the relevant variables and groupings in vardf
vardf <- stCombinations(vars, groupings, currentYear)
vardf <- vardf[!duplicated(vardf), ]

vardf <- vardf[rep(seq_len(nrow(vardf)), each = 3), ]
number_rows_vardf <- nrow(vardf) / 3
answer <- c("yes", "no", "dont_know")
vardf$Answer <- rep(answer, number_rows_vardf)

vardf_excl_dk <- vardf %>%
  filter(Answer != "dont_know")

vardf <- vardf %>% left_join(allVars, by = c(
  "var1" = "var", "group1" = "group",
  "grouping1" = "grouping", "year1" = "year",
  "Answer" = "Answer"
))

colnames(vardf) <- c(
  "year1", "var1", "group1", "grouping1", "year2", "var2", "group2",
  "grouping2", "Answer", "question1", "n", "p"
)

vardf <- vardf %>%
  left_join(allVars, by = c(
    "var2" = "var", "group2" = "group",
    "grouping2" = "grouping", "year2" = "year",
    "Answer" = "Answer"
  ))

colnames(vardf) <- c(
  "year1", "var1", "group1", "grouping1", "year2", "var2", "group2",
  "grouping2", "answer", "question1",
  "n1", "p1", "question2", "n2", "p2"
)

vardf$p1[is.na(vardf$p1)] <- 0
vardf$p2[is.na(vardf$p2)] <- 0

vardf_excl_dk$year1 <- sub("data_current", "data_current_excl_dk", vardf_excl_dk$year1)
vardf_excl_dk$year1 <- sub("data_last", "data_last_excl_dk", vardf_excl_dk$year1)
vardf_excl_dk$year2 <- sub("data_current", "data_current_excl_dk", vardf_excl_dk$year2)
vardf_excl_dk$year2 <- sub("data_last", "data_last_excl_dk", vardf_excl_dk$year2)

vardf_excl_dk <- vardf_excl_dk %>% left_join(allVars_excl_dk, by = c(
  "var1" = "var", "group1" = "group",
  "grouping1" = "grouping", "year1" = "year",
  "Answer" = "Answer"
)) #
colnames(vardf_excl_dk) <- c(
  "year1", "var1", "group1", "grouping1", "year2", "var2", "group2",
  "grouping2", "Answer", "question1", "n", "p"
)
vardf_excl_dk <- vardf_excl_dk %>% left_join(allVars_excl_dk, by = c(
  "var2" = "var", "group2" = "group",
  "grouping2" = "grouping", "year2" = "year",
  "Answer" = "Answer"
))
colnames(vardf_excl_dk) <- c(
  "year1", "var1", "group1", "grouping1", "year2", "var2", "group2",
  "grouping2", "answer", "question1",
  "n1", "p1", "question2", "n2", "p2"
)
vardf_excl_dk$p1[is.na(vardf_excl_dk$p1)] <- 0
vardf_excl_dk$p2[is.na(vardf_excl_dk$p2)] <- 0

# apply function SignificanceTest to extract significance and directional info
for (i in 1:nrow(vardf)) {
  x <- significanceTest(vardf[i, "p1"], vardf[i, "n1"], vardf[i, "p2"], vardf[i, "n2"])
  vardf[i, "significance"] <- x["significance"]
  vardf[i, "direction"] <- x["direction"]
  vardf[i, "score"] <- x["score"]
}

for (i in 1:nrow(vardf_excl_dk)) {
  x <- significanceTest(vardf_excl_dk[i, "p1"], vardf_excl_dk[i, "n1"], vardf_excl_dk[i, "p2"], vardf_excl_dk[i, "n2"])
  vardf_excl_dk[i, "significance"] <- x["significance"]
  vardf_excl_dk[i, "direction"] <- x["direction"]
  vardf_excl_dk[i, "score"] <- x["score"]
}

# structure the data in a more friendly way for presenting and order so that significant changes are first
vardf <- vardf %>%
  select(year1, group1, grouping1, var1, question1, n1, p1, year2, group2, grouping2, var2, answer, n2, p2, significance, direction, score) %>%
  arrange(desc(significance))

vardf_excl_dk <- vardf_excl_dk %>%
  select(year1, group1, grouping1, var1, question1, n1, p1, year2, group2, grouping2, var2, answer, n2, p2, significance, direction, score) %>%
  arrange(desc(significance))


wb <- createWorkbook()
modifyBaseFont(wb, fontSize = 12, fontName = "Arial")


excel_df <- vardf %>%
  mutate(
    year1 = case_when(
      year1 == "data_current" ~ analysis_year,
      year1 == "data_last" ~ comparison_year
    ),
    year2 = case_when(
      year2 == "data_current" ~ analysis_year,
      year2 == "data_last" ~ comparison_year
    ),
    score = as.numeric(score)
  ) %>%
  select(year1, grouping1, var1, year2, grouping2, var2, answer, score) %>%
  rename(`Grouping 1` = grouping1, `Grouping 2` = grouping2, `z Score` = score) %>%
  arrange(var1)

excel_df <- excel_df[!duplicated(excel_df), ]

excel_df_excl_dk <- vardf_excl_dk %>%
  mutate(
    year1 = case_when(
      year1 == "data_current_excl_dk" ~ analysis_year,
      year1 == "data_last_excl_dk" ~ comparison_year
    ),
    year2 = case_when(
      year2 == "data_current_excl_dk" ~ analysis_year,
      year2 == "data_last_excl_dk" ~ comparison_year
    ),
    score = as.numeric(score)
  ) %>%
  select(year1, grouping1, var1, year2, grouping2, var2, answer, score) %>%
  rename(`Grouping 1` = grouping1, `Grouping 2` = grouping2, `z Score` = score) %>%
  arrange(var1)

# Sorting columns

excel_df_excl_dk <- excel_df_excl_dk[!duplicated(excel_df_excl_dk), ]


for (i in 1:nrow(vars)) {
  assign(
    paste0(vars$data_current[i], "_df"),
    excel_df %>%
      filter(var1 == vars$data_current[i]) %>%
      select(-var1, -var2) %>%
      mutate(
        `Grouping 1` = factor(`Grouping 1`, levels = c(grouping_order)),
        `Grouping 2` = factor(`Grouping 2`, levels = c(grouping_order))
      ) %>%
      arrange(year2, `Grouping 1`, `Grouping 2`)
  )

  if (vars$data_current[i] == "AwareNISRA2") {
    AwareNISRA2_df <- AwareNISRA2_df %>%
      filter(answer == "yes")
  }

  addWorksheet(wb, vars$data_current[i])

  writeDataTable(wb,
    sheet = vars$data_current[i],
    x = get(paste0(vars$data_current[i], "_df")),
    startRow = 2,
    startCol = 1,
    colNames = TRUE,
    tableStyle = "none",
    tableName = vars$data_current[i],
    withFilter = FALSE,
    bandedRows = FALSE
  )

  for (j in 1:nrow(get(paste0(vars$data_current[i], "_df")))) {
    if (!is.na(get(paste0(vars$data_current[i], "_df"))[j, 6])) {
      if (abs(get(paste0(vars$data_current[i], "_df"))[j, 6]) > qnorm(0.975)) {
        addStyle(wb, vars$data_current[i],
          style = sig,
          rows = 2 + j,
          cols = 6
        )
      } else {
        addStyle(wb, vars$data_current[i],
          style = not_sig,
          rows = 2 + j,
          cols = 6
        )
      }
    }
  }

  if (vars$data_current[i] != "AwareNISRA2") {
    assign(
      paste0(vars$data_current[i], "_df_excl_dk"),
      excel_df_excl_dk %>%
        filter(var1 == vars$data_current[i] & answer != "no") %>%
        select(-var1, -var2) %>%
        mutate(
          `Grouping 1` = factor(`Grouping 1`, levels = c(grouping_order)),
          `Grouping 2` = factor(`Grouping 2`, levels = c(grouping_order))
        ) %>%
        arrange(year2, `Grouping 1`, `Grouping 2`)
    )

    addWorksheet(wb, paste0(vars$data_current[i], "_excl_dk"))

    writeDataTable(wb,
      sheet = paste0(vars$data_current[i], "_excl_dk"),
      x = get(paste0(vars$data_current[i], "_df_excl_dk")),
      startRow = 2,
      startCol = 1,
      colNames = TRUE,
      tableStyle = "none",
      tableName = paste0(vars$data_current[i], "_excl_dk"),
      withFilter = FALSE,
      bandedRows = FALSE
    )

    for (j in 1:nrow(get(paste0(vars$data_current[i], "_df_excl_dk")))) {
      if (!is.na(get(paste0(vars$data_current[i], "_df_excl_dk"))[j, 6])) {
        if (abs(get(paste0(vars$data_current[i], "_df_excl_dk"))[j, 6]) > qnorm(0.975)) {
          addStyle(wb, paste0(vars$data_current[i], "_excl_dk"),
            style = sig,
            rows = 2 + j,
            cols = 6
          )
        } else {
          addStyle(wb, paste0(vars$data_current[i], "_excl_dk"),
            style = not_sig,
            rows = 2 + j,
            cols = 6
          )
        }
      }
    }
  }
}

saveWorkbook(wb,
  paste0(here(), "/outputs/significance outputs/exploratory significance output ", analysis_year, " - with ", comparison_year," comparison.xlsx"),
  overwrite = TRUE
)

openXL(paste0(here(), "/outputs/significance outputs/exploratory significance output ", analysis_year, " - with ", comparison_year," comparison.xlsx"))

