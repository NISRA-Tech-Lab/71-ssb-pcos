library(here)
source(paste0(here(), "/code/html_publication/data_prep.R"))

if (!dir.exists(paste0(here(), "/outputs/infographics"))) {
  dir.create(paste0(here(), "/outputs/infographics"))
}

# Overview ####

# Overview Infographic
## Respondents' awareness of NISRA ####
awareness_of_nisra <- data.frame(
  aware_of_nisra = aware_nisra_data$pct[aware_nisra_data$year == current_year],
  not_aware_of_nisra = 100 - aware_nisra_data$pct[aware_nisra_data$year == current_year]
) %>%
  t() %>%
  as.data.frame() %>%
  mutate(
    Answer = c("Yes", "No"),
    Percentage = round_half_up(V1),
    year = current_year
  ) %>%
  select(-V1)

donut_chart_df <- awareness_of_nisra %>%
  mutate(
    label = toupper(paste0(Percentage, "% ", Answer)),
    ymax = cumsum(Percentage),
    ymin = c(0, head(ymax, n = -1))
  )

## Trust in NISRA statistics ####
trust_nisra_stats <- trust_stats_data %>%
  tail(5) %>%
  mutate(year = as.numeric(year)) %>%
  set_names(c(
    "Year",
    "Tend to trust/trust a great deal",
    "Tend to distrust/distrust a great deal",
    "Don't know"
  ))

trust_df <- gather(trust_nisra_stats, Category, Percentage, -Year) %>%
  mutate(
    Percentage = round_half_up(Percentage),
    Year = as.character(Year)
  )

# Confidentiality
confidentiality <- confidential_data %>%
  tail(5) %>%
  mutate(year = as.numeric(year)) %>%
  set_names(c(
    "Year",
    "Strongly Agree/Tend to agree",
    "Tend to disagree/Strongly disagree",
    "Don't know"
  )) %>%
  gather(Category, Percentage, -Year) %>%
  mutate(
    Percentage = round_half_up(Percentage),
    Year = as.character(Year),
    Category = factor(Category,
      levels = c(
        "Don't know",
        "Tend to disagree/Strongly disagree",
        "Strongly Agree/Tend to agree"
      )
    )
  )


## Personal Information provided to NISRA will be kept confidential ####
importance <- stats_important_data %>%
  tail(5) %>%
  mutate(year = as.numeric(year)) %>%
  set_names(c(
    "Year",
    "Strongly agree/tend to agree",
    "Tend to disagree/strongly disagree",
    "Don't know"
  ))
important_df <- gather(importance, Category, Percentage, -Year)
important_df$Percentage <- round_half_up(important_df$Percentage)
important_df$Year <- as.character(important_df$Year)

## NISRA compared to other institutions ####
institutions <- trust_institutions_data
institutions <- institutions %>%
  set_names(c(
    "Institution",
    "Tend to trust/trust a great deal",
    "Tend to distrust/distrust a great deal",
    "Don't know"
  ))
institutions_df <- gather(institutions, Category, Percentage, -Institution)
institutions_df$Percentage <- round_half_up(institutions_df$Percentage)
trust_compared_df <- institutions_df

# Trust Infographic ####
## Chart 1 ####

new_trust_names <- c("Org", "Yes", "No", "Don't know")

trust_info_data1 <- trust_stats_nisra_ons_data[trust_stats_nisra_ons_data$org %like% "NISRA", ] %>%
  set_names(new_trust_names) %>%
  gather(class, prop, -Org) %>%
  mutate(
    prop = round_half_up(prop),
    label = paste0(prop, "% ", toupper(class))
  )

## Chart 2 ####

new_info_names <- c("Category", "Year", "Percentage\n")

trust_info_data2 <- readRDS(paste0(data_folder, "Trend/", current_year, "/table_4a_data.RDS")) %>%
  filter(`Response (%)` == "Tend to trust/trust a great deal") %>%
  gather(Year, Percentage, -`Response (%)`) %>%
  mutate(
    Percentage = round_half_up(Percentage),
    Year = as.numeric(Year),
    `Response (%)` = "Trust in NISRA Statistics"
  ) %>%
  set_names(new_info_names)

line_chart_df <- trust_info_data2

## Chart 3 ####
trust_info_data3 <- readRDS(paste0(data_folder, "Trend/", current_year, "/table_6a_data.RDS"))
trust_info_data3 <- trust_info_data3[trust_info_data3$`Response (%)` %like% "Strongly agree/Tend to agree", ]
trust_info_data3 <- gather(trust_info_data3, Year, Percentage, -`Response (%)`)
trust_info_data3$Percentage <- round_half_up(trust_info_data3$Percentage)
new_info_names <- c("Category", "Year", "Percentage")
trust_info_data3 <- trust_info_data3 %>%
  set_names(new_info_names)
trust_info_data3$Year <- as.character(trust_info_data3$Year)
line_chart_df <- trust_info_data3

# Chart 4

trust_info_data4 <- readRDS(paste0(data_folder, "Trend/", current_year, "/table_4d_data.RDS")) %>%
  filter(Response == "Yes") %>%
  t() %>%
  as.data.frame() %>%
  filter(V1 != "Yes" & rownames(.) != "2018") %>%
  mutate(
    Organisation = "ONS",
    Year = rownames(.),
    Percentage = round_half_up(as.numeric(V1))
  ) %>%
  select(-V1) %>%
  bind_rows(trust_info_data2 %>%
    as.data.frame() %>%
    filter(!Year %in% 2019:2020) %>%
    mutate(
      Organisation = "NISRA",
      Year = as.character((Year))
    ) %>%
    select(Organisation, Year, Percentage = `Percentage\n`)) %>%
  arrange(Organisation)

rownames(trust_info_data4) <- 1:nrow(trust_info_data4)

if (current_year != ons_year) {
  trust_info_data4 <- trust_info_data4 %>%
    bind_rows(data.frame(
      Organisation = "ONS",
      Year = as.character(current_year),
      Percentage = NA
    ))
}



# Awareness Infographic
# Chart 2

new_info_names <- c("Category", "Year", "Percentage")

awareness_info_data1 <- readRDS(paste0(data_folder, "Trend/", current_year, "/table_1a_data.RDS")) %>%
  filter(`Response (%)` == "Yes") %>%
  gather(Year, Percentage, -`Response (%)`) %>%
  filter(as.numeric(Year) >= 2016) %>%
  mutate(
    Percentage = round_half_up(Percentage),
    Year = as.numeric(Year),
    end = Percentage / 2
  ) %>%
  rename(Category = `Response (%)`) %>%
  mutate(
    rank = rank(-Percentage, ties.method = "first"),
    shape = case_when(
      rank == 1 ~ "blue circle 1",
      rank == 2 ~ "blue circle 2",
      rank == 3 ~ "blue circle 3",
      rank == max(rank) ~ "green circle",
      TRUE ~ "blue circle 4"
    ),
    text_colour = case_when(
      rank <= 3 ~ "#ffffff",
      TRUE ~ "#000000"
    ),
    text_size = case_when(
      rank <= 3 ~ 5,
      TRUE ~ 3.5
    ),
    diameter = Percentage / sum(Percentage)
  )

weighted_trend <- readRDS(paste0(data_folder, "Trend/", current_year, "/weighted trend data.RDS"))

aware_nisra_trend <- f_trend("Awareness")
aware_nisra_trend[[1]] <- gsub("% ", "", aware_nisra_trend[[1]])

aware_nisra_z <- f_trend_z_scores(aware_nisra_trend, "Yes") %>%
  filter(.[[1]] == current_year) %>%
  t() %>%
  as.data.frame() %>%
  filter(.[[1]] != current_year) %>%
  mutate(
    Year = as.numeric(rownames(.)),
    Z = as.numeric(.[[1]])
  ) %>%
  select(Year, Z) %>%
  mutate(significance = case_when(
    Z < qnorm(0.975) * -1 ~ "significantly lower",
    Z > qnorm(0.975) ~ "significantly higher",
    TRUE ~ "not significantly different"
  ))



# Chart 3
awareness_info_data2 <- readRDS(paste0(data_folder, "Trend/", current_year, "/aware_nisra_ons_data.RDS"))
awareness_info_data2$year <- as.character(awareness_info_data2$year)
awareness_info_data2 <- subset(awareness_info_data2, awareness_info_data2$year == "2016" |
  awareness_info_data2$year == "2018" |
  awareness_info_data2$year == "2021" |
  awareness_info_data2$year == "2022")
awareness_info_data2 <- gather(awareness_info_data2, Group, Percentage, -`year`)
awareness_info_data2$Percentage <- round_half_up(awareness_info_data2$Percentage)
awareness_info_data2$Group <- toupper(awareness_info_data2$Group)

# Chart 4
awareness_info_data3 <- gather(aware_stats_data, Answer, Percentage, -`output`) %>%
  rename(Group = output) %>%
  mutate(
    Group = case_when(
      grepl("Percentage of journeys", Group) ~ "Journeys by walking, cycling, public transport",
      TRUE ~ Group
    ),
    Percentage = round_half_up(Percentage, 1),
    Answer = factor(Answer,
      levels = c("dont_know", "no", "yes"),
      labels = c("Don't Know", "No", "Yes")
    )
  )
