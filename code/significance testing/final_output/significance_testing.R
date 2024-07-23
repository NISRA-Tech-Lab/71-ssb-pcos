library(here)
# source(paste0(here(), "/code/config.R"))
# 
# data_last <- readRDS(paste0(data_folder, "Final/PCOS 2021 Final Dataset.RDS"))
# data_current <- readRDS(paste0(data_folder, "Final/PCOS 2022 Final Dataset.RDS"))

# Awareness of NISRA ####

## Current Year vs Previous Year ####

awareness_year <- f_significance_year("PCOS1", "Yes")

## In Work vs Not in work ####

work_status <- data.frame(stat = c("%", "Base"),
                          
                          work = c(f_return_p_group(data_current$PCOS1, "Yes", data_current$EMPST2, "In paid employment"),
                                   f_return_n_group(data_current$PCOS1, data_current$EMPST2, "In paid employment")),
                          
                          not = c(f_return_p_group(data_current$PCOS1, "Yes", data_current$EMPST2, "Not in paid employment"),
                                  f_return_n_group(data_current$PCOS1, data_current$EMPST2, "Not in paid employment")),
                          
                          z = c(f_return_z(p1 = f_return_p_group(data_current$PCOS1, "Yes", data_current$EMPST2, "In paid employment"),
                                           n1 = f_return_n_group(data_current$PCOS1, data_current$EMPST2, "In paid employment"),
                                           p2 = f_return_p_group(data_current$PCOS1, "Yes", data_current$EMPST2, "Not in paid employment"),
                                           n2 = f_return_n_group(data_current$PCOS1, data_current$EMPST2, "Not in paid employment")),
                                NA))

names(work_status) <- c(" ", "In work", "Not in work", "Z Score")

## Age groups ####

age_groups <- levels(data_current$AGE2)

age_stats <- data.frame(stat = c("%", "Base"))

for (age in age_groups) {
  age_stats[[age]] <- c(f_return_p_group(data_current$PCOS1, "Yes", data_current$AGE2, age),
                        f_return_n_group(data_current$PCOS1, data_current$AGE2, age))
}

names(age_stats)[names(age_stats) == "stat"] <- ""

age_z_scores <- f_age_z_scores("PCOS1", "Yes")

## Qualifications ####

quals <- levels(data_current$DERHIanalysis)[!levels(data_current$DERHIanalysis) %in% c("Refusal", "DontKnow", "Other qualifications")]

qual_stats <- data.frame(stat = c("%", "Base"))

for (qual in quals) {
  qual_stats[[qual]] <- c(f_return_p_group(data_current$PCOS1, "Yes", data_current$DERHIanalysis, qual),
                          f_return_n_group(data_current$PCOS1, data_current$DERHIanalysis, qual))
}

qual_z_scores <- f_qual_z_scores("PCOS1", "Yes")

# Products ####

products <- c("Number of deaths in NI",
              "Recorded levels of crime in NI",
              "Qualifications of school leavers in NI",
              "The number of people who live in NI",
              "Statistics on hospital waiting times in NI",
              "The NI Census every ten years",
              "The unemployment rate in NI",
              "People living in poverty in NI",
              "Percentage of journey made by walking, cycling or public transport in NI")

## Heard of NISRA vs Had not heard of NISRA ####
 
products_stats <- data.frame(product = products)

for (i in 1:length(products)) {
  products_stats$heard[i] <- f_return_p(data_current[[paste0("PCOS1c", i)]], "Yes")
  products_stats$not[i] <- f_return_p(data_current[[paste0("PCOS1d", i)]], "Yes")
  products_stats$z[i] <- f_return_z(p1 = f_return_p(data_current[[paste0("PCOS1c", i)]], "Yes"),
                                    n1 = f_return_n(data_current[[paste0("PCOS1c", i)]]),
                                    p2 = f_return_p(data_current[[paste0("PCOS1d", i)]], "Yes"),
                                    n2 = f_return_n(data_current[[paste0("PCOS1d", i)]]))
}

products_stats <- products_stats %>%
  mutate(diff = heard - not) %>%
  rbind(data.frame(product = "Base",
                   heard = f_return_n(data_current$PCOS1c1),
                   not = f_return_n(data_current$PCOS1d1),
                   z = NA,
                   diff = NA))

names(products_stats) <- c(" ", "% Had heard of NISRA", "% Had not heard of NISRA", "Z", "Difference in %")

## Had heard of NISRA: This year vs previous year ####

heard_stats <- data.frame(product = products)

for (i in 1:length(products)) {
  heard_stats$last[i] <- f_return_p(data_last[[paste0("PCOS1c", i)]], "Yes")
  heard_stats$current[i] <- f_return_p(data_current[[paste0("PCOS1c", i)]], "Yes")
  heard_stats$z[i] <- f_return_z(p1 = f_return_p(data_last[[paste0("PCOS1c", i)]], "Yes"),
                                 n1 = f_return_n(data_last[[paste0("PCOS1c", i)]]),
                                 p2 = f_return_p(data_current[[paste0("PCOS1c", i)]], "Yes"),
                                 n2 = f_return_n(data_current[[paste0("PCOS1c", i)]]))
}

heard_stats <- heard_stats %>%
  mutate(diff = current - last) %>%
  rbind(data.frame(product = "Base",
                   last = f_return_n(data_last$PCOS1c1),
                   current = f_return_n(data_current$PCOS1c1),
                   z = NA,
                   diff = NA))

names(heard_stats) <- c("Had heard of NISRA", current_year - 1, current_year, "Z", "Difference in %")

## Had not heard of NISRA: This year vs previous year ####
 
not_heard_stats <- data.frame(product = products)

for (i in 1:length(products)) {
  not_heard_stats$last[i] <- f_return_p(data_last[[paste0("PCOS1d", i)]], "Yes")
  not_heard_stats$current[i] <- f_return_p(data_current[[paste0("PCOS1d", i)]], "Yes")
  not_heard_stats$z[i] <- f_return_z(p1 = f_return_p(data_last[[paste0("PCOS1d", i)]], "Yes"),
                                     n1 = f_return_n(data_last[[paste0("PCOS1d", i)]]),
                                     p2 = f_return_p(data_current[[paste0("PCOS1d", i)]], "Yes"),
                                     n2 = f_return_n(data_current[[paste0("PCOS1d", i)]]))
}

not_heard_stats <- not_heard_stats %>%
  mutate(diff = current - last) %>%
  rbind(data.frame(product = "Base",
                   last = f_return_n(data_last$PCOS1d1),
                   current = f_return_n(data_current$PCOS1d1),
                   z = NA,
                   diff = NA))

names(not_heard_stats) <- c("Had not heard of NISRA", current_year - 1, current_year, "Z", "Difference in %")

# Trust in NISRA ####

## Trust in NISRA: This year vs previous year ####

trust_year <- f_significance_year("TrustNISRA2", "Trust a great deal/Tend to trust")

## Distrust in NISRA: This year vs previous year ####

distrust_year <- f_significance_year("TrustNISRA2", "Tend to distrust/Distrust greatly")

## Don't Know trust in NISRA: This year vs previous year ####

dont_know_trust <- f_significance_year("TrustNISRA2", "Don't know")

## Trust in NISRA - unweighted and base figures by age group ####

trust_age_stats <- data.frame(stat = c("% Yes", "% No", "% DK", "Base"))

for (age in age_groups) {
  trust_age_stats[[age]] <- c(f_return_p_group(data_current$TrustNISRA2, "Trust a great deal/Tend to trust", data_current$AGE2, age) * 100,
                              f_return_p_group(data_current$TrustNISRA2, "Tend to distrust/Distrust greatly", data_current$AGE2, age) * 100,
                              f_return_p_group(data_current$TrustNISRA2, "Don't know", data_current$AGE2, age) * 100,
                              f_return_n_group(data_current$TrustNISRA2, data_current$AGE2, age))
}

names(trust_age_stats)[names(trust_age_stats) == "stat"] <- ""

## Trust in NISRA by Age group compare ####

trust_age_z_scores <- f_age_z_scores("TrustNISRA2", "Trust a great deal/Tend to trust")

## Distrust in NISRA by Age group compare ####

distrust_age_z_scores <- f_age_z_scores("TrustNISRA2", "Tend to distrust/Distrust greatly")

## Don't know trust in NISRA by Age group compare ####

dont_know_trust_age_z_scores <- f_age_z_scores("TrustNISRA2", "Don't know")

## Trust in NISRA - unweighted and base figures by qualification ####

trust_qual_stats <- data.frame(stat = c("% Yes", "% No", "% DK", "Base"))

for (qual in quals) {
  trust_qual_stats[[qual]] <- c(f_return_p_group(data_current$TrustNISRA2, "Trust a great deal/Tend to trust", data_current$DERHIanalysis, qual) * 100,
                               f_return_p_group(data_current$TrustNISRA2, "Tend to distrust/Distrust greatly", data_current$DERHIanalysis, qual) * 100,
                               f_return_p_group(data_current$TrustNISRA2, "Don't know", data_current$DERHIanalysis, qual) * 100,
                               f_return_n_group(data_current$TrustNISRA2, data_current$DERHIanalysis, qual))
}

names(trust_qual_stats)[names(trust_qual_stats) == "stat"] <- ""

## Trust in NISRA by qualification compare ####

trust_qual_z_scores <- f_qual_z_scores("TrustNISRA2", "Trust a great deal/Tend to trust")

## Distrust in NISRA by Age group compare ####

distrust_qual_z_scores <- f_qual_z_scores("TrustNISRA2", "Tend to distrust/Distrust greatly")

## Don't know trust in NISRA by Age group compare ####

dont_know_qual_age_z_scores <- f_qual_z_scores("TrustNISRA2", "Don't know")

## Trust in NISRA by Work Status ####

trust_work_status <- data.frame(stat = c("% Trust", "% Distrust", "% DK", "Base"),
                          
                          work = c(f_return_p_group(data_current$TrustNISRA2, "Trust a great deal/Tend to trust", data_current$EMPST2, "In paid employment") * 100,
                                   f_return_p_group(data_current$TrustNISRA2, "Tend to distrust/Distrust greatly", data_current$EMPST2, "In paid employment") * 100,
                                   f_return_p_group(data_current$TrustNISRA2, "Don't know", data_current$EMPST2, "In paid employment") * 100,
                                   f_return_n_group(data_current$TrustNISRA2, data_current$EMPST2, "In paid employment")),
                          
                          not = c(f_return_p_group(data_current$TrustNISRA2, "Trust a great deal/Tend to trust", data_current$EMPST2, "Not in paid employment") * 100,
                                  f_return_p_group(data_current$TrustNISRA2, "Tend to distrust/Distrust greatly", data_current$EMPST2, "Not in paid employment") * 100,
                                  f_return_p_group(data_current$TrustNISRA2, "Don't know", data_current$EMPST2, "Not in paid employment") * 100,
                                  f_return_n_group(data_current$TrustNISRA2, data_current$EMPST2, "Not in paid employment")),
                          
                          z = c(f_return_z(p1 = f_return_p_group(data_current$TrustNISRA2, "Trust a great deal/Tend to trust", data_current$EMPST2, "In paid employment"),
                                           n1 = f_return_n_group(data_current$TrustNISRA2, data_current$EMPST2, "In paid employment"),
                                           p2 = f_return_p_group(data_current$TrustNISRA2, "Trust a great deal/Tend to trust", data_current$EMPST2, "Not in paid employment"),
                                           n2 = f_return_n_group(data_current$TrustNISRA2, data_current$EMPST2, "Not in paid employment")),
                                f_return_z(p1 = f_return_p_group(data_current$TrustNISRA2, "Tend to distrust/Distrust greatly", data_current$EMPST2, "In paid employment"),
                                           n1 = f_return_n_group(data_current$TrustNISRA2, data_current$EMPST2, "In paid employment"),
                                           p2 = f_return_p_group(data_current$TrustNISRA2, "Tend to distrust/Distrust greatly", data_current$EMPST2, "Not in paid employment"),
                                           n2 = f_return_n_group(data_current$TrustNISRA2, data_current$EMPST2, "Not in paid employment")),
                                f_return_z(p1 = f_return_p_group(data_current$TrustNISRA2, "Don't know", data_current$EMPST2, "In paid employment"),
                                           n1 = f_return_n_group(data_current$TrustNISRA2, data_current$EMPST2, "In paid employment"),
                                           p2 = f_return_p_group(data_current$TrustNISRA2, "Don't know", data_current$EMPST2, "Not in paid employment"),
                                           n2 = f_return_n_group(data_current$TrustNISRA2, data_current$EMPST2, "Not in paid employment")),
                                NA))

names(trust_work_status) <- c(" ", "In work", "Not in work", "Z Score")

# ValueIntConfYears ####

value_year <- f_significance_year("NISRAstatsImp2", "Strongly Agree/Tend to Agree")

interference_year <- f_significance_year("Political2", "Strongly Agree/Tend to Agree")

confidential_year <- f_significance_year("Confidential2", "Strongly Agree/Tend to Agree")

# Value ####



# Interference ####
# Confidentiality ####
# ONS vs NISRA ####
# Trust NISRA Statistics ####
# Institutions Trust this year vs last year ####
