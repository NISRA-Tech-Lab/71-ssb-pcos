library(here)
source(paste0(here(), "/code/config.R"))


# Create trend data up to 2021 in R format (once off)
if (!file.exists(paste0(data_folder, "Trend/2021/unweighted trend data.RDS"))) {
  source(paste0(here(), "/code/significance testing/final_output/unweighted_trend.R"))
}

data_last <- readRDS(paste0(data_folder, "Final/PCOS ", current_year - 1, " Final Dataset.RDS"))
data_current <- readRDS(paste0(data_folder, "Final/PCOS ", current_year, " Final Dataset.RDS"))

data_ons_raw <- read.xlsx(paste0(data_folder, "ONS/", ons_filename), sheet = "weighted_pct") %>%
  filter(Year == ons_year)

names(data_ons_raw) <- gsub(".", " ", names(data_ons_raw), fixed = TRUE)

data_ons <- data_ons_raw %>%
  mutate(`Weighted base` = 100 - `Prefer not to answer`,
         across(.cols = `Don't know`:`Strongly disagree`, ~ .x / `Weighted base` * 100)) %>%
  select(-`Prefer not to answer`, -`Weighted base`)

unweighted_ons <- read.xlsx(paste0(data_folder, "ONS/", ons_filename), sheet = "unweighted_n") %>%
  filter(Year == ons_year)

names(unweighted_ons) <- gsub(".", " ", names(unweighted_ons), fixed = TRUE)

unweighted_ons <- unweighted_ons %>%
  mutate(`Unweighted base` = `Unweighted base` - `Prefer not to answer`,
         `Trust a great deal/Tend to trust` = `Trust a great deal` + `Tend to trust`,
         `Tend to distrust/Distrust greatly` = `Tend to distrust` + `Distrust greatly`,
         `Strongly Agree/Tend to Agree` = `Strongly agree` + `Tend to agree`,
         `Tend to disagree/Strongly disagree` = `Tend to disagree` + `Strongly disagree`) %>%
  select(-`Prefer not to answer`)

# Add unweighted trend data to trend file ####

unweighted_old <- readRDS(paste0(data_folder, "Trend/", current_year - 1, "/unweighted trend data.RDS"))

unweighted_new <- data.frame(stat = unweighted_old$stat) %>%
  mutate(new = c(f_return_p(data_current$PCOS1, "Yes") * 100,
                 f_return_n(data_current$PCOS1),
                 f_return_p(data_current$TrustNISRA2, "Trust a great deal/Tend to trust") * 100,
                 f_return_p(data_current$TrustNISRA2, "Tend to distrust/Distrust greatly") * 100,
                 f_return_p(data_current$TrustNISRA2, "Don't know") * 100,
                 f_return_n(data_current$TrustNISRA2),
                 f_return_p(data_current$TrustNISRA2[data_current$TrustNISRA2 != "Don't know"], "Trust a great deal/Tend to trust") * 100,
                 f_return_n(data_current$TrustNISRA2[data_current$TrustNISRA2 != "Don't know"]),
                 f_return_p(data_current$TrustNISRAstats2, "Trust a great deal/Tend to trust") * 100,
                 f_return_p(data_current$TrustNISRAstats2, "Tend to distrust/Distrust greatly") * 100,
                 f_return_p(data_current$TrustNISRAstats2, "Don't know") * 100,
                 f_return_n(data_current$TrustNISRAstats2),
                 f_return_p(data_current$TrustNISRAstats2[data_current$TrustNISRAstats2 != "Don't know"], "Trust a great deal/Tend to trust") * 100,
                 f_return_n(data_current$TrustNISRAstats2[data_current$TrustNISRAstats2 != "Don't know"]),
                 f_return_p(data_current$NISRAstatsImp2, "Strongly Agree/Tend to Agree") * 100,
                 f_return_p(data_current$NISRAstatsImp2, "Tend to disagree/Strongly disagree") * 100,
                 f_return_p(data_current$NISRAstatsImp2, "Don't know") * 100,
                 f_return_n(data_current$NISRAstatsImp2),
                 f_return_p(data_current$NISRAstatsImp2[data_current$NISRAstatsImp2 != "Don't know"], "Strongly Agree/Tend to Agree") * 100,
                 f_return_n(data_current$NISRAstatsImp2[data_current$NISRAstatsImp2 != "Don't know"]),
                 f_return_p(data_current$Political2, "Strongly Agree/Tend to Agree") * 100,
                 f_return_p(data_current$Political2, "Tend to disagree/Strongly disagree") * 100,
                 f_return_p(data_current$Political2, "Don't know") * 100,
                 f_return_n(data_current$Political2),
                 f_return_p(data_current$Political2[data_current$Political2 != "Don't know"], "Strongly Agree/Tend to Agree") * 100,
                 f_return_n(data_current$Political2[data_current$Political2 != "Don't know"]),
                 f_return_p(data_current$Confidential2, "Strongly Agree/Tend to Agree") * 100,
                 f_return_p(data_current$Confidential2, "Tend to disagree/Strongly disagree") * 100,
                 f_return_p(data_current$Confidential2, "Don't know") * 100,
                 f_return_n(data_current$Confidential2),
                 f_return_p(data_current$Confidential2[data_current$Confidential2 != "Don't know"], "Strongly Agree/Tend to Agree") * 100,
                 f_return_n(data_current$Confidential2[data_current$Confidential2 != "Don't know"])
                 ))

names(unweighted_new) <- c("stat", current_year)

unweighted_trend <- left_join(unweighted_new,
                              unweighted_old,
                              by = "stat")

saveRDS(unweighted_trend, paste0(data_folder, "Trend/", current_year, "/unweighted trend data.RDS"))

# This year vs last year with DKs ####

## Awareness of NISRA ####

awareness_year <- f_significance_year("PCOS1", "Yes")

## Trust NISRA #### 

trust_year <- f_significance_year("TrustNISRA2", "Trust a great deal/Tend to trust")

distrust_year <- f_significance_year("TrustNISRA2", "Tend to distrust/Distrust greatly")

dont_know_trust <- f_significance_year("TrustNISRA2", "Don't know")

## Trust in NISRA Statistics ####

trust_stats_year <- f_significance_year("TrustNISRAstats2", "Trust a great deal/Tend to trust")

distrust_stats_year <- f_significance_year("TrustNISRAstats2", "Tend to distrust/Distrust greatly")

dont_know_trust_stats_year <- f_significance_year("TrustNISRAstats2", "Don't know")

## NISRA Stats Importance ####

value_year <- f_significance_year("NISRAstatsImp2", "Strongly Agree/Tend to Agree")

no_value_year <- f_significance_year("NISRAstatsImp2", "Tend to disagree/Strongly disagree")

dont_know_value_year <- f_significance_year("NISRAstatsImp2", "Don't know")

## Interference ####

interference_year <- f_significance_year("Political2", "Strongly Agree/Tend to Agree")

no_interference_year <- f_significance_year("Political2", "Tend to disagree/Strongly disagree")

dont_know_interference_year <- f_significance_year("Political2", "Don't know")

## Confidential ####

confidential_year <- f_significance_year("Confidential2", "Strongly Agree/Tend to Agree")

no_confidential_year <- f_significance_year("Confidential2", "Tend to disagree/Strongly disagree")

dont_know_confidential_year <- f_significance_year("Confidential2", "Don't know")

## Trust in the Civil Service ####
 
trust_nics_year <- f_significance_year("TrustCivilService2", "Trust a great deal/Tend to trust")

distrust_nics_year <- f_significance_year("TrustCivilService2", "Tend to distrust/Distrust greatly")

dont_know_trust_nics <- f_significance_year("TrustCivilService2", "Don't know")

## Trust in the Assembly ####

trust_assembly_year <- f_significance_year("TrustNIAssembly2", "Trust a great deal/Tend to trust")

distrust_assembly_year <- f_significance_year("TrustNIAssembly2", "Tend to distrust/Distrust greatly")

dont_know_trust_assembly <- f_significance_year("TrustNIAssembly2", "Don't know")

## Trust in the Media ####

trust_media_year <- f_significance_year("TrustMedia2", "Trust a great deal/Tend to trust")

distrust_media_year <- f_significance_year("TrustMedia2", "Tend to distrust/Distrust greatly")

dont_know_trust_media <- f_significance_year("TrustMedia2", "Don't know")

# This year vs last year excl DKs ####

## Trust in NISRA ####

trust_year_ex_dk <- f_significance_year("TrustNISRA2", "Trust a great deal/Tend to trust", dk = FALSE) 

## Trust in NISRA Statistics ####

trust_stats_year_ex_dk <- f_significance_year("TrustNISRAstats2", "Trust a great deal/Tend to trust", dk = FALSE)

## NISRA Stats Importance ####

value_year_ex_dk <- f_significance_year("NISRAstatsImp2", "Strongly Agree/Tend to Agree", dk = FALSE)

## Interference ####

interference_year_ex_dk <- f_significance_year("Political2", "Strongly Agree/Tend to Agree", dk = FALSE)

## Confidential ####

confidential_year_ex_dk <- f_significance_year("Confidential2", "Strongly Agree/Tend to Agree", dk = FALSE)

## Trust in the Civil Service ####

trust_nics_year_ex_dk <- f_significance_year("TrustCivilService2", "Trust a great deal/Tend to trust", dk = FALSE)

## Trust in the Assembly ####
 
trust_assembly_year_ex_dk <- f_significance_year("TrustNIAssembly2", "Trust a great deal/Tend to trust", dk = FALSE)

## Trust in the Media ####

trust_media_year_ex_dk <- f_significance_year("TrustMedia2", "Trust a great deal/Tend to trust", dk = FALSE)

# ONS vs NISRA ####

## Heard of NISRA vs heard of ONS ####

heard_nisra_ons <- f_nisra_ons(var = "PCOS1",
                               val_1 = "Yes",
                               val_2 = "No")

## Trust NISRA vs Trust ONS ####

trust_nisra_ons <- f_nisra_ons(var = "TrustNISRA2",
                               val_1 = "Trust a great deal/Tend to trust",
                               val_2 = "Tend to distrust/Distrust greatly")

## Trust NISRA stats vs Trust ONS stats ####

trust_stats_nisra_ons <- f_nisra_ons(var = "TrustNISRAstats2",
                                     val_1 = "Trust a great deal/Tend to trust",
                                     val_2 = "Tend to distrust/Distrust greatly")

## Stats are important: NISRA vs ONS ####

value_nisra_ons <- f_nisra_ons(var = "NISRAstatsImp2",
                               val_1 = "Strongly Agree/Tend to Agree",
                               val_2 = "Tend to disagree/Strongly disagree")

## Stats are free from political interference: NISRA vs ONS ####

interference_nisra_ons <- f_nisra_ons(var = "Political2",
                                      val_1 = "Strongly Agree/Tend to Agree",
                                      val_2 = "Tend to disagree/Strongly disagree")

## Information will be kept confidential: NISRA vs ONS ####

confidential_nisra_ons <- f_nisra_ons(var = "Confidential2",
                                      val_1 = "Strongly Agree/Tend to Agree",
                                      val_2 = "Tend to disagree/Strongly disagree")

# ONSvNISRAexcDKs ####
 
## Heard of NISRA vs heard of ONS (exc DKs) ####

nisra_ons_heard_ex_dk <- f_nisra_ons_ex_dk("PCOS1", "Yes")

## Trust in NISRA vs Trust in ONS (exc DKs) ####

nisra_ons_trust_ex_dk <- f_nisra_ons_ex_dk("TrustNISRA2", "Trust a great deal/Tend to trust")

## Trust in NISRA stats vs Trust in ONS stats (exc DKs) ####

nisra_ons_trust_stats_ex_dk <- f_nisra_ons_ex_dk("TrustNISRAstats2", "Trust a great deal/Tend to trust")

## NISRA stats are important vs ONS stats are important (exc DKs) ####

nisra_ons_important_ex_dk <- f_nisra_ons_ex_dk("NISRAstatsImp2", "Strongly Agree/Tend to Agree")

## NISRA stats are free from political interference vs ONS stats are free from political interference (exc DKs) ####

nisra_ons_political_ex_dk <- f_nisra_ons_ex_dk("Political2", "Strongly Agree/Tend to Agree") 

## NISRA will keep my information confidential vs ONS will keep my information confidential (exc DKs) ####

nisra_ons_confidential_ex_dk <- f_nisra_ons_ex_dk("Confidential2", "Strongly Agree/Tend to Agree") 



# Awareness of NISRA ####


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
  products_stats$heard[i] <- f_return_p(data_current[[paste0("PCOS1c", i)]], "Yes") * 100
  products_stats$not[i] <- f_return_p(data_current[[paste0("PCOS1d", i)]], "Yes") * 100
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
  heard_stats$last[i] <- f_return_p(data_last[[paste0("PCOS1c", i)]], "Yes") * 100
  heard_stats$current[i] <- f_return_p(data_current[[paste0("PCOS1c", i)]], "Yes") * 100
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
  not_heard_stats$last[i] <- f_return_p(data_last[[paste0("PCOS1d", i)]], "Yes") * 100
  not_heard_stats$current[i] <- f_return_p(data_current[[paste0("PCOS1d", i)]], "Yes") * 100
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

## Trust in NISRA - unweighted and base figures by age group ####

trust_age_stats <- f_age_stats("TrustNISRA2", "Trust a great deal/Tend to trust", "Tend to distrust/Distrust greatly")

## Trust in NISRA by Age group compare ####

trust_age_z_scores <- f_age_z_scores("TrustNISRA2", "Trust a great deal/Tend to trust")

## Distrust in NISRA by Age group compare ####

distrust_age_z_scores <- f_age_z_scores("TrustNISRA2", "Tend to distrust/Distrust greatly")

## Don't know trust in NISRA by Age group compare ####

dont_know_trust_age_z_scores <- f_age_z_scores("TrustNISRA2", "Don't know")

## Trust in NISRA - unweighted and base figures by qualification ####

trust_qual_stats <- f_qual_stats("TrustNISRA2", "Trust a great deal/Tend to trust", "Tend to distrust/Distrust greatly")

## Trust in NISRA by qualification compare ####

trust_qual_z_scores <- f_qual_z_scores("TrustNISRA2", "Trust a great deal/Tend to trust")

## Distrust in NISRA by Age group compare ####

distrust_qual_z_scores <- f_qual_z_scores("TrustNISRA2", "Tend to distrust/Distrust greatly")

## Don't know trust in NISRA by Age group compare ####

dont_know_qual_age_z_scores <- f_qual_z_scores("TrustNISRA2", "Don't know")

## Trust in NISRA by Work Status ####

trust_work_status <- f_work_stats("TrustNISRA2", "Trust a great deal/Tend to trust", "Tend to distrust/Distrust greatly")






# Value ####

value_work_stats <- f_work_stats("NISRAstatsImp2", "Strongly Agree/Tend to Agree", "Tend to disagree/Strongly disagree")

value_age_stats <- f_age_stats("NISRAstatsImp2", "Strongly Agree/Tend to Agree", "Tend to disagree/Strongly disagree")

value_age_z_scores <- f_age_z_scores("NISRAstatsImp2", "Strongly Agree/Tend to Agree")

value_disagree_age_z_scores <- f_age_z_scores("NISRAstatsImp2", "Tend to disagree/Strongly disagree")

value_dont_know_age_z_scores <- f_age_z_scores("NISRAstatsImp2", "Don't know")

value_qual_stats <- f_qual_stats("NISRAstatsImp2", "Strongly Agree/Tend to Agree", "Tend to disagree/Strongly disagree")

value_qual_z_scores <- f_qual_z_scores("NISRAstatsImp2", "Strongly Agree/Tend to Agree")

value_disagree_qual_z_scores <- f_qual_z_scores("NISRAstatsImp2", "Tend to disagree/Strongly disagree")

value_dont_know_qual_z_scores <- f_qual_z_scores("NISRAstatsImp2", "Don't know")

# Interference ####

interference_work_stats <- f_work_stats("Political2", "Strongly Agree/Tend to Agree", "Tend to disagree/Strongly disagree")

interference_age_stats <- f_age_stats("Political2", "Strongly Agree/Tend to Agree", "Tend to disagree/Strongly disagree")

interference_age_z_scores <- f_age_z_scores("Political2", "Strongly Agree/Tend to Agree")

interference_disagree_age_z_scores <- f_age_z_scores("Political2", "Tend to disagree/Strongly disagree")

interference_dont_know_age_z_scores <- f_age_z_scores("Political2", "Don't know")

interference_qual_stats <- f_qual_stats("Political2", "Strongly Agree/Tend to Agree", "Tend to disagree/Strongly disagree")

interference_qual_z_scores <- f_qual_z_scores("Political2", "Strongly Agree/Tend to Agree")

interference_disagree_qual_z_scores <- f_qual_z_scores("Political2", "Tend to disagree/Strongly disagree")

interference_dont_know_qual_z_scores <- f_qual_z_scores("Political2", "Don't know") 

# Confidentiality ####

confidential_work_stats <- f_work_stats("Confidential2", "Strongly Agree/Tend to Agree", "Tend to disagree/Strongly disagree")

confidential_age_stats <- f_age_stats("Confidential2", "Strongly Agree/Tend to Agree", "Tend to disagree/Strongly disagree")

confidential_age_z_scores <- f_age_z_scores("Confidential2", "Strongly Agree/Tend to Agree")

confidential_disagree_age_z_scores <- f_age_z_scores("Confidential2", "Tend to disagree/Strongly disagree")

confidential_dont_know_age_z_scores <- f_age_z_scores("Confidential2", "Don't know")

confidential_qual_stats <- f_qual_stats("Confidential2", "Strongly Agree/Tend to Agree", "Tend to disagree/Strongly disagree")

confidential_qual_z_scores <- f_qual_z_scores("Confidential2", "Strongly Agree/Tend to Agree")

confidential_disagree_qual_z_scores <- f_qual_z_scores("Confidential2", "Tend to disagree/Strongly disagree")

confidential_dont_know_qual_z_scores <- f_qual_z_scores("Confidential2", "Don't know") 

# ONS vs NISRA ####

nisra_ons_trust <- data.frame(trust = c("Trust", "Distrust", "Don't know", "Base"),
                              ons = c(data_ons$`Trust a great deal`[data_ons$`Related Variable` == "TrustNISRA2"] + data_ons$`Tend to trust`[data_ons$`Related Variable` == "TrustNISRA2"],
                                      data_ons$`Tend to distrust`[data_ons$`Related Variable` == "TrustNISRA2"] + data_ons$`Distrust greatly`[data_ons$`Related Variable` == "TrustNISRA2"],
                                      data_ons$`Don't know`[data_ons$`Related Variable` == "TrustNISRA2"],
                                      unweighted_ons$`Unweighted base`[unweighted_ons$`Related Variable` == "TrustNISRA2"]),
                              nisra = c(trust_year[[as.character(current_year)]][1],
                                        distrust_year[[as.character(current_year)]][1],
                                        dont_know_trust[[as.character(current_year)]][1],
                                        trust_year[[as.character(current_year)]][2])) %>%
  mutate(Z = case_when(trust == "Base" ~ NA,
                       TRUE ~ f_return_z(ons / 100, ons[trust == "Base"], nisra / 100, nisra[trust == "Base"])))

names(nisra_ons_trust) <- c("ONS figure is weighted", paste("ONS", ons_year), paste("NISRA", current_year), "Z Score")



# Trust NISRA Statistics ####

trust_stats_work_stats <- f_work_stats("TrustNISRAstats2", "Trust a great deal/Tend to trust", "Tend to distrust/Distrust greatly")

trust_stats_age_stats <- f_age_stats("TrustNISRAstats2", "Trust a great deal/Tend to trust", "Tend to distrust/Distrust greatly")

trust_stats_age_z_scores <- f_age_z_scores("TrustNISRAstats2", "Trust a great deal/Tend to trust")

trust_stats_disagree_age_z_scores <- f_age_z_scores("TrustNISRAstats2", "Tend to distrust/Distrust greatly")

trust_stats_dont_know_age_z_scores <- f_age_z_scores("TrustNISRAstats2", "Don't know")

trust_stats_qual_stats <- f_qual_stats("TrustNISRAstats2", "Trust a great deal/Tend to trust", "Tend to distrust/Distrust greatly")

trust_stats_qual_z_scores <- f_qual_z_scores("TrustNISRAstats2", "Trust a great deal/Tend to trust")

trust_stats_disagree_qual_z_scores <- f_qual_z_scores("TrustNISRAstats2", "Tend to distrust/Distrust greatly")

trust_stats_dont_know_qual_z_scores <- f_qual_z_scores("TrustNISRAstats2", "Don't know") 




# Trust in NISRA (exc DK) ####

## In work vs not in work ####

trust_nisra_work_ex_dk <- f_work_stats("TrustNISRA2", "Trust a great deal/Tend to trust", dk = FALSE)

##  By Age ####

trust_nisra_age_ex_dk <- f_age_stats("TrustNISRA2", "Trust a great deal/Tend to trust", dk = FALSE)

## Age comparison ####

trust_nisra_age_z_scores_ex_dk <- f_age_z_scores("TrustNISRA2", "Trust a great deal/Tend to trust", dk = FALSE)

## By qualification ####

trust_nisra_qual_ex_dk <- f_qual_stats("TrustNISRA2", "Trust a great deal/Tend to trust", dk = FALSE)

## Qualification comparison ####

trust_nisra_qual_z_scores_ex_dk <- f_qual_z_scores("TrustNISRA2", "Trust a great deal/Tend to trust", dk = FALSE)

# Trust NISRA stats (exc DK) ####

## In work vs not in work ####

trust_stats_work_ex_dk <- f_work_stats("TrustNISRAstats2", "Trust a great deal/Tend to trust", dk = FALSE)

##  By Age ####

trust_stats_age_ex_dk <- f_age_stats("TrustNISRAstats2", "Trust a great deal/Tend to trust", dk = FALSE)

## Age comparison ####

trust_stats_age_z_scores_ex_dk <- f_age_z_scores("TrustNISRAstats2", "Trust a great deal/Tend to trust", dk = FALSE)

## By qualification ####

trust_stats_qual_ex_dk <- f_qual_stats("TrustNISRAstats2", "Trust a great deal/Tend to trust", dk = FALSE)

## Qualification comparison ####

trust_stats_qual_z_scores_ex_dk <- f_qual_z_scores("TrustNISRAstats2", "Trust a great deal/Tend to trust", dk = FALSE)

# Value NISRA stats (exc DK) ####

## In work vs not in work ####

value_work_ex_dk <- f_work_stats("NISRAstatsImp2", "Strongly Agree/Tend to Agree", dk = FALSE)

##  By Age ####

value_age_ex_dk <- f_age_stats("NISRAstatsImp2", "Strongly Agree/Tend to Agree", dk = FALSE)

## Age comparison ####

value_age_z_scores_ex_dk <- f_age_z_scores("NISRAstatsImp2", "Strongly Agree/Tend to Agree", dk = FALSE)

## By qualification ####

value_qual_ex_dk <- f_qual_stats("NISRAstatsImp2", "Strongly Agree/Tend to Agree", dk = FALSE)

## Qualification comparison ####

value_qual_z_scores_ex_dk <- f_qual_z_scores("NISRAstatsImp2", "Strongly Agree/Tend to Agree", dk = FALSE)

# NISRA stats free from interference (exc DK) ####

## In work vs not in work ####

interference_work_ex_dk <- f_work_stats("Political2", "Strongly Agree/Tend to Agree", dk = FALSE)

##  By Age ####

interference_age_ex_dk <- f_age_stats("Political2", "Strongly Agree/Tend to Agree", dk = FALSE)

## Age comparison ####

interference_age_z_scores_ex_dk <- f_age_z_scores("Political2", "Strongly Agree/Tend to Agree", dk = FALSE)

## By qualification ####

interference_qual_ex_dk <- f_qual_stats("Political2", "Strongly Agree/Tend to Agree", dk = FALSE)

## Qualification comparison ####

interference_qual_z_scores_ex_dk <- f_qual_z_scores("Political2", "Strongly Agree/Tend to Agree", dk = FALSE)

# NISRA will keep my information confidential (exc DK) ####

## In work vs not in work ####

confidential_work_ex_dk <- f_work_stats("Confidential2", "Strongly Agree/Tend to Agree", dk = FALSE)

##  By Age ####

confidential_age_ex_dk <- f_age_stats("Confidential2", "Strongly Agree/Tend to Agree", dk = FALSE)

## Age comparison ####

confidential_age_z_scores_ex_dk <- f_age_z_scores("Confidential2", "Strongly Agree/Tend to Agree", dk = FALSE)

## By qualification ####

confidential_qual_ex_dk <- f_qual_stats("Confidential2", "Strongly Agree/Tend to Agree", dk = FALSE)

## Qualification comparison ####

confidential_qual_z_scores_ex_dk <- f_qual_z_scores("Confidential2", "Strongly Agree/Tend to Agree", dk = FALSE)

