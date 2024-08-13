

# Returns number of non blank values in a column "var"
f_return_n <- function(var) {
  length(var[!is.na(var)])
}

# Returns percentage of times a given "value" occurs in column "var"
f_return_p <- function(var, value) {
  length(var[!is.na(var) & var == value]) / f_return_n(var)
}

f_return_n_group <- function(var, group_var, group_value) {
  length(var[!is.na(var) & !is.na(group_var) & group_var == group_value])
}

f_return_p_group <- function(var, value, group_var, group_value) {
  length(var[!is.na(var) & !is.na(group_var) & var == value & group_var == group_value]) / f_return_n_group(var, group_var, group_value)
}

f_return_z <- function(p1, n1, p2, n2) {
  
  s1 <- (1 / n1) + (1 / n2)
  s2 <- p1 * n1
  s3 <- p2 * n2
  s4 <- s2 / n1
  s5 <- s3 / n2
  s6 <- s4 - s5
  s7 <- (s2 + s3) / (n1 + n2)
  s8 <- 1 - s7
  s9 <- sqrt(s7 * s8 *s1)
  s6 / s9
  
}

f_significance_year <- function(var, value, dk = TRUE) {
  
  data_current_f <- data_current
  data_last_f <- data_last
  
  if (dk == FALSE) {
    
    data_current_f <- data_current_f %>%
      filter(.[[var]] != "Don't know")
    
    data_last_f <- data_last_f %>%
      filter(.[[var]] != "Don't know")
    
  }
  
  sig_table <- data.frame(stat = c("%", "Base"),
                           
                           current = c(f_return_p(data_current_f[[var]], value) * 100,
                                       f_return_n(data_current_f[[var]])),
                           
                           last = c(f_return_p(data_last_f[[var]], value) * 100,
                                    f_return_n((data_last_f[[var]]))),
                           
                           z = c(f_return_z(p1 = f_return_p(data_current_f[[var]], value),
                                            n1 = f_return_n(data_current_f[[var]]),
                                            p2 = f_return_p(data_last_f[[var]], value),
                                            n2 = f_return_n(data_last_f[[var]])),
                                 NA))
  
  names(sig_table) <- c(" ", current_year, current_year - 1, "Z Score")
  
  sig_table
  
}

f_age_z_scores <- function(var, value, dk = TRUE) {
  
  age_groups <- levels(data_current$AGE2)
  
  age_z_scores <- data.frame(age = age_groups)
  
  for (i in 1:length(age_groups)) {
    col <- c()
    for(j in 1:length(age_groups)) {
      if (i > j) {
        if (dk) {
          col[j] <- f_return_z(p1 = f_return_p_group(data_current[[var]], value, data_current$AGE2, age_groups[j]),
                               n1 = f_return_n_group(data_current[[var]], data_current$AGE2, age_groups[j]),
                               p2 = f_return_p_group(data_current[[var]], value, data_current$AGE2, age_groups[i]),
                               n2 = f_return_n_group(data_current[[var]], data_current$AGE2, age_groups[i]))
        } else {
          col[j] <- f_return_z(p1 = data_current %>%
                                 filter(!is.na(.[[var]]) & .[[var]] == value & AGE2 == age_groups[j]) %>%
                                 nrow() / data_current %>%
                                 filter(!is.na(.[[var]]) & .[[var]] != "Don't know" & AGE2 == age_groups[j]) %>%
                                 nrow(),
                               n1 = data_current %>%
                                 filter(!is.na(.[[var]]) & .[[var]] != "Don't know" & AGE2 == age_groups[j]) %>%
                                 nrow(),
                               p2 = data_current %>%
                                 filter(!is.na(.[[var]]) & .[[var]] == value & AGE2 == age_groups[i]) %>%
                                 nrow() / data_current %>%
                                 filter(!is.na(.[[var]]) & .[[var]] != "Don't know" & AGE2 == age_groups[i]) %>%
                                 nrow(),
                               n2 = data_current %>%
                                 filter(!is.na(.[[var]]) & .[[var]] != "Don't know" & AGE2 == age_groups[i]) %>%
                                 nrow())
        }
      } else {
        col[j] <- NA
      }
    }
    age_z_scores[[age_groups[i]]] <- col
  }
  
  names(age_z_scores)[names(age_z_scores) == "age"] <- " "
  
  age_z_scores
  
}

f_qual_z_scores <- function (var, value, dk = TRUE) {
  
  quals <- levels(data_current$DERHIanalysis)[!levels(data_current$DERHIanalysis) %in% c("Refusal", "DontKnow", "Other qualifications")]
  
  qual_z_scores <- data.frame(qual = quals)
  
  for (i in 1:length(quals)) {
    col <- c()
    for(j in 1:length(quals)) {
      if (i > j) {
        if (dk) {
          col[j] <- f_return_z(p1 = f_return_p_group(data_current[[var]], value, data_current$DERHIanalysis, quals[j]),
                               n1 = f_return_n_group(data_current[[var]], data_current$DERHIanalysis, quals[j]),
                               p2 = f_return_p_group(data_current[[var]], value, data_current$DERHIanalysis, quals[i]),
                               n2 = f_return_n_group(data_current[[var]], data_current$DERHIanalysis, quals[i]))
        } else {
          col[j] <- f_return_z(p1 = data_current %>%
                                 filter(!is.na(.[[var]]) & .[[var]] == value & DERHIanalysis == quals[j]) %>%
                                 nrow() / data_current %>%
                                 filter(!is.na(.[[var]]) & .[[var]] != "Don't know" & DERHIanalysis == quals[j]) %>%
                                 nrow(),
                               n1 = data_current %>%
                                 filter(!is.na(.[[var]]) & .[[var]] != "Don't know" & DERHIanalysis == quals[j]) %>%
                                 nrow(),
                               p2 = data_current %>%
                                 filter(!is.na(.[[var]]) & .[[var]] == value & DERHIanalysis == quals[i]) %>%
                                 nrow() / data_current %>%
                                 filter(!is.na(.[[var]]) & .[[var]] != "Don't know" & DERHIanalysis == quals[i]) %>%
                                 nrow(),
                               n2 = data_current %>%
                                 filter(!is.na(.[[var]]) & .[[var]] != "Don't know" & DERHIanalysis == quals[i]) %>%
                                 nrow())
        }
      } else {
        col[j] <- NA
      }
    }
    qual_z_scores[[quals[i]]] <- col
  }
  
  names(qual_z_scores)[names(qual_z_scores) == "qual"] <- " "
  
  qual_z_scores
}

f_work_stats <- function(var, value1, value2 = NA, dk = TRUE) {
  
  if (dk) {

    work_stats <- data.frame(stat = c("% Yes", "% No", "% DK", "Base"),
                                    
                                    work = c(f_return_p_group(data_current[[var]], value1, data_current$EMPST2, "In paid employment") * 100,
                                             f_return_p_group(data_current[[var]], value2, data_current$EMPST2, "In paid employment") * 100,
                                             f_return_p_group(data_current[[var]], "Don't know", data_current$EMPST2, "In paid employment") * 100,
                                             f_return_n_group(data_current[[var]], data_current$EMPST2, "In paid employment")),
                                    
                                    not = c(f_return_p_group(data_current[[var]], value1, data_current$EMPST2, "Not in paid employment") * 100,
                                            f_return_p_group(data_current[[var]], value2, data_current$EMPST2, "Not in paid employment") * 100,
                                            f_return_p_group(data_current[[var]], "Don't know", data_current$EMPST2, "Not in paid employment") * 100,
                                            f_return_n_group(data_current[[var]], data_current$EMPST2, "Not in paid employment")),
                                    
                                    z = c(f_return_z(p1 = f_return_p_group(data_current[[var]], value1, data_current$EMPST2, "In paid employment"),
                                                     n1 = f_return_n_group(data_current[[var]], data_current$EMPST2, "In paid employment"),
                                                     p2 = f_return_p_group(data_current[[var]], value1, data_current$EMPST2, "Not in paid employment"),
                                                     n2 = f_return_n_group(data_current[[var]], data_current$EMPST2, "Not in paid employment")),
                                          f_return_z(p1 = f_return_p_group(data_current[[var]], value2, data_current$EMPST2, "In paid employment"),
                                                     n1 = f_return_n_group(data_current[[var]], data_current$EMPST2, "In paid employment"),
                                                     p2 = f_return_p_group(data_current[[var]], value2, data_current$EMPST2, "Not in paid employment"),
                                                     n2 = f_return_n_group(data_current[[var]], data_current$EMPST2, "Not in paid employment")),
                                          f_return_z(p1 = f_return_p_group(data_current[[var]], "Don't know", data_current$EMPST2, "In paid employment"),
                                                     n1 = f_return_n_group(data_current[[var]], data_current$EMPST2, "In paid employment"),
                                                     p2 = f_return_p_group(data_current[[var]], "Don't know", data_current$EMPST2, "Not in paid employment"),
                                                     n2 = f_return_n_group(data_current[[var]], data_current$EMPST2, "Not in paid employment")),
                                          NA))
  
  } else {
    
    work_stats <- data.frame(trust = c("%", "Base"),
                                    work = c(data_current %>%
                                               filter(!is.na(.[[var]]) & .[[var]] == value1 & EMPST2 == "In paid employment") %>%
                                               nrow() / data_current %>%
                                               filter(!is.na(.[[var]]) & .[[var]] != "Don't know" & EMPST2 == "In paid employment") %>%
                                               nrow() * 100,
                                             data_current %>%
                                               filter(!is.na(.[[var]]) & .[[var]] != "Don't know" & EMPST2 == "In paid employment") %>%
                                               nrow()),
                                    not = c(data_current %>%
                                              filter(!is.na(.[[var]]) & .[[var]] == value1 & EMPST2 == "Not in paid employment") %>%
                                              nrow() / data_current %>%
                                              filter(!is.na(.[[var]]) & .[[var]] != "Don't know" & EMPST2 == "Not in paid employment") %>%
                                              nrow() * 100,
                                            data_current %>%
                                              filter(!is.na(.[[var]]) & .[[var]] != "Don't know" & EMPST2 == "Not in paid employment") %>%
                                              nrow())) %>%
      mutate(Z = case_when(trust == "Base" ~ NA,
                           TRUE ~ f_return_z(work / 100, work[trust == "Base"], not / 100, not[trust == "Base"])))
    
  }
  
  names(work_stats) <- c(" ", "In work", "Not in work", "Z Score")
  
  work_stats

}

f_age_stats <- function(var, value1, value2 = NA, dk = TRUE) {
  
  age_groups <- levels(data_current$AGE2)
  
  if (dk) {
  
    age_stats <- data.frame(stat = c("% Yes", "% No", "% DK", "Base"))
    
    for (age in age_groups) {
      age_stats[[age]] <- c(f_return_p_group(data_current[[var]], value1, data_current$AGE2, age) * 100,
                                  f_return_p_group(data_current[[var]], value2, data_current$AGE2, age) * 100,
                                  f_return_p_group(data_current[[var]], "Don't know", data_current$AGE2, age) * 100,
                                  f_return_n_group(data_current[[var]], data_current$AGE2, age))
    }
  
  } else {
    
    age_stats <- data.frame(stat = c("% Yes", "Base"))
    
    for (age in age_groups) {
      age_stats[[age]] <- c(data_current %>%
                              filter(!is.na(.[[var]]) & .[[var]] == value1 & data_current$AGE2 == age) %>%
                              nrow() /
                              data_current %>%
                                filter(!is.na(.[[var]]) & .[[var]] != "Don't know" & data_current$AGE2 == age) %>%
                                nrow() * 100,
                            data_current %>%
                              filter(!is.na(.[[var]]) & .[[var]] != "Don't know" & data_current$AGE2 == age) %>%
                              nrow())
    }
    
  }
  
  names(age_stats)[names(age_stats) == "stat"] <- " "
  
  age_stats
  
}

f_qual_stats <- function(var, value1, value2 = NA, dk = TRUE) {
  
  quals <- levels(data_current$DERHIanalysis)[!levels(data_current$DERHIanalysis) %in% c("Refusal", "DontKnow", "Other qualifications")]
  
  if (dk) {
  
    qual_stats <- data.frame(stat = c("% Yes", "% No", "% DK", "Base"))
    
    for (qual in quals) {
      qual_stats[[qual]] <- c(f_return_p_group(data_current[[var]], value1, data_current$DERHIanalysis, qual) * 100,
                              f_return_p_group(data_current[[var]], value2, data_current$DERHIanalysis, qual) * 100,
                              f_return_p_group(data_current[[var]], "Don't know", data_current$DERHIanalysis, qual) * 100,
                              f_return_n_group(data_current[[var]], data_current$DERHIanalysis, qual))
    }
  
  } else {
    
    qual_stats <- data.frame(stat = c("% Yes", "Base"))
    
    for (qual in quals) {
      qual_stats[[qual]] <- c(data_current %>%
                                filter(!is.na(.[[var]]) & .[[var]] == value1 & DERHIanalysis == qual) %>%
                                nrow() / data_current %>%
                                filter(!is.na(.[[var]]) & .[[var]] != "Don't know" & DERHIanalysis == qual) %>%
                                nrow() * 100,
                              data_current %>%
                                filter(!is.na(.[[var]]) & .[[var]] != "Don't know" & DERHIanalysis == qual) %>%
                                nrow())
    }
    
  }
   
  names(qual_stats)[names(qual_stats) == "stat"] <- " "
  
  qual_stats
}

f_insert_sig_table <- function (df, sheet, title, c = 1) {
  
  writeData(wb, sheet,
            x = title,
            startRow = r,
            startCol = c)
  
  addStyle(wb, sheet,
           style = pt2,
           rows = r,
           cols = c)
  
  r <<- r + 1
  
  writeDataTable(wb, sheet,
                 x = df,
                 startRow = r,
                 startCol = c,
                 tableStyle = "none",
                 headerStyle = ch,
                 withFilter = FALSE)
  
  addStyle(wb, sheet,
           style = ns3d,
           rows = (r + 1):(r + nrow(df) - 1),
           cols = (c + 1):(c + ncol(df) - 1),
           gridExpand = TRUE)
  
  addStyle(wb, sheet,
           style = ns_comma,
           rows = r + nrow(df),
           cols = (c + 1):(c + ncol(df) - 1),
           gridExpand = TRUE)
  
  if ("Z Score" %in% names(df)) {
    
    for (i in 1:(nrow(df) - 1)) {
  
      if (abs(df$`Z Score`[i]) > 1.96) {
        addStyle(wb, sheet,
                 style = sig,
                 rows = r + i,
                 cols = c + ncol(df) - 1)
      } else {
        addStyle(wb, sheet,
                 style = not_sig,
                 rows = r + i,
                 cols = c + ncol(df) - 1)
      }
      
    }
    
  }
  
  r <<- r + nrow(df) + 2
  
}

f_insert_z_table <- function (df, sheet, title) {
  
  writeData(wb, sheet,
            x = title,
            startRow = r)
  
  addStyle(wb, sheet,
           style = pt2,
           rows = r,
           cols = 1)
  
  r <<- r + 1
  
  writeDataTable(wb, sheet,
                 x = df,
                 startRow = r,
                 tableStyle = "none",
                 headerStyle = ch,
                 withFilter = FALSE)
  
  addStyle(wb, sheet,
           style = ns3d,
           rows = (r + 1):(r + nrow(df)),
           cols = 2:ncol(df),
           gridExpand = TRUE)
  
  for (i in 1:nrow(df)) {
    for (j in 2:ncol(df)) {
      if (!is.na(df[i, j])) {
        if (abs(df[i, j]) > 1.96) {
          addStyle(wb, sheet,
                   style = sig,
                   rows = r + i,
                   cols = j)
        } else {
          addStyle(wb, sheet,
                   style = not_sig,
                   rows = r + i,
                   cols = j)
        }
      }
    }
  }
  
  for (i in 1:nrow(df)) {
    addStyle(wb, sheet,
             style = grey,
             rows = r + i,
             cols = 1 + i)
  }
  
  r <<- r + nrow(df) + 2
  
}

f_nisra_ons_ex_dk <- function (var, val) {
  
  df <- data.frame(trust = c("% Yes", "Base"),
                                      nisra = c(data_current %>%
                                                  filter(.[[var]] == val) %>%
                                                  nrow() / data_current %>%
                                                  filter(!is.na(.[[var]]) & .[[var]] != "Don't know") %>%
                                                  nrow() * 100,
                                                data_current %>%
                                                  filter(!is.na(.[[var]]) & .[[var]] != "Don't know") %>%
                                                  nrow()),
                                      ons = c(unweighted_ons[[val]][unweighted_ons$`Related Variable` == var] /
                                                (unweighted_ons$`Unweighted base`[unweighted_ons$`Related Variable` == var] -
                                                   unweighted_ons$`Don't know`[unweighted_ons$`Related Variable` == var]) * 100,
                                              unweighted_ons$`Unweighted base`[unweighted_ons$`Related Variable` == var] -
                                                unweighted_ons$`Don't know`[unweighted_ons$`Related Variable` == var])) %>%
    mutate(Z = case_when(trust == "Base" ~ NA,
                         TRUE ~ f_return_z(ons / 100, ons[trust == "Base"], nisra / 100, nisra[trust == "Base"])))
  
  names(df) <- c(" ", paste("NISRA", current_year), paste("ONS", ons_year), "Z Score")
  
  df
  
}

f_nisra_ons <- function (var, val_1, val_2) {
  
  ons_values <- unweighted_ons %>%
    filter(`Related Variable` == var)
  
  df <- data.frame(trust = c("% Yes", "% No", "% Don't know", "Base"),
                   nisra = c(f_return_p(data_current[[var]], val_1) * 100,
                             f_return_p(data_current[[var]], val_2) * 100,
                             f_return_p(data_current[[var]], "Don't know") * 100,
                             f_return_n(data_current[[var]])),
                   ons = c(ons_values[[val_1]] / ons_values$`Unweighted base` * 100,
                           ons_values[[val_2]] / ons_values$`Unweighted base` * 100,
                           ons_values$`Don't know` / ons_values$`Unweighted base` * 100,
                           ons_values$`Unweighted base`)) %>%
    mutate(Z = case_when(trust == "Base" ~ NA,
                         TRUE ~ f_return_z(ons / 100, ons[trust == "Base"], nisra / 100, nisra[trust == "Base"])))
  
  names(df) <- c(" ", paste("NISRA", current_year), paste("ONS", ons_year), "Z Score")
  
  df
  
}
