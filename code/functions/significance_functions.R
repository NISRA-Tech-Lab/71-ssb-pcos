
f_return_n <- function(var) {
  length(var[!is.na(var)])
}

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

f_significance_year <- function(var, value) {
  
  sig_table <- data.frame(stat = c("%", "Base"),
                           
                           current = c(f_return_p(data_current[[var]], value),
                                       f_return_n(data_current[[var]])),
                           
                           last = c(f_return_p(data_last[[var]], value),
                                    f_return_n((data_last[[var]]))),
                           
                           z = c(f_return_z(p1 = f_return_p(data_current[[var]], value),
                                            n1 = f_return_n(data_current[[var]]),
                                            p2 = f_return_p(data_last[[var]], value),
                                            n2 = f_return_n(data_last[[var]])),
                                 NA))
  
  names(sig_table) <- c(" ", current_year, current_year - 1, "Z Score")
  
  sig_table
  
}

f_age_z_scores <- function(var, value) {
  
  age_groups <- levels(data_current$AGE2)
  
  age_z_scores <- data.frame(age = age_groups)
  
  for (i in 1:length(age_groups)) {
    col <- c()
    for(j in 1:length(age_groups)) {
      if (i > j) {
        col[j] <- f_return_z(p1 = f_return_p_group(data_current[[var]], value, data_current$AGE2, age_groups[j]),
                             n1 = f_return_n_group(data_current[[var]], data_current$AGE2, age_groups[j]),
                             p2 = f_return_p_group(data_current[[var]], value, data_current$AGE2, age_groups[i]),
                             n2 = f_return_n_group(data_current[[var]], data_current$AGE2, age_groups[i]))
      } else {
        col[j] <- NA
      }
    }
    age_z_scores[[age_groups[i]]] <- col
  }
  
  names(age_z_scores)[names(age_z_scores) == "age"] <- " "
  
  age_z_scores
  
}

f_qual_z_scores <- function (var, value) {
  
  quals <- levels(data_current$DERHIanalysis)[!levels(data_current$DERHIanalysis) %in% c("Refusal", "DontKnow", "Other qualifications")]
  
  qual_z_scores <- data.frame(qual = quals)
  
  for (i in 1:length(quals)) {
    col <- c()
    for(j in 1:length(quals)) {
      if (i > j) {
        col[j] <- f_return_z(p1 = f_return_p_group(data_current[[var]], value, data_current$DERHIanalysis, quals[j]),
                             n1 = f_return_n_group(data_current[[var]], data_current$DERHIanalysis, quals[j]),
                             p2 = f_return_p_group(data_current[[var]], value, data_current$DERHIanalysis, quals[i]),
                             n2 = f_return_n_group(data_current[[var]], data_current$DERHIanalysis, quals[i]))
      } else {
        col[j] <- NA
      }
    }
    qual_z_scores[[quals[i]]] <- col
  }
  
  names(qual_z_scores)[names(qual_z_scores) == "qual"] <- " "
  
  qual_z_scores
}


