
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