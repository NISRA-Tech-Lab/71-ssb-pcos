f_email <- function(email) {
  require(htmltools)
  a(href = paste0("mailto:", email), email)
}
