# Core wrapping function:
f_wrap_it <- function(x, len) {
  sapply(x, function(y) {
    paste(strwrap(y, len),
      collapse = " \n"
    )
  },
  USE.NAMES = FALSE
  )
}

# Call this function with a list or vector:
f_wrap_labels <- function(x, len) {
  if (is.list(x)) {
    lapply(x, f_wrap_it, len)
  } else {
    f_wrap_it(x, len)
  }
}
