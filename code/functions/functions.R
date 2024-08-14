# check list of packages and install if needed before loading
# if package already installed will be loaded
f_install_and_load_packages <- function(packages) {
  installed_packages <- rownames(installed.packages())
  packages_to_install <- packages[!packages %in% installed_packages]

  if (length(packages_to_install) > 0) {
    message(paste0(
      "Installing packages: ",
      paste(packages_to_install, collapse = ", ")
    ))
    install.packages(packages_to_install)
  } else {
    message("All packages already installed.")
  }

  # load all packages, whether newly installed or not
  for (pkg in packages) {
    library(pkg, character.only = TRUE, quietly = TRUE)
  }
}

# Core wrapping function:
f_wrap_it <- function(x, len)
{ 
  sapply(x, function(y) paste(strwrap(y, len), 
                              collapse = " \n"), 
         USE.NAMES = FALSE)
}

# Call this function with a list or vector:
f_wrap_labels <- function(x, len)
{
  if (is.list(x))
  {
    lapply(x, f_wrap_it, len)
  } else {
    f_wrap_it(x, len)
  }
}
