# live search of package drive for repo folders
repos <- list.files("//pr-clus-vfpdfp/DOF_NISRA_R_Packages/")
repos <- (grep("^production", repos, value = T))
# add CRAN line to end of repo list
repos <- c(repos, "https://cran.rstudio.com/")
repo_list <- list()

# create list of repo folders to inform options("repos") and options("renv.config.repos.override")
for (i in seq_along(repos)) {
  
  if(grepl("^https", repos[i])) {
    repo <- repos[i]
  } else {
    repo <- paste0("file:////pr-clus-vfpdfp/DOF_NISRA_R_Packages/",repos[i])
  }
  
  if(i == 1) {
    repo_list[[paste0("internal_repo")]] <- repo
  } else {
    repo_list[[paste0("internal_repo",i)]] <- repo
  }
}
names(repo_list)[length(repo_list)] <- "CRAN"

# set renv.config.repos.override option
options(renv.config.repos.override = unlist(repo_list))
# set repos option
options(repos = unlist(repo_list))

# conditional activation of renv
source("renv/activate.R")

# tell renv to prefer binary installs over source
options(renv.config.install.prefer.binary = TRUE)

# reset repos option - cleared by activate
options(repos = unlist(repo_list))

# clear working directory
rm(list=ls())
