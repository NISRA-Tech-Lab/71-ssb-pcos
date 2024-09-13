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
    repo_list[[paste0("CRAN")]] <- repo
  } else {
    repo_list[[paste0("CRAN",i)]] <- repo
  }
}

options(repos = unlist(repo_list))
options("repos")
options(renv.config.repos.override = unlist(repo_list))
options("renv.config.repos.override")

# clear working directory
rm(list=ls())

# conditional activation of renv
source("renv/activate.R")
