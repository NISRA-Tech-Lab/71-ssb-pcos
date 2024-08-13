# Code to adapt DfT R Code to run in conjuction with ITAssist policy
# Project directory must be under "C:\User\*\Documents\R\" (where * is your username) for this work

f_vbs_execute <- function(vbs_file, ...){
  
  ##Convert arguments into a single string
  arguments <- paste(c(...), collapse = " ")
  #Create system command for specified vbs file
  system_command <- paste("WScript",
                          vbs_file,
                          arguments,
                          sep = " ")
  #Run specified command
  system(command = system_command)
}


f_convert_to_ods <- function(path) {

  ##Stop if file is not found
  ##Stop if file is not found
  if(file.exists(path) == FALSE){
    stop("File not found")
  }
  
  #Stop if file is not an xlsx
  if(grepl(".xlsx", path, fixed = TRUE) == FALSE){
    stop("File is not an xlsx file")
  }
  
  ##Convert path to absolute one
  xlsx_all <-  paste0('"',normalizePath(path),  '"')
  
  ods_all <-  gsub(".xlsx", ".ods", xlsx_all, fixed = TRUE)
  
  
  #Run VBS script passing it the file paths
  f_vbs_execute(paste0(here(), "/code/functions/save.vbs"),
                xlsx_all,
                ods_all)
  
}

