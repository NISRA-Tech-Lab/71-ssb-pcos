f_read_spss <- function (filepath, pass = "") {
  
  data <- readspss::read.spss(paste0(data_folder, "Raw/", data_filename),
                              pass = password,
                              use.missings = FALSE
  )
  
  # Add labels to variables
  raw_labels <- attributes(data)$var.label
  has_label <- attributes(data)$varmatrix[, 2]
  
  all_labels <- rep(NA, length(has_label))
  
  all_labels[has_label == 1] <- raw_labels
  
  labels <- data.frame(
    Variable = names(data),       
    HasLabel = has_label,        
    Label = all_labels
  )
  
  for (i in 1:nrow(labels)) {
    attributes(data[[labels$Variable[i]]])$label <- labels$Label[i]
  }
  
  data
  
}