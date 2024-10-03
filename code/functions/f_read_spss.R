f_read_spss <- function (filepath, pass = NA) {
  
  data <- if (is.na(pass)) {
    readspss::read.sav(filepath,
                       use.missings = FALSE)
  } else {
    readspss::read.sav(filepath,
                       pass = pass,
                       use.missings = FALSE)
  }

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