library(tidyverse)

chi-sq.matrix <- function(data) {
  # Check if 'data' argument is a dataframe
  if(!is.data.frame(data)){
    stop("Argument 'data' must be a dataframe")
  }
  
  # Check if each column is a factor
  map(data, function(column){
    if(!is.factor(column) | is.character(column)){
      stop("All columns must be type 'factor' or 'character'")
    }
  })
  
}

