cor_d <- function(x,y) {
  # Correlation distance
  # Args: x and y vectors to be compared

  source("standardize.R")
  d <- ( standardize(x) %*% standardize(y) ) / ( length(x)-1 )
  return(d)
}
