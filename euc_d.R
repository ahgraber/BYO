euc_d <- function(x,y) {
  # Euclidian distance
  # Args: x and y vectors to be compared

  d <- sqrt( sum( (x-y)^2 ) )
  return(d)
}
