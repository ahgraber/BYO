cos_d <- function(x,y) {
  # Cosine similarity
  # Args: x and y vectors to be compared

  d <- (x %*% y) / ( sqrt(sum(x^2))*sqrt(sum(y^2)) )
  return(d)
}
