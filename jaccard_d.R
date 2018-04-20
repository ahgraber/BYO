jaccard_d <- function(x,y) {
  # Jaccard similarity
  # Args: *binary* x and y vectors to be compared

  f11 <- length( which(x==1 & y==1) )
  f10 <- length( which(x==1 & y==0) )
  f01 <- length( which(x==0 & y==1) )
  # f00 <- length( which(x==0 & y==0) )

  d <- f11 / (f11 + f10 + f01)
  return(d)
}





