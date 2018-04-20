normalize <- function(df) {
  # Normalize provided data frame such that each column's range is [0,1]
  # Args:
    # df - dataframe to normalize

  df <- as.matrix(df)
  min <- apply(x,2, min)
  max <- apply(x,2, max)
  range <- max-min
  norm <- function(x) { (x-min) / range } # normalize to [0,1] range

  return(as.data.frame(t(apply(df,1,norm))))
}
