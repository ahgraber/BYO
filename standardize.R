standardize <- function(df) {
  # Standardize provided data frame such that each column's range is [0,1]
  # Args:
    # df - dataframe to normalize

  df <- as.matrix(df)
  mean <- apply(df, 2, mean)
  sd <- apply(df, 2, sd)
  stdz <- function(x) { (x-mean) / sd } # standardize to mean = 0, std.dev = 1

  return(as.data.frame(t(apply(df,1,stdz))))
}
