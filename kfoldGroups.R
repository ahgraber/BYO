kfoldGroups <- function (data, k) {
  # Return vector containing group identity from 1:k per row
  # Args:
    # k - number cross-validation partitions
    # data - dataset

  kGrpId <- ceiling( runif(nrow(data), min=0, max=k) )
  return(kGrpId)
}
