RMSE <- function(actual, prediction) {
  # Calculate Root Mean Squared Error (RMSE) based on provided data & predictions
  rmse <- sqrt( MSE(actual, prediction) )
  return(rmse)
}
