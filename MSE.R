MSE <- function(actual, prediction) {
  # Calculate Mean Squared Error (RMSE) based on provided data & predictions
  difference <- prediction - actual
  mse <- sum(difference^2) / length(actual)
  return(mse)
}
