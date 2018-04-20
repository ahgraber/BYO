MAD <- function(actual, prediction) {
  # Calculate Mean Absolute Deviation (MAD) based on provided data & predictions
  difference <- prediction-median(actual)
  mad <- sum( abs(difference) ) / length(actual)
  return(mad)
}
