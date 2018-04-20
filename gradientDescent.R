gradientDescent <- function(X, y, theta, alpha=0.01, max_iters=1000) {
  # Perform gradientDescent algorithm to identify weights theta that minimize our error
    # Per Andrew Ng's Machine Learning course, we take partial derivative of the cost function
    # with respect to theta (similar to OLS regression).
    # The derivative (gradient) is multiplied by a learning rate to update the estimated values
    # of the parameters (thetas). Assuming a proper learning rate, if we iterate,
    # we should achieve convergence at a (hopefully) global minima
    # (i.e., our coefficients will give us a line with the least error)

  # Args:
    # X - data matrix (independent variables)
    # y - actual values (dependent variable)
    # theta - (aka betas) coefficients for features we're trying to minimize
    # alpha - learning rate (how big of step to take)
    # max_iters - maximum number of steps to take to find convergence

  ### housekeeping:
  # identify number of records m to learn from
  m <- length(y)
  cost_history = rep(0, max_iters);

  cost <- function(X, y, theta) {
    error <- (X %*% theta) - y    # difference between predicted and actual
    J = 1/(2*m) * sum( error^2 )
    return(J)
  }


  for (i in 1:max_iters) {
    error <- (X %*% theta) - y    # difference between predicted and actual
    gradient <- (1/m) * t(X) %*% error    # derivative of cost w/ respect to theta
    theta <- theta - (alpha * gradient)    # update theta

    # save cost of every iteration
    cost_history[i] <- cost(X, y, theta)


    ### Stopping criteria:
    # if converge (defined by difference in cost between iterations < .001), stop iterating
    if (i > 1) {
      if ( (cost_history[i-1] - cost_history[i]) <= .00001 ) {
        break
      }
      # if non-converge, suggest different learning rate
      else if ( cost_history[i] > cost_history[i-1] ) {
        stop('Gradient Descent is non-converging; try smaller alpha')
      } # end if/elseif
  } # end if

  } # end for


  ### Check for convergence after max_iters
  # if non-converge, suggest different learning rate
  if ( (cost_history[i-1] - cost_history[i]) > .00001 ) {
      stop('Gradient Descent is non-converging after max iterations; try larger alpha')
  }
  # otherwise, return the optimal thetas
  else {
    return(theta)
  } # end if/else


} # end function


