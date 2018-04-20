linearReg <- function(X, y, scaled=F, gradDesc=F, alpha=0.01, max_iters=1000) {
  # Run linear regression on data
  # Return list of (list of beta coefficients, residuals, R^2)
  # Args:
    # y - vector of actual values (dependent variable trainers)
    # X - data with features to model (independent variables)
    # [scaled] - use feature scaling or not (default F)
    # [gradDesc] - force use of gradient decscent (default F)
    # [alpha] - learning rate for gradient descent
    # [max_iters] - maximum allowed iterations for gradient descent


  ### Housekeeping:
  # in case data comes in df, force transform df into matrix
  X <- as.matrix(X)

  # identify number of records m to learn from
  m <- length(y)
  # confirm this is equivalent to length of X
  if (m != nrow(X)) {stop: 'X and y do not have same number of rows!'}

  # Standardize(?)
  if (scaled) {
    source('standardize.R')
    X <- standardize(X)
  } else {
    # no change
  }

  # add intercept term to data matrix
  int <- rep(1, m)     # vector of as many 1s as we have rows of data
  X <- as.matrix(cbind(int, X))   # add data matrix to our column of 1s

  # initialize theta with random starting point
  theta <- runif(ncol(X), 0, 1)


  ### SOLVE:
  # Do we want to use closed-form linalg solution or gradient descent algorithm?
  # Depends on invertiblity and size of X data matrix
  cache <- tryCatch( solve(t(X) %*% X),
                     error = function(e) {
                       print("Data matrix is not invertible; using gradient descent")
                       return(FALSE)
                       }
                     )

  # if small and invertible, use closed form of ordinary least squares to solve the matrix
  if ( (ncol(X) < 5000) & (is.matrix(cache)) & (gradDesc == F)) {
    theta <- cache %*% t(X) %*% y
  }
  # otherwise use gradient descent to search for (hopefully) optimal coefficients
  else {
    source('gradientDescent.R')
    # debug(gradientDescent)
    theta <- gradientDescent(X, y, theta, alpha, max_iters)
  } # end if/else


  ### Return structures (named list of coefficients, R2):
  pred <- X %*% theta
  r2 <- sum( (pred - mean(y))^2 ) / sum( (y - mean(y))^2 )

  result <- list(theta, r2)
  names(result) <- c("coefficients", "R2")

  return(result)

} # end function
