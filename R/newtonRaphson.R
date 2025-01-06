
#' newtonRaphson.Function
#'
#' @description
#' @param funcToEval
#' @param Ysol Default: 0
#' @param x0 (Default: \code{NULL}).
#' @param range Default: c(-10, 10)
#' @param relTol Default: 1e-06
#' @param absTol Default: 1e-09
#' @param n Default: 1000
#' @param iterMax Default: 1000
#' @return
#' @export
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
#' @family General Functions
newtonRaphson.Function <- function(
  funcToEval,        # Function to evaluate for the equation
  Ysol    = 0,         # The value such as "newtonRaphson.Function" returns the solution Xsol of Ysol=f(Xsol)
  x0      = NULL,      # Initial value for the algorithm. If not provided, it will analyze the derivative to find the optimum start point.
  range   = c(-10,10), # Numeric vector of two scalars that determine the range of the search
  relTol  = 1e-6,      # Relative tolerance of the solution
  absTol  = 1e-9,      # Absolute tolerance of the solution
  n       = 1000,      # number of interval to estimate the derivative
  iterMax = 1000       # Maximum number of iteration
) {

  # Check that 'range' is properly defined:
  if (!is.numeric(range) && length(unique(range))!=2){
    stop("'range' should be a numeric vector of length 2 in newton.Function.R")
  }

  # Order 'range':
  range <- sort(range)

  # Check if x0 is NULL:
  #   'range' will be used to estimate the optimum x0 to start with using the derivative
  if (is.null(x0)){
    # Calculate optimum x0:
    #   Estimate funcToEval at various points
    X <- seq(range[1], range[2], by=(range[2]-range[1])/n)
    Y <- numeric(length=n+1)
    for (k in 1:(n+1)){
      Y[k] <- funcToEval(X[k])
    }
    #   Estimate the derivative of the function funcToEval at X
    dY    <- numeric(length=n+1)
    dY[1] <- (Y[2]-Y[1])/(X[2]-X[1])
    dY[n] <- (Y[n]-Y[n-1])/(X[n]-X[n-1])
    for (i in 2:(n-1)){
      dY[i] <- (Y[i+1]-Y[i-1])/(X[i+1]-X[i-1])
    }

    # Estimate optimum x0::
    ix = which.max(abs(dY))  # Start where the derivative is maximum
    x0 = X[ix]
  }

  # Initialize Algorithm:
  X0 <- ifelse(x0==0,
               1e3*absTol,
               x0*(1+1e3*relTol)+sign(x0)*1e3*absTol)
  X1 <- x0
  k  <- 0
  dx <- (range[2]-range[1])/n

  # Iterate:
  while((abs(X1-X0)>max(absTol,relTol*max(abs(X0),abs(X1)))) & (k<iterMax)){
    # Update X0:
    X0 <- X1

    # Estimate f0:
    f0 <- funcToEval(X0)

    # Estimate f0':
    f0_minus <- funcToEval(X0-dx)
    f0_plus  <- funcToEval(X0+dx)
    df0      <- (f0_plus-f0_minus)/(2*dx)

    # Estimate X1:
    X1 = X0 - f0/df0

    # Update k:
    k  = k+1
  }

  # Output:
  return(X1)
}
#' newtonRaphson.Vector
#'
#' @description
#' @param X
#' @param Y
#' @param Ysol Default: 0
#' @param x0 Default: FALSE
#' @param relTol Default: 1e-06
#' @param absTol Default: 1e-09
#' @param eps Default: 1e-06
#' @param iterMax Default: 1000
#' @return
#' @export
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
#' @family General Functions
newtonRaphson.Vector <- function(
  X,                # Vector X of where the value of the function f are known
  Y,                # Vector Y such as Y=f(X)
  Ysol    = 0,      # The value such as "newtonRaphson.Vector" returns the solution Xsol of Ysol=f(Xsol)
  x0      = FALSE,  # Initial value for the algorithm. If not provided, it will analyze the derivative to find the optimum start point.
  relTol  = 1e-6,   # Relative tolerance of the solution
  absTol  = 1e-9,   # Absolute tolerance of the solution
  eps     = 1e-6,   # Step to use to estimate derivative
  iterMax = 1000    # Maximum number of iteration
) {

  # Length of Y:
  n <- length(Y)

  # Check that X has the same length:
  if (length(X)!=n) {stop("X & Y have different length. Please Check.")}

  # Increasing or Decreasing:
  if (Y[1]<=Y[n]){Increase=TRUE} else{Increase=FALSE}

  # First Check if Ysol is in the range of Y: i.e. min(Y)<=Ysol<=max(Y)
  if (Ysol>=max(Y)){
    warning("Ysol is greater than the maximum value in Y. Returns Xsol such as Ymax=f(Xsol)")
    if (Increase) {
      ix <- which.max(rev(Y))
      ix <- length(Y)-ix+1
    } else {
      ix <- which.max(Y)
    }
    return(X[ix])
  }
  if (Ysol<=min(Y)){
    warning("Ysol is smaller than the minimum value in Y. Returns Xsol such as Ymin=f(Xsol)")
    if (Increase) {
      ix <- which.min(Y)
    } else {
      ix <- which.min(rev(Y))
      ix <- length(Y)-ix+1
    }
    ix <- which.min(Y)
    return(X[ix])
  }

  # Change Y such as the equation becomes g(Xsol)=0, ehere g(x)=f(x)-Ysol
  Ynorm <- Y - Ysol

  # Define X0 & X1
  if (is.numeric(x0)){
    X0 <- ifelse(x0==0,
                 1e3*absTol,
                 x0*(1+1e3*relTol)+sign(x0)*1e3*absTol)
    X1 <- x0
  } else{
    # Estimate the derivative of the function f at X to find the optimum starting point:
    dY    <- numeric(n)
    dY[1] <- (Y[2]-Y[1])/(X[2]-X[1])
    dY[n] <- (Y[n]-Y[n-1])/(X[n]-X[n-1])
    for (i in 2:(n-1)){
      dY[i] <- (Y[i+1]-Y[i-1])/(X[i+1]-X[i-1])
    }

    # Initialize Algorithm:
    ix <- which.max(abs(dY))  # Start where the derivative is maximum
    X0 <- ifelse(X[ix]==0,
                 1e3*absTol,
                 X[ix]*(1+1e3*relTol)+sign(X[ix])*1e3*absTol)
    X1 <- X[ix]
  }

  k <- 0
  while((abs(X1-X0)>max(absTol,relTol*max(abs(X0),abs(X1)))) & (k<iterMax)){
    # Update X0:
    X0 <- X1

    # Estimate f0:
    res <- approx(x=X, y=Ynorm, xout=X0, rule=2)
    f0  <- res$y

    # Estimate f0':
    if ((X0-eps)>=min(X)){
      if ((X0+eps)<=max(X)){
        res      <- approx(x=X, y=Ynorm, xout=X0-eps, rule=2)
        f0_minus <- res$y
        res      <- approx(x=X, y=Ynorm, xout=X0+eps, rule=2)
        f0_plus  <- res$y
        df0      <- (f0_plus-f0_minus)/(2*eps)
      }
      else{
        res      <- approx(x=X, y=Ynorm, xout=X0-eps, rule=2)
        f0_minus <- res$y
        df0      <- (f0-f0_minus)/eps
      }
    }else if ((X0+eps)<=max(X)){
      res      <- approx(x=X, y=Ynorm, xout=X0+eps, rule=2)
      f0_plus  <- res$y
      df0      <- (f0_plus-f0)/(eps)
    }else{
      res      <- approx(x=X, y=Ynorm, xout=min(X), rule=2)
      f0_minus <- res$y
      res      <- approx(x=X, y=Ynorm, xout=max(X), rule=2)
      f0_plus  <- res$y
      df0      <- (f0_plus-f0_minus)/(max(X)-min(X))
    }

    # Estimate X1:
    X1 <- X0 - f0/df0
    k  <- k+1
  }

  return(X1)
}
