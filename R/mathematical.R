######################################################################
# Functions for numerical intervals ----------------------------------
######################################################################

#' Check if value is in the closed interval \code{[low, high]}
#'
#' @param value numeric.
#' @param low numeric.
#' @param high numeric.
#'
#' @return TRUE or FALSE. If any of the arguments is not numeric or low <= high is not true an 
#' error is raised. 
#' @export
#' 
#' @author Venelin Mitov (IntiQuan)
#' 
#' @examples
#' IsInClosedInterval(2, 1, 3)
#' IsInClosedInterval(2, 1, 2)
#' IsInClosedInterval(2, 1, 1.5)
#' 
#' @family Mathematical
IsInClosedInterval <- function(value, low, high) {
  stopifnot(is.numeric(low) && is.numeric(high) && isTRUE(all(low <= high)))
  value2 <- as.numeric(value)
  value2 >= low & value2 <= high
}

#' Check if value is in the open interval \code{(low, high)}
#'
#' @inheritParams IsInClosedInterval
#'
#' @return TRUE or FALSE. If any of the arguments is not numeric or low < high is not true an 
#' error is raised. 
#' @export
#' 
#' @author Venelin Mitov (IntiQuan)
#'
#' @examples
#' IsInClosedInterval(2, 1, 3)
#' IsInClosedInterval(2, 1, 2)
#' IsInClosedInterval(2, 1, 1.5)
#' error <- try(IsInClosedInterval(2, 1.5, 1), silent = TRUE)
#' 
#' @family Mathematical
IsInOpenInterval <- function(value, low, high) {
  stopifnot(is.numeric(low) && is.numeric(high) && isTRUE(all(low < high)))
  value2 <- as.numeric(value)
  value2 > low & value2 < high
}


#' Check if value is in the left open interval \code{(low, high]}
#'
#' @inheritParams IsInClosedInterval
#'
#' @return TRUE or FALSE. If any of the arguments is not numeric or low < high is not true an 
#' error is raised. 
#' @export
#' 
#' @author Venelin Mitov (IntiQuan)
#'
#' @examples
#' IsInLeftOpenInterval(2, 1, 3)
#' IsInLeftOpenInterval(2, 1, 2)
#' IsInLeftOpenInterval(2, 1, 1.5)
#' error <- try(IsInLeftOpenInterval(2, 1.5, 1), silent = TRUE)
#' 
#' @family Mathematical
IsInLeftOpenInterval <- function(value, low, high) {
  stopifnot(is.numeric(low) && is.numeric(high) && isTRUE(all(low < high)))
  value2 <- as.numeric(value)
  value2 > low & value2 <= high
}

#' Check if value is in the right open interval \code{[low, high)}
#'
#' @inheritParams IsInClosedInterval
#'
#' @return TRUE or FALSE. If any of the arguments is not numeric or low < high is not true an 
#' error is raised. 
#' @export
#' 
#' @author Venelin Mitov (IntiQuan)
#'
#' @examples
#' IsInRightOpenInterval(2, 1, 3)
#' IsInRightOpenInterval(2, 1, 2)
#' IsInRightOpenInterval(2, 1, 1.5)
#' error <- try(IsInRightOpenInterval(2, 1.5, 1), silent = TRUE)
#' 
#' @family Mathematical
IsInRightOpenInterval <- function(value, low, high) {
  stopifnot(is.numeric(low) && is.numeric(high) && isTRUE(all(low < high)))
  value2 <- as.numeric(value)
  value2 >= low & value2 < high
}

###########################################################
# Functions for numerical integration ------------------------------------------
###########################################################

#' Calculate the integral of a function using the Riemann sum method.
#' 
#' @param x Vector x where the values of the function f(x) are known
#' @param y Vector y such as y=f(x)
#' @param FLAGright Logical; if `TRUE`, the right Riemann sum is used. If `FALSE`, 
#' the left Riemann sum is used. Default is `FALSE`.
#'
#' @return A numeric value representing the integral of the function over the given points.
#'
#' @details The function calculates the integral of a function using the Riemann sum method.
#' It requires that the vectors `x` and `y` have the same length and contain at least two 
#' points. The user can choose between the right Riemann sum and the left Riemann sum by 
#' setting the `FLAGright` parameter.
#'
#' @examples
#' x <- c(0, 1, 2, 3, 4)
#' y <- c(0, 1, 4, 9, 16)
#' rectintMMV(x, y)
#' rectintMMV(x, y, FLAGright = TRUE)
#' @export
#' 
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
#' @family Mathematical
rectintMMV <- function(x,               
                       y,                
                       FLAGright = FALSE
) {

  # Length of X & Y:
  nX <- length(x)
  nY <- length(y)

  # Check that X & Y have the same length:
  if (nX!=nY) {stop("X & Y have different length. Please Check.")}

  # Check that the length is at least 2:
  if (nX<2) {stop("X & Y should be of length 2 minimum.")}

  # Estimate Integrale
  #   Right Riemann sum
  if(FLAGright){
    Integral <- sum(y[2:nY]*(x[2:nX]-x[1:(nX-1)]))

  #   Left Riemann sum
  }else{
    Integral <- sum(y[1:(nY-1)]*(x[2:nX]-x[1:(nX-1)]))
  }


  # Output:
  return(Integral)
}


#' log-Linear Trapez Integration Method
#'
#' @description This function estimates the integral of a function using the trapeze method. 
#' It uses the log-lin rule when the function is decreasing.
#' 
#' @param x Numerical vector such as y=f(x)
#' @param y Numerical vector such as y=f(x)
#' @param logCrieria Criteria to use the log-lin rule (Default: 0.99)
#'
#' @return A numeric value representing the integral of the function over the given points.
#'
#' @details The function calculates the integral of a function using the trapezoidal rule. 
#' If the function is decreasing and the y-values meet the logCrieria, it uses the log-linear 
#' rule for integration. It requires that the vectors `x` and `y` have the same length and 
#' contain at least two points.
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' y <- c(10, 8, 6, 4, 2)
#' logLinTrapzMMV(x, y)
#'
#' @export
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
#' @family Mathematical
logLinTrapzMMV <- function(
  x,
  y,
  logCrieria = 0.99){

  # Length of X & Y:
  nX <- length(x)
  nY <- length(y)

  # Check that X & Y have the same length:
  if (nX!=nY) {stop("X & Y have different length. Please Check.")}

  # Check that the length is at least 2:
  if (nX<2) {stop("X & Y should be of length 2 minimum.")}

  # Estimate Integrale:
  #   Test if the logLin Trapeze method should be used
  idx_Test <- ((y[2:nY]<logCrieria*y[1:(nY-1)]) & y[1:(nY-1)]>0 & y[2:nY]>0)
  #   Integral
  if (any(idx_Test)){
    Integral <- sum(ifelse(idx_Test,
                           (y[1:(nY-1)] - y[2:nY])/(log(y[1:(nY-1)])-log(y[2:nY]))*(x[2:nX]-x[1:(nX-1)]),
                           (y[1:(nY-1)] + y[2:nY])/2                              *(x[2:nX]-x[1:(nX-1)])))
  }else{
    Integral <- sum((y[1:(nY-1)] + y[2:nY])*(x[2:nX]-x[1:(nX-1)]))/2
  }

  # Output:
  return(Integral)
}

#' Trapezoidal Integration
#'
#' This function performs numerical integration using the trapezoidal rule.
#'
#' @param x A numeric vector of x-values where the function values are known.
#' @param y A numeric vector of y-values corresponding to the function values at the x-values.
#'
#' @return A numeric value representing the integral of the function over the given points.
#'
#' @details The function calculates the integral of a function using the trapezoidal rule. 
#' It requires that the vectors `x` and `y` have the same length and contain at least two points.
#'
#' @examples
#' x <- c(0, 1, 2, 3, 4)
#' y <- c(0, 1, 4, 9, 16)
#' trapzMMV(x, y)
#'
#' @export
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
#' @family Mathematical
trapzMMV <- function(x,               # Vector x of where the value of the function f are known
                     y                # Vector y such as y=f(x)
) {

  # Length of X & Y:
  nX <- length(x)
  nY <- length(y)

  # Check that X & Y have the same length:
  if (nX!=nY) {stop("X & Y have different length. Please Check.")}

  # Check that the length is at least 2:
  if (nX<2) {stop("X & Y should be of length 2 minimum.")}

  # Estimate Integrale
  Integral <- sum((y[1:(nY-1)] + y[2:nY])*(x[2:nX]-x[1:(nX-1)]))/2

  # Output:
  return(Integral)
}


#' Simpson's Rule Integration
#'
#' This function performs numerical integration using Simpson's rule. If the number of
#'  points is less than three, it falls back to the trapezoidal rule.
#'
#' @param x A numeric vector of x-values where the function values are known.
#' @param y A numeric vector of y-values corresponding to the function values at the x-values.
#' @param tolInt A numeric value specifying the tolerance to check if the step size is similar 
#' among the intervals. Default is 1e-09.
#'
#' @return A numeric value representing the integral of the function over the given points.
#'
#' @details The function calculates the integral of a function using Simpson's rule. If the 
#' number of points is less than three, it uses the trapezoidal rule. It requires that the
#' vectors `x` and `y` have the same length and contain at least two points. If the step sizes 
#' are not consistent, it adapts the Simpson's rule accordingly.
#'
#' @examples
#' x <- c(0, 1, 2, 3, 4)
#' y <- c(0, 1, 4, 9, 16)
#' simpsonMMV(x, y)
#'
#' @export
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
#' @family Mathematical
simpsonMMV <- function(
  x,               # Vector x of where the value of the function f are known
  y,               # Vector y such as y=f(x)
  tolInt = 1e-9    # Tolerance to use to check if the step size is similar among the interval
) {

  # Length of X & Y:
  nX <- length(x)
  nY <- length(y)

  # Check that X & Y have the same length:
  if (nX!=nY) {stop("X & Y have different length. Please Check.")}

  if (nX<2){
    warning("Length of X & Y less than two: Integral set to 0.")
    Integral <- 0

  }else if(nX==2){
    # Trapez Method:
    Integral <- (x[2] - x[1])*(y[1] + y[2])/2

  }else if(nX==3){
    # Check if the integration step dx have same length:
    if ((abs(x[1] + x[3] -2*x[2])<(x[2]-x[1])*tolInt)){
      # Simpson Method:
      Integral <- (x[3] - x[1])*(y[1] + 4*y[2] + y[3])/6

    }else{
      # Trapez Method:
      Integral <- sum((y[1:(nY-1)] + y[2:nY])*(x[2:nX]-x[1:(nX-1)]))/2
    }

  }else{
    # First Check if all sub-interval of x have similar length:
    dx   <- x[2:nX]-x[1:(nX-1)]
    TEST <- (abs(dx-dx[1])<dx[1]*tolInt)

    # Initialize with first interval using the trapez method:
    Integral <- (x[2]-x[1])*(y[1]+y[2])/2

    if (all(TEST)){
      # Simpson Method simplified for constant dx step:
      Integral <- Integral + sum((x[3:nX] - x[1:(nX-2)])*(y[1:(nX-2)] + 4*y[2:(nX-1)] + y[3:nX]))/6

    }else{
      # Simpson Method by starting with the first point:
      #   Here, as the points are not equally distant, the formula
      #   had to be adapted to account for it.

      # Some constants:
      Cak <- y[1:(nX-2)]/(x[1:(nX-2)] - x[2:(nX-1)])/(x[1:(nX-2)] - x[3:nX]    )
      Cmk <- y[2:(nX-1)]/(x[2:(nX-1)] - x[1:(nX-2)])/(x[2:(nX-1)] - x[3:nX]    )
      Cbk <- y[3:nX]    /(x[3:nX]     - x[1:(nX-2)])/(x[3:nX]     - x[2:(nX-1)])

      # More constants:
      Ak <-   Cak                         + Cmk                         + Cbk
      Bk <- -(Cak*(x[2:(nX-1)] + x[3:nX]) + Cmk*(x[1:(nX-2)] + x[3:nX]) + Cbk*(x[1:(nX-2)] + x[2:(nX-1)]))
      Ck <-   Cak*(x[2:(nX-1)] * x[3:nX]) + Cmk*(x[1:(nX-2)] * x[3:nX]) + Cbk*(x[1:(nX-2)] * x[2:(nX-1)])

      # Integral:
      Integral <- Integral + sum(Ak/3*(x[3:nX]^3 - x[1:(nX-2)]^3) + Bk/2*(x[3:nX]^2 - x[1:(nX-2)]^2) + Ck*(x[3:nX] - x[1:(nX-2)]))
    }

    # Add last interval by the trapez method:
    Integral <- Integral + (x[nX]-x[nX-1])*(y[nX-1]+y[nX])/2

    # Each Interval was counted twice:
    Integral <- Integral/2
  }

  # Output:
  return(Integral)
}


##################################################################
# Newton-Raphson Method for Finding Roots -----
##################################################################
#' Find the root of of y=f(x) within an interval 
#'
#' This function uses the Newton-Raphson method to find the root of a given function.
#'
#' @param funcToEval A function to evaluate for the equation.
#' @param Ysol A numeric value representing the solution such that the function returns the solution Xsol of Ysol = f(Xsol). Default is 0.
#' @param x0 Initial value for the algorithm. If not provided, it will analyze the derivative to find the optimum start point. Default is \code{NULL}.
#' @param range A numeric vector of two scalars that determine the range of the search. Default is c(-10, 10).
#' @param relTol Relative tolerance of the solution. Default is 1e-06.
#' @param absTol Absolute tolerance of the solution. Default is 1e-09.
#' @param n Number of intervals to estimate the derivative. Default is 1000.
#' @param iterMax Maximum number of iterations. Default is 1000.
#'
#' @return A numeric value representing the root of the function.
#'
#' @details The function uses the Newton-Raphson method to find the root of a given function. If the initial value `x0` is not provided, the function will analyze the derivative to find the optimum start point within the specified `range`. The algorithm iterates until the relative or absolute tolerance is met or the maximum number of iterations is reached.
#'
#' @examples
#' # Define a function
#' f <- function(x) { x^2 - 4 }
#' # Find the root using the Newton-Raphson method
#' newtonRaphson.Function(f)
#'
#' @export
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
#' @family Mathematical
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

#' Newton-Raphson Method for Finding Roots in a Vector
#'
#' This function uses the Newton-Raphson method to find the root of a given function represented by vectors of x and y values.
#'
#' @param X A numeric vector of x-values where the function values are known.
#' @param Y A numeric vector of y-values corresponding to the function values at the x-values.
#' @param Ysol A numeric value representing the solution such that the function returns the solution Xsol of Ysol = f(Xsol). Default is 0.
#' @param x0 Initial value for the algorithm. If not provided, it will analyze the derivative to find the optimum start point. Default is \code{FALSE}.
#' @param relTol Relative tolerance of the solution. Default is 1e-06.
#' @param absTol Absolute tolerance of the solution. Default is 1e-09.
#' @param eps Step to use to estimate the derivative. Default is 1e-06.
#' @param iterMax Maximum number of iterations. Default is 1000.
#'
#' @return A numeric value representing the root of the function.
#'
#' @details The function uses the Newton-Raphson method to find the root of a given function represented by vectors of x and y values. If the initial value `x0` is not provided, the function will analyze the derivative to find the optimum start point. The algorithm iterates until the relative or absolute tolerance is met or the maximum number of iterations is reached.
#'
#' @examples
#' # Define vectors X and Y
#' X <- c(0, 1, 2, 3, 4)
#' Y <- c(0, 1, 4, 9, 16)
#' # Find the root using the Newton-Raphson method
#' newtonRaphson.Vector(X, Y)
#'
#' @export
#' @importFrom stats approx
#'
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
#' @family Mathematical
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

#####################################################################
# Find minimum ----
#####################################################################

#' Find minimum of a continuous function (specified by x,y values) by interpolation
#'
#' Finds the minimum of \code{Y(X)} by interpolation with one of the 3 methods: no interpolation, cubic spline or quadratic.
#' Returns the minimum value of \code{Y} (\code{Ymin}) and the value of \code{X} when \code{Y} is minimum (\code{Xmin})
#'
#' @param X numeric vector
#' @param Y numeric vector, same length as X
#' @param dx numeric value, refined interpolation interval in case of the cubic spline interpolation
#' @param Method interpolation method to choose from  \code{"NoInterpolation"}, \code{"CubicSpline"} or \code{"Quadratic"} (Default: \code{"Quadratic"})
#'
#' @return Data frame with \code{Xmin} anf \code{Ymin}: \code{data.frame(Xmin=Xmin,Ymin=Ymin)}
#'
#' @examples
#' X  = c(1,2,3,4,5,6,7)
#' Y  = c(10,4,1,1,6,10,20)
#' Y1 = find_MinMMV(X,Y,Method = "NoInterpolation")
#' Y2 = find_MinMMV(X,Y,Method = "CubicSpline")
#' Y3 = find_MinMMV(X,Y,Method = "Quadratic")
#' plot(X,Y,type="b",ylim=c(0,20))
#' points(Y1,pch=16,col="blue")
#' points(Y2,pch=16,col="green")
#' points(Y3,pch=16,col="red")
#' legend("topleft",legend=c("NoInterpolation","CubicSpline","Quadratic"),
#'        pch=rep(16,3),col=c("blue","green","red"))
#'
#' @export
#' @importFrom pracma cubicspline
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
#' 
#' @family Mathematical
find_MinMMV <- function(X,
                        Y,
                        dx = as.numeric((X[2]-X[1])/1000),
                        Method = "Quadratic") {

  #-----------------------------------------------------------------------------#
  # STEP 1: Check Input Variables ----
  #-----------------------------------------------------------------------------#

  # Check if X and Y have same length:
  if (length(X)!=length(Y)){
    stop("'X' and 'Y' have different lengths. Please adjust the vectors.")
  }

  # Check if Method is valid:
  if (!(tolower(Method) %in% c("nointerpolation", "cubicspline", "quadratic"))){
    stop("'Method' is not a valid method. Please choose between 'NoInterpolation', 'CubicSpline', 'Quadratic', or implement a new method.")
  }


  #-----------------------------------------------------------------------------#
  # STEP 2: Find MIN ----
  #-----------------------------------------------------------------------------#

  # Get index of the minimum value in Y:
  idxMIN <- which.min(Y)

  # If no interpolation:
  if (tolower(Method)=="nointerpolation"){
    Xmin   <- X[idxMIN]
    Ymin   <- Y[idxMIN]


    # Cubic Spline:
  } else if(tolower(Method)=="cubicspline"){

    # If MIN is first or last point:
    if (idxMIN==1 | idxMIN==length(Y)) {
      Xmin   <- X[idxMIN]
      Ymin   <- Y[idxMIN]

      # Otherwise:
    } else {

      # Use 5 or 3 points depending on where idxMIN is:
      if ((idxMIN==length(Y)-1) | (idxMIN==2)){
        # (x;y) points to use for the spline:
        X0 <- X[c(idxMIN-1, idxMIN, idxMIN+1)]
        Y0 <- Y[c(idxMIN-1, idxMIN, idxMIN+1)]

      }else{
        # (x;y) points to use for the spline:
        X0 <- X[c(idxMIN-2, idxMIN-1, idxMIN, idxMIN+1, idxMIN+2)]
        Y0 <- Y[c(idxMIN-2, idxMIN-1, idxMIN, idxMIN+1, idxMIN+2)]
      }

      # X points to use for the spline interpolation:
      Xs <- seq(X[idxMIN-1], X[idxMIN+1], dx)

      # Spline Cubic:
      Ys <- pracma::cubicspline(X0, Y0, Xs)

      # Find minimum:
      idxMINs <- which.min(Ys)
      Xmin   <- Xs[idxMINs]
      Ymin   <- Ys[idxMINs]
    }


    # Quadratic:
  } else if(tolower(Method)=="quadratic"){


    # If MIN is first or last point:
    if (idxMIN==1 | idxMIN==length(Y)) {
      Xmin   <- X[idxMIN]
      Ymin   <- Y[idxMIN]

      # Otherwise:
    } else{
      # FIRST ---
      # Interpolation order 2 to find the "real" minimum:

      # Points for the regression:
      #   Parasitemia before the observed minimum
      X1 <- X[idxMIN-1]
      Y1 <- Y[idxMIN-1]
      #   Parasitemia at observed minimum
      X2 <- X[idxMIN]
      Y2 <- Y[idxMIN]
      #   Parasitemia after the observed minimum
      X3 <- X[idxMIN+1]
      Y3 <- Y[idxMIN+1]

      # Constants of interest to estimate the polynome constants:
      A1 <- Y1/((X1-X2)*(X1-X3))
      A2 <- Y2/((X2-X1)*(X2-X3))
      A3 <- Y3/((X3-X1)*(X3-X2))

      # Constant of the polynom:
      a <- A1 + A2 + A3
      b <- -(A1*(X2+X3) + A2*(X1+X3) + A3*(X1+X2))
      c <- A1*X2*X3 + A2*X1*X3 + A3*X1*X2

      # Minimum Time:
      Xmin <- -b/(2*a)
      Ymin <- a*Xmin^2 + b*Xmin + c
    }
  }


  #Genereate Ouput:
  PointMIN <- data.frame(Xmin = Xmin,
                         Ymin = Ymin,
                         stringsAsFactors = TRUE)

  # Return:
  return(PointMIN)
}


