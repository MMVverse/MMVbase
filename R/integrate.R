
# Functions for numerical integration

#' Calculate the integral of a function using the Riemann sum method.
#' 
#' @param x Vector x where the values of the function f(x) are known
#' @param y Vector y such as y=f(x)
#' @param FLAGright Default: FALSE
#' @return The integral of the function f(x) using the Riemann sum method.
#' @export
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
#' @family General Functions
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
#' @return
#'
#' @export
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
#' @family General Functions
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

#' trapzMMV
#'
#' @description
#' @param x
#' @param y
#' @return
#' @export
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
#' @family General Functions
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


#' simpsonMMV
#'
#' @description
#' @param x
#' @param y
#' @param tolInt Default: 1e-09
#' @return
#' @export
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
#' @family General Functions
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