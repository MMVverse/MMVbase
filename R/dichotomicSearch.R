#' Dichotomic Search
#'
#' Algorithm to find \code{Xsol} such as \code{f(Xsol)=Ysol}, where the abscissa \code{X} and
#' ordinate \code{Y} are the vectorized representation of the function \code{f}.
#'
#' The algorithm assumes that \code{f} is a monotonic function. It will first find \code{X[k]} and
#' \code{X[k+1]} such as \code{Xsol} is contained in \code{Y[k]-Y[k+1]} if increasing function (or
#' \code{Y[k+1]-Y[k]} if decreasing function). Then it is assumed that the function \code{f} is
#' linear between \code{X[k]} and \code{X[k+1]} to solve \code{f(Xsol)=Ysol}.
#'
#' @param X Vector of the abscissa where \code{f} is evaluated.
#' @param Y Vector of the ordinate where \code{f} is evaluated such as \code{f(X)=Y}.
#' @param Ysol Value of \code{f} for which we want to find \code{Xsol} (Default: 0).
#' @param tol Tolerance of the solution (Default: 1e-09).
#'
#' @return The solution \code{Xsol}
#'
#' @export
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org}), Venelin Mitov (IntiQuan)
#' @family General Functions
dichotomicSearch <- function(
  X,               # Vector X of where the value of the function f are known
  Y,               # Vector Y such as Y=f(X)
  Ysol     = 0,    # The value such as "dichotomicSearch" returns the solution Xsol of Ysol=f(Xsol)
  tol      = 1e-9  # Tolerance of the solution
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
      # ix = which.max(rev(Y))
      # ix = length(Y)-ix+1
      return(max(X))
    } else {
      # ix = which.max(Y)
      return(min(X))
    }
    # return(X[ix])
  }
  if (Ysol<=min(Y)){
    warning("Ysol is smaller than the minimum value in Y. Returns Xsol such as Ymin=f(Xsol)")
    if (Increase) {
      # ix = which.min(Y)
      return(min(X))
    } else {
      # ix = which.min(rev(Y))
      # ix = length(Y)-ix+1
      return(max(X))
    }
    # ix = which.min(Y)
    # return(X[ix])
  }

  # Change Y such as the equation becomes g(Xsol)=0, where g(x)=f(x)-Ysol
  Ynorm <- Y - Ysol

  # Define Starting Points:
  ia <- 1
  ya <- Ynorm[ia]
  ib <- n
  yb <- Ynorm[ib]

  # Dichotomy Algorithm
  while ((ib-ia)>1){
    ic <- floor((ia+ib)/2)
    yc <- Ynorm[ic]
    if (abs(yc)<tol){
      if (yc<0){
        ia <- ic
        ya <- yc
        ib <- ia+1
        yb <- Ynorm[ib]
      } else{
        ib <- ic
        yb <- yc
        ia <- ib-1
        ya <- Ynorm[ia]
      }
    } else if (ya*yc>0){
      ia <- ic
      ya <- yc
    } else{
      ib <- ic
      yb <- yc
    }
  }

  # As we have vectors as input, a linear resolution to refine the solution can help.
  if (ya!=yb){
    Xsol <- (X[ia]*yb-X[ib]*ya)/(yb-ya)
  } else{
    Xsol <- X[ia]
  }

  # Output:
  Xsol
}
