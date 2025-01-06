
#' Find Minimum
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
#' legend("topleft",legend=c("NoInterpolation","CubicSpline","Quadratic"),pch=rep(16,3),col=c("blue","green","red"))
#'
#' @export
#' @family General Functions
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
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


