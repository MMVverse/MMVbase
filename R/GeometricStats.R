#' Geometric mean of a positive numeric vector
#'
#' @param x a numeric vector of positive values. If negative values are present, 
#' a warning is issued and NA_real_ is returned.
#' @param na.rm logical indicating if NA values should be removed during the calculation. 
#' If FALSE (the default) and NA values are present in x, then NA_real_ is returned. 
#' @return The geometric mean of x if all values ofor NA_real_if x contains non-positive values. 
#' @export
#' 
#' @examples 
#' 
#' x <- c(1, 2, 3, 4, 5)
#' GeometricMean(x)
#' GeometricMean(c(1, 2, 3, 4, 5, -1)) # issues a warning and returns NA_real_
#' GeometricMean(c(1, 2, 3, 4, 5, NA)) # returns NA_real_
#' GeometricMean(c(1, 2, 3, 4, 5, NA), na.rm = TRUE) # returns 2.605171
#' GeometricMean(c(1, 2, 3, 4, 5, NA, 0)) # issues a warning and returns NA_real_
#' GeometricMean(c(1, 2, 3, 4, 5, NA, 0), na.rm = TRUE) # issues a warning and returns NA_real_
#' 
#' @author Venelin Mitov (IntiQuan)
GeometricMean <- function(x, na.rm = FALSE) {
  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector")
  }
  if (na.rm) {
    x <- x[!is.na(x)]
  } else if (isTRUE(any(is.na(x)))) {
    if (isTRUE(any(x <= 0))) {
      warning("Non-positive values in 'x'")
    }
    return(NA_real_)
  }
  if (isTRUE(any(x <= 0))) {
    warning("Non-positive values in 'x'")
    return(NA_real_)
  }
  return(exp(mean(log(x))))
}

#' Geometric standard deviation of a positive numeric vector
#'
#' This function calculates the geometric standard deviation of a numeric vector
#' containing only positive values. If non-positive values are present in the vector,
#' a warning is issued and NA_real_ is returned.
#'
#' @param x A numeric vector of positive values.
#' @param na.rm Logical indicating whether to remove missing values from x.
#'   If na.rm=FALSE (the default) and x contains missing values, then a missing
#'   value (NA_real_) is returned. If na.rm=TRUE, missing values are removed from x
#'   prior to computing the geometric standard deviation.
#' @param sqrt.unbiased Logical specifying what method to use to compute
#'   the sample standard deviation of the log-transformed observations. If
#'   sqrt.unbiased=TRUE (the default), the square root of the unbiased estimator
#'   of variance is used. Otherwise, the method of moments estimator of standard
#'   deviation is used. See the DETAILS section for more information.
#'
#' @return The geometric standard deviation of the input vector.
#'
#' @details The geometric standard deviation is a measure of the spread or dispersion
#' of a set of positive values. It is calculated as the exponential of the standard
#' deviation of the logarithm of the values. The geometric standard deviation is
#' useful when dealing with data that follows a log-normal distribution.
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' GeometricSD(x)
#'
#' y <- c(10, 100, 1000, 10000)
#' GeometricSD(y)
#'
#' z <- c(0.5, 1, 2, 4, 8)
#' GeometricSD(z)
#'
#' w <- c(1, 2, NA, 4, 5)
#' GeometricSD(w, na.rm = TRUE)
#'
#' v <- c(1, 2, 3, 4, 5)
#' GeometricSD(v, sqrt.unbiased = FALSE)
#'
#' u <- c(1, 2, 3, 4, 5)
#' GeometricSD(u, na.rm = TRUE, sqrt.unbiased = FALSE)
#'
#' @author Venelin Mitov (IntiQuan)
#' @export
GeometricSD <- function(x, na.rm = FALSE, sqrt.unbiased = TRUE) {
  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector")
  }
  if (na.rm) {
    x <- x[!is.na(x)]
  } else if (any(is.na(x))) {
    return(NA_real_)
  }
  if (any(x <= 0)) {
    warning("Non-positive values in 'x'")
    return(NA_real_)
  }
  sd.log <- sqrt(stats::var(log(x)))
  if (!sqrt.unbiased) {
    n <- length(x)
    sd.log <- sqrt((n - 1) / n) * sd.log
  }
  exp(sd.log)
}



