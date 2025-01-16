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
#' @family Statistical
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
#' @family Statistical
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



#' Clopper-Pearson Test
#'
#' Statistical test to estimate the confidence interval of
#' a binomial distribution using the Clopper-Pearson Test
#' (see \href{https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Clopper%E2%80%93Pearson_interval}{Wikipedia})
#'
#' @param k Number of successful observations of a trial
#' @param n Number of observation
#' @param CI Confidence Interval to estimate (Default: \code{0.9})
#'
#' @return Return a vector with the lower \code{CI_low} and high \code{CI_high} limit of the CI.
#'
#' @examples
#' # Probability of success:
#' p <- 0.8
#' # Number of observations:
#' n <- 100
#' # Number of successful observation:
#' k <- rbinom(1,n,p)
#' # Empirical probability of success:
#' p_Empirical <- k/n
#' # 90% confidence interval of the empirical probability of success:
#' CI <- clopperPearsonMMV(k,n,CI=0.9)
#'
#' @export
#' @importFrom stats qbeta
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
#' @family Statistical
clopperPearsonMMV <- function(k,
                              n,
                              CI = 0.90) {
  
  #-----------------------------------------------------------------------------#
  # STEP 0: Some Checks ----
  #-----------------------------------------------------------------------------#
  
  # Check if arguments are numerics:
  if(!is.numeric(k) || !is.numeric(n) || !is.numeric(CI)){
    stop("'k', 'n' and/or 'CI' are not numeric: Please adjust.")
  }
  
  # Check if CI is larger than one:
  if(CI>1){
    CI <- CI/100
    warning("'CI' is larger than 1: It was assumed to be expressed in percent and, therfore, convert to a fraction.")
  }
  
  
  #-----------------------------------------------------------------------------#
  # STEP 1: Estimate CI ----
  #-----------------------------------------------------------------------------#
  
  # Estimate low and high limits using the beta function:
  limits_low <- qbeta(p      = (1-CI)/2,
                      shape1 = k,
                      shape2 = n - k +1)
  
  limits_high <- qbeta(p      = 1 - (1-CI)/2,
                       shape1 = k + 1,
                       shape2 = n - k)
  
  # Generate output:
  limits <- c(limits_low, limits_high)
  names(limits) <- c("CI_low", "CI_high")
  
  # Output:
  limits
}
