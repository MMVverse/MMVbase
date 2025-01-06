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
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
#' @family General Functions
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
