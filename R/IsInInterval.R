
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
IsInRightOpenInterval <- function(value, low, high) {
  stopifnot(is.numeric(low) && is.numeric(high) && isTRUE(all(low < high)))
  value2 <- as.numeric(value)
  value2 >= low & value2 < high
}