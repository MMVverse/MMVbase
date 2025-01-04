# Functions to manipulate data in vectors or data frames.


#' Reduce character to the non-repeating entries
#'
#' Entries which are the same as the preceding one are replaced by empty character.
#' The inverse of insert_duplicates().
#'
#' @param x character vector.
#' @return character of the same length as x.
#'
#' @export
#' @family Data
remove_duplicates <- function(x) {

  x <- as.character(x)

  N <- length(x)
  if (N == 1) return(x)

  is_same_as_preceding <- x[2:N] == x[1:(N-1)]
  x[2:N][is_same_as_preceding] <- ""

  x

}



