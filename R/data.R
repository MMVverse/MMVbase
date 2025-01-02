# Functions to manipulate data in vectors or data frames.

#' Load RData into a list
#'
#' @param fileName Path to the RData file name.
#'
#' @return List with all object saved in `fileName`
#'
#' @export
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
#' @family Data
load_RDataAsList <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  variableNames <- ls()[!(ls() %in% c("fileName"))]
  out <- lapply(variableNames,function(x){
    get(x)
  })
  names(out) <- variableNames
  out
}



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

