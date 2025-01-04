
#' Load RData into a list
#'
#' @param fileName Path to the RData file name.
#'
#' @return List with all object saved in `fileName`
#'
#' @export
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
#' @family Save RData
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


#' saveMMV
#' Save R objects
#'
#' [saveMMV] writes an external representation of R objects to the specified file.
#' It uses the function [base::save]. If 'file' is a path, it will create the directory
#' if not existent unlike [base::save].
#'
#' @param list A character vector containing the names of objects to be saved.
#' @param file The name of the file where the data will be saved.
#'
#' @md
#'
#' @export
#' @seealso [base::save]
#' @family Save RData
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
saveMMV <- function(list,
                    file){

  # Get directory name:
  dirName <- dirname(file)

  # Check if the directory name exists, otherwise create it:
  if (!dir.exists(dirName)){
    dir.create(dirName, recursive = TRUE)
  }

  # save list of object:
  save(list = list,
       file = file)
}
