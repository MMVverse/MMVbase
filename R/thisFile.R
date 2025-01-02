
#' thisFile
#'
#' @description
#' @param fullName Default: FALSE
#' @param pathNormalized Default: FALSE
#' @return
#' @export
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
#' @family General Functions
thisFile <- function(fullName = FALSE, pathNormalized = FALSE) {

  # use commandArgs:
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle  <- "--file="
  match   <- grep(needle, cmdArgs)

  # Test with cmdArgs
  if (length(match) > 0) {
    # Rscript
    thisFileName <- sub(needle, "", cmdArgs[match])

    # Otherwise use sys.frames
  } else {
    # 'source'd via R console
    #return(normalizePath(sys.frames()[[1]]$ofile))
    thisFileName <- sys.frames()[[1]]$ofile
  }

  # if thisFIleName is NULL it is called from the console:
  if (is.null(thisFileName)){
    thisFileName <- "Console"
  }

  # Keep only fiel name if needed:
  #   In that case the path can not be normilized
  if(!fullName){
    split_fileName <- strsplit(thisFileName, split="/")
    thisFileName   <- split_fileName[[1]][length(split_fileName[[1]])]
    pathNormalized <- FALSE
  }

  # Normalized path if needed
  if (pathNormalized){
    thisFileName <- normalizePath(thisFileName)
  }

  return(thisFileName)
}
