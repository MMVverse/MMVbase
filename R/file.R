
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

#' is.fileMMV
#' Checks if 'filename' is a file
#'
#' @md
#'
#' @param filename Path to a file
#'
#' @return TRUE/FALSE
#' @export
#' @seealso [base::file.exists], [base::dir.exists]
#' @family General Function
#' @author Mohammed H. Cherkaoui (MMV)

is.fileMMV <- function(filename){
  out <- (file.exists(filename) && !dir.exists(filename))
  
  return(out)
}


#' File Manipulation
#'
#' [file.copyMMV] works in a similar way to [base::file.copy] but automatically create
#' the destination folder if needed. Copying to existing destination files is skipped
#' unless \code{overwrite = TRUE}. The to argument can specify a single existing directory.
#' If \code{copy.mode = TRUE} file read/write/execute permissions are copied where possible,
#' restricted by \code{'umask'}. (On Windows this applies only to files.) Other security
#' attributes such as ACLs are not copied. On a POSIX filesystem the targets of symbolic
#' links will be copied rather than the links themselves, and hard links are copied separately.
#' Using \code{copy.date = TRUE} may or may not copy the timestamp exactly (for example, fractional
#' seconds may be omitted), but is more likely to do so as from R 3.4.0.
#'
#' @param from Character vectors, containing file names or paths.
#' @param to Character vectors, containing file names or paths.
#' @param overwrite Logical; should existing destination files be overwritten?
#' @param recursive Logical. If to is a directory, should directories in from be copied (and their contents)? (Like cp -R on POSIX OSes.)
#' @param copy.mode Logical: should file permission bits be copied where possible?
#' @param copy.date Logical: should file dates be preserved where possible? See Sys.setFileTime.
#'
#' @md
#'
#' @export
#' @seealso [base::file.copy]
#' @family General Functions
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
file.copyMMV <- function(from,
                         to,
                         overwrite = recursive,
                         recursive = FALSE,
                         copy.mode = TRUE,
                         copy.date = FALSE){
  
  # Get directory name of output:
  if (is.fileMMV(from)){
    dirName <- dirname(to)
  } else{
    dirName <- to
  }
  
  # Check if the directory name exists, otherwise create it:
  if (!dir.exists(dirName)){
    dir.create(dirName, recursive = TRUE)
  }
  
  # Copy file(s):
  if (is.fileMMV(from)){
    file.copy(from = from,
              to   = to,
              overwrite = overwrite,
              recursive = recursive,
              copy.mode = copy.mode,
              copy.date = copy.date)
  } else{
    sapply(as.vector(dir(from)),
           FUN = function(from_k){
             dirName_k <- ifelse(dirname(from_k)==".",
                                 "",
                                 dirname(from_k))
             file.copy(from = file.path(from, from_k),
                       to   = file.path(to  , dirName_k),
                       overwrite = overwrite,
                       recursive = recursive,
                       copy.mode = copy.mode,
                       copy.date = copy.date)
           })
  }
  
}
