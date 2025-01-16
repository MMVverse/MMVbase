
#' Get the executing script file name
#'
#' Returns the name of the current script file being executed. It can return either the full path or just the file name.
#'
#' @param fullName Logical. If `TRUE`, returns the full path of the file. If `FALSE`, returns only the file name. Default is `FALSE`.
#' @param pathNormalized Logical. If `TRUE`, returns the normalized path of the file. Default is `FALSE`.
#'
#' @return A character string representing the file name or the full path of the current script.
#'
#' @details
#' The function uses `commandArgs` to determine the file name when the script is run using `Rscript`. If the script is sourced via the R console, it uses `sys.frames` to get the file name. If the function is called from the console, it returns "Console".
#'
#' @examples
#' # Example usage:
#' thisFile()
#' thisFile(fullName = TRUE)
#' thisFile(pathNormalized = TRUE)
#'
#' @export
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
#' @family File
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

#' Checks if filename is an existing file and not a directory 
#'
#' @md
#'
#' @param filename Path to a file
#'
#' @return TRUE/FALSE
#' @export
#' @seealso [base::file.exists], [base::dir.exists]
#' @family File
#' @author Mohammed H. Cherkaoui (MMV)
is.fileMMV <- function(filename){
  out <- (file.exists(filename) && !dir.exists(filename))
  
  return(out)
}


#' Copy a file with creating the destination folder if it does not exist
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
#' @family File
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



#' Get the extension of a filename
#'
#' @md
#'
#' @param filename Path to a file
#'
#' @return TRUE/FALSE
#' @export
#' @family File
#' @author Mohammed H. Cherkaoui (MMV), \email{cherkaouim@@mmv.org}
get_fileNameExtension <- function(filename){
  
  # Split the last part of the path:
  ex <- strsplit(basename(filename), split="\\.")[[1]]
  
  # Get Extension:
  if (length(ex)>1){
    out <- ex[length(ex)]
  }else{
    out <- ""
  }
  
  # Get extension:
  return(out)
}


#' Load RData into a list
#'
#' @param fileName Path to the RData file name.
#'
#' @return List with all object saved in `fileName`
#'
#' @export
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
#' @family File
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


#' Common Sub-Path
#'
#' Identify the common sub-path in the vector of paths \code{x}
#'
#' @param x Vector of paths for which the sub-path needs to be detected
#'
#' @return Character of the common sub-path
#'
#' @examples
#' Paths <- c("C:/Users/Default",
#'            "C:/Users/Public")
#' aux_CommonSubPath(Paths)
#'
#' @export
#'
#' @author Anne Kuemmel (IntiQuan), Nathalie Gobeau (MMV, \email{gobeaun@@mmv.org})
#' @family File
aux_CommonSubPath <- function(x) {
  # Sort the vector:
  x <- sort(x)
  
  # Split the first and last element by path separator:
  d_x <- strsplit(x[c(1,length(x))], "/")
  
  # Search for the first non common element to get the last matching one:
  der_com <- match(FALSE, do.call("==",d_x)) - 1
  
  # If there is no matching element, return an empty vector, else return the common part:
  out <- NULL
  if(der_com==0){
    out <- character(0)
  }else{
    out <- paste0(d_x[[1]][1:der_com], collapse = "/")
  }
  
  # Output:
  out
}


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
#' @family File
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
