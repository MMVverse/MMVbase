# Functions to manipulate data in vectors or data frames.


#' Format Error Name
#'
#' Converts Error Parameter Names as used in sysFIT to format as used in NLME
#'
#' @param parameterNames Vector of character with the names of the parameters
#'
#' @return \code{parameterNames} with converted format
#'
#' @author Anne Kuemmel (IntiQuan)
#' @family General Functions
aux_formatErrorName <- function(parameterNames) {
  # Converts error parameter names as used in sysfit to format as used in nlme
  idxErr <- grep("OUTPUT", parameterNames)
  for (k in idxErr) {
    parameterNames[k] <- paste0(parameterNames[k], substr(parameterNames[k],7,7))
  }
  parameterNames <- gsub("_sigma_abs", "error_ADD", parameterNames)
  parameterNames <- gsub("_sigma_rel", "error_PROP", parameterNames)
  parameterNames <- gsub("OUTPUT[[:digit:]]", "", parameterNames)
  
  # Output:
  parameterNames
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

#' Add names to percentiles vector
#'
#' @param percentiles Vector with percentiles values.
#'
#' @return The vector `percentiles` with names.
#'
#' @author Mohammed H. Cherkaoui (MMV)
#' @family Data
aux_addNamesToPercentiles <- function(percentiles){
  
  # Check that it is between 0 and 100:
  if(any(percentiles>100) || any(percentiles<0)){
    stop("'percentiles' should be a vector containings only values between 0 and 100: Please adjust.")
  }
  
  # Add Names:
  percentiles        <- sort(percentiles)
  names(percentiles) <- ifelse(percentiles==50,
                               "Median",
                               paste0(percentiles, "th Percentile"))
  
  # OUtput:
  percentiles
}


#' Construct CI percentiles from CI level
#'
#' @param CIlevel Numeric containing the confidence interval in percent to be estimated (Default: 90)
#'
#' @return The vector \code{CI.percentiles} with names.
#'
#' @author Mohammed H. Cherkaoui (MMV)
#' @family Data
aux_constructCIpercentiles <- function(CIlevel){
  
  # Define percentiles of CI:
  if(CIlevel<0 || CIlevel>100){
    stop("'CIlevel' should be between 0 and 100: Please adjust.")
  }else if(CIlevel==0){
    CI.percentile        <- c(50)
    names(CI.percentile) <- c("CI Median")
  }else{
    CI.percentile        <- c(50 - CIlevel/2, 50         , 50 + CIlevel/2)
    names(CI.percentile) <- c("CI Low"      , "CI Median", "CI High"     )
  }
  
  # Output:
  CI.percentile
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
#' @family General Functions
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


#' Create USUBJID
#'
#' Add column \code{USUBJID} to \code{data} based on columns \code{STUDY}, \code{GROUP} and \code{SUBJECT},
#' when preparing IQR data from SCID dataset
#'
#' @param data Dataset for which to create \code{USUBJID}
#'
#' @return \code{data} with extra column \code{USUBJID}
#'
#' @examples
#' data <- data.frame(STUDY   = "StudyID",
#'                    GROUP   = c("G1","G1","G2","G2"),
#'                    SUBJECT = c("M1","M2", "M1", "M2"),
#'                    stringsAsFactors = FALSE)
#' data$USUBJID <- MMVmalaria:::aux_createUSUBJID(data)
#'
#' @export
#' @author Aline Fuchs (MMV), Anne Kuemmel (IntiQuan)
#' @family General Functions
aux_createUSUBJID <- function(data) {
  with(data, {
    
    # Format group with 2 digits
    if (is.character(GROUP)) {
      GROUP <- ifelse(nchar(GROUP) == 1, sprintf("0%s", GROUP), sprintf("%s", GROUP))
    } else {
      GROUP <- sprintf("%02i",GROUP)
    }
    
    # Format Subject with minimum 2 digits
    if (is.character(SUBJECT)) {
      SUBJECT <- ifelse(nchar(SUBJECT) == 1, sprintf("0%s", SUBJECT), sprintf("%s", SUBJECT))
    } else {
      SUBJECT <- sprintf("%02i",SUBJECT)
    }
    
    # Create subject ID nbr
    paste(STUDY,GROUP,SUBJECT,sep="_")
  } )
}


#' Removal of escape characters
#'
#' @param x A character vector.
#' @param escapeChars Escape character to substituted with space (Default: \code{c("\\r\\n", "\\n", "\\r", "\\t")})
#'
#' @return \code{x} where all escape character defined in \code{escapeChars} were replaced by a space.
#'
#' @author Anne Kuemmel (IntiQuan)
#' @export
#' @family General Functions
aux_removeEscapeChar <- function(x, escapeChars = c("\r\n","\n","\r","\t")) {
  # Removal of escape characters
  # Under windows anyway substituted by white space, but not for linux
  for (ek in escapeChars) {
    x <- gsub(ek," ", x, fixed = TRUE)
  }
  
  # Output:
  x
}



#' Customizable Collapse Function
#'
#' Collapses a character vector of any length into a character string separated by
#' \code{collapseSymbole}, except for the last element where \code{andSymbole} is used.
#'
#' @param x Character vector of length \code{n}.
#' @param collapseSymbole Character used as a separator for the \code{n-1} first elements in \code{x} (Default: \code{", "})
#' @param andSymbole Character used as a separator for the last element in \code{x} (Default: \code{" & "}).
#' @param messageEmpty Character to print if \code{x} is empty (Default: \code{NULL}).
#'
#' @return A character string where the elements of \code{x} were collapsed.
#'
#' @examples
#' PMXmember <- c("Aline", "Catalina", "Mohammed", "Nathalie")
#' msg       <- paste0("The PMX group is composed of ", collapseMMV(PMXmember), ".")
#' cat(msg)
#'
#' @export
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
#' @family General Functions, Editing Functions
collapseMMV <- function(x,
                        collapseSymbole = ", ",
                        andSymbole      = " & ",
                        messageEmpty    = NULL){
  text <- character(0)
  if(length(x)==0){
    warning("'x' is of length 0 in the function 'collapseMMV'")
    text <- paste0(messageEmpty)
    
  }else if(length(x)==1){
    text <- x[1]
    
  }else{
    n_x  <- length(x)
    text <- paste0(paste0(x[1:(n_x-1)], collapse = collapseSymbole),
                   andSymbole, x[n_x])
  }
  
  # Output:
  text
}



#' get_fileNameExtension
#' Get the extension of 'filename'
#'
#' @md
#'
#' @param filename Path to a file
#'
#' @return TRUE/FALSE
#' @export
#' @family General Function
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