# Needed for testthat::with_mocked_bindings (see ?with_mocked_bindings):
getwd <- NULL

#' Get the directory path of an activity
#'
#' Get activity path to be used in graphs. This allows to keep track of where the
#' figures are located and which script was used to generate them.
#' 
#' @param ActivityPath If the user want to force the activity path to plot. (Default: \code{NULL}).
#'
#' @return A character vector with the activity path. Some examples to understand how the function 
#' behaves:
#' * If no ../tags.RData file exists:
#'   - current directory: /Projects_Discovery/Serie1/Work/CompX/A01_PK/Scripts - activity path: Serie1/Work/CompX/A01_PK
#'   - current directory: /Projects/CompX/Work/A01_PK/Scripts - activity path: CompX/Work/A01_PK
#'   - current directory: /some/other/path - activity path NULL with warning 'The current activity is not in the project folder'.
#' * If file ../tags.RData exists and has entries: itemPath = "/sites/department/ModellingTeam/ProjectX",
#' itemName = "ActivityY", then ProjectX/ActivityY is returned.
#' 
#' @details
#' This function is used by some of the graphical functions in MMVbase. It is 
#' not intended for use in scripts unless the user is working on an MMV activity. 
#' 
#' @export
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org}), 
#'   Venelin Mitov (IntiQuan, \email{venelin.mitov@@intiquan.com})
#' @md
#' @family Utility
get_ActivityPath <- function(ActivityPath = NULL) {

  # Define Activity Path for caption:
  if (is.null(ActivityPath)) {
    # ActivityPath if on PiNK
    if(file.exists("../tags.RData")){
      # prevent check note
      tags <- NULL
      
      load("../tags.RData")
      ActivityPath <- file.path(gsub("/sites/department/ModellingTeam/","",tags$itemPath),
                                tags$itemName)

    # ActivityPath if on S:/M&S
    }else{
      # Get Current Working Directory:
      WorkDir       <- getwd()
      WorkDir_Split <- strsplit(WorkDir, "/", fixed = TRUE)[[1]]
      n_str         <- length(WorkDir_Split)

      if (n_str>5 && WorkDir_Split[n_str-4]!="Projects" && WorkDir_Split[n_str-5]=="Projects_Discovery"){

        # Define Serie:
        SerieName  <- WorkDir_Split[n_str-4]

        # Define Project Name:
        ProjectName <- WorkDir_Split[n_str-2]

        # Define ActivityName if NULL:
        ActivityName  <- WorkDir_Split[n_str-1]

        # Define Activity Path:
        ActivityPath <- file.path(SerieName,"Work", ProjectName,  ActivityName)

      }else if(n_str>4 && WorkDir_Split[n_str-4]=="Projects"){
        # Define Project Name:
        ProjectName <- WorkDir_Split[n_str-3]

        # Define ActivityName if NULL:
        ActivityName  <- WorkDir_Split[n_str-1]

        # Define Activity Path:
        ActivityPath <- file.path(ProjectName, "Work", ActivityName)
      } else{
        warning("The current activity is not in the project folder and 'ActivityPath' is set to NULL, therefore, it is not automatically generated\nYou can manually enter a value for 'ActivityPath'")
        ActivityPath <- "NULL"
      }
    }
  }

  # Output:
  ActivityPath
}



#' Format Error Name
#'
#' Converts Error Parameter Names as used in sysFIT to format as used in NLME
#'
#' @param parameterNames Vector of character with the names of the parameters
#'
#' @return \code{parameterNames} with converted format
#'
#' @author Anne Kuemmel (IntiQuan)
#' @export
#' @family Utility
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
#' @family Utility
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
#' @family Utility
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

#' Removal of escape characters
#'
#' @param x A character vector.
#' @param escapeChars Escape character to substituted with space (Default: \code{c("\\r\\n", "\\n", "\\r", "\\t")})
#'
#' @return \code{x} where all escape character defined in \code{escapeChars} were replaced by a space.
#'
#' @author Anne Kuemmel (IntiQuan)
#' @export
#' @family Utility
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
#' @family Utility
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



