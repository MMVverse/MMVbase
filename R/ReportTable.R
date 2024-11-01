#' Generate a ReportTable or add rows to an existing one. 
#' @param table a data.frame or NULL (default: NULL). 
#' @param Style a character string. Default: NA_character_.
#' @param Text a character string. Default: NA_character_.
#' @param ... name value pairs to become columns in the table.
#' 
#' @return a data.frame
#' @importFrom data.table data.table ':=' rbindlist
#' 
#' @author Venelin Mitov
#' @export
#' 
#' @examples 
#' 
#' # Code to put/adapt in the beginning of your script or function definition:
#' envResultTable <- new.env()
#' envResultTable$resultTable <- NULL
#' AppendResult <- function(...) {
#'   envResultTable$resultTable <- CreateOrAppendToReportTable(
#'     envResultTable$resultTable, ...)
#' }
#' AppendParameter <- function(
#'   Parameter, 
#'   Rat = NA_real_, Mouse = NA_real_, Dog = NA_real_, Human = NA_real_, 
#'   Value = NA_real_, Unit = NA_character_) {
#'   
#' AppendResult(
#'   Parameter = Parameter, 
#'   Rat = list(Rat), Mouse = list(Mouse), Dog = list(Dog), Human = list(Human), 
#'   Value = list(Value), Unit = Unit)
#' }
#' 
#' # Results added "on the go" in the implementation of a function or a procedure 
#' # in a script
#' AppendResult(Style = "subtitle", Text = "PK Parameter Summary (plasma based)")
#' 
#' # Parameter values can be both, character or numeric
#' AppendParameter("Selected Prediction Method", Value = "IVIVE", Unit = "method")
#' 
#' CL_plasma_mLminkg <- 20
#' AppendParameter("Total Clearance", Human = CL_plasma_mLminkg, Unit = "mL/min/kg")
#' RenalClearance_H_mLminkg <- 3
#' AppendResult(
#'   Style = "comment", 
#'   Text = "Hepatic clearance obtained by substracting renal from total plasma clearanc")
#' AppendParameter(
#'   "Hepatic Clearance", 
#'   Human = CL_plasma_mLminkg-RenalClearance_H_mLminkg, Unit = "mL/min/kg")
#' 
#' # At the end of the function return the accumulated result table or assign it 
#' # to a variable to use later:
#' resultTable <- envResultTable$resultTable
#' 
#' # Getting a predicted parameter value from the result table
#' Human_CL_plasma_mLminkg <- get_FromTable(
#'   resultTable, "Hepatic Clearance", "Human")
CreateOrAppendToReportTable <- function(table = NULL,
                                        Style = NA_character_,
                                        Text = NA_character_,
                                        ...) {
  
  #  cat("CreateOrAppendToReportTable:", toString(...), "\n")
  args <- c(as.list(environment()), list(...))
  
  # Needed to prevent CRAN check note
  IncludeInReport <- NULL

  res <- do.call(data.table::data.table, args[-1])
  if(!"IncludeInReport" %in% names(res)) {
    res[, IncludeInReport := TRUE]
  }
  res <- data.table::rbindlist(list(args[[1]], res), use.names = TRUE, fill = TRUE)
  res
}

#' Convert a reportTable to a list of data.frames corresponding to non-NA 
#' entries in the Style column
#' 
#' @param table a data.frame with a character column named Style.
#' @return a list of data.tables
#' @importFrom data.table data.table
#' 
#' @author Venelin Mitov
#' 
#' @export
ConvertReportTableToList <- function(table) {
  
  chunkStart <- which(!is.na(table$Style))
  chunkStart <- sort(unique(c(chunkStart, chunkStart[chunkStart < nrow(table)] + 1)))
  
  if(length(chunkStart) == 0) {
    list(table)
  } else {
    lapply(seq_along(chunkStart), function(i) {
      beginChunk <- chunkStart[i]
      endChunk <- if(i < length(chunkStart)) {
        chunkStart[i + 1] 
      } else {
        nrow(table) + 1
      }
      tablePart <- table[seq(beginChunk, endChunk - 1), ]
      
      # We want to simplify list columns to vectors of the same type, e.g. numeric - 
      # this enables proper formatting if the table is to be exported in Excel. 
      # Howerver, we need to be careful not to call unlist on a list column that has
      # NULL values in it. 
      tablePart <- do.call(data.table, lapply(tablePart, function(column) {
        if(is.list(column)) {
          unlistedColumn <- unlist(column)
          if(length(unlistedColumn) == length(column)) {
            unlistedColumn
          } else {
            column
          }
        } else {
          column
        }
      }))
      tablePart
    })
  }
}

#' Extract a value for a parameter or a constant from a data.frame such as a report table 
#' 
#' 
#' @param table a data.frame having a Parameter and, at least, a column named as the 
#' \code{Column} parameter.
#' @param Parameter a character string denoting a Parameter name or a regular 
#' expression to be searched for in \code{table$Parameter}.
#' @param Column a character string (default: "Value") indicating the column 
#' for which the value to be returned once a row matching 
#' \code{'Parameter'} is found. 
#' @param ValueIfNotFound a value to return if Parameter not found in table.
#' @param UnitIfNotFound a character string to return if Parameter not found in table.
#' @param FLAG_USE_GREPL logical (default: FALSE) indicating if \code{\link{grepl}} 
#' should be used instead of \code{\link{match}} for 
#' finding 
#' @param ... additional arguments passed to \code{\link{grepl}} if FLAG_USE_GREPL is TRUE.
#' 
#' @return If a matching row for Parameter was found, the Value with added attribute 
#' 'row' containing the matching row, otherwize \code{ValueIfNotFound} also with 
#' added attribute 'row'.
#' @importFrom data.table setnames
#' 
#' @author Venelin Mitov
#' 
#' @examples
#' # Assume we have a report table called 'resultTable'
#' resultTable <- data.frame(
#'   Parameter = c("Selected Prediction Method", "Total Clearance", "Hepatic Clearance"),
#'   Human = c("IVIVE", 20, 17),
#'   Unit = c("method", "mL/min/kg", "mL/min/kg")
#' )
#' 
#' # Get the value for the 'Total Clearance' parameter for 'Human'
#' totalClearance <- get_FromTable(resultTable, "Total Clearance", "Human")
#' print(totalClearance)
#' @export
get_FromTable <- function(table, 
                          Parameter, Column = "Value", 
                          ValueIfNotFound = NULL, 
                          UnitIfNotFound = NULL, 
                          FLAG_USE_GREPL = FALSE,
                          ...) {
  
  stopifnot(is.data.frame(table))
  
  if(!isTRUE(all(c("Parameter", Column, "Unit") %in% names(table)))) {
    warning("get_FromTable: table does not have a column ", Column, ". Returning NA for Parameter ", Parameter, ".")
    row <- CreateOrAppendToReportTable(Parameter = Parameter, Value = ValueIfNotFound, Unit = UnitIfNotFound)
    if(Column != "Value") {
      data.table::setnames(row, old = "Value", new = Column)
    }
    
  } else {
    if(FLAG_USE_GREPL) {
      matchedParameters <- grepl(Parameter, table$Parameter, ...)
      indexRow <- which(matchedParameters)[1]
    } else {
      indexRow <- match(Parameter, table$Parameter)
    }
    if(is.na(indexRow)) {
      args <- list(Parameter = Parameter, Value = ValueIfNotFound, Unit = UnitIfNotFound)
      names(args) <- c("Parameter", Column, "Unit")
      row <- CreateOrAppendToReportTable(Parameter = Parameter, Value = ValueIfNotFound, Unit = UnitIfNotFound)
      if(Column != "Value") {
        data.table::setnames(row, old = "Value", new = Column)
      }
    } else {
      row <- table[indexRow, ]
    }
  }
  
  structure(unlist(row[[Column]], recursive = FALSE), row = row)
}


#' Print a report table in rmakrdown format
#' 
#' @param reportTable a data.frame - a report table
#' 
#' @return nothing. This function only produces output to the standard output device
#' 
#' @export
PrintReportTableAsRmd <- function(resultTable) {
  #browser()
  stopifnot(is.data.frame(resultTable))
  
  if("IncludeInReport" %in% names(resultTable)) {
    resultTable <- resultTable[IncludeInReport == TRUE,]
  }
  
  # Put the comments ending with ':' to a column Comment in the following row of the table
  resultList <- MMVbase::ConvertReportTableToList(resultTable)
  i <- 1
  runningComment <- NULL
  while(i <= length(resultList)){
    r <- resultList[[i]]
    if(all(is.na(r$Style))) {
      # r is a chunk of the table containing parameter values
      if(!is.null(runningComment)) {
        r[1, Comment := runningComment]
        resultList[[i]] <- r
        runningComment <- NULL
      }
      i <- i + 1
    } else if(r$Style == "comment" && endsWith(r$Text, ":")) {
      runningComment <- gsub(":$", "", r$Text, perl = TRUE)
      # This will delete the row from the list, hence we must not increment i
      resultList[[i]] <- NULL
    } else {
      i <- i+1
    }
  }
  
  for(r in resultList) {
    if(is.null(r)) {
      next
    } else if(all(is.na(r$Style))) {
      colnames <- c("Parameter", "Rat", "Mouse", "Dog", "Human", "Value", "Unit")
      if("Comment" %in% names(r)) {
        colnames <- c(colnames, "Comment")
      }
      print(knitr::kable(r[, colnames, with = FALSE], 
                         digits = 2,
                         format = "markdown"))
    } else {
      #print(r$Style)
      if(r$Style == "title") {
        cat("###", r$Text)
      } else if(r$Style == "subtitle") {
        cat("####", r$Text)
      } else if(r$Style == "comment") {
        cat(r$Text, "\n")
      } else if(r$Style == "minorcomment") {
        cat(r$Text, "\n\n")
      } else {
        stop("Unknown style", r$Style)
      }
      
    }
  }
}
