#' Convert Unit
#'
#' Automate the conversion of unit in a data frame.
#'
#' @param data data.frame where to convert value
#' @param UNIT_OUT Desired unit for output
#' @param colUNIT Name of the column unit (Default: \code{"UNIT"})
#' @param colVALUE Name of the column value (Default: \code{"VALUE"})
#' @param conversionTable Conversion table - a data.frame with the following character columns:
#' * UNIT_IN
#' * UNIT_OUT
#' * FORMULA 
#' * COMMENT
#' If \code{NULL} (default), the internal conversion table of the MMVbase 
#' package will be used.
#'
#' @return data.frame identical to \code{data} but with the \code{colVALUE} adjust to the desired output unit as defined in \code{UNIT_OUT}.
#'
#' @examples
#' dat <- data.frame(TIME  = c( 0, 1, 2, 4, 8),
#'                   VALUE = c(10, 6, 4, 3, 2),
#'                   UNIT  = "ng/mL",
#'                   stringsAsFactors = FALSE)
#'  dat <- convert_Unit(data     = dat,
#'                      UNIT_OUT = c("ug/mL"))
#'
#' @export
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
#' @family General Functions
convert_Unit <- function(data,
                         UNIT_OUT,
                         colUNIT        = "UNIT",
                         colVALUE       = "VALUE",
                         conversionTable = NULL) {
  

  # Load conversion table from package data if not provided
  if (is.null(conversionTable)) {
    if (exists("conversionTable", envir = asNamespace("MMVbase"))) {
      conversionTable <- get("conversionTable", envir = asNamespace("MMVbase"))
    } else {
      stop("Conversion table not found in package data.")
    }
  }

  FLAGfactor <- FALSE
  if(is.factor(data[[colUNIT]])){
    FLAGfactor <-  TRUE
    data[[colUNIT]] <-  as.character(data[[colUNIT]])
  }
  
  # Loop on Unit Out:
  for(UNIT_OUT_k in UNIT_OUT){
    conversionTable_k <- conversionTable[conversionTable[["UNIT_OUT"]] == UNIT_OUT_k,]
    if(nrow(conversionTable_k)==0){
      message("Unit '", UNIT_OUT_k, "' not defined in 'conversionFile': Please adjust your conversion CSV file")
      
    }else{
      for(UNIT_IN_j in unique(conversionTable_k[["UNIT_IN"]])){
        conversionTable_kj <- conversionTable_k[conversionTable_k[["UNIT_IN"]] == UNIT_IN_j,]
        if(nrow(conversionTable_kj)==0){
          message("Unit '", UNIT_IN_j, "' not defined with unit '",UNIT_OUT_k,"' in 'conversionFile': Please adjust your conversion CSV file")
        }else if(nrow(conversionTable_kj)>1){
          stop("Unit '", UNIT_IN_j, "' is defined multiple times with '",UNIT_OUT_k,"' in 'conversionFile': Please adjust your conversion CSV file")
        }else{
          idx_kj <- data[[colUNIT]] == UNIT_IN_j
          data[[colUNIT]][idx_kj]   <- UNIT_OUT_k
          
          Formula_kj <- conversionTable_kj[["FORMULA"]]
          Formula_kj <- gsub(" "   , ""                          , Formula_kj, fixed = TRUE)
          Formula_kj <- gsub("X_out="   , ""                     , Formula_kj, fixed = TRUE)
          Formula_kj <- gsub("X_in", "data[[colVALUE]][idx_kj]"  , Formula_kj, fixed = TRUE)
          data[[colVALUE]][idx_kj] <- eval(parse(text=Formula_kj))
        }
      }
    }
  }
  if(FLAGfactor){
    data[[colUNIT]] <-  as.factor(data[[colUNIT]])
  }
  
  # Output:
  data
}

