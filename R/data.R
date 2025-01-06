
#' Get Row ID to be removed
#'
#' _IXGDFtoRemove
#'
#' @description
#' @param dataGeneral A general dataset to look into
#' @param obNAME Default: 'log(Parasitemia Total)'
#' @param removeType Default: 'ToLastOb'
#' @return
#' @export
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
#' @family General Functions
get_IXGDFtoRemove <- function(dataGeneral,
                              obNAME     = "log(Parasitemia Total)",
                              removeType = "ToLastOb") {
  
  # Reduce dataset to observation of interest:
  dataRegress <- dataGeneral[dataGeneral$NAME==obNAME,]
  
  # Do the right operations:
  if (toupper(removeType)=="TOLASTOB"){
    
    # First detect time of last observation:
    idx_Above <- which(dataRegress$VALUE>dataRegress$LLOQ)
    Tmax      <- max(dataRegress$TIME[idx_Above])
    
    # Get all IXGDF to discard:
    idx_ToRemove <- sapply(unique(dataRegress$USUBJID),
                           function(ID){
                             # Reduce dataset:
                             dataRegress_ID <- dataRegress[dataRegress$USUBJID==ID,]
                             
                             # Discard observations after Tmax:
                             idx_ToRemove_ID <- which(dataRegress_ID$TIME>Tmax)
                             if (length(idx_ToRemove_ID)>0){
                               out <- dataRegress_ID$IXGDF[idx_ToRemove_ID]
                             }else{
                               out <- NULL
                             }
                             
                             # Output:
                             return(out)
                           })
    
    # Unlist:
    idx_ToRemove        <- unlist(idx_ToRemove)
    names(idx_ToRemove) <- NULL
    
    
  }else if(toupper(removeType)=="REMOVECURE"){
    
    # Get all USUBJID:
    idx_ToRemove <- sapply(unique(dataRegress$USUBJID),
                           function(ID){
                             # Reduce dataset:
                             dataRegress_ID <- dataRegress[dataRegress$USUBJID==ID,]
                             
                             # Get index of maximum time:
                             idx_Tmax <- which.max(dataRegress_ID$TIME)
                             
                             # Generate output:
                             if (dataRegress_ID$VALUE[idx_Tmax]<=dataRegress_ID$LLOQ[idx_Tmax]){
                               # Get all indexes for which parasitemia was above LLOQ
                               idx_Above <- which(dataRegress_ID$VALUE>dataRegress_ID$LLOQ)
                               
                               # Get the last time for which a parasitemia was observed:
                               Tlast <- max(dataRegress_ID$TIME[idx_Above])
                               
                               # Get IXGDF between Tlast and Tmax:
                               out <- dataRegress_ID$IXGDF[dataRegress_ID$TIME>Tlast]
                             } else{
                               out <- NULL
                             }
                             
                             # Output:
                             return(out)
                           })
    
    # Unlist:
    idx_ToRemove        <- unlist(idx_ToRemove)
    names(idx_ToRemove) <- NULL
    
    
  }else if(toupper(removeType)=="CURE1WEEK"){
    
    # Get all USUBJID:
    idx_ToRemove <- sapply(unique(dataRegress$USUBJID),
                           function(ID){
                             # Reduce dataset:
                             dataRegress_ID <- dataRegress[dataRegress$USUBJID==ID,]
                             
                             # Get index of maximum time:
                             idx_Tmax <- which.max(dataRegress_ID$TIME)
                             
                             # Generate output:
                             if (dataRegress_ID$VALUE[idx_Tmax]<=dataRegress_ID$LLOQ[idx_Tmax]){
                               # Get all indexes for which parasitemia was above LLOQ
                               idx_Above <- which(dataRegress_ID$VALUE>dataRegress_ID$LLOQ)
                               
                               # Get the last time for which a parasitemia was observed:
                               Tlast <- max(dataRegress_ID$TIME[idx_Above])
                               
                               # Get IXGDF between Tlast and Tmax:
                               out <- dataRegress_ID$IXGDF[dataRegress_ID$TIME>(Tlast+24*7)]
                             } else{
                               out <- NULL
                             }
                             
                             # Output:
                             return(out)
                           })
    
    # Unlist:
    idx_ToRemove        <- unlist(idx_ToRemove)
    names(idx_ToRemove) <- NULL
    
    
  }else{
    idx_ToRemove <- NULL
  }
  
  
  # Output:
  return(idx_ToRemove)
}

#' Convert saved IQR table back to data.frame
#'
#' @param inputFileName Path to text file saved with `IQRtools::IQRoutputTable` to convert back to data.frame
#'
#' @return
#'
#' @export
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
#' @family General Functions
IQRtableToDataFrame <- function(inputFileName) {

  # This function allows to re-generate a dataFrame, title and footer from a table generated
  # using IQRoutputTable.
  #
  # This can be useful, if a table needs to be changed, for example.

  # Load table:
  rawTable <- readLines(inputFileName)

  # Retrieve the information of each line:
  Title   <- FALSE
  Header  <- TRUE
  xTitle  <- NULL
  Footer  <- FALSE
  xFooter <- NULL
  for (k in 1:length(rawTable)){
    # Check if the first line is a title:
    if (k==1 & !grepl("|", rawTable[k], fixed=TRUE, useBytes=TRUE)){
      xTitle <- stringr::str_trim(rawTable[k])
    }

    # Retrieve Data Frame:
    if (grepl("|", rawTable[k], fixed=TRUE, useBytes=TRUE)){
      #   First Header:
      if (Header){
        # Get columns name:
        ColumnsName <- strsplit(rawTable[k], "|", fixed=TRUE, useBytes=TRUE)[[1]]
        ColumnsName <- stringr::str_trim(ColumnsName)

        # Create dataFrame to return:
        dataFrame <- data.frame(matrix(vector(), nrow=0, ncol=length(ColumnsName)),
                                stringsAsFactors = FALSE)
        colnames(dataFrame) <- ColumnsName

        # The rest of the table is not header:
        Header <- FALSE

        #   Then the rest of the data frame:
      }else{
        # Retrieve next line:
        rawTable_k <- strsplit(rawTable[k], "|", fixed=TRUE, useBytes=TRUE)[[1]]
        rawTable_k <- stringr::str_trim(rawTable_k)
        rawTable_k <- as.data.frame(t(rawTable_k), stringsAsFactors = FALSE)
        colnames(rawTable_k) <- ColumnsName

        # Add it to dataFrame:
        dataFrame <- rbind(dataFrame,rawTable_k)
      }


      # Retrieve footer if there is one:
    } else if(!grepl("----", rawTable[k], fixed=TRUE) & !grepl("====", rawTable[k], fixed=TRUE) & rawTable[k]!="" & k!=1){
      if (!Footer){
        xFooter <- rawTable[k]
        Footer  <- TRUE
      } else{
        xFooter <- paste0(xFooter,"<br>",rawTable[k])
      }
    }
  }

  # Adapt First column name, title and footer if needed:
  xTitle           <- gsub("<TT>   ", "", xTitle          , fixed=TRUE, useBytes=TRUE)
  names(dataFrame) <- gsub("<TH>   ", "", names(dataFrame), fixed=TRUE, useBytes=TRUE)
  dataFrame[,1]    <- gsub("<TR>   ", "", dataFrame[,1]   , fixed=TRUE, useBytes=TRUE)
  dataFrame[,1]    <- gsub("<TR>"   , "", dataFrame[,1]   , fixed=TRUE, useBytes=TRUE)
  xFooter          <- gsub("<TF>   ", "", xFooter         , fixed=TRUE, useBytes=TRUE)

  # Output:
  return(list(dataFrame=dataFrame, xTitle=xTitle, xFooter=xFooter))
}


#' Check_MissingDatabyNAME
#'
#' Check for for each variable defined in `NAME`, if it is missing for
#' any subject defined in the column USUBJID  of the dataset `dataMaster`.
#'
#' @md
#'
#' @param dataMaster Master dataset in MMV format
#' @param NAME list of variable to test
#' @param colNAME Column name to use to identify subject (default: "USUBJID")
#'
#' @return dataSummaryMissing, which is a data.frame with the list of missing subject for each variable in `NAME`
#' @export
#' @family Data Preparation
#' @author Mohammed H. Cherkaoui (MMV), \email{cherkaouim@@mmv.org}
Check_MissingDatabyNAME <- function(dataMaster,
                                    NAME,
                                    colNAME = c("USUBJID")){
  # Create environment:
  LocalEnv <- environment()
  
  # List of USUBJID:
  dataUSUBJID <- unique(dataMaster[,colNAME, drop = FALSE])
  plyr::a_ply(NAME,
              .margins = 1,
              .fun = function(NAME_k){
                dataName_k <- unique(subset(dataMaster, NAME==NAME_k)[,c(colNAME, "VALUE", "VALUETXT")])
                FLAGnum <- !all(is.na(as.numeric(dataName_k$VALUE)))
                
                if (FLAGnum){
                  dataName_k <- dataName_k[,c(colNAME, "VALUE")]
                  LocalEnv$dataUSUBJID <- left_join(LocalEnv$dataUSUBJID, dataName_k, by=colNAME)
                  data.table::setnames(LocalEnv$dataUSUBJID, "VALUE", NAME_k)
                  
                }else{
                  dataName_k <- dataName_k[,c(colNAME, "VALUETXT")]
                  LocalEnv$dataUSUBJID <- left_join(LocalEnv$dataUSUBJID, dataName_k, by=colNAME)
                  data.table::setnames(LocalEnv$dataUSUBJID, "VALUETXT", NAME_k)
                }
              })
  
  
  # Missing data:
  dataMissingSummary <- data.frame()
  plyr::a_ply(NAME,
              .margins = 1,
              .fun = function(NAME_k){
                
                dataMissing_k <- dataUSUBJID[is.na(dataUSUBJID[NAME_k]),]
                rownames(dataMissing_k) <- NULL
                
                if (nrow(dataMissing_k)>0){
                  dataMissingSummary_k <- data.frame(NAME    = NAME_k,
                                                     USUBJID = collapseMMV(x = dataMissing_k$USUBJID),
                                                     N       = length(dataMissing_k$USUBJID),
                                                     stringsAsFactors = FALSE)
                }else{
                  dataMissingSummary_k <- data.frame(NAME    = NAME_k,
                                                     USUBJID = "",
                                                     N       = 0,
                                                     stringsAsFactors = FALSE)
                }
                
                LocalEnv$dataMissingSummary <- plyr::rbind.fill(LocalEnv$dataMissingSummary,dataMissingSummary_k)
              })
  
  # Output:
  return(dataMissingSummary)
}


#' check_dataGeneralMMV
#' Checks if CENTER and CENTERNAME is well defined within `dataGeneral`
#'
#' Checks if CENTER and CENTERNAME (number and name of the center conducting the study) is defined within `dataGeneral`, according to MMV's standards.
#'
#' The name and number of the center conducting the study will be checked according to the up-to-date center list, see [list_Center].
#' The [check_dataGenetalMMV_Center] function is used internally.
#'
#' @md
#'
#' @param dataGeneral IQRdataGENERAL object
#'
#' @return IQRdataGENERAL object
#' @export
#' @seealso [list_Center], [check_dataGenetalMMV_Center]
#' @family Data Preparation
#' @author Mohammed H. Cherkaoui (MMV), \email{cherkaouim@@mmv.org}
check_dataGeneralMMV <- function(dataGeneral) {
  
  # Check Centers:
  dataGeneral <- check_dataGeneralMMV_Center(dataGeneral)
  
  # Return Dataset:
  return(dataGeneral)
}

#' check_dataGeneralMMV_Center
#' Checks if CENTER and CENTERNAME is well defined within `dataGeneral`
#'
#' Checks if CENTER and CENTERNAME (number and name of the center conducting the study) is defined within `dataGeneral`, according to MMV's standards.
#'
#' The name and number of the center conducting the study will be checked according to the up-to-date center list, see [list_Center].
#' If both CENTERNAME and CENTER columns exist in `dataGeneral`, CENTERNAME column is taken as the reference to correct CENTER column.
#' If only CENTERNAME column exists in `dataGeneral`, the function will create CENTER column.
#' If only CENTER column exists in `dataGeneral`, the function will create CENTERNAME column.
#'
#' @md
#'
#' @param dataGeneral IQRdataGENERAL object
#'
#' @return IQRdataGENERAL object
#' @export
#' @seealso [list_Center]
#' @family Data Preparation
#' @author Mohammed H. Cherkaoui (MMV), \email{cherkaouim@@mmv.org}
check_dataGeneralMMV_Center <- function(dataGeneral) {
  
  # CASE 1: CENTER and CENTERNAME columns exist
  #   CENTERNAME is taken as the reference to correct CENTER
  if (all(c("CENTER","CENTERNAME") %in% names(dataGeneral))){
    
    # Load Valid Center List:
    CenterList <- list_Center()
    
    if (all(unique(dataGeneral$CENTERNAME) %in% names(CenterList))){
      idx_Center <- match(dataGeneral$CENTERNAME,names(CenterList))
      
      # Check if CENTER number are valid:
      if (!all(dataGeneral$CENTER == unlist(CenterList)[idx_Center])){
        # Update CENTER column:
        dataGeneral$CENTER <- unlist(CenterList)[idx_Center]
        warning(cat("The column CENTER was updated.",sep="\n\t"))
      }
    }else{
      stop(cat(cat("One or more CENTERNAME are not valid. They should be defined as one of the following element:",
                   names(CenterList),sep="\n\t"),
               "If new CENTERNAME, please update the function 'list_Center()'",sep="\n"))
    }
    
    # CASE 2: Only CENTERNAME column exists
  } else if (all(c("CENTERNAME") %in% names(dataGeneral))){
    
    # Load Valid Center List:
    CenterList <- list_Center()
    
    if (all(unique(dataGeneral$CENTERNAME) %in% names(CenterList))){
      idx_Center <- match(dataGeneral$CENTERNAME,names(CenterList))
      
      # Create CENTER column:
      dataGeneral$CENTER <- unlist(CenterList)[idx_Center]
      
      # Check if CENTER number are valid:
      if (!all(dataGeneral$CENTER == unlist(CenterList)[idx_Center])){
        idx_WrongCenter = which(dataGeneral$CENTER != unlist(CenterList)[idx_Center])
        stop(cat("At least one CENTERNAME has a wrong CENTER value. Check for CENTERNAME:",
                 unique(dataGeneral$CENTERNAME[idx_WrongCenter]),sep="\n\t"))
      }
    }else{
      stop(cat(cat("One or more CENTERNAME are not valid. They should be defined as one of the following element:",
                   names(CenterList),sep="\n\t"),
               "If new CENTERNAME, please update the function 'list_Center()'",sep="\n"))
    }
    
    # CASE 3: Only CENTER column exists
  }else if (all(c("CENTER") %in% names(dataGeneral))){
    
    # Load Valid Center List:
    CenterList <- list_Center()
    
    if (all(unique(dataGeneral$CENTER) %in% unlist(CenterList))){
      idx_Center <- match(dataGeneral$CENTER,unlist(CenterList))
      
      # Create CENTERNAME column
      dataGeneral$CENTERNAME <- names(CenterList)[idx_Center]
      
      # Check if CENTER number are valid:
      if (!all(dataGeneral$CENTER == unlist(CenterList)[idx_Center])){
        idx_WrongCenter = which(dataGeneral$CENTER != unlist(CenterList)[idx_Center])
        stop(cat("At least one CENTER has a wrong CENTERNAME value. Check for CENTER:",
                 unique(dataGeneral$CENTER[idx_WrongCenter]),sep="\n\t"))
      }
    }else{
      stop(cat(cat("One or more CENTER code are not valid. They should be defined as one of the following element:",
                   CenterList,sep="\n\t"),
               "If new CENTER code, please update the function 'list_Center()'",sep="\n"))
    }
  }else{
    stop("'CENTER' and/or 'CENTERNAME' are not defined in the dataset: Please add them.")
  }
  
  # Return Dataset:
  return(dataGeneral)
}
