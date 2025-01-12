
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
#' @param CenterList list of center name and number
#' 
#' @return IQRdataGENERAL object
#' @export
#' @seealso [list_Center], [check_dataGenetalMMV_Center]
#' @family Data Preparation
#' @author Mohammed H. Cherkaoui (MMV), \email{cherkaouim@@mmv.org}
check_dataGeneralMMV <- function(dataGeneral, CenterList = NULL) {
  
  if(is.null(CenterList)) {
    # Check Centers:
    dataGeneral <- check_dataGeneralMMV_Center(dataGeneral, CenterList)
  }
  
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
#' @param CenterList list of center name and number
#' 
#' @return IQRdataGENERAL object
#' 
#' @seealso [list_Center]
#' @family Data Preparation
#' @author Mohammed H. Cherkaoui (MMV), \email{cherkaouim@@mmv.org}
check_dataGeneralMMV_Center <- function(dataGeneral, CenterList) {
  
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

#' swapName_MMVnameToName
#' Swaps from MMV name for compound for nick name of interest
#'
#' Swaps the MMV name for the nick name of interest within `data` based on `CompoundList`.
#' If `data` is a IQRdataGENERAL object, MMV name for compound will also be swapped in IQRdataGENERAL object attributes.
#'
#' @md
#'
#' @param data IQRdataGENERAL object, data frame or character vector with MMV name for compound
#' @param CompoundList list with both MMV and nick name for the compound
#'
#' @return `data` object with nick name for compound
#' @export
#' @family Data Preparation
#' @author To be defined
swapName_MMVnameToName <- function(data,
                                   CompoundList){

  # If IQRdataGENERAL class:
  if(is_IQRdataGENERAL(data)){
    data <- within(data,
                   {
                     # Loop of the list of compound to check:
                     for(k in 1:length(CompoundList)){
                       if(exists("COMPOUND")){
                         COMPOUND <- gsub(CompoundList[[k]]$MMVname,
                                          CompoundList[[k]]$Name,
                                          COMPOUND)
                       }
                       if(exists("TRTNAME")){
                         TRTNAME <- gsub(CompoundList[[k]]$MMVname,
                                         CompoundList[[k]]$Name,
                                         TRTNAME)
                       }
                       if(exists("TRTNAMER")){
                         TRTNAMER <- gsub(CompoundList[[k]]$MMVname,
                                          CompoundList[[k]]$Name,
                                          TRTNAMER)
                       }
                       if(exists("NAME")){
                         NAME <- gsub(CompoundList[[k]]$MMVname,
                                      CompoundList[[k]]$Name,
                                      NAME)
                       }
                     }

                     # Remove the column k:
                     rm(k)
                   }, keepAttrs = FALSE)

    # Modify attributes:
    for(k in 1:length(CompoundList)){
      attributes(data)$doseNAMES    <- gsub(CompoundList[[k]]$MMVname,
                                            CompoundList[[k]]$Name,
                                            attributes(data)$doseNAMES)
      attributes(data)$obsNAMES     <- gsub(CompoundList[[k]]$MMVname,
                                            CompoundList[[k]]$Name,
                                            attributes(data)$obsNAMES)
      attributes(data)$covInfo$NAME <- gsub(CompoundList[[k]]$MMVname,
                                            CompoundList[[k]]$Name,
                                            attributes(data)$covInfo$NAME)

      # Change everything but study:
      idx_Study <- which(attributes(data)$catInfo$COLNAME %in% c("STUDY","STUDYN"))
      for(i in 1:length(length(attributes(data)$catInfo$COLNAME))){
        if (length(idx_Study)==0 || !(i %in% idx_Study)){
          attributes(data)$catInfo$VALUETXT[i] <- gsub(CompoundList[[k]]$MMVname,
                                                       CompoundList[[k]]$Name,
                                                       attributes(data)$catInfo$VALUETXT[i])
        }
      }
    }


    # If data frame:
  }else if(is.data.frame(data)){
    data <- within(data,
                   {
                     # Loop of the list of compound to check:
                     for(k in 1:length(CompoundList)){
                       if(exists("COMPOUND")){
                         COMPOUND <- gsub(CompoundList[[k]]$MMVname,
                                          CompoundList[[k]]$Name,
                                          COMPOUND)
                       }
                       if(exists("TRTNAME")){
                         TRTNAME <- gsub(CompoundList[[k]]$MMVname,
                                         CompoundList[[k]]$Name,
                                         TRTNAME)
                       }
                       if(exists("TRTNAMER")){
                         TRTNAMER <- gsub(CompoundList[[k]]$MMVname,
                                          CompoundList[[k]]$Name, TRTNAMER)
                       }
                       if(exists("NAME")){
                         NAME <- gsub(CompoundList[[k]]$MMVname,
                                      CompoundList[[k]]$Name,
                                      NAME)
                       }
                     }

                     # Remove the column k:
                     rm(k)
                   }, keepAttrs = FALSE)

    # If vector:
  }else if(is.vector(data)){
    # Loop of the list of compound to check:
    for(k in 1:length(CompoundList)){
      data <- gsub(CompoundList[[k]]$MMVname,
                   CompoundList[[k]]$Name,
                   data)
    }


  }else{
    stop("Only dataGeneral, dataFrame and Vector can be used within 'swapName_MMVnameToName'.")
  }

  return(data)
}


#' swapName_NameToMMVname
#' Swaps from nick name of interest for compound for MMV name
#'
#' Swaps the nick name of interest for the MMV name within `data` based on `CompoundList`.
#' If `data` is a IQRdataGENERAL object, nick name of interest for compound will also be swapped in IQRdataGENERAL object attributes.
#'
#' @md
#'
#' @param data IQRdataGENERAL object, data frame or character vector with nick name of interest for compound
#' @param CompoundList list with both MMV and nick name for the compound
#'
#' @return `data` object with MMV name for compound
#' @export
#' @family Data Preparation
#' @author To be defined
swapName_NameToMMVname <- function(data,
                                   CompoundList){


  # If IQRdataGENERAL class:
  if (is_IQRdataGENERAL(data)){
    data <- within(data,
                   {
                     # Loop of the list of compound to check:
                     for (k in 1:length(CompoundList)){
                       if(exists("COMPOUND")){
                         COMPOUND <- gsub(CompoundList[[k]]$Name,
                                          CompoundList[[k]]$MMVname,
                                          COMPOUND)
                       }
                       if(exists("TRTNAME")){
                         TRTNAME <- gsub(CompoundList[[k]]$Name,
                                         CompoundList[[k]]$MMVname,
                                         TRTNAME)
                       }
                       if(exists("TRTNAMER")){
                         TRTNAMER <- gsub(CompoundList[[k]]$MMVname,
                                          CompoundList[[k]]$Name,
                                          TRTNAMER)
                       }
                       if(exists("NAME")){
                         NAME <- gsub(CompoundList[[k]]$Name,
                                      CompoundList[[k]]$MMVname,
                                      NAME)
                       }
                     }

                     # Remove the column k:
                     rm(k)
                   },
                   keepAttrs = FALSE)

    # Modify attributes:
    for (k in 1:length(CompoundList)){
      attributes(data)$doseNAMES       <- gsub(CompoundList[[k]]$Name,
                                               CompoundList[[k]]$MMVname,
                                               attributes(data)$doseNAMES)
      attributes(data)$obsNAMES        <- gsub(CompoundList[[k]]$Name,
                                               CompoundList[[k]]$MMVname,
                                               attributes(data)$obsNAMES)
      attributes(data)$covInfo$MMVname <- gsub(CompoundList[[k]]$Name,
                                               CompoundList[[k]]$MMVname,
                                               attributes(data)$covInfo$MMVname)

      # Change everything but study:
      idx_Study <- which(attributes(data)$catInfo$COLNAME %in% c("STUDY","STUDYN"))
      for (i in 1:length(length(attributes(data)$catInfo$COLNAME))){
        if (length(idx_Study)==0 || !(i %in% idx_Study)){
          attributes(data)$catInfo$VALUETXT[i] <- gsub(CompoundList[[k]]$Name,
                                                       CompoundList[[k]]$MMVname,
                                                       attributes(data)$catInfo$VALUETXT[i])
        }
      }
    }


    # If data frame:
  }else if (is.data.frame(data)){
    data <- within(data,
                   {
                     # Loop of the list of compound to check:
                     for (k in 1:length(CompoundList)){
                       if(exists("COMPOUND")){
                         COMPOUND <- gsub(CompoundList[[k]]$Name,
                                          CompoundList[[k]]$MMVname,
                                          COMPOUND)
                       }
                       if(exists("TRTNAME")){
                         TRTNAME <- gsub(CompoundList[[k]]$Name,
                                         CompoundList[[k]]$MMVname,
                                         TRTNAME)
                       }
                       if(exists("TRTNAMER")) {
                         TRTNAMER <- gsub(CompoundList[[k]]$MMVname,
                                          CompoundList[[k]]$Name,
                                          TRTNAMER)
                       }
                       if(exists("NAME")){
                         NAME <- gsub(CompoundList[[k]]$Name,
                                      CompoundList[[k]]$MMVname,
                                      NAME)
                       }
                     }

                     # Remove the column k:
                     rm(k)
                   },
                   keepAttrs = FALSE)

    # If vector:
  }else if (is.vector(data)){
    # Loop of the list of compound to check:
    for (k in 1:length(CompoundList)){
      data <- gsub(CompoundList[[k]]$Name,
                   CompoundList[[k]]$MMVname,
                   data)
    }


  }else{
    stop("Only dataGeneral, dataFrame and Vector can be used within 'swapName_NameToMMVname'.")
  }

  return(data)
}


#' transform_dataFrame
#'
#' @description
#' @param dataFrame_Wide
#' @param key
#' @param value
#' @param colMaster
#' @param colPattern Default: paste0(".", value)
#' @return
#' @export
#' @author Mohammed H. Cherkaoui (MMV)
#' @family Data Preparation
transform_dataFrame_WideToLong <- function(dataFrame_Wide,
                                           key,
                                           value,
                                           colMaster,
                                           colPattern = paste0(".", value)){

  # Create output:
  dataFrame_Long <- NULL

  # Loop over colPattern:
  for (k in seq_along(value)){
    # Get parameter k of interest:
    value_k      <- value[k]
    colPattern_k <- colPattern[k]

    # Get column index:
    colPattern_Names  <- names(dataFrame_Wide)[(grepl(colPattern_k, names(dataFrame_Wide)) & !(names(dataFrame_Wide) %in% colMaster))]

    # Wide to Long:
    dataFrame_Long_k  <- tidyr::gather(dataFrame_Wide[,c(colMaster, colPattern_Names)] , key="key", value="value", colPattern_Names)
    names(dataFrame_Long_k) <- c(colMaster, key, value_k)
    dataFrame_Long_k[,key] <- gsub(colPattern_k, "", dataFrame_Long_k[,key])

    # Merge Long Data:
    if (is.null(dataFrame_Long)){
      dataFrame_Long <- dataFrame_Long_k
    }else{
      dataFrame_Long <- merge(dataFrame_Long,
                              dataFrame_Long_k)
    }
  }

  # Return output:
  return(dataFrame_Long)
}




#' transform_dataFrame_LongToWide
#'
#' @description
#' @param dataFrame_Long
#' @param key
#' @param value
#' @param colMaster
#' @param patternConst Default: function(x) {
#'    out <- paste0(".", x)
#'}
#' @return
#' @export
#' @author Mohammed H. Cherkaoui (MMV)
#' @family Data Preparation
transform_dataFrame_LongToWide <- function(dataFrame_Long,
                                           key,
                                           value,
                                           colMaster,
                                           patternConst = function(x){
                                             out <- paste0(".", x)
                                           }){

  # Create output:
  dataFrame_Wide <- NULL

  # Loop over colPattern:
  for (k in seq_along(value)){
    # Get parameter k of interest:
    value_k      <- value[k]
    colPattern_k <- patternConst(value_k)

    # Long to Wide:
    dataFrame_Wide_k <- tidyr::spread(dataFrame_Long[,c(colMaster, key, value_k)], key = key, value = value_k)

    # Adjust column names:
    names(dataFrame_Wide_k) <- paste0(names(dataFrame_Wide_k),
                                      ifelse(names(dataFrame_Wide_k) %in% colMaster, "", colPattern_k))

    # Merge Wide Data:
    if (is.null(dataFrame_Wide)){
      dataFrame_Wide <- dataFrame_Wide_k
    }else{
      dataFrame_Wide <- merge(dataFrame_Wide,
                              dataFrame_Wide_k)
    }
  }

  # Re-order the columns:
  #   (1) Count the number of parameter
  n_Par <- (ncol(dataFrame_Wide) - length(colMaster))/length(value)
  #   (2) Generate the appropriate order
  idx_col <- seq_along(colMaster)
  for (k in 1:n_Par){
    idx_col <- c(idx_col,
                 seq(length(colMaster) + k, length(colMaster) + k + n_Par*(length(value)-1), by=n_Par))
  }
  dataFrame_Wide <- dataFrame_Wide[,idx_col]

  # Return output:
  return(dataFrame_Wide)
}

