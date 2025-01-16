#' Create USUBJID
#'
#' Add column \code{USUBJID} to \code{data} based on columns \code{STUDY}, \code{GROUP}, and \code{SUBJECT},
#' when preparing IQR data from SCID dataset.
#'
#' @param data A data frame containing the columns \code{STUDY}, \code{GROUP}, and \code{SUBJECT}.
#'
#' @return A character vector that can be assigned as USUBJID column to data.
#'
#' @details The function creates a unique subject identifier (\code{USUBJID}) by concatenating the values
#' of the \code{STUDY}, \code{GROUP}, and \code{SUBJECT} columns. The \code{GROUP} and \code{SUBJECT} values
#' are formatted to have at least two digits.
#'
#' @examples
#' data <- data.frame(STUDY   = "StudyID",
#'                    GROUP   = c("G1","G1","G2","G2"),
#'                    SUBJECT = c("M1","M2", "M1", "M2"),
#'                    stringsAsFactors = FALSE)
#' data$USUBJID <- aux_createUSUBJID(data)
#'
#' @export
#' @author Aline Fuchs (MMV), Anne Kuemmel (IntiQuan)
#' @family Data
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

#' Get IXGDF indices of observations below LLOQ to remove
#'
#' This function identifies and returns the indices of observations to remove from a dataset
#' based on the specified observation name and removal type.
#'
#' @param dataGeneral An IQRdataGeneral dataset or a data.frame containing at least the following 
#' columns: IXGDF,USUBJID,TIME,NAME,VALUE,LLOQ columns.
#' @param obNAME A character string specifying the name of the observation of interest. 
#' Default is `"log(Parasitemia Total)"`.
#' @param removeType A character string specifying the type of removal operation. Options are:
#'  
#' - `"ToLastOb"`: Finds the last time-point where at least one subject has value above LLOQ. 
#' Removes all observations after this time point for all subjects.  
#' - `"RemoveCure"`: For each subject, removes observations after the last time point where the value 
#' is above the LLOQ, if the value at the last time point is below or equal to the LLOQ.
#' - `"Cure1Week"`: For each subject, removes observations after one week from the last time point where the 
#' value is above the LLOQ, if the value at the maximum time point is below or equal to the LLOQ.
#'
#' @return A vector of IXGDF indices of the observations to remove.
#'
#' @details This function was written with malaria parasitemia measurements in mind, but can be used
#' to trim measurements below LLOQ in other cases.
#' 
#' @examples
#' 
#' data <- data.frame(
#'   USUBJID = rep(1:3, each = 6),
#'   NAME = rep("log(Parasitemia Total)", 18),
#'   TIME = rep((1:6)*72, 3),
#'   VALUE = c(10, 4, 3, 4, 6, 5, 12, 10, 8, 5, 4, 3, 14, 3, 3, 3, 3, 3),
#'   LLOQ = rep(5, 18),
#'   IXGDF = 1:18
#'   )
#' get_IXGDFtoRemove(data, obNAME = "log(Parasitemia Total)", removeType = "ToLastOb")
#' get_IXGDFtoRemove(data, obNAME = "log(Parasitemia Total)", removeType = "RemoveCure")
#' get_IXGDFtoRemove(data, obNAME = "log(Parasitemia Total)", removeType = "Cure1Week")
#'
#' @export
#' @author Mohammed H. Cherkaoui (MMV), Venelin Mitov (IntiQuan GmbH)
#' @family Data
#' @md
get_IXGDFtoRemove <- function(dataGeneral,
                              obNAME     = "log(Parasitemia Total)",
                              removeType = "ToLastOb") {
  
  # Reduce dataset to observation of interest:
  dataRegress <- dataGeneral[dataGeneral$NAME==obNAME,]
  
  # Do the right operations:
  if (toupper(removeType)=="TOLASTOB"){
    # Detect time of last observation above LLOQ:
    idx_Above <- which(dataRegress$VALUE>dataRegress$LLOQ)
    Tmax      <- max(dataRegress$TIME[idx_Above])
    
    # Get all IXGDF to discard:
    idx_ToRemove <- sapply(unique(dataRegress$USUBJID),
                           function(ID){
                             # Reduce dataset to this ID:
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
                               
                               # Get the last time for which a parasitemia > LLOQ was observed :
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
#' @return a list with the following entries:.
#'  - data.frame: a data.frame
#'  - xTitle: a character string with the title of the table
#'  - xFooter: a character string with the footer of the table
#' 
#' @export
#' @examples
#' 
#' # Create a sample IQRoutputTable text
#' sample_text <- c(
#'   "Sample Table Title",
#'   "Column1|Column2|Column3",
#'   "Row1Col1|Row1Col2|Row1Col3",
#'   "Row2Col1|Row2Col2|Row2Col3",
#'   "Sample Footer"
#' )
#' 
#' # Write the sample text to a file
#' writeLines(sample_text, "sample_table.txt")
#' 
#' # Convert the saved IQR table back to a data.frame
#' result <- IQRtableToDataFrame("sample_table.txt")
#' unlink("sample_table.txt")
#' 
#' # Print the result
#' print(result$dataFrame)
#' print(result$xTitle)
#' print(result$xFooter)
#' 
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
#' @family Data
IQRtableToDataFrame <- function(inputFileName) {

  # This function allows to re-generate a dataFrame, title and footer from a table generated
  # using \code{IQRtools::IQRoutputTable}.
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
#'
#' @examples 
#' 
#' # Create a sample dataset
#' dataMaster <- data.frame(
#'   USUBJID = rep(1:5, each = 3),
#'   NAME = rep(c("Var1", "Var2", "Var3"), 5),
#'   VALUE = c(1, NA, 3, 4, 5, NA, 7, 8, 9, NA, 11, 12, 13, 14, NA),
#'   VALUETXT = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O")
#' )
#' 
#' # Define the list of variables to check
#' NAME <- c("Var1", "Var2", "Var3")#'
#' # Check for missing data
#' Check_MissingDatabyNAME(dataMaster, NAME)
#' 
#' @export
#' 
#' @importFrom dplyr left_join
#' @importFrom plyr a_ply rbind.fill
#' @family Data
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
#' Checks if CENTER and CENTERNAME (number and name of the center conducting the study) is 
#' defined within `dataGeneral`, according to MMV's standards.
#'
#' The name and number of the center conducting the study will be checked according to the 
#' up-to-date center list, see \code{MMVworkflows::list_Center}.
#' The \code{check_dataGeneralMMV_Center} function is used internally.
#'
#' @md
#'
#' @param dataGeneral IQRdataGENERAL object
#' @param CenterList list of center name and number. Default is NULL. For internal MMV projects, 
#' the center list is automatically can be retrieved from the MMVworkflows::list_Center function.
#' 
#' @return IQRdataGENERAL object
#' 
#' @examples
#' 
#' # Create a sample dataGeneral dataset
#' dataGeneral <- data.frame(
#'   USUBJID = rep(1:3, each = 4),
#'   CENTER = c(101, 101, 102, 102, 103, 103, 104, 104, 105, 105, 106, 106),
#'   CENTERNAME = c("Center A", "Center A", "Center B", "Center B", "Center C", 
#'                  "Center C", "Center D", "Center D", "Center E", "Center E", 
#'                  "Center F", "Center F"),
#'   VALUE = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120)
#' )
#' 
#' # Define a mock CenterList
#' CenterList <- list(
#'   "Center A" = 101,
#'   "Center B" = 102,
#'   "Center C" = 103,
#'   "Center D" = 104,
#'   "Center E" = 105,
#'   "Center F" = 106
#' )
#' 
#' # Check the dataGeneral dataset
#' checkedData <- check_dataGeneralMMV(dataGeneral, CenterList)
#' print(checkedData)
#' 
#' @export
#' @seealso \code{\link{check_dataGeneralMMV_Center}}
#' @family Data
#' @author Mohammed H. Cherkaoui (MMV), \email{cherkaouim@@mmv.org}
check_dataGeneralMMV <- function(dataGeneral, CenterList = NULL) {
  
  if(!is.null(CenterList)) {
    # Check Centers:
    dataGeneral <- check_dataGeneralMMV_Center(dataGeneral, CenterList)
  }
  
  # Return Dataset:
  return(dataGeneral)
}

#' check_dataGeneralMMV_Center
#' Checks if CENTER and CENTERNAME is well defined within `dataGeneral`
#'
#' Checks if CENTER and CENTERNAME (number and name of the center conducting the study) is 
#' defined within `dataGeneral`, according to MMV's standards.
#'
#' The name and number of the center conducting the study will be checked according to the 
#' up-to-date center list (for internal MMV use, see \code{MMVworkflows::list_Center}).
#' If both CENTERNAME and CENTER columns exist in `dataGeneral`, CENTERNAME column is taken 
#' as the reference to correct CENTER column.
#' If only CENTERNAME column exists in `dataGeneral`, the function will create CENTER column.
#' If only CENTER column exists in `dataGeneral`, the function will create CENTERNAME column.
#'
#' @md
#'
#' @param dataGeneral IQRdataGENERAL object
#' @param CenterList named list of center numbers (names are center names).
#' 
#' @return IQRdataGENERAL object
#' 
#' @details This function is internal and supposed to only be called from \code{\link{check_dataGeneralMMV}}.
#' 
#' @family Data 
#' @author Mohammed H. Cherkaoui (MMV), \email{cherkaouim@@mmv.org}
check_dataGeneralMMV_Center <- function(dataGeneral, CenterList) {
  
  # CASE 1: CENTER and CENTERNAME columns exist
  #   CENTERNAME is taken as the reference to correct CENTER
  if (all(c("CENTER","CENTERNAME") %in% names(dataGeneral))){
    
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
               "If new CENTERNAME, please supply a valid CenterList argument. ", 
               "You might need to check the function 'MMVworkflows::list_Center()'",sep="\n"))
    }
    
    # CASE 2: Only CENTERNAME column exists
  } else if (all(c("CENTERNAME") %in% names(dataGeneral))){
        
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
               "If new CENTERNAME, please supply a valid CenterList argument. ", 
               "You might need to check the function 'MMVworkflows::list_Center()'",sep="\n"))
    }
    
    # CASE 3: Only CENTER column exists
  }else if (all(c("CENTER") %in% names(dataGeneral))){
    
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
               "If new CENTER code, please supply a valid CenterList argument." ,
               "You might need to check the function 'MMVworkflows::list_Center()'",sep="\n"))
    }
  }else{
    stop("'CENTER' and/or 'CENTERNAME' are not defined in the dataset: Please add them.")
  }
  
  # Return Dataset:
  return(dataGeneral)
}

#' Swaps from MMV name for compound for nick name of interest
#'
#' Swaps the MMV name for the nick name of interest within `data` based on `CompoundList`.
#' If `data` is a IQRdataGENERAL object, MMV name for compound will also be swapped in 
#' IQRdataGENERAL object attributes.
#'
#' @md
#'
#' @param data IQRdataGENERAL object, data frame or character vector with MMV name for compound
#' @param CompoundList list with both MMV and nick name for the compound
#'
#' @return `data` object with nick name for compound
#' 
#' @examples
#' 
#' # Create a mock IQRdataGeneral dataset
#' data <- data.frame(
#'   USUBJID = rep(1:3, each = 4),
#'   COMPOUND = c("MMV123", "MMV123", "MMV456", "MMV456", "MMV789", "MMV789", "MMV101",
#'                "MMV101", "MMV112", "MMV112", "MMV123", "MMV123"),
#'   TRTNAME = c("MMV123 12mg", "MMV123 12mg", "MMV456 20mg", "MMV456 20mg", "MMV789 25mg", 
#'               "MMV789 25mg", "MMV101 30mg", "MMV101 30mg", "MMV112 50mg", "MMV112 50mg", 
#'               "MMV123 50mg", "MMV123 50mg"),
#'   NAME = c("Conc MMV123", "Conc MMV123", "Conc MMV456", "Conc MMV456", "Conc MMV789", 
#'            "Conc MMV789", "Conc MMV101", "Conc MMV101", "Conc MMV112", "Conc MMV112", 
#'            "Conc MMV123", "Conc MMV123"),
#'   VALUE = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120)
#' )
#' 
#' # Define a mock CompoundList
#' CompoundList <- list(
#'   list(MMVname = "MMV123", Name = "CompoundA"),
#'   list(MMVname = "MMV456", Name = "CompoundB"),
#'   list(MMVname = "MMV789", Name = "CompoundC"),
#'   list(MMVname = "MMV101", Name = "CompoundD"),
#'   list(MMVname = "MMV112", Name = "CompoundE")
#' )
#' 
#' # Swap MMV names to nick names
#' swappedData <- swapName_MMVnameToName(data, CompoundList)
#' print(swappedData)
#' 
#' @seealso \code{\link{swapName_NameToMMVname}}
#' 
#' @export
#' @family Data
#' @author Mohammed H. Cherkaoui (MMV), \email{cherkaouim@@mmv.org}, Venelin Mitov (IntiQuan GmbH)
swapName_MMVnameToName <- function(data,
                                   CompoundList){

  # If IQRdataGENERAL class:
  if(methods::is(data, "IQRdataGENERAL")){
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
#' 
#' @examples
#' 
#' # Create a mock IQRdataGENERAL dataset
#' data <- data.frame(
#'   USUBJID = rep(1:3, each = 4),
#'   COMPOUND = c("CompoundA", "CompoundA", "CompoundB", "CompoundB", "CompoundC", 
#'       "CompoundC", "CompoundD", "CompoundD", "CompoundE", "CompoundE", 
#'        "CompoundA", "CompoundA"),
#'   TRTNAME = c("CompoundA 12mg", "CompoundA 12mg", "CompoundB 20mg",
#'           "CompoundB 20mg", "CompoundC 25mg", "CompoundC 25mg", "CompoundD 30mg", 
#'           "CompoundD 30mg", "CompoundE 50mg", "CompoundE 50mg", "CompoundA 50mg", 
#'           "CompoundA 50mg"),
#'   NAME = c("Conc CompoundA", "Conc CompoundA", "Conc CompoundB", "Conc CompoundB", 
#'            "Conc CompoundC", "Conc CompoundC", "Conc CompoundD", "Conc CompoundD", 
#'            "Conc CompoundE", "Conc CompoundE", "Conc CompoundA", "Conc CompoundA"),
#'   VALUE = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120)
#' )
#' 
#' # Define a mock CompoundList
#' CompoundList <- list(
#'   list(MMVname = "MMV123", Name = "CompoundA"),
#'   list(MMVname = "MMV456", Name = "CompoundB"),
#'   list(MMVname = "MMV789", Name = "CompoundC"),
#'   list(MMVname = "MMV101", Name = "CompoundD"),
#'   list(MMVname = "MMV112", Name = "CompoundE")
#' )
#' 
#' # Swap nick names to MMV names
#' swappedData <- swapName_NameToMMVname(data, CompoundList)
#' print(swappedData)
#' 
#' @seealso \code{\link{swapName_MMVnameToName}}
#' 
#' @family Data
#' @author Mohammed H. Cherkaoui (MMV), \email{cherkaouim@@mmv.org}, Venelin Mitov (IntiQuan GmbH)
swapName_NameToMMVname <- function(data,
                                   CompoundList){


  # If IQRdataGENERAL class:
  if (methods::is(data, "IQRdataGENERAL")){
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


#' Transform Data Frame from Wide to Long Format
#'
#' This function transforms a data frame from wide format to long format based on specified 
#' key and value columns.
#'
#' @param dataFrame_Wide A data frame in wide format.
#' @param key A character string specifying the name of the key column in the returned long 
#' format data.frame.
#' @param value A character vector specifying the names of the value columns in the returned 
#' long format data.frame.
#' @param colMaster A character vector specifying the names of the master columns to retain in 
#' the returned long format data.frame.
#' @param colPattern A character vector specifying the patterns to match for the value columns 
#' in the wide format. Default is `paste0(".", value)`.
#'
#' @return A data frame in long format.
#'
#' @details
#' See and run the example to understand what the function does!
#'
#' @examples
#' keyPDParams_Static <- data.frame(
#'    MIC.MEAN = 3.5,
#'    MIC.MIN = 2.5,
#'    MIC.MAX = 4.5,
#'    MIC.N = 10,
#'    MPC90.MEAN = 8.2,
#'    MPC90.MIN = 7.3,
#'    MPC90.MAX = 10.1,
#'    MPC90.N = 10,
#'    EC50.MEAN = 6.3,
#'    EC50.MIN = 4.5,
#'    EC50.MAX = 8.0,
#'    EC50.N = 10,
#'    PRR48.MEAN = 3.2,
#'    PRR48.MIN = 2.5,
#'    PRR48.MAX = 4.3,
#'    PRR48.N = 10
#' )
#' keyPDParams_Dynamic <- keyPDParams_Static * 2
#' 
#' keyPDParams_Static$PARAMETERTYPE <- "Static"
#' keyPDParams_Static$METHODCLASS <- "SCID Modelling"
#' keyPDParams_Static$METHOD <- "EMAX"
#' keyPDParams_Static$NLMETEM <- "In Vivo"
#' keyPDParams_Static$BINDINGCONVERSION <- "-"
#' keyPDParams_Static$DOSE <- "-"
#' 
#' keyPDParams_Dynamic$PARAMETERTYPE <- "Dynamic"
#' keyPDParams_Dynamic$METHODCLASS <- "SCID Modelling"
#' keyPDParams_Dynamic$METHOD <- "EMAX"
#' keyPDParams_Dynamic$NLMETEM <- "In Vivo"
#' keyPDParams_Dynamic$BINDINGCONVERSION <- "-"
#' keyPDParams_Dynamic$DOSE <- "-"
#' 
#' KeyPDpar <- rbind(keyPDParams_Static, keyPDParams_Dynamic)
#' print(KeyPDpar)
#' 
#' KeyPDPar_long <- transform_dataFrame_WideToLong(KeyPDpar,
#'  key = "key",
#'  value = c("MEAN", "MIN", "MAX", "N"),
#'  colMaster = c("PARAMETERTYPE", "METHODCLASS", "METHOD", "NLMETEM", "BINDINGCONVERSION", "DOSE")
#' )
#' print(KeyPDPar_long)
#'
#' @export
#' @importFrom tidyr gather
#' @author Mohammed H. Cherkaoui (MMV), Venelin Mitov (IntiQuan GmbH)
#' @family Data
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


#' Transform Data Frame from Long to Wide Format
#'
#' This function transforms a data frame from long format to wide format based on specified key and value columns.
#'
#' @param dataFrame_Long A data frame in long format.
#' @param key A character string specifying the name of the key column in the long format.
#' @param value A character vector specifying the names of the value columns in the long format.
#' @param colMaster A character vector specifying the names of the master columns to retain in the wide format.
#' @param patternConst A function that generates the pattern for the value columns in the wide format. Default is `function(x) paste0(".", x)`.
#'
#' @return A data frame in wide format.
#'
#' @details
#' See the example to understand what the function does!
#'
#' @examples
#' KeyPDPar_long <- tidyr::tribble(
#'   ~PARAMETERTYPE,      ~METHODCLASS,  ~METHOD,  ~NLMETEM, ~key,   ~MEAN, ~MIN, ~MAX,  ~N,
#'   "Dynamic", "SCID Modelling",   "EMAX", "In Vivo",       "EC50", 12.6,  9.0, 16.0, 20,
#'   "Dynamic", "SCID Modelling",   "EMAX", "In Vivo",        "MIC",  7.0,  5.0,  9.0, 20,
#'   "Dynamic", "SCID Modelling",   "EMAX", "In Vivo",      "MPC90", 16.4, 14.6, 20.2, 20,
#'   "Dynamic", "SCID Modelling",   "EMAX", "In Vivo",      "PRR48",  6.4,  5.0,  8.6, 20,
#'   "Static", "SCID Modelling",   "EMAX", "In Vivo",       "EC50",  6.3,  4.5,  8.0, 10,
#'   "Static", "SCID Modelling",   "EMAX", "In Vivo",        "MIC",  3.5,  2.5,  4.5, 10,
#'   "Static", "SCID Modelling",   "EMAX", "In Vivo",      "MPC90",  8.2,  7.3, 10.1, 10,
#'   "Static", "SCID Modelling",   "EMAX", "In Vivo",      "PRR48",  3.2,  2.5,  4.3, 10
#'   )
#'   
#' print(KeyPDPar_long)
#' 
#' KeyPDPar <- transform_dataFrame_LongToWide(
#'   KeyPDPar_long,
#'   key = "key",
#'   value = c("MEAN", "MIN", "MAX", "N"),
#'   colMaster = c("PARAMETERTYPE", "METHODCLASS", "METHOD", "NLMETEM")
#' )
#' print(KeyPDPar)
#'   
#' @export
#' @importFrom tidyr spread
#' @author Mohammed H. Cherkaoui (MMV), Venelin Mitov (IntiQuan GmbH)
#' @family Data
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

#########################################################
# Unit conversion ----
#########################################################

#' Conversion Table
#'
#' A dataset containing conversion factors for various units.
#'
#' @format A data frame with several rows and columns:
#' \describe{
#'   \item{UNIT_IN}{The unit to convert from.}
#'   \item{UNIT_OUT}{The unit to convert to.}
#'   \item{FORMULA}{The formula to use for the conversion, e.g. X_out=X_in*1000.}
#'   \item{COMMENT}{A comment on the conversion.}
#' }
#' 
#' @details This dataset is used to convert between different units of measurement.
#'
#' @examples
#' data(conversionTable)
#' head(conversionTable)
#'
#' @source Internal MMV data
#' @keywords datasets
"conversionTable"

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
#' @family Data
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


#########################################################
# Summarize trial data ----
#########################################################

#' Summarize given outcome variable(s) for a single trial
#'
#' Summarize given outcome variable(s) for a single trial using a choice of metrics
#' (mean, percentile or geomean). If no metric is provided, percentile is used by default
#' for numeric variable, while mean is used for logical variable.
#'
#' @param dataInput data.table containing outcome variable(s) and selected other variables for each individual.
#' @param varCOL List containing metrics (percentiles, mean and/or geomean) to be calculated for each outcome variable - i.e. \code{list(Cmax=c("Percentile","Mean"),APR="Mean")}.
#' @param percentiles Percentiles of outcome variable(s) to be calculated if appropriate (Default: \code{c(5,50,90)}).
#' @param usubjidCOL Name(s) of column(s) of `dataInput` uniquely describing individuals (Default: \code{c("IndivID","USUBJID")}).
#' @param trialCOL Name(s) of column(s) of `dataInput` uniquely describing trial (Default: \code{c("ScenID","ExpID","DoseID","Dose","nbrDoses","TrialID","TIME")}).
#'
#' @return data table containing values of outcome metrics for each trial
#'
#' @export
#' @author Sam Jones (MMV), Mohammed H. Cherkaoui (MMV)
#' @family Data
summaryByTrial <- function(dataInput,
                           varCOL,
                           percentiles = c(5,50,95),
                           usubjidCOL  = c("IndivID","USUBJID"),
                           trialCOL    = c("ScenID","ExpID","DoseID","Dose","nbrDoses","TrialID","TIME")){
  
  # Declare some variable identifiers to prevent notes during check (needed for data.table)
  Value <- NULL

  #---------------------------------------------------#
  # STEP 1: Run some checks and initialization ----
  #---------------------------------------------------#
  
  # Which metrics are accepted for calculation by this code:
  #   NOTE: Adding additional metrics will require additional coding
  accepted_metrics <- c("mean","geomean","percentiles")
  
  # Add/Adjust name to percentiles (Predictive percentiles):
  percentiles <- aux_addNamesToPercentiles(percentiles)
  
  # Generate colKey to use:
  colKey <- intersect(c(trialCOL,usubjidCOL),
                      names(dataInput))
  
  # Check if dataInput is a data.table:
  if(!data.table::is.data.table(dataInput)){
    data.table::setDT(dataInput, key = colKey)
  }
  
  # VarCOL checks:
  #   If varCOL is not provided
  if(is.null(varCOL)) {
    stop("varCOL is not provided - it must be provided in as a list with outcome measures as list names i.e: list(PRR=c('Percentiles','Mean')), please adjust")
  }
  #   If varCOL is provided but without correct structure, generate error
  if(is.null(names(varCOL))){
    varCOL <- sapply(varCOL, function(x){
      out <- "percentiles"
      names(out) <- x
      out
    },USE.NAMES = FALSE)
    # stop("varCOL must be provided as a list with outcome measures as list names i.e: list(PRR=c('Percentiles','Mean')), please adjust")
  }
  # If varCOL is provided, but not in the data:
  #   NOTE: This is calling names as varCOL is a list...
  idx_ToKeep <- (names(varCOL) %in% names(dataInput))
  if(!any(idx_ToKeep)){
    stop("None of the variables in 'varCOL' are present in dataInput: Please adjust.")
  }
  
  # Force varCOL to a list:
  varCOL <- as.list(varCOL[idx_ToKeep])
  
  # Create measure.vars:
  measure.vars <- names(varCOL)
  
  # Make sure that only mean is being used for logical variables:
  class.vars   <- sapply(measure.vars, function(x){class(dataInput[[x]])})
  logical.vars <- measure.vars[class.vars=="logical"]
  if(any(tolower(varCOL[logical.vars])!="mean")){
    varCOL[logical.vars] <- "mean"
    warning("Metrics other than arithmetic mean cannot be calculated for logical outcome variables: It was automatically adjusted.")
  }
  
  # Ensure that only mean, geomean and percentiles are chosen in varCOL
  metrics_diff <- lapply(varCOL, function(x){
    out <- setdiff(tolower(x), accepted_metrics)
    out
  })
  if(any(lapply(metrics_diff,length)>0)){
    stop("Metric provided in varCOL that cannot be calculated. Accepted metrics are 'mean','geomean','percentiles' ")
  }
  
  
  #---------------------------------------------------#
  # STEP 2: Identify what to apply for each variable in varCOL ----
  #---------------------------------------------------#
  idx_mean        <- sapply(varCOL, function(x){"mean"        %in% tolower(x)})
  idx_geomean     <- sapply(varCOL, function(x){"geomean"     %in% tolower(x)})
  idx_percentiles <- sapply(varCOL, function(x){"percentiles" %in% tolower(x)})
  
  
  #---------------------------------------------------#
  # STEP 3: Produce long format data tables to operate on ----
  #---------------------------------------------------#
  
  # Add variables and values to data-frame if tagged to calculate mean:
  if(any(idx_mean)){
    data.byIndiv.mean <- data.table::melt.data.table(dataInput,
                                                     id.vars       = colKey,
                                                     measure.vars  = measure.vars[idx_mean],
                                                     variable.name = "Variable",
                                                     value.name    = "Value")
  }else{
    data.byIndiv.mean <- NULL
  }
  # Add variables and values to data-frame if tagged to calculate geometric mean
  if(any(idx_geomean)){
    data.byIndiv.geomean <- data.table::melt.data.table(dataInput,
                                                        id.vars       = colKey,
                                                        measure.vars  = measure.vars[idx_geomean],
                                                        variable.name = "Variable",
                                                        value.name    = "Value")
  }else{
    data.byIndiv.geomean <- NULL
  }
  # Add variables and values to data-frame if tagged to calculate percentiles
  if(any(idx_percentiles)){
    data.byIndiv.percentiles <- data.table::melt.data.table(dataInput,
                                                            id.vars       = colKey,
                                                            measure.vars  = measure.vars[idx_percentiles],
                                                            variable.name = "Variable",
                                                            value.name    = "Value")
  }else{
    data.byIndiv.percentiles <- NULL
  }
  
  
  #---------------------------------------------------#
  # STEP 4: Operate on data tables to obtain outcome metrics described in varCOL ----
  #---------------------------------------------------#
  
  # Query: What's better here - re-using if(any(idx...)) or using if(length(data.by.Indiv... >0)) ?
  # Either way the intent is the same - process calculations for specific metrics if idx_ objects are TRUE for any of varCOL.
  
  # Mean:
  if(any(idx_mean)){
    colKey <- intersect(trialCOL,
                        names(data.byIndiv.mean))
    data.byTrial.mean <- data.byIndiv.mean[,{
      list(Metric = "Mean",
           Value  = mean(Value,na.rm=TRUE))
    },
    by = c(colKey,"Variable")]
  }else{
    data.byTrial.mean<-NULL
  }
  
  # Geometric Mean
  if(any(idx_geomean)){
    colKey <- intersect(trialCOL,
                        names(data.byIndiv.geomean))
    data.byTrial.geomean <- data.byIndiv.geomean[,{
      list(Metric = "Geomean",
           Value  = GeometricMean(Value,na.rm=TRUE))
    },
    by = c(colKey,"Variable")]
  }else{
    data.byTrial.geomean<-NULL
  }
  
  # Percentiles
  if(any(idx_percentiles)){
    colKey <- intersect(trialCOL,
                        names(data.byIndiv.percentiles))
    data.byTrial.percentiles <- data.byIndiv.percentiles[,{
      list(Metric =  names(percentiles),
           Value  =  quantile(Value, probs = percentiles/100, na.rm = TRUE))
    },
    by = c(colKey,"Variable")]
  }else{
    data.byTrial.percentiles<-NULL
  }
  
  # Combine data tables of all metrics
  data.byTrial <- data.table::rbindlist(list(data.byTrial.mean,
                                             data.byTrial.geomean,
                                             data.byTrial.percentiles))
  
  # Output:
  data.byTrial
}


#' Summarize confidence intervals for provided metrics across trials.
#'
#' @param dataInput data.table containing summarized outcome variable(s) summarized by trials - produced by `summaryByTrial` or similar format.
#' @param CIlevel Numeric containing the confidence interval in percent to be estimated (Default: 90)
#' @param metricCOL name of column of `dataInput` containing names of metrics on which CI is being estimated (Default: \code{"Metric"})
#' @param variableCOL name of column of `dataInput` containing names of variables (Default: \code{"Variable"})
#' @param valueCOL name of column of `dataInput` containing values of metrics for specific variables (Default: \code{"Value"})
#' @param scenCOL Name(s) of column(s) of `dataInput` uniquely describing scenario (Default: \code{c("ScenID","ExpID","DoseID","Dose","nbrDoses","TIME")})
#'
#' @return data table containing confidence intervals for selected outcome variable(s) and summarized metric(s)
#'
#' @export
#' @importFrom stats quantile
#' @author Sam Jones (MMV), Mohammed H. Cherkaoui (MMV)
#' @family Data
summaryAcrossTrials <- function(dataInput,
                                CIlevel     = 90,
                                metricCOL   = "Metric",
                                variableCOL = "Variable",
                                valueCOL    = "Value",
                                scenCOL     = c("ScenID","ExpID","DoseID","Dose","nbrDoses","TIME")){
  
  #---------------------------------------------------#
  # STEP 1: Run some checks and initialization ----
  #---------------------------------------------------#
  
  # Define quantile of CI:
  CI.percentile <- aux_constructCIpercentiles(CIlevel)
  
  # Check if data is data.table:
  if(!data.table::is.data.table(dataInput)){
    colKey <- intersect(c(scenCOL,variableCOL,metricCOL,valueCOL),
                        names(dataInput))
    data.table::setDT(dataInput, key = colKey)
  }
  
  
  #---------------------------------------------------#
  # STEP 2: Calculate CI for metrics of variables ----
  #---------------------------------------------------#
  
  # Get CI:
  colKey <- intersect(c(scenCOL,variableCOL,metricCOL),
                      names(dataInput))
  data <- dataInput[,{
    Variable.Q        <- quantile(get(valueCOL), probs = CI.percentile/100, na.rm = TRUE)
    Variable.Q        <- as.list(Variable.Q)
    names(Variable.Q) <- names(CI.percentile)
    Variable.Q
  },
  by = colKey]
  
  # Add CI level:
  data$`CI Level` <- CIlevel
  
  # Output:
  data
}


#' Generic function to estimate prediction intervals (PI) or confidence intervals (CI) of a metric
#'
#' Estimate the confidence interval of a metric (mean, percentiles, and/or geoman)
#' of various given variables, when multiple trials have been simulated. It calls `summaryByTrial`
#' and `summmaryAcrossTrials` in a row. If no metric is provided, percentile is used by default
#' for numeric variable, while mean is used for logical variable.
#'
#' @param dataInput data.table containing outcome variable(s) and selected other variables for each individual.
#' @param varCOL List containing metrics (percentiles, mean and/or geomean) to be calculated for each outcome variable - i.e. \code{list(Cmax=c("Percentile","Mean"),APR="Mean")}.
#' @param percentiles Percentiles of outcome variable(s) to be calculated if appropriate (Default: \code{c(5,50,90)}).
#' @param CIlevel Numeric containing the confidence interval in percent to be estimated (Default: 90)
#' @param usubjidCOL Name(s) of column(s) of `dataInput` uniquely describing individuals (Default: \code{c("IndivID","USUBJID")}).
#' @param trialCOL Name(s) of column(s) of `dataInput` uniquely describing trial (Default: \code{c("ScenID","ExpID","DoseID","Dose","nbrDoses","TrialID","TIME")}).
#' @param scenCOL Name(s) of column(s) of `dataInput` uniquely describing scenario (Default: \code{c("ScenID","ExpID","DoseID","Dose","nbrDoses","TIME")})
#'
#' @return a data.frame with the following columns: 
#'  - columns in scenCOL
#'  - Variable - the names of columns from varCOL
#'  - Metric - percentile according to the percentiles vector
#'  - CI Low, CI Median, CI High: quantiles of the CI or PI
#'  - CI Level - the level of the CI or PI accoridng to the CIlevel parameter
#' 
#' @examples 
#' # Example data
#' set.seed(123)
#' dataInput <- data.frame(
#'   Scenario = rep(1:3, each = 100),
#'   Trial = rep(1:2, each = 50, times = 3),
#'   ID = rep(1:10, each = 10, times = 3),
#'   TIME = rep(1:10, times = 30),
#'   Value = rnorm(300, mean = 50, sd = 10)
#' )
#' 
#' # Print the first few rows of the example data
#' head(dataInput)
#' 
#' # Define the columns and parameters
#' varCOL <- c("Value")
#' CIlevel <- 80
#' PIpercentiles <- c(5, 50, 95)
#' 
#' # Call the function
#' summaryResult <- summarize_PIandCIgeneric(
#'   dataInput = dataInput,
#'   varCOL = varCOL,
#'   percentiles = PIpercentiles,
#'   CIlevel = CIlevel,
#'   usubjidCOL = "ID",
#'   trialCOL = c("Scenario", "Trial", "TIME"),
#'   scenCOL = c("Scenario", "TIME")
#'   )
#' 
#' # Print the summary result
#' print(summaryResult)
#' 
#' # Average over TIMEs
#' summaryResult2 <- summarize_PIandCIgeneric(
#'   dataInput = dataInput,
#'   varCOL = varCOL,
#'   percentiles = PIpercentiles,
#'   CIlevel = CIlevel,
#'   usubjidCOL = "ID",
#'   trialCOL = c("Scenario", "Trial"),
#'   scenCOL = c("Scenario")
#'   )
#' 
#' # Print the summary result
#' print(summaryResult2)
#' 
#' @export
#' @author Mohammed H. Cherkaoui (MMV)
#' @family Data
summarize_PIandCIgeneric <- function(dataInput,
                                     varCOL,
                                     percentiles = c(5,50,90),
                                     CIlevel     = 90,
                                     usubjidCOL  = c("IndivID","USUBJID"),
                                     trialCOL    = c("ScenID","ExpID","DoseID","Dose","nbrDoses","TrialID","TIME"),
                                     scenCOL     = c("ScenID","ExpID","DoseID","Dose","nbrDoses","TIME")){
  
  # Summarize 'dataInput' by Trial:
  data.byTrial <- summaryByTrial(dataInput   = dataInput,
                                 percentiles = percentiles,
                                 varCOL      = varCOL,
                                 usubjidCOL  = usubjidCOL,
                                 trialCOL    = unique(c(scenCOL,trialCOL)))
  
  # Summarize 'data.byTrial' Across Trials:
  data <- summaryAcrossTrials(dataInput = data.byTrial,
                              CIlevel   = CIlevel,
                              scenCOL   = scenCOL)
  
  # Output:
  data
}


