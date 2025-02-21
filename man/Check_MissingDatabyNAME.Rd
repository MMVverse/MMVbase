% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{Check_MissingDatabyNAME}
\alias{Check_MissingDatabyNAME}
\title{Check_MissingDatabyNAME}
\usage{
Check_MissingDatabyNAME(dataMaster, NAME, colNAME = c("USUBJID"))
}
\arguments{
\item{dataMaster}{Master dataset in MMV format}

\item{NAME}{list of variable to test}

\item{colNAME}{Column name to use to identify subject (default: "USUBJID")}
}
\value{
dataSummaryMissing, which is a data.frame with the list of missing subject for each variable in \code{NAME}
}
\description{
Check for for each variable defined in \code{NAME}, if it is missing for
any subject defined in the column USUBJID  of the dataset \code{dataMaster}.
}
\examples{

# Create a sample dataset
dataMaster <- data.frame(
  USUBJID = rep(1:5, each = 3),
  NAME = rep(c("Var1", "Var2", "Var3"), 5),
  VALUE = c(1, NA, 3, 4, 5, NA, 7, 8, 9, NA, 11, 12, 13, 14, NA),
  VALUETXT = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O")
)

# Define the list of variables to check
NAME <- c("Var1", "Var2", "Var3")#'
# Check for missing data
Check_MissingDatabyNAME(dataMaster, NAME)

}
\seealso{
Other Data: 
\code{\link{IQRtableToDataFrame}()},
\code{\link{aux_constructCIpercentiles}()},
\code{\link{aux_createUSUBJID}()},
\code{\link{check_dataGeneralMMV_Center}()},
\code{\link{check_dataGeneralMMV}()},
\code{\link{convert_Unit}()},
\code{\link{get_IXGDFtoRemove}()},
\code{\link{summarize_PIandCIgeneric}()},
\code{\link{summaryAcrossTrials}()},
\code{\link{summaryByTrial}()},
\code{\link{swapName_MMVnameToName}()},
\code{\link{swapName_NameToMMVname}()},
\code{\link{transform_dataFrame_LongToWide}()},
\code{\link{transform_dataFrame_WideToLong}()}
}
\author{
Mohammed H. Cherkaoui (MMV), \email{cherkaouim@mmv.org}
}
\concept{Data}
