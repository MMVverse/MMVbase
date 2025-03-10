% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{check_dataGeneralMMV}
\alias{check_dataGeneralMMV}
\title{check_dataGeneralMMV
Checks if CENTER and CENTERNAME is well defined within \code{dataGeneral}}
\usage{
check_dataGeneralMMV(dataGeneral, CenterList = NULL)
}
\arguments{
\item{dataGeneral}{IQRdataGENERAL object}

\item{CenterList}{list of center name and number. Default is NULL. For internal MMV projects,
the center list is automatically can be retrieved from the MMVworkflows::list_Center function.}
}
\value{
IQRdataGENERAL object
}
\description{
Checks if CENTER and CENTERNAME (number and name of the center conducting the study) is
defined within \code{dataGeneral}, according to MMV's standards.
}
\details{
The name and number of the center conducting the study will be checked according to the
up-to-date center list, see \code{MMVworkflows::list_Center}.
The \code{check_dataGeneralMMV_Center} function is used internally.
}
\examples{

# Create a sample dataGeneral dataset
dataGeneral <- data.frame(
  USUBJID = rep(1:3, each = 4),
  CENTER = c(101, 101, 102, 102, 103, 103, 104, 104, 105, 105, 106, 106),
  CENTERNAME = c("Center A", "Center A", "Center B", "Center B", "Center C", 
                 "Center C", "Center D", "Center D", "Center E", "Center E", 
                 "Center F", "Center F"),
  VALUE = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120)
)

# Define a mock CenterList
CenterList <- list(
  "Center A" = 101,
  "Center B" = 102,
  "Center C" = 103,
  "Center D" = 104,
  "Center E" = 105,
  "Center F" = 106
)

# Check the dataGeneral dataset
checkedData <- check_dataGeneralMMV(dataGeneral, CenterList)
print(checkedData)

}
\seealso{
\code{\link{check_dataGeneralMMV_Center}}

Other Data: 
\code{\link{Check_MissingDatabyNAME}()},
\code{\link{IQRtableToDataFrame}()},
\code{\link{aux_constructCIpercentiles}()},
\code{\link{aux_createUSUBJID}()},
\code{\link{check_dataGeneralMMV_Center}()},
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
