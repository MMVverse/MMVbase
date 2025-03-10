% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{get_IXGDFtoRemove}
\alias{get_IXGDFtoRemove}
\title{Get IXGDF indices of observations below LLOQ to remove}
\usage{
get_IXGDFtoRemove(
  dataGeneral,
  obNAME = "log(Parasitemia Total)",
  removeType = "ToLastOb"
)
}
\arguments{
\item{dataGeneral}{An IQRdataGeneral dataset or a data.frame containing at least the following
columns: IXGDF,USUBJID,TIME,NAME,VALUE,LLOQ columns.}

\item{obNAME}{A character string specifying the name of the observation of interest.
Default is \code{"log(Parasitemia Total)"}.}

\item{removeType}{A character string specifying the type of removal operation. Options are:
\itemize{
\item \code{"ToLastOb"}: Finds the last time-point where at least one subject has value above LLOQ.
Removes all observations after this time point for all subjects.
\item \code{"RemoveCure"}: For each subject, removes observations after the last time point where the value
is above the LLOQ, if the value at the last time point is below or equal to the LLOQ.
\item \code{"Cure1Week"}: For each subject, removes observations after one week from the last time point where the
value is above the LLOQ, if the value at the maximum time point is below or equal to the LLOQ.
}}
}
\value{
A vector of IXGDF indices of the observations to remove.
}
\description{
This function identifies and returns the indices of observations to remove from a dataset
based on the specified observation name and removal type.
}
\details{
This function was written with malaria parasitemia measurements in mind, but can be used
to trim measurements below LLOQ in other cases.
}
\examples{

data <- data.frame(
  USUBJID = rep(1:3, each = 6),
  NAME = rep("log(Parasitemia Total)", 18),
  TIME = rep((1:6)*72, 3),
  VALUE = c(10, 4, 3, 4, 6, 5, 12, 10, 8, 5, 4, 3, 14, 3, 3, 3, 3, 3),
  LLOQ = rep(5, 18),
  IXGDF = 1:18
  )
get_IXGDFtoRemove(data, obNAME = "log(Parasitemia Total)", removeType = "ToLastOb")
get_IXGDFtoRemove(data, obNAME = "log(Parasitemia Total)", removeType = "RemoveCure")
get_IXGDFtoRemove(data, obNAME = "log(Parasitemia Total)", removeType = "Cure1Week")

}
\seealso{
Other Data: 
\code{\link{Check_MissingDatabyNAME}()},
\code{\link{IQRtableToDataFrame}()},
\code{\link{aux_constructCIpercentiles}()},
\code{\link{aux_createUSUBJID}()},
\code{\link{check_dataGeneralMMV}()},
\code{\link{check_dataGeneralMMV_Center}()},
\code{\link{convert_Unit}()},
\code{\link{summarize_PIandCIgeneric}()},
\code{\link{summaryAcrossTrials}()},
\code{\link{summaryByTrial}()},
\code{\link{swapName_MMVnameToName}()},
\code{\link{swapName_NameToMMVname}()},
\code{\link{transform_dataFrame_LongToWide}()},
\code{\link{transform_dataFrame_WideToLong}()}
}
\author{
Mohammed H. Cherkaoui (MMV), Venelin Mitov (IntiQuan GmbH)
}
\concept{Data}
