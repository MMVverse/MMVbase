% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{summarize_PIandCIgeneric}
\alias{summarize_PIandCIgeneric}
\title{Generic function to estimate prediction intervals (PI) or confidence intervals (CI) of a metric}
\usage{
summarize_PIandCIgeneric(
  dataInput,
  varCOL,
  percentiles = c(5, 50, 90),
  CIlevel = 90,
  usubjidCOL = c("IndivID", "USUBJID"),
  trialCOL = c("ScenID", "ExpID", "DoseID", "Dose", "nbrDoses", "TrialID", "TIME"),
  scenCOL = c("ScenID", "ExpID", "DoseID", "Dose", "nbrDoses", "TIME")
)
}
\arguments{
\item{dataInput}{data.table containing outcome variable(s) and selected other variables for each individual.}

\item{varCOL}{List containing metrics (percentiles, mean and/or geomean) to be calculated for each outcome variable - i.e. \code{list(Cmax=c("Percentile","Mean"),APR="Mean")}.}

\item{percentiles}{Percentiles of outcome variable(s) to be calculated if appropriate (Default: \code{c(5,50,90)}).}

\item{CIlevel}{Numeric containing the confidence interval in percent to be estimated (Default: 90)}

\item{usubjidCOL}{Name(s) of column(s) of \code{dataInput} uniquely describing individuals (Default: \code{c("IndivID","USUBJID")}).}

\item{trialCOL}{Name(s) of column(s) of \code{dataInput} uniquely describing trial (Default: \code{c("ScenID","ExpID","DoseID","Dose","nbrDoses","TrialID","TIME")}).}

\item{scenCOL}{Name(s) of column(s) of \code{dataInput} uniquely describing scenario (Default: \code{c("ScenID","ExpID","DoseID","Dose","nbrDoses","TIME")})}
}
\value{
a data.frame with the following columns:
\itemize{
\item columns in scenCOL
\item Variable - the names of columns from varCOL
\item Metric - percentile according to the percentiles vector
\item CI Low, CI Median, CI High: quantiles of the CI or PI
\item CI Level - the level of the CI or PI accoridng to the CIlevel parameter
}
}
\description{
Estimate the confidence interval of a metric (mean, percentiles, and/or geoman)
of various given variables, when multiple trials have been simulated. It calls \code{summaryByTrial}
and \code{summmaryAcrossTrials} in a row. If no metric is provided, percentile is used by default
for numeric variable, while mean is used for logical variable.
}
\examples{
# Example data
set.seed(123)
dataInput <- data.frame(
  Scenario = rep(1:3, each = 100),
  Trial = rep(1:2, each = 50, times = 3),
  ID = rep(1:10, each = 10, times = 3),
  TIME = rep(1:10, times = 30),
  Value = rnorm(300, mean = 50, sd = 10)
)

# Print the first few rows of the example data
head(dataInput)

# Define the columns and parameters
varCOL <- c("Value")
CIlevel <- 80
PIpercentiles <- c(5, 50, 95)

# Call the function
summaryResult <- summarize_PIandCIgeneric(
  dataInput = dataInput,
  varCOL = varCOL,
  percentiles = PIpercentiles,
  CIlevel = CIlevel,
  usubjidCOL = "ID",
  trialCOL = c("Scenario", "Trial", "TIME"),
  scenCOL = c("Scenario", "TIME")
  )

# Print the summary result
print(summaryResult)

# Average over TIMEs
summaryResult2 <- summarize_PIandCIgeneric(
  dataInput = dataInput,
  varCOL = varCOL,
  percentiles = PIpercentiles,
  CIlevel = CIlevel,
  usubjidCOL = "ID",
  trialCOL = c("Scenario", "Trial"),
  scenCOL = c("Scenario")
  )

# Print the summary result
print(summaryResult2)

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
\code{\link{get_IXGDFtoRemove}()},
\code{\link{summaryAcrossTrials}()},
\code{\link{summaryByTrial}()},
\code{\link{swapName_MMVnameToName}()},
\code{\link{swapName_NameToMMVname}()},
\code{\link{transform_dataFrame_LongToWide}()},
\code{\link{transform_dataFrame_WideToLong}()}
}
\author{
Mohammed H. Cherkaoui (MMV)
}
\concept{Data}
