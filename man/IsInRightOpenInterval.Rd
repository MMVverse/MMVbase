% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IsInInterval.R
\name{IsInRightOpenInterval}
\alias{IsInRightOpenInterval}
\title{Check if value is in the right open interval \code{[low, high)}}
\usage{
IsInRightOpenInterval(value, low, high)
}
\arguments{
\item{value}{numeric.}

\item{low}{numeric.}

\item{high}{numeric.}
}
\value{
TRUE or FALSE. If any of the arguments is not numeric or low < high is not true an
error is raised.
}
\description{
Check if value is in the right open interval \code{[low, high)}
}
\examples{
IsInRightOpenInterval(2, 1, 3)
IsInRightOpenInterval(2, 1, 2)
IsInRightOpenInterval(2, 1, 1.5)
error <- try(IsInRightOpenInterval(2, 1.5, 1), silent = TRUE)
}
\author{
Venelin Mitov (IntiQuan)
}
