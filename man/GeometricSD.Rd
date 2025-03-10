% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statistical.R
\name{GeometricSD}
\alias{GeometricSD}
\title{Geometric standard deviation of a positive numeric vector}
\usage{
GeometricSD(x, na.rm = FALSE, sqrt.unbiased = TRUE)
}
\arguments{
\item{x}{A numeric vector of positive values.}

\item{na.rm}{Logical indicating whether to remove missing values from x.
If na.rm=FALSE (the default) and x contains missing values, then a missing
value (NA_real_) is returned. If na.rm=TRUE, missing values are removed from x
prior to computing the geometric standard deviation.}

\item{sqrt.unbiased}{Logical specifying what method to use to compute
the sample standard deviation of the log-transformed observations. If
sqrt.unbiased=TRUE (the default), the square root of the unbiased estimator
of variance is used. Otherwise, the method of moments estimator of standard
deviation is used. See the DETAILS section for more information.}
}
\value{
The geometric standard deviation of the input vector.
}
\description{
This function calculates the geometric standard deviation of a numeric vector
containing only positive values. If non-positive values are present in the vector,
a warning is issued and NA_real_ is returned.
}
\details{
The geometric standard deviation is a measure of the spread or dispersion
of a set of positive values. It is calculated as the exponential of the standard
deviation of the logarithm of the values. The geometric standard deviation is
useful when dealing with data that follows a log-normal distribution.
}
\examples{
x <- c(1, 2, 3, 4, 5)
GeometricSD(x)

y <- c(10, 100, 1000, 10000)
GeometricSD(y)

z <- c(0.5, 1, 2, 4, 8)
GeometricSD(z)

w <- c(1, 2, NA, 4, 5)
GeometricSD(w, na.rm = TRUE)

v <- c(1, 2, 3, 4, 5)
GeometricSD(v, sqrt.unbiased = FALSE)

u <- c(1, 2, 3, 4, 5)
GeometricSD(u, na.rm = TRUE, sqrt.unbiased = FALSE)

}
\seealso{
Other Statistical: 
\code{\link{GeometricMean}()},
\code{\link{clopperPearsonMMV}()}
}
\author{
Venelin Mitov (IntiQuan)
}
\concept{Statistical}
