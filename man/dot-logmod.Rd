% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trinaryUtils.r
\name{.logmod}
\alias{.logmod}
\title{Take the logarithm of the absolute value of a number with its sign}
\usage{
.logmod(x)
}
\arguments{
\item{x}{A numerical input.}
}
\value{
The result of taking the logarithm of the absolute value of \code{x}
with its sign.
}
\description{
This function takes a numerical input \code{x}, computes the logarithm of its
absolute value and multiplies it by the sign of \code{x}. This ensures that the
result has the same sign as the input \code{x}.
}
\examples{
.logmod(10)
.logmod(-5)
}
\keyword{internal}
