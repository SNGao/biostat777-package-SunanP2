% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_ci_class().R
\name{make_ci_class}
\alias{make_ci_class}
\title{Create a constructor function for the ci_class}
\usage{
make_ci_class(x, conf = 0.95)
}
\arguments{
\item{x}{the input data vector with the length of N}

\item{conf}{the \code{1-α} confidence interval}
}
\value{
the ci_class for confidence interval
}
\description{
Create a constructor function for the ci_class called make_ci_class()
}
\details{
This function creates a constructor function for the ci_class called make_ci_class().

If the input data contains NaN value, the function will print warning to remind.

If the input data contains Non-numeric value, the function will print warning to remind.
}
\examples{
library(SunanP2)
set.seed(1234)
x <- rnorm(100)
obj <- make_ci_class(x)
print(obj)
}
