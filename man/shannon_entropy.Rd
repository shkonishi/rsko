% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shannon_entropy.R
\name{shannon_entropy}
\alias{shannon_entropy}
\title{Calcurate Shannon Index}
\usage{
shannon_entropy(x)
}
\arguments{
\item{x}{vector}
}
\description{
Calcurate Shannon Index
}
\examples{
rsko::shannon_entropy(rpois(10, 2))
rsko::shannon_entropy(iris[-5])
}
