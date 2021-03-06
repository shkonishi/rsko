% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zconv.R
\name{zconv}
\alias{zconv}
\title{Z-conversion}
\usage{
zconv(dat, columns, mrgn, cntr, scl)
}
\arguments{
\item{dat}{A list of ggplot objects}

\item{columns}{select columns}

\item{mrgn}{1 or 2}

\item{cntr}{an argument of base::scale. The default value is TRUE}

\item{scl}{an argument of base::scale. The default value is TRUE}
}
\value{
converted data.frame
}
\description{
Z-conversion
}
\examples{
##
zrdat <- zconv(iris, -5, 1)
zcdat <- zconv(iris, -5, 2)
head(apply(zrdat[-5], 1, mean)); head(apply(zrdat[-5], 1, sd))
head(apply(zcdat[-5], 2, mean)); head(apply(zcdat[-5], 2, sd))
}
