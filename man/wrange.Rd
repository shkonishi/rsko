% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrange.R
\name{wrange}
\alias{wrange}
\title{Round to integer}
\usage{
wrange(v)
}
\arguments{
\item{v}{a numeric vector, or data.frame.}
}
\value{
a numeric vector for ylim aruguments of barplot
}
\description{
Create ylim value, including negative value, for barplot.
}
\examples{
tN <- table(Ni <- stats::rpois(100, lambda = 5))
barplot(tN, col = rainbow(20), ylim = rsko::wrange(tN), main='long y-axis')

ziris <- rsko::zconv(iris, 1:4, 1)
dat <- ziris[c("1", "51", "101"),-5]
par(mfcol=c(1,3), mar=c(6,2,1,1))
 res <- lapply(seq(nrow(dat)), function(i){
 barplot(unlist(dat[i,]), ylim = wrange(dat), las=2)
 })

}
