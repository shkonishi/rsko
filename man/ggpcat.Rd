% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggpcat.R
\name{ggpcat}
\alias{ggpcat}
\title{PCA with prcomp and ggplot of select pc score}
\usage{
ggpcat(dat, x, y, scaling, fctr1, fctr2, m.cols, txt, lgnd, main, cumpov)
}
\arguments{
\item{dat}{dataframe, matrix or prcomp object}

\item{x, y}{axis of pca score. The default values 'x=1, y=2' means xaxis for PC1 score and yaxis for PC2 score}

\item{scaling}{logical: A prcomp argument for scaling or not. The default is "TRUE"}

\item{fctr1}{factor: for colour of pca. The default value is rep(1,nrow(dat))}

\item{fctr2}{factor: for x axis of pc-scores (optional)}

\item{m.cols}{A vector of manual colour for 'ggplot::scale_colour_manual'. (optional)}

\item{txt}{integer: size of adding text (optional). The default value is NULL.}

\item{lgnd}{legend position "none","left","right","bottom", and c(1,3). The default is "right".}

\item{main}{graph title(optional)}

\item{cumpov}{numeric. A cumulative proportion of variance for graphed pc-scores.
The default value is 70}
}
\value{
List of prcomp and ggplot object
}
\description{
This functions returns the list consit of prcom object and ggplot object.
}
\examples{
# minimum usage
res <- ggpcat(dat = iris[-5], x = 1, y = 2, scaling = TRUE, fctr1 = iris$Species)
summary(res$res.prcmp)
plot(res$gg)
plot(res$gg_pcs)

# manual color
res <- ggpcat(iris[-5], fctr1 = iris$Species, m.cols = 1:3)

\dontrun{
nfpkm <- rskodat::nfpkm
res2 <- ggpcat(nfpkm[-1:-4], fctr1=nfpkm$days, fctr2=nfpkm$days, cumpov=64)
res3 <- ggpcat(nfpkm[-1:-4], fctr1=nfpkm$runs, fctr2=nfpkm$days, cumpov=64)
res4 <- ggpcat(nfpkm[-1:-4], fctr1=nfpkm$runs, fctr2=nfpkm$days, m.cols = 1:3, cumpov=64)
}
}
