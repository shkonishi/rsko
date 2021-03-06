% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extendtree.R
\name{extendtree}
\alias{extendtree}
\title{Hierarchical clustering with amap::Dist and draw dendrogram with dendextend}
\usage{
extendtree(dat, extend, distm, clm, lab, gp, vcol, cexlab, blwd, ...)
}
\arguments{
\item{dat}{data frame or matrix}

\item{extend}{use with dendextend or not}

\item{distm}{distance measure one of the amap::Dist methods "euclidean", "maximum", "manhattan", "canberra", "binary", "pearson", "abspearson", "correlation", "abscorrelation", "spearman" or "kendall".
The default is "spearman".}

\item{clm}{one of the hclust methods "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty"(= WPGMA), "median"(= WPGMC) or "centroid"(= UPGMC).
The default is "ward.D2" .}

\item{lab}{Leaf labels}

\item{gp}{Group of labels as factor}

\item{vcol}{A vector of colours corresponding to group of leaves}

\item{cexlab}{cex of leaf label. The default is 0.8}

\item{blwd}{line width of edges. The default is 1}

\item{...}{arbitary option, if not using dendextend. E.g.
nodePar = list(pch = NA, lab.cex = 0.8), edgePar = list(lwd = 2), leaflab ="none"}
}
\value{
Character vector, length of n
}
\description{
This functions returns the list consit of hclust object and dendrogram object.
dendrogram is coloured by factor
}
\examples{
\dontrun{ # minimum usage
res1 <- rsko::extendtree(dat = mtcars, extend = FALSE, lab = rownames(mtcars))
res1[[1]]
plot(res1[[2]])

# using dist object from other distance measure
cos.mat <- rsko::siml_mat(dat = t(iris[,1:4]), method = "cosine", type = "distance")
dist.cos <- as.dist(cos.mat)
res2 <-  rsko::extendtree(dat = iris[,1:4], extend = TRUE, distm = dist.cos, clm = "average",
    lab = as.character(iris$Species), gp = iris$Species, vcol = 1:3, cexlab = 0.5, blwd = 1)
plot(res2[[2]])

# using dendextend with several 'set' options
res3 <- rsko::extendtree(dat = iris[,1:4], extend = TRUE, distm = "euclidean", clm = "average",
    lab = as.character(iris$Species), gp = iris$Species, vcol = 1:3, cexlab = 0.5, blwd = 1.5)
plot(res3[[2]])
}
}
