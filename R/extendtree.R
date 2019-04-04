#' Hierarchical clustering with amap::Dist and draw dendrogram with dendextend
#' @description This functions returns the list consit of hclust object and dendrogram object.
#' dendrogram is coloured by factor
#' @usage extendtree(dat, extend, distm, clm, lab, gp, vcol, cexlab, blwd, ...)
#' @param dat data frame or matrix
#' @param extend use with dendextend or not
#' @param clm one of the hclust methods "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty"(= WPGMA), "median"(= WPGMC) or "centroid"(= UPGMC).
#' The default is "ward.D2" .
#' @param distm distance measure one of the amap::Dist methods "euclidean", "maximum", "manhattan", "canberra", "binary", "pearson", "abspearson", "correlation", "abscorrelation", "spearman" or "kendall".
#' The default is "spearman".
#' @param lab Leaf labels
#' @param gp Group of labels as factor
#' @param vcol A vector of colours corresponding to group of leaves
#' @param cexlab cex of leaf label. The default is 0.8
#' @param blwd line width of edges. The default is 1
#' @param ... arbitary option, if not using dendextend. E.g.
#' nodePar = list(pch = NA, lab.cex = 0.8), edgePar = list(lwd = 2), leaflab ="none"
#' @return Character vector, length of n
#' @examples \dontrun{ # minimum usage
#' res1 <- rsko::extendtree(dat = mtcars, extend = FALSE, lab = rownames(mtcars))
#' res1[[1]]
#' plot(res1[[2]])
#'
#' # using dist object from other distance measure
#' cos.mat <- rsko::siml_mat(dat = t(iris[,1:4]), method = "cosine", type = "distance")
#' dist.cos <- as.dist(cos.mat)
#' res2 <-  rsko::extendtree(dat = iris[,1:4], extend = TRUE, distm = dist.cos, clm = "average",
#'     lab = as.character(iris$Species), gp = iris$Species, vcol = 1:3, cexlab = 0.5, blwd = 1)
#' plot(res2[[2]])
#'
#' # using dendextend with several 'set' options
#' res3 <- rsko::extendtree(dat = iris[,1:4], extend = TRUE, distm = "euclidean", clm = "average",
#'     lab = as.character(iris$Species), gp = iris$Species, vcol = 1:3, cexlab = 0.5, blwd = 1.5)
#' plot(res3[[2]])
#' }
#' @importFrom dplyr %>%
#' @export
extendtree <- function(dat, extend, distm="spearman", clm="ward.D2", lab, gp, vcol, cexlab = 0.8, blwd = 1, ...){

  # argument check: dat and lab ----
  if (is.data.frame("data.frame") | is.matrix("matrix")) {
    if (missing(lab) & is.data.frame("data.frame")) {
      lab <- rownames(dat)

    } else if (missing(lab) & is.matrix("matrix") & is.null(dimnames(dat)[[1]])) {
      lab <- 1:nrow(dat)

    } else if (missing(lab) & is.matrix("matrix") & !is.null(dimnames(dat)[[1]])) {
      lab <- dimnames(dat)[[1]]

    } else if (is.factor(lab)) {
      stop("'lab' is a character vector")

    } else if (length(lab) != nrow(dat)) {
      stop("'lab' length must be same as nrow of 'dat'.")

    }
  } else {
    stop("'dat' is a data frame or matrix")
  }

  # argument check: gp ----
  if (!missing(gp)) {
    if (!is.factor(gp)) {
      stop("'gp' is a factor for coloured label")
    } else if (nlevels(gp) != length(vcol)) {
      stop("The nlevels of 'gp' and length of 'vcol' must to be the same.")

    }
  }

  # Hierarchical cluster analysis with amap::Dist and hclust ----
  ## default distance measure and hclust method. ----

  ## Dist method selected from amap::Dist or your dist object ----
  Distms <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "pearson",
              "abspearson", "correlation", "abscorrelation", "spearman", "kendall")
  clms <- c("ward.D", "ward.D2", "single", "complete", "average",
            "mcquitty", "median", "centroid")

  ## argument check: 'clm' ----
  if (!any(clms %in% clm)) {
    stop(paste0("Select a clustering method form ", paste(clms, collapse = " | ")))
  }

  ## argument check: 'distm' and execution of hclust ----
  if (!any(Distms %in% distm) & class(distm) == "dist") {
    cl <- stats::hclust(distm, method = clm)

  } else if (any(Distms %in% distm)) {
    cl <- stats::hclust(amap::Dist(as.matrix(dat),method = distm ), method = clm)

  } else {
    stop(paste0("select a distance measure form ", paste(Distms, collapse = " | ")))
  }


  # convert hclust object to dendrogram object ----
  den <- stats::as.dendrogram(cl)

  # modification of the dendrogram object with dendextend ----
  if (extend) {

    ## set leaf labels to dendrogram ----
    leaf_order <- cl$order   # leaf order
    leaf_lab <- lab[leaf_order] # leaf label
    den <- den %>%
      dendextend::set("labels", leaf_lab)

    ## create leaf colors of edge binding to node ----
    leaf_col <- labels(den)
    llab <- split(lab, gp)
    invisible(lapply(seq_along(llab), function(i){leaf_col[leaf_col %in% llab[[i]]] <<- vcol[i]}))

    ## add colors of edge binding leaf to dendrogram object ----
    f <- function(x, y){
      den <<- dendextend::set(den, "by_labels_branches_col", value = x, TF_values = y)
    }
    invisible(mapply(f, llab, vcol))

    # set branch width, leaf labels color and labels cex
    den <- den %>%
      dendextend::set("branches_lwd", value = blwd) %>%
      dendextend::set("labels_col", value = vcol[gp][leaf_order]) %>%
      dendextend::set("labels_cex", cexlab)

    # when not using dendextend
  }
  # }else{
  #   plot(den, ...)
  #   mtext( paste("Measure:",distm, ", ", "Method:", clm, sep = ""),
  #          cex = 0.9, side = 3, line = 0, at = NA)
  # }

  # print clustering measure ----
  if ( class(distm) == "dist") {
    print(paste("Measure:", deparse(quote(dist)), ", ", "Method:", clm, sep = ""))
  } else {
    print(paste("Measure:",distm, ", ", "Method:", clm, sep = ""))
  }

  # return the list of hclust and dendrogram objects ----
  return(list(cl = cl, den = den))
}



