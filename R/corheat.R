#' Gene clustering and cutting cluster with dynamic cut
#' @description Gene clustering for RNA-seq data and cutting cluster with dynamic cut
#' @usage corheat(dat, distm, clm, method_dycut, draw)
#' @return draw heatmap and results of dynamic cut
#' @param dat RNA-seq count data(row:sample, column:gene)
#' @param distm select from c("pearson", "abspearson", "squarepearson", "spearman")
#' @param clm clustering method of hclust
#' @param method_dycut method of 'cutreeDynamic' select from "tree", or "hybrid
#' @param draw logical draw heatmap or nod.
#' @examples
#' \dontrun{
#' # data: normalized fpkm
#' nfpkm <- as.data.frame(rskodat::nfpkm)
#' res.dcut <-cornet::dycutdf(nfpkm[-1:-4], distm = "correlation")
#' cl_dat <- res.dcut$cluster_dat
#' corheat with dynamic tree cut
#' res <- corheat(dat = nfpkm[-1:-4], distm = "correlation", clm = "average",
#'                method_dycut = "tree", draw = TRUE)
#' }
#' @export
corheat <- function(dat, distm, clm, method_dycut, draw){

  # argument check: dat ----
  classdat <- class(dat)
  if (any(classdat %in% c("data.frame", "matrix", "tbl_df"))) {
    if (ncol(dat) > 1000) {
      cat(" In case of 'ncol(dat)' over 1000, no drawing heat map of all genes, and just return hclust object.")
      dat <- dat[,sample(1:ncol(dat), 1000)]
    }
  } else {
    stop("'dat' class is data frame or matrix")
  }

  # argument check: dism ----
  distms <- c("pearson", "correlation", "abscorrelation",
              "spearman", "squarepearson")
  if (!any(distms %in% distm)) {
    stop(paste('Select a distance measure form', paste(distms, collapse = ", ")))
  }

  # argument check: clm ----
  clms <- c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median","centroid")
  if (!any(clms %in% clm)) {
    stop(paste('Select a clustering meathod form', paste(clms, collapse = ", ")))
  }

  # clustering ----
  dat <- as.matrix(dat)
  if (distm == "correlation" ) {  # pearson distance
    cor.dat <- stats::cor(dat)
    dis.mat <- stats::as.dist(1 - cor.dat)
    r_hcl <- stats::hclust(amap::Dist(t(dat), method = distm), method = clm)
    r_den <- dendextend::hang.dendrogram(stats::as.dendrogram(r_hcl))

  } else if (distm == "abscorrelation") { # abs pearson
    cor.dat <- abs(stats::cor(dat))
    dis.mat <- stats::as.dist(1 - cor.dat)
    r_hcl <- stats::hclust(amap::Dist(t(dat), method = distm), method = clm)
    r_den <- dendextend::hang.dendrogram(stats::as.dendrogram(r_hcl))

  } else if (distm == "pearson" ) {  # cosine distance
    cor.dat <- rsko::siml_mat(t(dat), "cosine", "similarity", "both")
    r_hcl <- stats::hclust(amap::Dist(dat, method = distm), method = clm)
    r_den <- dendextend::hang.dendrogram(stats::as.dendrogram(r_hcl))

  # } else if (distm == "abspearson" ){  # cosine distance
  #   cor.dat <- rsko::siml_mat(dat, "cosine", "similarity", "both")
  #   r_hcl <- stats::hclust(amap::Dist(t(dat), method = distm), method = clm)
  #   r_den <- dendextend::hang.dendrogram(stats::as.dendrogram(r_hcl))

  } else if (distm == "spearman") {
    cor.dat <- stats::cor(dat, method = distm)
    dis.mat <- stats::as.dist(1 - cor.dat)
    r_hcl <- stats::hclust(amap::Dist(t(dat), method = distm), method = clm)
    r_den <- dendextend::hang.dendrogram(stats::as.dendrogram(r_hcl))

  } else   if (distm == "squarepearson") {
    cor.dat <- stats::cor(dat)^2
    dis.mat <- stats::as.dist(1 - cor.dat)
    r_hcl <- stats::hclust(dis.mat, method = clm)
    r_den <- dendextend::hang.dendrogram(stats::as.dendrogram(r_hcl))

  } else {
    stop('Select a distance measure form "correlation", "abscorrelation", "pearson(cosine)", "spearman" ')
  }

  # dynamic cut  method "tree", "hybrid" ----
  dycmethods <- c("tree", "hybrid", "height")
  if(!any(dycmethods %in% method_dycut)){
    stop("'method_dycut' select from 'tree','hybrid', and 'height'.")
  }

  # dynamicTree cutmethod "tree" or "hybrid" ----
  ## leaf label(tree order) ----
  if(any(length(labels(r_hcl)))){
    lab <- labels(r_den)
  }else{
    lab <- as.character(1:ncol(dat))
  }
  ## cutreeDynamic ----
  if (method_dycut == "tree"){
    dyct <- dynamicTreeCut::cutreeDynamic(dendro = r_hcl, method = method_dycut, minClusterSize = 5 )
    dyct <- dyct[r_hcl$order]
    names(dyct) <- lab

  } else if (method_dycut == "hybrid"){
    dyct <- dynamicTreeCut::cutreeDynamic(dendro = r_hcl, distM = as.matrix(dis.mat), method = method_dycut )
    dyct <- dyct[r_hcl$order]
    names(dyct) <- lab
  } else {
    stop("cut height still be deveropping")
  }

  ## create colour(tree order) -----
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
  }
  vcol <- gg_color_hue(length(unique(dyct))) # colours corresponding to clusters
  cols.ccl <- vcol[as.factor(dyct)] # vector of leaf colours
  llab <- split(lab, factor(dyct))
  side_col <- vcol[factor(dyct[r_hcl$labels])] # side color bar for heatmap
  rside_col <- rev(vcol)[factor(dyct[r_hcl$labels])]

  # modify dendrogram object with 'dendextend' ----
  ## add edge colour concatenated to leaf belongs to same cluster ----
  f <- function(x, y){
    r_den <<- dendextend::set(r_den, "by_labels_branches_col", value = x, TF_values = y)
  }
  invisible(mapply(f, llab, vcol))

  ## set parameter of these branch width, leaf labels color and labels cex to dendrogram object. ----
  r_den <- r_den %>%
    dendextend::set("branches_lwd", value = 0.5) %>%
    dendextend::set("labels_col", value = cols.ccl) %>%
    dendextend::set("labels_cex", 0.5)

  # draw heat map -----
  if (draw == TRUE) {
    gplots::heatmap.2(
      as.matrix(cor.dat),
      symm = T,
      #revC = T,
      col = rev(grDevices::heat.colors(256)), # bluered(256), rev(heat.colors(256))
      scale = "none", #"row", "Column", "both", "none"
      dendrogram = "both", #"none",#"col", #"both",
      Colv = r_den,
      Rowv = r_den,
      ColSideColors = side_col, # sample order
      RowSideColors = side_col,
      key = TRUE,
      keysize = 1,
      symkey = FALSE,
      density.info = "none",
      trace = "none",
      margin = c(6,5),
      cexCol = 0.8,
      labRow = FALSE,
      labCol = FALSE
    )
    return(list(cormat = cor.dat, r_hcl = r_hcl, cl_with_dynamiccut = dyct))
  } else {
    return(list(cormat = cor.dat, r_hcl = r_hcl, cl_with_dynamiccut = dyct))
  }
}
