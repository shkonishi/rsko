#' Draw heatmap usig gplots
#' @description hierarchical clustering using amap::Dsit, and draw heatmap using by gplots::heatmap.2.
#' Usinga a categorical data, consists from integer vectors.
#' @usage myheat(d, squared, categorical, draw, cden, rden,
#' c.distm, c.clm, r.distm, r.clm, scale, color, ...)
#' @param d data.frame or matrix
#' @param squared logical: squared matrix like a correlation matrix is TRUE. The default value is FALSE
#' @param categorical logical: categorical data or not.
#' @param draw "both","row","column", or "none"
#' @param cden dendrogram object like a result of rsko::extendtree
#' @param rden dendrogram object like a result of rsko::extendtree
#' @param c.distm A distance measure for column side dendrogram. The default value is "spearman".
#' If categorical was TRUE, the default value is "jaccard", and select from these "jaccard", "simpson", "dice" , and "smc".
#' @param c.clm A clustering method for column side dendrogram.
#' @param r.distm A distance measure for row side dendrogram.
#' @param r.clm A clustering method for row side dendrogram.
#' @param scale character "row", "column", "none"
#' @param color character select from "bluered", "greenred", "heat.colors", "BuRd". If categorical was TRUE,
#' colour vectors which lengh same as levels of 'd'.
#' @param ... additional optioins for gplots::heatmap.2.  E.g.  cexRow = 0.8, cexCol = 0.8, labRow = FALSE = F, labCol = F
#' @examples \dontrun{
#' # clustering and draw heatmap
#' dat = iris[-5]
#' myheat(d = dat, scale = "column", cexCol = 1)
#' myheat(d = dat, draw = "both", scale = "column", color = "BuRd", labRow = FALSE, labCol = FALSE)
#' myheat(d = dat, draw = "both", scale = "column", color = "bluered", cexRow = 0.8, cexCol = 0.8)
#'
#' # give dendrogram object
#' rden <- rsko::extendtree(dat = dat, extend = TRUE,
#'                          distm = "euclidean", clm = "average",
#'                          lab = as.character(iris[[5]]),
#'                          gp = iris[,5], vcol = 1:3)
#'
#' cden <- rsko::extendtree(dat = t(dat), extend = FALSE,
#'                          distm = "correlation", clm = "average")
#'
#' myheat(d = t(dat), categorical = FALSE, squared = FALSE, draw = "both",
#'        cden = rden[[2]], rden = cden[[2]],  scale = "row", cexRow = 0.8)
#'
#' # squared matrix
#' cormat <- as.data.frame(cor(t(dat)))
#' myheat(cormat, squared = TRUE, draw = "both", scale = "none",
#' c.distm = "correlation", r.distm = "correlation", c.clm = "average", r.clm = "average",
#' color = "heat.colors")
#'
#' # categorical data, which converted from character to integer
#' scale_col <- function(x) as.vector(scale(x, center = min(x), scale = max(x)-min(x)))
#' fct_col <- function(x) {
#'    cut(x, breaks = seq(0, 1, 0.25), labels = letters[1:4], include.lowest = TRUE)
#'    }
#' dat <- as.data.frame(do.call(cbind, lapply(iris[-5], function(x) { fct_col(scale_col(x))})))
#' col <- RColorBrewer::brewer.pal(4, "Dark2")
#' myheat(d=dat, categorical=TRUE, c.distm="smc", r.distm="smc", color=col, cexCol=0.8)
#'
#'
#' # categorical data
#' data("listeria", package = "qtl")
#' dat <- listeria$geno[[1]][[1]]
#' col <- c("steelblue", "darkseagreen1","violetred2")
#' rsko::myheat(d=dat, categorical = T, color = col, c.distm="jaccard", r.distm="smc")
#' legend("topleft", legend = c(levels(factor(dat)),"null"), col = c(col,NA), pch = 15, cex = 0.8)
#' }
#'
#' @importFrom dplyr %>%
#' @export
myheat <- function(d, squared = FALSE, categorical = FALSE,
                   draw = "both", cden = NULL, rden = NULL,
                   c.distm = "spearman", c.clm = "ward.D2",
                   r.distm = "spearman", r.clm = "ward.D2",
                   scale = "row", color = "bluered", ...) {

  # argument check: squared matrix or not ----
  if (squared == T) {
    if (length(unique(dim(d))) == 1) {
      scale <- "none"
    } else {
      stop("this is not squared matrix")
    }
    if (!identical(cden, rden) & !identical(c.distm, r.distm) & !identical(c.clm, r.clm)) {
      stop("If squared matrix likea as correlation matrix, row side dendrogram and col side dendrogram must be the same")
    }
  }

  # argument check: categorical data, change defalut clustering arguments ----
  if (categorical == T) {
    # defalut clustering methods for categorical data

    # no scaling with categorical data
    scale <- "none"

    # check length of color vector, same as levels of factorized 'd'
    ncolour <- nlevels(factor(as.matrix(d)))
    if (length(color) != ncolour) stop(" color must be nlevels of 'd'. ")

    # check distance measure for categorical data
    if (is.null(cden) | is.null(rden)) {
      fct_mthds <- c("jaccard", "simpson", "dice" , "smc")
      # if is.null distance measure, set the default distance measure.
      if (is.null(c.distm) & is.null(r.distm)) {
        c.distm <- "jaccard"; r.distm <- "jaccard"

      } else if (is.null(c.distm) & !is.null(r.distm)) {
        if (!r.distm %in% fct_mthds) {
          stop(paste0("If the 'd' was categorical data, clustering method select from ",
                      paste(fct_mthds, collapse = ", "), "."))
        } else {
          c.distm <- "jaccard"
        }

      } else if (!is.null(c.distm) & is.null(r.distm)) {
        if (!c.distm %in% fct_mthds) {
          stop(paste0("If the 'd' was categorical data, clustering method select from ",
                      paste(fct_mthds, collapse = ", "), "."))
        } else {
          r.distm <- "jaccard"
        }

      } else {
        if (!r.distm %in% fct_mthds | !c.distm %in% fct_mthds) {
          stop(paste0("If the 'd' was categorical data, clustering method select from ",
                      paste(fct_mthds, collapse = ", "), "."))
        }
      }
    }
  }

  # argument check: scale ----
  scale_args <- c("row", "column", "none")
  if (!any(c("row", "column", "none") %in% scale)) {
    stop( paste(" 'scale' must be select from ", paste(scale_args, collapse = ", ")))
  }


  # clustering or not ----
  if (draw == "both" & categorical == F) { # drow dendrogram both sides
    if (!is.null(cden) & !is.null(rden) &
        dendextend::is.dendrogram(cden) & dendextend::is.dendrogram(rden)) {

    } else if (is.null(cden) & !is.null(rden)) { # exist row dendrogram
      cden <- stats::as.dendrogram(stats::hclust(amap::Dist(t(d), method = c.distm), method = c.clm))

    } else if (!is.null(cden) & is.null(rden)) { # exist column dendrogram
      rden <- stats::as.dendrogram(stats::hclust(amap::Dist(d, method = r.distm), method = r.clm))

    } else if (is.null(cden) & is.null(rden) & length(unique(dim(d))) != 1) { # no exist both dendrogram and not squared matrix
      rden <- stats::as.dendrogram(stats::hclust(amap::Dist(d, method = r.distm), method = r.clm))
      cden <- stats::as.dendrogram(stats::hclust(amap::Dist(t(d), method = c.distm), method = c.clm))

    } else if (is.null(cden) & is.null(rden) & length(unique(dim(d))) == 1) { # no exist both dendrogram and squared matrix
      rden <- stats::as.dendrogram(stats::hclust(amap::Dist(d, method = r.distm), method = r.clm))
    }

  } else if (draw == "both" & categorical == T) {
    if (!is.null(cden) & !is.null(rden) &
        dendextend::is.dendrogram(cden) & dendextend::is.dendrogram(rden)) {

    } else if (is.null(cden) & !is.null(rden)) { # exist row dendrogram
      cden <- data.frame(d, check.names = F, stringsAsFactors = F) %>%
        dplyr::mutate_if(is.numeric, as.character) %>%
        rsko::siml_mat(., method = c.distm, vorder = T) %>%
        {stats::as.dendrogram(stats::hclust(stats::as.dist(.$jaccard_distance), c.clm))}

    } else if (!is.null(cden) & is.null(rden)) { # exist column dendrogram
      rden <- data.frame(t(d), check.names = F, stringsAsFactors = F) %>%
        dplyr::mutate_if(is.numeric, as.character) %>%
        rsko::siml_mat(., method = r.distm, vorder = T) %>%
        {stats::as.dendrogram(stats::hclust(stats::as.dist(.$jaccard_distance), r.clm))}

    } else if (is.null(cden) & is.null(rden)) { # no exist both dendrogram and not squared matrix
      cden <- as.data.frame(d) %>%
        dplyr::mutate_if(is.numeric, as.character) %>%
        rsko::siml_mat(., method = c.distm, vorder = T) %>%
        {stats::as.dendrogram(stats::hclust(stats::as.dist(.[[1]]), c.clm))}

      rden <- data.frame(t(d), check.names = F) %>%
        dplyr::mutate_if(is.numeric, as.character) %>%
        rsko::siml_mat(., method = r.distm, vorder = T) %>%
        {stats::as.dendrogram(stats::hclust(stats::as.dist(.[[1]]), r.clm))}


    } else if (is.null(cden) & is.null(rden) & length(unique(dim(d))) == 1 & squared == T) {# no exist both dendrogram and squared matrix
      rden <- stats::as.dendrogram(stats::hclust(amap::Dist(d, method = r.distm), method = r.clm))

    }

  } else if (draw == "row") { # row side only ----
    cden <- NULL
    if (is.null(rden) & categorical == F) {
      rden <- stats::as.dendrogram(stats::hclust(amap::Dist(d, method = r.distm), method = r.clm))

    } else if (is.null(rden) & categorical == T) {
      rden <- data.frame(t(d), check.names = F) %>%
        dplyr::mutate_if(is.numeric, as.character) %>%
        rsko::siml_mat(., method = r.distm, vorder = T) %>%
        {stats::as.dendrogram(stats::hclust(stats::as.dist(.[[1]]), r.clm))}

    } else if (!dendextend::is.dendrogram(rden)) {
      stop("'rden' is not dendrogram object.")

    }

  } else if (draw == "column") { # col side only ----
    rden <- NULL

    if (is.null(cden) & categorical == F) {
      cden <- stats::as.dendrogram(stats::hclust(amap::Dist(t(d), method = c.distm), method = c.clm))

    } else if (is.null(cden) & categorical == T) {
      cden <- data.frame(d, check.names = F) %>%
        dplyr::mutate_if(is.numeric, as.character) %>%
        rsko::siml_mat(., method = r.distm, vorder = T) %>%
        {stats::as.dendrogram(stats::hclust(stats::as.dist(.[[1]]), r.clm))}

    } else if (!dendextend::is.dendrogram(cden)) {
      stop("'cden' is not dendrogram object.")

    }
  } else {# no side ----
    cden <- NULL; rden <- NULL
  }

  # color ----
  heatcols <- c("bluered", "greenred", "heat.colors", "BuRd")
  if (any(heatcols %in% color)) {
    colors <- rep(color, 256)
    hcol <- ifelse(colors == "bluered",  gplots::bluered(256),
                   ifelse(colors == "greenred", gplots::greenred(256),
                          ifelse(colors == "heat.colors", rev(grDevices::heat.colors(256)),
                                 grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(9, "RdBu")))(256))))
  } else if (length(color) == ncolour) {
    hcol <- color

  }else {
    stop('select from "bluered", "greenred", "heat.colors", and "BuRd", or others corresponding to
         categorical levels.')
  }



  # heatmap.2 ----
  if (squared == F & categorical == F) {
    # numerical data ----
    gplots::heatmap.2(
      as.matrix(d),
      col = hcol,
      scale = scale, # "none", "row", "column"
      dendrogram = draw, # "none", "row", "column", "both"
      Colv = cden, # Column dendrogram object
      Rowv = rden, # Row dendrogram object
      key = TRUE,
      keysize = 1,
      key.title = NA,
      symkey = FALSE,
      density.info = "none",
      trace = "none",
      margin = c(6,6),
      ...)

  } else if (squared == F & categorical == T) {
    # categorical data ----
    gplots::heatmap.2(
      as.matrix(d),
      col = hcol,
      scale = scale, # "none", "row", "column"
      dendrogram = draw, # "none", "row", "column", "both"
      Colv = cden, # Column dendrogram object
      Rowv = rden, # Row dendrogram object
      key = FALSE,
      #keysize = 1,
      #key.title = NA,
      symkey = FALSE,
      density.info = "none",
      trace = "none",
      margin = c(6,6),
      ...)

  } else {
    # squared matrix ----
    gplots::heatmap.2(
      as.matrix(d),
      symm = T,
      revC = T,
      col = hcol,
      scale = scale, # "none","row", "column"
      dendrogram = draw, # "none", "row", "column", "both"
      Colv = rden, # Column dendrogram object
      Rowv = rden, # Row dendrogram object
      key = TRUE,
      keysize = 1,
      key.title = NA,
      symkey = FALSE,
      density.info = "none",
      trace = "none",
      margin = c(6,6), #
      ...)
  }
}
