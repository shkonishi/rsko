#' PCA with prcomp and plot of select pc score
#' @description This functions returns a result of prcomp object.
#' @usage pcat(dat, x, y, scaling, cols, txt, tpos, ...)
#' @param dat dataframe, matrix or prcomp object
#' @param x,y axis of pca score. 'x=1, y=2' means xaxis for PC1 score and yaxis for PC2 score
#' @param scaling prcomp option, scaling or not. The default is "TRUE"
#' @param cols A vector of manual colour
#' @param txt A size of text(optional)
#' @param tpos text position(optional)
#' @param ... Further plot arguments. E.g., pch
#' @return return prcomp object
#' @examples # minimum usage
#' res <- pcat(iris[-5], scaling = TRUE, cols = c(1:3)[iris$Species])
#' res <- pcat(iris[-5], x =1, y = 3, scaling = TRUE, cols = c(1:3)[iris$Species], pch = 20)
#' @export
pcat <- function(dat, x = 1, y = 2, scaling = TRUE, cols = NULL, txt = NULL, tpos = 1, ...) {
  # execution prcomp or definition of prcomp object
  if (class(dat) == "prcomp") {
    res <- dat
  } else if (is.matrix(dat) || is.data.frame(dat)) {
    res <- stats::prcomp(dat, scale = scaling)
  }

  # contribution ratio
  pov1 <- round(summary(res)$importance[2, x] * 100, digits = 2)
  pov2 <- round(summary(res)$importance[2, y] * 100, digits = 2)


  # ylim, xlim
  d <- abs(range(res$x[, c(1, 2)]))
  lim <- c(-max(d), max(d))

  # plot PCA score
  graphics::plot(res$x[, c(x, y)], xlim = lim, ylim = lim, col = cols, cex.main = 1,
       xlab = paste("PC", x, "(", pov1, "%)", sep = ""),
       ylab = paste("PC", y, "(", pov2, "%)", sep = ""), ...)

  ## display text
  if (!is.null(txt)) {
    if (!is.null(rownames(res$x))) {
      lab <- rownames(res$x)
    } else {
      lab <- 1:nrow(res$x)
    }
    graphics::text(res$x[, x], res$x[, y], pos = tpos, labels = lab, cex = txt)

  }
  ## abline
  graphics::abline(h = 0, v = 0, lty = 3)

  # return prcomp object
  if (class(dat) != "prcomp") {
    fL <- function(res){
      loading = sweep(res$rotation, MARGIN = 2, res$sdev, FUN = "*")
      npcs <- 1:3
      lapply(npcs, function(i){
        rownames(loading)[loading[,i] >= 0.8]
        }) %>%
        stats::setNames(., dimnames(loading)[[2]][npcs]) %>%
        .[sapply(., length)!=0]

    }

    return(list(res.prcmp = res, factor_loading = fL(res)))
  }
}
