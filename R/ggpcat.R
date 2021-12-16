#' PCA with prcomp and ggplot of select pc score
#' @description This functions returns the list consit of prcom object and ggplot object.
#' @usage ggpcat(dat, x, y, scaling, fctr1, fctr2, m.cols, txt, lgnd, main, cumpov)
#' @param dat dataframe, matrix or prcomp object
#' @param x,y axis of pca score. The default values 'x=1, y=2' means xaxis for PC1 score and yaxis for PC2 score
#' @param scaling logical: A prcomp argument for scaling or not. The default is "TRUE"
#' @param fctr1 factor: for colour of pca. The default value is rep(1,nrow(dat))
#' @param fctr2 factor: for x axis of pc-scores (optional)
#' @param m.cols A vector of manual colour for 'ggplot::scale_colour_manual'. (optional)
#' @param txt integer: size of adding text (optional). The default value is NULL.
#' @param lgnd legend position "none","left","right","bottom", and c(1,3). The default is "right".
#' @param main graph title(optional)
#' @param cumpov numeric. A cumulative proportion of variance for graphed pc-scores.
#'     The default value is 70
#' @return List of prcomp and ggplot object
#' @examples # minimum usage
#' res <- ggpcat(dat = iris[-5], x = 1, y = 2, scaling = TRUE, fctr1 = iris$Species)
#' summary(res$res.prcmp)
#' plot(res$gg)
#' plot(res$gg_pcs)
#'
#' # manual color
#' res <- ggpcat(iris[-5], fctr1 = iris$Species, m.cols = 1:3)
#'
#' \dontrun{
#' nfpkm <- rskodat::nfpkm
#' res2 <- ggpcat(nfpkm[-1:-4], fctr1=nfpkm$days, fctr2=nfpkm$days, cumpov=64)
#' res3 <- ggpcat(nfpkm[-1:-4], fctr1=nfpkm$runs, fctr2=nfpkm$days, cumpov=64)
#' res4 <- ggpcat(nfpkm[-1:-4], fctr1=nfpkm$runs, fctr2=nfpkm$days, m.cols = 1:3, cumpov=64)
#' }
#' @export
ggpcat <- function(dat, x = 1, y = 2, scaling = TRUE,
                   fctr1 = rep(1, nrow(dat)), fctr2 = NULL, m.cols = NULL,
                   txt = NULL, lgnd = "right", main = "", cumpov = 70) {

  # argument check: argument check and definition of prcomp object ----
  if (all(class(dat) == "prcomp")) {
    res <- dat
  } else if (all(class(dat) == "matrix") | any(class(dat) == "data.frame")) {
    if (scaling == T) {
      dat <- dat[, apply(dat, 2, sum) != 0]
      res <- stats::prcomp(dat, scale = scaling)

    } else {
      res <- stats::prcomp(dat, scale = scaling)

    }

  } else {
    stop("dat is a 'procomp' object or data frame")

  }

  # argument check: scaling
  if (!is.logical(scaling))
    stop("scaling is a logical value")

  # argument check: fctr1 as factor for colour
  if (!is.factor(fctr1)) {
    fctr1 <- factor(fctr1)
    if (length(fctr1) != nrow(dat)) {
      stop(" 'fctr1' is a factor for colours and same length as nrow dat")
    }
  }

  # argument check: fctr2
  if (!is.null(fctr2) & !is.factor(fctr2)) {
    if (length(fctr2) != nrow(dat)) {
      stop(" 'fctr1' is a factor for colours and same length as nrow dat")
    } else {
      fctr2 <- factor(fctr2)
    }
  }

  # argument check: m.cols
  if (!is.null(m.cols)) {
    if (nlevels(fctr1) != length(m.cols)) {
      stop("'nlevels of 'fctr1' and length of m.cols must to be the same.")
    }
  }


  # ggplot arguments pc-score1,2
  pov1 <- round(summary(res)$importance[2, x] * 100, digits = 2)
  pov2 <- round(summary(res)$importance[2, y] * 100, digits = 2)

  # x and y label
  labx <- paste0("PC", x, "(", pov1, "%)")
  laby <- paste0("PC", y, "(", pov2, "%)")

  # ggplot
  Samples <- fctr1
  score.xy <- data.frame(res$x[, c(x, y)], Samples)

  # legend options
  pos_lgnd <- c("none", "left", "right", "bottom", "top")
  if (!any(pos_lgnd %in% lgnd)) {
    if (!is.numeric(lgnd) & length(lgnd) != 2) {
      stop("'lgnd' is a legend position ")
    }
  }

  # ggplot 'txt' is NULL, and default colors
  if (is.null(txt) & is.null(m.cols)) {
    if (nlevels(fctr1) == 1) {
      gg <- ggplot2::ggplot(data = score.xy, ggplot2::aes(x = score.xy[,1], y = score.xy[, 2])) +
        ggplot2::geom_point(size = 5, alpha = 0.5) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = lgnd) +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1))) +
        ggplot2::labs(title = main, x = labx, y = laby, colour = "")

    } else {
      gg <- ggplot2::ggplot(data = score.xy, ggplot2::aes(x = score.xy[,1], y = score.xy[, 2], col = Samples)) +
        ggplot2::geom_point(size = 5, alpha = 0.5) +
        ggplot2::theme_bw() + ggplot2::theme(legend.position = lgnd) +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1))) +
        ggplot2::labs(title = main, x = labx, y = laby, colour = "")
    }


  } else if (is.null(txt) & !is.null(m.cols)) {
    # 'txt' is NULL, and manual color
    gg <- ggplot2::ggplot(data = score.xy, ggplot2::aes(x = score.xy[,1], y = score.xy[, 2], col = Samples)) +
      ggplot2::geom_point(size = 5, alpha = 0.5) +
      ggplot2::scale_color_manual(values = m.cols) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = lgnd) +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1))) +
      ggplot2::labs(title = main, x = labx, y = laby, colour = "")

  } else if (is.null(m.cols) & !is.null(txt)) {
    # display text and default color
    if (nlevels(fctr1) == 1) {
      gg <- ggplot2::ggplot(data = score.xy, ggplot2::aes(x = score.xy[,1], y = score.xy[, 2])) +
        ggrepel::geom_text_repel(label = rownames(score.xy), size = txt, segment.size = 0.1, show.legend = FALSE) +
        ggplot2::geom_point(size = 5, alpha = 0.5) + ggplot2::theme_bw() +
        ggplot2::theme(legend.position = lgnd) +
        ggplot2::labs(title = main, x = labx, y = laby, colour = "")

    } else {
      gg <- ggplot2::ggplot(data = score.xy, ggplot2::aes(x = score.xy[,1], y = score.xy[, 2], col = Samples)) +
        ggrepel::geom_text_repel(label = rownames(score.xy), size = txt, segment.size = 0.1, show.legend = FALSE) +
        ggplot2::geom_point(size = 5, alpha = 0.5) + ggplot2::theme_bw() +
        ggplot2::theme(legend.position = lgnd) + ggplot2::labs(title = main, x = labx, y = laby, colour = "")
    }

  } else {
    # display text and manual color
    gg <- ggplot2::ggplot(data = score.xy, ggplot2::aes(x = score.xy[,1], y = score.xy[, 2], col = Samples)) +
      ggrepel::geom_text_repel(label = rownames(score.xy), size = txt, segment.size = 0.1, show.legend = FALSE) +
      ggplot2::geom_point(size = 5, alpha = 0.5) +
      ggplot2::scale_color_manual(values = m.cols) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = lgnd) +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1))) +
      ggplot2::labs(title = main, x = labx, y = laby, colour = "")
  }

  # plot pc-scores label of pc-scores
  povs <- round(summary(res)$importance[2, ] * 100, digits = 2)
  cumpov.th <- which(cumsum(povs) > cumpov)[1]
  pcslab <- paste0(names(povs)[1:cumpov.th], "(", povs[1:cumpov.th], "%)")
  fct <- ceiling(cumpov.th/2)

  # ggplot pc-scores
  `PC-Scores` <- NULL
  if (is.null(fctr2)) {
    pcs_dat <- stats::setNames(data.frame(fctr1, res$x[, 1:cumpov.th]), c("fctr1", pcslab)) %>%
      tidyr::gather(., key = "key", value = "PC-Scores", -1, factor_key = T)

    if (is.null(m.cols)) {
      ggpcs <- pcs_dat %>%
        ggplot2::ggplot(., ggplot2::aes(x = fctr1, y = `PC-Scores`, colour = fctr1)) +
        ggplot2::geom_point(size = 5, alpha = 0.5) +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "", y = "PC-Scores", colour = "") +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1))) +
        ggplot2::facet_wrap(~key, ncol = fct)

    } else {
      ggpcs <- pcs_dat %>%
        ggplot2::ggplot(., ggplot2::aes(x = fctr1, y = `PC-Scores`, colour = fctr1)) +
        ggplot2::geom_point(size = 5, alpha = 0.5) +
        ggplot2::scale_color_manual(values = m.cols) +
        ggplot2::theme_bw() +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1))) +
        ggplot2::labs(x = "", y = "PC-Scores", colour = "") +
        ggplot2::facet_wrap(~key, ncol = fct)

    }


  } else {
    pcs_dat <- stats::setNames(data.frame(fctr1, fctr2, res$x[, 1:cumpov.th]), c("fctr1", "fctr2", pcslab)) %>%
      tidyr::gather(., key = "key", value = "PC-Scores", -1:-2, factor_key = T)

    if (is.null(m.cols)) {
      ggpcs <- pcs_dat %>%
        ggplot2::ggplot(., ggplot2::aes(x = fctr2, y = `PC-Scores`, colour = fctr1)) +
        ggplot2::geom_point(size = 5, alpha = 0.5) +
        ggplot2::theme_bw() +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1))) +
        ggplot2::labs(title = main, x = "", y = "PC-Scores",colour = "") +
        ggplot2::facet_wrap(~key, ncol = fct)

    } else {
      ggpcs <- pcs_dat %>%
        ggplot2::ggplot(., ggplot2::aes(x = fctr2, y = `PC-Scores`, colour = fctr1)) +
        ggplot2::geom_point(size = 5, alpha = 0.5) +
        ggplot2::scale_color_manual(values = m.cols) +
        ggplot2::theme_bw() +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1))) +
        ggplot2::labs(title = main, x = "", y = "PC-Scores",colour = "") +
        ggplot2::facet_wrap(~key, ncol = fct)

    }

  }

  # factor loading
  fL <- function(res, n = cumpov.th){
    loading = sweep(res$rotation, MARGIN = 2, res$sdev, FUN = "*")
    npcs <- 1:n
    rn <- dimnames(loading)[[1]]
    loading[,1:n] %>% tibble::as_tibble() %>% tibble::add_column(., rn, .before = 1)
  }

  f.loading = fL(res)

  # return the result of prcomp and ggplot object
  return(stats::setNames(list(res, gg, ggpcs, f.loading),
                         c("res.prcmp", "gg", "gg_pcs", "factor_loading")
                         ))
}
