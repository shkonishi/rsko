#' Data summarization and ggplot of multivariate statistics by multiple factors
#' @description This functions returns the ggplot object
#' @usage ggfactorial(dat, column, y, x, reps, ycols, xcols, func, facet, type)
#' @param dat data frame, matrix, or vector; if 'dat' is a data frame or matrix,
#'     vector and missing 'func', return boxplot objects only.
#' @param column column number
#' @param y atomic vector; groups of experiment. e.g. c("setA","setA","setA","setB","setB","setB").
#'     y_fctr is a first factor and its levels are same or less than x_fctr as second factor.
#' @param x atomic vector; groups of experiment. e.g. c(1,2,3,1,2,3),
#' @param reps groups of replicate experiment(optional)
#' @param ycols colors corresponding to levels of y_fctr
#' @param xcols colors corresponding to levels of x_fctr
#' @param func calcurate statistics for margin 1 of 'dat'. (optional)
#' @param facet logical; if TRUE, additional ggplot objects using facet_wrap are created.
#'     if 'type' is 'all', could not adding facet_wrap.
#' @param type type of ggplot select from "point", "segment", "line", and "all". The default value is "point".
#' @return List containing  ggplot objects
#' @examples # # sample dat
#' \dontrun{
#' dat1 <- rskodat::dat1
#' dat2 <- rskodat::dat2
#' dat1norep <- rskodat::dat1norep
#' dat2norep <- rskodat::dat2norep
#'
#' # data.frame input
#' res1 <- ggfactorial(dat = dat1, column = -1:-4, y = strains, x = days,
#'                     facet = TRUE, func = mean, type = "point")
#' do.call(gridExtra::grid.arrange, c(res1, list(ncol = 2)))
#'
#' # single column data.frame input
#' res2 <- ggfactorial(dat = dat2, column = -1:-4, y = strains, x = days,
#'                     reps = reps, type = "line")
#' do.call(gridExtra::grid.arrange, c(res2, list(ncol = 2)))
#'
#' # single column data.frame input
#' res3 <- ggfactorial(dat = dat2norep, column =5, y=strains, x = days, type = "segment")
#' do.call(gridExtra::grid.arrange, c(res3, list(ncol = 2)))
#'
#' # data.frame input and manual colour setting
#' col2 <- c(1,2); col3 <- c(1,2,4)
#' res4 <- ggfactorial(dat = dat1, column=-1:-4, y = strains, x = days,
#'                     ycols = col2, xcols = col3,
#'                     facet = TRUE, func = mean, type = "point")
#' do.call(gridExtra::grid.arrange, c(res4, list(ncol = 2)))
#'
#' # type is 'all'
#' res5 <- ggfactorial(dat = dat1, column =-1:-4, y = strains, x = days, reps = reps,
#'                     fun = mean, type = "all")
#' do.call(gridExtra::grid.arrange, c(res5, list(ncol = 2)))
#'
#' # none replicate factor, and function is none
#' res6 <- ggfactorial(dat = dat1norep, column=-1:-4, y = strains, x_fctr = days,
#'                     type = "none")
#' }
#' @export
ggfactorial <- function(dat, column, y, x, reps=NULL,
                        ycols=NULL, xcols=NULL,
                        func=NULL, facet=NULL, type = "point") {

  # arguments check: 'dat' is matrix, data.frame, or vector ----
  if (is.matrix(dat) | is.data.frame(dat) ) {
    fctr_len <- nrow(dat)
  } else if (is.atomic(dat)) {
    fctr_len <- length(dat)
  } else {
    stop("The 'dat' is a data frame, matrix, or vector.")
  }

  # arguments check: 'type' ----
  types <- c("segment", "point", "line", "all", "none")
  if (!any(types %in% type ) & !is.null(type)) {
    stop('Choose type from one of them "segment", "point", "line", "all", "none"')
  }

  # arguments check: substitute 'x' and 'y' and converte to factor  ----
  x <- substitute(x); y <- substitute(y)
  x_fctr <- dat[[x]]
  y_fctr <- dat[[y]]

  if (!is.factor(x_fctr)) {
    x_fctr <- factor(as.character(x_fctr), unique(as.character(x_fctr)))
  }
  if (!is.factor(y_fctr)) {
    y_fctr <- factor(as.character(y_fctr), unique(as.character(y_fctr)))
  }

  # arguments check: 'xcols' and 'ycols' ----
  if (!is.null(ycols) & !length(ycols) == nlevels(y_fctr)) {
    stop("length of 'ycols' and levels of 'y_fctr' must to be the same.")
  }
  if (!is.null(xcols) & !length(xcols) == nlevels(x_fctr)) {
    stop("length of 'xcols' and levels of 'x_fctr' must to be the same.")
  }

  # argument check: 'rep_fctr' additional option and force grouping for replicate ----
  gp_x <- NULL;  gp_y <- NULL;
  gp_yx <- data.frame(cbind(y_fctr, x_fctr)) %>%
    dplyr::group_by(y_fctr) %>%
    dplyr::mutate(gp_y = dplyr::row_number()) %>%
    dplyr::ungroup(y_fctr) %>%
    dplyr::group_by(x_fctr) %>%
    dplyr::mutate(gp_x = dplyr::row_number()) %>%
    dplyr::ungroup(x_fctr) %>%
    dplyr::select(gp_y, gp_x)

  # argument check: the length of these objects must be same ----
  reps <- substitute(reps)
  if (!is.null(reps)) {
    rep_fctr <- dat[[reps]]
  } else {
    rep_fctr <- rep(NA, fctr_len)
  }

  if (is.data.frame(dat) | is.matrix(dat)) {
    args_len <- list(length(x_fctr), length(y_fctr), nrow(dat), length(rep_fctr))
    if (!all(sapply(args_len, identical, length(x_fctr)))) {
      stop('You should give arguments are same length of \"y_fctr\", \"x_fctr\",
           additinal factors and ncol of data frame or matrix')
    }
  }else{
    args_len <- list(length(x_fctr), length(y_fctr), length(dat), length(rep_fctr))
    if (!all(sapply(args_len, identical, length(x_fctr)))) {
      stop('You should give arguments are same length of \"y_fctr\", \"x_fctr\",
           additinal factors and ncol of data frame or matrix')
    }
  }

  # argument check: facet ----
  if (is.null(facet)) {
    facet <- FALSE
  } else if (!is.logical(facet)) {
    stop("'facet' is a logical for creating facet_wrap object or not.")
  } else if (is.logical(facet) & type == "all") {
    stop("if 'type' option is 'all', could not adding facet_wrap.
         an argument 'facet' to be 'FALSE'. ")
  }

  # argument check: func and combined data creating.
  d <- dat[column]
  # boxplot with tidy data ----
  key <- NULL; value <- NULL;
  tidy_dat <- data.frame(x_fctr, y_fctr, rep_fctr, d) %>%
    tidyr::gather(., key = "key", value = "value", 4:ncol(.))
  if (is.null(xcols)) {
    ggbx <-
      ggplot2::ggplot(tidy_dat, ggplot2::aes(x = y_fctr, y = value), group = y_fctr) +
      ggplot2::geom_boxplot(ggplot2::aes(fill = x_fctr)) +
      ggplot2::labs(x = "", fill = "") +
      ggplot2::theme_bw() +
      ggplot2::theme(text = ggplot2::element_text(size = 20))
  } else {
    ggbx <-
      ggplot2::ggplot(tidy_dat, ggplot2::aes(x = y_fctr, y = value), group = y_fctr) +
      ggplot2::geom_boxplot(ggplot2::aes(fill = x_fctr)) +
      ggplot2::scale_fill_manual(values = xcols) +
      ggplot2::labs(x = "", fill = "") +
      ggplot2::theme_bw() +
      ggplot2::theme(text = ggplot2::element_text(size = 20))
  }
  if (is.null(ycols)) {
    ggby <-
      ggplot2::ggplot(tidy_dat, ggplot2::aes(x = x_fctr, y = value), group = x_fctr) +
      ggplot2::geom_boxplot(ggplot2::aes(fill = y_fctr)) +
      ggplot2::labs(x = "", fill = "") +
      ggplot2::theme_bw() +
      ggplot2::theme(text = ggplot2::element_text(size = 20))
  } else {
    ggby <-
      ggplot2::ggplot(tidy_dat, ggplot2::aes(x = x_fctr, y = value), group = x_fctr) +
      ggplot2::geom_boxplot(ggplot2::aes(fill = y_fctr)) +
      ggplot2::scale_fill_manual(values = ycols) +
      ggplot2::labs(x = "", fill = "") +
      ggplot2::theme_bw() +
      ggplot2::theme(text = ggplot2::element_text(size = 20))
  }

  # argument check: func calcurate statistics as logng as d is data.frame or matrix ----
  if (!is.null(func) & (is.matrix(d) | is.data.frame(d))) {
    ## 'd' is data.frame or matirx
    value <- apply(d, 1, func)
    ## additional value for 'geom_segment'
    x_st <- c(0:length(unique(x_fctr)))[factor(x_fctr)] + 0.1
    x_ed <- c(0:length(unique(x_fctr)))[factor(x_fctr)] + 0.9
    y_st <- c(0:length(unique(y_fctr)))[factor(y_fctr)] + 0.1
    y_ed <- c(0:length(unique(y_fctr)))[factor(y_fctr)] + 0.9
    dx <- data.frame(y_fctr, x_fctr, rep_fctr, value, x_st, x_ed, y_st, y_ed, stringsAsFactors = F)

  } else if ( is.atomic(d)) {
    ## 'd' is atomic vector ----
    value <- d
    ## additional value for 'geom_segment' ----
    x_st <- c(0:length(unique(x_fctr)))[factor(x_fctr)] + 0.1
    x_ed <- c(0:length(unique(x_fctr)))[factor(x_fctr)] + 0.9
    y_st <- c(0:length(unique(y_fctr)))[factor(y_fctr)] + 0.1
    y_ed <- c(0:length(unique(y_fctr)))[factor(y_fctr)] + 0.9
    dx <- data.frame(y_fctr, x_fctr, rep_fctr, value = value, x_st, x_ed, y_st, y_ed, stringsAsFactors = F)
  } else if (ncol(d) == 1) {
    # 'd' is data.frame ----
    value <- d

    x_st <- c(0:length(unique(x_fctr)))[factor(x_fctr)] + 0.1
    x_ed <- c(0:length(unique(x_fctr)))[factor(x_fctr)] + 0.9
    y_st <- c(0:length(unique(y_fctr)))[factor(y_fctr)] + 0.1
    y_ed <- c(0:length(unique(y_fctr)))[factor(y_fctr)] + 0.9
    dx <- data.frame(tidy_dat, x_st, x_ed, y_st, y_ed, stringsAsFactors = F)


  } else if (is.null(func) & (is.matrix(d) | is.data.frame(d) & ncol(d) > 1)) {
        cat("If 'func' is null and 'dat[column]' is not a vector or  a data.frame from single column,
            you should use rsko::ggmat. ")
    return(list(box_x = ggbx, box_y = ggby))

  }

  # ggplot object of several types respectively  ----
  if (type == "segment") {
    # type is 'segment' ----
    if (is.null(ycols)) {
      ggsy <-
        ggplot2::ggplot(data = dx, ggplot2::aes(x = x_st, xend = x_ed, y = value, yend = value, colour = y_fctr)) +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid.major.x = ggplot2::element_line(size = 0.5, linetype = "dotted")) +
        ggplot2::labs(x = "", colour = "") +
        ggplot2::scale_x_continuous(breaks = dx$x_st + 0.4, labels = x_fctr) +
        ggplot2::geom_segment(ggplot2::aes(group = y_fctr), size = 1.5, alpha = 0.5) +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))
    } else {
      ggsy <-
        ggplot2::ggplot(data = dx, ggplot2::aes(x = x_st, xend = x_ed, y = value, yend = value, colour = y_fctr)) +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid.major.x = ggplot2::element_line(size = 0.5, linetype = "dotted")) +
        ggplot2::labs(x = "", colour = "") +
        ggplot2::scale_x_continuous(breaks = dx$x_st + 0.4, labels = x_fctr) +
        ggplot2::geom_segment(ggplot2::aes(group = y_fctr), size = 1.5, alpha = 0.5) +
        ggplot2::scale_color_manual(values = ycols) +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))
    }
    if (is.null(xcols)) {
      ggsx <-
        ggplot2::ggplot(data = dx, ggplot2::aes(x = y_st, xend = y_ed,
                                                y = value, yend = value, colour = x_fctr)) +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "", colour = "") +
        ggplot2::theme(panel.grid.major.x = ggplot2::element_line(size = 0.5, linetype = "dotted")) +
        ggplot2::scale_x_continuous(breaks = unique(dx$y_st + 0.4), labels = unique(dx$y_fctr)) +
        ggplot2::geom_segment(size = 1.5, alpha = 0.5) +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))
    } else {
      ggsx <-
        ggplot2::ggplot(data = dx, ggplot2::aes(x = y_st, xend = y_ed,
                                                y = value, yend = value, colour = x_fctr)) +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "", colour = "") +
        ggplot2::theme(panel.grid.major.x = ggplot2::element_line(size = 0.5, linetype = "dotted")) +
        ggplot2::scale_x_continuous(breaks = unique(dx$y_st + 0.4), labels = unique(dx$y_fctr)) +
        ggplot2::geom_segment(size = 1.5, alpha = 0.5) +
        ggplot2::scale_color_manual(values = xcols) +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))

    }

    # ggplot objects ----
    ggobj <- list(segment_y = ggsy, segment_x = ggsx)

  } else if (type == "point") {
    # type is 'point'----
    if (is.null(ycols)) {
      ggpy <-
        ggplot2::ggplot(data = dx, ggplot2::aes(x = x_fctr, y = value, colour = y_fctr)) +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "", colour = "") +
        ggplot2::geom_point(size = 5, alpha = 0.5) +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))
    } else {
      ggpy <-
        ggplot2::ggplot(data = dx, ggplot2::aes(x = x_fctr, y = value, colour = y_fctr)) +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "", colour = "") +
        ggplot2::geom_point(size = 5, alpha = 0.5) +
        ggplot2::scale_color_manual(values = ycols) +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))
    }
    if (is.null(xcols)) {
      ggpx <-
        ggplot2::ggplot(data = dx, ggplot2::aes(x = y_fctr, y = value, colour = x_fctr)) +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "", colour = "") +
        ggplot2::geom_point(size = 5, alpha = 0.5) +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))

    } else {
      ggpx <-
        ggplot2::ggplot(data = dx, ggplot2::aes(x = y_fctr, y = value, colour = x_fctr)) +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "", colour = "") +
        ggplot2::geom_point(size = 5, alpha = 0.5) +
        ggplot2::scale_color_manual(values = xcols) +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))
    }

    # ggplot objects ----
    ggobj <- list(point_y = ggpy, point_x = ggpx)

  } else if (type == "line" & all(is.na(rep_fctr))) {
    # type is 'line' with no replicate ----
    gglx <-
      ggplot2::ggplot(data = dx, ggplot2::aes(x = y_fctr, y = value, group = x_fctr)) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "", colour = "") +
      ggplot2::geom_point(ggplot2::aes(colour = x_fctr), size = 5, alpha = 0.5) +
      ggplot2::geom_line(size = 1, alpha = 0.3)

    ggly <-
      ggplot2::ggplot(data = dx, ggplot2::aes(x = x_fctr, y = value, colour = y_fctr, group = y_fctr)) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "", colour = "") +
      ggplot2::geom_line(size = 5, alpha = 0.5)

    # ggplot objects ----
    ggobj <- list(line_y = ggly, line_x = gglx)


  } else if (type == "line" & all(!is.na(rep_fctr))) {
    # line with replicate ----
    dx <- cbind(dx, gp_yx)
    ggly <-
      ggplot2::ggplot(data = dx, ggplot2::aes(x = y_fctr, y = value, colour = x_fctr, group = gp_y)) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "", colour = "") +
      #ggplot2::geom_point(size =5, alpha = 0.5) +
      ggplot2::geom_line(size = 1, alpha = 0.3) +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))

    gglx <-
      ggplot2::ggplot(data = dx, ggplot2::aes(x = x_fctr, y = value, colour = y_fctr, group = gp_x)) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "", colour = "") +
      # ggplot2::geom_point(size =5, alpha = 0.5) +
      ggplot2::geom_line(size = 1, alpha = 0.5) +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))

    # ggplot objects ----
    ggobj <- list(line_y = ggly, line_x = gglx)

  } else if (type == "all" & all(is.na(rep_fctr))) { # all no replicate----
    # segment ----
    ggsy <-
      ggplot2::ggplot(data = dx, ggplot2::aes(x = x_st, xend = x_ed, y = value, yend = value, colour = y_fctr)) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_line(size = 0.5, linetype = "dotted")) +
      ggplot2::labs(x = "", colour = "") +
      ggplot2::scale_x_continuous(breaks = dx$x_st + 0.4, labels = x_fctr) +
      ggplot2::geom_segment(ggplot2::aes(group = y_fctr), size = 1.5, alpha = 0.5) +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))

    ggsx <-
      ggplot2::ggplot(data = dx, ggplot2::aes(x = y_st, xend = y_ed, y = value, yend = value, colour = x_fctr)) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "", colour = "") +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_line(size = 0.5, linetype = "dotted")) +
      ggplot2::scale_x_continuous(breaks = unique(dx$y_st + 0.4), labels = unique(dx$y_fctr)) +
      ggplot2::geom_segment(size = 1.5, alpha = 0.5) +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))

    # point ----
    ggpy <-
      ggplot2::ggplot(data = dx, ggplot2::aes(x = x_fctr, y = value, colour = y_fctr)) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "", colour = "") +
      ggplot2::geom_point(size = 5, alpha = 0.5) +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))

    ggpx <-
      ggplot2::ggplot(data = dx, ggplot2::aes(x = y_fctr, y = value, colour = x_fctr)) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "", colour = "") +
      ggplot2::geom_point(size = 5, alpha = 0.5) +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))

    # line ----
    ggly <-
      ggplot2::ggplot(data = dx, ggplot2::aes(x = x_fctr, y = value, colour = y_fctr, group = y_fctr)) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "", colour = "") +
      ggplot2::geom_line(size = 5, alpha = 0.5) +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))

    gglx <-
      ggplot2::ggplot(data = dx, ggplot2::aes(x = y_fctr, y = value, group = x_fctr)) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "", colour = "") +
      ggplot2::geom_point(ggplot2::aes(colour = x_fctr), size = 5, alpha = 0.5) +
      ggplot2::geom_line(size = 1, alpha = 0.5) +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))


    ggobj <- list(ggpy, ggpx, ggsy, ggsx, ggly, gglx)

  } else if (type == "all" & !all(is.na(rep_fctr))) { # all with replicate ----
    # segment ----
    dx <- cbind(dx, gp_yx)
    ggsy <-
      ggplot2::ggplot(data = dx, ggplot2::aes(x = x_st, xend = x_ed, y = value, yend = value, colour = y_fctr)) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_line(size = 0.5, linetype = "dotted")) +
      ggplot2::labs(x = "", colour = "") +
      ggplot2::scale_x_continuous(breaks = dx$x_st + 0.4, labels = x_fctr) +
      ggplot2::geom_segment(ggplot2::aes(group = y_fctr), size = 1.5, alpha = 0.5) +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))

    ggsx <-
      ggplot2::ggplot(data = dx, ggplot2::aes(x = y_st, xend = y_ed,
                                              y = value, yend = value, colour = x_fctr)) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "", colour = "") +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_line(size = 0.5, linetype = "dotted")) +
      ggplot2::scale_x_continuous(breaks = unique(dx$y_st + 0.4), labels = unique(dx$y_fctr)) +
      ggplot2::geom_segment(size = 1.5, alpha = 0.5) +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))
    # point ----
    ggpy <-
      ggplot2::ggplot(data = dx, ggplot2::aes(x = x_fctr, y = value, colour = y_fctr)) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "", colour = "") +
      ggplot2::geom_point(size = 5, alpha = 0.5) +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))
    ggpx <-
      ggplot2::ggplot(data = dx, ggplot2::aes(x = y_fctr, y = value, colour = x_fctr)) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "", colour = "") +
      ggplot2::geom_point(size = 5, alpha = 0.5) +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))

    # line with replicate ----
    ggly <-
      ggplot2::ggplot(data = dx, ggplot2::aes(x = x_fctr, y = value, colour = y_fctr, group = gp_y)) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "", colour = "") +
      #ggplot2::geom_point(size =5, alpha = 0.5) +
      ggplot2::geom_line(size = 1, alpha = 0.3) +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))

    gglx <-
      ggplot2::ggplot(data = dx, ggplot2::aes(x = y_fctr, y = value, colour = x_fctr, group = gp_x)) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "", colour = "") +
      # ggplot2::geom_point(size =5, alpha = 0.5) +
      ggplot2::geom_line(size = 1, alpha = 0.5) +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))

    # all ggplot objects -----
    ggobj <- list(ggpy, ggpx, ggsy, ggsx, ggly, gglx)
  }

  # addition of facet_grid ----
  if (facet == TRUE & length(ggobj) == 2 & type != "line") {
    ncoly <- ifelse(nlevels(y_fctr) %% 2 == 0, 2, 3)
    ncolx <- ifelse(nlevels(x_fctr) %% 2 == 0, 2, 3)

    ggy_fac <- ggobj[[1]] + ggplot2::facet_wrap(~y_fctr, ncol = ncoly)
    ggx_fac <- ggobj[[2]] + ggplot2::facet_wrap(~x_fctr, ncol = ncolx)
    ggobj <- c(ggobj, stats::setNames(c(list(ggy_fac, ggx_fac)), c("fct_y", "fct_x")))

  } else if (facet == TRUE & length(ggobj) == 2 & type == "line") {
    ncoly <- ifelse(nlevels(y_fctr) %% 2 == 0, 2, 3)
    ncolx <- ifelse(nlevels(x_fctr) %% 2 == 0, 2, 3)

    ggy_fac <- ggobj[[1]] + ggplot2::facet_wrap(~x_fctr, ncol = ncolx)
    ggx_fac <- ggobj[[2]] + ggplot2::facet_wrap(~y_fctr, ncol = ncoly)
    ggobj <- c(ggobj, list(ggy_fac, ggx_fac))
  }

  # return ----
  return(c(stats::setNames(c(list(ggby, ggbx)), c("box_x", "box_y")), ggobj))
}
