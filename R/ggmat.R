#' Matrix plot using ggplot2
#' @description Matrix plot using ggplot2
#' @usage ggmat(dat, column, xfctr, lwd, mcols, col_fctr, labs, vselect, vcol, xlab, ylab, main)
#' @param dat data.frame
#' @param column select column of data.frame E.g. -1:-4
#' @param xfctr factor for xaxis
#' @param lwd integer for line width
#' @param mcols manual colour setting
#' @param col_fctr factor for coloured groups
#' @param labs character: text for plotting at additional area
#' @param vselect select variable names for highlight. if it was given, 'col_fctr' must be null.
#'    if 'vselect' was named vector, names of 'vselect' were factor for colours.
#' @param vcol gives vector only high light colors
#' @param xlab character for x-labels
#' @param ylab character for y-labels
#' @param main character for main title
#' @examples \dontrun{
#'
#' # data
#' dat1 <- rskodat::dat1norep
#' dat2 <- dat1[1:3,]
#'
#' # ggmat
#' rsko::ggmat(dat = dat1, column = -1:-4, xfctr = dat1$days, col_fctr = dat1$strains)
#' rsko::ggmat(dat = dat1, column = -1:-4, xfctr = dat1$days, col_fctr = dat1$strains,
#'     mcols = 1:2)
#' rsko::ggmat(dat = dat2, column = -1:-4, xfctr = dat2$days)
#'
#' # add text
#' library(dplyr)
#' dat <- Orange %>% mutate(Tree = as.character(Tree))
#' ggmat(dat, -1:-2, xfctr = dat$age, col_fctr = dat$Tree)
#' ggmat(dat, -1:-2, xfctr = dat$age, col_fctr = dat$Tree, labs = dat$Tree)
#'
#' ## Give variable names as vectors or named vectors for highlight lines.
#' slct1 <- c('X10', 'X5', 'X3')
#' slct2 <- setNames(slct1, c('first', 'second', 'second'))
#' slct3 <- setNames(names(dat2)[-1:-4], rep(c('I','II','III'), c(2,4,4)))
#' ## vector of line colors corresponding to variable name selected.
#' col1 <- c('red', 'blue','green')
#' col2 <- c('red', 'blue')
#' col3 <- c(0:2)[factor(names(slct3))]
#'
#' # ggmat
#' rsko::ggmat(dat = dat2, column = -1:-4, xfctr = dat2$days,
#'             col_fctr = NULL, vselect=slct1, vcol=col1)
#' rsko::ggmat(dat = dat2, column = -1:-4, xfctr = dat2$days,
#'             col_fctr = NULL, vselect=slct2, vcol=col2)
#' rsko::ggmat(dat = dat2, column = -1:-4, xfctr = dat2$days,
#'             col_fctr = NULL, vselect=slct3, vcol=col1)
#'
#' # summarised line
#' library(dplyr)
#' dat1 %>% tidyr::gather('k','v', -1:-4) %>%
#'  group_by(strains,days) %>%
#'  mutate(med_v=median(v)) %>%
#'  rsko::ggmat(., column=7, col_fctr = .$strains, xfctr = .$days)
#'
#' }
#' @export

ggmat <- function(dat, column = 1:ncol(dat), xfctr, lwd = 1,
                  mcols = NULL, col_fctr = NULL, labs = NULL,
                  vselect = NULL, vcol = NULL,
                  xlab = NULL, ylab = NULL, main = NULL) {

  # argument check: dat
  if (!is.data.frame(dat)) stop("'dat' must be data.frame.")

  # argument check: xfctr
  if (nrow(dat) != length(xfctr))
    stop("The length of 'xfctr' must be same as nrow of 'dat'. ")

  # select column from 'dat'
  dat <- dat[column]

  # defined global variant
  hl_fctr = NULL
  key = NULL
  value = NULL

  # mat lines
  if (!is.null(col_fctr) & length(col_fctr) == nrow(dat)) {# coloured with 'col_fctr'
    if (!is.null(labs)) {
      # data
      if (ncol(dat) ==1) {
        ggdat <- data.frame(labs, xfctr, col_fctr, dat) %>%
          tidyr::gather(., key = "key", value = "value", -1:-3)
      } else {
        ggdat <- data.frame(xfctr, col_fctr, dat) %>%
          tidyr::gather(., key = "labs", value = "value", -1:-2)
      }

      # additional plot area for drawing text
      if (is.numeric(xfctr)) {
        xlm <- c(min(xfctr), max(xfctr) + diff(range(xfctr)) * 0.2)
        mx <- max(xfctr)

      } else {
        xr <- seq_along(unique(xfctr))
        xlm <- c(min(xr), max(xr) + length(unique(xfctr)) * 0.2)
        mx <- levels(factor(xfctr))[nlevels(factor(xfctr))]
      }

      # ggplot
      res <-
        ggplot2::ggplot(ggdat, ggplot2::aes(x = xfctr, y = value, colour = col_fctr,
                                            group = interaction(col_fctr, labs))) +
        ggplot2::geom_line(size = lwd) +
        ggplot2::theme_minimal(base_size = 20) +
        ggplot2::coord_cartesian(xlim = xlm) +
        ggrepel::geom_text_repel(data = subset(ggdat, xfctr == mx),
                                 ggplot2::aes(label = labs),
                                 size = 6, nudge_x = 45, segment.color = NA) +
        ggplot2::theme(legend.position = "none") +
        ggplot2::labs(title = main, x = xlab, y = ylab, colour = "")

      # manual colour or not
      if (!is.null(mcols)) res + ggplot2::scale_color_manual(values = mcols)
      # 'xfctr' is character or not
      if (is.numeric(xfctr)) res + ggplot2::scale_x_continuous(breaks = xfctr)

    } else {## no labels
      # data
      ggdat <- data.frame(xfctr, col_fctr, dat) %>%
        tidyr::gather(., key = "labs", value = "value", -1:-2)

      # ggplot
      res <-
        ggplot2::ggplot(ggdat, ggplot2::aes(x = xfctr, y = value, colour = col_fctr,
                                            group = interaction(labs, col_fctr))) +
        ggplot2::geom_line(size = lwd) +
        ggplot2::theme_minimal(base_size = 20) +
        ggplot2::labs(title = main, x = xlab, y = ylab, colour = "")

      # manual colour
      if (!is.null(mcols)) res + ggplot2::scale_color_manual(values = mcols)

      # 'xfctr' is character or not
      if (is.numeric(xfctr)) res + ggplot2::scale_x_continuous(breaks = xfctr)

    }

  } else if (is.null(col_fctr)) {# no coloured and high lighted lines
    if (is.null(vselect)) {## no high lighted lines
      ggdat <- data.frame(xfctr, dat) %>%
        tidyr::gather(., key = "key", value = "value", -1)

      res <-
        ggplot2::ggplot(ggdat, ggplot2::aes(x = xfctr, y = value, group = key)) +
        ggplot2::geom_line(size = lwd) +
        ggplot2::theme_minimal(base_size = 20) +
        ggplot2::labs(title = main, x = xlab, y = ylab, colour = "")

    } else if (!is.null(vselect)) { ## high lighted lines for selected variable
      if (is.null(names(vselect))) { ##
        gdat <- data.frame(xfctr, dat) %>%
          tidyr::gather(., key = "key", value = "value", -1) %>%
          dplyr::mutate(hl_fctr = ifelse(key %in% vselect, key, "")) %>%
          dplyr::mutate(hl_fctr = factor(hl_fctr, levels = c(vselect, "")))

        if (!is.null(vcol) & length(vselect) == length(vcol)) {# Gives manual colours for high lighted lines by 'vcol'
          res <- gdat %>%
            ggplot2::ggplot(., ggplot2::aes(x = xfctr, y = value, colour = hl_fctr, group = key)) +
            ggplot2::geom_line(size = lwd) +
            ggplot2::scale_color_manual(values = c(vcol, "gray80")) +
            ggplot2::theme_minimal(base_size = 20) +
            ggplot2::labs(title = main, x = xlab, y = ylab, colour = "")

        } else if (is.null(vcol)) { # Default colours for high lighted lines.
          gdat %>% ggplot2::ggplot(., ggplot2::aes(x = xfctr, y = value, colour = hl_fctr, group = key)) +
            ggplot2::geom_line(size = lwd) +
            ggplot2::scale_color_manual(values = c(rsko::gg_cols(length(vselect)),  "gray80")) +
            ggplot2::theme_minimal(base_size = 20) +
            ggplot2::labs(title = main, x = xlab,  y = ylab, colour = "")

        } else {
          stop("length of 'vselect' and 'vcol' must be same")
        }

      } else {# High lighted lines with names of selected variable
        gdat <- data.frame(xfctr, dat) %>%
          tidyr::gather(., key = "key", value = "value", -1) %>%
          dplyr::mutate(hl_fctr = rsko::mmatch_sub(vselect, key ,names(vselect), "")) %>%
          dplyr::mutate(hl_fctr = factor(hl_fctr, levels = unique(c(names(vselect), ""))))

        ## High lighted colours using manual or default colours
        ## if 'vselect' was named vector, names of 'vselect' were factor for colours.
        if (!is.null(vcol) & length(unique(names(vselect))) == length(vcol)) {
          ## Gives manual colours for high lighted lines by 'vcol'.----
          res <- gdat %>% ggplot2::ggplot(., ggplot2::aes(x = xfctr, y = value, colour = hl_fctr, group = key)) +
            ggplot2::geom_line(size = lwd) +
            ggplot2::scale_color_manual(values = c(vcol, "gray80")) +
            ggplot2::theme_minimal(base_size = 20) +
            ggplot2::labs(title = main, x = xlab, y = ylab, colour = "")

        } else if (is.null(vcol)) {
          ## Default colours for high lighted lines. ----
          res <- gdat %>% ggplot2::ggplot(., ggplot2::aes(x = xfctr, y = value, colour = hl_fctr, group = key)) +
            ggplot2::geom_line(size = lwd) +
            ggplot2::scale_color_manual(values = c(rsko::gg_cols(length(vselect)), "gray80")) +
            ggplot2::theme_minimal(base_size = 20) +
            ggplot2::labs(title = main, x = xlab, y = ylab, colour = "")

        } else {
          stop("The names of 'vcol' must be the same as the unique length of 'vselect'")
        }
      }

    }
  }
  return(res)
}
