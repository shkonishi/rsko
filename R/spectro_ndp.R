#' parsing a result file of nanodrop
#' @description A tsv file of nanodrop output.
#' @usage spectro_ndp(fp, smp, facet_col)
#' @param fp nanodrop result file path
#' @param smp character: sample names
#' @param facet_col integer: default value is 0L
#' @return data.frame
#' @importFrom dplyr %>%
#' @examples # arguments
#' fp1 <- system.file("extdata/nanodrop_out1.tsv", package="rskodat")
#' smp1 <- c("YL_N_1","YL_N_2", "ML_N_1","ML_N_2")
#'
#' # execution
#' res1 <- spectro_ndp(fp1, smp1, 2L)
#' # res2 <- spectro_ndp(fp1, NULL, 2L)
#' # res3 <- spectro_ndp(fp1, NULL, 0L)
#' do.call(gridExtra::grid.arrange, c(res1$gg_list, list(ncol = 4)))
#'
#' @export

spectro_ndp <- function(fp, smp = NULL, facet_col = 0L) {
  # file read
  ndrop <- readLines(fp)
  str <- "\\u30b5\\u30f3\\u30d7\\u30eb"  # created by `stringi::stri_escape_unicode()`
  st <- grep(stringi::stri_unescape_unicode(str), ndrop)
  ed <- c((st - 1)[-1], length(ndrop))
  res <- mapply(function(x, y) ndrop[x:y], st, ed)

  # argument check: smp
  if (!is.null(smp) & !identical(length(smp), length(res))) {
    stop("Sample number is different from argument 'smp'.")
  } else if (!is.null(smp) & identical(length(smp), length(res))) {
    nm_smp <- smp
  } else if (is.null(smp)) {
    nm_smp <- sapply(res, "[", 1)
  }
  # argument check: facet_col
  if (!is.integer(facet_col)) stop("'facet_col' must be an integer")


  # split lines per sample
  lst_al <- lapply(seq_along(res), function(i) {
    x <- res[[i]]
    x[nchar(x) != 0] %>%
      .[-1:-grep("//QSpecEnd:", .)] %>%
      strsplit(., "\t") %>%
      data.frame(.) %>%
      t() %>%
      data.frame(., row.names = NULL, stringsAsFactors = F) %>%
      stats::setNames(., c("wl", nm_smp[i]))
  })

  # merge all samples to one data.frame ----
  absorbance = NULL
  sample = NULL
  wl = NULL
  dat <- Reduce(function(x, y) {
    merge(x, y, by = "wl")
    }, lst_al) %>%
    tidyr::gather(., key = "sample", value = "absorbance", -1) %>%
    dplyr::mutate_at(dplyr::vars(c(1, 3)), dplyr::funs(as.numeric)) %>%
    dplyr::mutate(sample = factor(sample, levels = nm_smp))

  # calcurate abs ratio ----
  `A260/A230` = NULL; `A260/A280` = NULL; `nuc(ng/uL)`= NULL
  absratio <- dat %>% dplyr::group_by(sample) %>%
    dplyr::filter(wl %in% c(230, 260, 280)) %>%
    dplyr::mutate(`A260/A280` = absorbance[wl == 260] / absorbance[wl == 280],
                  `A260/A230` = absorbance[wl == 260] / absorbance[wl == 230],
                  `nuc(ng/uL)` = 40*absorbance[wl == 260] ) %>%
    dplyr::distinct(sample, `nuc(ng/uL)`, `A260/A280`, `A260/A230`)

  # plot ----
  ## splitFacet function
  splitFacet <- function(x) {
    facet_vars <- names(x$facet$params$facets)         # 1 facet vars
    x$facet    <- ggplot2::ggplot()$facet              # 2
    datasets   <- split(x$data, x$data[facet_vars])    # 3
    new_plots  <- lapply(datasets,function(new_data) { # 4
      x$data <- new_data
      x})
  }

  ## sample name ----
  if (facet_col != 0L) {
    if (is.null(smp)) {
      gg <- ggplot2::ggplot(dat, ggplot2::aes(x = wl, y = absorbance)) +
        ggplot2::theme_minimal(base_family = "HiraKakuPro-W3") +
        ggplot2::geom_line() +
        ggplot2::labs(x = "wavelength(nm)") +
        ggplot2::facet_wrap(~sample, ncol = facet_col)

    } else if (!is.null(smp)) {
      gg <- ggplot2::ggplot(dat, ggplot2::aes(x = wl, y = absorbance)) +
        ggplot2::theme_minimal() +
        ggplot2::geom_line() +
        ggplot2::labs(x = "wavelength(nm)") +
        ggplot2::facet_wrap(~sample, ncol = facet_col)
    }
    gg_list <- splitFacet(gg)

    return(list(data = dat, absratio = absratio, gg_list = gg_list))

  } else {
    if (is.null(smp)) {
      gg <- ggplot2::ggplot(dat, ggplot2::aes(x = wl, y = absorbance, colour = sample)) +
        ggplot2::theme_minimal(base_family = "HiraKakuPro-W3") +
        ggplot2::labs(x = "wavelength(nm)") +
        ggplot2::geom_line()

    }else if (!is.null(smp)) {
      gg <- ggplot2::ggplot(dat, ggplot2::aes(x = wl, y = absorbance, colour = sample)) +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "wavelength(nm)") +
        ggplot2::geom_line()
    }
    return(list(data = dat, absratio = absratio, gg = gg))

  }
  return(list(data = dat, absratio = absratio, gg = gg))

}
