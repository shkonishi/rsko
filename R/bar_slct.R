#' Extract data.frame and barplot
#' @description Extract data.frame and barplot
#' @usage bar_slct(dat, mrgn, slct, target, pm, nm, yscale, gg, wrapcol, xlb, ylb, ...)
#' @param dat data.frame
#' @param mrgn integer 1 or 2 search row is 1, search col is 2
#' @param slct character
#' @param target character
#' @param pm character strings. A type of pattern match, as 'mgrep', 'mmatch', or 'match'.
#'   The default value is 'mmatch'
#' @param nm character vector, as name of labels of barplot.
#'   If it is 'NULL', names or rownames of data.frame will used as 'nm'.
#' @param yscale character 'free' or 'fixed'. The default value is 'free'
#' @param gg logical default value is FALSE
#' @param wrapcol integer
#' @param xlb character
#' @param ylb character
#' @param ... other barplot options
#' @examples
#' \dontrun{
#' # sample data
#' zdat <- rsko::zconv(iris, -5, 1)
#' nfpkm <- rskodat::nfpkm[1:36,]
#' kodat <- rskodat::kodat
#' cpdat <- rskodat::cpdat
#' cge100 <- rskodat::cge100
#'
#' # search strings
#' slct1 <- c('1', '51', '101')
#' slct2 <- c('gene1', 'gene10', 'gene150')
#' slct3 <- c('K07230', 'K02030', 'K02073')
#' slct4 <- c('100770489', '100689463', '100774853')
#'
#' # subset rows of data.frame
#' par(mar=c(6, 4, 2, 1), mfrow=c(1,3))
#' res1 <- rsko::bar_slct(zdat[-5], mrgn=1, slct=slct1, target=rownames(zdat), yscale='fixed')
#' res2 <- rsko::bar_slct(zdat[-5], mrgn=1, slct=slct1, target=rownames(zdat), yscale='fixed', gg=TRUE)
#' print(res2)
#'
#' # subset columns of data.frame
#' res3 <- rsko::bar_slct(dat=nfpkm[-1:-4], mrgn=2, slct=slct2,
#'  target=names(nfpkm[-1:-4]), nm=nfpkm$id)
#' res4 <- rsko::bar_slct(dat=nfpkm[-1:-4], mrgn=2, slct=slct2,
#'  target=names(nfpkm[-1:-4]), gg=TRUE, xlb = 'sample', ylb = 'fpkm')
#'
#' # subset rows of data.frame
#' par(mfrow=c(1,3))
#' res4 <- rsko::bar_slct(dat=kodat, mrgn=1, slct=slct3, target=rownames(kodat))
#' res5 <- rsko::bar_slct(dat=kodat, mrgn=1, slct=slct3, target=rownames(kodat), yscale='fixed')
#' res6 <- rsko::bar_slct(dat=kodat, mrgn=1, slct=slct3, target=rownames(kodat), gg=TRUE,
#'                  yscale = 'fixed', wrapcol=3, xlb='condition', ylb='relative exp.')
#'
#' # partial match of search strings　
#' res7 <- rsko::bar_slct(dat=cge100, mrgn=1, slct=slct4, target=rownames(cge100),
#'                  pm = 'mgrep', yscale = 'fixed')
#'
#' # result of degview genes
#' res.degv <- rskodat::res.degv
#' res8 <- lapply(1:nrow(res.degv), function(i){
#'                   subg <- unlist(strsplit(unlist(res.degv[i, 'kos']), ';'))
#'                   rsko::bar_slct(dat=kodat, mrgn=1, slct=subg, yscale = 'fixed',
#'                   target=rownames(kodat), gg=TRUE)
#'                   })
#' # result of degview cpds
#' res9 <- lapply(1:nrow(res.degv), function(i){
#'                   subg <- unlist(strsplit(unlist(res.degv[i, 'cpds']), ';'))
#'                   rsko::bar_slct(dat=cpdat, mrgn=1, slct=subg, target=rownames(cpdat),
#'                                  yscale = 'fixed', gg=TRUE)
#'                   })
#' }
#' @importFrom dplyr %>%
#' @export

bar_slct <- function(dat, mrgn, slct, target = names(dat), pm = "mmatch",
                     nm = NULL, yscale = "free", gg = FALSE,
                     wrapcol = 3, xlb = "key", ylb = "value", ...) {

  # argument check: dat ----
  if (!any(class(dat) == "data.frame")) {
    stop("'dat' must be a data.frame.")
  }

  # argument check: nm ----
  if (is.null(nm) & mrgn == 2) {
    nm <- factor(rownames(dat), rownames(dat))
  }
  if (!is.null(nm) & mrgn == 2) {
    if (length(nm) != nrow(dat)) {
      stop("'nm' must be the same as nrow of 'dat'.")
    } else if (length(unique(nm)) != nrow(dat) & gg == TRUE) {
      stop("'names' of barplot are unique labels fo ggplot")
    }
  }

  # functions of pattern match
  mgrep <- function(patterns, x, na.rm = FALSE, value = FALSE){
    # Multiple pattern matching
    if (value == T) {
      res <- sapply(patterns, function(s) {
        if (any(grep(s, x))) { grep(s, x, value = T) } else {NA}
      }, simplify = F)

    } else {
      res <- sapply(patterns, function(s) {
        if (any(grep(s, x))) { grep(s, x) } else {NA}
      }, simplify = F)

    }
    # without no grepped pattern
    if (na.rm == T) res <- res[!is.na(res)]
    # return
    res
  }
  mmatch <- function(patterns, x, na.rm = FALSE){
    # Process multiple patterns with sapply
    res <- sapply(patterns, function(s){
      if (any(x %in% s)) {
        which(x %in% s)
      } else {
        NA
      }
    }, simplify = F )
    # without no grepped pattern
    if (na.rm == T) res <- res[!sapply(res, function(x) all(is.na(x)))]
    # return
    res
  }

  # select row or column by search strings 'slct' to names of data.frame or 'target' which length as ----
  if (mrgn == 1 & nrow(dat) == length(target)) {
    if (pm == "mmatch") {
      pos <- mmatch(slct, target, na.rm = T)
      slct.dat <- dat[unlist(pos),]

    } else if (pm == "mgrep") {
      # pos <- rsko::mstrings(slct, target, "mgrep", narm = T)
      pos <- mgrep(slct, target, na.rm = T)
      slct.dat <- dat[unlist(pos), ]

    } else if (pm == "match" & length(slct)) {
      pos <- match(slct, target)
      slct.dat <- dat[pos, ]
    }

  } else if (mrgn == 2 & ncol(dat) == length(target)) {
    if (pm == "mmatch") {
      pos <- mmatch(slct, target, na.rm = T)
      slct.dat <- dat[, unlist(pos)]
    } else if (pm == "mgrep") {
      pos <- mgrep(slct, target, na.rm = T)
      slct.dat <- dat[, unlist(pos)]
    } else if (pm == "match" & length(slct) == 1) {
      pos <- match(slct, target)
      slct.dat <- dat[, pos]

    }

  } else {
    stop("If 'mrgn' is 1, a nrow of 'dat' and length of 'target' must to be the same\n
         and if 'mrgn' is 2, a ncol of 'dat' and length of 'target' must to be the same.")
  }

  # argument check: yscale ---- create ylim ----
  if (yscale == "free" & mrgn == 1) {
    ylms <- lapply(1:nrow(slct.dat), function(i) {
      rsko::wrange(unlist(slct.dat[i, ]))
    })

  } else if (yscale == "free" & mrgn == 2) {
    ylms <- lapply(1:ncol(slct.dat), function(i) {
      rsko::wrange(unlist(slct.dat[, i]))
    })

  } else if (yscale == "fixed") {
    ylms <- rsko::wrange(unlist(slct.dat))

  }

  # base plot ----
  if (gg == F) {
    if (mrgn == 1) {
      if (yscale == "fixed") {
        ## margin 1, y-scale is fixed
        res <- lapply(seq(nrow(slct.dat)), function(i) {
          graphics::barplot(unlist(slct.dat[i, ]), ylim = ylms, las = 2, main = slct[i], ...)
        })

      } else if (yscale == "free") {
        ## margin 1, y-scale is free
        res <- lapply(seq(nrow(slct.dat)), function(i) {
          graphics::barplot(unlist(slct.dat[i, ]), ylim = ylms[[i]], las = 2, main = slct[i], ...)
        })

      }

    } else if (mrgn == 2 & ncol(dat) == length(target)) {
      if (yscale == "fixed") {
        ## margin 2, y-scale is fixed
        res <- lapply(seq(ncol(slct.dat)), function(i) {
          slct.v <- stats::setNames(unlist(slct.dat[i]), nm)
          graphics::barplot(slct.v, las = 2, ylim = ylms, main = slct[i], ...)
        })

      } else if (yscale == "free") {
        ## margin 2, y-scale is free
        res <- lapply(seq(ncol(slct.dat)), function(i) {
          slct.v <- stats::setNames(unlist(slct.dat[i]), nm)
          graphics::barplot(slct.v, las = 2, ylim = ylms[[i]], main = slct[i], ...)
        })
      }

    } else {
      stop("If 'mrgn' is 1, a nrow of 'dat' and length of 'target' must to be the same\n
           and if 'mrgn' is 2, a ncol of 'dat' and length of 'target' must to be the same.")
    }

  } else {
    # using ggplot2
    id = NULL
    key = NULL
    value = NULL
    if (mrgn == 1 & nrow(dat) == length(target)) {
      res <- data.frame(id = target[unlist(pos)], slct.dat, check.names = F) %>%
        tidyr::gather("key", "value", -id) %>%
        dplyr::mutate(id = factor(id, levels = slct), key = factor(key, levels = unique(key))) %>%
        ggplot2::ggplot(., ggplot2::aes(x = key, y = value)) +
        ggplot2::theme_light(base_size = 15) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
        ggplot2::facet_wrap(~id, ncol = wrapcol, scales = yscale) +
        ggplot2::labs(x = xlb, y = ylb)

    } else if (mrgn == 2 & ncol(dat) == length(target)) {
      res <- slct.dat %>% data.frame(nm, ., check.names = F) %>%
        tidyr::gather("key", "value", -1) %>%
        dplyr::mutate(key = factor(key,
                                   levels = slct)) %>%
        ggplot2::ggplot(., ggplot2::aes(x = nm, y = value)) +
        ggplot2::theme_light(base_size = 15) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
        ggplot2::facet_wrap(~key, ncol = wrapcol, scales = yscale) +
        ggplot2::labs(x = xlb, y = ylb)

    } else {
      stop("If 'mrgn' is 1, a nrow of 'dat' and length of 'target' must to be the same\n
             and if 'mrgn' is 2, a ncol of 'dat' and length of 'target' must to be the same.")
    }

  }
  return(res)

}



