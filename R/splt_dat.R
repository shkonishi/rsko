#' Split the character to data.frame
#'
#' Split the character to data.frame
#'
#' @usage splt_dat(x, splt, lab)
#' @param x vector
#' @param splt character. as an argument of 'split' for 'strsplit'
#' @param lab logical.
#' @examples \dontrun{
#'
#' # examples
#' x <- paste(LETTERS[1:5], letters[1:5], 1:5, sep="_")
#' splt_dat(x,"_", c("x","y","z"))
#'
#' # A list have different length of splitted elements
#' x1 <- c("xxx;yyy", "xyx", "xxy;yyx;xxz")
#' x2 <- setNames(x1, letters[1:3])
#' splt_dat(x1, ";")
#' splt_dat(x2, ";")
#' splt_dat(x2, ";", c("v","idx"))
#'
#' # A Named lists have different lengths for split elements.
#' equas <- rskodat::react_equas_ko
#' x <- equas$KOids[1:10] %>% setNames(., equas$Rid[1:10])
#' res <- splt_dat(x, ";", c("Kid", "Rid"))
#' split(res, res$Rid)
#'
#' }
#' @export
splt_dat <- function(x, splt, lab = NULL){
  # split the x by 'splt'
  spltx <- strsplit(x, splt)

  # the number of splitted elements
  n_elm <- unique(sapply(spltx, length))

  if (length(n_elm) == 1) { # the length of splitted elements are same
    # convert to data.frame
    d <- data.frame(do.call(rbind, spltx), stringsAsFactors = F)

    # add labels to the data.frame
    if (is.null(lab)) {
      lab <- paste0("v", 1:n_elm)
      stats::setNames(d, lab)
    } else if (length(lab) == n_elm) {
      stats::setNames(d, lab)
    } else {
      stop("the length of 'lab' and splitted elements must to be the same.")
    }

  } else {# different length of splitted elements
    if (is.null(names(spltx))) {
      spltx <- stats::setNames(spltx, 1:length(spltx))
      if (is.null(lab)) {
        utils::stack(spltx)
      } else if (length(lab) == 2) {
        melt_dat <- utils::stack(spltx)
        stats::setNames(melt_dat, lab)
      } else {
        stop("the length of 'lab' must to be the two.")
      }

    } else {
      if (is.null(lab)) {
        utils::stack(spltx)
      } else if (length(lab) == 2) {
        melt_dat <- utils::stack(spltx)
        stats::setNames(melt_dat, lab)
      } else {
        stop("the length of 'lab' must to be the two.")
      }
    }
  }
}

