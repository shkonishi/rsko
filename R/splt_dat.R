#' Split the character to data.frame
#'
#' Split the character to data.frame
#'
#' @usage splt_dat(x, splt, lab)
#' @param x vector
#' @param splt character. as an argument of 'split' for 'strsplit'
#' @param lab logical.
#' @examples \dontrun{ # examples
#' x <- paste(LETTERS[1:5], letters[1:5], 1:5, sep="_")
#' splt_dat(x,"_", c("x","y","z"))
#'
#' ## A list have different length of splitted elements
#' x <- c("xxx;yyy", "xyx", "xxy;yyx;xxz")
#' splt_dat(x, ";")
#'
#' ## A Named lists have different lengths for split elements.
#' equas <- rskodat::react_equas_ko
#' x <- equas$KOids[1:10] %>% setNames(., equas$Rid[1:10])
#' splt_dat(x, ";")
#' split(res, res$ind)
#' }
#' @export
splt_dat <- function(x, splt, lab = NULL){
  # argument check: x must character
  # argument check: length(lab) same as splitted elements, or two
  spltx <- strsplit(x, splt)
  n_elm <- unique(sapply(spltx, length))
  if (length(n_elm) == 1) {
    d <- data.frame(do.call(rbind, spltx), stringsAsFactors = F)
    if (is.null(lab)) {
      lab <- paste0("v", 1:n_elm)
      stats::setNames(d, lab)
    } else if (length(lab) == n_elm) {
      stats::setNames(d, lab)
    } else {
      stop("the length of 'lab' and splitted elements must to be the same.")
    }

  } else {
    if (is.null(names(spltx))) {
      spltx <- stats::setNames(spltx, 1:length(spltx))
      utils::stack(spltx)
    } else {
      utils::stack(spltx)
    }
  }
}

