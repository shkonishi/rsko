#' Sliding window for vector elements
#' @description Sliding window for vector elements
#' @usage slw(x, w)
#' @param x target vector
#' @param w integer: window size
#' @return list
#' @examples #
#' v1 <- 1:13
#' v2 <- letters[1:13]
#' idx1 <- slw(v1, 3)
#' idx2 <- slw(v2, 3)
#' @export
slw <- function(x, w) {
  rv <- utils::head(seq_along(x), -w + 1)
  res <- lapply(rv, function(i) x[i:(i + w - 1)])
  if (w != 1) {
    stats::setNames(res, paste(utils::head(x, -w + 1), utils::tail(x, -w + 1), sep = ":"))
  } else {
    stop("Window size must be >1.")
  }
}
