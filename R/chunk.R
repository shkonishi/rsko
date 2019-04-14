#' Create a chunk list from an atomic vector.
#' @description Create chunk list from vector.
#' @usage chunk(x, n, r)
#' @param x atomic:
#' @param n integer: the length of each chunk elements
#' @param r logical: All of the chunks except for the last remainder are exactly 'n'.
#'  The default value is FALSE.
#'
#' @examples
#' v1 <- c(1, 1, 2, NA, 3, 3, 4, 4, NA )
#' v2 <- factor(v1)
#' chunk(v1, 4)
#' chunk(v1, 4, TRUE)
#' chunk(v2, 4)
#'
#' @export
chunk <- function(x, n, r = FALSE){
  if (r) {
    nchnk <- ceiling(seq_along(x)/n)
    split(x, nchnk)

  } else {
    nchnk <- ceiling(length(x)/n)
    split(x, cut(seq_along(x), nchnk, labels = FALSE))
  }

}



