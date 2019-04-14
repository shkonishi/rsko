#' The utility for handling factors
#' @name fct_utls
#' @rdname fct_utls
#'
#' @title The utility of handling factors
#'
#' @usage fct_add(x, v)
#'
#' @param x factor
#' @param v atomic
#'
#' @examples # unique length
#' x <- factor(c(1,3,5))
#' v <- 1:10
#' fct_add(x, v)
#' @export
fct_add <- function(x, v){
  lv <- levels(x)
  factor(x, levels = unique(rsko::char_sort(c(lv, v))))
}
