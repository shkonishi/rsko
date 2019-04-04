#' Create colour code using ggplot2
#' @description Create defaoult colour code of ggplot2
#' @usage ggcols(n)
#' @param n numbers of colour code
#' @examples ## ggcols
#' cols <- ggcols(n = 10)
#' barplot(setNames(rep(1,10), cols), col = cols, las =2)
#' @export
ggcols <- function(n) {
  hues = seq(15, 375, length = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}
