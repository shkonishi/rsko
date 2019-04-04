#' bindfill
#' @description bindfill
#' @usage bindfill(x, margin, fill)
#' @param x target list
#' @param margin character: "c" as column or "r" as row.
#' @param fill fill strings: default value is NA
#' @return list
#' @examples #
#' v <- list(letters[1:4], letters[5:7], letters[8:16])
#' res1 <- rsko::bindfill(v)
#' res2 <- rsko::bindfill(v, fill = "")
#' @export
bindfill <- function(x, margin = "c",  fill = NA){
  # argument check: x
  if (class(x) != "list") {
    cat("arugument 'x', must be a list")
    stop
  }

  # legnth of the list elements
  mlen <- max(sapply(x, length))

  # fill the list and covert to data.frame
  if (margin == "c") {
    bindfilled_x <- do.call(cbind, lapply(x, function(y) c(y, rep(fill, mlen - length(y)))))
  } else if (margin == "r") {
    bindfilled_x <- do.call(rbind, lapply(x, function(y) c(y, rep(fill, mlen - length(y)))))
  }
  return(bindfilled_x)
}
