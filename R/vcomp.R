#' Compares the elements of two lists against each other and outputs a count table
#' @description Compares the elements of two lists against each other and outputs a count table
#' @usage vcomp(query, target, rate)
#' @param query list
#' @param target list
#' @param rate logical
#' @examples \dontrun{
#' set.seed(1)
#' l_target <- setNames(lapply(1:10, function(i) sample(letters, 5)),
#'                      nm = paste0("t",sprintf("%02d", 1:10)))
#' l_search <- setNames(lapply(1:3, function(i) sample(letters, 3)),
#'                      nm = paste0("s",sprintf("%02d", 1:3)))
#' res <- vcomp(query = l_search, target = l_target, rate = T)
#' @export
vcomp <- function(query, target, rate = F){
  cnt <- sapply(query, function(x) sapply(target, function(y) sum(y %in% x)))

  if (rate) {
    rate <- as.data.frame(cnt/sapply(target, length))
    data.frame(target=rownames(cnt),
               cbind(total = sapply(target, length), cnt),
               setNames(rate, paste0(names(rate), "_rate")),
               row.names = NULL)

  } else {
    data.frame(target=rownames(cnt),
               cbind(total = sapply(target, length), cnt),
               row.names = NULL)
  }
}
