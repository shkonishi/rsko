#' Count the number of matches in a list elements.
#' @description Count the number of matches between all combinations of two list elements
#' @usage str_num(query, target, rate)
#' @param query list
#' @param target list
#' @param rate logical
#' @examples \dontrun{
#' set.seed(1)
#' l_target <- lapply(1:10, function(i) sample(letters, 5)) %>%
#'     setNames(., paste0("t",sprintf("%02d", 1:10)))
#' l_search <- lapply(1:3, function(i) sample(letters, 3)) %>%
#'     setNames(., paste0("s",sprintf("%02d", 1:3)))
#'
#' res <- str_num(query = l_search, target = l_target, rate = T)
#' }
#' @export
str_num <- function(query, target, rate = F){
  if (rate) {
    sapply(query, function(x) sapply(target, function(y) sum(y %in% x))) %>%
      cbind(., total = sapply(target, length)) %>%
      data.frame %>%
      tibble::rownames_to_column(var = "target") %>%
      dplyr::mutate_at( 2:(2 + length(query) - 1),  list(rate = ~./total ) )

  } else {
    sapply(query, function(x) sapply(target, function(y) sum(y %in% x))) %>%
      cbind(., total = sapply(target, length)) %>%
      data.frame %>%
      tibble::rownames_to_column(var = "target")

  }
}
