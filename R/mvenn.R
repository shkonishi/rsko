#' Multiple venn diagram, between all combinations of two list elements.
#' @description Multiple venn diagram, between all combinations of two list elements. If only one list,
#' all combinations in the list elements, using the function gplots::venn.
#' @usage mvenn(x, y, show.plt, ...)
#' @param x list
#' @param y list [default = NULL]
#' @param show.plt logical: draw plot or not. [default=F]
#' @param ... other gplots::venn options
#' @examples \dontrun{
#' # sample data
#' set.seed(1)
#' n <- 3; n_1 <- c(5,6,4); n_2 <- c(4,6,4)
#' v1 <- setNames(lapply(1:n, function(i){
#'  sample(LETTERS[1:10], n_1[i])
#' }), paste0("e",as.character(1:n)) )
#'
#' v2 <- setNames(lapply(1:n, function(i){
#'  sample(LETTERS[1:10], n_2[i])
#' }) , paste0("f",as.character(1:n)))
#'
#' # venn
#' mvenn(x = v1, show.plt = T)
#' mvenn(x = v1, y = v2)
#' }
#'
#' @importFrom magrittr %$% %>%
#' @export
mvenn <- function(x, y = NULL, show.plt = F, ...){
  if (is.null(y)) {
    idx <- utils::combn(seq_along(x), 2) %>% split(., col(.))
    res_venn <- lapply(seq_along(idx), function(i){
      gplots::venn(x[idx[[i]]], show.plot = show.plt)
    })

  } else if (!is.null(names(x)) & !is.null(names(y))) {
    idx <- expand.grid(names(x), names(y)) %>%
      as.matrix %>%
      split(., row(.))

  } else {
    idx <- expand.grid(1:length(x), 1:length(y)) %>%
      as.matrix %>%
      split(., row(.))

  }

  if (!is.null(y)) {
    res_venn <- lapply(idx, function(v){
      gplots::venn(list(x[[v[1]]], y[[v[2]]]), show.plot = show.plt)
    })
  } else {
    res_venn <- lapply(idx, function(v){
      gplots::venn(list(x[[v[1]]], x[[v[2]]]), show.plot = show.plt)
    })
  }


  dif_A <- NULL; dif_B <- NULL

  res <- do.call(rbind, lapply(res_venn, function(res){
    c(dif_A = res[3, 1], dif_B = res[2, 1], intersect = res[4, 1])}) ) %>%
    cbind(comb = sapply(idx, paste, collapse = "_"), .) %>%
    as.data.frame(., stringsAsFactors = F) %>%
    dplyr::mutate_at(-1, as.numeric) %>%
    dplyr::mutate(., dif_A_rate = round(dif_A/(dif_A + intersect), 2),
           dif_B_rate = round(dif_B/(dif_B + intersect), 2) ,
           insecA_rate = round(intersect/(dif_A + intersect), 2),
           insecB_rate = round(intersect/(dif_B + intersect), 2)
    )
  return(res)
}
