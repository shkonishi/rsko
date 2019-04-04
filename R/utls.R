#' My R utility functions.
#' @name utls
#' @rdname utls
#'
#' @title My R utility functions.
#'
#' @usage ul(x)
#' @usage dup(x)
#' @usage char_sort(x)
#'
#' @param x atomic
#'
#' @examples # return the list of position of duplicated value.
#' set.seed(12)
#' v1 <- sample(letters[1:5], size = 5, TRUE)
#' v2 <- sample(letters[1:5], size = 5, FALSE)
#' v3 <- sort(paste0("V", as.character(1:13)))
#' ul(v1)
#' dup(v1)
#' dup(v2)
#' char_sort(v3)
#'
#' @rdname utls
#' @export
ul <- function(x) length(unique(x))

#' @rdname utls
#' @export
dup <- function(x){
  de <- sort(unique(x[duplicated(x)]))
  pos <- lapply(de, function(i) which(x %in% i ))
  stats::setNames(pos, de)
}

#' @rdname utls
#' @export
char_sort <- function(x){
  x[order(nchar(x), x)]
}
