#' My R utility functions.
#' @name utls
#' @rdname utls
#'
#' @title My R utility functions.
#'
#' @usage ul(x)
#' @usage dup(x)
#' @usage char_sort(x)
#' @usage str_tail(x, n)
#' @usage str_head(x, n)
#' @usage fct_char(x)
#'
#' @param x atomic
#' @param n integer
#'
#' @examples # unique length
#' set.seed(12)
#' v1 <- sample(letters[1:5], size = 5, TRUE)
#' ul(v1)
#'
#' # return the list of position of duplicated value.
#' v2 <- sample(letters[1:5], size = 5, FALSE)
#' dup(v1)
#' dup(v2)
#'
#' # Sort string vectors in numerical order
#' v3 <- sort(paste0("V", as.character(1:13)))
#' char_sort(v3)
#'
#' # create factor with levels by numerical order
#' fct_char(v3)
#'
#' # head and tail for strings
#' v4 <- sapply(1:3, function(i) paste(sample(c("A","T","G","C"), 10, TRUE), collapse = ""))
#' str_head(v4, 3)
#' str_tail(v4, 3)
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

#' @rdname utls
#' @export
fct_char <- function(x){
  lv <- unique(x[order(nchar(x), x)])
  factor(x, levels = lv)
}


#' @rdname utls
#' @export
str_tail <- function(x, n) {
  if (n < 0)
    substring(x, 1 - n)
  else
    substring(x, nchar(x) - n + 1)
}

#' @rdname utls
#' @export
str_head <- function(x, n) {
  if (n < 0)
    substr(x, 1, nchar(x) + n)
  else
    substr(x, 1, n)
}
