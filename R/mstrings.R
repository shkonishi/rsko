#' Multiple patterns or values matching
#' @name mstrings
#' @rdname mstrings
#'
#' @title Multiple patterns or values matching
#'
#' @usage mgrep(patterns, x, na.rm, value)
#' @usage mmatch(patterns, x, na.rm)
#' @usage mgrep_ext(patterns, x, y, na.rm)
#' @usage mmatch_ext(patterns, x, y, na.rm)
#'
#' @param patterns atomic
#' @param x atomic
#' @param y atomic
#' @param na.rm logical
#' @param value logical
#'
#' @examples \dontrun{
#' ## Multiple patterns matching
#' x <- c("A,D", "E,F", "G,C", "C,H")
#' p1 <- c("A", "B", "C", "D")
#' p2 <- c("X", "Y")
#' p3 <- "A"
#'
#' mgrep(p1, x)
#' mgrep(p1, x, F, T)
#' mgrep(p1, x, T, F)
#' mgrep(p1, x, T, T)
#'
#' ## If none of the patterns match, a list consisting of NAs or an empty list is returned.
#' mgrep(p2, x)
#' mgrep(p2, x, na.rm = T)
#'
#' ## Return as a list even if there is only one pattern
#' mgrep(p3, x)
#'
#' ## Multiple values matching
#' p4 <- c("A", "B", "C", "D", "X")
#' p5 <- c("H", "J", "K")
#' v2 <- c("A", "B", "C", "D", "B", "C", "D", "A", "B", "C")
#' mmatch(p4, v2)
#' mmatch(p4, v2, na.rm = T)
#' mmatch(p5, v2)
#' mmatch(p5, v2, na.rm = T)
#'
#' ## Multiple values mathing and extract correspondig data.
#' p <- c("A", "B", "C", "D")
#' x <- c("A,D", "E,F", "G,C", "C,H")
#' y <- c("v1", "v2", "v3", "v4")
#' dat <- data.frame(x, y, stringsAsFactors = FALSE)
#'
#' mgrep_ext(p, dat$x, dat$y)
#'
#' ## Multiple values matching and extract corresponding data.
#' p <- c("A", "B", "C", "D", "X")
#' x <- c("A", "B", "C", "D", "B", "C", "D", "A", "B", "C")
#' y <- rpois(10, 5)
#' dat <- data.frame(x, y, stringsAsFactors = FALSE)
#'
#' mmatch_ext(p, x, y)
#' }
#'
#' @rdname mstrings
#' @export
mgrep <- function(patterns, x, na.rm = FALSE, value = FALSE){
  # argument check: patterns
  if (!is.atomic(x) | !is.atomic(patterns)) stop("'x' and 'patterns' are must be atomic.")

  # Multiple pattern matching
  if (value == T) {
    res <- sapply(patterns, function(s) {
      if (any(grep(s, x))) { grep(s, x, value = T) } else {NA}
    }, simplify = F)

  } else {
    res <- sapply(patterns, function(s) {
      if (any(grep(s, x))) { grep(s, x) } else {NA}
    }, simplify = F)

  }

  # without no grepped pattern
  if (na.rm == T) res <- res[!is.na(res)]

  # return
  res

}

#' @rdname mstrings
#' @export
mmatch <- function(patterns, x, na.rm = FALSE){
  # Process multiple patterns with sapply
  res <- sapply(patterns, function(s){
    if (any(x %in% s)) {
      which(x %in% s)
    } else {
      NA
    }
  }, simplify = F )


  # without no grepped pattern
  if (na.rm == T) res <- res[!is.na(res)]

  # return
  res
}

#' @rdname mstrings
#' @export
mgrep_ext <- function(patterns, x, y = x, na.rm = TRUE){
  # argument check: the length of 'x' and 'y' must be the same
  if (length(x) != length(y)) {
    stop("the length of 'x' and 'y' must be the same.")
  }

  # Multiple patterns matching and extract corresponding values.
  res <- sapply(patterns, function(s) {
    if (any(grep(s, x))) { y[grepl(s, x)] } else {NA}
  }, simplify = F)

  # without no grepped pattern
  if (na.rm == T) res <- res[!is.na(res)]

  # return
  res

}

#' @rdname mstrings
#' @export
mmatch_ext <- function(patterns, x, y, na.rm = TRUE){
  # Process multiple values with sapply
  res <- sapply(patterns, function(s){
    if (any(x %in% s)) {
      y[which(x %in% s)]
    } else {
      NA
    }
  }, simplify = F )

  # without no matched values
  if (na.rm == T) res <- res[!is.na(res)]

  # return
  res

}

