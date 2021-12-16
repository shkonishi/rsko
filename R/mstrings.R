#' Multiple patterns or values matching
#' @name mstrings
#' @rdname mstrings
#'
#' @title Multiple patterns or values matching
#'
#' @usage mgrep(pat, x, na.rm, value)
#' @usage mmatch(pat, x, na.rm)
#' @usage mgrep_ext(pat, x, y, na.rm)
#' @usage mmatch_ext(pat, x, y, na.rm)
#' @usage mmatch_sub(pat, x, y, missing)
#'
#' @param pat atomic: pattern strings
#' @param x atomic: target strings
#' @param y atomic: strings for extraction or substitution
#' @param na.rm logical
#' @param value logical
#' @param missing atomic: the default value is NA
#'
#' @examples \dontrun{
#'
#' # Multiple patterns matching
#' x <- c("A,D", "E,F", "G,C", "C,H")
#' p1 <- c("A", "B", "C", "D")
#' p2 <- c("X", "Y")
#' p3 <- "A"
#' mgrep(p1, x)
#' mgrep(p1, x, F, T)
#' mgrep(p1, x, T, F)
#' mgrep(p1, x, T, T)
#'
#' # If none of the patterns match, a list consisting of NAs or an empty list is returned.
#' mgrep(p2, x)
#' mgrep(p2, x, na.rm = T)
#'
#' # Return as a list even if there is only one pattern
#' mgrep(p3, x)
#'
#' # Multiple values matching
#' x <- c("A", "B", "C", "D", "B", "C", "D", "A", "B", "C")
#' p4 <- c("A", "B", "C", "D", "X")
#' p5 <- c("H", "J", "K")
#' mmatch(p4, x)
#' mmatch(p4, x, na.rm = T)
#' mmatch(p5, x)
#' mmatch(p5, x, na.rm = T)
#'
#' # Multiple values mathing and extract correspondig data.
#' x <- c("A,D", "E,F", "G,C", "C,H")
#' y <- c("v1", "v2", "v3", "v4")
#' p <- c("A", "B", "C", "D")
#' dat <- data.frame(x, y, stringsAsFactors = FALSE)
#' mgrep_ext(p, dat$x, dat$y)
#'
#' # Multiple values matching and extract corresponding data.
#' x <- c("A", "B", "C", "D", "B", "C", "D", "A", "B", "C")
#' p <- c("A", "B", "C", "D", "X")
#' y <- rpois(10, 5)
#' dat <- data.frame(x, y, stringsAsFactors = FALSE)
#' mmatch_ext(p, x, y)
#'
#' # Multiple values matching and substitute to corresponding values.
#' x <- c("A", "B", "E", "C", "D", "F", "B", "C", "D", "G", "A", "B", "C")
#' p <- c("A", "B", "C", "D", "X")
#' y <- c("a", "b", "c", "d", "x")
#' mmatch_sub(p, x, y, missing = NA)
#'
#' }
#'
#' @rdname mstrings
#' @export
mgrep <- function(pat, x, na.rm = FALSE, value = FALSE){
  # argument check: pat
  if (!is.atomic(x) | !is.atomic(pat)) stop("'x' and 'pat' are must be atomic.")

  # Multiple pattern matching
  if (value == T) {
    res <- sapply(pat, function(s) {
      if (any(grep(s, x))) { grep(s, x, value = T) } else {NA}
    }, simplify = F)

  } else {
    res <- sapply(pat, function(s) {
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
mmatch <- function(pat, x, na.rm = FALSE){
  # Process multiple patterns with sapply
  res <- sapply(pat, function(s){
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
mgrep_ext <- function(pat, x, y = x, na.rm = TRUE){
  # argument check: the length of 'x' and 'y' must be the same
  if (length(x) != length(y)) {
    stop("the length of 'x' and 'y' must be the same.")
  }

  # Multiple patterns matching and extract corresponding values.
  res <- sapply(pat, function(s) {
    if (any(grep(s, x))) { y[grepl(s, x)] } else {NA}
  }, simplify = F)

  # without no grepped pattern
  if (na.rm == T) res <- res[!is.na(res)]

  # return
  res

}

#' @rdname mstrings
#' @export
mmatch_ext <- function(pat, x, y, na.rm = TRUE){
  # Process multiple values with sapply
  res <- sapply(pat, function(s){
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

#' @rdname mstrings
#' @export
mmatch_sub <- function(pat, x, y, missing = NA){
  # argument check: the length of 'pat' and 'y' must be the same
  if (length(pat) != length(y)) {
    stop("the length of matched patterns and 'subst' must be the same")
  }

  # no matching elements convert to missing value
  x <- ifelse(x %in% pat, x, missing)

  # the list of positions value matching
  pos <- rsko::mmatch(pat, x, na.rm = T)

  # convert all elements of 'x' to corresponding values
  new_x <- x
  invisible(lapply(seq_along(pos), function(i) new_x[pos[[i]]] <<- y[i] ))
  new_x
}

