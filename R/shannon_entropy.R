#' Calcurate Shannon Index
#' @description Calcurate Shannon Index
#' @usage shannon_entropy(x)
#' @param x vector
#' @examples
#' rsko::shannon_entropy(rpois(10, 2))
#' rsko::shannon_entropy(iris[-5])
#' @export
shannon_entropy <- function(x){
  shannon <- function(x){
    Pi <- x/sum(x)
    - sum(Pi*log2(Pi), na.rm = T)
  }

  if (is.numeric(x)) {
    return(shannon(x))

  } else if (is.list(x) & all(sapply(x, is.double)) | is.matrix(x) & is.double(x)) {
    return(sapply(1:ncol(x), function(i){ shannon(x[,i])}))

  } else {
    stop("'x' must to be a numeric vector, a data.frame, or a matrix")

  }

}
