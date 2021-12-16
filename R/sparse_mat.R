#' Create sparse matrix from a list of integer or character vectors
#' @description This functions returns a binary matrix
#' @usage sparse_mat(vlist, vorder)
#' @param vlist The vlist a list consists from character, integer or binary vectors.
#' @param vorder logical: If 'vlist' consists of an ordered vector or not. The default value is FALSE.
#' @return a data frame of sparse matrix which first column unique elements of a list
#' @examples
#' set.seed(1)
#' int_list <- lapply(1:6, function(i) abs(as.integer(rnorm(6, mean = i))))
#' bin_list <- lapply(1:6, function(i) round(runif(4, 0, 1)))
#' chr_list <- lapply(1:6, function(i) sample(letters, size = 4))
#' ochr_list <- lapply(1:6, function(i) setNames(sample(c('A','B','C'), 4, replace =TRUE), 1:4))
#' ochr_dat <- data.frame(ochr_list, stringsAsFactors = FALSE)
#' sparse_mat(int_list); sparse_mat(bin_list);
#' sparse_mat(chr_list); sparse_mat(ochr_list, vorder =TRUE)
#' sparse_mat(ochr_dat, vorder =TRUE)
#' @export
sparse_mat <- function(vlist, vorder = F) {

  # unique elements of all sets ----
  id <- sort(unique(unlist(vlist)))

  # If 'vlist' consists of an ordered vector, the size of each set must be the same ----
  if (vorder == T) {

    # size of an set
    vl <- unique(sapply(vlist, length))

    # sparse matrix from a list consist of ordered vectors
    if (length(vl) == 1) {
      vdummy <- as.vector(outer(id, 1:vl, function(x, y) paste(x, y, sep = "_")))
      binlist <- lapply(seq_along(vlist), function(i) {
        as.vector(do.call(rbind, lapply(id, function(x) {
          sapply(vlist[[i]], function(y) ifelse(y == x, 1, 0))
          })))
      })
      dat <- data.frame(vdummy, do.call(cbind, binlist))

    } else {
      stop("argument 'vlist' consist from ordered vectors, which length must to be all the same.")

    }

    # sparse matrix from a list consist of multiple sets -----
  } else {
    cnt_list <- lapply(vlist, function(x) {
      sapply(seq_along(id), function(i) sum(x %in% id[i]))
    })
    dat <- data.frame(id, do.call(cbind, cnt_list), stringsAsFactors = F)

    if (!is.null(names(vlist))) {
      dat <- stats::setNames(dat, c("id", names(vlist)))
    }

  }

  # return data.frame
  return(dat)

}
