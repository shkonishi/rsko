#' Measure of the similarity and create of similarity or distance matrix
#' @description This functions returns a matrix of the similarity or distance.
#' The similarity measure is selected from "Jaccard", "Simpson", "smc", and "Dice".
#' If an argument of 'dat' consisting numeric values, select from "Tanimoto" or "cossine".
#' These measured from all combinations of list or columns of data frame consisting character vectors.
#'
#' @usage siml_mat(dat, method, vorder, type, mtype)
#' @param dat A list of vectors, data.frame or matrix
#' @param method One of the "jaccard", "simpson", "dice" , "smc", "tanimoto", and "cosine".
#'     In case of continuous values, use "tanimoto" as extende jaccard or "cosine".
#' @param vorder logical: The element of 'dat' consists of an orderd vector or not.
#' @param type Chose output matrix type "distance" or "similarity". The default value is "distance".
#' @param mtype Chose output matrix type "lower", "upper", "both". The default value is "lower".
#'
#' @return  Matrix of similarities or distances (Dij = 1-Sij), and sparse matrix
#' @examples
#' # A list which composed of character vectors
#' v1 = c("a", "b", "c", "d")
#' v2 = c("b", "d", "a", "c")
#' v3 = c("a", "b", "c", "e")
#' v4 = c("b", "c", "e", "f")
#' v5 = c("f", "h", "i", "j")
#' v6 = c("a", "e", "f", "g")
#' x <- list(v1,v2,v3,v4,v5,v6); x <- setNames(x, paste0("v",1:6))
#' siml.jc <- siml_mat(dat = x, method = "jaccard", type = "similarity")
#' siml.dc <- siml_mat(dat = x, method = "dice", type = "similarity")
#' dist.jc <- siml_mat(dat = x, method = "jaccard", type = "distance")
#' plot(hclust(as.dist(dist.jc$jaccard_distance)))
#'
#' # A list of different length
#' y <- list(v1 = head(v1, 3), v2 = v2, v3 = head(v3, 3), v4 = v4, v5 = v5, v6 = v6)
#' dist2.smp <- siml_mat(dat = y, method = "simpson", vorder = FALSE, type = "distance")
#'
#' # A list of ordered categorical vector
#' z <- lapply(1:6, function(i) setNames(sample(c("A","B","C"), 4, replace = TRUE), 1:4))
#' dist2.smc <- siml_mat(dat = z, method = "jaccard", vorder = TRUE, type = "distance")
#'
#' # A data frame which composed of character vectors
#' # In case of this data frame or list is converted to sparse matrix.
#' dat1 <- as.data.frame(x, stringsAsFactors = FALSE)
#' names(dat1) <- c("v1", "v2", "v3", "v4", "v5", "v6")
#' dist.jc2 <- siml_mat(dat = dat1, method = "jaccard", vorder = FALSE, type = "distance")
#' identical(dist.jc, dist.jc2)
#'
#' # A data frame which composed of numeric vectors
#' dat2 <- as.data.frame(t(iris[-5]), strngsAsFactors = FALSE)
#' names(dat2) <- paste0(iris$Species, 1:150)
#' dist_tani <- siml_mat(dat = dat2, method = "tanimoto", type = "distance")
#'
#' # Convert a lower triangular matrix 'as.dist' and hieralchical clustaring
#' plot(hclust(as.dist(dist.jc[[1]])))
#' @importFrom dplyr %>%
#' @export

siml_mat <- function(dat, method, vorder = FALSE, type ="distance", mtype="lower"){

  # argument check: dat is a list, a data frame, or a matrix ----
  class_dat <- c("data.frame", "matrix", "list")
  if (any(class_dat %in% class(dat))) {
    if (any(class(dat) %in% "matrix")) { # matrix convert to data frame
      dat <- data.frame(dat, stringsAsFactors = F)

    } else if (any(class(dat) %in% "list")) { # list convert to data frame
      if (length(unique(sapply(dat, length))) == 1) {
        dat <- data.frame(do.call(cbind, dat), stringsAsFactors = F)
      }
    }
  } else {
    stop("'dat' is  'data.frame', 'matrix', or 'list' class object")
  }

  # argument check: method ----
  sim_methods <- c("jaccard", "simpson", "dice" , "smc", "tanimoto", "cosine")
  if (!any(sim_methods %in% method)) {
    stop("'method' is one of these 'jaccard', 'simpson', 'dice', 'smc', 'tanimoto', and 'cosine'")
  }

  # argument check: type ----
  res_type <- c("distance", "similarity")
  if (!any(res_type %in% type)) {
    stop("argument 'type' is 'distance' or 'similarity'")
  }

  # combination matirx ----
  comb_mat <- utils::combn(1:length(dat), 2)

  # in case of character convert to sparse matrix ----
  if (all(sapply(dat, is.character)) & vorder == F) {
    sp_dat <- rsko::sparse_mat(dat)
    comb1 <- as.list(sp_dat[-1])[comb_mat[1,]]
    comb2 <- as.list(sp_dat[-1])[comb_mat[2,]]
    n <- ncol(sp_dat) - 1

  } else if (all(sapply(dat, is.character)) & vorder == T) {
    sp_dat <- rsko::sparse_mat(dat, vorder = T)
    comb1 <- as.list(sp_dat[-1])[comb_mat[1,]]
    comb2 <- as.list(sp_dat[-1])[comb_mat[2,]]
    n <- ncol(sp_dat) - 1

  } else {
    comb1 <- as.list(dat)[comb_mat[1,]]
    comb2 <- as.list(dat)[comb_mat[2,]]
    n <- length(dat)

  }

  # similarity index from all combination of list ----
  siml <- mapply(rsko::siml_idx, comb1, comb2, MoreArgs = list(method, vorder))

  if (type == "similarity" & all(sapply(dat, mode) == "character")) {
    ## similarity (character) ----
    siml.sim <- matrix(0, nrow = n, ncol = n)
    siml.sim[lower.tri(siml.sim)] <- siml
    dimnames(siml.sim)[[1]] <- names(dat)
    dimnames(siml.sim)[[2]] <- names(dat)

    res <- list(siml.sim, sp_dat) %>%
      stats::setNames(., c(paste0(method, "_similarity"), "sparse_matrix"))

  } else if (type == "distance" & all(sapply(dat, mode) == "character")) {
    ## distance (character) ----
    siml.dist <- matrix(0, nrow = n, ncol = n)
    siml.dist[lower.tri(siml.dist)] <- 1 - siml
    dimnames(siml.dist)[[1]] <- names(dat)
    dimnames(siml.dist)[[2]] <- names(dat)

    res <- list(stats::as.dist(siml.dist), sp_dat) %>%
      stats::setNames(., c(paste0(method, "_distance"), "sparse_matrix"))

  } else if (type == "distance" & all(sapply(dat, is.numeric))) {
    ## dsitance (numeric) ----
    siml.dist <- matrix(0, nrow = n, ncol = n)
    dimnames(siml.dist)[[1]] <- names(dat)
    dimnames(siml.dist)[[2]] <- names(dat)

    ## matrix upper lower triangle, or both ----
    if (mtype == "lower") {
      siml.dist[lower.tri(siml.dist)] <- 1 - siml

    } else if (mtype == "upper") {
      siml.dist[upper.tri(siml.dist)] <- 1 - siml

    } else if (mtype == "both") {
      siml.dist[lower.tri(siml.dist)] <- 1 - siml
      siml.dist <- t(siml.dist)
      siml.dist[lower.tri(siml.dist)] <- 1 - siml
    }

    # result ----
    print(paste0(method, "_distance"))
    res <- siml.dist

  } else if (type == "similarity" & all(sapply(dat, is.numeric))) {
    ## similarity (numeric) ----
    siml.sim <- matrix(0, nrow = n, ncol = n)
    siml.sim[lower.tri(siml.sim)] <- siml
    dimnames(siml.sim)[[1]] <- names(dat)
    dimnames(siml.sim)[[2]] <- names(dat)

    ## matrix upper lower triangle, or both ----
    if (mtype == "lower") {
      siml.sim[lower.tri(siml.sim)] <- siml

    } else if (mtype == "upper") {
      siml.sim[upper.tri(siml.sim)] <- siml

    } else if (mtype == "both") {
      siml.sim[lower.tri(siml.sim)] <- siml
      siml.sim <- t(siml.sim)
      siml.sim[lower.tri(siml.sim)] <- siml

    }

    print(paste0(method, "_similarity"))
    res <- siml.sim
  }

  return(res)
}
