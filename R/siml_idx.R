#' Measure of the similarity index between two vectors
#' @description This functions returns a numeric vector length 1.
#' @usage siml_idx(x, y, method, vorder)
#' @param x a character or binary vector
#' @param y a character or binary vector
#' @param method One of the "jaccard", "simpson", "dice", "smc", and "tanimoto"
#' @param vorder The 'x' and 'y' are oredered vectors or not.
#' @return a numeric vector
#' @importFrom dplyr %>%
#' @examples
#' # character vector
#' v1 <- c("a", "b", "c", "d")
#' v2 <- c("a", "b", "e", "f")
#' siml_idx(v1, v2, method="jaccard")
#' siml_idx(v1, v2, method="simpson")
#' siml_idx(v1, v2, method="smc")
#' siml_idx(v1, v2, method="dice")
#'
#' # Two sets of different size
#' v1 <- c("a", "b", "c")
#' v2 <- c("a", "b", "d", "e")
#' siml_idx(v1, v2, "smc")
#'
#' # ordered categorical vector
#' ov1 <- c("3", NA,  NA,  "3", "2", "2")
#' ov2 <- c("3", "3", "2", "2", "2", "3")
#' siml_idx(ov1, ov2, "jaccard", TRUE)
#'
#' # binary vector
#' bin1 <- c(1, 1, 1, 1, 0, 0, 0)
#' bin2 <- c(1, 1, 0, 0, 1, 1, 0)
#' siml_idx(bin1, bin2, method="jaccard")
#' siml_idx(bin1, bin2, method="simpson")
#' siml_idx(bin1, bin2, method="smc")
#' siml_idx(bin1, bin2, method="dice")
#'
#' # numeric vector
#' n1 <- c(4.47, 4.35, 4.94, 3.09)
#' n2 <- c(3.86, 5.95, 5.48, 5.18)
#' siml_idx(n1, n2, method="tanimoto")
#' siml_idx(n1, n2, method="cosine")
#'
#' @export
siml_idx <- function(x, y, method, vorder = FALSE){
  # if x and y are character vector, convert to binary vector ----
  if (is.character(x) & is.character(y)){
    ## create dummy variable from ordered vector and convert to binary vector
    if (vorder == T & length(x)==length(y)){
      sp_mat <- rsko::sparse_mat(list(x,y), T)
      x <- sp_mat[[2]] %>% .[!is.na(.) & !is.na(sp_mat[[3]])]
      y <- sp_mat[[3]] %>% .[!is.na(.) & !is.na(sp_mat[[2]])]

    ## character sets
    } else {
      sp_mat <- rsko::sparse_mat(list(x,y))
      x <- sp_mat[[2]]
      y <- sp_mat[[3]]

    }
  }

  # if x and y are binary vector and NA containing ----
  if (all(stats::na.omit(c(x,y)) %in% c(1, 0))){
    pos <- !is.na(x) & !is.na(y)
    x <- x[pos]; y <- y[pos]
  }

  # if x and y are binary vector ----
  if (all(stats::na.omit(c(x, y)) %in% c(1, 0))){
    if (method=="jaccard"){
      siml <- sum(x==1 & y==1)/(sum(x)+sum(y)-sum(x==1 & y==1))

    } else if (method=="simpson"){
      siml <- sum(x==1 & y==1)/ifelse(sum(x) >= sum(y), sum(y), sum(x))

    } else if (method=="dice"){
      siml <- 2*sum(x==1 & y==1)/(sum(x)+sum(y))

    } else if (method == "smc"){
      siml <- sum(x==y)/(length(x))

    } else if (method == "tanimoto"){
      siml <- as.vector(x%*%y/(x%*%x + y%*%y - x%*%y))

    } else if (method == "cosine"){
      siml <- as.vector(x%*%y/(sqrt(x%*%x)*sqrt(y%*%y)))

    } else {
      stop("method is one of these 'jaccard', 'simpson', 'dice', 'smc', and 'cosine'")
    }

  # numeric vector ----
  } else if (!all(c(x, y) == 1 | c(x, y) == 0) & is.numeric(c(x, y))){
    if (method=="tanimoto"){
      siml <- as.vector(x%*%y/(x%*%x + y%*%y - x%*%y))

    } else if (method == "cosine"){
      siml <- as.vector(x%*%y/(sqrt(x%*%x)*sqrt(y%*%y)))

    } else {
      stop("method is one of these \'tanimoto\' and \'cosine\'")
    }
  }
  return(siml)
}



