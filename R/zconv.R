#' Z-conversion
#' @description Z-conversion
#' @usage zconv(dat, columns, mrgn, cntr, scl)
#' @param dat A list of ggplot objects
#' @param columns select columns
#' @param mrgn 1 or 2
#' @param cntr an argument of base::scale. The default value is TRUE
#' @param scl an argument of base::scale. The default value is TRUE
#' @return converted data.frame
#' @examples ##
#' zrdat <- zconv(iris, -5, 1)
#' zcdat <- zconv(iris, -5, 2)
#' head(apply(zrdat[-5], 1, mean)); head(apply(zrdat[-5], 1, sd))
#' head(apply(zcdat[-5], 2, mean)); head(apply(zcdat[-5], 2, sd))
#' @export
zconv <- function(dat, columns, mrgn, cntr = TRUE, scl = TRUE) {
  if (mrgn == 1) {
    dat[columns] <- t(apply(dat[, columns], mrgn, scale, center = cntr, scale = scl))
  } else {
    dat[columns] <- apply(dat[, columns], mrgn, scale, center = cntr, scale = scl)
  }

  return(dat)
}

