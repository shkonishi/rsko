#' Create a transposed data.frame with names and columns of factor
#'
#' Create a transposed data.frame with names and columns of factor
#'
#' @usage td(dat, nm, fctr_dat)
#' @param dat data.frame
#' @param nm character
#' @param fctr_dat data.frame or vector
#' @examples \dontrun{
#' dat <- rskodat::fpkm
#' nm <- rownames(dat)
#' fctr_dat <- rsko::splt_dat(names(dat), "_", c("cnd","time","rep"))
#' tdat <- td(dat, nm, fctr_dat)
#' tdat[1:6]
#' }
#' @export
td <- function(dat, nm = NULL, fctr_dat=NULL){
  # names of 'dat' set as a column of transposed data.frame
  lab <- names(dat)

  # the 'nm' names of a transposed data.frame
  if (is.null(nm)) {
    nm <- paste0("V", 1:nrow(dat))
  } else if (length(nm) != nrow(dat)) {
    stop(" the length of 'nm' and nrow 'dat' must be the same.")
  }

  # transpose and as data.frame with 'nm' as columns name.
  tdat <- stats::setNames(as.data.frame(t(dat), check.names = F), nm)

  # add fctr_columns
  if (!is.null(fctr_dat)) {
    cbind(fctr_dat, tdat)
  } else {
    cbind(lab, tdat)
  }
}
