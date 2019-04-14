#' The utility for handling data.frame
#'
#' The utility for handling data.frame
#'
#' @name df_utls
#' @rdname df_utls
#'
#' @usage td(dat, nm, fctr_dat)
#' @usage subd(dat, idlist, idx, mrgn)
#'
#' @param dat data.frame
#'
#' @param nm character
#' @param fctr_dat data.frame or vector
#'
#' @param idlist list: a list for extracting subset
#' @param idx atomic: a column of index for splitting the data.frame
#' @param mrgn integer: margin of data.frame
#'
#' @examples \dontrun{
#' # Create a transposed data.frame with names and columns of factor
#' dat <- rskodat::fpkm
#' nm <- rownames(dat)
#' fctr_dat <- rsko::splt_dat(names(dat), "_", c("cnd","time","rep"))
#' tdat <- td(dat, nm, fctr_dat)
#' tdat[1:6]
#'
#' # Subset with a list for index column
#' slst <- list(toyota = c("Toyota Corolla", "Toyota Corona"),
#'     mazda = c("Mazda RX4", "Mazda RX4 Wag"))
#' subd(mtcars, slst, rownames(mtcars), 1)
#' }
#' @rdname df_utls
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

#' @rdname df_utls
#' @export
subd <- function(dat, idlist, idx, mrgn ){
  # argument check
  if (mrgn == 1 & nrow(dat) != length(idx)) {
    stop("the length of 'idx' and 'nrow(dat)' must be the same. ")
  } else if (mrgn == 2 & ncol(dat) == length(idx)) {
    stop("the length of 'idx' and 'ncol(dat)' must be the same. ")
  }

  lapply(idlist, function(x){
    if (mrgn == 2) {
      dat[idx %in% x ]
    } else if (mrgn == 1) {
      dat[idx %in% x,]
    }
  })
}
