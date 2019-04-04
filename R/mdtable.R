#' Convert data.frame to table for markdown format
#' @description Mapping transcriptome and/or metabolome data on KEGG pathway
#' @usage mdtable(dat, add_row)
#' @param dat data.frame or matrix
#' @param add_row logical: add rownames to column or not.
#' @examples \dontrun{ ##
#' dfiris <- head(iris); tbliris <- dplyr::as.tbl(head(iris)); mtxiris <- as.matrix(head(iris))
#' mdtable(dat = dfiris, add_row = TRUE)
#' mdtable(dat = tbliris)
#' mdtable(dat = mtxiris)
#' }
#' @export
mdtable <- function(dat, add_row = FALSE) {
  # function of convert a data.frame to table of markdown format output
  convertmd <- function(d) {
    d <- data.frame(lapply(d, as.character), stringsAsFactors = F,
                    check.names = F)
    cat(paste0("|", paste(names(d), collapse = " | "), " |", "\n"))  # header
    cat(paste0(paste(rep("| --- ", ncol(d)), collapse = ""), "|", "\n"))
    invisible(sapply(1:nrow(d), function(i) {
      cat(paste0("| ", paste(d[i, ], collapse = " | "), "|", "\n"))
    }))
  }

  # add row
  if (add_row == T) {
    dat <- tibble::rownames_to_column(dat)
    names(dat)[1] <- " "
  }

  # argument check and convertmd
  if (any(class(dat) == "data.frame")) {
    convertmd(d = dat)
  } else if (any(class(dat) == "matrix")) {
    dat <- as.data.frame(dat)
    convertmd(d = dat)
  } else {
    stop("d must be a data.frame, tbl, or matrix.")
  }

}
