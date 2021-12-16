#' colours657 window
#' @description colours657 window
#' @usage colours657(distinct)
#' @return ggplot object
#' @param distinct logical an argument for colours function, the default value is TRUE
#' @importFrom dplyr %>%
#' @importFrom plyr .
#' @examples #
#' res <- colours657()
#' @export
colours657 <- function(distinct = F){
  # colours pallet
  Row <- NULL; k <- NULL; v <- NULL
  if (distinct == T) {
    len.col <- length(grDevices::colours(distinct = T))
    nfillna <- 60 - len.col %% 60
    nm.col <- grDevices::colours(distinct = T)
    slct_col <- c(21,137:180,342,353)
    cex.cols <- ifelse(1:(len.col + nfillna) %in% c(grep("dark", nm.col),slct_col), "white", "black")

    d <- data.frame(matrix(c(1:len.col, rep(NA, nfillna)), nrow = 60)) %>%
      stats::setNames(., letters[1:ncol(.)]) %>%
      tibble::rownames_to_column("Row") %>%
      tidyr::gather(., key = k, value = v, -Row) %>%
      dplyr::mutate(Row = factor(Row, levels = 60:1)) %>%
      dplyr::mutate_at(dplyr::vars(2:3), as.factor)

  } else {
    len.col <- length(grDevices::colours())
    nfillna <- 60 - len.col %% 60
    nm.col <- grDevices::colours()
    slct_col <- c(24,61,153:199,260:309,477,490,491)
    cex.cols <- ifelse(1:(len.col + nfillna) %in% c(grep("dark", nm.col), slct_col), "white", "black")

    d <- data.frame(matrix(c(1:length(grDevices::colours()), rep(NA, nfillna)), nrow = 60)) %>%
      stats::setNames(., letters[1:ncol(.)]) %>%
      tibble::rownames_to_column("Row") %>%
      tidyr::gather(., key = k, value = v, -Row) %>%
      dplyr::mutate(Row = factor(Row, levels = 60:1)) %>%
      dplyr::mutate_at(dplyr::vars(2:3), as.factor)

  }

  # ggplot
  ggcols <- ggplot2::ggplot(d, ggplot2::aes(k, Row)) +
    ggplot2::geom_tile(ggplot2::aes(fill = v), colour = "white") +
    ggplot2::scale_fill_manual(values = nm.col) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "", y = "") +
    ggplot2::geom_text(ggplot2::aes(label = paste0(1:(len.col + nfillna), ":",c(nm.col, rep("", nfillna)))),
                                    col = factor(cex.cols), size = 1.8)

  print(ggcols)
  return(ggcols)
}
