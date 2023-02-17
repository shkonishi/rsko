#' My utility functions for handling ggplot objects.
#' @name gg_utls
#'
#' @title My utility functions for handling ggplot objects.
#'
#' @usage gg_leg(x)
#' @usage unfacet(x)
#' @usage splitFacet(x)
#'
#' @usage gg_cols(n)
#'
#' @param x ggplot class object
#' @param n integer: numbers of colour code
#'
#' @examples \dontrun{
#' # gg_leg: separate a ggplot objetc to a legend and a ggplot object without legend.
#' gg1 <- ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Sepal.Width, colour = Species)) +
#'  ggplot2::geom_point()
#' gg2 <- ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Petal.Length, colour = Species)) +
#'  ggplot2::geom_point()
#'
#' res1 <- gg_leg(gg1); res2 <- gg_leg(gg2)
#' gg1_nl <- res1[[1]]; gg2_nl <- res2[[1]]; lgnd <- res1[[2]]
#'
#' gridExtra::grid.arrange(gg1, gg2, ncol=2)
#' gridExtra::grid.arrange(gridExtra::arrangeGrob(gg1_nl, gg2_nl, lgnd,
#'  ncol = 3, widths = c(3/7, 3/7, 1/7)))
#'
#' # splitFacet: split a facet to list
#' gg <- ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Petal.Length)) +
#'  ggplot2::geom_point() +
#'  ggplot2::facet_wrap(~Species)
#' lst <- splitFacet(gg)
#' do.call(gridExtra::grid.arrange, c(lst, list(ncol = 3)))
#'
#' # gg_cols: Create defaoult colour code of ggplot2
#' cols <- gg_cols(10)
#' barplot(setNames(rep(1,10), cols), col = cols, las =2)
#'
#' }
#'
#' @rdname gg_utls
#' @export
gg_leg <- function(x) {
  # argument check: x
  if (!ggplot2::is.ggplot(x)) stop("'x' must to be a ggplot object")

  # create legend
  tbgr <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(x))
  legend <- tbgr$grobs[[which(sapply(tbgr$grobs, function(x) x$name) == "guide-box")]]

  # ggplot object without legend
  gg_noleg <- x + ggplot2::theme(legend.position = "none")

  # result
  res <- list(nolgd = gg_noleg, lgd = legend)
  res
}

#' @rdname gg_utls
#' @export
unfacet <- function(x){
  x$facet <- ggplot2::ggplot()$facet
  x
}

#' @rdname gg_utls
#' @export
splitFacet <- function(x){
  facet_vars <- names(x$facet$params$facets)         # 1
  x$facet    <- ggplot2::ggplot()$facet              # 2
  datasets   <- split(x$data, x$data[facet_vars])    # 3
  new_plots  <- lapply(datasets,function(new_data){  # 4
    x$data <- new_data
    x})
  new_plots
}

#' @rdname gg_utls
#' @export
gg_cols <- function(n) {
  hues = seq(15, 375, length = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}
