#' utility for kableExtra
#' @name kb_utls
#' @rdname kb_utls
#'
#' @title utility for kableExtra
#'
#' @usage kb_utls(x, kb_opt, ext_opt)
#'
#' @param x vector, matrix, data.frame
#' @param kb_opt list, options for knitr::kable,
#'     default = list(format = 'html', align = 'c', row.names = F)
#' @param ext_opt list, options for kableExtra::kable_styling
#'     default = list(bootstrap_options = 'striped', full_width = F, position = 'float_left')
#'
#' @examples \dontrun{
#' # default parameter
#' kb_utls(head(iris))
#'
#' # additional options of 'kable' and 'kable_styling'
#' kb_utls(head(iris), list(caption = 'iris', align = 'c'),
#'     list(bootstrap_options = 'hover'))
#'
#' # no use kable_styling
#' kb_utls(head(iris), list(format = "pandoc", caption = "iris"), NULL)
#'
#' }
#'
#' @rdname kb_utls
#' @export
kb_utls <- function(x, kb_opt = list(format = 'html', align = 'c', row.names = F),
                   ext_opt = list(bootstrap_options = 'striped', full_width = F, position = 'left')){
  # default options
  kb_opt_def = list(format = 'html', align = 'c', row.names = F)
  ext_opt_def = list(bootstrap_options = 'striped', full_width = F, position = 'left')

  # merge options
  kb_opt <- c(kb_opt_def[!names(kb_opt_def) %in% names(kb_opt)], kb_opt)
  if (!is.null(ext_opt)) {
    ext_opt <- c(ext_opt_def[!names(ext_opt_def) %in% names(ext_opt)], ext_opt)
  }

  if (!is.null(ext_opt)) {# using kable_styling
    do.call(function(x, ...) kableExtra::kable(x, ...), c(list(x = x), kb_opt)) %>%
    {do.call(function(x, ...) kableExtra::kable_styling(x, ...), c(list(x = .), ext_opt))}

  } else {# no use kable_styling
    do.call(function(x, ...) knitr::kable(x, ...), c(list(x = x), kb_opt))
  }
}
