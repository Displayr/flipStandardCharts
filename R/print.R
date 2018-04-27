#' Printing \code{StandardChart} objects
#' @method print StandardChart
#' @description All objects of class \code{StandardChart} are
#'    expected to contain a component called htmlwidget is the
#'    original htmlwidget (e.g. plotly, leaflet, etc)
#' @export
print.StandardChart <- function(x, ...)
{
    return(print(x$htmlwidget))
}
