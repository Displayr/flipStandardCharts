#' Printing \code{StandardChart} objects
#' @method print StandardChart
#' @param x Object of class \code{StandardChart}
#' @param ... Not used
#' @description All objects of class \code{StandardChart} are
#'    expected to contain a component called htmlwidget is the
#'    original htmlwidget (e.g. plotly, leaflet, etc)
#' @export
print.StandardChart <- function(x, ...)
{
    if (is.null(x$htmlwidget))
        stop("StandardChart object does not contain htmlwidget")
    if (inherits(x$htmlwidget, "streamgraph"))
        return(suppressWarnings(print(x$htmlwidget)))
    return(print(x$htmlwidget))
}
