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
    widget <- x$htmlwidget
    # Displayr authors of widgets tend to put other attributes (like ChartData) into this encapsulating
    # object (`x`), and then rely on the implicit `print()` on our R servers to extract the
    # widget that should be displayed.  For their convenience we copy across this attribute.
    if (inherits(x, "can-run-in-root-dom"))
        class(widget) <- append("can-run-in-root-dom", class(widget))
    return(print(widget))
}
