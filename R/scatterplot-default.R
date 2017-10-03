#' Scatterplot chart
#' 
#' Create a scatter plot or a labeled scatter plot if labels are provided.
#'
#' @param x Input data for charting
#' @param scatter.labels Alternative way to provide data labels.
#' @param show.labels.as.hovertext Use a normal scatterplot even if labels are provided.
#' @inheritParams ScatterChart
#' @export

ScatterPlot <- function (x, scatter.labels = NULL, show.labels.as.hovertext=FALSE, ...)
{
    if (!show.labels.as.hovertext)
    {
        if (!is.null(scatter.labels) || !is.null(rownames(x)) || !is.null(names(x)))
            return(LabeledScatterChart(x = x, scatter.labels = scatter.labels, ...))
    }
    return(ScatterChart(x = x, scatter.labels = scatter.labels, ...))
}
