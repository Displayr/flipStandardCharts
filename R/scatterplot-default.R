#' Scatterplot chart
#'
#' Create a scatter plot or a labeled scatter plot if labels are provided.
#'
#' @param x Input data for charting
#' @param scatter.labels Alternative way to provide data labels.
#' @param show.labels.as.hovertext Use a normal scatterplot even if labels are provided.
#' @param max.label.length Maximum number of points above which Labeled Scatter plots will not be used.
#' @inheritParams ScatterChart
#' @export

ScatterPlot <- function (x, scatter.labels = NULL, show.labels.as.hovertext=FALSE, max.label.length = 200, ...)
{
    if (!show.labels.as.hovertext)
    {
        # Labeled Scatter plots cannot handle too many points
        if ((!is.null(scatter.labels) && length(scatter.labels) < max.label.length) ||
            (!is.null(rownames(x)) && nrow(x) < max.label.length) ||
            (!is.null(names(x)) && length(x) < max.label.length))
            return(LabeledScatterChart(x = x, scatter.labels = scatter.labels, ...))
    }
    return(ScatterChart(x = x, scatter.labels = scatter.labels, ...))
}
