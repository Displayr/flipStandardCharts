#' @importFrom verbs SumColumns
lineChart <- function(chart.matrix,
                      series.line.width,
                      series.marker.show)
{
    ErrorIfNotEnoughData(chart.matrix)
    no.data.in.series <- SumColumns(is.na(chart.matrix), remove.rows = NULL, remove.missing = FALSE) >= length(chart.matrix[, 1])
    if (any(no.data.in.series))
        chart.matrix <- chart.matrix[, !no.data.in.series]

    ## Check that line width is at least 1
    if (series.line.width < 1)
        series.line.width <- 1

    ## Showing markers and lines
    series.mode <- if (series.line.width >= 1 && (is.null(series.marker.show) || series.marker.show == "none"))
        "lines"
    else
        "lines+markers"

    return(list(chart.matrix = chart.matrix,
                series.mode = series.mode,
                series.line.width = series.line.width))
}
