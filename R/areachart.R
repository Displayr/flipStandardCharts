areaChart <- function(chart.matrix,
                      opacity,
                      type,
                      y.tick.format.manual = "",
                      y.tick.suffix = "",
                      series.line.width,
                      series.marker.show)
{
    if (is.null(opacity))
        opacity <- 0.4

    if (any(is.na(as.matrix(chart.matrix))))
        warning("Missing values have been interpolated or omitted.")

    no.data.in.series <- colSums(is.na(chart.matrix)) >= length(chart.matrix[, 1]) - 1
    if (any(no.data.in.series))
        chart.matrix <- chart.matrix[, !no.data.in.series]

    ## Change the matrix data according to requirements of the chart type
    if (type == "Stacked Area")
        chart.matrix <- cum.data(chart.matrix, "cumulative.sum")
    else if (type == "100% Stacked Area")
        chart.matrix <- cum.data(chart.matrix, "cumulative.percentage")

    ## Issue warning if opacity is = 1 and type = "Area"
    if (opacity == 1 && type == "Area" && ncol(chart.matrix) > 1)
        warning("Displaying this chart with opacity set to 1 will make it difficult to read as some data series may be obscured.")

    ## Determine whether to draw to zero y (overlapping area chart) or to next y (for stacked)
    if (type == "Area")
        fill.bound <- "tozeroy"
    else
        fill.bound <- "tonexty"

    ## Group legend items if it's a stacked area chart as taking individual items off makes no sense
    legend.group <- ""
    if (type != "Area")
        legend.group <- "grouped"

    ## If it's a 100% Stacked Area chart, and no options have been specified for y.tick.format, then set to %
    y.tickformat <- ""
    if (type == "100% Stacked Area" && y.tick.format.manual == "" && y.tick.suffix == "")
        y.tickformat <- "%"

    ## Showing markers and lines
    series.mode = "lines+markers"  #default = line and marker
    if (series.line.width == 0 && series.marker.show != "none")
        series.mode = "markers"

    else if (series.line.width >= 1 && series.marker.show == "none")
        series.mode = "lines"

    else if (series.line.width == 0 && series.marker.show == "none")
        series.mode = "lines"

    return(list(chart.matrix = chart.matrix,
                fill.bound = fill.bound,
                legend.group = legend.group,
                y.tickformat = y.tickformat,
                series.mode = series.mode,
                opacity = opacity))
}

