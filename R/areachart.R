areaChart <- function(chart.matrix,
                      transparency,
                      type,
                      y.tick.format.manual = "",
                      y.tick.suffix = "",
                      y.tick.decimals = 0,
                      series.line.width,
                      series.marker.show)
{
    ## Check for negative values in the input table
    if (type == "Stacked Area" || type == "100% Stacked Area")
    {
        if (any(chart.matrix < 0))
            stop("Negative values are not compatible with stacked charts.")

        if (any(is.na(chart.matrix)))
            stop("NaN values are not compatible with stacked charts.")
    }

    ## Change the matrix data according to requirements of the chart type
    if (type == "Stacked Area")
        chart.matrix <- cum.data(chart.matrix, "cumulative.sum")
    else if (type == "100% Stacked Area")
        chart.matrix <- cum.data(chart.matrix, "cumulative.percentage")

    ## Issue warning if transparency is = 1 and type = "Area"
    if (transparency == 1 && type == "Area")
        warning("Displaying this chart without transparent series will make it difficult to read as some data series may be obscured.")

    ## Having transparency on non-overlapping series serves no purpose.
    if (transparency != 1 && type != "Area")
        transparency <- 1

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
        series.mode = "none"

    return(list(chart.matrix = chart.matrix,
                fill.bound = fill.bound,
                legend.group = legend.group,
                y.tickformat = y.tickformat,
                series.mode = series.mode,
                transparency = transparency))
}

