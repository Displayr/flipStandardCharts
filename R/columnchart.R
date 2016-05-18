columnChart <- function(chart.matrix,
                      transparency,
                      type,
                      y.tick.format.manual = "",
                      y.tick.suffix = "",
                      y.tick.decimals = 0,
                      series.line.width,
                      series.marker.show)
{
    ## Change the matrix data according to requirements of the chart type
    if (type == "Stacked Column")
        chart.matrix <- cum.data(chart.matrix, "cumulative.sum")
    else if (type == "100% Stacked Column")
        chart.matrix <- cum.data(chart.matrix, "cumulative.percentage")

    # Should we stack or should we not?
    if (type != "Column")
        barmode = "stack"
    else
        barmode = NULL

    ## Group legend items if it's a stacked Column chart as taking individual items off makes no sense
    legend.group <- ""
    if (type != "Column")
        legend.group <- "grouped"

    ## If it's a 100% Stacked Column chart, and no options have been specified for y.tick.format, then set to %
    y.tickformat <- ""
    if (type == "100% Stacked Column" && y.tick.format.manual == "" && y.tick.suffix == "" && y.tick.decimals == 0)
        y.tickformat <- "%"

    ## Showing markers and lines
    # series.mode = "lines+markers"  #default = line and marker
    # if (series.line.width == 0 && series.marker.show != "none")
    #     series.mode = "markers"
    #
    # else if (series.line.width >= 1 && series.marker.show == "none")
    #     series.mode = "lines"
    #
    # else if (series.line.width == 0 && series.marker.show == "none")
    #     series.mode = "none"

    return(list(chart.matrix = chart.matrix,
                legend.group = legend.group,
                y.tickformat = y.tickformat,
                # series.mode = series.mode,
                orientation = "v",
                type = "bar",
                barmode = barmode))
}

