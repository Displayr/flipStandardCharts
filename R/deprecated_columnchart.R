columnChart <- function(chart.matrix,
                        type,
                        y.tick.format.manual = "",
                        series.marker.border.width)
{
    ErrorIfNotEnoughData(chart.matrix)
    # Data is not changed, but NAs look like zeros on the chart
    if (any(is.na(as.matrix(chart.matrix))))
        warning("Missing values have been set to zero.")

    if (type == "100% Stacked Column")
    {
        chart.matrix <- cum.data(chart.matrix, "column.percentage")
        if (y.tick.format.manual != "%")
            y.tick.format.manual <- ".0%"
    }

    if (type == "100% Stacked Column")
        chart.matrix <- cum.data(chart.matrix, "column.percentage")

    # Should we stack or should we not?
    if (type != "Column")
        barmode <- "stack"
    else
        barmode <- ""

    # Bar or column?
    orientation <- "v"

    ## Group legend items if it's a stacked Column chart as taking individual items off makes no sense
    legend.group <- ""
    if (type != "Column")
        legend.group <- "grouped"

    return(list(chart.matrix = chart.matrix,
                legend.group = legend.group,
                y.tickformat = y.tick.format.manual,
                orientation = orientation,
                barmode = barmode))
}

