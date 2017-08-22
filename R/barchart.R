barChart <- function(chart.matrix,
                     type,
                     x.tick.format.manual = "",
                     series.marker.border.width)
{
    # Data is not changed, but NAs look like zeros on the chart
    if (any(is.na(as.matrix(chart.matrix))))
        warning("Missing values have been set to zero.")

    if (type == "100% Stacked Bar")
    {
        chart.matrix <- cum.data(chart.matrix, "column.percentage")
        if (x.tick.format.manual != "%")
            x.tick.format.manual <- "%"
    }

    # Should we stack or should we not?
    if (type != "Bar")
        barmode <- "stack"
    else
        barmode <- ""

    # Bar or column?
    orientation <- "h"

    ## Group legend items if it's a stacked Column chart as taking individual items off makes no sense
    legend.group <- ""
    if (type != "Bar")
        legend.group <- "grouped"

    return(list(chart.matrix = chart.matrix,
                legend.group = legend.group,
                x.tickformat = x.tick.format.manual,
                orientation = orientation,
                barmode = barmode))
}
