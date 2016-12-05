columnChart <- function(chart.matrix,
                        type,
                        y.tick.format.manual = "",
                        series.marker.border.width,
                        bar.group.gap)
{
    if (any(is.na(as.matrix(chart.matrix))))
    {
        warning("Missing values have been set to zero.")
        chart.matrix[which(is.na(chart.matrix))] <- 0
    }

    if (type == "100% Stacked Column")
    {
        chart.matrix <- cum.data(chart.matrix, "column.percentage")
        if (y.tick.format.manual != "%")
            y.tick.format.manual <- "%"
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

    ## If it's got a series.line.width > 0 then set the bar.group.gap to that value.
    if (series.marker.border.width > 0 && is.null(bar.group.gap))
        bar.group.gap <- series.marker.border.width * 0.035
    else
        bar.group.gap <- bar.group.gap

    return(list(chart.matrix = chart.matrix,
                legend.group = legend.group,
                y.tickformat = y.tick.format.manual,
                orientation = orientation,
                barmode = barmode,
                bar.group.gap = bar.group.gap))
}

