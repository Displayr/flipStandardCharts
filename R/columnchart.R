columnChart <- function(chart.matrix,
                      transparency,
                      type,
                      y.tick.format.manual = "",
                      y.tick.suffix = "",
                      y.tick.decimals = 0,
                      series.marker.border.width,
                      series.marker.show,
                      data.label,
                      bar.group.gap)
{
    ## Change the matrix data according to requirements of the chart type
    # if (type == "Stacked Column")
    #     chart.matrix <- cum.data(chart.matrix, "cumulative.sum")
    if (type == "100% Stacked Column")
        chart.matrix <- cum.data(chart.matrix, "column.percentage")

    # Should we stack or should we not?
    if (type != "Column")
        barmode = "stack"
    else
        barmode = ""

    ## Group legend items if it's a stacked Column chart as taking individual items off makes no sense
    legend.group <- ""
    if (type != "Column")
        legend.group <- "grouped"

    ## If it's a 100% Stacked Column chart, and no options have been specified for y.tick.format, then set to %
    if (type == "100% Stacked Column" && y.tick.format.manual == "" && y.tick.suffix == "" && y.tick.decimals == 0)
        y.tickformat <- "%"
    else
        y.tickformat <- y.tick.format.manual

    ## If it's got a series.line.width > 0 then set the bar.group.gap to that value.
    if (series.marker.border.width > 0 && is.null(bar.group.gap))
        bar.group.gap <- series.marker.border.width * 0.035
    else
        bar.group.gap <- bar.group.gap

    return(list(chart.matrix = chart.matrix,
                legend.group = legend.group,
                y.tickformat = y.tickformat,
                # series.mode = series.mode,
                orientation = "v",
                type = "bar",
                barmode = barmode,
                bar.group.gap = bar.group.gap))
}

