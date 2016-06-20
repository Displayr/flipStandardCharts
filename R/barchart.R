barChart <- function(chart.matrix,
                        transparency,
                        type,
                        y.tick.format.manual = "",
                        y.tick.suffix = "",
                        y.tick.decimals = 0,
                        series.marker.border.width,
                        series.marker.show,
                        data.label,
                        bar.group.gap,
                        y.bounds.minimum,
                        y.bounds.maximum,
                        y.bounds.units.major,
                        y.nticks)
{
    ## Change the matrix data according to requirements of the chart type
    # if (type == "Stacked Column")
    #     chart.matrix <- cum.data(chart.matrix, "cumulative.sum")
    if (type == "100% Stacked Bar")
        chart.matrix <- cum.data(chart.matrix, "column.percentage")

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

    ## If it's a 100% Stacked Column chart, and no options have been specified for y.tick.format, then set to %
    if (type == "100% Stacked Bar" && y.tick.format.manual == "" && y.tick.suffix == "" && y.tick.decimals == 0)
        y.tickformat <- "%"
    else
        y.tickformat <- y.tick.format.manual

    ## If it's got a series.line.width > 0 then set the bar.group.gap to that value.
    if (series.marker.border.width > 0 && is.null(bar.group.gap))
        bar.group.gap <- series.marker.border.width * 0.135
    else
        bar.group.gap <- bar.group.gap

    ## If there are no set boundaries, then set some
    if (is.null(y.bounds.minimum) && is.null(y.bounds.maximum) && is.null(y.bounds.units.major))
    {
        ## Check that the colnames are actually numeric, and if so, proceed
        if (length(which(is.na(suppressWarnings(as.numeric(colnames(chart.matrix)))))) == 0)
        {
            y.bounds <- as.numeric(colnames(chart.matrix))
            y.bounds.units.major <- abs(y.bounds[2]) - abs(y.bounds[1])
            y.bounds.minimum <- min(y.bounds) - y.bounds.units.major
            y.bounds.maximum <- max(y.bounds) + y.bounds.units.major
        }
    }

    return(list(chart.matrix = chart.matrix,
                legend.group = legend.group,
                y.tickformat = y.tickformat,
                # series.mode = series.mode,
                orientation = orientation,
                type = "bar",
                barmode = barmode,
                bar.group.gap = bar.group.gap,
                swap.axes.and.data = TRUE,
                y.bounds.minimum = y.bounds.minimum,
                y.bounds.maximum = y.bounds.maximum,
                y.bounds.units.major = y.bounds.units.major,
                y.nticks = y.nticks))
}

