pieChart <- function(y,
                     chart.matrix,
                     type,
                     transpose,
                     values.color,
                     colors.reverse,
                     pie.values.font.family,
                     pie.values.font.size,
                     pie.values.prefix,
                     pie.values.suffix,
                     pie.values.display.format,
                     pie.values.thres.percent,
                     pie.values.order,
                     pie.values.decimals,
                     pie.labels.font.family,
                     pie.labels.font.size,
                     pie.labels.font.color,
                     pie.labels.minFontSize,
                     pie.labels.inner,
                     pie.groups.font.family,
                     pie.groups.font.size,
                     pie.groups.font.color,
                     pie.groups.minFontSize,
                     pie.groups.colors,
                     pie.groups.colors.reverse,
                     pie.groups.order,
                     pie.groups.radius,
                     pie.border.color,
                     pie.segment.color.gradient,
                     donut.hole.radius,
                     pie.segment.colors.repeat.by.group,
                     table.statistic)
{
    ## Check that the table statistic is appropriate for the chart type
    print(table.statistic)

    permitted.statistics <- c("Total %", "n", "Population", "Average", "Sum", "% Share", "% Total Share")

    if (!length(permitted.statistics[which(table.statistic == permitted.statistics)]) > 0)
        warning("It is recommended that you use one of the following statistics in this chart: Total %, n, Population, Average, Sum, % Total Share, or Trimmed Average")

    ## If transpose is false and there's only one row in chart.matrix
    if (nrow(chart.matrix) == 1)
        chart.matrix <- t(chart.matrix)

    ## As some charts get passed in as xtabs objects, rather than pure matrices, we need to unclass, for the stack to work later.
    chart.matrix <- stripClassAndCallFromXtabs(chart.matrix)

    ## If there's only one column at this stage, then we need to manually provide some data.
    if (ncol(chart.matrix) == 1)
    {
        # pie.data <- cbind(as.numeric(chart.matrix[ ,1]))
        chart.matrix <- cbind(chart.matrix, rownames(chart.matrix))
        chart.matrix <- cbind(chart.matrix, rep("group", nrow(chart.matrix)))
        pie.data <- chart.matrix
    }
    else
    {
        pie.data <- cbind(suppressWarnings(stack(as.data.frame(chart.matrix[,1:ncol(chart.matrix)]))), labels = rep(rownames(chart.matrix),ncol(chart.matrix))) #cbind(suppressWarnings(stack(chart.matrix)), labels = rep(rownames(chart.matrix), ncol(chart.matrix))) #as.data.frame(chart.matrix[ , 1:ncol(chart.matrix)])
        pie.data <- pie.data[with(pie.data,order(pie.data[,2])),]
    }

    ## Stop if asked for a donut but passed a 2D table
    if (length(unique(pie.data[, 3])) >= 2 && type == "Donut")
        warning("Donuts should not be used to display 2D data.  Change chart type to: Pie")

    ## set inner.radius from donut.hole.radius or pie.groups.radius
    if (type == "Donut" && donut.hole.radius == 0)
        inner.radius <- donut.hole.radius <- 70

    if (type == "Pie" && pie.groups.radius == 0)
        inner.radius <- pie.groups.radius <- 70

    ## First column is values, second groups, third is labels.
    d.values <- as.numeric(pie.data[, 1])
    if (length(unique(pie.data[, 3])) == 1)
    {
        d.labels <- as.character(pie.data[, 2])
        d.groups <- NULL
        if (type == "Pie")
            inner.radius <- 0
        else
            inner.radius <- donut.hole.radius
    }
    else
    {
        d.labels <- as.character(pie.data[, 3])
        d.groups <- as.character(pie.data[, 2])
        if (type == "Pie")
            inner.radius <- pie.groups.radius
        else
            inner.radius <- donut.hole.radius
    }

    # Resolving colors
    num.colors <- nrow(chart.matrix)
    if (!pie.segment.colors.repeat.by.group)
        num.colors <- nrow(chart.matrix) * ncol(chart.matrix)


    values.color <- stripAlphaChannel(flipChartBasics::ChartColors(number.colors.needed = num.colors, given.colors = values.color, reverse = colors.reverse))
    groups.color <- stripAlphaChannel(flipChartBasics::ChartColors(number.colors.needed = ncol(chart.matrix), given.colors = pie.groups.colors, reverse = pie.groups.colors.reverse))

    values.color <- rep(values.color, ncol(chart.matrix))

    # Values display
    if (pie.values.display.format == "%")
    {
        values.display <- "percentage"
        if (pie.values.suffix == "")
            pie.values.suffix <- "%"
    }
    else
        values.display <- "original"

    # If Donut chart, ensure default hole is a hole
    if (type == "Donut" && donut.hole.radius == 0)
        inner.radius = 70

    # Convert pie.inner.radius to character
    inner.radius <- paste(inner.radius, "%", sep = "")

    output <- list(values.data = d.values,
                labels = d.labels,
                groups = d.groups,
                pie.values.font.family = pie.values.font.family,
                pie.values.font.size = pie.values.font.size,
                pie.values.color = values.color,
                pie.values.prefix = pie.values.prefix,
                pie.values.suffix = pie.values.suffix,
                pie.values.display.format = values.display,
                pie.values.thres.percent = pie.values.thres.percent,
                pie.values.order = pie.values.order,
                pie.values.decimals = pie.values.decimals,
                pie.labels.font.family = pie.labels.font.family,
                pie.labels.font.size = pie.labels.font.size,
                pie.labels.font.color = pie.labels.font.color,
                pie.labels.minFontSize = pie.labels.minFontSize,
                pie.labels.inner = pie.labels.inner,
                pie.groups.font.family = pie.groups.font.family,
                pie.groups.font.size = pie.groups.font.size,
                pie.groups.font.color = pie.groups.font.color,
                pie.groups.min.font.size = pie.groups.minFontSize,
                pie.groups.colors = groups.color,
                pie.groups.colors.reverse = pie.groups.colors.reverse,
                pie.groups.order = pie.groups.order,
                inner.radius = inner.radius,
                pie.border.color = pie.border.color,
                pie.segment.color.gradient = pie.segment.color.gradient
                )

    return(output)
}
