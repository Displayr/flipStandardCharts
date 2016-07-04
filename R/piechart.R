pieChart <- function(chart.matrix,
                     transpose,
                     pie.values.font.family,
                     pie.values.font.size,
                     pie.segment.colors,
                     pie.values.prefix,
                     pie.values.suffix,
                     pie.values.display.format,
                     pie.values.thres.percent,
                     pie.values.order,
                     pie.labels.font.family,
                     pie.labels.font.size,
                     pie.labels.font.color,
                     pie.labels.minFontSize,
                     pie.labels.inner,
                     pie.groups.font.family,
                     pie.groups.font.size,
                     pie.groups.colors,
                     pie.groups.order,
                     pie.border.color,
                     pie.segment.color.gradient,
                     pie.inner.radius,
                     pie.max.label.length)
{
    ## If transpose is false and there's only one row in chart.matrix
    if (transpose == FALSE && nrow(chart.matrix) == 1)
        chart.matrix <- t(chart.matrix)

    ## Convert the chart matrix into a flat format so that we can use it as a pie chart
    df.chart.matrix <-  as.data.frame(chart.matrix[ , 1:ncol(chart.matrix)])
    pie.data <- cbind(suppressWarnings(stack(df.chart.matrix)), labels = rep(rownames(chart.matrix), ncol(chart.matrix)))

    ## First column is values, second groups, third is labels.
    d.values <- pie.data[, 1]
    if (length(unique(pie.data[,2])) == 1)
    {
        d.labels <- as.character(pie.data[, 3])
        d.groups <- NULL
    }
    else
    {
        d.labels <- as.character(pie.data[, 3])
        d.groups <- as.character(pie.data[, 2])
        if (pie.inner.radius == 0)
            pie.inner.radius <- 70
    }

    # Make sure there are enough colours to cover the categories
    values.color <- rep(rep_len(pie.segment.colors, nrow(chart.matrix)), ncol(chart.matrix))
    values.color <- stripAlphaChannel(values.color)

    # ...and that enough have been provided to cover the groups
    groups.color <- stripAlphaChannel(pie.groups.colors)

    # Values display
    if (pie.values.display.format == "%")
    {
        values.display <- "percentage"
        if (pie.values.suffix == "")
            pie.values.suffix <- "%"
    }
    else
        values.display <- "original"

    # Convert pie.inner.radius to character
    inner.radius <- paste(pie.inner.radius, "%", sep = "")

    return(list(values = d.values,
                labels = d.labels,
                values.font = pie.values.font.family,
                values.size = pie.values.font.size,
                values.color = values.color,
                values.display = values.display,
                values.thres = pie.values.thres.percent,
                values.order = pie.values.order,
                labels.font = pie.labels.font.family,
                labels.size = pie.labels.font.size,
                labels.color = pie.labels.font.color,
                labels.minFontSize = pie.labels.minFontSize,
                labels.inner = pie.labels.inner,
                groups = d.groups,
                groups.font = pie.groups.font.family,
                groups.size = pie.groups.font.size,
                groups.color = groups.color,
                groups.order = pie.groups.order,
                prefix = pie.values.prefix,
                suffix = pie.values.suffix,
                border.color = pie.border.color,
                gradient = pie.segment.color.gradient,
                inner.radius = inner.radius,
                max.label.length = pie.max.label.length))
}
