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
                     donut.hole.radius)
{
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

    ## set inner.radius from donut.hole.radius or pie.groups.radius
    inner.radius <- pie.groups.radius
    if (type == "Donut")
        inner.radius <- donut.hole.radius

    ## First column is values, second groups, third is labels.
    d.values <- as.numeric(pie.data[, 1])
    if (length(unique(pie.data[, 3])) == 1)
    {
        d.labels <- as.character(pie.data[, 2])
        d.groups <- NULL
        inner.radius <- 0
    }
    else
    {
        d.labels <- as.character(pie.data[, 3])
        d.groups <- as.character(pie.data[, 2])
        if (inner.radius == 0)
            inner.radius <- 70
    }

    # Resolving colors
    values.color <- stripAlphaChannel(flipChartBasics::ChartColors(number.colors.needed = nrow(chart.matrix), given.colors = values.color, reverse = colors.reverse))
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
    if (type == "Donut" && inner.radius == 0)
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

                # values.color = values.color,
                # values.display = values.display,
                # values.thres = pie.values.thres.percent,
                # values.order = pie.values.order,
                # labels.font = pie.labels.font.family,
                # labels.size = pie.labels.font.size,
                # labels.color = pie.labels.font.color,
                # labels.minFontSize = pie.labels.minFontSize,
                # labels.inner = pie.labels.inner,
                # groups = d.groups,
                # groups.font = pie.groups.font.family,
                # groups.size = pie.groups.font.size,
                # groups.color = groups.color,
                # groups.order = pie.groups.order,
                # prefix = pie.values.prefix,
                # suffix = pie.values.suffix,
                # border.color = pie.border.color,
                # gradient = pie.segment.color.gradient,
                # inner.radius = inner.radius
                )

    return(output)
}
