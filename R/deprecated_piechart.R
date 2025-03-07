#' @importFrom utils stack
#' @importFrom flipChartBasics ChartColors
#' @importFrom flipU StopForUserError
#' @importFrom verbs Sum
pieChart <- function(chart.matrix,
                     type,
                     values.color,
                     colors.reverse,
                     colors.custom.color,
                     colors.custom.gradient.start,
                     colors.custom.gradient.end,
                     colors.custom.palette,
                     title = title,
                     title.font.family,
                     title.font.size,
                     title.font.color,
                     pie.values.font.family,
                     pie.values.font.size,
                     pie.values.prefix,
                     pie.values.suffix,
                     pie.data.threshold,
                     pie.values.order,
                     pie.values.decimals,
                     pie.labels.font.family,
                     pie.labels.font.size,
                     pie.labels.font.color,
                     pie.groups.font.family,
                     pie.groups.font.size,
                     pie.groups.font.color,
                     pie.subslice.colors,
                     pie.subslice.colors.custom.color,
                     pie.subslice.colors.custom.gradient.start,
                     pie.subslice.colors.custom.gradient.end,
                     pie.subslice.colors.custom.palette,
                     pie.subslice.colors.reverse,
                     pie.groups.order,
                     pie.inner.radius,
                     pie.border.color,
                     pie.subslice.colors.repeat,
                     pie.show.percentages,
                     table.statistic)
{
    ErrorIfNotEnoughData(chart.matrix)
    chart.matrix <- as.matrix(chart.matrix)

    if (any(is.na(chart.matrix)) || any(chart.matrix < 0))
    {   # negative values are automatically removed by plotly
        warning("Missing and negative values have been omitted.")
        chart.matrix[which(is.na(chart.matrix))] <- 0
    }

    if (is.null(pie.data.threshold))
        pie.data.threshold <- 0.003

    # If the statistic contains percentages but the total does not sum to 100, show warning
    if (length(grep("%", table.statistic)) > 0 && round(Sum(chart.matrix, remove.missing = FALSE)) != 100)
        warning(paste("The percentage values in the table do not sum to 100%.",
                      "Consider choosing a different statistic for the table."))

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
        pie.data <- cbind(suppressWarnings(stack(as.data.frame(chart.matrix[,1:ncol(chart.matrix)]))), labels = rep(rownames(chart.matrix),ncol(chart.matrix)))

    is.data.2d <- length(unique(pie.data[, 3])) > 1

    ## Stop if asked for a donut but passed a 2D table
    if (is.data.2d && type == "Donut")
        StopForUserError("The table supplied is two-dimensional and cannot be displayed as a donut chart.  Please change the chart type to 'Pie' and update.")

    ## First column is values, second groups, third is labels.
    d.values <- as.numeric(pie.data[, 1])
    if (length(unique(pie.data[, 3])) == 1)
    {
        d.labels <- as.character(pie.data[, 2])
        d.groups <- NULL
    }
    else
    {
        d.labels <- as.character(pie.data[, 3])
        d.groups <- as.character(pie.data[, 2])
    }

    # Resolving colors
    num.colors <- nrow(chart.matrix)
    if (!pie.subslice.colors.repeat)
        num.colors <- nrow(chart.matrix) * ncol(chart.matrix)


    if (is.data.2d)
    {
        pie.colors <- if (is.null(pie.subslice.colors))
            NULL
        else
            rep(ChartColors(number.colors.needed = num.colors,
                                              given.colors = pie.subslice.colors,
                                              custom.color = pie.subslice.colors.custom.color,
                                              custom.gradient.start = pie.subslice.colors.custom.gradient.start,
                                              custom.gradient.end = pie.subslice.colors.custom.gradient.end,
                                              custom.palette = pie.subslice.colors.custom.palette,
                                              reverse = pie.subslice.colors.reverse), ncol(chart.matrix))
        pie.groups.colors <- ChartColors(number.colors.needed = ncol(chart.matrix),
                                                           given.colors = values.color,
                                                           custom.color = colors.custom.color,
                                                           custom.gradient.start = colors.custom.gradient.start,
                                                           custom.gradient.end = colors.custom.gradient.end,
                                                           custom.palette = colors.custom.palette,
                                                           reverse = colors.reverse)
    }
    else
    {
        pie.colors <- ChartColors(number.colors.needed = num.colors,
                                                    given.colors = values.color,
                                                    custom.color = colors.custom.color,
                                                    custom.gradient.start = colors.custom.gradient.start,
                                                    custom.gradient.end = colors.custom.gradient.end,
                                                    custom.palette = colors.custom.palette,
                                                    reverse = colors.reverse)
        pie.groups.colors <- NULL
    }

    if (type == "Pie" && !is.data.2d)
        pie.inner.radius <- 0

    # Convert pie.inner.radius to character
    inner.radius <- pie.inner.radius / 100

    values.display.as <- if (pie.show.percentages) "percentage" else "original"
    if (pie.show.percentages)
    {
        pie.values.prefix <- ""
        pie.values.suffix <- "%"
    }

    if (length(pie.colors) > 0)
        pie.colors <- rep(pie.colors, length = length(d.values))

    rhtmlDonut::Donut(values = d.values,
                      labels = d.labels,
                      values.color = pie.colors,
                      values.order = pie.values.order,
                      values.font.family = pie.values.font.family,
                      values.font.size = pie.values.font.size,
                      values.decimal.places = pie.values.decimals,
                      values.display.as = values.display.as,
                      values.display.thres = pie.data.threshold,
                      labels.font.family = pie.labels.font.family,
                      labels.font.color = pie.labels.font.color,
                      labels.font.size = pie.labels.font.size,
                      groups = d.groups,
                      groups.color = pie.groups.colors,
                      groups.order = pie.groups.order,
                      groups.font.family = pie.groups.font.family,
                      groups.font.color = pie.groups.font.color,
                      groups.font.size = pie.groups.font.size,
                      title = title,
                      title.font.family = title.font.family,
                      title.font.size = title.font.size,
                      title.font.color = title.font.color,
                      prefix = pie.values.prefix,
                      suffix = pie.values.suffix,
                      border.color = pie.border.color,
                      inner.radius = inner.radius)
}
