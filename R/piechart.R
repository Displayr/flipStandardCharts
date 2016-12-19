#' @importFrom utils stack
#' @importFrom flipChartBasics StripAlphaChannel ChartColors
pieChart <- function(chart.matrix,
                     type,
                     values.color,
                     colors.reverse,
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
                     pie.subslice.colors.reverse,
                     pie.groups.order,
                     pie.inner.radius,
                     pie.border.color,
                     pie.subslice.colors.repeat,
                     table.statistic,
                     qinput)
{
    chart.matrix <- as.matrix(chart.matrix)

    if (any(is.na(chart.matrix)) || any(chart.matrix < 0))
    {
        warning("Missing and negative values have been omitted.")
        chart.matrix[which(is.na(chart.matrix))] <- 0
    }

    if (is.null(pie.data.threshold))
        pie.data.threshold <- 0.003

    ## Check that the table statistic is appropriate for the chart type
    permitted.statistics <- c("%", "Total %", "n", "Population", "Average", "Sum", "% Share", "% Total Share")

    if (!length(permitted.statistics[which(table.statistic == permitted.statistics)]) > 0 && qinput)
        warning("It is recommended that you use one of the following statistics in this chart: %, Total %, n, Population, Average, Sum, % Share, or % Total Share")

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
        pie.data <- cbind(suppressWarnings(stack(as.data.frame(chart.matrix[,1:ncol(chart.matrix)]))), labels = rep(rownames(chart.matrix),ncol(chart.matrix)))
        pie.data <- pie.data[with(pie.data,order(pie.data[,2])),]
    }

    is.data.2d <- length(unique(pie.data[, 3])) > 1

    ## Stop if asked for a donut but passed a 2D table
    if (is.data.2d && type == "Donut")
        stop("The table supplied is two-dimensional and cannot be displayed as a donut chart.  Please change the chart type to 'Pie' and update.")

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
            rep(StripAlphaChannel(ChartColors(number.colors.needed = num.colors,
                                              given.colors = pie.subslice.colors,
                                              reverse = pie.subslice.colors.reverse)), ncol(chart.matrix))
        pie.groups.colors <- StripAlphaChannel(ChartColors(number.colors.needed = ncol(chart.matrix),
                                                           given.colors = values.color,
                                                           reverse = colors.reverse))
    }
    else
    {
        pie.colors <- StripAlphaChannel(ChartColors(number.colors.needed = num.colors,
                                                      given.colors = values.color,
                                                      reverse = colors.reverse))
        pie.groups.colors <- NULL
    }

    # Convert pie.inner.radius to character
    inner.radius <- paste(pie.inner.radius, "%", sep = "")

    rhtmlDonut::Donut(values = d.values,
                      labels = d.labels,
                      values.color = pie.colors,
                      values.order = pie.values.order,
                      values.font.family = pie.values.font.family,
                      values.font.size = pie.values.font.size,
                      values.decimal.places = pie.values.decimals,
                      values.display.as = "percentage",
                      values.display.thres = pie.data.threshold * 100,
                      labels.font.family = pie.labels.font.family,
                      labels.font.color = pie.labels.font.color,
                      labels.font.size = pie.labels.font.size,
                      labels.min.font.size = pie.labels.font.size,
                      groups = d.groups,
                      groups.color = pie.groups.colors,
                      groups.order = pie.groups.order,
                      groups.font.family = pie.groups.font.family,
                      groups.font.color = pie.groups.font.color,
                      groups.font.size = pie.groups.font.size,
                      groups.min.font.size = pie.groups.font.size,
                      title = title,
                      title.font.family = title.font.family,
                      title.font.size = title.font.size,
                      title.font.color = title.font.color,
                      prefix = pie.values.prefix,
                      suffix = pie.values.suffix,
                      border.color = pie.border.color,
                      inner.radius = inner.radius)
}
