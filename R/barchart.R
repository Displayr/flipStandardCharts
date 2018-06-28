#' Bar
#'
#' Bar Chart
#' @inherit Column
#' @param y.tick.label.wrap Logical; whether to wrap long labels on the y-axis (vertical).
#' @param y.tick.label.wrap.nchar Integer; number of characters in each line when \code{y.tick.label.wrap} is \code{TRUE}.
#' @param x.tick.suffix x-axis tick label suffix
#' @param x.tick.prefix x-axis tick label prefix
#' @examples
#' z <- structure(c(1L, 2L, 3L, 4L, 5L, 2L, 3L, 4L, 5L, 6L),  .Dim = c(5L, 2L),
#'       .Dimnames = list(c("T", "U", "V", "W", "X"), c("A", "B")))
#' Bar(z, type="100% Stacked Bar")
#' @importFrom grDevices rgb
#' @importFrom flipChartBasics ChartColors
#' @importFrom plotly plot_ly config toRGB add_trace add_text layout hide_colorbar
#' @importFrom stats loess loess.control lm predict
#' @export
Bar <- function(x,
                    type = "Bar",
                    fit.type = "None", # can be "Smooth" or anything else
                    fit.ignore.last = FALSE,
                    fit.line.type = "dot",
                    fit.line.width = 1,
                    fit.line.name = "Fitted",
                    fit.line.opacity = 1,
                    fit.CI.show = FALSE,
                    fit.CI.colors = colors,
                    fit.CI.opacity = 0.4,
                    global.font.family = "Arial",
                    global.font.color = rgb(44, 44, 44, maxColorValue = 255),
                    title = "",
                    title.font.family = global.font.family,
                    title.font.color = global.font.color,
                    title.font.size = 16,
                    subtitle = "",
                    subtitle.font.family = global.font.family,
                    subtitle.font.color = global.font.color,
                    subtitle.font.size = 12,
                    footer = "",
                    footer.font.family = global.font.family,
                    footer.font.color = global.font.color,
                    footer.font.size = 8,
                    footer.wrap = TRUE,
                    footer.wrap.nchar = 100,
                    colors = ChartColors(max(1, ncol(x), na.rm = TRUE)),
                    fit.line.colors = colors,
                    opacity = NULL,
                    background.fill.color = rgb(255, 255, 255, maxColorValue = 255),
                    background.fill.opacity = 0,
                    charting.area.fill.color = background.fill.color,
                    charting.area.fill.opacity = 0,
                    legend.show = TRUE,
                    legend.position.x = 1.02,
                    legend.position.y = 1.00,
                    legend.ascending = NA,
                    legend.fill.color = background.fill.color,
                    legend.fill.opacity = 0,
                    legend.border.color = rgb(44, 44, 44, maxColorValue = 255),
                    legend.border.line.width = 0,
                    legend.font.color = global.font.color,
                    legend.font.family = global.font.family,
                    legend.font.size = 10,
                    margin.top = NULL,
                    margin.bottom = NULL,
                    margin.left = NULL,
                    margin.right = NULL,
                    margin.inner.pad = NULL,
                    grid.show = TRUE,
                    y.title = "",
                    y.title.font.color = global.font.color,
                    y.title.font.family = global.font.family,
                    y.title.font.size = 12,
                    y.line.width = 0,
                    y.line.color = rgb(0, 0, 0, maxColorValue = 255),
                    y.tick.mark.length = 5,
                    y.bounds.minimum = NULL,
                    y.bounds.maximum = NULL,
                    y.tick.distance = NULL,
                    y.zero = FALSE,
                    y.zero.line.width = 0,
                    y.zero.line.color = rgb(225, 225, 225, maxColorValue = 255),
                    y.data.reversed = FALSE,
                    y.grid.width = 0 * grid.show,
                    y.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                    y.tick.show = TRUE,
                    y.tick.suffix = "",
                    y.tick.prefix = "",
                    y.tick.format= "",
                    y.hovertext.format= y.tick.format,
                    y.tick.angle = NULL,
                    y.tick.font.color = global.font.color,
                    y.tick.font.family = global.font.family,
                    y.tick.font.size = 10,
                    x.title = "",
                    x.title.font.color = global.font.color,
                    x.title.font.family = global.font.family,
                    x.title.font.size = 12,
                    x.line.width = 0,
                    x.line.color = rgb(0, 0, 0, maxColorValue = 255),
                    x.tick.marks = "",
                    x.tick.mark.length = 5,
                    x.bounds.minimum = NULL,
                    x.bounds.maximum = NULL,
                    x.tick.distance = NULL,
                    x.zero = TRUE,
                    x.zero.line.width = 0,
                    x.zero.line.color = rgb(225, 225, 225, maxColorValue = 255),
                    x.data.reversed = FALSE,
                    x.grid.width = 1 * grid.show,
                    x.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                    x.tick.show = TRUE,
                    x.tick.suffix = "",
                    x.tick.prefix = "",
                    x.tick.format = "",
                    x.hovertext.format = x.tick.format,
                    x.tick.angle = NULL,
                    x.tick.font.color = global.font.color,
                    x.tick.font.family = global.font.family,
                    x.tick.font.size = 10,
                    y.tick.label.wrap = TRUE,
                    y.tick.label.wrap.nchar = 21,
                    marker.border.width = 1,
                    marker.border.colors = colors,
                    marker.border.opacity = opacity,
                    tooltip.show = TRUE,
                    modebar.show = FALSE,
                    bar.gap = 0.15,
                    data.label.show = FALSE,
                    data.label.font.family = global.font.family,
                    data.label.font.size = 10,
                    data.label.font.color = global.font.color,
                    data.label.format = "",
                    data.label.prefix = "",
                    data.label.suffix = "",
                    data.label.threshold = NULL,
                    average.series = NULL,
                    average.color = rgb(230, 230, 230, maxColorValue = 255))
{
    # Data checking
    ErrorIfNotEnoughData(x)
    chart.matrix <- checkMatrixNames(x)
    is.stacked <- grepl("Stacked", type, fixed=T)
    is.hundred.percent.stacked <- grepl("100% Stacked", type, fixed=T)
    if (is.stacked && ncol(chart.matrix) < 2)
        stop(paste(type, "requires more than one series. Use Bar charts instead for this data."))
    if (is.stacked && (any(is.na(chart.matrix)) || any(chart.matrix < 0)))
        stop("Stacked charts cannot be produced with missing or negative values.")
    if (is.hundred.percent.stacked && any(rowSums(chart.matrix) == 0))
        stop("100% stacked charts cannot be produced with rows that do not contain positive values.")
    if (any(is.na(as.matrix(chart.matrix))))
        warning("Missing values have been set to zero.")
    if (type == "Stacked")
        type <- "Stacked Bar"
    if (type == "100% Stacked")
        type <- "100% Stacked Bar"

    # Some minimal data cleaning
    # Assume formatting and Qtable/attribute handling already done
    data.label.mult <- 1
    if (is.hundred.percent.stacked)
        chart.matrix <- cum.data(chart.matrix, "column.percentage")

    if (percentFromD3(data.label.format)) {
        data.label.suffix <- paste0("%", data.label.suffix)
        data.label.mult <- 100
    }
    data.label.decimals <- decimalsFromD3(data.label.format)

    matrix.labels <- names(dimnames(chart.matrix))
    if (nchar(y.title) == 0 && length(matrix.labels) == 2)
        y.title <- matrix.labels[1]
    x.labels.full <- rownames(chart.matrix)

    # Constants
    hover.mode <- if (tooltip.show) "closest" else FALSE
    barmode <- if (is.stacked) "stack" else ""
    if (is.null(opacity))
        opacity <- 1
    if (is.null(marker.border.opacity))
        marker.border.opacity <- opacity
    eval(colors) # not sure why, but this is necessary for bars to appear properly

    title.font = list(family = title.font.family, size = title.font.size, color = title.font.color)
    subtitle.font = list(family = subtitle.font.family, size = subtitle.font.size, color = subtitle.font.color)
    x.title.font = list(family = x.title.font.family, size = x.title.font.size, color = x.title.font.color)
    y.title.font = list(family = y.title.font.family, size = y.title.font.size, color = y.title.font.color)
    ytick.font = list(family = y.tick.font.family, size = y.tick.font.size, color = y.tick.font.color)
    xtick.font = list(family = x.tick.font.family, size = x.tick.font.size, color = x.tick.font.color)
    footer.font = list(family = footer.font.family, size = footer.font.size, color = footer.font.color)
    legend.font = list(family = legend.font.family, size = legend.font.size, color = legend.font.color)
    data.label.font = list(family = data.label.font.family, size = data.label.font.size, color = data.label.font.color)

    if (ncol(chart.matrix) == 1)
        legend.show <- FALSE
    legend <- setLegend(type, legend.font, legend.ascending, legend.fill.color, legend.fill.opacity,
                        legend.border.color, legend.border.line.width, legend.position.x, legend.position.y)
    footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate=FALSE)

    # Format axis labels
    tmp.label <- sprintf(paste0("%s%.", data.label.decimals, "f%s"),
                data.label.prefix, max(chart.matrix), data.label.suffix)
    x.range <- setValRange(x.bounds.minimum, x.bounds.maximum, chart.matrix, is.null(x.tick.distance))
    xtick <- setTicks(x.range$min, x.range$max, x.tick.distance, x.data.reversed,
                  data = if (data.label.show && !is.stacked) chart.matrix else NULL, type = type,
                  labels = tmp.label, label.font.size = data.label.font.size)
    ytick <- setTicks(y.bounds.minimum, y.bounds.maximum, y.tick.distance, !y.data.reversed)
    axisFormat <- formatLabels(chart.matrix, type, y.tick.label.wrap, y.tick.label.wrap.nchar,
                               y.tick.format, x.tick.format)

    yaxis <- setAxis(y.title, "left", axisFormat, y.title.font,
                  y.line.color, y.line.width, y.grid.width * grid.show, y.grid.color,
                  ytick, ytick.font, y.tick.angle, y.tick.mark.length, y.tick.distance,
                  y.tick.format, y.tick.prefix, y.tick.suffix, y.tick.show,
                  y.zero, y.zero.line.width, y.zero.line.color,
                  y.hovertext.format)
    xaxis <- setAxis(x.title, "bottom", axisFormat, x.title.font,
                  x.line.color, x.line.width, x.grid.width * grid.show, x.grid.color,
                  xtick, xtick.font, x.tick.angle, x.tick.mark.length, x.tick.distance,
                  x.tick.format, x.tick.prefix, x.tick.suffix, x.tick.show,
                  x.zero, x.zero.line.width, x.zero.line.color,
                  x.hovertext.format)

    # Work out margin spacing
    margins <- list(t = 20, b = 20, r = 60, l = 80, pad = 0)
    margins <- setMarginsForAxis(margins, axisFormat, yaxis)
    margins <- setMarginsForAxis(margins, as.character(range(x)), xaxis)
    margins <- setMarginsForText(margins, title, subtitle, footer, title.font.size,
                                 subtitle.font.size, footer.font.size)
    margins <- setMarginsForLegend(margins, legend.show, legend, colnames(chart.matrix))
    margins <- setCustomMargins(margins, margin.top, margin.bottom, margin.left,
                    margin.right, margin.inner.pad)
    footer.axis <- setFooterAxis(footer, footer.font, margins)

    # Data label annotations
    data.annotations <- NULL
    if (data.label.show)
        data.annotations <- dataLabelPositions(chart.matrix = chart.matrix,
                            annotations = NULL,
                            data.label.mult = data.label.mult,
                            bar.decimals = data.label.decimals,
                            bar.prefix = data.label.prefix,
                            bar.suffix = data.label.suffix,
                            barmode = barmode,
                            swap.axes.and.data = TRUE,
                            bar.gap = bar.gap,
                            display.threshold = data.label.threshold,
                            dates = axisFormat$ymd)

    ## Initiate plotly object
    p <- plot_ly(as.data.frame(chart.matrix))
    if (is.null(rownames(chart.matrix)))
        rownames(chart.matrix) <- 1:nrow(chart.matrix)
    x.labels <- axisFormat$labels
    y.labels <- colnames(chart.matrix)
    yaxis2 <- NULL

    ## Add a trace for each col of data in the matrix
    for (i in 1:ncol(chart.matrix))
    {
        y <- as.numeric(chart.matrix[, i])
        x <- x.labels

        marker <- list(color = toRGB(colors[i], alpha = opacity),
                      line = list(color = toRGB(marker.border.colors[i],
                      alpha = marker.border.opacity),
                      width = marker.border.width))

        # add invisible line to force all categorical labels to be shown
        if (!is.stacked && i == 1)
            p <- add_trace(p, x = rep(min(y,na.rm = TRUE), length(y)), y = x,
                           type = "scatter", mode = "lines",
                           hoverinfo = "none", showlegend = FALSE, opacity = 0)

        # this is the main trace for each data series
        p <- add_trace(p, x = y, y = x, type = "bar", orientation = "h", marker = marker,
                       name  =  y.labels[i], legendgroup  =  i,
                       text = autoFormatLongLabels(x.labels.full, wordwrap = TRUE, truncate = FALSE),
                       hoverinfo  = setHoverText(yaxis, chart.matrix, is.bar = TRUE))

        if (fit.type != "None" && is.stacked && i == 1)
            warning("Line of best fit not shown for stacked charts.")
        if (fit.type != "None" && !is.stacked)
        {
            tmp.fit <- fitSeries(x, y, fit.type, fit.ignore.last, yaxis$type)
            tmp.fname <- if (ncol(chart.matrix) == 1)  fit.line.name
                         else sprintf("%s: %s", fit.line.name, y.labels[i])
            p <- add_trace(p, x = tmp.fit$y, y = tmp.fit$x, type = 'scatter', mode = "lines",
                      name = tmp.fname, legendgroup = i, showlegend = FALSE,
                      line = list(dash = fit.line.type, width = fit.line.width,
                      color = fit.line.colors[i], shape = 'spline'), opacity = fit.line.opacity)
        }

        # Only used for small multiples
        if (!is.null(average.series))
            p <- add_trace(p, y = x, x = average.series, name = "Average",
                    type = "scatter", mode = "lines", showlegend = FALSE,
                    line = list(color = average.color))



        if (data.label.show && !is.stacked)
        {
            y.range <- getRange(x, yaxis, axisFormat)
            yaxis2 <- list(overlaying = "y", visible = FALSE, range = y.range)
            x.sign <- sign(data.annotations$x[,i])
            if (x.data.reversed)
                x.sign <- -1 * x.sign
            x.diff <- x.sign * diff(range(data.annotations$x))/100
            p <- add_text(p, yaxis = "y2", x = data.annotations$x[,i] + x.diff,
                      y = data.annotations$y[,i],
                      text = data.annotations$text[,i],
                      textposition = ifelse(x.sign >= 0, "middle right", "middle left"),
                      textfont = data.label.font, hoverinfo = "none",
                      showlegend = FALSE, legendgroup = i)
        }
    }
    p <- addSubtitle(p, subtitle, subtitle.font, margins)
    p <- config(p, displayModeBar = modebar.show)
    p$sizingPolicy$browser$padding <- 0
    p <- layout(p,
        title = title,
        showlegend = legend.show,
        legend = legend,
        yaxis = yaxis,
        xaxis4 = footer.axis,
        yaxis2 = yaxis2,
        xaxis = xaxis,
        margin = margins,
        plot_bgcolor = toRGB(charting.area.fill.color, alpha = charting.area.fill.opacity),
        paper_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity),
        hovermode = hover.mode,
        titlefont = title.font,
        font = data.label.font,
        annotations = if (is.stacked) data.annotations else NULL,
        bargap = bar.gap,
        barmode = barmode
    )
    result <- list(htmlwidget = p)
    class(result) <- "StandardChart"
    result
}

