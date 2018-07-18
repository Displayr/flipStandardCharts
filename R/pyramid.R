#' Pyramid
#'
#' Bar charts with a centred axis
#' @inherit Bar
#' @importFrom flipChartBasics ChartColors
#' @importFrom plotly plot_ly layout
#' @export
Pyramid <- function(x,
                    colors = ChartColors(max(1, length(x))),
                    opacity = NULL,
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
                    background.fill.color = rgb(255, 255, 255, maxColorValue = 255),
                    background.fill.opacity = 0,
                    charting.area.fill.color = background.fill.color,
                    charting.area.fill.opacity = 0,
                    margin.top = NULL,
                    margin.bottom = NULL,
                    margin.left = NULL,
                    margin.right = NULL,
                    margin.inner.pad = NULL,
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
                    y.grid.width = 0,
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
                    x.grid.width = 0,
                    x.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                    x.tick.show = FALSE,
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
                    marker.border.width = 0,
                    marker.border.colors = colors,
                    marker.border.opacity = opacity,
                    data.label.show = FALSE,
                    data.label.font.family = global.font.family,
                    data.label.font.size = 10,
                    data.label.font.color = global.font.color,
                    data.label.format = "",
                    data.label.prefix = "",
                    data.label.suffix = "",
                    data.label.threshold = NULL,
                    tooltip.show = TRUE,
                    modebar.show = FALSE,
                    bar.gap = 0.15)
{
    if (length(dim(x)) > 1 && all(dim(x)) > 1)
        stop("Input data 'x' must be a vector or 1-dimensional table")
    if (any(sign(x) * sign(x)[1] < 0))
        stop("'Pyramid' charts cannot show a mixture of positive and negative values.")
    chart.matrix <- checkMatrixNames(x)

    data.label.mult <- 1
    if (percentFromD3(data.label.format)) {
        data.label.suffix <- paste0("%", data.label.suffix)
        data.label.mult <- 100
    }
    data.label.decimals <- decimalsFromD3(data.label.format)

    matrix.labels <- names(dimnames(chart.matrix))
    if (nchar(y.title) == 0 && length(matrix.labels) == 2)
        y.title <- matrix.labels[1]

    # Constants
    hover.mode <- if (tooltip.show) "closest" else FALSE
    if (is.null(opacity))
        opacity <- 1
    if (is.null(marker.border.opacity))
        marker.border.opacity <- opacity
    colors <- paste0(rep("", nrow(chart.matrix)), colors)
    #eval(colors) # not sure why, but this is necessary for bars to appear properly

    title.font = list(family = title.font.family, size = title.font.size, color = title.font.color)
    subtitle.font = list(family = subtitle.font.family, size = subtitle.font.size, color = subtitle.font.color)
    x.title.font = list(family = x.title.font.family, size = x.title.font.size, color = x.title.font.color)
    y.title.font = list(family = y.title.font.family, size = y.title.font.size, color = y.title.font.color)
    ytick.font = list(family = y.tick.font.family, size = y.tick.font.size, color = y.tick.font.color)
    xtick.font = list(family = x.tick.font.family, size = x.tick.font.size, color = x.tick.font.color)
    footer.font = list(family = footer.font.family, size = footer.font.size, color = footer.font.color)
    data.label.font = list(family = data.label.font.family, size = data.label.font.size, color = data.label.font.color)

    type <- "Bar"
    tmp.label <- sprintf(paste0("%s%.", data.label.decimals, "f%s"),
                data.label.prefix, max(chart.matrix), data.label.suffix)
    x.range <- setValRange(x.bounds.minimum, x.bounds.maximum, chart.matrix, is.null(x.tick.distance))
    xtick <- setTicks(x.range$min, x.range$max, x.tick.distance, x.data.reversed,
                  data = NULL, type = type,
                  labels = tmp.label, label.font.size = data.label.font.size)
    ytick <- setTicks(y.bounds.minimum, y.bounds.maximum, y.tick.distance, !y.data.reversed)
    axisFormat <- formatLabels(chart.matrix, type, y.tick.label.wrap, y.tick.label.wrap.nchar,
                               y.tick.format, x.tick.format)

    yaxis <- setAxis(y.title, "left", axisFormat, y.title.font,
                  y.line.color, y.line.width, y.grid.width, y.grid.color,
                  ytick, ytick.font, y.tick.angle, y.tick.mark.length, y.tick.distance,
                  y.tick.format, y.tick.prefix, y.tick.suffix, y.tick.show,
                  y.zero, y.zero.line.width, y.zero.line.color,
                  y.hovertext.format)
    xaxis <- setAxis(x.title, "bottom", axisFormat, x.title.font,
                  x.line.color, x.line.width, x.grid.width, x.grid.color,
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
    #margins <- setMarginsForLegend(margins, legend.show, legend, colnames(chart.matrix))
    margins <- setCustomMargins(margins, margin.top, margin.bottom, margin.left,
                    margin.right, margin.inner.pad)
    footer.axis <- setFooterAxis(footer, footer.font, margins)

    # Data label annotations
    # Also used for hovertext
    data.annotations <- dataLabelPositions(chart.matrix = chart.matrix,
                        annotations = NULL,
                        data.label.mult = data.label.mult,
                        bar.decimals = data.label.decimals,
                        bar.prefix = data.label.prefix,
                        bar.suffix = data.label.suffix,
                        barmode = "",
                        swap.axes.and.data = TRUE,
                        bar.gap = bar.gap,
                        display.threshold = data.label.threshold,
                        dates = axisFormat$ymd)

    x <- axisFormat$labels
    y <- as.numeric(chart.matrix[,1])
    p <- plot_ly(as.data.frame(chart.matrix))
    for (i in 1:nrow(chart.matrix))
    {
        marker <- list(color = toRGB(colors[i], alpha = opacity),
                      line = list(color = toRGB(marker.border.colors[i],
                      alpha = marker.border.opacity),
                      width = marker.border.width))
        p <- add_trace(p, x = y[i]/2, y = x[i], type = "bar", orientation = "h",
                       marker = marker, name  =  x[i],
                       text = formatByD3(y[i], x.hovertext.format), hoverinfo  = "name+text")
        p <- add_trace(p, x = -y[i]/2, y = x[i], type = "bar", orientation = "h",
                       marker = marker, name  =  x[i],
                       text = formatByD3(y[i], x.hovertext.format), hoverinfo  = "name+text")
    }

    yaxis2 <- NULL
    if (data.label.show)
    {
        y.range <- getRange(x, yaxis, axisFormat)
        yaxis2 <- list(overlaying = "y", visible = FALSE, range = y.range)
        x.sign <- sign(data.annotations$x[,1])
        if (x.data.reversed)
            x.sign <- -1 * x.sign
        x.diff <- x.sign * diff(range(data.annotations$x))/100
        p <- add_text(p, yaxis = "y2", x = rep(0, nrow(chart.matrix)),
                  y = data.annotations$y[,1],
                  text = data.annotations$text[,1],
                  textposition = "middle center",
                  textfont = data.label.font, hoverinfo = "none",
                  showlegend = FALSE, legendgroup = i)
    }

    p <- addSubtitle(p, subtitle, subtitle.font, margins)
    p <- config(p, displayModeBar = modebar.show)
    p$sizingPolicy$browser$padding <- 0
    p <- layout(p,
        title = title,
        showlegend = FALSE,
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
        bargap = bar.gap,
        barmode = 'overlay'
    )
    result <- list(htmlwidget = p)
    class(result) <- "StandardChart"
    result
}

