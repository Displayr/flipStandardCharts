#' BarMultiColor
#'
#' Bar charts with many colors
#' @inherit Bar
#' @importFrom flipChartBasics ChartColors
#' @importFrom plotly plot_ly layout
#' @export
BarMultiColor <- function(x,
                    annotation.list = NULL,
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
                    background.fill.color = "transparent",
                    background.fill.opacity = 1,
                    charting.area.fill.color = background.fill.color,
                    charting.area.fill.opacity = 0,
                    margin.top = NULL,
                    margin.bottom = NULL,
                    margin.left = NULL,
                    margin.right = NULL,
                    margin.inner.pad = NULL,
                    margin.autoexpand = TRUE,
                    grid.show = TRUE,
                    y.title = "",
                    y.title.font.color = global.font.color,
                    y.title.font.family = global.font.family,
                    y.title.font.size = 12,
                    y.line.width = 0,
                    y.line.color = rgb(0, 0, 0, maxColorValue = 255),
                    y.tick.mark.length = 3,
                    y.bounds.minimum = NULL,
                    y.bounds.maximum = NULL,
                    y.tick.distance = NULL,
                    y.tick.maxnum = 11,
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
                    x.tick.mark.length = 0,
                    x.bounds.minimum = NULL,
                    x.bounds.maximum = NULL,
                    x.tick.distance = NULL,
                    x.tick.maxnum = NULL,
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
                    x.tick.angle = NULL,
                    x.tick.font.color = global.font.color,
                    x.tick.font.family = global.font.family,
                    x.tick.font.size = 10,
                    y.tick.label.wrap = TRUE,
                    y.tick.label.wrap.nchar = 21,
                    hovertext.font.family = global.font.family,
                    hovertext.font.size = 11,
                    marker.border.width = 1,
                    marker.border.colors = colors,
                    marker.border.opacity = NULL,
                    data.label.show = FALSE,
                    data.label.font.autocolor = FALSE,
                    data.label.font.family = global.font.family,
                    data.label.font.size = 10,
                    data.label.font.color = global.font.color,
                    data.label.format = "",
                    data.label.prefix = "",
                    data.label.suffix = "",
                    data.label.threshold = NULL,
                    x.hovertext.format = data.label.format,
                    tooltip.show = TRUE,
                    modebar.show = FALSE,
                    zoom.enable = TRUE,
                    bar.gap = 0.15)
{
    ErrorIfNotEnoughData(x)
    if (isPercentData(x))
    {
        if (isAutoFormat(x.tick.format))
            x.tick.format <- paste0(x.tick.format, "%")
        if (isAutoFormat(x.hovertext.format))
            x.hovertext.format <- paste0(x.hovertext.format, "%")
        if (isAutoFormat(data.label.format))
            data.label.format <- paste0(data.label.format, "%")

        sfx <- checkSuffixForExtraPercent(c(x.tick.suffix, data.label.suffix),
            c(x.tick.format, data.label.format))
        x.tick.suffix <- sfx[1]
        data.label.suffix <- sfx[2]

    }

    chart.matrix <- checkMatrixNames(x)
    annot.data <- x
    if (NCOL(chart.matrix) > 1)
    {
        warning("Column chart with multi color series can only show a single series. To show multiple series use Small Multiples")
        chart.matrix <- chart.matrix[,1, drop = FALSE]
    }
    if (bar.gap < 0.0 || bar.gap >= 1.0)
    {
        warning("Parameter 'bar gap' must be between 0 and 1. ",
                "Invalid 'bar gap' set to default value of 0.15.")
        bar.gap <- 0.15
    }


    matrix.labels <- names(dimnames(chart.matrix))
    if (nchar(y.title) == 0 && length(matrix.labels) == 2)
        y.title <- matrix.labels[1]

    # Constants
    hover.mode <- if (tooltip.show) "closest" else FALSE
    if (is.null(opacity))
        opacity <- 1
    if (is.null(marker.border.opacity) && opacity > 0.85)
        marker.border.opacity <- opacity
    else if (is.null(marker.border.opacity)) # trying to hide gap in the middle
        marker.border.opacity <- opacity/(4 + 3*(opacity < 0.7))
    colors <- vectorize(colors, NROW(chart.matrix))
    if (is.null(marker.border.colors))
        marker.border.colors <- colors
    marker.border.colors <- vectorize(marker.border.colors, NROW(chart.matrix))

    if (data.label.font.autocolor)
        dlab.color <- autoFontColor(colors)
    else
        dlab.color <- vectorize(data.label.font.color, NROW(chart.matrix))

    data.label.show = vectorize(data.label.show, NROW(chart.matrix))
    data.label.font = list(family = data.label.font.family, size = data.label.font.size, color = dlab.color)
    title.font = list(family = title.font.family, size = title.font.size, color = title.font.color)
    subtitle.font = list(family = subtitle.font.family, size = subtitle.font.size, color = subtitle.font.color)
    x.title.font = list(family = x.title.font.family, size = x.title.font.size, color = x.title.font.color)
    y.title.font = list(family = y.title.font.family, size = y.title.font.size, color = y.title.font.color)
    ytick.font = list(family = y.tick.font.family, size = y.tick.font.size, color = y.tick.font.color)
    xtick.font = list(family = x.tick.font.family, size = x.tick.font.size, color = x.tick.font.color)
    footer.font = list(family = footer.font.family, size = footer.font.size, color = footer.font.color)
    footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate = FALSE)

    type <- "Bar"
    tmp.label <- formatByD3(max(chart.matrix), data.label.format,
                 data.label.prefix, data.label.suffix, decimals = 0)

    if (is.null(x.bounds.maximum) || is.na(x.bounds.maximum) || x.bounds.maximum == "")
        x.bounds.maximum <- NULL
    x.range <- setValRange(x.bounds.minimum, x.bounds.maximum, chart.matrix, x.zero, is.null(x.tick.distance))
    y.range <- setValRange(y.bounds.minimum, y.bounds.maximum, axisFormat, y.zero, is.null(y.tick.distance), is.bar = TRUE)
    xtick <- setTicks(x.range$min, x.range$max, x.tick.distance, x.data.reversed,
                  data = NULL, type = type,
                  labels = tmp.label, label.font.size = data.label.font.size)
    ytick <- setTicks(y.range$min, y.range$max, y.tick.distance, !y.data.reversed)
    axisFormat <- formatLabels(chart.matrix, type, y.tick.label.wrap, y.tick.label.wrap.nchar,
                               y.tick.format, x.tick.format)

    yaxis <- setAxis(y.title, "left", axisFormat, y.title.font,
                  y.line.color, y.line.width, y.grid.width, y.grid.color,
                  ytick, ytick.font, y.tick.angle, y.tick.mark.length, y.tick.distance,
                  y.tick.format, y.tick.prefix, y.tick.suffix, y.tick.show,
                  y.zero, y.zero.line.width, y.zero.line.color,
                  y.hovertext.format, num.maxticks = y.tick.maxnum, zoom.enable = zoom.enable)
    xaxis <- setAxis(x.title, "bottom", axisFormat, x.title.font,
                  x.line.color, x.line.width, x.grid.width, x.grid.color,
                  xtick, xtick.font, x.tick.angle, x.tick.mark.length, x.tick.distance,
                  x.tick.format, x.tick.prefix, x.tick.suffix, x.tick.show,
                  x.zero, x.zero.line.width, x.zero.line.color,
                  x.hovertext.format, num.maxticks = x.tick.maxnum, zoom.enable, zoom.enable)

    # Work out margin spacing
    margins <- list(t = 20, b = 20, r = 60, l = 80, pad = 0)
    margins <- setMarginsForAxis(margins, axisFormat, yaxis)
    margins <- setMarginsForAxis(margins, as.character(range(x)), xaxis)
    margins <- setMarginsForText(margins, title, subtitle, footer, title.font.size,
                                 subtitle.font.size, footer.font.size)
    margins <- setMarginsForLegend(margins, FALSE, NULL, NULL)
    margins <- setCustomMargins(margins, margin.top, margin.bottom, margin.left,
                    margin.right, margin.inner.pad)
    margins$autoexpand <- margin.autoexpand

    x <- axisFormat$labels
    y <- as.numeric(chart.matrix[,1])
    y.filled <- ifelse(is.finite(y), y, 0)
    x.text <- formatByD3(y, x.hovertext.format)
    marker = list(color = toRGB(colors, alpha = opacity),
                line = list(color = toRGB(marker.border.colors,
                alpha = marker.border.opacity), width = marker.border.width))
    hoverfont <- list(color = autoFontColor(colors), size = hovertext.font.size,
                family = hovertext.font.family)

    # Add invisible trace to force all labels to be shown
    # (including missing)
    tmp.min <- if (any(is.finite(chart.matrix))) min(chart.matrix[is.finite(chart.matrix)])
               else x.bounds.minimum
    p <- plot_ly(as.data.frame(chart.matrix))
    p <- add_trace(p, y = x,
            x = rep(tmp.min, length(x)),
            mode = if (notAutoRange(xaxis)) "markers" else "lines",
            type = "scatter", cliponaxis = TRUE,
            hoverinfo = "skip", showlegend = FALSE, opacity = 0)

    p <- add_trace(p, y = x, x = y.filled, type = "bar", orientation = "h",
                   marker = marker, hoverlabel = list(font = hoverfont),
                   cliponaxis = FALSE,
                   hovertemplate = "%{x}<extra>%{y}</extra>")

    if (any(data.label.show))
    {
        source.text <- formatByD3(y, data.label.format,
               data.label.prefix, data.label.suffix, decimals = 0)
        p <- addBarTypeChartLabelAnnotTrace(p, type = "Bar", NULL,
                data.label.xpos = y.filled, data.label.ypos = x, data.label.text = source.text,
                data.label.show = data.label.show, data.label.sign = getSign(y, xaxis), 0,
                annotation.list, annot.data, i = 1,
                xaxis = "x", yaxis = "y", data.label.font, is.stacked = FALSE, data.label.centered = FALSE)
    }

    # add scatter trace to ensure hover is always shown
    p <- add_trace(p, x = y, y = x, type = "scatter",
                   mode = "markers", marker = list(color = colors, opacity = 0),
                   hoverlabel = list(font = list(color = autoFontColor(colors),
                   size = hovertext.font.size, family = hovertext.font.family),
                   bgcolor = colors), hovertemplate = "%{x}<extra>%{y}</extra>")

    annot <- list(setSubtitle(subtitle, subtitle.font, margins),
                           setTitle(title, title.font, margins),
                           setFooter(footer, footer.font, margins))
    annot <- Filter(Negate(is.null), annot)

    p <- config(p, displayModeBar = modebar.show)
    p$sizingPolicy$browser$padding <- 0
    p <- layout(p,
        showlegend = FALSE,
        yaxis = yaxis,
        xaxis = xaxis,
        margin = margins,
        plot_bgcolor = toRGB(charting.area.fill.color, alpha = charting.area.fill.opacity),
        paper_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity),
        annotations = annot,
        hoverlabel = list(namelength = -1, bordercolor = "transparent",
            font = list(size = hovertext.font.size, family = hovertext.font.family)),
        hovermode = hover.mode,
        bargap = bar.gap,
        barmode = 'overlay'
    )
    attr(p, "can-run-in-root-dom") <- TRUE
    result <- list(htmlwidget = p)
    class(result) <- "StandardChart"
    attr(result, "ChartType") <- "Bar Clustered"
    result
}

