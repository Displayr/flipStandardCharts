#' Line chart
#'
#' Plot line chart
#' @inherit AreaChart 
#' @examples
#' z <- structure(c(1L, 2L, 3L, 4L, 5L, 2L, 3L, 4L, 5L, 6L),  .Dim = c(5L, 2L),
#'       .Dimnames = list(c("T", "U", "V", "W", "X"), c("A", "B")))
#' LineChart(z)
#' @importFrom grDevices rgb
#' @importFrom flipChartBasics ChartColors
#' @importFrom plotly plot_ly config toRGB add_trace add_text layout hide_colorbar
#' @importFrom stats loess loess.control lm predict
#' @export
LineChart <-   function(x,
                    type = "Line",
                    fit.type = "None", # can be "Smooth" or anything else
                    fit.ignore.last = FALSE,
                    fit.line.type = "dot",
                    fit.line.width = 1,
                    fit.line.name = "Fitted",
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
                    colors = ChartColors(max(1, ncol(x))),
                    fit.line.colors = colors,
                    opacity = 1,
                    grid.show = TRUE,
                    background.fill.color = rgb(255, 255, 255, maxColorValue = 255),
                    background.fill.opacity = 1,
                    charting.area.fill.color = background.fill.color,
                    charting.area.fill.opacity = 1,
                    legend.show = TRUE,
                    legend.fill.color = background.fill.color,
                    legend.fill.opacity = 1,
                    legend.border.color = rgb(44, 44, 44, maxColorValue = 255),
                    legend.border.line.width = 0,
                    legend.font.color = global.font.color,
                    legend.font.family = global.font.family,
                    legend.font.size = 10,
                    legend.position.x = 1.02,
                    legend.position.y = 1.00,
                    legend.ascending = NA,
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
                    y.zero = TRUE,
                    y.zero.line.width = 0,
                    y.zero.line.color = rgb(44, 44, 44, maxColorValue = 255),
                    y.data.reversed = FALSE,
                    y.grid.width = 1 * grid.show,
                    y.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                    y.tick.show = TRUE,
                    y.tick.suffix = "",
                    y.tick.prefix = "",
                    y.tick.format = "",
                    y.hovertext.format = y.tick.format,
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
                    x.zero.line.width = 0,
                    x.zero.line.color = rgb(44, 44, 44, maxColorValue = 255),
                    x.data.reversed = FALSE,
                    x.grid.width = 0 * grid.show,
                    x.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                    x.tick.show = TRUE,
                    x.tick.format = "",
                    x.hovertext.format = x.tick.format,
                    x.tick.angle = NULL,
                    x.tick.font.color = global.font.color,
                    x.tick.font.family = global.font.family,
                    x.tick.font.size = 10,
                    x.tick.label.wrap = TRUE,
                    x.tick.label.wrap.nchar = 21,
                    series.marker.show = NULL,
                    series.marker.colors = colors,
                    series.marker.opacity = 1,
                    series.marker.size = 6,
                    series.line.width = 3,
                    series.line.colors = colors,
                    series.line.opacity = 1,
                    series.marker.border.width = 1,
                    series.marker.border.colors = colors,
                    series.marker.border.opacity = 1,
                    tooltip.show = TRUE,
                    modebar.show = FALSE,
                    data.label.show = FALSE,
                    data.label.font.family = global.font.family,
                    data.label.font.color = global.font.color,
                    data.label.font.size = 10,
                    data.label.decimals = 2,
                    data.label.prefix = "",
                    data.label.suffix = "",
                    data.label.position = "top middle")
{
    # Some minimal data cleaning
    # Assume formatting and Qtable/attribute handling already done
    # Find gaps which are NOT at the ends of the series
    chart.matrix <- checkMatrixNames(x)
    if (is.null(series.line.width))
        series.line.width <- 3
    matrix.labels <- names(dimnames(chart.matrix))
    if (nchar(x.title) == 0 && length(matrix.labels) == 2)
        x.title <- matrix.labels[1]
    x.labels.full <- rownames(chart.matrix)

    # Constants
    plotly.type <- "scatter"
    hover.mode <- if (tooltip.show) "closest" else FALSE
    series.marker.symbols <- if (is.null(series.marker.show)) rep(100, ncol(chart.matrix))
                             else series.marker.show
    series.mode <- "lines+markers"
    if (is.null(series.marker.show) || series.marker.show == "none")
        series.mode <- "lines"
    eval(colors) # not sure why, but this is necessary for bars to appear properly

    title.font=list(family=title.font.family, size=title.font.size, color=title.font.color)
    subtitle.font=list(family=subtitle.font.family, size=subtitle.font.size, color=subtitle.font.color)
    x.title.font=list(family=x.title.font.family, size=x.title.font.size, color=x.title.font.color)
    y.title.font=list(family=y.title.font.family, size=y.title.font.size, color=y.title.font.color)
    ytick.font=list(family=y.tick.font.family, size=y.tick.font.size, color=y.tick.font.color)
    xtick.font=list(family=x.tick.font.family, size=x.tick.font.size, color=x.tick.font.color)
    footer.font=list(family=footer.font.family, size=footer.font.size, color=footer.font.color)
    legend.font=list(family=legend.font.family, size=legend.font.size, color=legend.font.color)
    data.label.font=list(family=data.label.font.family, size=data.label.font.size, color=data.label.font.color)

    if (ncol(chart.matrix) == 1)
        legend.show <- FALSE
    legend <- setLegend(type, legend.font, legend.ascending, legend.fill.color, legend.fill.opacity,
                        legend.border.color, legend.border.line.width, legend.position.x, legend.position.y)
    footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate=FALSE)

    # Format axis labels
    xtick <- setTicks(x.bounds.minimum, x.bounds.maximum, x.tick.distance, x.data.reversed)
    ytick <- setTicks(y.bounds.minimum, y.bounds.maximum, y.tick.distance, y.data.reversed)
    axisFormat <- formatLabels(chart.matrix, type, x.tick.label.wrap, x.tick.label.wrap.nchar,
                               x.tick.format, y.tick.format)

    yaxis <- setAxis(y.title, "left", axisFormat, y.title.font,
                  y.line.color, y.line.width, y.grid.width, y.grid.color,
                  ytick, ytick.font, y.tick.angle, y.tick.mark.length, y.tick.distance, y.tick.format,
                  y.tick.prefix, y.tick.suffix,
                  y.tick.show, y.zero, y.zero.line.width, y.zero.line.color,
                  y.hovertext.format)
    xaxis <- setAxis(x.title, "bottom", axisFormat, x.title.font,
                  x.line.color, x.line.width, x.grid.width, x.grid.color,
                  xtick, xtick.font, x.tick.angle, x.tick.mark.length, x.tick.distance, x.tick.format,
                  "", "", x.tick.show, FALSE, x.zero.line.width, x.zero.line.color,
                  x.hovertext.format, axisFormat$labels)

    # Work out margin spacing
    margins <- list(t = 20, b = 50, r = 60, l = 80, pad = 0)
    margins <- setMarginsForAxis(margins, axisFormat, xaxis)
    margins <- setMarginsForText(margins, title, subtitle, footer, title.font.size,
                                 subtitle.font.size, footer.font.size)
    margins <- setMarginsForLegend(margins, legend.show, legend)
    if (!is.null(margin.top))
        margins$top <- margin.top
    if (!is.null(margin.bottom))
        margins$bottom <- margin.bottom
    if (!is.null(margin.left))
        margins$left <- margin.left
    if (!is.null(margin.right))
        margins$right <- margin.right
    if (!is.null(margin.inner.pad))
        margins$pad <- margin.inner.pad

    # Finalise text in margins
    footer.axis <- setFooterAxis(footer, footer.font, margins)
    subtitle.axis <- setSubtitleAxis(subtitle, subtitle.font, title, title.font)

    ## Initiate plotly object
    p <- plot_ly(as.data.frame(chart.matrix))
    if (is.null(rownames(chart.matrix)))
        rownames(chart.matrix) <- 1:nrow(chart.matrix)
    x.labels <- axisFormat$labels
    y.labels <- colnames(chart.matrix)
    xaxis2 <- NULL

    ## Add a trace for each col of data in the matrix
    for (i in 1:ncol(chart.matrix))
    {
        y <- as.numeric(chart.matrix[, i])
        x <- x.labels

        lines <- list(width = series.line.width,
                      color = toRGB(colors[i], alpha = series.line.opacity))

        marker <- NULL
        if (!is.null(series.mode) && regexpr('marker', series.mode) >= 1)
            marker <- list(size = series.marker.size,
                       color = toRGB(series.marker.colors[i], alpha = series.marker.opacity),
                       symbol = series.marker.symbols[i],
                       line = list(
                       color = toRGB(series.marker.border.colors[i], alpha = series.marker.border.opacity),
                       width = series.marker.border.width))
        y.label <- y.labels[i]
        tmp.group <- paste("group", i)

        # Need to add data labels first otherwise it will override hovertext in area chart
        if (data.label.show)
        {
            source.text <- paste(data.label.prefix,
                 FormatAsReal(chart.matrix[, i], decimals = data.label.decimals),
                 data.label.suffix, sep = "")

            p <- add_trace(p, x = x, y = y,
                   type = "scatter",
                   mode = "text",
                   legendgroup = tmp.group,
                   name = y.label,
                   text = source.text,
                   textfont = data.label.font,
                   textposition = data.label.position,
                   hoverinfo = "none",
                   showlegend = FALSE)
        }

       # Draw line - main trace
        p <- add_trace(p,
                   type = plotly.type,
                   x = x,
                   y = y,
                   connectgaps = FALSE,
                   line = lines,
                   name = y.label,
                   showlegend = (type == "Line"),
                   legendgroup = tmp.group,
                   text = autoFormatLongLabels(x.labels.full, wordwrap=T, truncate=F),
                   hoverinfo  = setHoverText(xaxis, chart.matrix), 
                   marker = marker,
                   mode = series.mode)

        # single points (no lines) need to be added separately
        not.na <- is.finite(y)
        is.single <- not.na & c(TRUE, !not.na[-nrow(chart.matrix)]) & c(!not.na[-1], TRUE)
        if (any(is.single) && type == "Line")
        {
            p <- add_trace(p,
                       type = "scatter",
                       mode = "markers",
                       x = x[is.single],
                       y = y[is.single],
                       legendgroup = tmp.group,
                       name = y.label,
                       marker = if (!is.null(marker)) marker
                                else list(color = toRGB(colors[i]),
                                     size = series.marker.size),
                       text = autoFormatLongLabels(x.labels.full[is.single], wordwrap=T, truncate=F),
                       hoverinfo  = setHoverText(xaxis, chart.matrix), 
                       showlegend = FALSE)
        }
        if (fit.type != "None")
        {
            tmp.fname <- if (ncol(chart.matrix) == 1)  fit.line.name
                     else sprintf("%s: %s", fit.line.name, y.labels[i])
            tmp.fit <- fitSeries(x, y, fit.type, fit.ignore.last, xaxis$type)
            p <- add_trace(p, x=tmp.fit$x, y=tmp.fit$y, type='scatter', mode="lines",
                      name=tmp.fname, legendgroup=tmp.group, showlegend=F,
                      line=list(dash=fit.line.type, width=fit.line.width,
                      color=fit.line.colors[i], shape='spline'))
        }
    }

    p <- config(p, displayModeBar = modebar.show)
    p$sizingPolicy$browser$padding <- 0
    p <- layout(p,
        title = title,
        showlegend = legend.show,
        legend = legend,
        yaxis = yaxis,
        xaxis4 = footer.axis,
        xaxis3 = subtitle.axis,
        xaxis2 = xaxis2,
        xaxis = xaxis,
        margin = margins,
        plot_bgcolor = toRGB(charting.area.fill.color, alpha = charting.area.fill.opacity),
        paper_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity),
        hovermode = hover.mode,
        titlefont = title.font,
        font = data.label.font
    )
    result <- list(plotly.plot = p)
    class(result) <- "StandardChart"
    result
}

