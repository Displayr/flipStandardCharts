#' Bar
#'
#' Bar Chart
#' @inherit Column
#' @param y.tick.label.wrap Logical; whether to wrap long labels on the y-axis (vertical).
#' @param y.tick.label.wrap.nchar Integer; number of characters in each line when \code{y.tick.label.wrap} is \code{TRUE}.
#' @param x.tick.suffix x-axis tick label suffix
#' @param x.tick.prefix x-axis tick label prefix
#' @importFrom grDevices rgb
#' @importFrom flipChartBasics ChartColors
#' @importFrom plotly plot_ly config toRGB add_trace add_text layout hide_colorbar
#' @importFrom stats loess loess.control lm predict
#' @examples
#' z <- structure(c(1L, 2L, 3L, 4L, 5L, 2L, 3L, 4L, 5L, 6L),  .Dim = c(5L, 2L),
#'       .Dimnames = list(c("T", "U", "V", "W", "X"), c("A", "B")))
#' Bar(z, type="100% Stacked Bar")
#' @export
Bar <- function(x,
                    type = "Bar",
                    colors = ChartColors(max(1, ncol(x), na.rm = TRUE)),
                    opacity = NULL,
                    fit.type = "None", # can be "Smooth" or anything else
                    fit.line.colors = colors,
                    fit.ignore.last = FALSE,
                    fit.line.type = "dot",
                    fit.line.width = 1,
                    fit.line.name = "Fitted",
                    fit.line.opacity = 1,
                    fit.CI.show = FALSE,
                    fit.CI.colors = fit.line.colors,
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
                    background.fill.color = "transparent",
                    background.fill.opacity = 1,
                    charting.area.fill.color = background.fill.color,
                    charting.area.fill.opacity = 0,
                    legend.show = NA,
                    legend.orientation = "Vertical",
                    legend.wrap = TRUE,
                    legend.wrap.nchar = 30,
                    legend.position.x = NULL,
                    legend.position.y = NULL,
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
                    hovertext.font.family = global.font.family,
                    hovertext.font.size = 11,
                    marker.border.width = 1,
                    marker.border.colors = colors,
                    marker.border.opacity = opacity,
                    tooltip.show = TRUE,
                    modebar.show = FALSE,
                    bar.gap = 0.15,
                    data.label.show = FALSE,
                    data.label.font.autocolor = FALSE,
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
    if (bar.gap < 0.0 || bar.gap >= 1.0)
    {
        warning("Parameter 'bar gap' must be between 0 and 1. ",
                "Invalid 'bar gap' set to default value of 0.15.")
        bar.gap <- 0.15
    }

    chart.matrix <- checkMatrixNames(x)
    is.stacked <- grepl("Stacked", type, fixed=T)
    if (is.stacked && ncol(chart.matrix) < 2)
    {
        warning("No stacking performed for only one series.")
        is.stacked <- FALSE
    }
    is.hundred.percent.stacked <- grepl("100% Stacked", type, fixed=T)
    if (any(is.na(as.matrix(chart.matrix))))
        warning("Missing values have been set to zero.")
    if (type == "Stacked")
        type <- "Stacked Bar"
    if (type == "100% Stacked")
        type <- "100% Stacked Bar"
    if (!is.stacked)
        type <- "Bar"

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
    barmode <- if (is.stacked) "relative" else "group"
    if (is.null(opacity))
        opacity <- if (fit.type == "None") 1 else 0.6
    if (is.null(marker.border.opacity))
        marker.border.opacity <- opacity
    eval(colors) # not sure why, but this is necessary for bars to appear properly

    if (is.stacked && data.label.font.autocolor)
        dlab.color <- autoFontColor(colors)
    else
        dlab.color <- vectorize(data.label.font.color, ncol(chart.matrix))

    data.label.font = lapply(dlab.color,
        function(cc) list(family = data.label.font.family, size = data.label.font.size, color = cc))
    title.font = list(family = title.font.family, size = title.font.size, color = title.font.color)
    subtitle.font = list(family = subtitle.font.family, size = subtitle.font.size, color = subtitle.font.color)
    x.title.font = list(family = x.title.font.family, size = x.title.font.size, color = x.title.font.color)
    y.title.font = list(family = y.title.font.family, size = y.title.font.size, color = y.title.font.color)
    ytick.font = list(family = y.tick.font.family, size = y.tick.font.size, color = y.tick.font.color)
    xtick.font = list(family = x.tick.font.family, size = x.tick.font.size, color = x.tick.font.color)
    footer.font = list(family = footer.font.family, size = footer.font.size, color = footer.font.color)
    legend.font = list(family = legend.font.family, size = legend.font.size, color = legend.font.color)

    legend.show <- setShowLegend(legend.show, NCOL(chart.matrix))
    legend <- setLegend(type, legend.font, legend.ascending, legend.fill.color, legend.fill.opacity,
                        legend.border.color, legend.border.line.width, legend.position.x, legend.position.y,
                        FALSE, legend.orientation)
    footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate = FALSE)

    # Format axis labels
    axisFormat <- formatLabels(chart.matrix, type, y.tick.label.wrap, y.tick.label.wrap.nchar,
                               y.tick.format, x.tick.format)
    x.range <- setValRange(x.bounds.minimum, x.bounds.maximum, chart.matrix, x.zero, is.null(x.tick.distance))
    y.range <- setValRange(y.bounds.minimum, y.bounds.maximum, axisFormat, y.zero, is.null(y.tick.distance), is.bar = TRUE)
    tmp.label <- sprintf(paste0("%s%.", data.label.decimals, "f%s"),
                data.label.prefix, max(chart.matrix), data.label.suffix)
    xtick <- setTicks(x.range$min, x.range$max, x.tick.distance, x.data.reversed,
                  data = if (data.label.show && !is.stacked) chart.matrix else NULL, type = type,
                  labels = tmp.label, label.font.size = data.label.font.size)
    ytick <- setTicks(y.range$min, y.range$max, y.tick.distance, !y.data.reversed, is.bar = TRUE)

    yaxis <- setAxis(y.title, "left", axisFormat, y.title.font,
                  y.line.color, y.line.width, y.grid.width * grid.show, y.grid.color,
                  ytick, ytick.font, y.tick.angle, y.tick.mark.length, y.tick.distance,
                  y.tick.format, y.tick.prefix, y.tick.suffix, y.tick.show,
                  y.zero, y.zero.line.width, y.zero.line.color,
                  y.hovertext.format, with.bars = TRUE)
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

    legend.text <- autoFormatLongLabels(colnames(chart.matrix), legend.wrap, legend.wrap.nchar)
    margins <- setMarginsForLegend(margins, legend.show, legend, legend.text)
    margins <- setCustomMargins(margins, margin.top, margin.bottom, margin.left,
                    margin.right, margin.inner.pad)

    # Set up numeric x-axis - this is used for data labels and hovertext
    y.range <- getRange(x, yaxis, axisFormat)
    yaxis2 <- list(overlaying = "y", visible = FALSE, range = y.range)
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
                        dates = axisFormat$ymd,
                        font = data.label.font)

    ## Initiate plotly object
    p <- plot_ly(as.data.frame(chart.matrix))
    if (is.null(rownames(chart.matrix)))
        rownames(chart.matrix) <- 1:nrow(chart.matrix)
    x.labels <- axisFormat$labels
    y.labels <- colnames(chart.matrix)

    ## Add a trace for each col of data in the matrix
    for (i in 1:ncol(chart.matrix))
    {
        y <- as.numeric(chart.matrix[, i])
        y.filled <- ifelse(is.finite(y), y, 0)
        x <- x.labels

        marker <- list(color = toRGB(colors[i], alpha = opacity),
                      line = list(color = toRGB(marker.border.colors[i],
                      alpha = marker.border.opacity),
                      width = marker.border.width))

        # add invisible line to force all categorical labels to be shown
        if (!is.stacked && i == 1)
        {
            p <- add_trace(p, x = rep(min(y,na.rm = TRUE), length(y)), y = x,
                           type = "scatter", mode = "lines",
                           hoverinfo = "skip", showlegend = FALSE, opacity = 0)
        }

        # this is the main trace for each data series
        # need to use y.filled to avoid plotly bug affecting bar-width
        p <- add_trace(p, x = y.filled, y = x, type = "bar", orientation = "h",
                       marker = marker, name  =  legend.text[i],
                       text = autoFormatLongLabels(x.labels.full, wordwrap = TRUE),
                       hoverlabel = list(font = list(color = autoFontColor(colors[i]),
                       size = hovertext.font.size, family = hovertext.font.family)),
                       hovertemplate = setHoverTemplate(i, yaxis, chart.matrix, is.bar = TRUE),
                       legendgroup = if (is.stacked && data.label.show) "all" else i)

        if (fit.type != "None" && is.stacked && i == 1)
            warning("Line of best fit not shown for stacked charts.")
        if (fit.type != "None" && !is.stacked)
        {
            tmp.fit <- fitSeries(x, y, fit.type, fit.ignore.last, yaxis$type, fit.CI.show)
            tmp.fname <- if (ncol(chart.matrix) == 1)  fit.line.name
                         else sprintf("%s: %s", fit.line.name, y.labels[i])
            p <- add_trace(p, x = tmp.fit$y, y = tmp.fit$x, type = 'scatter', mode = "lines",
                      name = tmp.fname, legendgroup = i, showlegend = FALSE,
                      hoverlabel = list(font = list(color = autoFontColor(fit.line.colors[i]),
                      size = hovertext.font.size, family = hovertext.font.family)),
                      line = list(dash = fit.line.type, width = fit.line.width,
                      color = fit.line.colors[i], shape = 'spline'), opacity = fit.line.opacity)
            if (fit.CI.show && !is.null(tmp.fit$lb))
            {
                p <- add_trace(p, y = tmp.fit$x, x = tmp.fit$lb, type = 'scatter',
                        mode = 'lines', name = "Lower bound of 95%CI",
                        hoverlabel = list(font = list(color = autoFontColor(fit.CI.colors[i]),
                        size = hovertext.font.size, family = hovertext.font.family)),
                        showlegend = FALSE, legendgroup = i,
                        line=list(color=fit.CI.colors[i], width=0, shape='spline'))
                p <- add_trace(p, y = tmp.fit$x, x = tmp.fit$ub, type = 'scatter',
                        mode = 'lines', name = "Upper bound of 95% CI",
                        hoverlabel = list(font = list(color = autoFontColor(fit.CI.colors[i]),
                        size = hovertext.font.size, family = hovertext.font.family)),
                        fill = "tonextx",
                        fillcolor = toRGB(fit.CI.colors[i], alpha = fit.CI.opacity),
                        showlegend = FALSE, legendgroup = i,
                        line = list(color=fit.CI.colors[i], width=0, shape='spline'))
            }
        }

        # Only used for small multiples
        if (!is.null(average.series))
            p <- add_trace(p, y = x, x = average.series, name = "Average",
                    type = "scatter", mode = "lines", showlegend = FALSE,
                    hoverlabel = list(font = list(color = autoFontColor(average.color),
                    size = hovertext.font.size, family = hovertext.font.family)),
                    line = list(color = average.color))

        # Plotly text marker positions are not spaced properly when placed to
        # the left of the bar (i.e. negative values or reversed axis).
        # Adjusted by controlling the size of the marker
        if (data.label.show)
        {
            x.sign <- getSign(data.annotations$x[,i], xaxis)
            x.diff <- if (is.stacked) 0 else diff(range(data.annotations$x))/100
            textpos <- if (is.stacked) "middle center"
                       else            ifelse(x.sign >= 0, "middle right", "middle left")
            p <- add_trace(p, x = data.annotations$x[,i] + x.diff, type = "scatter",
                      mode = "markers+text", marker = list(color = colors[i], opacity = 0,
                      size = ifelse(!is.stacked & data.annotations$x[,i] < 0, 7, 0)),
                      y = if (NCOL(chart.matrix) > 1) data.annotations$y[,i] else x,
                      yaxis = if (NCOL(chart.matrix) > 1) "y2" else "y",
                      text = data.annotations$text[,i], textposition = textpos,
                      textfont = data.label.font[[i]],
                      hovertemplate = setHoverTemplate(i, yaxis, chart.matrix, 
                      is.bar = TRUE, hide.category = TRUE),
                      hoverlabel = list(font = list(color = autoFontColor(colors[i]),
                      size = hovertext.font.size, family = hovertext.font.family)),
                      showlegend = FALSE, legendgroup = if (is.stacked) "all" else i)
        }

        # add scatter trace to ensure hover is always shown
        ypos <- if (NCOL(chart.matrix) > 1) data.annotations$y[,i] else x
        p <- add_trace(p, x = y.filled, y = ypos, type = "scatter", name = legend.text[i],
                   mode = "markers", marker = list(color = colors[i], opacity = 0),
                   text = autoFormatLongLabels(x.labels.full, wordwrap = TRUE),
                   hovertemplate = setHoverTemplate(i, yaxis, chart.matrix,
                   hide.category = yaxis$type == "date", is.bar = TRUE),
                   hoverlabel = list(font = list(color = autoFontColor(colors[i]),
                   size = hovertext.font.size, family = hovertext.font.family),
                   bgcolor = colors[i]), showlegend = FALSE,
                   yaxis = if (NCOL(chart.matrix) > 1) "y2" else "y")

    }
    annotations <- NULL
    n <- length(annotations)
    annotations[[n+1]] <- setFooter(footer, footer.font, margins)
    annotations[[n+2]] <- setSubtitle(subtitle, subtitle.font, margins)
    annotations[[n+3]] <- setTitle(title, title.font, margins)
    annotations <- Filter(Negate(is.null), annotations)

    p <- config(p, displayModeBar = modebar.show)
    p$sizingPolicy$browser$padding <- 0
    p <- layout(p,
        showlegend = legend.show,
        legend = legend,
        yaxis = yaxis,
        yaxis2 = yaxis2,
        xaxis = xaxis,
        margin = margins,
        plot_bgcolor = toRGB(charting.area.fill.color, alpha = charting.area.fill.opacity),
        paper_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity),
        hoverlabel = list(namelength = -1, bordercolor = "transparent",
            font = list(size = hovertext.font.size, family = hovertext.font.family)),
        hovermode = if (tooltip.show) "closest" else FALSE,
        annotations =  annotations,
        bargap = bar.gap,
        barmode = barmode
    )
    result <- list(htmlwidget = p)
    class(result) <- "StandardChart"
    result
}

