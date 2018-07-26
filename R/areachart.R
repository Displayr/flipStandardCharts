#' Area
#'
#' Area chart
#' @param marker.show Can be "none", "automatic" or a vector referencing
#' the plotly symbol dictionary using either numerics or strings.
#' @param marker.colors Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' be reversed. Only used if \code{marker.show} is \code{TRUE}.
#' @param marker.opacity Opacity for markers as an alpha value (0 to 1).
#' @param marker.size Size in pixels of marker
#' @param marker.border.width Width in pixels of border/line
#' around markers; 0 is no line
#' @param marker.border.colors Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' @param marker.border.opacity Opacity of border/line around
#' markers as an alpha value (0 to 1).
#' @param line.thickness Thickness, in pixels, of the series line
#' @param line.colors  Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' @param line.opacity Opacity for series lines as an alpha value (0 to 1).
#' @inherit Column
#' @examples
#' z <- structure(c(1L, 2L, 3L, 4L, 5L, 2L, 3L, 4L, 5L, 6L),  .Dim = c(5L, 2L),
#'       .Dimnames = list(c("T", "U", "V", "W", "X"), c("A", "B")))
#' Area(z)
#' @importFrom grDevices rgb
#' @importFrom flipChartBasics ChartColors
#' @importFrom plotly plot_ly config toRGB add_trace add_text layout hide_colorbar
#' @importFrom stats loess loess.control lm predict
#' @export
Area <- function(x,
                    type = "Area",
                    colors = ChartColors(max(1, ncol(x), na.rm = TRUE)),
                    opacity = NULL,
                    fit.line.colors = colors,
                    fit.type = "None", # can be "Smooth" or anything else
                    fit.ignore.last = FALSE,
                    fit.line.type = "dot",
                    fit.line.width = 1,
                    fit.line.name = "Fitted",
                    fit.line.opacity = 1,
                    fit.CI.show = FALSE,
                    fit.CI.opacity = 0.4,
                    fit.CI.colors = fit.line.colors,
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
                    legend.show = TRUE,
                    legend.fill.color = background.fill.color,
                    legend.fill.opacity = 0,
                    legend.border.color = rgb(44, 44, 44, maxColorValue = 255),
                    legend.border.line.width = 0,
                    legend.font.color = global.font.color,
                    legend.font.family = global.font.family,
                    legend.font.size = 10,
                    legend.position.x = 1.02,
                    legend.position.y = 1.0,
                    legend.ascending = NA,
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
                    y.zero = TRUE,
                    y.zero.line.width = 0,
                    y.zero.line.color = rgb(225, 225, 225, maxColorValue = 255),
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
                    x.zero = FALSE,
                    x.zero.line.width = 0,
                    x.zero.line.color = rgb(225, 225, 225, maxColorValue = 255),
                    x.data.reversed = FALSE,
                    x.grid.width = 0 * grid.show,
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
                    x.tick.label.wrap = TRUE,
                    x.tick.label.wrap.nchar = 21,
                    line.thickness = NULL,
                    line.colors = colors,
                    line.opacity = 1,
                    marker.show = NULL,
                    marker.colors = colors,
                    marker.opacity = 1,
                    marker.size = 6,
                    marker.border.width = 1,
                    marker.border.colors = colors,
                    marker.border.opacity = 1,
                    tooltip.show = TRUE,
                    modebar.show = FALSE,
                    data.label.show = FALSE,
                    data.label.font.family = global.font.family,
                    data.label.font.size = 10,
                    data.label.font.color = global.font.color,
                    data.label.format = "",
                    data.label.prefix = "",
                    data.label.suffix = "")
{
    # Data checking
    ErrorIfNotEnoughData(x)
    chart.matrix <- checkMatrixNames(x)
    is.stacked <- grepl("Stacked", type, fixed = TRUE)
    is.hundred.percent.stacked <- grepl("100% Stacked", type, fixed = TRUE)
    if (is.stacked && ncol(chart.matrix) < 2)
        stop(paste(type, "requires more than one series. Use Area charts instead for this data."))
    if (is.stacked && (any(is.na(chart.matrix)) || any(chart.matrix < 0)))
        stop("Stacked charts cannot be produced with missing or negative values.")
    if (is.hundred.percent.stacked && any(rowSums(chart.matrix) == 0))
        stop("100% stacked charts cannot be produced with rows that do not contain positive values.")
    if (any(is.na(as.matrix(chart.matrix))))
        warning("Missing values have been interpolated or omitted.")

    num.notNA <- colSums(!is.na(chart.matrix))
    if (all(num.notNA < 2))
        stop("Area is not defined for a series with only one data point.")

    # Some minimal data cleaning
    # Assume formatting and Qtable/attribute handling already done
    # Find gaps which are NOT at the ends of the series
    has.gap <- FALSE
    for (i in 1:ncol(chart.matrix))
    {
        na.seq <- rle(!is.finite(chart.matrix[,i]))
        n <- length(na.seq$values)
        if (any(na.seq$values[-c(1,n)]))
            has.gap <- TRUE
    }
    if (is.null(line.thickness))
        line.thickness <- if (!has.gap || is.stacked) 0 else 3
    if (is.hundred.percent.stacked)
        chart.matrix <- cum.data(chart.matrix, "cumulative.percentage")
    else if (is.stacked)
        chart.matrix <- cum.data(chart.matrix, "cumulative.sum")

    data.label.mult <- 1
    if (percentFromD3(data.label.format)) {
        data.label.suffix <- paste0("%", data.label.suffix)
        data.label.mult <- 100
    }
    data.label.decimals <- decimalsFromD3(data.label.format)

    matrix.labels <- names(dimnames(chart.matrix))
    if (nchar(x.title) == 0 && length(matrix.labels) == 2)
        x.title <- matrix.labels[1]
    x.labels.full <- rownames(chart.matrix)

    # Constants
    hover.mode <- if (tooltip.show) "closest" else FALSE
    barmode <- if (is.stacked) "stack" else ""
    fill.bound <- if (is.stacked) "tonexty" else "tozeroy"

    marker.symbols <- if (is.null(marker.show)) rep(100, ncol(chart.matrix))
                             else marker.show
    if (is.null(line.thickness))
        line.thickness <- if (!has.gap || is.stacked) 0 else 3

    series.mode <- "lines+markers"
    if (is.null(marker.show))
        series.mode <- "lines"
    else if (line.thickness == 0 && marker.show != "none")
        series.mode <- "markers"
    else if (line.thickness >= 1 && marker.show == "none")
        series.mode <- "lines"
    else if (line.thickness == 0 && marker.show == "none")
        series.mode <- "lines"

    eval(colors) # not sure why, but this is necessary for bars to appear properly
    if (is.null(opacity))
        opacity <- if (!is.stacked || fit.type != "None") 0.4 else 1
    if (opacity == 1 && !is.stacked && ncol(chart.matrix) > 1)
        warning("Displaying this chart with opacity set to 1 will make it difficult to read as some data series may be obscured.")

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
                        legend.border.color, legend.border.line.width,
                        legend.position.x, legend.position.y, y.data.reversed)
    footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate = FALSE)

    # Format axis labels
    y.range <- setValRange(y.bounds.minimum, y.bounds.maximum, chart.matrix, is.null(y.tick.distance))
    ytick <- setTicks(y.range$min, y.range$max, y.tick.distance, y.data.reversed)
    xtick <- setTicks(x.bounds.minimum, x.bounds.maximum, x.tick.distance, x.data.reversed)
    axisFormat <- formatLabels(chart.matrix, type, x.tick.label.wrap, x.tick.label.wrap.nchar,
                               x.tick.format, y.tick.format)

    yaxis <- setAxis(y.title, "left", axisFormat, y.title.font,
                  y.line.color, y.line.width, y.grid.width * grid.show, y.grid.color,
                  ytick, ytick.font, y.tick.angle, y.tick.mark.length, y.tick.distance,
                  y.tick.format, y.tick.prefix, y.tick.suffix,
                  y.tick.show, y.zero, y.zero.line.width, y.zero.line.color,
                  y.hovertext.format)
    xaxis <- setAxis(x.title, "bottom", axisFormat, x.title.font,
                  x.line.color, x.line.width, x.grid.width * grid.show, x.grid.color,
                  xtick, xtick.font, x.tick.angle, x.tick.mark.length, x.tick.distance,
                  x.tick.format, x.tick.prefix, x.tick.suffix, x.tick.show, x.zero, x.zero.line.width, x.zero.line.color,
                  x.hovertext.format, axisFormat$labels)

    # Work out margin spacing
    margins <- list(t = 20, b = 20, r = 60, l = 80, pad = 0)
    margins <- setMarginsForAxis(margins, axisFormat, xaxis)
    margins <- setMarginsForText(margins, title, subtitle, footer, title.font.size,
                                 subtitle.font.size, footer.font.size)
    margins <- setMarginsForLegend(margins, legend.show, legend, colnames(chart.matrix))
    margins <- setCustomMargins(margins, margin.top, margin.bottom, margin.left,
                    margin.right, margin.inner.pad)

    ## Initiate plotly object
    p <- plot_ly(as.data.frame(chart.matrix))
    if (is.null(rownames(chart.matrix)))
        rownames(chart.matrix) <- 1:nrow(chart.matrix)
    x.labels <- axisFormat$labels
    y.labels <- colnames(chart.matrix)

    # Invisible trace to ensure enough space for data labels
    # and that tick bounds are shown properly
    # This must happen before ANY of the area traces are put in
    if (data.label.show || notAutoRange(yaxis)) 
        p <- add_trace(p, type = "scatter", mode = "markers",
           x = x.labels, y = apply(chart.matrix, 1, max, na.rm = TRUE) * 1.01,
           marker = list(color = "red", opacity = 0.0),
           hoverinfo = "none", showlegend = FALSE, cliponaxis = FALSE)

    ## Add a trace for each col of data in the matrix
    for (i in 1:ncol(chart.matrix))
    {
        y <- as.numeric(chart.matrix[, i])
        x <- x.labels

        lines <- list(width = line.thickness,
                      color = toRGB(line.colors[i], alpha = line.opacity))

        marker <- NULL
        if (!is.null(series.mode) && regexpr('marker', series.mode) >= 1)
            marker <- list(size = marker.size,
                       color = toRGB(marker.colors[i], alpha = marker.opacity),
                       symbol = marker.symbols[i],
                       line = list(color = toRGB(marker.border.colors[i],
                       alpha = marker.border.opacity),
                       width = marker.border.width))

        source.text <- ""
        if (data.label.show)
        {
            source.text <- paste(data.label.prefix,
                 FormatAsReal(chart.matrix[, i] * data.label.mult, decimals = data.label.decimals),
                 data.label.suffix, sep = "")
            y.sign <- getSign(chart.matrix[,i], yaxis)
            if (is.stacked)
                y.sign <- -y.sign
            data.label.position <- ifelse(y.sign >= 0, "top middle", "bottom middle")
            data.label.position[1] <- gsub("middle", "right", data.label.position[1])
            tmp.len <- length(data.label.position)
            data.label.position[tmp.len] <- gsub("middle", "left", data.label.position[tmp.len])
        }

        # add invisible line to force all categorical labels to be shown
        if (i == 1)
            p <- add_trace(p, x = x, y = rep(min(y,na.rm=T), length(x)),
                           type = "scatter", mode = "lines",
                           hoverinfo = "none", showlegend = F, opacity = 0)

        if (!is.stacked)
        {
            y.label <- y.labels[i]

            # Need to add data labels first otherwise it will override hovertext in area chart
            if (data.label.show)
                p <- add_trace(p,
                           type = "scatter",
                           mode = "text",
                           x = x,
                           y = y,
                           legendgroup = i,
                           name = y.label,
                           text = source.text,
                           textfont = data.label.font,
                           textposition = data.label.position,
                           cliponaxis = FALSE, 
                           hoverinfo = "none",
                           showlegend = FALSE)

           # draw line
           if (any(!is.na(y)) && (has.gap || line.thickness > 0))
                p <- add_trace(p,
                           type = "scatter",
                           x = x,
                           y = y,
                           connectgaps = FALSE,
                           line = lines,
                           name = y.label,
                           showlegend = FALSE,
                           legendgroup = i,
                           hoverinfo = "skip",
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
                           legendgroup = i,
                           name = y.label,
                           marker = if (!is.null(marker)) marker
                                    else list(color = toRGB(colors[i]),
                                         size = marker.size),
                           hoverinfo = "skip", #if(ncol(chart.matrix) > 1) "x+y+name" else "x+y",
                           showlegend = FALSE)
            }

            # Area chart (with no line)
            # We need to do this separately because connectgaps = FALSE
            # has strange behaviour with single points
            # This is done last, to retain the hovertext
            if (any(!is.na(y)))
                p <- add_trace(p,
                           type = "scatter",
                           x = x,
                           y = y,
                           fill = fill.bound,
                           fillcolor = toRGB(colors[i], alpha = opacity),
                           connectgaps = TRUE,
                           line = list(width = 0),
                           name = y.label,
                           legendgroup = i,
                           hoverlabel = list(bgcolor=colors[i]),
                           text = autoFormatLongLabels(x.labels.full, wordwrap=T, truncate=F),
                           hoverinfo = setHoverText(xaxis, chart.matrix),
                           marker = marker,
                           mode = series.mode)


            if (fit.type != "None")
            {
                tmp.fname <- if (ncol(chart.matrix) == 1)  fit.line.name
                         else sprintf("%s: %s", fit.line.name, y.labels[i])
                tmp.fit <- fitSeries(x, y, fit.type, fit.ignore.last, xaxis$type, fit.CI.show)
                p <- add_trace(p, x = tmp.fit$x, y = tmp.fit$y, type = 'scatter', mode = "lines",
                          name = tmp.fname, legendgroup = i, showlegend = FALSE,
                          line = list(dash = fit.line.type, width = fit.line.width,
                          color = fit.line.colors[i], shape = 'spline'), opacity = fit.line.opacity)    
                if (fit.CI.show && !is.null(tmp.fit$lb))
                {
                    p <- add_trace(p, x = tmp.fit$x, y = tmp.fit$lb, type = 'scatter',
                            mode = 'lines', name = "Lower bound of 95%CI",
                            showlegend = FALSE, legendgroup = i,
                            line=list(color=fit.CI.colors[i], width=0, shape='spline'))
                    p <- add_trace(p, x = tmp.fit$x, y = tmp.fit$ub, type = 'scatter',
                            mode = 'lines', name = "Upper bound of 95% CI",
                            fill = "tonexty", fillcolor = toRGB(fit.CI.colors[i], alpha = fit.CI.opacity),
                            showlegend = FALSE, legendgroup = i,
                            line = list(color=fit.CI.colors[i], width=0, shape='spline'))
                }
            }
        }
        else
        {
            if (fit.type != "None" && is.stacked && i == 1)
                warning("Line of best fit not shown for stacked charts.")
            fill.bound <- if (is.stacked && i > 1) "tonexty" else "tozeroy"

            # plotly has bug where for stacked area charts,
            # text and line must occur together as a single trace
            y.label <- y.labels[i]
            p <- add_trace(p,
                           type = "scatter",
                           x = x,
                           y = y,
                           fill = fill.bound,
                           fillcolor = toRGB(colors[i], alpha = opacity),
                           line = lines,
                           name = y.label,
                           legendgroup = i,
                           text = if (!data.label.show) NULL else source.text,
                           textfont = if (!data.label.show) NULL else data.label.font,
                           textposition = if (!data.label.show) NULL else data.label.position,
                           hoverinfo = if (ncol(chart.matrix) > 1) "x+y+name" else "x+y",
                           mode = if (data.label.show) "lines+text" else "lines",
                           marker = marker)
         }
    }
    p <- config(p, displayModeBar = modebar.show)
    p$sizingPolicy$browser$padding <- 0
    p <- layout(p,
        title = title,
        showlegend = legend.show,
        legend = legend,
        yaxis = yaxis,
        xaxis = xaxis,
        margin = margins,
        plot_bgcolor = toRGB(charting.area.fill.color, alpha = charting.area.fill.opacity),
        paper_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity),
        annotations = list(setSubtitle(subtitle, subtitle.font, margins),
                           setFooter(footer, footer.font, margins)),
        hovermode = hover.mode,
        titlefont = title.font,
        font = data.label.font
    )
    result <- list(htmlwidget = p)
    class(result) <- "StandardChart"
    result
}

