#' Area
#'
#' Area chart
#' @param marker.show Can be "none", "automatic" or a vector referencing
#' the plotly symbol dictionary using either numerics or strings.
#' @param marker.colors Character; a vector containing on/mae or more colors specified as hex codes.
#' @param marker.opacity Opacity for markers as an alpha value (0 to 1).
#' @param marker.size Size in pixels of marker
#' @param marker.border.width Width in pixels of border/line around markers; 0 is no line
#' @param marker.border.colors Character; a vector containing one or more colors specified as hex codes.
#' @param marker.border.opacity Opacity of border/line around
#' markers as an alpha value (0 to 1).
#' @param line.thickness Thickness, in pixels, of the series line
#' @param line.colors  Character; a vector containing one or more colors specified as hex codes.
#' @param line.opacity Opacity for series lines as an alpha value (0 to 1).
#' @inherit Column
#' @importFrom grDevices rgb
#' @importFrom flipChartBasics ChartColors
#' @importFrom plotly plot_ly config toRGB add_trace add_text layout hide_colorbar
#' @importFrom stats loess loess.control lm predict
#' @examples
#' z <- structure(c(1L, 2L, 3L, 4L, 5L, 2L, 3L, 4L, 5L, 6L),  .Dim = c(5L, 2L),
#'       .Dimnames = list(c("T", "U", "V", "W", "X"), c("A", "B")))
#' Area(z)
#' @export
Area <- function(x,
                    type = "Area",
                    colors = ChartColors(max(1, ncol(x), na.rm = TRUE)),
                    opacity = NULL,
                    fit.line.colors = colors,
                    fit.type = "None", # can be "Smooth" or anything else
                    fit.ignore.last = FALSE,
                    fit.window.size = 2,
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
                    background.fill.color = "transparent",
                    background.fill.opacity = 1,
                    charting.area.fill.color = background.fill.color,
                    charting.area.fill.opacity = 0,
                    legend.show = NA,
                    legend.orientation = "Vertical",
                    legend.wrap = TRUE,
                    legend.wrap.nchar = 30,
                    legend.fill.color = background.fill.color,
                    legend.fill.opacity = 0,
                    legend.border.color = rgb(44, 44, 44, maxColorValue = 255),
                    legend.border.line.width = 0,
                    legend.font.color = global.font.color,
                    legend.font.family = global.font.family,
                    legend.font.size = 10,
                    legend.position.x = NULL,
                    legend.position.y = NULL,
                    legend.ascending = NA,
                    margin.autoexpand = TRUE,
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
                    hovertext.font.family = global.font.family,
                    hovertext.font.size = 11,
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
                    data.label.font.autocolor = FALSE,
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
    if (is.stacked && ncol(chart.matrix) < 2)
    {
        warning("No stacking performed for only one series.")
        is.stacked <- FALSE
    }
    if (is.stacked && (any(is.na(chart.matrix)) || any(chart.matrix < 0)))
        stop("Stacked Area charts cannot be produced with missing or negative values. Try using Bar or Column charts with stacking")
    if (any(is.na(as.matrix(chart.matrix))))
        warning("Missing values have been interpolated or omitted.")


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
    if (length(line.thickness) == 0 || line.thickness == "")
        line.thickness <- if (!has.gap || is.stacked) 0 else 3
    if (is.character(line.thickness))
    {
        tmp.txt <- TextAsVector(line.thickness)
        line.thickness <- suppressWarnings(as.numeric(tmp.txt))
        na.ind <- which(is.na(line.thickness))
        if (length(na.ind) == 1)
            warning("Non-numeric line thickness value '", tmp.txt[na.ind], "' was ignored.")
        if (length(na.ind) > 1)
            warning("Non-numeric line thickness values '",
            paste(tmp.txt[na.ind], collapse = "', '"), "' were ignored.")
        if (length(na.ind) > 0)
            line.thickness[na.ind] <- if (!has.gap || is.stacked) 0 else 3
    }
    line.thickness <- suppressWarnings(line.thickness * rep(1, ncol(chart.matrix)))

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
    barmode <- if (is.stacked) "stack" else ""
    fill.bound <- if (is.stacked) "tonexty" else "tozeroy"

    marker.symbols <- if (is.null(marker.show)) rep(100, ncol(chart.matrix))
                             else marker.show

    series.mode <- "lines+markers"
    if (is.null(marker.show))
        series.mode <- "lines"
    else if (all(line.thickness == 0) && marker.show != "none")
        series.mode <- "markers"
    else if (any(line.thickness >= 1) && marker.show == "none")
        series.mode <- "lines"
    else if (line.thickness == 0 && marker.show == "none")
        series.mode <- "lines"
    if (fit.type != "None" && is.null(fit.line.colors))
        fit.line.colors <- colors
    if (fit.CI.show && is.null(fit.CI.colors))
        fit.CI.colors <- fit.line.colors

    eval(colors) # not sure why, but this is necessary for bars to appear properly
    if (is.null(opacity))
        opacity <- if (!is.stacked || fit.type != "None") 0.4 else 1
    if (opacity == 1 && !is.stacked && ncol(chart.matrix) > 1)
        warning("Displaying this chart with opacity set to 1 will make it difficult to read as some data series may be obscured.")

    if (is.stacked && data.label.font.autocolor)
    {
        dlab.color <- autoFontColor(colors)
        if (sum(y.data.reversed, isTRUE(y.bounds.minimum > y.bounds.maximum)) != 1)
            dlab.color <- c(dlab.color[-1], global.font.color) # top datalabels are on the chart background

    } else
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
    data.label.show = vectorize(data.label.show, ncol(chart.matrix), nrow(chart.matrix))
    data.label.prefix <- vectorize(data.label.prefix, ncol(chart.matrix), nrow(chart.matrix), split = NULL)
    data.label.suffix <- vectorize(data.label.suffix, ncol(chart.matrix), nrow(chart.matrix), split = NULL)

    legend.show <- setShowLegend(legend.show, NCOL(chart.matrix))
    legend <- setLegend(type, legend.font, legend.ascending, legend.fill.color, legend.fill.opacity,
                        legend.border.color, legend.border.line.width,
                        legend.position.x, legend.position.y, y.data.reversed, legend.orientation)
    footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate = FALSE)

    # Format axis labels
    axisFormat <- formatLabels(chart.matrix, type, x.tick.label.wrap, x.tick.label.wrap.nchar,
                               x.tick.format, y.tick.format)
    x.range <- setValRange(x.bounds.minimum, x.bounds.maximum, axisFormat, x.zero, is.null(x.tick.distance), margin.autoexpand = margin.autoexpand)
    y.range <- setValRange(y.bounds.minimum, y.bounds.maximum, chart.matrix, y.zero, is.null(y.tick.distance))
    xtick <- setTicks(x.range$min, x.range$max, x.tick.distance, x.data.reversed)
    ytick <- setTicks(y.range$min, y.range$max, y.tick.distance, y.data.reversed)

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

    legend.text <- autoFormatLongLabels(colnames(chart.matrix), legend.wrap, legend.wrap.nchar)
    margins <- setMarginsForLegend(margins, legend.show, legend, legend.text)
    margins <- setCustomMargins(margins, margin.top, margin.bottom, margin.left,
                    margin.right, margin.inner.pad)
    margins$autoexpand <- margin.autoexpand

    ## Initiate plotly object
    p <- plot_ly(as.data.frame(chart.matrix))
    if (is.null(rownames(chart.matrix)))
        rownames(chart.matrix) <- 1:nrow(chart.matrix)
    x.labels <- axisFormat$labels
    y.labels <- colnames(chart.matrix)

    # Setting up info for data labels (may not be used)
    x.sign <- getSign(1.0, xaxis)
    m <- nrow(chart.matrix)
    data.label.pos <- rep("top middle", m)
    data.label.pos[1] <- gsub("middle", if (x.sign > 0) "right" else "left", data.label.pos[1])
    data.label.pos[m] <- gsub("middle", if (x.sign > 0) "left" else "right",  data.label.pos[m])

    # Invisible trace to ensure enough space for data labels
    # and that tick bounds are shown properly
    # This must happen before ANY of the area traces are put in
    if (any(data.label.show) || notAutoRange(yaxis))
        p <- add_trace(p, type = "scatter", mode = "markers",
           x = x.labels, y = apply(chart.matrix, 1, max, na.rm = TRUE) * 1.01,
           marker = list(color = "red", opacity = 0.0),
           hoverinfo = "skip", showlegend = FALSE, cliponaxis = FALSE)

    if (!is.stacked)
    {
        for (i in 1:ncol(chart.matrix))
        {
            y <- as.numeric(chart.matrix[, i])
            x <- x.labels
            y.label <- y.labels[i]

            lines <- list(width = line.thickness[i],
                          color = toRGB(line.colors[i], alpha = line.opacity))

            marker <- NULL
            if (!is.null(series.mode) && regexpr('marker', series.mode) >= 1)
                marker <- list(size = marker.size,
                           color = toRGB(marker.colors[i], alpha = marker.opacity),
                           symbol = marker.symbols[i],
                           line = list(color = toRGB(marker.border.colors[i],
                           alpha = marker.border.opacity),
                           width = marker.border.width))

            # Add invisible line to force all categorical labels to be shown
            if (i == 1)
            {
                tmp.min <- if (any(is.finite(chart.matrix))) min(chart.matrix[is.finite(chart.matrix)])
                           else y.bounds.minimum

                p <- add_trace(p, x = x, y = rep(tmp.min, length(x)),
                               type = "scatter", mode = "lines",
                               hoverinfo = "skip", showlegend = FALSE, opacity = 0)
            }

            # Draw line
            if (any(!is.na(y)) && (has.gap || line.thickness[i] > 0))
                p <- add_trace(p, type = "scatter", x = x, y = y, name = y.label,
                           connectgaps = FALSE, line = lines, marker = marker,
                           showlegend = FALSE, legendgroup = i,
                           hoverinfo = "skip", mode = series.mode)

            # Single points (no lines) need to be added separately
            not.na <- is.finite(y)
            is.single <- not.na & c(TRUE, !not.na[-nrow(chart.matrix)]) & c(!not.na[-1], TRUE)
            if (any(is.single) && type == "Line")
                p <- add_trace(p, type = "scatter", mode = "markers", name = y.label,
                           x = x[is.single], y = y[is.single], legendgroup = i,
                           marker = if (!is.null(marker)) marker
                                    else list(color = toRGB(colors[i]), size = marker.size),
                           hoverinfo = "skip", showlegend = FALSE)

            # Area chart (with no line) - main trace
            # We need to do this separately because connectgaps = FALSE
            # has strange behaviour with single points
            # This is done last, to retain the hovertext
            if (any(!is.na(y)))
                p <- add_trace(p, type = "scatter", x = x, y = y, name = legend.text[i],
                           fill = fill.bound, fillcolor = toRGB(colors[i], alpha = opacity),
                           connectgaps = TRUE, line = list(width = 0, color = colors[i]),
                           hoverlabel = list(font = list(color = autoFontColor(colors[i]),
                           size = hovertext.font.size, family = hovertext.font.family),
                           bgcolor = colors[i]), legendgroup = i,
                           hovertemplate = setHoverTemplate(i, xaxis, chart.matrix),
                           marker = marker, mode = series.mode)

            if (fit.type != "None")
            {
                tmp.fname <- if (ncol(chart.matrix) == 1)  fit.line.name
                         else sprintf("%s: %s", fit.line.name, y.labels[i])
                tmp.fit <- fitSeries(x, y, fit.type, fit.ignore.last, xaxis$type, fit.CI.show, fit.window.size)
                p <- add_trace(p, x = tmp.fit$x, y = tmp.fit$y, type = 'scatter', mode = "lines",
                          name = tmp.fname, legendgroup = i, showlegend = FALSE,
                          hoverlabel = list(font = list(color = autoFontColor(fit.line.colors[i]),
                          size = hovertext.font.size, family = hovertext.font.family)),
                          line = list(dash = fit.line.type, width = fit.line.width,
                          color = fit.line.colors[i], shape = 'spline'), opacity = fit.line.opacity)
                if (fit.CI.show && !is.null(tmp.fit$lb))
                {
                    p <- add_trace(p, x = tmp.fit$x, y = tmp.fit$lb, type = 'scatter',
                            mode = 'lines', name = "Lower bound of 95%CI",
                            showlegend = FALSE, legendgroup = i,
                            hoverlabel = list(font = list(color = autoFontColor(fit.CI.colors[i]),
                            size = hovertext.font.size, family = hovertext.font.family)),
                            line=list(color=fit.CI.colors[i], width=0, shape='spline'))
                    p <- add_trace(p, x = tmp.fit$x, y = tmp.fit$ub, type = 'scatter',
                            mode = 'lines', name = "Upper bound of 95% CI", fill = "tonexty",
                            fillcolor = toRGB(fit.CI.colors[i], alpha = fit.CI.opacity),
                            hoverlabel = list(font = list(color = autoFontColor(fit.CI.colors[i]),
                            size = hovertext.font.size, family = hovertext.font.family)),
                            showlegend = FALSE, legendgroup = i,
                            line = list(color=fit.CI.colors[i], width=0, shape='spline'))
                }
            }
            if (any(data.label.show[,i]))
            {
                ind.show <- which(data.label.show[,i])
                y <- as.numeric(chart.matrix[ind.show, i])
                x <- x.labels[ind.show]
                y.label <- y.labels[i]

                label.text <- paste(data.label.prefix[ind.show,i],
                     FormatAsReal(chart.matrix[ind.show,i] * data.label.mult, decimals = data.label.decimals),
                     data.label.suffix[ind.show,i], sep = "")

                m <- nrow(chart.matrix)
                data.label.pos <- rep("top middle", m)
                data.label.pos[1] <- gsub("middle", if (x.sign > 0) "right"
                                                    else "left", data.label.pos[1])
                data.label.pos[m] <- gsub("middle", if (x.sign > 0) "left"
                                                    else "right",  data.label.pos[m])
                data.label.pos <- data.label.pos[ind.show]

                p <- add_trace(p, type = "scatter", mode = "text", x = x, y = y,
                        legendgroup = i, showlegend = FALSE, name = y.label,
                        text = label.text, textfont = data.label.font[[i]],
                        textposition = data.label.pos, hoverinfo = "skip", cliponaxis = FALSE)
            }
        }
    }
    if (is.stacked)
    {
        for (i in 1:ncol(chart.matrix))
        {
            y <- as.numeric(chart.matrix[, i])
            x <- x.labels
            y.label <- y.labels[i]

            if (fit.type != "None" && is.stacked && i == 1)
                warning("Line of best fit not shown for stacked charts.")
            fill.bound <- if (is.stacked && i > 1) "tonexty" else "tozeroy"

            label.text <- NULL
            if (any(data.label.show))
            {
                # For stacked charts, all data labels must be added
                # but we can hide labels which are not needed
                ind.hide <- which(!data.label.show[,i])
                label.text <- paste(data.label.prefix[,i],
                     FormatAsReal(chart.matrix[,i] * data.label.mult, decimals = data.label.decimals),
                     data.label.suffix[,i], sep = "")
                label.text[ind.hide] <- ""
            }

            y.label <- y.labels[i]
            p <- add_trace(p, type = "scatter", x = x, y = y, name = legend.text[i],
                    fill = fill.bound, fillcolor = toRGB(colors[i], alpha = opacity),
                    line = list(width = line.thickness[i], color = toRGB(line.colors[i], alpha = line.opacity)),
                    stackgroup = "all", legendgroup = i, mode = "lines",
                    hovertemplate = setHoverTemplate(i, xaxis, chart.matrix),
                    hoverlabel = list(bgcolor=colors[i],
                    font = list(color = autoFontColor(colors[i]),
                    size = hovertext.font.size, family = hovertext.font.family)))

            if (any(data.label.show))
                p <- add_trace(p, type = "scatter", mode = "text", x = x, y = y,
                        legendgroup = i, stackgroup = "text", fill = "none",
                        showlegend = FALSE, name = y.label,
                        text = label.text, textfont = data.label.font[[i]],
                        textposition = data.label.pos, hoverinfo = "skip", cliponaxis = FALSE)
         }
    }

    annot <- list(setSubtitle(subtitle, subtitle.font, margins),
                           setTitle(title, title.font, margins),
                           setFooter(footer, footer.font, margins))
    annot <- Filter(Negate(is.null), annot)

    p <- config(p, displayModeBar = modebar.show)
    p$sizingPolicy$browser$padding <- 0
    p <- layout(p,
        showlegend = legend.show,
        legend = legend,
        yaxis = yaxis,
        xaxis = xaxis,
        margin = margins,
        plot_bgcolor = toRGB(charting.area.fill.color, alpha = charting.area.fill.opacity),
        paper_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity),
        annotations = annot,
        font = data.label.font[[1]],
        hovermode = if (tooltip.show) "x" else FALSE,
        hoverlabel = list(namelength = -1, bordercolor = "transparent",
            font = list(size = hovertext.font.size, family = hovertext.font.family))
    )
    #attr(p, "can-run-in-root-dom") <- TRUE
    result <- list(htmlwidget = p)
    class(result) <- "StandardChart"
    attr(result, "ChartType") <- if (is.stacked) "Area Stacked" else "Area"
    result
}

