#' Line
#'
#' Line chart
#' @inherit Column
#' @inherit Area
#' @param shape Either "linear" for straight lines between data points or "spline" for curved lines.
#' @param smoothing Numeric; smoothing if \code{shape} is "spline".
#' @param line.type Character; one of 'solid', 'dot', 'dashed'.
#' @param marker.symbols Character; marker symbols, which are only shown if marker.show = TRUE.
#'     if a vector is passed, then each element will be applied to a data series.
#' @param data.label.position Character; one of 'top' or 'bottom'. This can
#'    be a single value or a vector with one value for each series.
#' @param data.label.show.at.ends Logical; show data labels at the beginning and end of each
#'      data series. This value will override \code{data.label.show}.
#' @param marker.show.at.ends Logical; show markers at the begining and end of each
#'      data series. The value will override \code{marker.show}.
#' @importFrom grDevices rgb
#' @importFrom flipChartBasics ChartColors
#' @importFrom plotly plot_ly config toRGB add_trace add_text layout hide_colorbar
#' @importFrom stats loess loess.control lm predict
#' @importFrom flipFormat FormatAsPercent
#' @examples
#' z <- structure(c(1L, 2L, 3L, 4L, 5L, 2L, 3L, 4L, 5L, 6L),  .Dim = c(5L, 2L),
#'       .Dimnames = list(c("T", "U", "V", "W", "X"), c("A", "B")))
#' Line(z)
#' @export
Line <-   function(x,
                    type = "Line",
                    line.type = "Solid",
                    shape = c("linear", "spline")[1],
                    smoothing = 1,
                    colors = ChartColors(max(1, ncol(x), na.rm = TRUE)),
                    average.series = NULL,
                    average.color = rgb(230, 230, 230, maxColorValue = 255),
                    annotation.list = NULL,
                    opacity = NULL,
                    fit.type = "None", # can be "Smooth" or anything else
                    fit.window.size = 2,
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
                    title.align = "center",
                    subtitle = "",
                    subtitle.font.family = global.font.family,
                    subtitle.font.color = global.font.color,
                    subtitle.font.size = 12,
                    subtitle.align = "center",
                    footer = "",
                    footer.font.family = global.font.family,
                    footer.font.color = global.font.color,
                    footer.font.size = 8,
                    footer.align = "center",
                    footer.wrap = TRUE,
                    footer.wrap.nchar = 100,
                    grid.show = TRUE,
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
                    hovertext.template = NULL,
                    hovertext.font.family = global.font.family,
                    hovertext.font.size = 11,
                    hovertext.align = "left",
                    y.title = "",
                    y.title.font.color = global.font.color,
                    y.title.font.family = global.font.family,
                    y.title.font.size = 12,
                    y.line.width = 0,
                    y.line.color = rgb(0, 0, 0, maxColorValue = 255),
                    y.tick.mark.length = 0,
                    y.tick.mark.color = "transparent",
                    y.bounds.minimum = NULL,
                    y.bounds.maximum = NULL,
                    y.tick.distance = NULL,
                    y.tick.maxnum = NULL,
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
                    x.tick.mark.length = 3,
                    x.tick.mark.color = "transparent",
                    x.bounds.minimum = NULL,
                    x.bounds.maximum = NULL,
                    x.tick.distance = NULL,
                    x.tick.maxnum = 11,
                    x.zero = FALSE,
                    x.zero.line.width = 0,
                    x.zero.line.color = rgb(225, 225, 225, maxColorValue = 255),
                    x.data.reversed = FALSE,
                    x.grid.width = 0 * grid.show,
                    x.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                    x.tick.show = TRUE,
                    x.tick.format = "",
                    x.tick.prefix = "",
                    x.tick.suffix = "",
                    x.hovertext.format = x.tick.format,
                    x.tick.angle = NULL,
                    x.tick.font.color = global.font.color,
                    x.tick.font.family = global.font.family,
                    x.tick.font.size = 10,
                    x.tick.label.wrap = TRUE,
                    x.tick.label.wrap.nchar = 21,
                    line.thickness = 3,
                    marker.show = NULL,
                    marker.show.at.ends = FALSE,
                    marker.symbols = "circle",
                    marker.colors = colors,
                    marker.opacity = NULL,
                    marker.size = 6,
                    marker.border.width = 1,
                    marker.border.colors = colors,
                    marker.border.opacity = NULL,
                    tooltip.show = TRUE,
                    modebar.show = FALSE,
                    zoom.enable = TRUE,
                    data.label.show = FALSE,
                    data.label.show.at.ends = FALSE,
                    data.label.position = "Top",
                    data.label.font.family = global.font.family,
                    data.label.font.color = global.font.color,
                    data.label.font.autocolor = FALSE,
                    data.label.font.size = 10,
                    data.label.format = "",
                    data.label.prefix = "",
                    data.label.suffix = "")
{
    ErrorIfNotEnoughData(x)
    if (isPercentData(x))
    {
        if (isAutoFormat(y.tick.format))
            y.tick.format <- paste0(y.tick.format, "%")
        if (isAutoFormat(y.hovertext.format))
            y.hovertext.format <- paste0(y.hovertext.format, "%")
        if (isAutoFormat(data.label.format))
            data.label.format <- paste0(data.label.format, "%")

        sfx <- checkSuffixForExtraPercent(c(y.tick.suffix, data.label.suffix),
            c(y.tick.format, data.label.format))
        y.tick.suffix <- sfx[1]
        data.label.suffix <- sfx[2]
    }


    # Store data for chart annotations
    annot.data <- x
    chart.matrix <- checkMatrixNames(x)

    if (is.null(line.thickness))
        line.thickness <- 3
    matrix.labels <- names(dimnames(chart.matrix))
    if (nchar(x.title) == 0 && length(matrix.labels) == 2)
        x.title <- matrix.labels[1]
    x.labels.full <- rownames(chart.matrix)
    if (any(is.na(chart.matrix)))
        warning("Missing values have been omitted.")

    # Constants
    if (grepl("^curved", tolower(shape)))
        shape <- "spline"
    if (grepl("^straight", tolower(shape)))
        shape <- "linear"
    if (is.null(marker.show) || marker.show == "none") # included for backwards compatibility
        marker.show <- FALSE
    if (is.null(opacity))
        opacity <- if (fit.type == "None") 1 else 0.6
    if (is.null(marker.opacity))
        marker.opacity <- opacity
    if (is.null(marker.border.opacity))
        marker.border.opacity <- marker.opacity

    # Set colors
    n <- ncol(chart.matrix)
    colors <- vectorize(colors, n)
    if (fit.type != "None" && is.null(fit.line.colors))
        fit.line.colors <- colors
    if (fit.CI.show && is.null(fit.CI.colors))
        fit.CI.colors <- fit.line.colors
    if (is.null(marker.colors))
        marker.colors <- colors
    if (is.null(marker.border.colors))
        marker.border.colors <- marker.colors
    marker.colors <- vectorize(marker.colors, n)
    marker.border.colors <- vectorize(marker.border.colors, n)

    if (data.label.show.at.ends || marker.show.at.ends)
    {
        ends.show <- matrix(FALSE, nrow(chart.matrix), ncol(chart.matrix))
        for (i in 1:ncol(chart.matrix))
        {
            ind <- which(is.finite(chart.matrix[,i])) # ignore NAs
            if (length(ind) > 0)
            {
                ends.show[min(ind),i] <- TRUE
                ends.show[max(ind),i] <- TRUE
            }
        }
    }
    data.label.show <- if (data.label.show.at.ends) ends.show
                       else vectorize(data.label.show, ncol(chart.matrix), nrow(chart.matrix))
    marker.show <- if (marker.show.at.ends) ends.show
                   else  vectorize(marker.show, ncol(chart.matrix), nrow(chart.matrix))

    line.type <- vectorize(tolower(line.type), ncol(chart.matrix))
    marker.symbols <- vectorize(marker.symbols, ncol(chart.matrix), nrow(chart.matrix))
    marker.size <- vectorize(marker.size, ncol(chart.matrix), nrow(chart.matrix))
    dlab.color <- if (data.label.font.autocolor) colors
                  else vectorize(data.label.font.color, ncol(chart.matrix))
    dlab.pos <- vectorize(tolower(data.label.position), ncol(chart.matrix))
    dlab.prefix <- vectorize(data.label.prefix, ncol(chart.matrix), nrow(chart.matrix), split = NULL)
    dlab.suffix <- vectorize(data.label.suffix, ncol(chart.matrix), nrow(chart.matrix), split = NULL)
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
    legend <- setLegend("Line", legend.font, legend.ascending, legend.fill.color, legend.fill.opacity,
                        legend.border.color, legend.border.line.width,
                        legend.position.x, legend.position.y, FALSE, legend.orientation)
    footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate=FALSE)

    if (!is.null(average.series))
    {
        chart.matrix <- cbind(chart.matrix, average.series)
        colnames(chart.matrix)[ncol(chart.matrix)] <- "Average"
        colors <- c(colors, average.color)
        fit.line.colors <- c(fit.line.colors, average.color)
        fit.CI.colors <- c(fit.CI.colors, average.color)
        line.type <- line.type[c(1:n,1)]
        marker.show <- cbind(marker.show, FALSE)
        marker.size <- marker.size[,c(1:n,1)] # doesn't matter - marker is not shown
        marker.symbols <- marker.symbols[,c(1:n,1)]
    }

    # Format axis labels
    axisFormat <- formatLabels(chart.matrix, "Line", x.tick.label.wrap, x.tick.label.wrap.nchar,
                               x.tick.format, y.tick.format)
    x.range <- setValRange(x.bounds.minimum, x.bounds.maximum, axisFormat, x.zero, is.null(x.tick.distance))
    y.range <- setValRange(y.bounds.minimum, y.bounds.maximum, chart.matrix, y.zero, is.null(y.tick.distance))
    xtick <- setTicks(x.range$min, x.range$max, x.tick.distance, x.data.reversed)
    ytick <- setTicks(y.range$min, y.range$max, y.tick.distance, y.data.reversed)

    yaxis <- setAxis(y.title, "left", axisFormat, y.title.font,
                  y.line.color, y.line.width, y.grid.width * grid.show, y.grid.color,
                  ytick, ytick.font, y.tick.angle, y.tick.mark.length, y.tick.distance,
                  y.tick.format, y.tick.prefix, y.tick.suffix,
                  y.tick.show, y.zero, y.zero.line.width, y.zero.line.color,
                  y.hovertext.format, num.maxticks = y.tick.maxnum, 
                  tickcolor = y.tick.mark.color, zoom.enable = zoom.enable)
    xaxis <- setAxis(x.title, "bottom", axisFormat, x.title.font,
                  x.line.color, x.line.width, x.grid.width * grid.show, x.grid.color,
                  xtick, xtick.font, x.tick.angle, x.tick.mark.length, x.tick.distance,
                  x.tick.format, x.tick.prefix, x.tick.suffix, x.tick.show,
                  x.zero, x.zero.line.width, x.zero.line.color,
                  x.hovertext.format, axisFormat$labels, num.maxticks = x.tick.maxnum,
                  tickcolor = x.tick.mark.color, zoom.enable = zoom.enable)

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

    ## Add a trace for each col of data in the matrix
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
    }

    # Add invisible line to force all categorical labels to be shown
    tmp.min <- if (any(is.finite(chart.matrix))) min(chart.matrix[is.finite(chart.matrix)])
               else y.bounds.minimum
    p <- add_trace(p, x = x.labels, y = rep(tmp.min, length(x.labels)),
                   type = "scatter", mode = "lines",
                   hoverinfo = "skip", showlegend = FALSE, opacity = 0)

    line.thickness <- readLineThickness(line.thickness, ncol(chart.matrix))
    opacity <- opacity * rep(1, ncol(chart.matrix))
    for (i in 1:ncol(chart.matrix))
    {
        y <- as.numeric(chart.matrix[, i])
        x <- x.labels

        lines <- list(width = line.thickness[i], dash = line.type[i],
                      shape = shape, smoothing = smoothing,
                      color = toRGB(colors[i], alpha = opacity[i]))


        marker <- NULL
        series.mode <- "lines"
        if (any(marker.show[,i]) && any(is.finite(chart.matrix[,i])))
        {
            series.mode <- "lines+markers"
            sz.ind0 <- which(is.finite(chart.matrix[,i]))
            sz.ind <- min(sz.ind0):max(sz.ind0) # plotly ignores NAs at ends but not in the middle
            size.i <- rep(0, length(sz.ind))
            size.i[which(marker.show[sz.ind,i])] <-
                marker.size[intersect(which(marker.show[,i]), sz.ind),i]

            marker <- list(size = size.i,
                       color = toRGB(marker.colors[i], alpha = marker.opacity),
                       symbol = marker.symbols[i], opacity = 1.0,
                       line = list(
                       color = toRGB(marker.border.colors[i], alpha = marker.border.opacity),
                       width = marker.border.width))
        }
        y.label <- y.labels[i]
        hover.template <- setHoverTemplate(i, xaxis, chart.matrix, hovertext.template)

        # Draw line - main trace
        if (any(!is.na(y)))
            p <- add_trace(p, x = x, y = y, type = "scatter", mode = series.mode,
                   connectgaps = FALSE, line = lines, marker = marker, name = legend.text[i],
                   showlegend = (type == "Line"), legendgroup = i, cliponaxis = FALSE,
                   text = autoFormatLongLabels(x.labels.full, wordwrap=T, truncate=F),
                   hoverlabel = list(font = list(color = autoFontColor(colors[i]),
                   size = hovertext.font.size, family = hovertext.font.family)),
                   hovertemplate = hover.template)

        # single points (no lines) need to be added separately
        not.na <- is.finite(y)
        ind.single <- which(not.na & c(TRUE, !not.na[-nrow(chart.matrix)]) & c(!not.na[-1], TRUE))
        if (length(ind.single) > 0 && type == "Line")
        {
            p <- add_trace(p,
                       type = "scatter",
                       mode = "markers",
                       x = x[ind.single],
                       y = y[ind.single],
                       legendgroup = i,
                       name = y.label,
                       marker = list(color = toRGB(colors[i], alpha = marker.opacity), size = marker.size[1]),
                       text = autoFormatLongLabels(x.labels.full[ind.single], wordwrap=T, truncate=F),
                       hoverlabel = list(font = list(color = autoFontColor(colors[i]),
                       size = hovertext.font.size, family = hovertext.font.family)),
                       hovertemplate = hover.template[ind.single], 
                       showlegend = FALSE)
        }
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
                        mode = 'lines', name = "Upper bound of 95% CI",
                        fill = "tonexty", fillcolor = toRGB(fit.CI.colors[i], alpha = fit.CI.opacity),
                        showlegend = FALSE, legendgroup = i,
                        hoverlabel = list(font = list(color = autoFontColor(fit.CI.colors[i]),
                        size = hovertext.font.size, family = hovertext.font.family)),
                        line = list(color=fit.CI.colors[i], width=0, shape='spline'))
            }
        }
    }

    # Add data labels last to ensure they show on top of the lines
    # This also overrides the hoverlabels so we need to re-create them
    # We use a text trace instead of annotations because it will toggle with the legend
    chart.labels <- list(SeriesLabels = list())
    for (i in 1:n) # does not include average.series
    {
        ind.show <- which(data.label.show[,i] & is.finite(chart.matrix[,i]))
        y <- as.numeric(chart.matrix[ind.show, i])
        x <- x.labels[ind.show]
        source.text <- formatByD3(chart.matrix[,i], data.label.format, 
            dlab.prefix[,i], dlab.suffix[,i], decimals = 0)

        # Add attribute for PPT exporting
        chart.labels$SeriesLabels[[i]] <- list(Position = "Top",
            Font = setFontForPPT(data.label.font[[i]]), ShowValue = length(ind.show) > 0)

        if (length(ind.show > 0))
        {
            # Initialise custom points if annotations are used
            pt.segs <- NULL
            if (!is.null(annotation.list) || length(ind.show) < nrow(chart.matrix) ||
                any(nzchar(dlab.prefix[,i])) || any(nzchar(dlab.suffix[,i])))
            {
                chart.labels$SeriesLabels[[i]]$ShowValue <- FALSE
                pt.segs <- lapply((1:nrow(chart.matrix)),
                    function(ii) return(list(Index = ii-1, Segments = c(
                        if (nzchar(dlab.prefix[ii,i])) list(list(Text = dlab.prefix[ii,i])) else NULL,
                        list(list(Field="Value")),
                        if (nzchar(dlab.suffix[ii,i])) list(list(Text = dlab.suffix[ii,i])) else NULL))))
                for (ii in setdiff(1:nrow(chart.matrix), ind.show))
                    pt.segs[[ii]]$Segments <- NULL
            }
        
            # Apply annotations
            attr(source.text, "customPoints") <- pt.segs
            source.text <- applyAllAnnotationsToDataLabels(source.text, annotation.list,
                annot.data, i, ind.show, "Line", clean.pt.segs = TRUE)
            pt.segs <- attr(source.text, "customPoints")
            if (isTRUE(attr(pt.segs, "SeriesShowValue")))
            {
                chart.labels$SeriesLabels[[i]]$ShowValue <- TRUE
                attr(pt.segs, "SeriesShowValue") <- NULL
            }
            if (length(pt.segs) > 0)
                chart.labels$SeriesLabels[[i]]$CustomPoints <- pt.segs

            data.label.offset <- rep(line.thickness[i]/2, length(ind.show))
            if (any(marker.show[,i]))
                data.label.offset[which(marker.show[ind.show,i])] <- pmax(marker.size[ind.show,i], data.label.offset)
            p <- add_trace(p, x = x, y = y, type = "scatter", name = y.label,
                   cliponaxis = FALSE, mode = "markers+text",
                   marker = list(size = data.label.offset, color=colors[i], opacity = 0),
                   text = source.text[ind.show],
                   textfont = data.label.font[[i]], textposition = dlab.pos[i],
                   showlegend = FALSE, legendgroup = i, hoverinfo = "skip")
            
            # Add hover as a separate trace to avoid conflict between data labels and hover    
            hover.template <- rep(setHoverTemplate(i, xaxis, chart.matrix, hovertext.template), 
                length = nrow(chart.matrix))
            p <- add_trace(p, x = x, y = y, type = "scatter", name = y.label,
                   cliponaxis = FALSE, mode = "markers",
                   marker = list(size = data.label.offset, color=colors[i], opacity = 0),
                   showlegend = FALSE, legendgroup = i,
                   hoverlabel = list(font = list(color = autoFontColor(colors[i]),
                   size = hovertext.font.size, family = hovertext.font.family),
                   bgcolor = toRGB(colors[i], alpha = opacity)),
                   hovertemplate = hover.template[ind.show])
        }
    }
    serieslabels.num.changes <- vapply(chart.labels$SeriesLabels, function(s) isTRUE(s$ShowValue) + length(s$CustomPoints), numeric(1L))
    if (sum(serieslabels.num.changes) == 0)
        chart.labels <- NULL

    annot <- list(setSubtitle(subtitle, subtitle.font, margins, subtitle.align),
                           setTitle(title, title.font, margins, title.align),
                           setFooter(footer, footer.font, margins, footer.align))
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
        hovermode = if (tooltip.show) "closest" else FALSE,
        hoverlabel = list(namelength = -1, bordercolor = "transparent", align = hovertext.align,
            font = list(size = hovertext.font.size, family = hovertext.font.family))
    )
    attr(p, "can-run-in-root-dom") <- TRUE
    result <- list(htmlwidget = p)
    class(result) <- "StandardChart"
    attr(result, "ChartType") <- if (all(marker.show)) "Line Markers" else "Line"
    attr(result, "ChartLabels") <- chart.labels
    result
}

