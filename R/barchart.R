#' Bar
#'
#' Bar Chart
#' @inherit Column
#' @param y.tick.label.wrap Logical; whether to wrap long labels on the y-axis (vertical).
#' @param y.tick.label.wrap.nchar Integer; number of characters in each line when \code{y.tick.label.wrap} is \code{TRUE}.
#' @param x.tick.suffix x-axis tick label suffix
#' @param x.tick.prefix x-axis tick label prefix
#' @param pyramid Logical; show bar chart as a pyramid. Usually called internally
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
                    pyramid = FALSE,
                    annotation.list = NULL,
                    colors = ChartColors(max(1, ncol(x), na.rm = TRUE)),
                    multi.colors.within.series = FALSE,
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
                    y.tick.mark.length = 3,
                    y.tick.mark.color = "transparent",
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
                    x.tick.mark.color = "transparent",
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
                    x.hovertext.format = x.tick.format,
                    x.tick.angle = NULL,
                    x.tick.font.color = global.font.color,
                    x.tick.font.family = global.font.family,
                    x.tick.font.size = 10,
                    y.tick.label.wrap = TRUE,
                    y.tick.label.wrap.nchar = 21,
                    hovertext.font.family = global.font.family,
                    hovertext.font.size = 11,
                    hovertext.template = NULL,
                    hovertext.align = "left",
                    marker.border.width = 1,
                    marker.border.colors = NULL,
                    marker.border.opacity = opacity,
                    tooltip.show = TRUE,
                    modebar.show = FALSE,
                    zoom.enable = TRUE,
                    axis.drag.enable = FALSE,
                    bar.gap = 0.15,
                    bar.group.gap = 0.0,
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
    
    # Store data for chart annotations
    annot.data <- x
    chart.matrix <- checkMatrixNames(x)
    if (!is.numeric(chart.matrix))
        stop("Input data should be numeric.")
    if (multi.colors.within.series && NCOL(chart.matrix) > 1)
    {
        warning("Bar chart with multi color series can only show a single series. To show multiple series use Small Multiples")
        chart.matrix <- chart.matrix[,1, drop = FALSE]
    }
    x.labels.full <- rownames(chart.matrix)

    is.stacked <- grepl("Stacked", type, fixed=T)
    if (is.stacked && ncol(chart.matrix) < 2)
    {
        warning("No stacking performed for only one series.")
        is.stacked <- FALSE
    }
    is.hundred.percent.stacked <- grepl("100% Stacked", type, fixed=T)
    if (any(!is.finite(as.matrix(chart.matrix))))
        warning("Missing values have been set to zero.")
    if (type == "Stacked")
        type <- "Stacked Bar"
    if (type == "100% Stacked")
        type <- "100% Stacked Bar"
    if (!is.stacked)
        type <- "Bar"

    if (bar.gap < 0.0 || bar.gap >= 1.0)
    {
        warning("Parameter 'bar gap' must be between 0 and 1. ",
                "Invalid 'bar gap' set to default value of 0.15.")
        bar.gap <- 0.15
    }
    if (is.stacked || ncol(chart.matrix) < 2)
        bar.group.gap <- 0.0
    if (bar.group.gap < 0.0 || bar.group.gap >= 1.0)
    {
        warning("Parameter 'bar group gap' must be between 0 and 1. ",
                "Invalid 'bar group gap' set to default value of 0.0.")
        bar.group.gap <- 0.0
    }

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
    data.label.prefix <- vectorize(data.label.prefix, ncol(chart.matrix), nrow(chart.matrix), split = NULL)
    data.label.suffix <- vectorize(data.label.suffix, ncol(chart.matrix), nrow(chart.matrix), split = NULL)

    matrix.labels <- names(dimnames(chart.matrix))
    if (nchar(y.title) == 0 && length(matrix.labels) == 2)
        y.title <- matrix.labels[1]

    # Constants
    barmode <- "group"
    if (is.stacked) 
        barmode <- "relative"
    else if (pyramid)
        barmode <- "overlay"
    if (is.null(opacity))
        opacity <- if (fit.type == "None") 1 else 0.6
    if (is.null(marker.border.opacity))
        marker.border.opacity <- opacity
    if (!is.null(marker.border.colors))
        marker.border.colors <- vectorize(marker.border.colors, ncol(chart.matrix))

    colors <- if (multi.colors.within.series) vectorize(colors, nrow(chart.matrix))
              else                            vectorize(colors, ncol(chart.matrix))
    data.label.font.color <- if (multi.colors.within.series) vectorize(data.label.font.color, nrow(chart.matrix))
                             else                            vectorize(data.label.font.color, ncol(chart.matrix)) 
    data.label.show <- vectorize(data.label.show, NCOL(chart.matrix), NROW(chart.matrix))

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
    if (is.null(x.bounds.maximum) || is.na(x.bounds.maximum) || x.bounds.maximum == "")
        x.bounds.maximum <- NULL
    if (pyramid)
    {
        x.bounds.minimum <- if (!is.null(x.bounds.maximum)) -1 * charToNumeric(x.bounds.maximum)
                            else                            NULL
    }
    axisFormat <- formatLabels(chart.matrix, type, y.tick.label.wrap, y.tick.label.wrap.nchar,
                               y.tick.format, x.tick.format)
    x.range <- setValRange(x.bounds.minimum, x.bounds.maximum, chart.matrix, x.zero, is.null(x.tick.distance))
    y.range <- setValRange(y.bounds.minimum, y.bounds.maximum, axisFormat, y.zero, is.null(y.tick.distance), is.bar = TRUE)

    tmp.label <- sprintf(paste0("%s%.", data.label.decimals, "f%s"),
                data.label.prefix, max(chart.matrix), data.label.suffix)
    xtick <- setTicks(x.range$min, x.range$max, x.tick.distance, x.data.reversed, type = type,
                  #data = if (any(data.label.show) && !is.stacked && !pyramid) chart.matrix else NULL, 
                  labels = tmp.label, label.font.size = data.label.font.size)
    ytick <- setTicks(y.range$min, y.range$max, y.tick.distance, !y.data.reversed, is.bar = TRUE)

    yaxis <- setAxis(y.title, "left", axisFormat, y.title.font,
                  y.line.color, y.line.width, y.grid.width * grid.show, y.grid.color,
                  ytick, ytick.font, y.tick.angle, y.tick.mark.length, y.tick.distance,
                  y.tick.format, y.tick.prefix, y.tick.suffix, y.tick.show,
                  y.zero, y.zero.line.width, y.zero.line.color,
                  y.hovertext.format, with.bars = TRUE, num.maxticks = y.tick.maxnum,
                  tickcolor = y.tick.mark.color, zoom.enable = zoom.enable)
    if (yaxis$type == "category")
        yaxis$nticks = NROW(chart.matrix)
    xaxis <- setAxis(x.title, "bottom", axisFormat, x.title.font,
                  x.line.color, x.line.width, x.grid.width * grid.show, x.grid.color,
                  xtick, xtick.font, x.tick.angle, x.tick.mark.length, x.tick.distance,
                  x.tick.format, x.tick.prefix, x.tick.suffix, x.tick.show,
                  x.zero, x.zero.line.width, x.zero.line.color,
                  x.hovertext.format, num.maxticks = x.tick.maxnum, 
                  tickcolor = x.tick.mark.color, zoom.enable = zoom.enable)

    # Work out margin spacing
    margins <- list(t = 20, b = 20, r = 60, l = 80, pad = 0)
    margins <- setMarginsForAxis(margins, axisFormat, yaxis)
    margins <- setMarginsForAxis(margins, as.character(range(x)), xaxis)
    margins <- setMarginsForText(margins, title, subtitle, footer, title.font.size,
                                 subtitle.font.size, footer.font.size)

    legend.text <- autoFormatLongLabels(colnames(chart.matrix), legend.wrap, legend.wrap.nchar)
    if (!legend.show)
        legend.text[1] <- ""
    margins <- setMarginsForLegend(margins, legend.show, legend, legend.text)
    margins <- setCustomMargins(margins, margin.top, margin.bottom, margin.left,
                    margin.right, margin.inner.pad)
    margins$autoexpand <- margin.autoexpand

    # Set up numeric x-axis - this is used for data labels and hovertext
    y.range <- getRange(x, yaxis, axisFormat)
    yaxis2 <- list(overlaying = "y", visible = FALSE, range = y.range,
        rangemode = "match", match = "y", fixedrange = !zoom.enable)
    data.annotations <- dataLabelPositions(chart.matrix = chart.matrix,
                        axis.type = yaxis$type,
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
                        font = NULL)

    ## Initiate plotly object
    p <- plot_ly(as.data.frame(chart.matrix))
    if (is.null(rownames(chart.matrix)))
        rownames(chart.matrix) <- 1:nrow(chart.matrix)
    x.labels <- axisFormat$labels
    y.labels <- colnames(chart.matrix)
    chart.labels <- list(SeriesLabels = list())
    
    ## Add a trace for each col of data in the matrix
    for (i in 1:ncol(chart.matrix))
    {
        x <- x.labels
        y <- as.numeric(chart.matrix[, i])
        y.filled <- ifelse(is.finite(y), y, 0)

        # Evaluate hover template because otherwise scatterplot hover added at end will
        # show incorrect values (note stacking is not an issue)
        hover.template <- setHoverTemplate(i, yaxis, chart.matrix, hovertext.template, is.bar = TRUE)
        hover.template <- evalHoverTemplate(hover.template, y, x.hovertext.format, 
            x.tick.prefix, x.tick.suffix, x, y.hovertext.format, y.tick.prefix, y.tick.suffix)

        tmp.color <- if (multi.colors.within.series) colors else colors[i]
        tmp.border.color <- if (length(marker.border.colors) >= i) marker.border.colors[i] else tmp.color
        dlab.color <- if (multi.colors.within.series) data.label.font.color else data.label.font.color[i]
        if (data.label.font.autocolor && (is.stacked || pyramid))
            dlab.color <- autoFontColor(tmp.color)
        tmp.data.label.font = list(family = data.label.font.family, 
            size = data.label.font.size, color = dlab.color)
        hover.label <- list(bgcolor = tmp.color, font = list(color = autoFontColor(tmp.color),
                            size = hovertext.font.size, family = hovertext.font.family))

        if (any(!is.finite(y)))
        {
            tmp.border.color <- vectorize(tmp.border.color, NROW(chart.matrix))
            tmp.border.color[which(!is.finite(y))] <- "transparent"
        }
        marker <- list(color = toRGB(tmp.color, alpha = opacity),
                  line = list(color = toRGB(tmp.border.color,
                      alpha = marker.border.opacity),
                      width = marker.border.width))

        # add invisible line to force all categorical labels to be shown
        tmp.min <- if (any(is.finite(chart.matrix))) min(chart.matrix[is.finite(chart.matrix)])
                   else x.bounds.minimum
        if (pyramid)
            tmp.min <- 0
        if (!is.stacked && i == 1)
        {
            p <- add_trace(p, x = rep(tmp.min, length(y)), y = x,
                           type = "scatter", mode = "lines",
                           hoverinfo = "skip", showlegend = FALSE, opacity = 0)
        }

        # this is the main trace for each data series
        # need to use y.filled to avoid plotly bug affecting bar-width
        if (pyramid)  
            p <- add_trace(p, x = 2 * y.filled, y = x, base = -y, type = "bar", orientation = "h",
                   marker = marker, hoverlabel = hover.label, hovertemplate = hover.template)
        else
            p <- add_trace(p, x = y.filled, y = x, type = "bar", orientation = "h",
                       marker = marker, name = legend.text[i],
                       hoverlabel = hover.label, hovertemplate = hover.template,
                       legendgroup = if (is.stacked && any(data.label.show)) "all" else i)

        if (fit.type != "None" && is.stacked && i == 1)
            warning("Line of best fit not shown for stacked charts.")
        if (fit.type != "None" && !is.stacked)
        {
            tmp.fit.color <- if (length(fit.line.colors) >= i) fit.line.colors[i] else tmp.color[1] 
            tmp.fit <- fitSeries(x, y, fit.type, fit.ignore.last, yaxis$type, fit.CI.show, fit.window.size)
            tmp.fname <- if (ncol(chart.matrix) == 1)  fit.line.name
                         else sprintf("%s: %s", fit.line.name, y.labels[i])
            p <- add_trace(p, x = tmp.fit$y, y = tmp.fit$x, type = 'scatter', mode = "lines",
                      name = tmp.fname, legendgroup = i, showlegend = FALSE,
                      hoverlabel = list(font = list(color = autoFontColor(tmp.fit.color),
                      size = hovertext.font.size, family = hovertext.font.family)),
                      line = list(dash = fit.line.type, width = fit.line.width,
                      color = tmp.fit.color, shape = 'spline'), opacity = fit.line.opacity)
            if (fit.CI.show && !is.null(tmp.fit$lb))
            {
                tmp.CI.color <- if (length(fit.CI.colors) >= i) fit.CI.colors[i] else tmp.color[1] 
                p <- add_trace(p, y = tmp.fit$x, x = tmp.fit$lb, type = 'scatter',
                        mode = 'lines', name = "Lower bound of 95%CI",
                        hoverlabel = list(font = list(color = autoFontColor(tmp.CI.color),
                        size = hovertext.font.size, family = hovertext.font.family)),
                        showlegend = FALSE, legendgroup = i,
                        line=list(color=tmp.CI.color, width=0, shape='spline'))
                p <- add_trace(p, y = tmp.fit$x, x = tmp.fit$ub, type = 'scatter',
                        mode = 'lines', name = "Upper bound of 95% CI",
                        hoverlabel = list(font = list(color = autoFontColor(tmp.CI.color),
                        size = hovertext.font.size, family = hovertext.font.family)),
                        fill = "tonextx",
                        fillcolor = toRGB(tmp.CI.color, alpha = fit.CI.opacity),
                        showlegend = FALSE, legendgroup = i,
                        line = list(color=tmp.CI.color, width=0, shape='spline'))
            }
        }
        

        # Initialise attribute for PPT exporting - SeriesLabels cannot be NULL
        chart.labels$SeriesLabels[[i]] <- list(
            Font = setFontForPPT(tmp.data.label.font), ShowValue = any(data.label.show[,i]))

        if (any(data.label.show[,i]))
        {
            # Initialise custom points if annotations are used
            pt.segs <- NULL
            ind.show <- which(data.label.show[,i])
            tmp.suffix <- if (percentFromD3(data.label.format)) sub("%", "", data.label.suffix[,i])
                          else                                               data.label.suffix[,i]
            if (!is.null(annotation.list) || length(ind.show) < nrow(chart.matrix) ||
                any(nzchar(data.label.prefix[,i])) || any(nzchar(data.label.suffix[,i])))
            {
                chart.labels$SeriesLabels[[i]]$ShowValue <- FALSE
                pt.segs <- lapply((1:nrow(chart.matrix)),
                    function(ii) list(Index = ii-1, Segments = c(
                        if (nzchar(data.label.prefix[ii,i])) list(list(Text = data.label.prefix[ii,i])) else NULL,
                        list(list(Field="Value")),
                        if (nzchar(tmp.suffix[ii])) list(list(Text = tmp.suffix[ii])) else NULL)))
                for (ii in setdiff(1:nrow(chart.matrix), ind.show))
                    pt.segs[[ii]]$Segments <- NULL
            }

            # Apply annotations
            # Circle annotations are added to pt.segs but not to the data labels
            data.label.text <- data.annotations$text[,i]
            data.label.nchar <- nchar(data.label.text) # get length before adding html tags
            attr(data.label.text, "customPoints") <- pt.segs
            data.label.text <- applyAllAnnotationsToDataLabels(data.label.text, annotation.list,
            annot.data, i, ind.show, "Bar", clean.pt.segs = TRUE)
            pt.segs <- attr(data.label.text, "customPoints")
            p <- addTraceForBarTypeDataLabelAnnotations(p, type = "Bar", legend.text[i],
                    data.label.xpos = if (pyramid) rep(0, NROW(chart.matrix)) else data.annotations$x[,i],
                    data.label.ypos = if (NCOL(chart.matrix) > 1) data.annotations$y[,i] else x,
                    data.label.show = data.label.show[,i],
                    data.label.text = data.label.text,
                    data.label.sign = getSign(data.annotations$x[,i], xaxis), data.label.nchar,
                    annotation.list, annot.data, i,
                    yaxis = if (NCOL(chart.matrix) > 1) "y2" else "y", xaxis = "x",
                    tmp.data.label.font, is.stacked || pyramid, data.label.centered = FALSE)

            if (!is.null(pt.segs))
            {
                if (isTRUE(attr(pt.segs, "SeriesShowValue")))
                {
                    chart.labels$SeriesLabels[[i]]$ShowValue <- TRUE
                    attr(pt.segs, "SeriesShowValue") <- NULL
                }
                if (length(pt.segs) > 0)
                    chart.labels$SeriesLabels[[i]]$CustomPoints <- pt.segs
            }
        }

        # Add scatter trace to ensure hover is always shown
        # The hover in the main trace does not show if bars are too small
        # or if covered by the data labels
        # Changing layout.hovermode will make it more responsive but text is diagonal
        ypos <- if (NCOL(chart.matrix) > 1) data.annotations$y[,i] else x
        xpos <- if (NCOL(chart.matrix) > 1) data.annotations$x[,i] else y.filled
        if (pyramid)
            xpos <- rep(0, NROW(chart.matrix))
        ind.na <- which(!is.finite(y))
        if (length(ind.na) > 0)
            hover.template[ind.na] <- ""
        p <- add_trace(p, x = xpos, y = ypos, type = "scatter", name = legend.text[i],
                   mode = "markers", marker = list(color = tmp.color, opacity = 0),
                   hovertemplate = hover.template, hoverlabel = hover.label, 
                   showlegend = FALSE, yaxis = if (NCOL(chart.matrix) > 1) "y2" else "y")

    }

    # Only used for small multiples
    if (!is.null(average.series))
        p <- add_trace(p, y = x, x = average.series, name = "Average",
                type = "scatter", mode = "lines", showlegend = FALSE,
                hoverlabel = list(font = list(color = autoFontColor(average.color),
                size = hovertext.font.size, family = hovertext.font.family)),
                line = list(color = average.color))


    annotations <- NULL
    n <- length(annotations)
    annotations[[n+1]] <- setFooter(footer, footer.font, margins, footer.align)
    annotations[[n+2]] <- setSubtitle(subtitle, subtitle.font, margins, subtitle.align)
    annotations[[n+3]] <- setTitle(title, title.font, margins, title.align)
    annotations <- Filter(Negate(is.null), annotations)
    
    serieslabels.num.changes <- vapply(chart.labels$SeriesLabels, function(s) isTRUE(s$ShowValue) + length(s$CustomPoints), numeric(1L))
    if (sum(serieslabels.num.changes) == 0)
        chart.labels <- NULL

    p <- config(p, displayModeBar = modebar.show, showAxisDragHandles = axis.drag.enable)
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
        bargap = bar.gap, bargroupgap = bar.group.gap,
        barmode = barmode
    )
    attr(p, "can-run-in-root-dom") <- TRUE
    result <- list(htmlwidget = p)
    class(result) <- "StandardChart"
    attr(result, "ChartType") <- if (is.stacked) "Bar Stacked" else "Bar Clustered"
    attr(result, "ChartLabels") <- chart.labels
    result
}

