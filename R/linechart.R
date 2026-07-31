#' Line
#'
#' Line chart
#' @inherit Column
#' @inherit Area
#' @param shape Either "linear" for straight lines between data points or "spline" for curved lines.
#' @param smoothing Numeric; smoothing if \code{shape} is "spline".
#' @param line.type Character; one of 'solid', 'dot', 'dash'. This can be a single value
#'     or a vector with one value for each series.
#' @param marker.symbols Character; marker symbols, which are only shown if marker.show = TRUE.
#'     if a vector is passed, then each element will be applied to a data series.
#' @param data.label.position Character; one of 'top' or 'bottom'. This can
#'    be a single value or a vector with one value for each series.
#' @param data.label.show.at.ends Logical; show data labels at the beginning and end of each
#'      data series. This value will override \code{data.label.show}.
#' @param data.label.show.at.last.end Logical; show a data label at the end of each data
#'      series only, rather than at both ends. This value will override
#'      \code{data.label.show} and \code{data.label.show.at.ends}.
#' @param marker.show.at.ends Logical; show markers at the begining and end of each
#'      data series. The value will override \code{marker.show}.
#' @param marker.show.at.last.end Logical; show a marker at the end of each data series
#'      only, rather than at both ends. This value will override \code{marker.show} and
#'      \code{marker.show.at.ends}.
#' @param marker.opacity A single value between 0 and 1 for the opacity of the
#'      markers. Unlike \code{opacity} this cannot vary by series; give a color with
#'      an alpha value to do that.
#' @param marker.border.opacity A single value between 0 and 1 for the opacity of the
#'      marker borders. Unlike \code{opacity} this cannot vary by series; give a color
#'      with an alpha value to do that.
#' @param data.label.auto.placement Logical; position the data labels so that they do
#'      not overlap each other or the data points, instead of placing them at a fixed
#'      offset given by \code{data.label.position}. Only has an effect when data labels
#'      are shown. The chart is then drawn using rhtmlCombinedScatter rather than plotly,
#'      which does not support every charting option; a warning names any setting
#'      that has been ignored as a result.
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
                    y.zero.line.dash = "Solid",
                    y.data.reversed = FALSE,
                    y.grid.width = 1 * grid.show,
                    y.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                    y.grid.dash = "Solid",
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
                    x.zero.line.dash = "Solid",
                    x.data.reversed = FALSE,
                    x.grid.width = 0 * grid.show,
                    x.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                    x.grid.dash = "Solid",
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
                    marker.show.at.last.end = FALSE,
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
                    axis.drag.enable = FALSE,
                    data.label.show = FALSE,
                    data.label.show.at.ends = FALSE,
                    data.label.show.at.last.end = FALSE,
                    data.label.auto.placement = FALSE,
                    data.label.position = "Top",
                    data.label.font.family = global.font.family,
                    data.label.font.color = global.font.color,
                    data.label.font.autocolor = FALSE,
                    data.label.font.size = 10,
                    data.label.format = "",
                    data.label.prefix = "",
                    data.label.suffix = "")
{
    # Automatic placement only means anything when there are labels to place. Any one series
    # or point asking for a label is enough, so the setting is only split out of its text
    # form here, never sized: sizing it to a series count would answer from the values that
    # survived rather than from all of them, and the answer would depend on which series
    # asked. Compared with %in% rather than any(), because the text form cannot be coerced
    # by a logical operator.
    data.label.show.given <- if (is.character(data.label.show)) TextAsVector(data.label.show)
                             else data.label.show
    data.labels.requested <- isTRUE(data.label.show.at.ends) ||
        isTRUE(data.label.show.at.last.end) ||
        any(data.label.show.given %in% c(TRUE, "TRUE"))
    if (isTRUE(data.label.auto.placement) && data.labels.requested)
    {
        # labeledLine takes the same arguments as Line, except for the flag selecting
        # it, so the whole call is forwarded as-is.
        args <- mget(setdiff(names(formals()), "data.label.auto.placement"),
                     sys.frame(sys.nframe()))
        return(do.call(labeledLine, args))
    }

    ErrorIfNotEnoughData(x)
    # Assigned by the list2env below, but declared here because static analysis cannot
    # see through it. Everything else it sets is already a formal of this function.
    annot.data <- chart.matrix <- n <- x.labels.full <- NULL
    data.label.font <- dlab.pos <- dlab.prefix <- dlab.suffix <- NULL
    list2env(prepareLineSeriesFrom(sys.frame(sys.nframe())), environment())

    # The axis and tick fonts are assembled by prepareLineAxes, which is the only thing
    # that needs them; these three are used to place the text below
    title.font = list(family = title.font.family, size = title.font.size, color = title.font.color)
    subtitle.font = list(family = subtitle.font.family, size = subtitle.font.size, color = subtitle.font.color)
    footer.font = list(family = footer.font.family, size = footer.font.size, color = footer.font.color)

    # Assigned by the list2env below; declared because static analysis cannot see it
    axisFormat <- xaxis <- yaxis <- legend <- legend.text <- margins <- NULL
    list2env(prepareLineAxesFrom(sys.frame(sys.nframe())), environment())

    ## Initiate plotly object
    p <- plot_ly(as.data.frame(chart.matrix))
    x.labels <- axisFormat$labels
    y.labels <- colnames(chart.matrix)

    # Add invisible line to force all categorical labels to be shown
    tmp.min <- if (any(is.finite(chart.matrix))) min(chart.matrix[is.finite(chart.matrix)])
               else y.bounds.minimum
    tmp.mode <- if (any(marker.show) || any(data.label.show)) "lines+markers" else "lines"
    p <- add_trace(p, x = x.labels, y = rep(tmp.min, length(x.labels)),
                   type = "scatter", mode = tmp.mode,
                   hoverinfo = "skip", showlegend = FALSE, opacity = 0)

    ## Add a trace for each col of data in the matrix
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
                       marker = list(color = toRGB(colors[i], alpha = marker.opacity),
                                     size = marker.size[ind.single, i],
                                     symbol = marker.symbols[i]),
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
                        if (nzchar(dlab.prefix[ii,i])) list(list(Text = unescape_html(dlab.prefix[ii,i]))) else NULL,
                        list(list(Field="Value")),
                        if (nzchar(dlab.suffix[ii,i])) list(list(Text = unescape_html(dlab.suffix[ii,i]))) else NULL))))
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
                   bgcolor = toRGB(colors[i], alpha = opacity[i])),
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

    p <- config(p, displayModeBar = modebar.show, showAxisDragHandles = axis.drag.enable)
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
        shapes = zerolines(x.zero, x.zero.line.width, x.zero.line.color, x.zero.line.dash,
            y.zero, y.zero.line.width, y.zero.line.color, y.zero.line.dash),
        hovermode = if (tooltip.show) "closest" else FALSE,
        hoverlabel = list(namelength = -1, bordercolor = "transparent", align = hovertext.align,
            font = list(size = hovertext.font.size, family = hovertext.font.family))
    )
    attr(p, "can-run-in-root-dom") <- TRUE
    result <- list(htmlwidget = p)
    class(result) <- "StandardChart"
    attr(result, "ChartType") <- if (all(marker.show)) "Line Markers" else "Line"
    attr(result, "ChartLabels") <- chart.labels
    attr(result, "CustomPoints") <- markerPointsForPPT(marker.show, marker.size, n)
    result
}

