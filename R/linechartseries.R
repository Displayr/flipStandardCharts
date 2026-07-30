#' Names that prepareLineSeries returns
#'
#' The contract with the callers, checked on the way out. Most of these names are also
#' formals of the chart functions, so dropping one would leave the caller silently using
#' its own unprocessed argument instead: a wrong chart rather than an error.
#' @noRd
PREPARED.LINE.SERIES <- c(
    # formats, which the percentage handling may rewrite
    "y.tick.format", "y.hovertext.format", "y.tick.suffix",
    "data.label.format", "data.label.suffix",
    # the data being charted
    "annot.data", "chart.matrix", "n", "x.title", "x.labels.full",
    # series styling, one value per series
    "shape", "colors", "opacity", "line.type", "line.thickness",
    "fit.line.colors", "fit.CI.colors",
    # markers, per series except marker.show and marker.size which are per point
    "marker.show", "marker.symbols", "marker.colors", "marker.opacity",
    "marker.size", "marker.border.colors", "marker.border.opacity",
    # data labels, per series except data.label.show and the affixes
    "data.label.show", "data.label.font", "dlab.color", "dlab.pos",
    "dlab.prefix", "dlab.suffix",
    # chart furniture
    "legend.show", "footer")

#' Parses and checks the arguments shared by the line chart implementations
#'
#' \code{\link{Line}} and \code{labeledLine} accept the same arguments but render with
#' different libraries, so the work of turning those arguments into per-series and
#' per-point values is done here rather than in each of them. Recycling every setting to
#' the number of series happens in one place, which is what keeps the two charts
#' consistent when a new per-series option is added.
#'
#' Callers pass their own arguments straight through and merge the result back into their
#' frame:
#' \preformatted{
#'     prep <- do.call(prepareLineSeries,
#'                     mget(names(formals(prepareLineSeries)), sys.frame(sys.nframe())))
#'     list2env(prep, environment())
#' }
#'
#' @return A named list of the values in \code{PREPARED.LINE.SERIES}. Note that
#'     \code{chart.matrix}, and every setting recycled against it, gain a column when
#'     \code{average.series} is supplied.
#' @noRd
prepareLineSeries <- function(x,
    shape, colors, average.series, average.color, opacity,
    fit.type, fit.line.colors, fit.CI.show, fit.CI.colors,
    line.type, line.thickness,
    marker.show, marker.show.at.ends, marker.symbols, marker.colors,
    marker.opacity, marker.size, marker.border.colors, marker.border.opacity,
    data.label.show, data.label.show.at.ends, data.label.position,
    data.label.font.family, data.label.font.color, data.label.font.autocolor,
    data.label.font.size, data.label.format, data.label.prefix, data.label.suffix,
    legend.show, footer, footer.wrap, footer.wrap.nchar,
    x.title, y.tick.format, y.hovertext.format, y.tick.suffix)
{
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
    if (is.null(marker.show) || isTRUE(marker.show == "none")) # included for backwards compatibility
        marker.show <- FALSE
    if (is.null(opacity))
        opacity <- if (fit.type == "None") 1 else 0.6
    # opacity is per series, but the marker opacities are single values: the widget takes
    # one transparency for the whole chart, and a series that should be more transparent is
    # given a color with an alpha instead. Collapsing here rather than at each point of use
    # keeps the two chart paths identical and stops a vector reaching toRGB, which rejects
    # an alpha longer than the color it is applied to.
    # Only a value the caller set is worth warning about. Markers inherit `opacity`, which is
    # legitimately per series, and warnUnsupportedByAutoPlacement already reports that case
    # when markers are actually drawn.
    marker.opacity.given <- !is.null(marker.opacity)
    marker.border.opacity.given <- !is.null(marker.border.opacity)
    if (is.null(marker.opacity))
        marker.opacity <- opacity
    if (is.null(marker.border.opacity))
        marker.border.opacity <- marker.opacity
    marker.opacity <- firstOpacity(marker.opacity, "marker.opacity",
                                   warn = marker.opacity.given)
    marker.border.opacity <- firstOpacity(marker.border.opacity, "marker.border.opacity",
                                          warn = marker.border.opacity.given)

    # Set colors
    n <- ncol(chart.matrix)
    colors <- vectorize(colors, n)
    if (fit.type != "None" && is.null(fit.line.colors))
        fit.line.colors <- colors
    if (fit.CI.show && is.null(fit.CI.colors))
        fit.CI.colors <- fit.line.colors
    # These default to colors, and the default is resolved before colors is recycled here,
    # so they have to be recycled too. Left as they came, one color for several series
    # leaves the later fit lines with no color at all, and an average series takes the
    # place of the second one. They stay NULL when no fit is drawn, because vectorize
    # would turn that into empty strings.
    if (!is.null(fit.line.colors))
        fit.line.colors <- vectorize(fit.line.colors, n)
    if (!is.null(fit.CI.colors))
        fit.CI.colors <- vectorize(fit.CI.colors, n)
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
    marker.symbols <- vectorize(marker.symbols, ncol(chart.matrix))
    marker.size <- vectorize(readNumericSeries(marker.size, ncol(chart.matrix), "marker size"),
                             ncol(chart.matrix), nrow(chart.matrix))
    dlab.color <- if (data.label.font.autocolor) colors
                  else vectorize(data.label.font.color, ncol(chart.matrix))
    dlab.pos <- vectorize(tolower(data.label.position), ncol(chart.matrix))
    dlab.prefix <- vectorize(data.label.prefix, ncol(chart.matrix), nrow(chart.matrix), split = NULL)
    dlab.suffix <- vectorize(data.label.suffix, ncol(chart.matrix), nrow(chart.matrix), split = NULL)
    data.label.font <- lapply(dlab.color,
        function(cc) list(family = data.label.font.family, size = data.label.font.size, color = cc))

    legend.show <- setShowLegend(legend.show, NCOL(chart.matrix))
    footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate = FALSE)

    if (!is.null(average.series))
    {
        chart.matrix <- cbind(chart.matrix, average.series)
        colnames(chart.matrix)[ncol(chart.matrix)] <- "Average"
        colors <- c(colors, average.color)
        # The plotly chart never indexes the marker colors past the real series,
        # because the average series has no markers, but the widget builds its
        # per-point marker arrays across every column.
        marker.colors <- c(marker.colors, average.color)
        marker.border.colors <- c(marker.border.colors, average.color)
        fit.line.colors <- c(fit.line.colors, average.color)
        fit.CI.colors <- c(fit.CI.colors, average.color)
        line.type <- line.type[c(1:n,1)]
        marker.show <- cbind(marker.show, FALSE)
        marker.size <- marker.size[,c(1:n,1)] # doesn't matter - marker is not shown
        marker.symbols <- marker.symbols[c(1:n,1)]
    }

    if (is.null(rownames(chart.matrix)))
        rownames(chart.matrix) <- 1:nrow(chart.matrix)

    line.thickness <- readNumericSeries(line.thickness, ncol(chart.matrix), "line thickness")
    opacity <- readNumericSeries(opacity, ncol(chart.matrix), "opacity")

    prepared <- list(
        y.tick.format = y.tick.format, y.hovertext.format = y.hovertext.format,
        y.tick.suffix = y.tick.suffix, data.label.format = data.label.format,
        data.label.suffix = data.label.suffix,
        annot.data = annot.data, chart.matrix = chart.matrix, n = n,
        x.title = x.title, x.labels.full = x.labels.full,
        shape = shape, colors = colors, opacity = opacity, line.type = line.type,
        line.thickness = line.thickness, fit.line.colors = fit.line.colors,
        fit.CI.colors = fit.CI.colors,
        marker.show = marker.show, marker.symbols = marker.symbols,
        marker.colors = marker.colors, marker.opacity = marker.opacity,
        marker.size = marker.size, marker.border.colors = marker.border.colors,
        marker.border.opacity = marker.border.opacity,
        data.label.show = data.label.show, data.label.font = data.label.font,
        dlab.color = dlab.color, dlab.pos = dlab.pos,
        dlab.prefix = dlab.prefix, dlab.suffix = dlab.suffix,
        legend.show = legend.show, footer = footer)
    stopifnot(setequal(names(prepared), PREPARED.LINE.SERIES))
    prepared
}

#' Names that prepareLineAxes returns
#' @noRd
PREPARED.LINE.AXES <- c("axisFormat", "xaxis", "yaxis", "legend", "legend.text",
                        "margins")

#' Works out the axes and margin spacing of a line chart
#'
#' Extracted so that \code{labeledLine} can reserve the same margins as
#' \code{\link{Line}} rather than estimating them a second time. It only needs
#' \code{margins}, but the margins are worked out from the axis objects, so those are
#' built here too and \code{Line} takes them from the same place.
#'
#' The margins are only a starting point. Both renderers leave plotly's automargin on, and
#' automargin can grow a margin but never shrink one, so what is reserved here is a lower
#' bound: whichever is larger, this estimate or the width of the text as measured in the
#' browser. That is why the estimate has to be shared. The widget asks for a small fixed
#' margin of its own and lets automargin do the work, which produces a tighter chart than
#' Line and a visible reflow when automatic data label placement is turned on.
#'
#' @return A named list of the values in \code{PREPARED.LINE.AXES}.
#' @noRd
prepareLineAxes <- function(chart.matrix, legend.show,
    title, title.font.size, subtitle, subtitle.font.size, footer, footer.font.size,
    x.title, x.title.font.family, x.title.font.size, x.title.font.color,
    x.line.color, x.line.width, x.grid.width, x.grid.color, x.grid.dash,
    x.tick.font.family, x.tick.font.size, x.tick.font.color,
    x.tick.angle, x.tick.mark.length, x.tick.distance,
    x.tick.format, x.tick.prefix, x.tick.suffix, x.tick.show, x.tick.mark.color,
    x.tick.maxnum, x.tick.label.wrap, x.tick.label.wrap.nchar,
    x.bounds.minimum, x.bounds.maximum, x.zero, x.zero.line.width, x.zero.line.color,
    x.data.reversed, x.hovertext.format,
    y.title, y.title.font.family, y.title.font.size, y.title.font.color,
    y.line.color, y.line.width, y.grid.width, y.grid.color, y.grid.dash,
    y.tick.font.family, y.tick.font.size, y.tick.font.color,
    y.tick.angle, y.tick.mark.length, y.tick.distance,
    y.tick.format, y.tick.prefix, y.tick.suffix, y.tick.show, y.tick.mark.color,
    y.tick.maxnum, y.bounds.minimum, y.bounds.maximum, y.zero, y.zero.line.width,
    y.zero.line.color, y.data.reversed, y.hovertext.format,
    grid.show, zoom.enable,
    legend.font.family, legend.font.size, legend.font.color,
    legend.ascending, legend.fill.color,
    legend.fill.opacity, legend.border.color, legend.border.line.width,
    legend.position.x, legend.position.y, legend.orientation,
    legend.wrap, legend.wrap.nchar,
    margin.top, margin.bottom, margin.left, margin.right, margin.inner.pad,
    margin.autoexpand)
{
    # Built here rather than taken from the caller, so that both callers can pass their
    # own arguments straight through without holding any intermediate values
    x.title.font <- list(family = x.title.font.family, size = x.title.font.size,
                         color = x.title.font.color)
    y.title.font <- list(family = y.title.font.family, size = y.title.font.size,
                         color = y.title.font.color)
    xtick.font <- list(family = x.tick.font.family, size = x.tick.font.size,
                       color = x.tick.font.color)
    ytick.font <- list(family = y.tick.font.family, size = y.tick.font.size,
                       color = y.tick.font.color)
    legend.font <- list(family = legend.font.family, size = legend.font.size,
                        color = legend.font.color)

    legend <- setLegend("Line", legend.font, legend.ascending, legend.fill.color, legend.fill.opacity,
                        legend.border.color, legend.border.line.width,
                        legend.position.x, legend.position.y, FALSE, legend.orientation)

    # Format axis labels
    axisFormat <- formatLabels(chart.matrix, "Line", x.tick.label.wrap, x.tick.label.wrap.nchar,
                               x.tick.format, y.tick.format)
    x.range <- setValRange(x.bounds.minimum, x.bounds.maximum, axisFormat, x.zero, is.null(x.tick.distance))
    y.range <- setValRange(y.bounds.minimum, y.bounds.maximum, chart.matrix, y.zero, is.null(y.tick.distance))
    xtick <- setTicks(x.range$min, x.range$max, x.tick.distance, x.data.reversed)
    ytick <- setTicks(y.range$min, y.range$max, y.tick.distance, y.data.reversed)

    yaxis <- setAxis(y.title, "left", axisFormat, y.title.font,
                  y.line.color, y.line.width, y.grid.width * grid.show, y.grid.color, y.grid.dash,
                  ytick, ytick.font, y.tick.angle, y.tick.mark.length, y.tick.distance,
                  y.tick.format, y.tick.prefix, y.tick.suffix,
                  y.tick.show, y.zero, y.zero.line.width, y.zero.line.color,
                  y.hovertext.format, num.maxticks = y.tick.maxnum,
                  tickcolor = y.tick.mark.color, zoom.enable = zoom.enable)
    xaxis <- setAxis(x.title, "bottom", axisFormat, x.title.font,
                  x.line.color, x.line.width, x.grid.width * grid.show, x.grid.color, x.grid.dash,
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

    prepared <- list(axisFormat = axisFormat, xaxis = xaxis, yaxis = yaxis,
                     legend = legend, legend.text = legend.text, margins = margins)
    stopifnot(setequal(names(prepared), PREPARED.LINE.AXES))
    prepared
}

#' Calls prepareLineAxes with the calling chart function's own values
#'
#' The caller must have every formal of \code{prepareLineAxes} in scope, whether as its
#' own argument or as something it has worked out, such as the tick fonts.
#' @param frame The calling function's frame, from \code{sys.frame(sys.nframe())}.
#' @noRd
prepareLineAxesFrom <- function(frame)
    do.call(prepareLineAxes, mget(names(formals(prepareLineAxes)), frame))

#' Calls prepareLineSeries with the calling chart function's own arguments
#'
#' The caller must have every formal of \code{prepareLineSeries} among its own, which
#' both line chart implementations do. mget errors if one is missing, so a signature
#' that drifts out of step is caught rather than silently defaulted.
#' @param frame The calling function's frame, from \code{sys.frame(sys.nframe())}.
#' @return The list described by \code{PREPARED.LINE.SERIES}, ready for
#'     \code{list2env(.., environment())}.
#' @noRd
prepareLineSeriesFrom <- function(frame)
    do.call(prepareLineSeries, mget(names(formals(prepareLineSeries)), frame))
