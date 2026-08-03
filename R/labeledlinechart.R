#' Line chart with automatically placed data labels
#'
#' Draws a line chart using rhtmlCombinedScatter instead of plotly, so that the data
#' labels can be positioned by the label placement algorithm rather than sitting at a
#' fixed offset from each point. Called by \code{\link{Line}} when
#' \code{data.label.auto.placement} is turned on; it is not intended to be called
#' directly, and it accepts the same arguments as \code{Line} so that dispatch is a
#' straight pass-through.
#'
#' Some \code{Line} arguments have no equivalent in rhtmlCombinedScatter. These are
#' ignored, with a warning naming them, rather than causing an error.
#' @inheritParams Line
#' @importFrom grDevices rgb
#' @importFrom flipChartBasics ChartColors
#' @importFrom plotly toRGB
#' @importFrom rhtmlCombinedScatter CombinedScatter
#' @noRd
labeledLine <- function(x,
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
    # Checked before anything below vectorizes the arguments, so that the values
    # compared against the defaults are the ones the caller actually passed.
    warnUnsupportedByAutoPlacement(mget(names(formals()), sys.frame(sys.nframe())),
                                   NCOL(x), NROW(x))

    # Assigned by the list2env below, but declared here because static analysis cannot
    # see through it. Everything else it sets is already a formal of this function.
    annot.data <- chart.matrix <- n <- NULL
    data.label.font <- dlab.color <- dlab.prefix <- dlab.suffix <- NULL
    list2env(prepareLineSeriesFrom(sys.frame(sys.nframe())), environment())

    # Counted with %in% because vectorize returns the character form for text input
    n.labels <- sum(data.label.show %in% c(TRUE, "TRUE"))
    if (n.labels >= 100)
        warning("Data labels are not automatically placed when 100 or more labels are ",
                "shown. ", n.labels, " labels are shown in this chart.")

    n.row <- nrow(chart.matrix)
    n.col <- ncol(chart.matrix)

    # Reserve the same margins as the plotly line chart, so that turning automatic data
    # label placement on does not reflow the chart. Only the margins are wanted here; the
    # axis objects they are worked out from are of no use to the widget, which builds its
    # own axes. Left to itself the widget asks for a small fixed margin and lets plotly's
    # automargin grow it, which gives a tighter chart than Line.
    # The axis objects are also the source of the range mode and tick count, so that
    # whatever setAxis decides for the plotly chart reaches the widget too
    prepared.axes <- prepareLineAxesFrom(sys.frame(sys.nframe()))
    margins <- prepared.axes$margins
    xaxis <- prepared.axes$xaxis
    yaxis <- prepared.axes$yaxis

    # The widget places the title, subtitle and footer inside these margins, the way the
    # plotly chart does. setFooter pads the footer text down the bottom margin, so the
    # padded text is what has to be sent rather than the footer itself.
    footer.font <- list(family = footer.font.family, size = footer.font.size,
                        color = footer.font.color)
    footer.annot <- setFooter(footer, footer.font, margins, footer.align)
    footer.text <- if (is.null(footer.annot)) "" else footer.annot$text

    # The widget decides the axis type from the class of X, and does its own tick
    # formatting and label wrapping. Dates are parsed here because table rownames
    # always arrive as character, so the widget cannot recognise them on its own. Note
    # that this deliberately does not wrap the labels, unlike the call above that feeds
    # the margin estimate: the widget wraps them itself.
    axis.format <- formatLabels(chart.matrix, "Line", FALSE, x.tick.label.wrap.nchar,
                                x.tick.format, y.tick.format)
    x.axis.type <- axis.format$x.axis.type
    x.labels <- switch(x.axis.type,
        date = axis.format$labels,
        numeric = suppressWarnings(as.numeric(gsub(",", "", rownames(chart.matrix)))),
        rownames(chart.matrix))
    # There is deliberately no local fallback for labels that do not parse as numbers.
    # getAxisType only calls an axis numeric once this same parse succeeds on every label,
    # so it cannot happen, and a fallback here could not repair the chart if it did:
    # prepareLineAxes above has already worked the range, ticks, tick angle and margins out
    # from the same axis type, so switching it here would only trade one disagreement for
    # another. Should the parse ever need to give way, it has to give way inside
    # getAxisType, where every consumer of the axis type sees it.

    x.values <- rep(x.labels, times = n.col)
    y.values <- as.numeric(chart.matrix)
    groups <- rep(colnames(chart.matrix), each = n.row)

    # Data labels, hovertext and the PPT export metadata are all built per series.
    # Points whose label is hidden are given an empty label: the placement algorithm
    # skips those, and they do not count towards its 100 label limit.
    labels <- rep("", n.row * n.col)
    tooltips <- rep("", n.row * n.col)
    # Annotation markup is kept out of the label and sent alongside it, because the widget
    # escapes the label as text but appends these as markup
    pre.annots <- rep("", n.row * n.col)
    post.annots <- rep("", n.row * n.col)
    chart.labels <- list(SeriesLabels = list())
    for (i in 1:n) # does not include average.series
    {
        offset <- (i - 1) * n.row
        ind.show <- which(data.label.show[,i] & is.finite(chart.matrix[,i]))
        source.text <- formatByD3(chart.matrix[,i], data.label.format,
            dlab.prefix[,i], dlab.suffix[,i], decimals = 0)

        # Add attribute for PPT exporting
        chart.labels$SeriesLabels[[i]] <- list(Position = "Top",
            Font = setFontForPPT(data.label.font[[i]]), ShowValue = length(ind.show) > 0)

        if (length(ind.show) > 0)
        {
            # Initialise custom points if annotations are used
            pt.segs <- NULL
            if (!is.null(annotation.list) || length(ind.show) < n.row ||
                any(nzchar(dlab.prefix[,i])) || any(nzchar(dlab.suffix[,i])))
            {
                chart.labels$SeriesLabels[[i]]$ShowValue <- FALSE
                pt.segs <- lapply((1:n.row),
                    function(ii) return(list(Index = ii-1, Segments = c(
                        if (nzchar(dlab.prefix[ii,i])) list(list(Text = unescape_html(dlab.prefix[ii,i]))) else NULL,
                        list(list(Field="Value")),
                        if (nzchar(dlab.suffix[ii,i])) list(list(Text = unescape_html(dlab.suffix[ii,i]))) else NULL))))
                for (ii in setdiff(1:n.row, ind.show))
                    pt.segs[[ii]]$Segments <- NULL
            }

            # Apply annotations. The widget treats the label as text and escapes it, and
            # takes the markup separately, so the annotations are worked out around a
            # placeholder standing in for the label and then split off either side of it.
            # Going through applyAllAnnotationsToDataLabels rather than assembling the
            # markup here is what keeps every annotation type identical to the plotly
            # chart, including the ones that wrap the label rather than sit beside it.
            # tspan because the widget draws its labels as SVG.
            placeholder <- rep(ANNOTATION.LABEL.PLACEHOLDER, n.row)
            attr(placeholder, "customPoints") <- pt.segs
            placeholder <- applyAllAnnotationsToDataLabels(placeholder, annotation.list,
                annot.data, i, ind.show, "Line", clean.pt.segs = TRUE, tspan = TRUE)
            pt.segs <- attr(placeholder, "customPoints")

            around <- splitAroundPlaceholder(placeholder)
            pre.annots[offset + ind.show] <- around$before[ind.show]
            post.annots[offset + ind.show] <- around$after[ind.show]
            # an annotation that hides a label removes the placeholder with it
            source.text[!around$kept] <- ""
            if (isTRUE(attr(pt.segs, "SeriesShowValue")))
            {
                chart.labels$SeriesLabels[[i]]$ShowValue <- TRUE
                attr(pt.segs, "SeriesShowValue") <- NULL
            }
            if (length(pt.segs) > 0)
                chart.labels$SeriesLabels[[i]]$CustomPoints <- pt.segs

            labels[offset + ind.show] <- source.text[ind.show]
        }
    }

    # The widget shows the series name in a separate part of the tooltip, so the
    # <extra> block that plotly uses for it is dropped here.
    if (tooltip.show)
    {
        for (i in 1:n.col)
        {
            offset <- (i - 1) * n.row
            template <- setHoverTemplate(i, list(type = x.axis.type), chart.matrix,
                                         hovertext.template)
            template <- sub("<extra>.*</extra>$", "", template)
            tooltips[offset + (1:n.row)] <- evalHoverTemplate(
                rep(template, length = n.row),
                x.labels, x.hovertext.format, x.tick.prefix, x.tick.suffix,
                as.numeric(chart.matrix[,i]), y.hovertext.format,
                y.tick.prefix, y.tick.suffix)
        }
    }

    # Markers are drawn by the widget for every point, so hidden markers are given a
    # radius of zero. Hover still works because the line trace carries the tooltip.
    # as.vector is needed so these serialise as flat arrays rather than nested ones.
    # The border opacity is folded into the color, as the plotly chart does, because the
    # widget takes only a color per point for the border
    border.colors <- toRGB(marker.border.colors, alpha = marker.border.opacity)
    point.radius <- as.vector(ifelse(marker.show, marker.size / 2, 0))
    point.border.colors <- as.vector(ifelse(marker.show,
        rep(border.colors, each = n.row), ""))
    point.border.widths <- as.vector(ifelse(marker.show, marker.border.width, 0))

    # One symbol per point, as the widget slices these per series. Unlike the border color
    # a hidden marker keeps its symbol, because "" is not a valid plotly symbol and the
    # zero radius already hides the point.
    point.symbols <- as.vector(rep(marker.symbols, each = n.row))

    fit <- fitSeriesForCombinedScatter(chart.matrix, x.labels, x.axis.type, fit.type,
        fit.ignore.last, fit.CI.show, fit.window.size, fit.line.name, fit.line.colors,
        fit.CI.colors, fit.CI.opacity)

    p <- rhtmlCombinedScatter::CombinedScatter(
        X = x.values,
        Y = y.values,
        group = groups,
        x.levels = if (x.axis.type == "category") rownames(chart.matrix) else NULL,
        colors = colors,
        color.transparency = marker.opacity,
        label = labels,
        pre.label.annotations = pre.annots,
        post.label.annotations = post.annots,
        labels.show = TRUE,
        label.auto.placement = TRUE,
        labels.font.family = data.label.font.family,
        # One colour per point, in the same order as the labels: the widget colours each
        # label individually, so a colour per series is spread across that series' points.
        # Sized to the charted columns rather than to dlab.color, which is worked out before
        # an average series adds one and so would leave the arrays a series short.
        # Automatic colouring still sends nothing, leaving each label to take the colour of
        # its own point, which is already the series colour.
        labels.font.color = if (data.label.font.autocolor) NULL
                            else rep(vectorize(dlab.color, n.col), each = n.row),
        labels.font.size = data.label.font.size,
        line.show = TRUE,
        line.colors = toRGB(colors, alpha = opacity),
        line.thickness = line.thickness,
        line.type = line.type,
        line.shape = shape,
        line.smoothing = smoothing,
        point.radius = point.radius,
        point.symbol = point.symbols,
        point.border.color = point.border.colors,
        point.border.width = point.border.widths,
        grid = grid.show,
        legend.show = legend.show,
        legend.font.color = legend.font.color,
        legend.font.family = legend.font.family,
        legend.font.size = legend.font.size,
        legend.x = legend.position.x,
        legend.y = legend.position.y,
        legend.wrap = legend.wrap,
        legend.wrap.n.char = legend.wrap.nchar,
        legend.orientation = legend.orientation,
        # Already includes any margin.* the caller set, via setCustomMargins
        margin.autoexpand = margins$autoexpand,
        margin.top = margins$t,
        margin.bottom = margins$b,
        margin.left = margins$l,
        margin.right = margins$r,
        title = title,
        title.font.family = title.font.family,
        title.font.color = title.font.color,
        title.font.size = title.font.size,
        title.alignment = title.align,
        subtitle = subtitle,
        subtitle.font.family = subtitle.font.family,
        subtitle.font.color = subtitle.font.color,
        subtitle.font.size = subtitle.font.size,
        subtitle.alignment = subtitle.align,
        footer = footer.text,
        footer.font.family = footer.font.family,
        footer.font.color = footer.font.color,
        footer.font.size = footer.font.size,
        footer.alignment = footer.align,
        y.title = y.title,
        y.title.font.family = y.title.font.family,
        y.title.font.color = y.title.font.color,
        y.title.font.size = y.title.font.size,
        x.title = x.title,
        x.title.font.family = x.title.font.family,
        x.title.font.color = x.title.font.color,
        x.title.font.size = x.title.font.size,
        x.axis.show = x.tick.show,
        x.axis.font.family = x.tick.font.family,
        x.axis.font.color = x.tick.font.color,
        x.axis.font.size = x.tick.font.size,
        x.axis.tick.length = x.tick.mark.length,
        x.axis.tick.color = x.tick.mark.color,
        # setAxis rotates long categorical labels, so the angle it worked out has to be
        # used rather than the argument the caller passed
        x.axis.tick.angle = xaxis$tickangle,
        x.axis.line.width = x.line.width,
        x.axis.line.color = x.line.color,
        # The widget's origin flag gates both zero lines at once, so each one is turned
        # off through its own width instead, which is what makes x.zero and y.zero
        # independent of one another as they are in the plotly chart
        x.axis.zero.line.width = if (isTRUE(x.zero)) x.zero.line.width else 0,
        x.axis.zero.line.color = x.zero.line.color,
        x.axis.zero.line.dash = tolower(x.zero.line.dash),
        x.axis.grid.width = x.grid.width,
        x.axis.grid.color = x.grid.color,
        x.axis.grid.dash = tolower(x.grid.dash),
        x.axis.label.wrap = x.tick.label.wrap,
        x.axis.label.wrap.n.char = x.tick.label.wrap.nchar,
        # Taken from the axes setAxis built, so that x.zero and x.tick.maxnum reach the
        # widget by the same route the plotly chart uses
        x.axis.range.mode = xaxis$rangemode,
        x.axis.tick.maxnum = xaxis$nticks,
        y.axis.show = y.tick.show,
        y.axis.font.family = y.tick.font.family,
        y.axis.font.color = y.tick.font.color,
        y.axis.font.size = y.tick.font.size,
        y.axis.tick.length = y.tick.mark.length,
        y.axis.tick.color = y.tick.mark.color,
        y.axis.line.width = y.line.width,
        y.axis.line.color = y.line.color,
        y.axis.zero.line.width = if (isTRUE(y.zero)) y.zero.line.width else 0,
        y.axis.zero.line.color = y.zero.line.color,
        y.axis.zero.line.dash = tolower(y.zero.line.dash),
        y.axis.grid.width = y.grid.width,
        y.axis.grid.color = y.grid.color,
        y.axis.grid.dash = tolower(y.grid.dash),
        y.axis.tick.angle = yaxis$tickangle,
        y.axis.range.mode = yaxis$rangemode,
        y.axis.tick.maxnum = yaxis$nticks,
        x.format = x.tick.format,
        y.format = y.tick.format,
        x.hover.format = x.hovertext.format,
        y.hover.format = y.hovertext.format,
        x.prefix = x.tick.prefix,
        y.prefix = y.tick.prefix,
        x.suffix = x.tick.suffix,
        y.suffix = y.tick.suffix,
        x.bounds.minimum = x.bounds.minimum,
        x.bounds.maximum = x.bounds.maximum,
        x.bounds.units.major = x.tick.distance,
        y.bounds.minimum = y.bounds.minimum,
        y.bounds.maximum = y.bounds.maximum,
        y.bounds.units.major = y.tick.distance,
        origin = TRUE,
        tooltip.show = tooltip.show,
        tooltip.text = if (tooltip.show) tooltips else NULL,
        tooltip.font.family = hovertext.font.family,
        tooltip.font.size = hovertext.font.size,
        trend.lines.show = FALSE,
        fit.x = fit$fit.x,
        fit.y = fit$fit.y,
        fit.group = fit$fit.group,
        fit.lower.bound = fit$fit.lower.bound,
        fit.upper.bound = fit$fit.upper.bound,
        fit.line.names = fit$fit.line.names,
        fit.line.type = fit.line.type,
        fit.line.width = fit.line.width,
        fit.line.opacity = fit.line.opacity,
        fit.line.colors = fit$fit.line.colors,
        fit.ci.colors = fit$fit.ci.fill.colors,
        fit.ci.label.colors = fit$fit.ci.label.colors,
        background.color = toRGB(background.fill.color, alpha = background.fill.opacity),
        plot.background.color = toRGB(charting.area.fill.color,
                                      alpha = charting.area.fill.opacity),
        debug.mode = grepl("DEBUG_MODE_ON", title))

    serieslabels.num.changes <- vapply(chart.labels$SeriesLabels,
        function(s) isTRUE(s$ShowValue) + length(s$CustomPoints), numeric(1L))
    if (sum(serieslabels.num.changes) == 0)
        chart.labels <- NULL

    result <- list(htmlwidget = p)
    class(result) <- "StandardChart"
    attr(result, "ChartType") <- if (all(marker.show)) "Line Markers" else "Line"
    attr(result, "ChartLabels") <- chart.labels
    attr(result, "CustomPoints") <- markerPointsForPPT(marker.show, marker.size, n)
    result
}

#' Stands in for a data label while the annotations around it are worked out
#'
#' A control character, so that it cannot occur in a formatted value, a prefix or a
#' suffix and be mistaken for the placeholder.
#' @noRd
ANNOTATION.LABEL.PLACEHOLDER <- "\001"

#' Splits annotated placeholder text into the markup either side of the label
#'
#' @param text Annotations applied to \code{ANNOTATION.LABEL.PLACEHOLDER}. Annotations
#'     either precede the label, follow it, or wrap it, so each entry holds at most one
#'     placeholder; an annotation that hides a label leaves none.
#' @return A list of \code{before} and \code{after}, the markup on each side, and
#'     \code{kept}, FALSE where the label was hidden.
#' @noRd
splitAroundPlaceholder <- function(text)
{
    kept <- grepl(ANNOTATION.LABEL.PLACEHOLDER, text, fixed = TRUE)
    before <- after <- rep("", length(text))
    before[kept] <- sub(paste0(ANNOTATION.LABEL.PLACEHOLDER, ".*$"), "", text[kept])
    after[kept] <- sub(paste0("^.*", ANNOTATION.LABEL.PLACEHOLDER), "", text[kept])
    list(before = before, after = after, kept = kept)
}

#' Fits a line to each series in the form expected by rhtmlCombinedScatter
#'
#' Unlike the plotly line chart, which adds one trace per fitted series, the widget takes
#' the fits as parallel lists indexed by series.
#' @return A list of the fit.* arguments of \code{CombinedScatter}, all NULL when
#'     \code{fit.type} is "None".
#' @noRd
fitSeriesForCombinedScatter <- function(chart.matrix, x.labels, x.axis.type, fit.type,
    fit.ignore.last, fit.CI.show, fit.window.size, fit.line.name, fit.line.colors,
    fit.CI.colors, fit.CI.opacity)
{
    empty <- list(fit.x = NULL, fit.y = NULL, fit.group = NULL, fit.line.names = NULL,
                  fit.lower.bound = NULL, fit.upper.bound = NULL, fit.line.colors = NULL,
                  fit.ci.fill.colors = NULL, fit.ci.label.colors = NULL)
    if (fit.type == "None")
        return(empty)

    n.col <- ncol(chart.matrix)
    fit.x <- fit.y <- fit.lb <- fit.ub <- vector("list", n.col)
    for (i in 1:n.col)
    {
        tmp.fit <- fitSeries(x.labels, as.numeric(chart.matrix[,i]), fit.type,
                             fit.ignore.last, x.axis.type, fit.CI.show, fit.window.size)
        fit.x[[i]] <- tmp.fit$x
        fit.y[[i]] <- tmp.fit$y
        fit.lb[[i]] <- tmp.fit$lb
        fit.ub[[i]] <- tmp.fit$ub
    }
    has.ci <- fit.CI.show && !any(vapply(fit.lb, is.null, logical(1L)))
    list(fit.x = fit.x,
         fit.y = fit.y,
         fit.group = colnames(chart.matrix),
         fit.line.names = if (n.col == 1) fit.line.name
                          else sprintf("%s: %s", fit.line.name, colnames(chart.matrix)),
         fit.lower.bound = if (has.ci) fit.lb else NULL,
         fit.upper.bound = if (has.ci) fit.ub else NULL,
         fit.line.colors = fit.line.colors,
         fit.ci.fill.colors = if (has.ci) toRGB(fit.CI.colors, alpha = fit.CI.opacity) else NULL,
         fit.ci.label.colors = if (has.ci) fit.CI.colors else NULL)
}

# Arguments of Line() that rhtmlCombinedScatter has no equivalent for. Each entry is the
# value the argument has to hold for the chart to be unaffected; anything else is dropped.
UNSUPPORTED.BY.AUTO.PLACEMENT <- list(
    x.data.reversed = FALSE,
    y.data.reversed = FALSE,
    x.tick.marks = "",
    legend.ascending = NA,
    legend.fill.opacity = 0,
    legend.border.color = rgb(44, 44, 44, maxColorValue = 255),
    legend.border.line.width = 0,
    margin.inner.pad = NULL,
    hovertext.align = "left",
    modebar.show = FALSE,
    zoom.enable = TRUE,
    axis.drag.enable = FALSE)

#' Warns about arguments that automatic data label placement cannot honour
#'
#' @param args A named list of the evaluated arguments of \code{labeledLine}.
#' @param n.col The number of series being charted.
#' @param n.row The number of points in each series.
#' @noRd
warnUnsupportedByAutoPlacement <- function(args, n.col, n.row)
{
    # Compared after vectorizing, because a per-series setting may spell the default out
    # once per series ("Top, Top") or as a vector, and neither is a change worth warning
    # about. Only character settings can arrive per series; the rest are compared as they
    # are.
    ignored <- names(UNSUPPORTED.BY.AUTO.PLACEMENT)
    ignored <- ignored[vapply(ignored, function(nm)
    {
        given <- args[[nm]]
        default <- UNSUPPORTED.BY.AUTO.PLACEMENT[[nm]]
        # The identical() check comes first because vectorize() parses a character value as
        # comma-separated text: an empty string (e.g. x.tick.marks' default) splits to zero
        # tokens and recycles to NA instead of back to "", which would falsely count as a
        # change from the default.
        if (identical(given, default))
            return(FALSE)
        if (is.character(given) && is.character(default) && length(default) == 1)
            return(!all(tolower(vectorize(given, n.col)) == tolower(default)))
        TRUE
    }, logical(1L))]

    # A per-series opacity inherited by the markers is not listed here. One marker opacity for
    # the whole chart is the contract on both routes, so prepareLineSeries reports it for
    # either; naming it here as well would say it twice, and would promise that turning
    # automatic placement off gives the markers a per-series opacity, which it does not.
    # These default to another argument, so they can only be judged against it. The
    # widget takes one set of series colors, used for the markers and for automatically
    # colored data labels alike, so markers cannot be colored separately from the lines.
    if (!identical(args$marker.colors, args$colors))
        ignored <- c(ignored, "marker.colors")
    if (!identical(args$legend.fill.color, args$background.fill.color))
        ignored <- c(ignored, "legend.fill.color")

    if (length(ignored) > 0)
        warning("Automatic data label placement does not support ",
                ngettext(length(ignored), "the setting ", "the settings "),
                paste0("'", sort(ignored), "'", collapse = ", "),
                ". Turn off 'Automatically place data labels' to use ",
                ngettext(length(ignored), "it", "them"), ".")
    invisible(NULL)
}

#' Whether any marker is drawn, judged from the arguments as the caller passed them
#'
#' @inheritParams warnUnsupportedByAutoPlacement
#' @noRd
markersAreShown <- function(args, n.col, n.row)
{
    markersAreDrawn(args$marker.show, args$marker.show.at.ends,
                    args$marker.show.at.last.end, n.col, n.row)
}
