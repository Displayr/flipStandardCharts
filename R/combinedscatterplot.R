#' CombinedScatter
#'
#' Scatter plot (uses rhtmlCombinedScatter)
#' @inherit Scatter
#' @inherit LabeledScatter
#' @inherit SmallMultiples
#' @param scatter.groups A factor of the same length as \code{x} which is
#'  used to aggregate the data for small multiples.
#' @param scatter.groups.column The column of \code{x} which is used to aggregate
#'  the data for small multiples (ignored when \code{scatter.groups} is provided)
#' @param nrows Integer; Number of rows to arrange the small multiple panels.
#' @param share.axes Force range of the plot to be the same across all panels.
#' @param x.order A vector containing the list index of the columns in the order
#'  which they are to be shown, or a string with comma separated indices.
#' @param panel.x.gap A number between 0 and 1. Controls the horizontal space between panels.
#' @param panel.y.gap A number between 0 and 1. Controls the vertical space between panels.
#' @param legend.title Title to show above the legend (and color scale bar)
#' @param legend.title.font.color Font color of the legend (and color scale bar) title
#' @param legend.title.font.family Font family of the legend (and color scale bar) title
#' @param legend.title.font.size Font size of the legend (and color scale bar) title
#' @param legend.bubble.font.color Font color of the bubble legend
#' @param legend.bubble.font.family Font family of the bubble legend
#' @param legend.bubble.font.size Font size of the bubble legend
#' @param legend.bubble.title Title to show above the bubble legend
#' @param legend.bubble.title.font.color Font color of the bubble legend title
#' @param legend.bubble.title.font.family Font family of the bubble legend title
#' @param legend.bubble.title.font.size Font size of the bubble legend title
#' @param legend.show is the toggle to show the legend. Can be logical or "Automatic", "Show" or "Hide".
#'  When automatic, the legend is only shown when there is more than one group. Defaults to TRUE.
#'  When FALSE or "Hide", the colorscale and bubble legends are also hidden
#'  (if not overridden by their own "show" parameters).
#' @param color.scale.show is the toggle to show the color scale bar.
#' @param x.zero.line.dash Line type of x zero line. Can be one of 'Solid', 'Dot', 'Dash'.
#' @param x.grid.dash Line type of x grid line. Can be one of 'Solid', 'Dot', 'Dash'.
#' @param y.zero.line.dash Line type of y zero line. Can be one of 'Solid', 'Dot', 'Dash'.
#' @param y.grid.dash Line type of y grid line. Can be one of 'Solid', 'Dot', 'Dash'.
#' @importFrom rhtmlCombinedScatter CombinedScatter
#' @export
CombinedScatter <- function(x = NULL,
                            y = NULL,
                            scatter.x.column = 1,
                            scatter.y.column = 2,
                            scatter.labels = NULL,
                            scatter.labels.name = "",
                            scatter.sizes = NULL,
                            scatter.sizes.name = "",
                            scatter.sizes.column = 3,
                            scatter.sizes.as.diameter = FALSE,
                            scatter.colors = NULL,
                            scatter.colors.name = "",
                            scatter.colors.column = 4,
                            scatter.colors.as.categorical = TRUE,
                            scatter.groups = NULL,
                            scatter.groups.column = NULL,
                            scatter.labels.as.hovertext = TRUE,
                            scatter.max.labels = 50,
                            scatter.max.groups = 50,
                            annotation.list = NULL,
                            colors = ChartColors(12),
                            trend.lines = FALSE,
                            logos = NULL,
                            logo.size = 0.5,
                            fit.type = "None",
                            fit.window.size = 3,
                            fit.ignore.last = FALSE,
                            fit.line.type = "dot",
                            fit.line.width = 1,
                            fit.line.colors = colors,
                            fit.line.opacity = 1,
                            fit.CI.show = FALSE,
                            fit.CI.colors = fit.line.colors,
                            fit.CI.opacity = 0.4,
                            legend.show = TRUE,
                            legend.orientation = "Vertical",
                            legend.wrap = TRUE,
                            legend.wrap.nchar = 30,
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
                            panel.title.font.family = global.font.family,
                            panel.title.font.color = global.font.color,
                            panel.title.font.size = 14,
                            nrows = 2,
                            share.axes = TRUE,
                            x.order = NULL,
                            panel.x.gap = 0.2,
                            panel.y.gap = 0.3,
                            footer = "",
                            footer.font.family = global.font.family,
                            footer.font.color = global.font.color,
                            footer.font.size = 8,
                            footer.wrap = TRUE,
                            footer.wrap.nchar = 100,
                            data.label.font.family = global.font.family,
                            data.label.font.color = global.font.color,
                            data.label.font.autocolor = FALSE,
                            data.label.font.size = 10,
                            data.label.format = "",
                            data.label.prefix = "",
                            data.label.suffix = "",
                            opacity = NULL,
                            background.fill.color =  "transparent",
                            charting.area.fill.color =  background.fill.color,
                            legend.font.color = global.font.color,
                            legend.font.family = global.font.family,
                            legend.font.size = 10,
                            legend.position.y = 1,
                            legend.position.x = 1.02,
                            legend.title = "",
                            legend.title.font.color = global.font.color,
                            legend.title.font.family = global.font.family,
                            legend.title.font.size = 12,
                            legend.bubble.title = "",
                            legend.bubble.font.color = global.font.color,
                            legend.bubble.font.family = global.font.family,
                            legend.bubble.font.size = 10,
                            legend.bubble.title.font.color = global.font.color,
                            legend.bubble.title.font.family = global.font.family,
                            legend.bubble.title.font.size = 12,
                            margin.autoexpand = TRUE,
                            margin.top = NULL,
                            margin.bottom = NULL,
                            margin.left = NULL,
                            margin.right = NULL,
                            grid.show = TRUE,
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
                            y.zero.line.width = 0,
                            y.zero.line.color = rgb(225, 225, 225, maxColorValue = 255),
                            y.zero.line.dash = "Dash",
                            y.grid.width = 1 * grid.show,
                            y.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                            y.grid.dash = "Solid",
                            y.tick.show = TRUE,
                            y.tick.suffix = "",
                            y.tick.prefix = "",
                            y.tick.format = "",
                            y.hovertext.format = "",
                            y.tick.font.color = global.font.color,
                            y.tick.font.family = global.font.family,
                            y.tick.font.size = 10,
                            x.title = "",
                            x.title.font.color = global.font.color,
                            x.title.font.family = global.font.family,
                            x.title.font.size = 12,
                            x.line.width = 0,
                            x.line.color = rgb(0, 0, 0, maxColorValue = 255),
                            x.tick.mark.length = 3,
                            x.tick.mark.color = "transparent",
                            x.bounds.minimum = NULL,
                            x.bounds.maximum = NULL,
                            x.tick.distance = NULL,
                            x.tick.maxnum = NULL,
                            x.zero.line.width = 0,
                            x.zero.line.color = rgb(225, 225, 225, maxColorValue = 255),
                            x.zero.line.dash = "Dash",
                            x.grid.width = 1 * grid.show,
                            x.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                            x.grid.dash = "Solid",
                            x.tick.show = TRUE,
                            x.tick.suffix = "",
                            x.tick.prefix = "",
                            x.tick.format = "",
                            x.hovertext.format = "",
                            x.tick.angle = NULL,
                            x.tick.font.color = global.font.color,
                            x.tick.font.family = global.font.family,
                            x.tick.font.size = 10,
                            x.tick.label.wrap = TRUE,
                            x.tick.label.wrap.nchar = 21,
                            hovertext.font.family = global.font.family,
                            hovertext.font.size = 11,
                            marker.size = 6,
                            swap.x.and.y = FALSE,
                            legend.bubbles.show = NULL,
                            color.scale.show = NULL,
                            label.auto.placement = TRUE)
{
    orig.x <- x
    checkDataIsEnough(x, y)

    # Try to store name of variables
    scatter.mult.yvals <- isTRUE(attr(x, "scatter.mult.yvals"))
    if (!is.null(scatter.sizes) && !any(nzchar(scatter.sizes.name)))
        scatter.sizes.name <- deparse(substitute(scatter.sizes))
    if (!is.null(scatter.labels) && !any(nzchar(scatter.labels.name)))
        scatter.labels.name <- deparse(substitute(scatter.labels))
    if (!is.null(scatter.colors) && !any(nzchar(scatter.colors.name)))
        scatter.colors.name <- deparse(substitute(scatter.colors))

    num.tables <- 1
    if (is.list(x) && !is.null(ncol(x[[1]])))
    {
        output <- unlistX(x, trend.lines)
        x <- output$x
        scatter.colors <- output$groups
        num.tables <- output$num.tables
    }

    x <- convertPercentToProportion(x)
    annot.data <- x

    if (is.matrix(x) || is.data.frame(x))
    {
        output <- unpackColumnsFromX(x, y, scatter.labels, scatter.x.column,
                                     scatter.y.column, scatter.mult.yvals,
                                     x.title, y.title, scatter.sizes,
                                     scatter.sizes.column, scatter.sizes.name,
                                     scatter.colors, scatter.colors.column,
                                     scatter.colors.name, scatter.groups,
                                     scatter.groups.column)
        x <- output$x
        y <- output$y
        scatter.labels <- output$scatter.labels
        x.title <- output$x.title
        y.title <- output$y.title
        scatter.sizes <- output$scatter.sizes
        scatter.sizes.name <- output$scatter.sizes.name
        scatter.colors <- output$scatter.colors
        scatter.colors.name <- output$scatter.colors.name
        scatter.groups <- output$scatter.groups
    }

    scatter.groups <- reorderPanels(scatter.groups, x.order)

    if (is.null(x) && is.null(y))
        stop("At least one of x or y must be supplied.")

    # Warning if non-default selected but corresponding data is missing
    if (is.null(scatter.sizes) && scatter.sizes.as.diameter)
        warning("'Sizes' variable not provided.")
    if (!scatter.colors.as.categorical && is.null(scatter.colors))
    {
        warning("'Colors' variable not provided.")
        scatter.colors.as.categorical <- TRUE
    }
    if (!scatter.colors.as.categorical && length(colors) < 2)
    {
        warning("Supply a color palette of 2 or more colors to use a color scale")
        scatter.colors.as.categorical <- TRUE
    }
    qualitative.palettes <- c("Default colors", "Primary colors",
        "Light colors", "Strong colors", "Colorblind safe colors")
    if (!scatter.colors.as.categorical && !is.null(attr(colors, "palette.type"))
        && attr(colors, "palette.type") %in% qualitative.palettes)
        warning("For a numeric 'colors' variable, a qualitative palette should not be used. The colorscale is created by interpolating the colors.")

    if (scatter.colors.as.categorical && length(unique(scatter.colors)) > scatter.max.groups)
    {
        warning("The colors variable has been treated as a numeric scale because there ",
                "are more than ", scatter.max.groups, " categories and would be slow to render")
        scatter.colors.as.categorical <- FALSE
    }

    if (is.null(x))
    {
        x <- rep(0, length(y))
    }
    if (is.null(y))
    {
        y <- rep(0, length(x))
    }
    n <- length(x)

    if (!any(is.finite(scatter.max.labels)) || scatter.max.labels < 0)
        scatter.max.labels <- NULL

    scatter.labels <- processScatterLabels(scatter.labels, x, data.label.format,
                                           data.label.prefix, data.label.suffix,
                                           scatter.max.labels, scatter.labels.as.hovertext)

    if (swap.x.and.y)
    {
        tmp <- x
        x <- y
        y <- tmp
        tmp <- x.title
        x.title <- y.title
        y.title <- tmp
    }

    if (any(duplicated(cbind(x, y))))
        warning("Chart contains overlapping points in the same position.")

    if (is.null(marker.size) || is.na(marker.size))
        marker.size <- 6

    not.na <- nonMissing(x, y)

    scatter.sizes <- processScatterSizes(scatter.sizes, n)

    if (!is.null(scatter.sizes) && any(!is.finite(scatter.sizes)))
    {
        warning("Some points omitted due to missing values in 'scatter.sizes'.")
        not.na <- intersect(not.na, which(is.finite(scatter.sizes)))
    }
    if (!is.null(scatter.groups)) {
        not.na <- intersect(not.na, which(!is.na(scatter.groups)))
    }

    opacity <- getOpacity(opacity, scatter.sizes, fit.type)

    if (is.null(color.scale.show)) {
        color.scale.show <- !isFALSE(legend.show) && legend.show != "Hide"
    }
    if (is.null(legend.bubbles.show)) {
        legend.bubbles.show <- !isFALSE(legend.show) && legend.show != "Hide"
    }

    output <- getColors(scatter.groups, scatter.colors, colors, n, not.na,
                        scatter.colors.as.categorical, num.tables, legend.show)
    colors <- output$colors
    scatter.colors <- output$scatter.colors
    legend.show <- output$legend.show
    not.na <- output$not.na

    if (is.na(data.label.font.autocolor)) {
        data.label.font.autocolor <- length(unique(scatter.colors[not.na])) > 1
    }

    logo.urls <- getLogoUrls(logos, orig.x, scatter.labels, n)
    labels.or.logos <- if (!is.null(logo.urls)) logo.urls else scatter.labels
    logo.size <- rep(logo.size, n)

    if (any(nzchar(footer)) && footer != " ")
        footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate=FALSE)

    # Convert axis to the appropriate type based on axis values and tick format
    # Give warning where possible
    x.axis.type <- getAxisType(x[not.na], x.tick.format)
    x.tick.format <- checkD3Format(x.tick.format, x.axis.type, "X axis")
    x <- convertAxis(x, x.axis.type)
    y.axis.type <- getAxisType(y[not.na], y.tick.format)
    y.tick.format <- checkD3Format(y.tick.format, y.axis.type, "Y axis")
    y <- convertAxis(y, y.axis.type)
    x.bounds.units.major <- getAxisBoundsUnitsMajor(x.tick.distance,
                                                    x.tick.maxnum,
                                                    x.bounds.maximum,
                                                    x.bounds.minimum, x)
    y.bounds.units.major <- getAxisBoundsUnitsMajor(y.tick.distance,
                                                    y.tick.maxnum,
                                                    y.bounds.maximum,
                                                    y.bounds.minimum, y)

    tooltips.text <- getTooltipsText(scatter.labels, not.na, x, y, x.tick.format,
                                     x.tick.prefix, x.tick.suffix, y.tick.format,
                                     y.tick.prefix, y.tick.suffix, scatter.sizes,
                                     scatter.sizes.name, scatter.colors,
                                     scatter.colors.name)

    if (length(not.na) < n)
        warning("Data points with missing values have been omitted.")

    fit <- list()
    if (fit.type != "None") {
        x.axis.type <- getAxisType(unique(x[not.na]), x.tick.format)
        fit <- fitLines(scatter.colors, scatter.colors.as.categorical, scatter.groups,
                        x, y, not.na, fit.type, fit.ignore.last,
                        fit.CI.show, fit.window.size, colors, fit.line.colors,
                        fit.CI.colors, fit.CI.opacity, x.axis.type)
    }

    annotations <- processAnnotations(annotation.list, n, annot.data,
                                      labels.or.logos,
                                      !scatter.labels.as.hovertext,
                                      !is.null(scatter.groups),
                                      if (!scatter.colors.as.categorical) NULL else scatter.colors,
                                      marker.size, not.na)

    scatter.sizes <- if (is.null(scatter.sizes)) NULL else abs(scatter.sizes)
    x.axis.font.color <- if (!is.null(x.tick.font.color)) x.tick.font.color else "#2C2C2C"
    y.axis.font.color <- if (!is.null(y.tick.font.color)) y.tick.font.color else "#2C2C2C"
    labels.font.color <- if (data.label.font.autocolor) NULL else data.label.font.color
    color.scale <- NULL
    if (!scatter.colors.as.categorical)
        color.scale <- unique(colors) # undo possible recycling of colors by PrepareData
    if (!any(nzchar(legend.title)) && !is.null(scatter.colors))
        legend.title = scatter.colors.name
    if (!any(nzchar(legend.bubble.title)) && !is.null(scatter.sizes))
        legend.bubble.title = scatter.sizes.name

    p <- rhtmlCombinedScatter::CombinedScatter(
        X = x[not.na],
        Y = y[not.na],
        Z = scatter.sizes[not.na],
        group = scatter.colors[not.na],
        panels = scatter.groups[not.na],
        x.levels = levels(x),
        y.levels = levels(y),
        colors = colors,
        color.scale = color.scale,
        color.scale.show = color.scale.show,
        color.transparency = opacity,
        color.scale.title = legend.title,
        color.scale.title.font.color = legend.title.font.color,
        color.scale.title.font.family = legend.title.font.family,
        color.scale.title.font.size = legend.title.font.size,
        label = annotations$labels.or.logos[not.na],
        label.alt = scatter.labels[not.na],
        grid = grid.show,
        labels.show = !scatter.labels.as.hovertext,
        labels.max.shown = scatter.max.labels,
        label.auto.placement = label.auto.placement,
        legend.show = legend.show,
        legend.bubbles.show = legend.bubbles.show,
        legend.font.color = legend.font.color,
        legend.font.family = legend.font.family,
        legend.font.size = legend.font.size,
        legend.title = legend.title,
        legend.title.font.color = legend.title.font.color,
        legend.title.font.family = legend.title.font.family,
        legend.title.font.size = legend.title.font.size,
        legend.bubble.font.color = legend.bubble.font.color,
        legend.bubble.font.family = legend.bubble.font.family,
        legend.bubble.font.size = legend.bubble.font.size,
        legend.bubble.title.font.color = legend.bubble.title.font.color,
        legend.bubble.title.font.family = legend.bubble.title.font.family,
        legend.bubble.title.font.size = legend.bubble.title.font.size,
        legend.x = legend.position.x,
        legend.y = legend.position.y,
        legend.wrap = legend.wrap,
        legend.wrap.n.char = legend.wrap.nchar,
        legend.orientation = legend.orientation,
        margin.autoexpand = margin.autoexpand,
        margin.top = margin.top,
        margin.bottom = margin.bottom,
        margin.left = margin.left,
        margin.right = margin.right,
        y.title = y.title,
        y.title.font.family = y.title.font.family,
        y.title.font.color = y.title.font.color,
        y.title.font.size = y.title.font.size,
        subtitle = subtitle,
        subtitle.font.family = subtitle.font.family,
        subtitle.font.color = subtitle.font.color,
        subtitle.font.size = subtitle.font.size,
        footer = footer,
        footer.font.family = footer.font.family,
        footer.font.color = footer.font.color,
        footer.font.size = footer.font.size,
        x.axis.font.family = x.tick.font.family,
        x.axis.font.color = x.axis.font.color,
        x.axis.font.size = x.tick.font.size,
        x.axis.tick.length = x.tick.mark.length,
        x.axis.tick.color = x.tick.mark.color,
        x.axis.tick.angle = x.tick.angle,
        x.axis.line.width = x.line.width,
        x.axis.line.color = x.line.color,
        x.axis.zero.line.width = x.zero.line.width,
        x.axis.zero.line.color = x.zero.line.color,
        x.axis.zero.line.dash = tolower(x.zero.line.dash),
        x.axis.grid.width = x.grid.width,
        x.axis.grid.color = x.grid.color,
        x.axis.grid.dash = tolower(x.grid.dash),
        x.axis.label.wrap = x.tick.label.wrap,
        x.axis.label.wrap.n.char = x.tick.label.wrap.nchar,
        y.axis.font.family = y.tick.font.family,
        y.axis.font.color = y.axis.font.color,
        y.axis.font.size = y.tick.font.size,
        y.axis.tick.length = y.tick.mark.length,
        y.axis.tick.color = y.tick.mark.color,
        y.axis.line.width = y.line.width,
        y.axis.line.color = y.line.color,
        y.axis.zero.line.width = y.zero.line.width,
        y.axis.zero.line.color = y.zero.line.color,
        y.axis.zero.line.dash = tolower(y.zero.line.dash),
        y.axis.grid.width = y.grid.width,
        y.axis.grid.color = y.grid.color,
        y.axis.grid.dash = tolower(y.grid.dash),
        x.title = x.title,
        x.title.font.family = x.title.font.family,
        x.title.font.color = x.title.font.color,
        x.title.font.size = x.title.font.size,
        z.title = legend.bubble.title,
        x.format = x.tick.format,
        y.format = y.tick.format,
        x.hover.format = x.hovertext.format,
        y.hover.format = x.hovertext.format,
        x.prefix = x.tick.prefix,
        y.prefix = y.tick.prefix,
        x.suffix = x.tick.suffix,
        y.suffix = y.tick.suffix,
        title.font.family = title.font.family,
        title.font.color = title.font.color,
        title.font.size = title.font.size,
        labels.font.family = data.label.font.family,
        labels.font.color = labels.font.color,
        labels.font.size = data.label.font.size,
        panel.title.font.family = panel.title.font.family,
        panel.title.font.color = panel.title.font.color,
        panel.title.font.size = panel.title.font.size,
        panel.num.rows = nrows,
        panel.share.axes = share.axes,
        panel.x.gap = panel.x.gap,
        panel.y.gap = panel.y.gap,
        point.radius = 0.5 * marker.size,
        y.bounds.maximum = charToNumeric(y.bounds.maximum),
        y.bounds.minimum = charToNumeric(y.bounds.minimum),
        y.bounds.units.major = y.bounds.units.major,
        x.bounds.maximum = charToNumeric(x.bounds.maximum),
        x.bounds.minimum = charToNumeric(x.bounds.minimum),
        x.bounds.units.major = x.bounds.units.major,
        y.axis.show = y.tick.show,
        x.axis.show = x.tick.show,
        origin = TRUE,
        tooltip.font.family = hovertext.font.family,
        tooltip.font.size = hovertext.font.size,
        tooltip.text = tooltips.text,
        title = title,
        trend.lines.show = trend.lines,
        fit.x = fit$fit.x,
        fit.y = fit$fit.y,
        fit.group = fit$fit.group,
        fit.panel = fit$fit.panel,
        fit.lower.bound = if (fit.CI.show) fit$fit.lower.bound else NULL,
        fit.upper.bound = if (fit.CI.show) fit$fit.upper.bound else NULL,
        fit.line.names = fit$fit.line.names,
        fit.line.type = fit.line.type,
        fit.line.width = fit.line.width,
        fit.line.opacity = fit.line.opacity,
        fit.line.colors = fit$fit.line.colors,
        fit.ci.colors = fit$fit.ci.fill.colors,
        fit.ci.label.colors = fit$fit.ci.label.colors,
        marker.annotations = annotations$marker.annotations[not.na],
        pre.label.annotations = annotations$pre.label.annotations[not.na],
        post.label.annotations = annotations$post.label.annotations[not.na],
        point.border.color = annotations$point.border.color[not.na],
        point.border.width = annotations$point.border.width[not.na],
        labels.logo.scale = logo.size,
        background.color = background.fill.color,
        bubble.sizes.as.diameter = scatter.sizes.as.diameter,
        debug.mode = grepl("DEBUG_MODE_ON", title))

    result <- list(htmlwidget = p)
    class(result) <- "StandardChart"
    attr(result, "ChartType") <- chartType(scatter.sizes)
    attr(result, "ChartLabels") <- chartLabels(annotations$ppt.chart.labels, x.title, y.title)
    attr(result, "CustomPoints") <- annotations$ppt.custom.points
    result
}

checkDataIsEnough <- function(x, y) {
    if (!is.null(y))
        ErrorIfNotEnoughData(cbind(x, y))
    else
        ErrorIfNotEnoughData(x, require.tidy = FALSE)
}

checkNotNa <- function(not.na) {
    if (length(not.na) == 0)
        stop("No non-NA points to plot.")
}

convertPercentToProportion <- function(x) {
    tmp.stat <- attr(x, "statistic")
    if ((is.array(x) || is.numeric(x)) && isTRUE(grepl("%", tmp.stat)))
    {
        x <- x/100
        attr(x, "statistic") <- NULL
    }
    x
}

# Assume X is a list of tables, where each table has the same rownames
unlistX <- function(x, trend.lines) {
    num.tables <- length(x)
    n.tmp <- nrow(x[[1]])
    table.names <- unlist(lapply(1:num.tables,
                                 function(ii){res <- attr(x[[ii]], "name"); if (is.null(res)) res <- ii;
                                 return(as.character(res))}))
    x <- checkTableList(x, trend.lines)
    groups <- rep(rownames(x[[1]]), num.tables)
    x <- do.call(rbind, x)
    if (!trend.lines)
        rownames(x) <- sprintf("%s: %s",
                               rep(table.names, each = n.tmp), groups)
    list(x = x, groups = groups, num.tables = num.tables)
}

unpackColumnsFromX <- function(x, y, scatter.labels, scatter.x.column,
                               scatter.y.column, scatter.mult.yvals, x.title,
                               y.title, scatter.sizes, scatter.sizes.column,
                               scatter.sizes.name, scatter.colors,
                               scatter.colors.column, scatter.colors.name,
                               scatter.groups, scatter.groups.column) {
    .isValidColumnIndex <- function(n) {return (!is.null(n) && !is.na(n) && n > 0 && n <= ncol(x))}
    if (is.null(scatter.labels) && !is.null(rownames(x)))
        scatter.labels <- rownames(x)
    if (is.null(y) && .isValidColumnIndex(scatter.y.column))
    {
        if (!any(nzchar(y.title)) && !is.null(colnames(x)) && !scatter.mult.yvals)
            y.title <- colnames(x)[scatter.y.column]
        y <- x[,scatter.y.column]
    }
    if (is.null(scatter.sizes) && .isValidColumnIndex(scatter.sizes.column))
    {
        if (!any(nzchar(scatter.sizes.name)) && !is.null(colnames(x)))
            scatter.sizes.name <- colnames(x)[scatter.sizes.column]
        scatter.sizes.name <- trimws(scatter.sizes.name)
        scatter.sizes <- x[,scatter.sizes.column]
    }
    if (is.null(scatter.colors) && .isValidColumnIndex(scatter.colors.column))
    {
        if (!any(nzchar(scatter.colors.name)) || nchar(scatter.colors.name) == 0)
            scatter.colors.name <- colnames(x)[scatter.colors.column]
        scatter.colors.name <- trimws(scatter.colors.name)
        scatter.colors <- x[,scatter.colors.column]
    }
    if (is.null(scatter.groups) && .isValidColumnIndex(scatter.groups.column))
    {
        scatter.groups <- x[,scatter.groups.column]
    }
    if (!any(nzchar(x.title)) && (!is.null(colnames(x))) &&
        .isValidColumnIndex(scatter.x.column) && !scatter.mult.yvals)
        x.title <- colnames(x)[scatter.x.column]
    if (!.isValidColumnIndex(scatter.x.column))
        x <- NULL
    else
        x <- x[,scatter.x.column]

    list(x = x, y = y, scatter.labels = scatter.labels, x.title = x.title,
         y.title = y.title, scatter.sizes = scatter.sizes,
         scatter.sizes.name = scatter.sizes.name,
         scatter.colors = scatter.colors,
         scatter.colors.name = scatter.colors.name,
         scatter.groups = scatter.groups
    )
}

nonMissing <- function(x, y) {
    x.not.na <- if (is.numeric(x)) is.finite(x) else !is.na(x)
    y.not.na <- if (is.numeric(y)) is.finite(y) else !is.na(y)
    which(x.not.na & y.not.na)
}

processScatterSizes <- function(scatter.sizes, n) {
    if (is.null(scatter.sizes)) {
        return(scatter.sizes)
    }
    if (length(scatter.sizes) != n)
        stop("'scatter.sizes' should be a numeric vector with the same number of observations as 'x'.")
    sz.tmp <- AsNumeric(scatter.sizes, binary = FALSE)
    if (any(class(scatter.sizes) %in% c("Date", "POSIXct", "POSIXt"))) {
        sz.tmp <- sz.tmp - min(sz.tmp, na.rm = TRUE)
    }
    sz.tmp
}

getOpacity <- function(opacity, scatter.sizes, fit.type) {
    if (is.null(opacity)) {
        if (!is.null(scatter.sizes) || fit.type != "None") {
            opacity <- 0.4
        } else {
            opacity <- 1
        }
    }
    opacity
}

#' @importFrom flipChartBasics StripAlphaChannel
getColors <- function(scatter.groups, scatter.colors, colors, n, not.na,
                      scatter.colors.as.categorical, num.tables, legend.show)
{
    # Don't show legend if there is only one series in each panel
    if (!is.null(scatter.groups) && !is.null(scatter.colors) && scatter.colors.as.categorical) {
        r.groups <- rle(as.numeric(as.factor(scatter.groups)))$lengths
        r.colors <- rle(as.numeric(as.factor(scatter.colors)))$lengths
        if (length(r.groups) == length(r.colors) && all(r.groups == r.colors) && legend.show == "Automatic")
            legend.show <- FALSE
    } else if (is.null(scatter.colors) && !is.null(scatter.groups) && scatter.colors.as.categorical) {
        scatter.colors <- scatter.groups
        if (legend.show == "Automatic")
            legend.show <- FALSE
    }

    if (!is.null(scatter.colors))
    {
        if (length(scatter.colors) != n)
            stop("'scatter.colors' should be a vector with the same number of observations as 'x'.")
        if (any(is.na(scatter.colors)))
        {
            warning("Some points omitted due to missing values in 'scatter.colors'")
            not.na <- intersect(not.na, which(is.finite(scatter.colors)))
        }
    }

    # Determine color for each observation
    if (!is.null(scatter.colors) && !scatter.colors.as.categorical)
    {
        if (num.tables > 1)
            stop("'scatter.colors' cannot be used with multiple tables")
        legend.show <- FALSE # don't need to worry about order of groups
        colors <- StripAlphaChannel(colors, "Alpha values in selected colors were not used in the numeric color scale. Adjust 'opacity' for transparent points instead")
    }
    # Reorder data to make sure legend is ordered correctly
    if (!is.null(scatter.colors) && scatter.colors.as.categorical)
    {
        groups.ord <- order(suppressWarnings(AsNumeric(scatter.colors[not.na], binary = FALSE)))
        not.na <- not.na[groups.ord]
    }

    list(colors = colors, scatter.colors = scatter.colors,
         legend.show = legend.show, not.na = not.na)
}

#' @importFrom flipFormat FormatAsReal FormatAsPercent
processScatterLabels <- function(scatter.labels, x, data.label.format,
                                 data.label.prefix, data.label.suffix,
                                 scatter.max.labels, scatter.labels.as.hovertext) {
    if (is.null(scatter.labels) && !is.null(names(x)))
        scatter.labels <- names(x)

    if (is.null(scatter.labels))
        scatter.labels <- rep("", length(x))
    if (is.numeric(scatter.labels)) {
        if (percentFromD3(data.label.format))
            scatter.labels <- FormatAsPercent(scatter.labels, decimals = decimalsFromD3(data.label.format))
        else
            scatter.labels <- FormatAsReal(scatter.labels, decimals = decimalsFromD3(data.label.format))
    }
    scatter.labels <- paste0(data.label.prefix, scatter.labels, data.label.suffix)

    if (!scatter.labels.as.hovertext && any(is.finite(scatter.max.labels)) && length(scatter.labels) > scatter.max.labels)
    {
        if (scatter.max.labels == 50)
            warning("By default, only the first 50 labels are shown to avoid long running times. Adjust 'Maximum data labels to plot' to show more labels. Alternatively, to show a large number of points, show as 'Hovertext' instead.")
        else
            warning("Some labels have been hidden. Adjust 'Maximum data labels to plot' to show more labels by default. ",
                    "Labels can also be toggled on and off by clicking on the markers.")
    }
    scatter.labels
}

getLogoUrls <- function(logos, x, scatter.labels, n) {
    logo.urls <- NULL
    if (!is.null(logos) && any(nzchar(logos)))
    {
        logo.urls <- try(TextAsVector(logos))
        if (inherits(logo.urls, "try-error"))
            logo.urls <- NULL
    }
    if (is.list(x) && !is.null(ncol(x[[1]])))
    {
        num.tables <- length(x)
        if (!is.null(logo.urls))
            logo.urls <- rep(logo.urls, num.tables)
    }
    empty.logo <- which(nchar(logo.urls) == 0)
    if (length(empty.logo) > 0)
        logo.urls[empty.logo] <- scatter.labels[empty.logo]
    if (!is.null(logo.urls) && length(logo.urls) < n)
        logo.urls <- c(logo.urls, scatter.labels[(length(logo.urls)+1):n])

    logo.urls
}

isEmptyName <- function(x) {
    !any(nzchar(trimws(x)))
}

getAxisBoundsUnitsMajor <- function(tick.distance, tick.maxnum, bounds.maximum,
                                    bounds.minimum, values) {
    result <- charToNumeric(tick.distance)
    if (is.null(result) && !is.null(tick.maxnum))
        result <- calcUnitsForMaxNum(tick.maxnum, bounds.maximum,
                                     bounds.minimum, values)
    result
}


getTooltipsText <- function(scatter.labels, not.na, x, y, x.tick.format,
                            x.tick.prefix, x.tick.suffix, y.tick.format,
                            y.tick.prefix, y.tick.suffix, scatter.sizes,
                            scatter.sizes.name, scatter.colors,
                            scatter.colors.name) {
    tooltips.text <- sprintf("%s (%s, %s)", scatter.labels[not.na],
                             formatByD3(x[not.na], x.tick.format, x.tick.prefix, x.tick.suffix),
                             formatByD3(y[not.na], y.tick.format, y.tick.prefix, y.tick.suffix))
    if (!isEmptyName(scatter.sizes.name))
        tooltips.text <- sprintf("%s\n%s: %s", tooltips.text, scatter.sizes.name,
                                 formatByD3(scatter.sizes[not.na], ""))
    if (!isEmptyName(scatter.colors.name))
        tooltips.text <- sprintf("%s\n%s: %s", tooltips.text, scatter.colors.name,
                                 formatByD3(scatter.colors[not.na], ""))
    tooltips.text
}

chartType <- function(scatter.sizes) {
    if (!is.null(scatter.sizes))
        "Bubble"
    else
        "X Y Scatter"
}

chartLabels <- function(chart.labels, x.title, y.title) {
    if (!any(nzchar(x.title)) && !any(nzchar(y.title))) {
        return(chart.labels)
    }
    if (any(nzchar(x.title))) {
        chart.labels$PrimaryAxisTitle <- x.title
    }
    if (any(nzchar(y.title))) {
        chart.labels$ValueAxisTitle <- y.title
    }
    chart.labels
}

fitLines <- function(scatter.colors, scatter.colors.as.categorical, scatter.groups,
                     x, y, not.na, fit.type, fit.ignore.last,
                     fit.CI.show, fit.window.size, colors, input.fit.line.colors,
                     fit.CI.colors, fit.CI.opacity, x.axis.type) {
    n <- length(x)
    # "groups" is the vector of the colors of the scatter points.
    # Not to be confused with "scatter.groups",
    # which is the vector of the panels of the scatter points.
    groups <- scatter.colors
    if (is.null(groups) || !scatter.colors.as.categorical)
        groups <- rep(" ", n)

    g.list <- extractUniqueValues(groups, not.na)

    num.groups <- length(g.list)
    num.panels <- if (is.null(scatter.groups)) 1
                  else length(unique(scatter.groups))
    num.lines <- num.groups * num.panels

    fit.x <- vector("list", num.lines)
    fit.y <- vector("list", num.lines)
    fit.group <- character(num.lines)
    fit.panel <- integer(num.lines)
    fit.lower.bound <- vector("list", num.lines)
    fit.upper.bound <- vector("list", num.lines)
    fit.line.names <- character(num.lines)
    fit.line.colors <- character(num.lines)
    fit.ci.fill.colors <- character(num.lines)
    fit.ci.label.colors <- character(num.lines)

    if (is.null(input.fit.line.colors))
        input.fit.line.colors <- colors
    if (is.null(fit.CI.colors))
        fit.CI.colors <- input.fit.line.colors
    if (is.null(fit.CI.opacity))
        fit.CI.opacity <- 0.5

    p.list <- if (!is.null(scatter.groups))
        extractUniqueValues(scatter.groups, not.na)
    else
        NULL

    j <- 1
    for (p in 1:num.panels)
    {
        p.index <- if (is.null(scatter.groups)) 1:n
                   else which(scatter.groups == p.list[p])
        p.index <- intersect(p.index, not.na)
        for (ggi in 1:num.groups)
        {
            ind <- intersect(which(groups == g.list[ggi]), p.index)
            if (length(ind) == 0)
                next
            fit <- fitSeries(x[ind], y[ind], fit.type, fit.ignore.last, x.axis.type,
                             fit.CI.show, fit.window.size)
            fit.x[[j]] <- fit$x
            fit.y[[j]] <- fit$y
            fit.group[j] <- g.list[ggi]
            fit.panel[j] <- p - 1
            fit.lower.bound[[j]] <- fit$lb
            fit.upper.bound[[j]] <- fit$ub
            fit.line.names[j] <- paste0("Fitted: ", g.list[ggi])
            fit.line.colors[j] <- input.fit.line.colors[ggi]
            fit.ci.fill.colors[j] <- toRGB(fit.CI.colors[ggi], alpha = fit.CI.opacity)
            fit.ci.label.colors[j] <- fit.CI.colors[ggi]
            j <- j + 1
        }
    }
    list(fit.x = fit.x, fit.y = fit.y, fit.group = fit.group,
         fit.panel = fit.panel, fit.lower.bound = fit.lower.bound,
         fit.upper.bound = fit.upper.bound, fit.line.names = fit.line.names,
         fit.line.colors = fit.line.colors,
         fit.ci.fill.colors = fit.ci.fill.colors,
         fit.ci.label.colors = fit.ci.label.colors)
}

extractUniqueValues <- function(groups, not.na)
{
    if (is.factor(groups))
    {
        lvls <- levels(groups)
        return(lvls[lvls %in% groups[not.na]])
    }
    else if (any(class(groups) %in% c("Date", "POSIXct", "POSIXt", "integer", "numeric")))
        return(sort(unique(groups[not.na])))
    else
        return(unique(groups[not.na]))
}

processAnnotations <- function(annotation.list, n, annot.data, labels.or.logos,
                               data.label.show, is.small.multiples, groups,
                               marker.size, not.na) {

    # Annotations need to be separated out by series (i.e. groups) for PPT exporting
    if (is.null(groups))
        groups <- rep(" ", n)
    g.list <- extractUniqueValues(groups, not.na)
    num.groups <- length(g.list)

    # Initialise settings to return
    marker.annotations <- character(n)
    pre.label.annotations <- character(n)
    post.label.annotations <- character(n)
    point.border.color <- character(n)
    point.border.width <- numeric(n)
    ppt.custom.points <- vector(mode = "list", length = num.groups)
    ppt.chart.labels <- list(SeriesLabels = vector(mode = "list", length = num.groups))

    for (ggi in 1:num.groups)
    {
        ind.group <- which(groups == g.list[ggi])
        if (length(ind.group) == 0)
            next

        ppt.chart.labels$SeriesLabels[[ggi]] <- list(ShowValue = data.label.show)
        pt.segs <- lapply(ind.group,
            function(ii)
            {
                pt <- list(Index = ii-1)
                if (data.label.show)
                    pt$Segments <-  list(list(Field="Value"))
                else
                    pt$Segments <- list()
                return(pt)
            }
        )
        custom.pts <- vector(mode = "list", length = length(ind.group))

        # Traces for annotation need to occur before main trace to avoid hiding hover info
        annot.text <- rep("", length(ind.group))
        has.text.annot <- FALSE
        for (j in seq_along(annotation.list))
        {
            if (!checkAnnotType(annotation.list[[j]]$type, "Scatter"))
                next
            a.tmp <- annotation.list[[j]]
            tmp.dat <- getAnnotScatterData(annot.data, a.tmp$data, ind.group)
            a.tmp$threshold <- ParseText(a.tmp$threshold, tmp.dat)
            ind.sel <- if (is.null(a.tmp$threstype) || is.null(a.tmp$threshold))    1:length(tmp.dat)
                       else if (is.factor(tmp.dat) && !is.ordered(tmp.dat))         selectFactor(a.tmp$threshold, 1:length(tmp.dat), a.tmp$data, ggi)
                       else if (a.tmp$threstype == "above threshold")               which(tmp.dat > a.tmp$threshold)
                       else if (a.tmp$threstype == "below threshold")               which(tmp.dat < a.tmp$threshold)
                       else                                                         which(is.na(tmp.dat))

            if (length(ind.sel) == 0)
                next
            ind.sel.global <- ind.group[ind.sel] # get index wrt full data set

            if (a.tmp$type == "Marker border") {
                point.border.color[ind.sel.global] <- a.tmp$color
                point.border.width[ind.sel.global] <- a.tmp$width
                for (ii in ind.sel)
                    custom.pts[[ii]] <- list(Index = ind.group[ii] - 1,
                        OutlineColor = a.tmp$color, OutlineWidth = a.tmp$width,
                        OutlineStyle = "Solid", Style = "Circle", Size = marker.size) # required for PPT to show properly
            } else if (!data.label.show) {
                annot.text <- addAnnotToDataLabel("", a.tmp, tmp.dat[ind.sel], tspan = FALSE)
                # Remove </span> (7 characters)
                annot.text.prefix <- substr(annot.text, 1, nchar(annot.text) - 7)
                if (a.tmp$type == "Shadow" || a.tmp$type == "Border") {
                    marker.annotations[ind.sel.global] <- paste0(annot.text.prefix, marker.annotations[ind.sel.global], "</span>")
                } else if (a.tmp$type == "Text - before data label") {
                    marker.annotations[ind.sel.global] <- paste0(annot.text.prefix, marker.annotations[ind.sel.global])
                } else if (a.tmp$type == "Hide") {
                    marker.annotations[ind.sel.global] <- ""
                } else {
                    marker.annotations[ind.sel.global] <- paste0(marker.annotations[ind.sel.global], annot.text)
                }
                has.text.annot <- TRUE
                pt.segs <- getPointSegmentsForPPT(pt.segs, ind.sel, a.tmp, tmp.dat[ind.sel])
            } else {
                annot.text <- addAnnotToDataLabel("", a.tmp, tmp.dat[ind.sel], tspan = !is.small.multiples)
                close.span = if (is.small.multiples) "</span>" else "</tspan>"
                annot.text.prefix <- substr(annot.text, 1, nchar(annot.text) - nchar(close.span))
                if (a.tmp$type == "Shadow" || a.tmp$type == "Border") {
                    pre.label.annotations[ind.sel.global] <- paste0(pre.label.annotations[ind.sel.global], annot.text.prefix)
                    post.label.annotations[ind.sel.global] <- paste0(post.label.annotations[ind.sel.global], close.span)
                } else if (a.tmp$type == "Text - before data label") {
                    pre.label.annotations[ind.sel.global] <- paste0(pre.label.annotations[ind.sel.global], annot.text.prefix)
                } else if (a.tmp$type == "Hide") {
                    pre.label.annotations[ind.sel.global] <- ""
                    post.label.annotations[ind.sel.global] <- ""
                    labels.or.logos[ind.sel.global] <- ""
                } else {
                    post.label.annotations[ind.sel.global] <- paste0(post.label.annotations[ind.sel.global], annot.text)
                }
                has.text.annot <- TRUE
                pt.segs <- getPointSegmentsForPPT(pt.segs, ind.sel, a.tmp, tmp.dat[ind.sel])
            }
        }

        # Clean up PPT chart labels
        pt.segs <- tidyPointSegments(pt.segs, length(ind.group), index.map = ind.group, toggle.show.value = !has.text.annot)
        if (has.text.annot)
        {
            # Where labels and text annotations are both present, we need to set
            # SeriesLabels$ShowValue to FALSE to make PPT use the Segments.
            # But doing this means the row labels get lost so manually convert label to a text segment
            if (ppt.chart.labels$SeriesLabels[[ggi]]$ShowValue)
            {
                for (ii in 1:length(pt.segs))
                {
                    if (is.null(pt.segs[[ii]]$Segments) && isTRUE(pt.segs[[ii]]$ShowValue)) {
                        pt.segs[[ii]]$ShowValue <- FALSE # Avoid double-up in case this starts working again
                        pt.segs[[ii]]$Segments <- list(list(Text = labels.or.logos[pt.segs[[ii]]$Index + 1]))
                    }
                    else if (length(pt.segs[[ii]]$Segments) > 0)
                    {
                        for (j in 1:length(pt.segs[[ii]]$Segments))
                            if (!is.null(pt.segs[[ii]]$Segments[[j]]$Field) && pt.segs[[ii]]$Segments[[j]]$Field == "Value")
                            {
                                pt.segs[[ii]]$Segments[[j]] <- list(Text = labels.or.logos[pt.segs[[ii]]$Index + 1])
                                break
                            }
                    }

                }
                ppt.chart.labels$SeriesLabels[[ggi]]$ShowValue <- FALSE
            }
        }
        else if (isTRUE(attr(pt.segs, "SeriesShowValue")))
        {
            ppt.chart.labels$SeriesLabels[[ggi]]$ShowValue <- TRUE
            attr(pt.segs, "SeriesShowValue") <- NULL
        }
        if (length(pt.segs) > 0)
            ppt.chart.labels$SeriesLabels[[ggi]]$CustomPoints <- pt.segs
        if (any(sapply(custom.pts, Negate(is.null))))
            ppt.custom.points[[ggi]] <- custom.pts  # If there are any marker borders keep whole series to make merging easier

    }
    list(marker.annotations = marker.annotations,
         pre.label.annotations = pre.label.annotations,
         post.label.annotations = post.label.annotations,
         point.border.color = point.border.color,
         point.border.width = point.border.width,
         labels.or.logos = labels.or.logos,
         ppt.chart.labels = ppt.chart.labels,
         ppt.custom.points = ppt.custom.points
    )
}

#' @importFrom flipTransformations TextAsVector
reorderPanels <- function(scatter.groups, x.order) {
    if (is.null(scatter.groups) || is.null(x.order) || trimws(x.order) == "") {
        return(scatter.groups)
    }
    n.panels <- length(levels(scatter.groups))
    if (!is.numeric(x.order)) {
        x.order <- suppressWarnings(as.numeric(TextAsVector(x.order)))
    }
    if (!all(x.order %in% seq_len(n.panels))) {
        stop("'Order of panels' should be a comma separated list of indices (between 1 and ", n.panels, ")")
    }
    scatter.groups <- factor(scatter.groups)
    indices <- order(x.order)[as.numeric(scatter.groups)]
    lvls <- levels(scatter.groups)[x.order]
    factor(indices, labels = lvls)
}
