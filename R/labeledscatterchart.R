#' LabeledScatter
#'
#' Labeled Scatter Chart
#'
#' @inherit Column
#' @param x A numeric vector for the x-axis coordinates (which may be named); or a matrix or dataframe; or a list of matrices, where each matrix share the same row and column names
#' @param y Optional numeric vector for the y-axis coordinates. Should contain the same number of observations as x. If not provided, will use x instead.
#' @param scatter.x.column When \code{x} is a dataframe or matrix, the index of the column (1-based) which contains the x-coordinate data.
#' @param scatter.y.column When \code{x} is a dataframe or matrix, the index of the column (1-based) which contains the y-coordinate data.
#' @param scatter.sizes.column When \code{x} is a dataframe or matrix, the index of the column (1-based) which contains \code{scatter.sizes} data.
#' @param scatter.colors.column When \code{x} is a dataframe or matrix, the index of the column (1-based) which contains \code{scatter.colors} data.
#' @param scatter.labels Optional vector for labelling scatter points. This should be the same length as the number of observations in x and y.
#' @param scatter.labels.name Character; Used for labelling subtitles and footers.
#' @param scatter.sizes Numeric vector determining of the size of each observation. These can alternatively be provided as a column in \code{x}.
#' @param scatter.sizes.name Character; Used for labelling footers and legends.
#' @param scatter.colors Numeric, character, or categorical vector determining the color of each observation. These can alternatively be provided as a column in \code{x}.
#' @param scatter.colors.name Character; Used for labelling footers.
#' @param scatter.colors.as.categorical Boolean; Whether to treat colors as a categorical groups, or a numeric scale.
#' @param colors A vector of colors to use in the chart. When \code{scatter.colors.as.categorical}, the vector of colors should have the length as the number of categories in \code{scatter.colors}. If \code{scatter.colors} is used as numeric vector, then a color ramp is constructed from the colors listed.
#' @param data.label.font.autocolor Boolean; If true, \code{data.label.font.color} is ignored and labels are colored
#' according to the series color.
#' @param opacity of scatter point colors as an alpha value (0 to 1).
#' @param scatter.max.labels Integer; the maximum number of labels to show on a Labeled Scatterplot.
#' @param trend.lines Boolean indicating whether to plot trend lines for multiple tables.
#' @param logos Optional list of images to be used to label scatterplot instead of the row names.
#' This should be input as a comma-seperated list of URLs.
#' @param logo.size Numeric controlling the size of the logos.
#' @param marker.size Size in pixels of marker.
#' @param swap.x.and.y Swap the x and y axis around on the chart.
#' @param label.auto.placement Logical; whether the scatter plot labels are positioned automatically
#'  to reduce overlap.
#' @param legend.bubbles.show Logical; show legend for bubble sizes.
#' @param ... Other arguments which are ignored.
#' @importFrom grDevices rgb
#' @importFrom flipTransformations AsNumeric
#' @importFrom flipFormat FormatAsReal
#' @importFrom flipChartBasics ChartColors StripAlphaChannel
#' @importFrom rhtmlLabeledScatter LabeledScatter
#' @export
LabeledScatter <- function(x = NULL,
                                y = NULL,
                                scatter.x.column = 1,
                                scatter.y.column = 2,
                                scatter.labels = NULL,
                                scatter.labels.name = NULL,
                                scatter.sizes = NULL,
                                scatter.sizes.name = NULL,
                                scatter.sizes.column = 3,
                                scatter.colors = NULL,
                                scatter.colors.name = NULL,
                                scatter.colors.column = 4,
                                scatter.colors.as.categorical = TRUE,
                                label.auto.placement = TRUE,
                                trend.lines = FALSE,
                                logos = NULL,
                                logo.size  = 0.5,
                                colors = ChartColors(12),
                                opacity = NULL,
                                legend.show = TRUE,
                                legend.bubbles.show = TRUE,
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
                                scatter.max.labels = 50,
                                data.label.font.family = global.font.family,
                                data.label.font.color = global.font.color,
                                data.label.font.autocolor = NA,
                                data.label.font.size = 10,
                                data.label.format = "",
                                data.label.prefix = "",
                                data.label.suffix = "",
                                legend.font.color = global.font.color,
                                legend.font.family = global.font.family,
                                legend.font.size = 10,
                                grid.show = TRUE,
                                y.title = "",
                                y.title.font.color = global.font.color,
                                y.title.font.family = global.font.family,
                                y.title.font.size = 12,
                                y.line.width = 0,
                                y.line.color = rgb(0, 0, 0, maxColorValue = 255),
                                y.bounds.minimum = NULL,
                                y.bounds.maximum = NULL,
                                y.tick.distance = NULL,
                                #y.data.reversed = FALSE,
                                y.grid.width = 1,
                                y.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                                y.tick.show = TRUE,
                                y.tick.suffix = "",
                                y.tick.prefix = "",
                                y.tick.format = "",
                                y.tick.font.color = global.font.color,
                                y.tick.font.family = global.font.family,
                                y.tick.font.size = 10,
                                x.title = "",
                                x.title.font.color = global.font.color,
                                x.title.font.family = global.font.family,
                                x.title.font.size = 12,
                                x.line.width = 0,
                                x.line.color = rgb(0, 0, 0, maxColorValue = 255),
                                x.bounds.minimum = NULL,
                                x.bounds.maximum = NULL,
                                x.tick.distance = NULL,
                                #x.data.reversed = FALSE,
                                x.grid.width = 1,
                                x.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                                x.tick.show = TRUE,
                                x.tick.suffix = "",
                                x.tick.prefix = "",
                                x.tick.format = "",
                                x.tick.font.color = global.font.color,
                                x.tick.font.family = global.font.family,
                                x.tick.font.size = 10,
                                hovertext.font.family = global.font.family,
                                hovertext.font.size = 11,
                                marker.size = 6,
                                swap.x.and.y = FALSE,
                                ...)
{
    if (!is.null(y))
        ErrorIfNotEnoughData(cbind(x, y))
    else
        ErrorIfNotEnoughData(x, require.tidy = FALSE)

    logo.urls <- NULL
    if (!is.null(logos) && any(nchar(logos) != 0))
    {
        logo.urls <- try(TextAsVector(logos))
        if (inherits(logo.urls, "try-error"))
            logo.urls <- NULL
    }

    # Try to store name of variables
    scatter.mult.yvals <- isTRUE(attr(x, "scatter.mult.yvals"))
    if (!is.null(scatter.sizes) && is.null(scatter.sizes.name))
        scatter.sizes.name <- deparse(substitute(scatter.sizes))
    if (!is.null(scatter.labels) && is.null(scatter.labels.name))
        scatter.labels.name <- deparse(substitute(scatter.labels))
    if (!is.null(scatter.colors) && is.null(scatter.colors.name))
        scatter.colors.name <- deparse(substitute(scatter.colors))

    num.tables <- 1
    groups <- NULL
    if (is.list(x) && !is.null(ncol(x[[1]])))
    {
        num.tables <- length(x)
        n.tmp <- nrow(x[[1]])
        table.names <- unlist(lapply(1:num.tables,
            function(ii){res <- attr(x[[ii]], "name"); if (is.null(res)) res <- ii;
            return(as.character(res))}))
        x <- checkTableList(x, trend.lines)
        groups <- rep(rownames(x[[1]]), num.tables)
        x <- do.call(rbind, x)
        if (trend.lines)
            scatter.sizes.column <- 0
        if (!trend.lines)
            rownames(x) <- sprintf("%s: %s",
                rep(table.names, each = n.tmp), groups)
        if (!is.null(logo.urls))
            logo.urls <- rep(logo.urls, num.tables)
    }

    if (is.matrix(x) || is.data.frame(x))
    {
        .isValidColumnIndex <- function(n) {return (!is.null(n) && !is.na(n) && n > 0 && n <= ncol(x))}
        if (is.null(scatter.labels) && !is.null(rownames(x)))
            scatter.labels <- rownames(x)
        if (is.null(y) && .isValidColumnIndex(scatter.y.column))
        {
            if (sum(nchar(y.title), na.rm = TRUE) == 0 && !is.null(colnames(x)) && !scatter.mult.yvals)
                y.title <- colnames(x)[scatter.y.column]
            y <- x[,scatter.y.column]
        }
        if (is.null(scatter.sizes) && .isValidColumnIndex(scatter.sizes.column))
        {
            if (is.null(scatter.sizes.name) && !is.null(colnames(x)))
                scatter.sizes.name <- colnames(x)[scatter.sizes.column]
            scatter.sizes <- x[,scatter.sizes.column]
        }
        if (is.null(scatter.colors) && .isValidColumnIndex(scatter.colors.column))
        {
            if (is.null(scatter.colors.name) || nchar(scatter.colors.name) == 0)
                scatter.colors.name <- colnames(x)[scatter.colors.column]
            scatter.colors <- x[,scatter.colors.column]
        }
        if (sum(nchar(x.title), na.rm = TRUE) == 0 && (!is.null(colnames(x))) &&
            .isValidColumnIndex(scatter.x.column) && !scatter.mult.yvals)
            x.title <- colnames(x)[scatter.x.column]
        if (!.isValidColumnIndex(scatter.x.column))
            x <- NULL
        else
            x <- x[,scatter.x.column]
    }
    if (is.null(scatter.labels) && !is.null(names(x)))
        scatter.labels <- names(x)


    # Basic data checking
    if (is.null(x) && is.null(y))
        stop("At least one of x or y must be supplied.")
    if (is.null(x))
    {
        x <- rep(0, length(y))
        if (sum(nchar(x.bounds.minimum)) == 0)
            x.bounds.minimum = -0.25
        if (sum(nchar(x.bounds.maximum)) == 0)
            x.bounds.maximum = 0.25
    }
    n <- length(x)
    if (is.null(y))
    {
        y <- rep(0, n)
        if (sum(nchar(y.bounds.minimum)) == 0)
            y.bounds.minimum = -0.25
        if (sum(nchar(y.bounds.maximum)) == 0)
            y.bounds.maximum = 0.25
    }
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

    x.not.na <- if (is.numeric(x)) is.finite(x) else !is.na(x)
    y.not.na <- if (is.numeric(y)) is.finite(y) else !is.na(y)
    not.na <- x.not.na & y.not.na
    if (sum(not.na) != n)
        warning("Data points with missing values have been omitted.")

    n <- length(x)
    if (!is.null(scatter.sizes))
    {
        if (length(scatter.sizes) != n)
            stop("'scatter.sizes' should be a numeric vector with the same number of observations as 'x'.")
        sz.tmp <- AsNumeric(scatter.sizes, binary = FALSE)
        if (any(class(scatter.sizes) %in% c("Date", "POSIXct", "POSIXt")))
            sz.tmp <- sz.tmp - min(sz.tmp, na.rm = TRUE)
        scatter.sizes <- sz.tmp
        if (any(!is.finite(scatter.sizes)))
        {
            warning("Some points omitted due to missing values in 'scatter.sizes'.")
            not.na <- not.na & is.finite(scatter.sizes)
        }
        if (is.null(opacity))
            opacity <- 0.4
    }
    if (is.null(opacity))
        opacity <- 1

    scatter.colors.raw <- scatter.colors
    if (!is.null(scatter.colors))
    {
        if (!scatter.colors.as.categorical)
            scatter.colors <- AsNumeric(scatter.colors, binary = FALSE)
        if (length(scatter.colors) != n)
            stop("'scatter.colors' should be a vector with the same number of observations as 'x'.")
        if (any(is.na(scatter.colors)))
        {
            warning("Some points omitted due to missing values in 'scatter.colors'")
            not.na <- not.na & is.finite(scatter.colors)
        }
    }
    if (sum(not.na) == 0)
        stop("No non-NA points to plot.")
    not.na <- which(not.na) # indexing makes re-ordering easier later
    if (is.finite(scatter.max.labels) && scatter.max.labels < 0)
            scatter.max.labels <- NA

    # Determine color for each observation
    if (!is.null(scatter.colors) && !scatter.colors.as.categorical)
    {
        if (num.tables > 1)
            stop("'scatter.colors' cannot be used with multiple tables")
        legend.show <- FALSE # don't need to worry about order of groups
        groups <- 1:n # what about mult tables?
        col.fun <- colorRamp(unique(colors)) # undo recycling in PrepareColors
        scatter.colors.scaled <- (scatter.colors - min(scatter.colors, na.rm=T))/diff(range(scatter.colors, na.rm=T))
        #if (length(not.na) != length(scatter.colors))
        #    scatter.colors.scaled[-not.na] <- 0 # removed later
        colors <- rgb(col.fun(scatter.colors.scaled[not.na]), maxColorValue=255)

    } else
    {
        if (is.null(groups))
            groups <- scatter.colors.raw
        if (length(groups) != n)
            groups <- rep(" ", n)

        # Get list of all series names - including if those with all NAs
        groups.ord <- order(suppressWarnings(AsNumeric(groups, binary = FALSE)))
        g.list.all <- if (is.factor(groups)) levels(groups)
                      else unique(groups[groups.ord])
        colors <- paste0(rep("", length(g.list.all)), colors)
        names(colors) <- g.list.all
        legend.show <- setShowLegend(legend.show, length(g.list.all))


        # Extract only non-NA points and order based on series name
        groups.ord <- order(suppressWarnings(AsNumeric(groups[not.na], binary = FALSE)))
        not.na <- not.na[groups.ord]
        groups <- as.character(groups)
        g.list <- unique(groups[not.na])
        colors <- colors[g.list]
    }
    colors <- StripAlphaChannel(colors)
    if (is.na(data.label.font.autocolor))
        data.label.font.autocolor <- length(unique(groups[not.na])) > 1

    if (trend.lines)
        legend.show <- FALSE

    if (is.null(scatter.labels))
        scatter.labels <- rep("", n)
    if (is.numeric(scatter.labels)) {
        if (percentFromD3(data.label.format))
            scatter.labels <- FormatAsPercent(scatter.labels, decimals = decimalsFromD3(data.label.format))
        else
            scatter.labels <- FormatAsReal(scatter.labels, decimals = decimalsFromD3(data.label.format))
    }
    scatter.labels <- paste0(data.label.prefix, scatter.labels, data.label.suffix)

    empty.logo <- which(nchar(logo.urls) == 0)
    if (length(empty.logo) > 0)
        logo.urls[empty.logo] <- scatter.labels[empty.logo]
    if (!is.null(logo.urls) && length(logo.urls) < n)
        logo.urls <- c(logo.urls, scatter.labels[(length(logo.urls)+1):n])
    logo.size <- rep(logo.size, n)

    lab.tidy <- scatter.labels
    if (!is.na(scatter.max.labels) && length(scatter.labels) > scatter.max.labels)
    {
        if (scatter.max.labels == 50)
            warning("By default, only the first 50 labels are shown to avoid long running times. Adjust 'Maximum data labels to plot' to show more labels. Alternatively, to show a large number of points, show as 'Hovertext' instead.")
        else
            warning("Some labels have been hidden. Adjust 'Maximum data labels to plot' to show more labels.")
        lab.tidy[(scatter.max.labels+1):(length(scatter.labels))] <- ""
    }
    if (!is.null(logo.urls))
        lab.tidy <- logo.urls
    .isEmptyName <- function(x) { sum(nchar(trimws(x)), na.rm = TRUE) == 0 }
    if (length(footer) == 0 || nchar(footer) == 0)
    {
        footer <- ""
        if (!.isEmptyName(scatter.labels.name))
            footer <- sprintf("%sPoints labeled by '%s'; ", footer, scatter.labels.name)
        if (!.isEmptyName(scatter.colors.name) && !scatter.mult.yvals)
            footer <- sprintf("%sPoints colored according to '%s'; ", footer, scatter.colors.name)
        if (!.isEmptyName(scatter.sizes.name) && !scatter.mult.yvals)
            footer <- sprintf("%sArea of points are proportional to absolute value of '%s'; ",
                              footer, scatter.sizes.name)
    }
    if (sum(nchar(footer)) > 0 && footer != " ")
        footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate=FALSE)

    # Convert axis to the appropriate type based on axis values and tick format
    # Give warning where possible
    x.axis.type <- getAxisType(x[not.na], x.tick.format)
    x.tick.format <- checkD3Format(x.tick.format, x.axis.type, "X axis", convert = TRUE)
    x <- convertAxis(x, x.axis.type)
    y.axis.type <- getAxisType(y[not.na], y.tick.format)
    y.tick.format <- checkD3Format(y.tick.format, y.axis.type, "Y axis", convert = TRUE)
    y <- convertAxis(y, y.axis.type)

    tooltips.text <- sprintf("%s (%s, %s)", scatter.labels[not.na],
        formatByD3(x[not.na], x.tick.format, x.tick.prefix, x.tick.suffix),
        formatByD3(y[not.na], y.tick.format, y.tick.prefix, y.tick.suffix))
    if (!.isEmptyName(scatter.sizes.name))
        tooltips.text <- sprintf("%s\n%s: %s", tooltips.text, scatter.sizes.name,
        formatByD3(scatter.sizes[not.na], ""))
    if (!.isEmptyName(scatter.colors.name))
        tooltips.text <- sprintf("%s\n%s: %s", tooltips.text, scatter.colors.name,
        formatByD3(scatter.colors[not.na], ""))

    p <- rhtmlLabeledScatter::LabeledScatter(X = x[not.na],
                       Y = y[not.na],
                       Z = if (is.null(scatter.sizes)) NULL else abs(scatter.sizes[not.na]),
                       x.levels = levels(x),
                       y.levels = rev(levels(y)),
                       group = groups[not.na],
                       colors = colors,
                       color.transparency = opacity,
                       label = lab.tidy[not.na],
                       label.alt = scatter.labels[not.na],
                       fixed.aspect = FALSE,
                       grid = grid.show,
                       origin = FALSE,
                       origin.align = FALSE,
                       labels.show = TRUE,
                       label.placement.numSweeps = if (label.auto.placement) 500 else 0,
                       legend.show = legend.show,
                       legend.bubbles.show = !is.null(scatter.sizes) && isTRUE(legend.bubbles.show),
                       legend.font.color = legend.font.color,
                       legend.font.family = legend.font.family,
                       legend.font.size = legend.font.size,
                       legend.bubble.font.color = legend.font.color,
                       legend.bubble.font.family = legend.font.family,
                       legend.bubble.font.size = legend.font.size,
                       legend.bubble.title.font.color = legend.font.color,
                       legend.bubble.title.font.family = legend.font.family,
                       legend.bubble.title.font.size = legend.font.size,
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
                       axis.font.family = y.tick.font.family,
                       axis.font.color = if (!is.null(y.tick.font.color)) y.tick.font.color else "#2C2C2C",
                       axis.font.size = y.tick.font.size,
                       x.title = x.title,
                       x.title.font.family = x.title.font.family,
                       x.title.font.color = x.title.font.color,
                       x.title.font.size = x.title.font.size,
                       z.title = scatter.sizes.name,
                       x.format = x.tick.format,
                       y.format = y.tick.format,
                       x.prefix = x.tick.prefix,
                       y.prefix = y.tick.prefix,
                       x.suffix = x.tick.suffix,
                       y.suffix = y.tick.suffix,
                       title.font.family = title.font.family,
                       title.font.color = title.font.color,
                       title.font.size = title.font.size,
                       labels.font.family = data.label.font.family,
                       labels.font.color = if (data.label.font.autocolor) NULL else data.label.font.color,
                       labels.font.size = data.label.font.size,
                       point.radius = 0.5 * marker.size,
                       y.bounds.maximum = charToNumeric(y.bounds.maximum),
                       y.bounds.minimum = charToNumeric(y.bounds.minimum),
                       y.bounds.units.major = charToNumeric(y.tick.distance),
                       x.bounds.maximum = charToNumeric(x.bounds.maximum),
                       x.bounds.minimum = charToNumeric(x.bounds.minimum),
                       x.bounds.units.major = charToNumeric(x.tick.distance),
                       y.axis.show = y.tick.show,
                       x.axis.show = x.tick.show,
                       tooltip.font.family = hovertext.font.family,
                       tooltip.font.size = hovertext.font.size,
                       tooltip.text = tooltips.text,
                       plot.border.show = FALSE,
                       title = title,
                       trend.lines.show = trend.lines,
                       labels.logo.scale = logo.size,
                       debug.mode = grepl("DEBUG_MODE_ON", title))

    result <- list(htmlwidget = p)
    class(result) <- "StandardChart"
    attr(result, "ChartType") <- if (!is.null(scatter.sizes)) "Bubble"
                                 else                         "X Y Scatter"
    result
}


