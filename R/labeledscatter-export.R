#' Labeled Scatterplot chart
#'
#' Plot scatterplot with labels - best used for small sets of data
#'
#' @param x A numeric vector for the x-axis coordinates (which may be named); or a matrix or dataframe which may have 1-4 columns containing: 1:x, 2:y, 3:sizes, 4:colors (columns with all NAs ignored); or a list of matrices, where each matrix is in the format described and share the same row and column names
#' @param y Optional numeric vector for the y-axis coordinates. Should contain the same number of observations as x. If not provided, will use x instead.
#' @param scatter.labels Optional vector for labelling scatter points. This should be the same length as the number of observations in x andy. This is used in the hovertext and data labels.
#' @param scatter.labels.name Character; Used for labelling subtitles and footers.
#' @param scatter.sizes Numeric vector determining of the size of each observation. These can alternatively be provided as a column in \code{x}.
#' @param scatter.sizes.name Character; Used for labelling footers and legends.
#' @param scatter.colors Numeric, character, or categorical vector determining the color of each observation. These can alternatively be provided as a column in \code{x}.
#' @param scatter.colors.name Character; Used for labelling footers.
#' @param scatter.colors.as.categorical Boolean; Whether to treat colors as a categorical groups, or a numeric scale.
#' @param colors A vector of colors to use in the chart. When \code{scatter.colors.as.categorical}, the vector of colors should have the length as the number of categories in \code{scatter.colors}. If \code{scatter.colors} is used as numeric vector, then a color ramp is constructed from the colors listed.
#' @param title Character; chart title.
#' @param title.font.family Character; title font family. Can be "Arial Black",
#' "Arial", "Comic Sans MS", "Courier New", "Georgia", "Impact",
#' "Lucida Console", "Lucida Sans Unicode", "Marlett", "Symbol", "Tahoma",
#' "Times New Roman", "Trebuchet MS", "Verdana", "Webdings"
#' @param title.font.color Title font color as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param title.font.size Title font size; default = 10.
#' @param subtitle Character
#' @param subtitle.font.color subtitle font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param subtitle.font.family Character; subtitle font family
#' @param subtitle.font.size subtitle font size
#' @param footer Character
#' @param footer.font.color footer font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param footer.font.family Character; footer font family
#' @param footer.font.size footer font size
#' @param footer.wrap Logical; whether the footer text should be wrapped.
#' @param footer.wrap.nchar Number of characters (approximately) in each line of the footer when \code{footer.wordwrap} \code{TRUE}.
#' @param legend.show Logical; show the legend.
#' @param legend.font.color Legend font color as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param legend.font.family Character; legend font family.
#' @param legend.font.size Legend font size.
#' @param grid.show Logical; Whether to show grid lines.
#' @param y.title Character, y-axis title; defaults to chart input values;
#' to turn off set to "FALSE".
#' @param y.title.font.color y-axis title font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, max = 255)).
#' @param y.title.font.family Character; y-axis title font family
#' @param y.title.font.size y-axis title font size
#' @param y.line.width y-axis line in pixels, 0 = no line
#' @param y.line.color y-axis line color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param y.bounds.minimum Minimum of range for plotting;
#' NULL = no manual range set.  Must be less than y.bounds.maximum
#' @param y.bounds.maximum Maximum of range for
#' plotting; NULL = no manual range set.  Must be greater than y.bounds.minimum
#' @param y.tick.distance Tick mark distance.
#' @param y.grid.width Width of y-grid lines in pixels; 0 = no line
#' @param y.grid.color Color of y-grid lines as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param y.tick.show Whether to display the y-axis tick labels
#' @param y.tick.suffix y-axis tick label suffix
#' @param y.tick.prefix y-axis tick label prefix
#' @param y.tick.decimals y-axis tick label decimal places
#' @param y.tick.format.manual Overrides tick.prefix, suffix and decimals;
#' See https://github.com/mbostock/d3/wiki/Formatting#numbers or
#' https://docs.python.org/release/3.1.3/library/string.html#formatspec
#' @param y.tick.font.color y-axis tick label font color as a named color
#' in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param y.tick.font.family Character; y-axis tick label font family
#' @param y.tick.font.size y-axis tick label font size
#' @param x.title Character, x-axis title; defaults to chart input values;
#' to turn off set to "FALSE".
#' @param x.title.font.color x-axis title font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param x.title.font.family Character; x-axis title font family
#' @param x.title.font.size x-axis title font size
#' @param x.line.width x-axis line in pixels, 0 = no line
#' @param x.line.color x-axis line color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param x.tick.suffix x-axis tick label suffix
#' @param x.tick.prefix x-axis tick label prefix
#' @param x.bounds.minimum Minimum of range for plotting;
#' NULL = no manual range set.  Must be less than x.bounds.maximum
#' @param x.bounds.maximum Maximum of range for
#' plotting; NULL = no manual range set.  Must be greater than x.bounds.minimum
#' @param x.tick.distance Tick mark distance in
#' x-axis units between minimum and maximum for plotting; NULL = no manual
#' range set.
#' @param x.grid.width Width of y-grid lines in pixels; 0 = no line
#' @param x.grid.color Color of y-grid lines as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param x.tick.show Whether to display the x-axis tick labels
#' @param x.tick.decimals x-axis tick label decimal places
#' @param x.tick.font.color X-axis tick label font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param x.tick.font.family Character; x-axis tick label font family
#' @param x.tick.font.size x-axis tick label font size
#' @param series.marker.size Size in pixels of marker
#' @param global.font.family Character; font family for all occurrences of any
#' font attribute for the chart unless specified individually.
#' @param global.font.color Global font color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. #' rgb(0, 0, 0, maxColorValue = 255)).
#' @param data.label.font.family Character; font family for data label.
#' @param data.label.font.size Font size for data label.
#' @param data.label.font.color Font color as a named color
#' in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param scatter.max.labels Integer; the maximum number of labels to show on a Labeled Scatterplot.
#' @param trend.lines Boolean indicating whether to plot trend lines for multiple tables.
#' @param logos Optional list of images to be used to label scatterplot instead of the row names. It should be inputted as a comma-seperated list of URLs.
#' @param logo.size Numeric controlling the size of the logos.
#' @param swap.x.and.y Swap the x and y axis around on the chart.
#' @param ... Extra arguments that are ignored.
#' @importFrom grDevices rgb
#' @importFrom flipChartBasics ChartColors StripAlphaChannel
#' @importFrom rhtmlLabeledScatter LabeledScatter
#' @export
LabeledScatterChart <- function(x = NULL,
                                y = NULL,
                                scatter.labels = NULL,
                                scatter.labels.name = NULL,
                                scatter.sizes = NULL,
                                scatter.sizes.name = NULL,
                                scatter.colors = NULL,
                                scatter.colors.name = NULL,
                                scatter.colors.as.categorical = !is.null(groups),
                                trend.lines = FALSE,
                                logos = NULL,
                                logo.size  = 0.5,
                                colors = ChartColors(12),
                                legend.show = TRUE,
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
                                scatter.max.labels = 20,
                                data.label.font.family = global.font.family,
                                data.label.font.color = global.font.color,
                                data.label.font.size = 10,
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
                                y.tick.decimals = NULL,
                                y.tick.format.manual = "",
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
                                x.tick.decimals = NULL,
                                x.tick.font.color = global.font.color,
                                x.tick.font.family = global.font.family,
                                x.tick.font.size = 10,
                                series.marker.size = 6,
                                swap.x.and.y = FALSE,
                                ...)
{
    logo.urls <- NULL
    if (!is.null(logos) && nchar(logos) != 0)
    {
        logo.urls <- try(TextAsVector(logos))
        if (inherits(logo.urls, "try-error"))
            logo.urls <- NULL
        else if (any(nchar(logo.urls) == 0))
            stop("Logos cannot be an empty string\n")
    }

    # Try to store name of variables
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
        if (!trend.lines)
            rownames(x) <- sprintf("%s: %s", rep(table.names, n.tmp), groups)
        if (!is.null(logo.urls))
            logo.urls <- rep(logo.urls, num.tables)
    }

    if (is.matrix(x) || is.data.frame(x))
    {
        col.offset <- 0
        if (!is.null(rownames(x)))
            scatter.labels <- rownames(x)
        if (is.null(y) && ncol(x) >= col.offset + 2 && any(!is.na(x[,col.offset+2])))
        {
            if ((is.na(y.title) || nchar(y.title) == 0) && !is.null(colnames(x)))
                y.title <- colnames(x)[col.offset + 2]
            y <- x[,col.offset + 2]
        }
        if (is.null(scatter.sizes) && ncol(x) >= col.offset + 3 && any(!is.na(x[,col.offset+3])))
        {
            if (is.null(scatter.sizes.name) && !is.null(colnames(x)))
                scatter.sizes.name <- colnames(x)[col.offset + 3]
            scatter.sizes <- x[,col.offset + 3]
        }
        if (is.null(scatter.colors) && ncol(x) >= col.offset + 4 && any(!is.na(x[,col.offset+4])))
        {
            if (is.null(scatter.colors.name) || nchar(scatter.colors.name) == 0)
                scatter.colors.name <- colnames(x)[col.offset + 4]
            scatter.colors <- x[,col.offset + 4]
        }
        if (((is.na(x.title) || nchar(x.title) == 0) && !is.null(colnames(x))) && any(!is.na(x[,col.offset+1])))
            x.title <- colnames(x)[col.offset + 1]
        x <- x[,col.offset + 1]
        if (all(is.na(x)))
            x <- NULL
    }
    if (is.null(scatter.labels) && !is.null(names(x)))
        scatter.labels <- names(x)

    # Basic data checking
    if (is.null(x) && is.null(y))
        stop("At least one of x or y must be supplied.")
    if (is.null(x))
        x <- rep(0, length(y))
    n <- length(x)
    if (is.null(y))
        y <- rep(0, n)
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

    not.na <- is.finite(x) & is.finite(y)
    if (sum(not.na) != n)
        warning("Data points with missing values have been omitted.")

    n <- length(x)
    x <- as.numeric(x)
    y <- as.numeric(y)
    if (!is.null(scatter.sizes))
    {
        if (length(scatter.sizes) != n)
            stop("'scatter.sizes' should be a numeric vector with the same number of observations as 'x'.")
        scatter.sizes <- AsNumeric(scatter.sizes, binary=TRUE)
        if (any(!is.finite(scatter.sizes)))
        {
            warning("Some points omitted due to missing values in 'scatter.sizes'.")
            not.na <- not.na & is.finite(scatter.sizes)
        }
    }
    if (!is.null(scatter.colors))
    {
        if (!scatter.colors.as.categorical)
            scatter.colors <- AsNumeric(scatter.colors, binary = FALSE)
        if (length(scatter.colors) != n)
            stop("'scatter.colors' should be a vector with the same number of observations as 'x'.")
        if (any(is.null(scatter.colors)))
        {
            warning("Some points omitted due to missing values in 'scatter.colors'")
            not.na <- not.na & !is.null(scatter.colors)
        }
    }
    if (sum(not.na) == 0)
        stop("No non-NA points to plot.")

    # Determine color for each observation
    if (!is.null(scatter.colors) && !scatter.colors.as.categorical)
    {
        if (num.tables > 1)
            stop("'scatter.colors' cannot be used with multiple tables")
        legend.show <- FALSE # don't need to worry about order of groups
        groups <- 1:n # what about mult tables?
        col.fun <- colorRamp(colors)
        scatter.colors.scaled <- (scatter.colors - min(scatter.colors, na.rm=T))/diff(range(scatter.colors, na.rm=T))
        colors <- rgb(col.fun(scatter.colors.scaled), maxColorValue=255)

    } else
    {
        if (is.null(groups))
            groups <- scatter.colors
        if (length(groups) != n)
            groups <- rep(" ", n)

        groups <- as.character(groups)
        g.list <- unique(groups)
        colors <- paste0(rep("", length(g.list)), colors)
        names(colors) <- g.list
        colors <- colors[g.list]
    }
    colors <- StripAlphaChannel(colors)

    if (trend.lines)
        legend.show <- FALSE
    if (!is.null(logo.urls) && length(logo.urls) != n)
        stop(sprintf("Number of URLs supplied in logos is %.0f but must be equal to the number of rows in the table (%.0f)\n", length(logo.urls)/num.tables, n/num.tables))
    logo.size <- rep(logo.size, n)
    if (is.null(scatter.labels))
        scatter.labels <- rep("", n)
    lab.tidy <- scatter.labels
    if (length(scatter.labels) > scatter.max.labels)
        lab.tidy <- scatter.labels[(scatter.max.labels+1):(length(scatter.labels))] <- ""
    if (!is.null(logo.urls))
        lab.tidy <- logo.urls
    if (length(footer) == 0 || nchar(footer) == 0)
    {
        footer <- ""
        if (!is.null(scatter.labels.name))
            footer <- sprintf("%sPoints labeled by '%s'; ", footer, scatter.labels.name)
        if (!is.null(scatter.colors.name))
            footer <- sprintf("%sPoints colored according to '%s'; ", footer, scatter.colors.name)
        if (!is.null(scatter.sizes.name))
            footer <- sprintf("%sArea of points are proportional to absolute value of '%s'; ",
                              footer, scatter.sizes.name)
    }
    footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate=FALSE)

    return(LabeledScatter(X = x[not.na],
                       Y = y[not.na],
                       Z = if (is.null(scatter.sizes)) NULL else abs(scatter.sizes[not.na]),
                       group = groups[not.na],
                       colors = colors[not.na],
                       label = lab.tidy[not.na],
                       label.alt = scatter.labels[not.na],
                       fixed.aspect = FALSE,
                       grid = grid.show,
                       origin = FALSE,
                       origin.align = FALSE,
                       labels.show = TRUE,
                       legend.show = legend.show && length(g.list) > 1,
                       legend.bubbles.show = !is.null(scatter.sizes),
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
                       axis.font.color = y.tick.font.color,
                       axis.font.size = y.tick.font.size,
                       x.title = x.title,
                       x.title.font.family = x.title.font.family,
                       x.title.font.color = x.title.font.color,
                       x.title.font.size = x.title.font.size,
                       z.title = scatter.sizes.name,
                       x.decimals = if (is.null(x.tick.decimals)) decimalsToDisplay(x) else x.tick.decimals,
                       y.decimals = if (is.null(y.tick.decimals)) decimalsToDisplay(y) else y.tick.decimals,
                       x.prefix = x.tick.prefix,
                       y.prefix = y.tick.prefix,
                       x.suffix = x.tick.suffix,
                       y.suffix = y.tick.suffix,
                       title.font.family = title.font.family,
                       title.font.color = title.font.color,
                       title.font.size = title.font.size,
                       labels.font.family = data.label.font.family,
                       labels.font.color = data.label.font.color,
                       labels.font.size = data.label.font.size,
                       point.radius = 0.5 * series.marker.size,
                       y.bounds.maximum = y.bounds.maximum,
                       y.bounds.minimum = y.bounds.minimum,
                       y.bounds.units.major = y.tick.distance,
                       x.bounds.maximum = x.bounds.maximum,
                       x.bounds.minimum = x.bounds.minimum,
                       x.bounds.units.major = x.tick.distance,
                       y.axis.show = y.tick.show,
                       x.axis.show = x.tick.show,
                       plot.border.show = FALSE,
                       title = title,
                       trend.lines.show = trend.lines,
                       labels.logo.scale = logo.size,
                       debug.mode = grepl("DEBUG_MODE_ON", title)))
}


