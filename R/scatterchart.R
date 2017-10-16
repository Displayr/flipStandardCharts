#' Scatterplot chart
#'
#' Plot scatterplot chart
#'
#' @param x A numeric vector for the x-axis coordinates (which may be named); or a matrix or dataframe.
#' @param y Optional numeric vector for the y-axis coordinates. Should contain the same number of observations as x. If not provided, will use x instead.
#' @param scatter.x.column When \code{x} is a dataframe or matrix, the index of the column (1-based) which contains the x-coordinate data.
#' @param scatter.y.column When \code{x} is a dataframe or matrix, the index of the column (1-based) which contains the y-coordinate data.
#' @param scatter.sizes.column When \code{x} is a dataframe or matrix, the index of the column (1-based) which contains \code{scatter.sizes} data.
#' @param scatter.colors.column When \code{x} is a dataframe or matrix, the index of the column (1-based) which contains \code{scatter.colors} data.
#' @param scatter.labels Optional vector for labelling scatter points. This should be the same length as the number of observations in x andy. This is used in the hovertext and data labels.
#' @param scatter.labels.name Character; Used for labelling subtitles and footers.
#' @param scatter.sizes Numeric vector determining of the size of each observation. These can alternatively be provided as a column in \code{x}.
#' @param scatter.sizes.name Character; Used for labelling footers and legends.
#' @param scatter.colors Numeric, character, or categorical vector determining the color of each observation. These can alternatively be provided as a column in \code{x}.
#' @param scatter.colors.name Character; Used for labelling footers.
#' @param scatter.colors.as.categorical Whether to treat colors as a categorical groups, or a numeric scale.
#' @param colors A vector of colors to use in the chart. When \code{scatter.colors.as.categorical}, the vector of colors should have the length as the number of categories in \code{scatter.colors}. If \code{scatter.colors} is used as numeric vector, then a color ramp is constructed from the colors listed.
#' @param fit.type Character; type of line of best fit. Can be one of "None", "Linear" or "Smooth" (loess local polynomial fitting).
#' @param fit.ignore.last Boolean; whether to ignore the last data point in the fit.
#' @param fit.line.type Character; One of "solid", "dot", "dash, "dotdash", or length of dash "2px", "5px".
#' @param fit.line.width Numeric; Line width of line of best fit.
#' @param fit.line.name Character; Name of the line of best fit, which will appear in the hovertext.
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
#' @param opacity Opacity of area fill colors as an alpha value (0 to 1).
#' @param fit.line.colors Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' @param background.fill.color Background color in character format
#' (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param background.fill.opacity Background opacity as an alpha value
#' (0 to 1).
#' @param charting.area.fill.color Charting area background color as
#' a named color in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param charting.area.fill.opacity Charting area background
#' opacity as an alpha value (0 to 1).
#' @param legend.show Logical; show the legend.
#' @param legend.fill Same as \code{legend.fill.color}. Retained for backwards compatibility.
#' @param legend.fill.color Legend fill color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param legend.fill.opacity Legend fill opacity as an alpha value
#' (0 to 1).
#' @param legend.border.color Legend border color as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param legend.border.line.width Width in pixels of the border
#' around the legend.  0 = no border.
#' @param legend.font.color Legend font color as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param legend.font.family Character; legend font family.
#' @param legend.font.size Legend font size.
#' @param legend.position Where the legend will be placed; can be "left" or
#' "right" of plot.
#' @param legend.ascending Logical; TRUE for ascending, FALSE for descending.
#' By default, we set it to to FALSE if the chart is stacked and TRUE otherwise.
#' @param margin.top Margin between plot area and the top of the
#' graphic in pixels
#' @param margin.bottom Margin between plot area and the bottom of the
#' graphic in pixels
#' @param margin.left Margin between plot area and the left of the
#' graphic in pixels
#' @param margin.right Margin between plot area and the right of the
#' graphic in pixels
#' @param margin.inner.pad Padding in pixels between plot proper
#' and axis lines
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
#' @param y.tick.marks Character; whether and where to show tick marks on the
#' y axis.  Can be "outside", "inside", "none"
#' @param y.tick.mark.length Length of tick marks in pixels.
#' @param y.bounds.minimum Minimum of range for plotting;
#' NULL = no manual range set.  Must be less than y.bounds.maximum
#' @param y.bounds.maximum Maximum of range for
#' plotting; NULL = no manual range set.  Must be greater than y.bounds.minimum
#' @param y.tick.distance Tick mark distance.
#' @param y.zero Whether the y-axis should include zero.
#' @param y.zero.line.width Width in pixels of zero line; 0 = no zero line
#' shown
#' @param y.zero.line.color Color of horizontal zero line as a named
#' color in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param y.data.reversed Logical; whether to reverse y-axis or not
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
#' @param y.hovertext.decimals y-axis hover text decimal places
#' @param y.hovertext.format.manual Overrides hovertext decimals;
#' See https://github.com/mbostock/d3/wiki/Formatting#numbers or
#' https://docs.python.org/release/3.1.3/library/string.html#formatspec
#' @param y.tick.angle y-axis tick label angle in degrees.
#' 90 = vertical; 0 = horizontal
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
#' @param x.tick.marks Character; whether and where to show tick marks on the
#' x-axis.  Can be "outside", "inside", "none"
#' @param x.tick.mark.length Length of tick marks in pixels.
#' @param x.tick.suffix x-axis tick label suffix
#' @param x.tick.prefix x-axis tick label prefix
#' @param x.bounds.minimum Minimum of range for plotting;
#' NULL = no manual range set.  Must be less than x.bounds.maximum
#' @param x.bounds.maximum Maximum of range for
#' plotting; NULL = no manual range set.  Must be greater than x.bounds.minimum
#' @param x.tick.distance Tick mark distance in
#' x-axis units between minimum and maximum for plotting; NULL = no manual
#' range set.
#' @param x.zero.line.width Width in pixels of zero line; 0 = no zero line
#' shown
#' @param x.zero.line.color Color of horizontal zero (origo) line as a named
#' color in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param x.data.reversed Logical; whether to reverse x-axis or not
#' @param x.grid.width Width of y-grid lines in pixels; 0 = no line
#' @param x.grid.color Color of y-grid lines as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param x.tick.show Whether to display the x-axis tick labels
#' @param x.tick.decimals x-axis tick label decimal places
#' @param x.tick.format.manual Overrides tick.prefix, suffix and decimals;
#' See https://github.com/mbostock/d3/wiki/Formatting#numbers or
#' https://docs.px.hon.org/release/3.1.3/librarx.string.html#formatspec
#' @param x.hovertext.decimals X.axis hover text decimal places
#' @param x.hovertext.format.manual Overrides hovertext decimals;
#' See https://github.com/mbostock/d3/wiki/Formatting#numbers or
#' https://docs.px.hon.org/release/3.1.3/librarx.string.html#formatspec
#' @param x.tick.angle x-axis tick label angle in degrees.
#' 90 = vertical; 0 = horizontal
#' @param x.tick.font.color X-axis tick label font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param x.tick.font.family Character; x-axis tick label font family
#' @param x.tick.font.size x-axis tick label font size
#' @param label.wrap Logical; whether to wrap long labels on the x-axis.
#' @param label.wrap.nchar Integer; number of characters in each line when \code{label.wrap} is \code{TRUE}.
#' @param x.position Character; set x-axis position; can be "top" or "bottom"
#' @param y.position Character; set y-axis position; can be "left" or "right"
#' @param series.line.width Thickness, in pixels, of the series line
#' @param series.marker.show Can be "none", "automatic" or a vector referencing
#' the plotly symbol dictionary using either numerics or strings.
#' @param series.marker.colors Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' be reversed.
#' @param series.marker.opacity Opacity for series markers as an alpha value (0 to 1).
#' @param series.marker.size Size in pixels of marker
#' @param series.marker.border.width Width in pixels of border/line
#' around series markers; 0 is no line
#' @param tooltip.show Logical; whether to show a tooltip on hover.
#' @param modebar.show Logical; whether to show the zoom menu buttons or not.
#' @param global.font.family Character; font family for all occurrences of any
#' font attribute for the chart unless specified individually.
#' @param global.font.color Global font color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. #' rgb(0, 0, 0, maxColorValue = 255)).
#' @param data.label.show Logical; whether to show data labels.
#' @param data.label.font.family Character; font family for data label.
#' @param data.label.font.size Font size for data label.
#' @param data.label.font.color Font color as a named color
#' in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param data.label.decimals Number of decimal places to show in
#' data labels.
#' @param data.label.prefix Character; prefix for data values.
#' @param data.label.suffix Character; suffix for data values.
#' @param data.label.position Character; where to place the source data
#' value in relation to the marker icon.  Can be "top left", "top center", "top
#' right", "middle left", "middle center", "middle right", "bottom left",
#' "bottom center", "bottom right". Only applicable for line and area charts.
#' @param us.date.format Whether to apply the US convention when parsing dates.
#' @param swap.x.and.y Swap the x and y axis around on the chart.
#' @param ... Extra arguments that are ignored.
#' @importFrom grDevices rgb
#' @importFrom flipChartBasics ChartColors
#' @importFrom plotly plot_ly config toRGB add_trace add_text layout hide_colorbar
#' @importFrom stats loess loess.control lm predict
#' @export
ScatterChart <- function(x = NULL,
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
                         scatter.colors.as.categorical = FALSE,
                         colors = ChartColors(12),
                         fit.type = "None",
                         fit.ignore.last = FALSE,
                         fit.line.type = "dot",
                         fit.line.width = 1,
                         fit.line.name = "Fitted",
                         fit.line.colors = colors,
                         legend.show = TRUE,
                         tooltip.show = TRUE,
                         modebar.show = FALSE,
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
                         data.label.show = FALSE,
                         data.label.font.family = global.font.family,
                         data.label.font.color = global.font.color,
                         data.label.font.size = 10,
                         data.label.decimals = 2, # Ignored in Labeled Bubble and Scatterplots
                         data.label.prefix = "",
                         data.label.suffix = "",
                         data.label.position = "top middle",
                         opacity = NULL,
                         background.fill.color = rgb(255, 255, 255, maxColorValue = 255),
                         background.fill.opacity = 1,
                         charting.area.fill.color = rgb(255, 255, 255, maxColorValue = 255),
                         charting.area.fill.opacity = 1,
                         legend.fill = rgb(255, 255, 255, maxColorValue = 255), # retained for backwards compatibility
                         legend.fill.color = legend.fill,
                         legend.fill.opacity = 1,
                         legend.border.color = rgb(44, 44, 44, maxColorValue = 255),
                         legend.border.line.width = 0,
                         legend.font.color = global.font.color,
                         legend.font.family = global.font.family,
                         legend.font.size = 10,
                         legend.position = "right",
                         legend.ascending = NA,
                         margin.top = NULL,
                         margin.bottom = NULL,
                         margin.left = NULL,
                         margin.right = NULL,
                         margin.inner.pad = NULL,
                         grid.show = TRUE,
                         y.title = "",
                         y.title.font.color = global.font.color,
                         y.title.font.family = global.font.color,
                         y.title.font.size = 12,
                         y.line.width = 0,
                         y.line.color = rgb(0, 0, 0, maxColorValue = 255),
                         y.tick.marks = "",
                         y.tick.mark.length = 5,
                         y.bounds.minimum = NULL,
                         y.bounds.maximum = NULL,
                         y.tick.distance = NULL,
                         y.zero = TRUE,
                         y.zero.line.width = 0,
                         y.zero.line.color = rgb(44, 44, 44, maxColorValue = 255),
                         y.position = "left",
                         y.data.reversed = FALSE,
                         y.grid.width = 1 * grid.show,
                         y.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                         y.tick.show = TRUE,
                         y.tick.suffix = "",
                         y.tick.prefix = "",
                         y.tick.decimals = NULL,
                         y.tick.format.manual = "",
                         y.hovertext.decimals = NULL,
                         y.hovertext.format.manual = "",
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
                         x.zero.line.width = 0,
                         x.zero.line.color = rgb(44, 44, 44, maxColorValue = 255),
                         x.position = "bottom",
                         x.data.reversed = FALSE,
                         x.grid.width = 1 * grid.show,
                         x.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                         x.tick.show = TRUE,
                         x.tick.suffix = "",
                         x.tick.prefix = "",
                         x.tick.decimals = NULL,
                         x.tick.format.manual = "",
                         x.hovertext.decimals = NULL,
                         x.hovertext.format.manual = "",
                         x.tick.angle = NULL,
                         x.tick.font.color = global.font.color,
                         x.tick.font.family = global.font.family,
                         x.tick.font.size = 10,
                         label.wrap = TRUE,
                         label.wrap.nchar = 21,
                         series.line.width = 0,
                         series.marker.border.width = 1,
                         series.marker.size = 6,
                         series.marker.opacity = 1,
                         series.marker.show = "none", # ignored
                         series.marker.colors = NULL,
                         swap.x.and.y = FALSE,
                         us.date.format = FALSE,
                         ...)
{
    title.font=list(family=title.font.family, size=title.font.size, color=title.font.color)
    subtitle.font=list(family=subtitle.font.family, size=subtitle.font.size, color=subtitle.font.color)
    x.title.font=list(family=x.title.font.family, size=x.title.font.size, color=x.title.font.color)
    y.title.font=list(family=y.title.font.family, size=y.title.font.size, color=y.title.font.color)
    ytick.font=list(family=y.tick.font.family, size=y.tick.font.size, color=y.tick.font.color)
    xtick.font=list(family=x.tick.font.family, size=x.tick.font.size, color=x.tick.font.color)
    footer.font=list(family=footer.font.family, size=footer.font.size, color=footer.font.color)
    legend.font=list(family=legend.font.family, size=legend.font.size, color=legend.font.color)
    data.label.font=list(family=data.label.font.family, size=data.label.font.size, color=data.label.font.color)

    # Try to store name of variables
    if (!is.null(scatter.sizes) && is.null(scatter.sizes.name))
        scatter.sizes.name <- deparse(substitute(scatter.sizes))
    if (!is.null(scatter.labels) && is.null(scatter.labels.name))
        scatter.labels.name <- deparse(substitute(scatter.labels))
    if (!is.null(scatter.colors) && is.null(scatter.colors.name))
        scatter.colors.name <- deparse(substitute(scatter.colors))

    if (is.matrix(x) || is.data.frame(x))
    {
        is.valid.col <- function(n) {return (!is.null(n) && !is.na(n) && n > 0 && n <= ncol(x))}
        if (is.null(scatter.labels) && !is.null(rownames(x)))
            scatter.labels <- rownames(x)
        if (is.null(y) && is.valid.col(scatter.y.column))
        {
            if ((is.na(y.title) || nchar(y.title) == 0) && !is.null(colnames(x)))
                y.title <- colnames(x)[scatter.y.column]
            y <- x[,scatter.y.column]
        }
        if (is.null(scatter.sizes) && is.valid.col(scatter.sizes.column))
        {
            if (is.null(scatter.sizes.name) && !is.null(colnames(x)))
                scatter.sizes.name <- colnames(x)[scatter.sizes.column]
            scatter.sizes <- x[,scatter.sizes.column]
        }
        if (is.null(scatter.colors) && is.valid.col(scatter.colors.column))
        {
            if (is.null(scatter.colors.name) || nchar(scatter.colors.name) == 0)
                scatter.colors.name <- colnames(x)[scatter.colors.column]
            scatter.colors <- x[,scatter.colors.column]
        }
        if (((is.na(x.title) || nchar(x.title) == 0) && !is.null(colnames(x))) && is.valid.col(scatter.x.column))
            x.title <- colnames(x)[scatter.x.column]
        if (scatter.x.column <= 0 || scatter.x.column > ncol(x))
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

    # Remove NAs
    not.na <- !is.na(x) & !is.na(y)
    if (sum(not.na) != n)
        warning("Data points with missing values have been omitted.")
    n <- length(x)
    if (!is.null(scatter.sizes))
    {
        if (length(scatter.sizes) != n)
            stop("'scatter.sizes' should be a numeric vector with the same number of observations as 'x'.")
        if (any(!is.finite(scatter.sizes)))
        {
            warning("Some points omitted due to missing values in 'scatter.sizes'.")
            not.na <- not.na & is.finite(scatter.sizes)
        }
    }
    if (!is.null(scatter.colors))
    {
        if (length(scatter.colors) != n)
            stop("'scatter.colors' should be a vector with the same number of observations as 'x'.")
        if (any(is.na(scatter.colors)))
        {
            warning("Some points omitted due to missing values in 'scatter.colors'")
            not.na <- not.na & !is.na(scatter.colors)
        }
    }
    if (sum(not.na) == 0)
        stop("No non-NA points to plot.")
    if (any(not.na))
    {
        if (!is.null(scatter.labels))
            scatter.labels <- scatter.labels[which(not.na)]
        if (!is.null(x))
            x <- x[which(not.na)]
        if (!is.null(y))
            y <-y[which(not.na)]
        if (!is.null(scatter.sizes))
            scatter.sizes <- scatter.sizes[which(not.na)]
        if (!is.null(scatter.colors))
            scatter.colors <- scatter.colors[which(not.na)]
    }

    opacity <- 1
    n <- sum(not.na)
    if (!is.null(scatter.sizes))
    {
        sc.tmp <- sqrt(abs(as.numeric(scatter.sizes)))
        if (any(class(scatter.sizes) %in% c("Date", "POSIXct", "POSIXt")))
            scatter.sizes.scaled <- (sc.tmp - min(sc.tmp, na.rm=T))/diff(range(sc.tmp, na.rm=T)) * 50
        else
            scatter.sizes.scaled <- sc.tmp/max(sc.tmp, na.rm=T) * 50
        opacity <- 0.4
    }

    scatter.colors.as.numeric <- 0
    colorbar <- NULL
    groups <- rep("Series 1", n)
    if (!is.null(scatter.colors) && !scatter.colors.as.categorical)
    {
        # make colorscalebar
        col.fun <- colorRamp(colors)
        c.tmp <- rgb(col.fun((0:5)/5), maxColorValue=255)
        v.tmp <- seq(from=0, to=1, length=length(c.tmp))
        col.scale <- mapply(function(a,b)c(a,b), a=v.tmp, b=c.tmp, SIMPLIFY=F)

        # getting labels for all types
        if (is.character(scatter.colors))
            scatter.colors <- as.factor(scatter.colors)
        if (any(class(scatter.colors) %in% c("Date", "POSIXct", "POSIXt")))
        {
            tmp.seq <- seq(0, 1, length=5)
            colorbar <- list(tickmode="array", tickvals=tmp.seq,
                             ticktext=c(min(scatter.sizes) + diff(range(scatter.sizes)) * tmp.seq),
                             outlinewidth=0, tickfont=data.label.font)
        }
        else if (any(class(scatter.colors) == "factor"))
            colorbar <- list(tickmode="array", tickvals=seq(0, 1, length=nlevels(scatter.colors)),
                             ticktext=levels(scatter.colors), outlinewidth=0, tickfont=data.label.font)
        else
            colorbar <- list(outlinewidth = 0, tickfont=data.label.font)

        scatter.colors.as.numeric <- 1
        groups <- 1:n
        opacity <- 1
        col.tmp <- AsNumeric(scatter.colors, binary=FALSE)
        scatter.colors.scaled <- (col.tmp - min(col.tmp, na.rm=T))/diff(range(col.tmp, na.rm=T))
        scatter.colors.labels <- col.tmp
        if (any(class(scatter.colors) == "factor") || any(class(scatter.colors) %in% c("Date", "POSIXct", "POSIXt")))
            scatter.colors.labels <- scatter.colors.scaled
        colors <- rgb(col.fun(scatter.colors.scaled), maxColorValue=255)
    }
    if (!is.null(scatter.colors) && scatter.colors.as.categorical)
        groups <- as.character(scatter.colors)
    g.list <- unique(groups)
    num.groups <- length(g.list)
    num.series <- if (scatter.colors.as.numeric) 1 else num.groups

    # hovertext
    x.str <- if (is.numeric(x)) FormatAsReal(x, decimals = data.label.decimals) else as.character(x)
    y.str <- if (is.numeric(y)) FormatAsReal(y, decimals = data.label.decimals) else as.character(y)
    source.text <- paste0(scatter.labels, " (", x.tick.prefix, x.str, x.tick.suffix, ", ",
                          y.tick.prefix, y.str, y.tick.suffix, ")")
    if (!is.null(scatter.colors.name))
    {
        colors.str <- if (is.numeric(scatter.colors)) FormatAsReal(scatter.colors, decimals = data.label.decimals) else as.character(scatter.colors)
        source.text <- paste0(source.text, "<br>", scatter.colors.name, ": ", colors.str)
    }
    if (!is.null(scatter.sizes.name))
    {
        sizes.str <- if (is.numeric(scatter.sizes)) FormatAsReal(scatter.sizes, decimals=data.label.decimals) else as.character(scatter.sizes)
        source.text <- paste0(source.text, "<br>", scatter.sizes.name, ": ", sizes.str)
    }


    # other constants
    hover.mode <- if (tooltip.show) "closest" else FALSE
    legend.show <- legend.show && num.series > 1
    scatter.opacity <- if (!is.null(scatter.sizes)) 0.4 else 1
    series.mode <- if (is.null(series.line.width) || series.line.width == 0) "markers"
                   else "markers+lines"
    if (data.label.show)
        series.mode <- paste0(series.mode, "+text")
    series.marker.symbols <- if (is.null(series.marker.show) ||
                                 series.marker.show == "automatic" ||
                                 series.marker.show == "none")
        rep(100, 100) # disc
    else
        series.marker.show

    type <- "Scatterplot"
    legend <- setLegend("Scatterplot", legend.font, legend.ascending, legend.fill.color, legend.fill.opacity,
                        legend.border.color, legend.border.line.width)
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

    # Format axis labels
    if (is.null(y.tick.decimals))
        y.tick.decimals <- decimalsToDisplay(as.numeric(y))
    xtick <- setTicks(x.bounds.minimum, x.bounds.maximum, x.tick.distance, x.data.reversed)
    ytick <- setTicks(y.bounds.minimum, y.bounds.maximum, y.tick.distance, y.data.reversed)

    xlab.tmp <- if (!is.numeric(x)) as.character(x)
                else FormatAsReal(x, decimals=x.tick.decimals)
    ylab.tmp <- if (!is.numeric(y)) as.character(y)
                else FormatAsReal(y, decimals=y.tick.decimals)


    if (is.numeric(x))
    {
        x.abs.max <- max(abs(range(x, na.rm=T)), na.rm=T)
        if (!is.finite(x.abs.max) || x.abs.max == 0 || any(abs(range(x, na.rm=T))/x.abs.max < 1e-2))
            x.zero <- FALSE
    }
    if (is.numeric(y))
    {
        y.abs.max <- max(abs(range(y, na.rm=T)), na.rm=T)
        if (!is.finite(y.abs.max) || y.abs.max == 0 || any(abs(range(y, na.rm=T))/y.abs.max < 1e-2))
            y.zero <- FALSE
    }
    axisFormat <- formatLabels(list(x=xlab.tmp, y=ylab.tmp), type,
                       label.wrap, label.wrap.nchar, us.date.format)
    yaxis <- setAxis(y.title, "left", axisFormat, y.title.font,
                  y.line.color, y.line.width, y.grid.width, y.grid.color,
                  ytick, ytick.font, y.tick.angle, y.tick.mark.length,
                  y.tick.distance, y.tick.format.manual,
                  y.tick.decimals, y.tick.prefix, y.tick.suffix,
                  y.tick.show, y.zero, y.zero.line.width, y.zero.line.color,
                  y.hovertext.format.manual, y.hovertext.decimals)
    xaxis <- setAxis(x.title, "bottom", axisFormat, x.title.font,
                  x.line.color, x.line.width, x.grid.width, x.grid.color,
                  xtick, xtick.font, x.tick.angle, x.tick.mark.length, x.tick.distance, x.tick.format.manual,
                  x.tick.decimals, "", "", x.tick.show, FALSE, x.zero.line.width, x.zero.line.color,
                  x.hovertext.format.manual, x.hovertext.decimals, axisFormat$labels)

    # Work out margin spacing
    margins <- list(t = 20, b = 50, r = 60, l = 80, pad = 0)
    margins <- setMarginsForAxis(margins, axisFormat, xaxis)
    margins <- setMarginsForAxis(margins, ylab.tmp, yaxis)
    margins <- setMarginsForText(margins, title, subtitle, footer, title.font.size,
                                 subtitle.font.size, footer.font.size)
    margins <- setMarginsForLegend(margins, legend.show, legend)
    if (!is.null(margin.top))
        margins$top <- margin.top
    if (!is.null(margin.bottom))
        margins$bottom <- margin.bottom
    if (!is.null(margin.left))
        margins$left <- margin.left
    if (!is.null(margin.right))
        margins$right <- margin.right

    # Finalise text in margins
    footer.axis <- setFooterAxis(footer, footer.font, margins)
    subtitle.axis <- setSubtitleAxis(subtitle, subtitle.font, title, title.font)

    ## START PLOTTING
    p <- plot_ly(data.frame(x=x,y=y))
    for (ggi in 1:num.groups)
    {
        ind <- which(groups == g.list[ggi])
        tmp.size <- if (!is.null(scatter.sizes)) scatter.sizes.scaled[ind]
                 else series.marker.size

        # initialise marker/line settings
        line.obj <- if (is.null(series.line.width) || series.line.width == 0) NULL
                    else list(width=series.line.width, color=colors[ggi])
        if (ggi == 1 && scatter.colors.as.numeric)
            marker.obj <- list(size = tmp.size, sizemode = "diameter", opacity = opacity,
                            line = list(width = series.marker.border.width),
                            color = scatter.colors.labels, colorscale = col.scale,
                            showscale = T, colorbar = colorbar)
        else
            marker.obj <- list(size = tmp.size, sizemode = "diameter", opacity = opacity,
                            color = colors[ggi], line = list(width = series.marker.border.width))

        # add invisisble trace to force correct order
        if (ggi == 1 && is.factor(x))
        p <- add_trace(p, x = levels(x), y = minPosition(y, nlevels(x)), type = "scatter",
                       mode = "lines", hoverinfo = "none", showlegend = F, opacity = 0)
        if (ggi == 1 && is.factor(y))
        p <- add_trace(p, y = levels(y), x = minPosition(x, nlevels(y)), type = "scatter",
                       mode = "lines", hoverinfo = "none", showlegend = F, opacity = 0)

        # main trace
        p <- add_trace(p, x = x[ind], y = y[ind], name = g.list[ggi],
                showlegend=legend.show, legendgroup = if (num.series > 1) ggi else 1,
                textposition = data.label.position,
                marker = marker.obj, line = line.obj, text = source.text[ind],
                hoverinfo = if (num.series == 1) "text" else "name+text",
                type="scatter", mode=series.mode, symbols=series.marker.symbols)

        if (fit.type != "None" && num.series > 1)
        {
            tmp.fit <- fitSeries(x[ind], y[ind], fit.type, fit.ignore.last, xaxis$type)
            tmp.fname <- sprintf("%s: %s", fit.line.name, g.list[ggi])
            p <- add_trace(p, x = tmp.fit$x, y = tmp.fit$y, type = 'scatter', mode = "lines",
                      name = tmp.fname, legendgroup = ggi, showlegend = F,
                      line = list(dash = fit.line.type, width = fit.line.width,
                      color = fit.line.colors[ggi], shape = 'spline'))
        }
    }
    if (fit.type != "None" && num.series == 1)
    {
        tmp.fit <- fitSeries(x, y, fit.type, fit.ignore.last, xaxis$type)
        p <- add_trace(p, x = tmp.fit$x, y = tmp.fit$y, type = 'scatter', mode = 'lines',
                    name = fit.line.name, showlegend = F, line = list(dash = fit.line.type,
                    width = fit.line.width, color = fit.line.colors[1], shape = 'spline'))
    }
    p <- config(p, displayModeBar = modebar.show)
    p$sizingPolicy$browser$padding <- 0
    p <- layout(p,
        title = title,
        showlegend = legend.show,
        legend = legend,
        yaxis = yaxis,
        xaxis4 = footer.axis,
        xaxis3 = subtitle.axis,
        xaxis = xaxis,
        margin = margins,
        plot_bgcolor = toRGB(charting.area.fill.color, alpha = charting.area.fill.opacity),
        paper_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity),
        hovermode = hover.mode,
        titlefont = title.font,
        font = data.label.font
    )
    result <- list(plotly.plot = p)
    class(result) <- "StandardChart"
    result
}
