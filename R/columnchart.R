#' Column
#'
#' Column chart
#'
#' @param x Input data may be a matrix or a vector, containing the height of the columns
#' to be plotted, with the name/rownames used as the column names of the chart. Numeric and date labels
#' will be parsed automatically.
#' @param type One of "Column", "Stacked Column" or "100\% Stacked Column"
#' @param average.series y-values of additional data series which is shown as a line. Used by \code{SmallMultiples}.
#' @param average.color Color of the \code{average.series} as a hex code or string
#' @param fit.type Character; type of line of best fit. Can be one of "None", "Linear", "LOESS",
#'          "Friedman's super smoother" or "Cubic spline".
#' @param fit.ignore.last Logical; whether to ignore the last data point in the fit.
#' @param fit.line.type Character; One of "solid", "dot", "dash, "dotdash", or length of dash "2px", "5px".
#' @param fit.line.width Numeric; Line width of line of best fit.
#' @param fit.line.name Character; Name of the line of best fit, which will appear in the hovertext.
#' @param fit.line.opacity Opacity of trend line as an alpha value (0 to 1).
#' @param fit.CI.show Show 95\% confidence interval.
#' @param fit.CI.opacity Opacity of confidence interval ribbon as an alpha value (0 to 1).
#' @param fit.CI.colors Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.

#' @param title Character; chart title.
#' @param title.font.family Character; title font family. Can be "Arial Black",
#' "Arial", "Comic Sans MS", "Courier New", "Georgia", "Impact",
#' "Lucida Console", "Lucida Sans Unicode", "Marlett", "Symbol", "Tahoma",
#' "Times New Roman", "Trebuchet MS", "Verdana", "Webdings"
#' @param title.font.color Title font color as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param title.font.size Integer; Title font size; default = 10.
#' @param subtitle Character
#' @param subtitle.font.color subtitle font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param subtitle.font.family Character; subtitle font family
#' @param subtitle.font.size Integer; subtitle font size
#' @param footer Character
#' @param footer.font.color footer font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param footer.font.family Character; footer font family
#' @param footer.font.size Integer; footer font size
#' @param footer.wrap Logical; whether the footer text should be wrapped.
#' @param footer.wrap.nchar Number of characters (approximately) in each line of the footer when \code{footer.wordwrap} \code{TRUE}.
#' @param grid.show Logical; whether to show grid lines.
#' @param opacity Opacity of bars as an alpha value (0 to 1).
#' @param colors Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
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
#' @param charting.area.fill.opacity Charting area background opacity as an alpha value (0 to 1).
#' @param legend.show Logical; show the legend.
#' @param legend.fill.color Legend fill color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param legend.fill.opacity Legend fill opacity as an alpha value (0 to 1).
#' @param legend.ascending Logical; TRUE for ascending, FALSE for descending.
#' By default, we set it to to FALSE if the chart is stacked and TRUE otherwise.
#' @param legend.border.color Legend border color as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param legend.border.line.width Width in pixels of the border
#' around the legend.  0 = no border.
#' @param legend.position.x A numeric controlling the position of the legend.
#'   Values range from -0.5 (left) to 1.5 (right).
#' @param legend.position.y A numeric controlling the position of the legend.
#'   Values range from 0 (bottom) to 1 (top).
#' @param legend.font.color Legend font color as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param legend.font.family Character; legend font family.
#' @param legend.font.size Integer; Legend font size.
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
#' @param y.title Character, y-axis title; defaults to chart input values;
#' to turn off set to "FALSE".
#' @param y.title.font.color y-axis title font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0,
#' max = 255)).
#' @param y.title.font.family Character; y-axis title font family
#' @param y.title.font.size Integer; y-axis title font size
#' @param y.line.width y-axis line width in pixels (0 = no line).
#' @param y.line.color y-axis line color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param y.tick.mark.length Length of tick marks in pixels. Ticks are only shown when \code{y.line.width > 0}.
#' @param y.bounds.minimum Minimum of range for plotting; NULL = no manual range set.
#' @param y.bounds.maximum Maximum of range for plotting; NULL = no manual range set.
#' @param y.tick.distance Distance between tick marks. Requires that \code{values.bounds.minimum} and \code{values.bounds.maximum} have been set.
#' @param y.zero Whether the y-axis should include zero.
#' @param y.zero.line.width Width in pixels of zero line;
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
#' @param y.tick.format A string representing a d3 formatting code.
#' See https://github.com/d3/d3/blob/master/API.md#number-formats-d3-format
#' @param y.hovertext.format A string representing a d3 formatting code
#' See https://github.com/d3/d3/blob/master/API.md#number-formats-d3-format
#' @param y.tick.angle y-axis tick label angle in degrees.
#' 90 = vertical; 0 = horizontal
#' @param y.tick.font.color y-axis tick label font color as a named color
#' in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param y.tick.font.family Character; y-axis tick label font family
#' @param y.tick.font.size Integer; y-axis tick label font size
#' @param x.title Character, x-axis title; defaults to chart input values;
#' to turn off set to "FALSE".
#' @param x.title.font.color x-axis title font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param x.title.font.family Character; x-axis title font family
#' @param x.title.font.size Integer; x-axis title font size
#' @param x.line.width x-axis line in pixels, 0 = no line
#' @param x.line.color x-axis line color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param x.tick.marks Character; whether and where to show tick marks on the
#' x-axis.  Can be "outside", "inside", "none"
#' @param x.tick.mark.length Length of tick marks in pixels.
#' @param x.bounds.minimum Minimum of range for plotting;
#' NULL = no manual range set.  Must be less than x.bounds.maximum
#' @param x.bounds.maximum Maximum of range for
#' plotting; NULL = no manual range set.  Must be greater than x.bounds.minimum
#' @param x.tick.distance Tick mark distance in
#' x-axis units between minimum and maximum for plotting; NULL = no manual
#' range set.
#' @param x.zero Whether the x-axis should include zero.
#' @param x.zero.line.width Width in pixels of zero line.
#' @param x.zero.line.color Color of horizontal zero (origin) line as a named
#' color in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param x.data.reversed Logical; whether to reverse x-axis or not
#' @param x.grid.width Width of y-grid lines in pixels; 0 = no line
#' @param x.grid.color Color of y-grid lines as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param x.tick.show Whether to display the x-axis tick labels
#' @param x.tick.suffix x-axis tick label suffix
#' @param x.tick.prefix x-axis tick label prefix
#' @param x.tick.format A string representing a d3 formatting code.
#' See https://github.com/mbostock/d3/wiki/Formatting#numbers
#' @param x.hovertext.format A string representing a d3 formatting code.
#' See https://github.com/mbostock/d3/wiki/Formatting#numbers
#' @param x.tick.angle x-axis tick label angle in degrees.
#' 90 = vertical; 0 = horizontal
#' @param x.tick.font.color X-axis tick label font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param x.tick.font.family Character; x-axis tick label font family
#' @param x.tick.font.size Integer; x-axis tick label font size
#' @param x.tick.label.wrap Logical; whether to wrap long labels on the x-axis.
#' @param x.tick.label.wrap.nchar Integer; number of characters in each line when \code{label.wrap} is \code{TRUE}.
#' @param marker.border.width Width in pixels of border/line
#' around series bars; 0 is no line
#' @param marker.border.colors Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' @param marker.border.opacity Opacity of border around bars as an alpha value (0 to 1).
#' @param tooltip.show Logical; whether to show a tooltip on hover.
#' @param modebar.show Logical; whether to show the zoom menu buttons or not.
#' @param global.font.family Character; font family for all occurrences of any
#' font attribute for the chart unless specified individually.
#' @param global.font.color Global font color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. #' rgb(0, 0, 0, maxColorValue = 255)).
#' @param bar.gap Chart proportion between each bar or column if using
#' bar or column charts, or between each cluster of bars or columns.
#' @param data.label.show Logical; whether to show data labels.
#' @param data.label.font.family Character; font family for data label.
#' @param data.label.font.size Integer; Font size for data label.px.
#' @param data.label.font.color Font color as a named color
#' in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param data.label.format A string representing a d3 formatting code.
#' See https://github.com/mbostock/d3/wiki/Formatting#numbers
#' @param data.label.prefix Character; prefix for data values.
#' @param data.label.suffix Character; suffix for data values.
#' @param data.label.threshold The proportion of the total range below which
#' data labels should not be displayed. Only applicable for pie, bar and column
#' charts.
#' @examples
#' z <- structure(c(1L, 2L, 3L, 4L, 5L, 2L, 3L, 4L, 5L, 6L),  .Dim = c(5L, 2L),
#'       .Dimnames = list(c("T", "U", "V", "W", "X"), c("A", "B")))
#' Column(z, type="Stacked Column")
#' @importFrom grDevices rgb
#' @importFrom flipChartBasics ChartColors
#' @importFrom plotly plot_ly config toRGB add_trace add_text layout hide_colorbar
#' @importFrom stats loess loess.control lm predict
#' @export
Column <- function(x,
                    colors = ChartColors(max(1, ncol(x), na.rm = TRUE)),
                    opacity = NULL,
                    type = "Column",
                    fit.type = "None", # can be "Smooth" or anything else
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
                    background.fill.color = rgb(255, 255, 255, maxColorValue = 255),
                    background.fill.opacity = 0,
                    charting.area.fill.color = background.fill.color,
                    charting.area.fill.opacity = 0,
                    legend.show = TRUE,
                    legend.position.x = 1.02,
                    legend.position.y = 1,
                    legend.fill.color = background.fill.color,
                    legend.fill.opacity = 0,
                    legend.border.color = rgb(44, 44, 44, maxColorValue = 255),
                    legend.border.line.width = 0,
                    legend.font.color = global.font.color,
                    legend.font.family = global.font.family,
                    legend.font.size = 10,
                    legend.ascending = NA,
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
                    marker.border.width = 1,
                    marker.border.colors = colors,
                    marker.border.opacity = NULL,
                    tooltip.show = TRUE,
                    modebar.show = FALSE,
                    bar.gap = 0.15,
                    data.label.show = FALSE,
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
    # Data checking
    chart.matrix <- checkMatrixNames(x)
    if (!is.numeric(chart.matrix))
        stop("Input data should be numeric.")
    x.labels.full <- rownames(chart.matrix)

    is.stacked <- grepl("Stacked", type, fixed = TRUE)
    is.hundred.percent.stacked <- grepl("100% Stacked", type, fixed = TRUE)
    if (is.stacked && ncol(chart.matrix) < 2)
        stop(paste(type, "requires more than one series. Use Column Chart instead for this data."))
    if (is.stacked && (any(is.na(chart.matrix)) || any(chart.matrix < 0)))
        stop("Stacked charts cannot be produced with missing or negative values.")
    if (is.hundred.percent.stacked && any(rowSums(chart.matrix) == 0))
        stop("100% stacked charts cannot be produced with rows that do not contain positive values.")
    if (any(!is.finite(chart.matrix)))
        warning("Missing values have been set to zero.")

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

    matrix.labels <- names(dimnames(chart.matrix))
    if (nchar(x.title) == 0 && length(matrix.labels) == 2)
        x.title <- matrix.labels[1]

    # Constants
    hover.mode <- if (tooltip.show) "closest" else FALSE
    barmode <- if (is.stacked) "stack" else ""
    if (is.null(opacity))
        opacity <- if (fit.type == "None") 1 else 0.6
    if (is.null(marker.border.opacity))
        marker.border.opacity <- opacity
    eval(colors) # not sure why, but this is necessary for bars to appear properly

    title.font = list(family = title.font.family, size = title.font.size, color = title.font.color)
    subtitle.font = list(family = subtitle.font.family, size = subtitle.font.size, color = subtitle.font.color)
    x.title.font = list(family = x.title.font.family, size = x.title.font.size, color = x.title.font.color)
    y.title.font = list(family = y.title.font.family, size = y.title.font.size, color = y.title.font.color)
    ytick.font = list(family = y.tick.font.family, size = y.tick.font.size, color = y.tick.font.color)
    xtick.font = list(family = x.tick.font.family, size = x.tick.font.size, color = x.tick.font.color)
    footer.font = list(family = footer.font.family, size = footer.font.size, color = footer.font.color)
    legend.font = list(family = legend.font.family, size = legend.font.size, color = legend.font.color)
    data.label.font = list(family = data.label.font.family, size = data.label.font.size, color = data.label.font.color)

    if (ncol(chart.matrix) == 1)
        legend.show <- FALSE
    legend <- setLegend(type, legend.font, legend.ascending, legend.fill.color, legend.fill.opacity,
                        legend.border.color, legend.border.line.width,
                        legend.position.x, legend.position.y, y.data.reversed)
    footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate = FALSE)

    # Format axis labels
    # Turn off autorange if data labels are shown
    if (data.label.show && is.null(x.bounds.minimum))
    {
        xlab.tmp <- rownames(chart.matrix)
        tmp.range <- getRange(xlab.tmp, NULL, NULL)
        x.bounds.minimum <- tmp.range[1]
        x.bounds.maximum <- tmp.range[2]
        if (x.zero && all(!is.na(suppressWarnings(as.numeric(xlab.tmp)))))
        {
            x.bounds.minimum  <- min(0, x.bounds.minimum)
            x.bounds.maximum <- max(0, x.bounds.maximum)
        }
    }
    y.range <- setValRange(y.bounds.minimum, y.bounds.maximum, chart.matrix, is.null(y.tick.distance))
    ytick <- setTicks(y.range$min, y.range$max, y.tick.distance, y.data.reversed)
    xtick <- setTicks(x.bounds.minimum, x.bounds.maximum, x.tick.distance, x.data.reversed)

    axisFormat <- formatLabels(chart.matrix, type, x.tick.label.wrap, x.tick.label.wrap.nchar,
                        x.tick.format, y.tick.format)

    yaxis <- setAxis(y.title, "left", axisFormat, y.title.font,
                  y.line.color, y.line.width, y.grid.width * grid.show, y.grid.color,
                  ytick, ytick.font, y.tick.angle, y.tick.mark.length, y.tick.distance, y.tick.format,
                  y.tick.prefix, y.tick.suffix,
                  y.tick.show, y.zero, y.zero.line.width, y.zero.line.color,
                  y.hovertext.format)
    xaxis <- setAxis(x.title, "bottom", axisFormat, x.title.font,
                  x.line.color, x.line.width, x.grid.width * grid.show, x.grid.color,
                  xtick, xtick.font, x.tick.angle, x.tick.mark.length, x.tick.distance, x.tick.format,
                  x.tick.prefix, x.tick.suffix, x.tick.show, x.zero, x.zero.line.width, x.zero.line.color,
                  x.hovertext.format, axisFormat$labels, num.series = NCOL(chart.matrix))

    # Work out margin spacing
    margins <- list(t = 20, b = 20, r = 60, l = 80, pad = 0)
    margins <- setMarginsForAxis(margins, axisFormat, xaxis)
    margins <- setMarginsForText(margins, title, subtitle, footer, title.font.size,
                                 subtitle.font.size, footer.font.size)
    margins <- setMarginsForLegend(margins, legend.show, legend, colnames(chart.matrix))
    margins <- setCustomMargins(margins, margin.top, margin.bottom, margin.left,
                    margin.right, margin.inner.pad)

    # Data label annotations
    data.annotations <- NULL
    if (data.label.show)
        data.annotations <- dataLabelPositions(chart.matrix = chart.matrix,
                            annotations = NULL,
                            data.label.mult = data.label.mult,
                            bar.decimals = data.label.decimals,
                            bar.prefix = data.label.prefix,
                            bar.suffix = data.label.suffix,
                            barmode = barmode,
                            swap.axes.and.data = FALSE,
                            bar.gap = bar.gap,
                            display.threshold = data.label.threshold,
                            dates = axisFormat$ymd,
                            reversed = any(getSign(chart.matrix, yaxis) < 0),
                            font = data.label.font)

    ## Initiate plotly object
    p <- plot_ly(as.data.frame(chart.matrix))
    x.labels <- axisFormat$labels
    y.labels <- colnames(chart.matrix)
    xaxis2 <- NULL

    ## Add a trace for each col of data in the matrix
    for (i in 1:ncol(chart.matrix))
    {
        y <- as.numeric(chart.matrix[, i])
        x <- x.labels
        marker <- list(color = toRGB(colors[i], alpha = opacity),
                      line = list(color = toRGB(marker.border.colors[i],
                      alpha = marker.border.opacity),
                      width = marker.border.width))

        # Add invisible line to force all categorical labels to be shown
        # Type "scatter" ensures y-axis tick bounds are treated properly
        # but it also adds extra space next to the y-axis
        if (!is.stacked && i == 1)
            p <- add_trace(p, x = x, y = rep(min(y,na.rm = T), length(x)),
                           mode = if (notAutoRange(yaxis)) "markers" else "lines", 
                           type = "scatter", cliponaxis = TRUE,
                           hoverinfo = "none", showlegend = FALSE, opacity = 0)

        # this is the main trace for each data series
        p <- add_trace(p, x = x, y = y, type = "bar", orientation = "v", marker = marker,
                       name  =  y.labels[i], legendgroup = i,
                       text = autoFormatLongLabels(x.labels.full, wordwrap=T, truncate=F),
                       hoverinfo  = setHoverText(xaxis, chart.matrix))
        if (fit.type != "None" && is.stacked && i == 1)
            warning("Line of best fit not shown for stacked charts.")
        if (fit.type != "None" && !is.stacked)
        {
            tmp.fit <- fitSeries(x, y, fit.type, fit.ignore.last, xaxis$type, fit.CI.show)
            tmp.fname <- if (ncol(chart.matrix) == 1)  fit.line.name
                         else sprintf("%s: %s", fit.line.name, y.labels[i])
            p <- add_trace(p, x = tmp.fit$x, y = tmp.fit$y, type = 'scatter', mode = "lines",
                      name = tmp.fname, legendgroup = i, showlegend = FALSE, opacity = fit.line.opacity,
                      line = list(dash = fit.line.type, width = fit.line.width,
                      color = fit.line.colors[i], shape = 'spline'), opacity = fit.line.opacity)
            if (fit.CI.show && !is.null(tmp.fit$lb))
            {
                p <- add_trace(p, x = tmp.fit$x, y = tmp.fit$lb, type = 'scatter',
                        mode = 'lines', name = "Lower bound of 95%CI",
                        showlegend = FALSE, legendgroup = i,
                        line=list(color=fit.CI.colors[i], width=0, shape='spline'))
                p <- add_trace(p, x = tmp.fit$x, y = tmp.fit$ub, type = 'scatter',
                        mode = 'lines', name = "Upper bound of 95% CI",
                        fill = "tonexty", fillcolor = toRGB(fit.CI.colors[i], alpha = fit.CI.opacity),
                        showlegend = FALSE, legendgroup = i,
                        line = list(color=fit.CI.colors[i], width=0, shape='spline'))
            }
        }

        # Only used for small multiples
        if (!is.null(average.series))
            p <- add_trace(p, x = x, y = average.series, name = "Average",
                    type = "scatter", mode = "lines", showlegend = FALSE,
                    line = list(color = average.color))


        if (data.label.show && !is.stacked)
        {
            x.range <- getRange(x, xaxis, axisFormat)
            y.sign <- getSign(data.annotations$y[,i], yaxis)
            xaxis2 <- list(overlaying = "x", visible = FALSE, range = x.range)
            p <- add_text(p, xaxis = "x2", x = data.annotations$x[,i],
                      y = data.annotations$y[,i], cliponaxis = FALSE,
                      text = data.annotations$text[,i], textfont = data.label.font,
                      textposition = ifelse(y.sign >= 0, "top center", "bottom center"),
                      showlegend = FALSE, legendgroup = i, hoverinfo = "none")
        }
    }
    annotations <- NULL
    if (data.label.show && is.stacked)
        annotations <- data.annotations
    n <- length(annotations)
    annotations[[n+1]] <- setFooter(footer, footer.font, margins)
    annotations[[n+2]] <- setSubtitle(subtitle, subtitle.font, margins)

    p <- config(p, displayModeBar = modebar.show)
    p$sizingPolicy$browser$padding <- 0
    p <- layout(p,
        title = title,
        showlegend = legend.show,
        legend = legend,
        yaxis = yaxis,
        xaxis2 = xaxis2,
        xaxis = xaxis,
        margin = margins,
        plot_bgcolor = toRGB(charting.area.fill.color, alpha = charting.area.fill.opacity),
        paper_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity),
        hovermode = hover.mode,
        titlefont = title.font,
        font = data.label.font,
        annotations =  annotations,
        bargap = bar.gap,
        barmode = barmode
    )
    result <- list(htmlwidget = p)
    class(result) <- "StandardChart"
    result
}

