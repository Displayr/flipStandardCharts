#' Bar chart
#'
#' Plot bar chart 
#'
#' @param y A table, matrix, vector or data frame.
#' @param type One of "Bar", "Stacked Bar" or "100\% Stacked Bar"
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
#' @param y.title Character, y-axis title; defaults to chart input values;
#' to turn off set to "FALSE".
#' @param y.title.font.color y-axis title font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0,
#' max = 255)).
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
#' @param x.tick.suffix x-axis tick label suffix
#' @param x.tick.prefix x-axis tick label prefix
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
#' @param series.marker.colors Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' be reversed.
#' @param series.marker.opacity Opacity for series markers as an alpha value (0 to 1).
#' @param series.marker.size Size in pixels of marker
#' @param series.marker.border.width Width in pixels of border/line
#' around series markers; 0 is no line
#' @param series.marker.border.colors Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' @param series.marker.border.opacity Opacity of border/line around
#' series markers as an alpha value (0 to 1).
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
#' @param data.label.font.size Font size for data label.
#' @param data.label.font.color Font color as a named color
#' in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param data.label.decimals Number of decimal places to show in
#' data labels.
#' @param data.label.prefix Character; prefix for data values.
#' @param data.label.suffix Character; suffix for data values.
#' @param data.label.threshold The proportion of the total range below which
#' data labels should not be displayed. Only applicable for pie, bar and column
#' charts.
#' @param data.label.position Character; where to place the source data
#' value in relation to the marker icon.  Can be "top left", "top center", "top
#' right", "middle left", "middle center", "middle right", "bottom left",
#' "bottom center", "bottom right". Only applicable for line and area charts.
#' @param data.label.max.plot Integer; the maximum number of labels to show on a Labeled Scatterplot.
#' @param us.date.format Whether to apply the US convention when parsing dates.
#' @param ... Extra arguments that are ignored.
#' @examples
#' z <- structure(c(1L, 2L, 3L, 4L, 5L, 2L, 3L, 4L, 5L, 6L),  .Dim = c(5L, 2L), 
#'       .Dimnames = list(c("T", "U", "V", "W", "X"), c("A", "B")))
#' ColumnChart(z, type="Stacked Column")
#' @importFrom grDevices rgb
#' @importFrom flipChartBasics ChartColors
#' @importFrom plotly plot_ly config toRGB add_trace add_text layout hide_colorbar
#' @importFrom stats loess loess.control lm predict
#' @export
BarChart <- function(y = NULL,
                    type = "Bar",
                    fit.type = "None", # can be "Smooth" or anything else
                    fit.ignore.last = FALSE,
                    fit.line.type = "dot",
                    fit.line.width = 1,
                    fit.line.name = "Fitted",
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
                    colors = ChartColors(max(1, ncol(y))),
                    fit.line.colors = colors,
                    opacity = 1,
                    background.fill.color = rgb(255, 255, 255, maxColorValue = 255),
                    background.fill.opacity = 1,
                    charting.area.fill.color = background.fill.color,
                    charting.area.fill.opacity = 1,
                    legend.show = TRUE,
                    legend.fill = background.fill.color, # retained for backwards compatibility
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
                    y.title = "",
                    y.title.font.color = global.font.color,
                    y.title.font.family = global.font.family,
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
                    y.data.reversed = FALSE,
                    y.grid.width = 1,
                    y.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                    y.tick.show = TRUE,
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
                    x.data.reversed = FALSE,
                    x.grid.width = 1,
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
                    series.marker.colors = colors,
                    series.marker.opacity = 1,
                    series.marker.size = 6,
                    series.marker.border.width = 1,
                    series.marker.border.colors = colors,
                    series.marker.border.opacity = 1,
                    tooltip.show = TRUE,
                    modebar.show = FALSE,
                    bar.gap = 0.15,
                    data.label.show = FALSE,
                    data.label.font.family = global.font.family,
                    data.label.font.size = 10,
                    data.label.font.color = global.font.color,
                    data.label.decimals = 2, # Ignored in Labeled Bubble and Scatterplots
                    data.label.prefix = "",
                    data.label.suffix = "",
                    data.label.threshold = NULL,
                    data.label.position = "top middle",
                    data.label.max.plot = 50,
                    us.date.format = NULL,
                    ...)
{
    # Data checking
    chart.matrix <- as.matrix(y)
    is.stacked <- type != "Bar"
    is.hundred.percent.stacked <- type == "100% Stacked Bar"
    if (is.stacked && ncol(chart.matrix) == 0)
        stop(paste(type, "requires more than one series. Use Bar charts instead for this data."))
    if (is.stacked && (any(is.na(chart.matrix)) || any(chart.matrix < 0)))
        stop("Stacked charts cannot be produced with missing or negative values.")
    if (is.hundred.percent.stacked && any(rowSums(chart.matrix) == 0))
        stop("100% stacked charts cannot be produced with rows that do not contain positive values.")
    if (any(is.na(as.matrix(chart.matrix))))
        warnings("Missing values have been set to zero.")

    # Some minimal data cleaning
    # Assume formatting and Qtable/attribute handling already done
    data.label.mult <- 1
    if(is.hundred.percent.stacked)
    {
        chart.matrix <- cum.data(chart.matrix, "column.percentage")
        x.tick.format.manual <- "%" 
        data.label.suffix <- "%"
        data.label.mult <- 100
    }
    matrix.labels <- names(dimnames(chart.matrix))
    if (nchar(y.title) == 0 && length(matrix.labels) == 2)
        y.title <- matrix.labels[1]
    if (nchar(x.title) == 0 && length(matrix.labels) == 2)
        x.title <- matrix.labels[2]

    # Constants
    hover.mode <- if (tooltip.show) "closest" else FALSE
    barmode <- if (is.stacked) "stack" else ""
    legend.group <- if (is.stacked) "grouped" else ""
    if (is.null(opacity))
        opacity <- 1
    eval(colors) # not sure why, but this is necessary for bars to appear properly

    title.font=list(family=title.font.family, size=title.font.size, color=title.font.color)
    subtitle.font=list(family=subtitle.font.family, size=subtitle.font.size, color=subtitle.font.color)
    x.title.font=list(family=x.title.font.family, size=x.title.font.size, color=x.title.font.color)
    y.title.font=list(family=y.title.font.family, size=y.title.font.size, color=y.title.font.color)
    ytick.font=list(family=y.tick.font.family, size=y.tick.font.size, color=y.tick.font.color)
    xtick.font=list(family=x.tick.font.family, size=x.tick.font.size, color=x.tick.font.color)
    footer.font=list(family=footer.font.family, size=footer.font.size, color=footer.font.color)
    legend.font=list(family=legend.font.family, size=legend.font.size, color=legend.font.color)
    data.label.font=list(family=data.label.font.family, size=data.label.font.size, color=data.label.font.color)

    if (ncol(chart.matrix) == 1)
        legend.show <- FALSE
    legend <- setLegend(type, legend.font, legend.ascending, legend.fill.color, legend.fill.opacity,
                        legend.border.color, legend.border.line.width)
    footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate=FALSE)

    # Format axis labels
    if (is.null(x.tick.decimals))
        x.tick.decimals <- decimalsToDisplay(as.numeric(chart.matrix))
    tmp.label <- sprintf(paste0("%s%.", data.label.decimals, "f%s"), 
                data.label.prefix, max(chart.matrix), data.label.suffix)
    xtick <- setTicks(x.bounds.minimum, x.bounds.maximum, x.tick.distance, x.data.reversed,
                  data = if (data.label.show && type == "Bar") chart.matrix else NULL, type = type, 
                  labels = tmp.label, label.font.size = data.label.font.size)
    ytick <- setTicks(y.bounds.minimum, y.bounds.maximum, y.tick.distance, !y.data.reversed)
    axisFormat <- formatLabels(chart.matrix, type, label.wrap, label.wrap.nchar, us.date.format) 
    rownames(chart.matrix) <- axisFormat$labels
    
    yaxis <- setAxis(y.title, "left", axisFormat, y.title.font, 
                  y.line.color, y.line.width, y.grid.width, y.grid.color,
                  ytick, ytick.font, y.tick.angle, y.tick.mark.length, y.tick.distance, y.tick.format.manual, 
                  y.tick.decimals, "", "", y.tick.show, y.zero, y.zero.line.width, y.zero.line.color, 
                  y.hovertext.format.manual, y.hovertext.decimals)
    xaxis <- setAxis(x.title, "bottom", axisFormat, x.title.font,
                  x.line.color, x.line.width, x.grid.width, x.grid.color,
                  xtick, xtick.font, x.tick.angle, x.tick.mark.length, x.tick.distance, x.tick.format.manual,
                  x.tick.decimals, x.tick.prefix, x.tick.suffix,
                  x.tick.show, FALSE, x.zero.line.width, x.zero.line.color, 
                  x.hovertext.format.manual, x.hovertext.decimals)

    # Work out margin spacing 
    margins <- list(t = 20, b = 50, r = 60, l = 80, pad = 0)
    margins <- setMarginsForAxis(margins, axisFormat, yaxis)
    margins <- setMarginsForAxis(margins, axisFormat, xaxis)
    margins <- setMarginsForText(margins, title, subtitle, footer, title.font.size, 
                                 subtitle.font.size, footer.font.size)
    margins <- setMarginsForLegend(margins, legend.show, NULL)
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
 
    # Data label annotations
    data.annotations <- NULL
    if (data.label.show)
        data.annotations <- dataLabelAnnotation(chart.matrix = chart.matrix,
                            annotations = NULL,
                            data.label.mult = data.label.mult,
                            bar.decimals = data.label.decimals,
                            bar.prefix = data.label.prefix,
                            bar.suffix = data.label.suffix,
                            barmode = barmode,
                            swap.axes.and.data = TRUE,
                            bar.gap = bar.gap,
                            display.threshold = data.label.threshold,
                            dates = axisFormat$ymd)

    ## Initiate plotly object
    p <- plot_ly(as.data.frame(chart.matrix))
    x.labels <- rownames(chart.matrix)
    y.labels <- colnames(chart.matrix)
    yaxis2 <- NULL

    ## Add a trace for each col of data in the matrix
    for (i in 1:ncol(chart.matrix))
    {
        y <- as.numeric(chart.matrix[, i])
        x <- x.labels

        marker <- list(size = series.marker.size, color = toRGB(colors[i], alpha = opacity),
                    line = list(color = toRGB(series.marker.border.colors[i], 
                      alpha = series.marker.border.opacity),
                      width = series.marker.border.width))
                
        # add invisible line to force all categorical labels to be shown
        if (!is.stacked && i == 1)
            p <- add_trace(p, x=rep(min(y,na.rm=T), length(y)), y=x,
                           type="scatter", mode="lines",
                           hoverinfo="none", showlegend=F, opacity=0)

        # this is the main trace for each data series 
        tmp.group <- if (legend.group == "") paste("group", i) else legend.group
        p <- add_trace(p, x = y, y = x, type = "bar", orientation = "h", marker = marker,
                       name  =  y.labels[i], legendgroup  =  tmp.group,
                       hoverinfo  =  if(ncol(chart.matrix) > 1) "x+y+name" else "x+y")

        if (fit.type != "None" && !is.stacked)
        {
            tmp.fit <- fitSeries(x, y, fit.type, fit.ignore.last, yaxis$type)
            tmp.fname <- if (ncol(chart.matrix) == 1)  fit.line.name
                         else sprintf("%s: %s", fit.line.name, y.labels[i])
            p <- add_trace(p, x = tmp.fit$y, y = tmp.fit$x, type = 'scatter', mode = "lines",
                      name = tmp.fname, legendgroup = tmp.group, showlegend = F,
                      line = list(dash = fit.line.type, width = fit.line.width,
                      color = fit.line.colors[i]))
        }

        if (data.label.show && !is.stacked)
        {
            y.range <- getRange(x, data.annotations$y, yaxis, axisFormat)
            yaxis2 <- list(overlaying = "y", visible = FALSE, range = y.range)
            x.diff <- diff(range(data.annotations$x))/100
            p <- add_text(p, yaxis = "y2", x = data.annotations$x[,i] + x.diff, y = data.annotations$y[,i],
                      text = data.annotations$text[,i], textposition = "middle right",
                      textfont = data.label.font, hoverinfo = "none",
                      showlegend = FALSE, legendgroup = tmp.group)
        }
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
        yaxis2 = yaxis2,
        xaxis = xaxis,
        margin = margins,
        plot_bgcolor = toRGB(charting.area.fill.color, alpha = charting.area.fill.opacity),
        paper_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity),
        hovermode = hover.mode,
        titlefont = title.font,
        font = data.label.font,
        annotations = if (is.stacked) data.annotations else NULL,
        bargap = bar.gap,
        barmode = barmode
    )
    result <- list(plotly.plot = p)
    class(result) <- "StandardChart"
    result
}

