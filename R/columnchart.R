#' Column
#'
#' Column chart
#'
#' @param x Input data may be a matrix or a vector, containing the height of the columns
#' to be plotted, with the name/rownames used as the column names of the chart. Numeric and date labels
#' will be parsed automatically.
#' @param x2 Optional input data which is shown as lines on top of the column chart.
#'  a separate axis for these lines will be shown on the right.
#' @param type One of "Column", "Stacked Column" or "100\% Stacked Column"
#' @param annotation.list Optional list of annotations to modify the data labels.
#' @param overlay.annotation.list Optional list of annotations that is overlayed on top of the chart.
#' @param average.series y-values of additional data series which is shown as a line. Used by \code{SmallMultiples}.
#' @param average.color Color of the \code{average.series} as a hex code or string
#' @param fit.type Character; type of line of best fit. Can be one of "None", "Linear", "LOESS",
#'          "Friedman's super smoother", "Cubic spline", "Moving average", "Centered moving average".
#' @param fit.window.size Integer; Use to determine how the average is computed when \code{fit.type}
#'          "Moving average" or "Centered moving average".
#' @param fit.ignore.last Logical; whether to ignore the last data point in the fit.
#' @param fit.line.type Character; One of "solid", "dot", "dash, "dotdash", or length of dash "2px", "5px".
#' @param fit.line.width Numeric; Line width of line of best fit.
#' @param fit.line.name Character; Name of the line of best fit, which will appear in the hovertext.
#' @param fit.line.colors Character; a vector containing one or more colors specified as hex codes.
#' @param fit.line.opacity Opacity of trend line as an alpha value (0 to 1).
#' @param fit.CI.show Show 95\% confidence interval.
#' @param fit.CI.opacity Opacity of confidence interval ribbon as an alpha value (0 to 1).
#' @param fit.CI.colors Character; a vector containing one or more colors specified as hex codes.
#' @param title Character; chart title.
#' @param title.font.family Character; title font family. Can be "Arial Black",
#' "Arial", "Comic Sans MS", "Courier New", "Georgia", "Impact",
#' "Lucida Console", "Lucida Sans Unicode", "Marlett", "Symbol", "Tahoma",
#' "Times New Roman", "Trebuchet MS", "Verdana", "Webdings"
#' @param title.font.color Title font color as a named color in character
#' format (e.g. "black") or a hex code.
#' @param title.font.size Integer; Title font size; default = 10.
#' @param subtitle Character
#' @param subtitle.font.color subtitle font color as a named color in
#' character format (e.g. "black") or an a hex code.
#' @param subtitle.font.family Character; subtitle font family
#' @param subtitle.font.size Integer; subtitle font size
#' @param footer Character
#' @param footer.font.color footer font color as a named color in
#' character format (e.g. "black") or an a hex code.
#' @param footer.font.family Character; footer font family
#' @param footer.font.size Integer; footer font size
#' @param footer.wrap Logical; whether the footer text should be wrapped.
#' @param footer.wrap.nchar Number of characters (approximately) in each
#' line of the footer when \code{footer.wrap} \code{TRUE}.
#' @param grid.show Logical; whether to show grid lines.
#' @param opacity Opacity of bars as an alpha value (0 to 1).
#' @param colors Character; a vector containing one or more colors specified as hex codes.
#' @param x2.colors Character; a vector containing one or more colors for \code{x2}
#'  specified as hex codes.
#' @param background.fill.color Background color in character format (e.g. "black") or a hex code.
#' @param background.fill.opacity Background opacity as an alpha value (0 to 1).
#' @param charting.area.fill.color Charting area background color as
#' a named color in character format (e.g. "black") or a hex code.
#' @param charting.area.fill.opacity Charting area background opacity as an alpha value (0 to 1).
#' @param legend.show Controls whether legend is shown. This can be a logical (\code{TRUE} or \code{FALSE});
#'  or a string ("Show" or "Hide"). If it is \code{TRUE} or \code{NA} (the default), a legend will be
#'  shown only if there is more than one data series. To force a legend to be shown even with 1
#'  data series, use "Show" instead.
#' @param legend.wrap Logical; whether the legend text should be wrapped.
#' @param legend.wrap.nchar Number of characters (approximately) in each
#' line of the legend when \code{legend.wrap} \code{TRUE}.
#' @param legend.fill.color Legend fill color as a named color in character format
#' (e.g. "black") or a hex code.
#' @param legend.fill.opacity Legend fill opacity as an alpha value (0 to 1).
#' @param legend.ascending Logical; TRUE for ascending, FALSE for descending.
#' By default, we set it to to FALSE if the chart is stacked and TRUE otherwise.
#' @param legend.border.color Legend border color as a named color in character
#' format (e.g. "black") or a hex code.
#' @param legend.border.line.width Width in pixels of the border
#' around the legend.  0 = no border.
#' @param legend.position.x A numeric controlling the position of the legend.
#'   Values range from -0.5 (left) to 1.5 (right).
#' @param legend.position.y A numeric controlling the position of the legend.
#'   Values range from 0 (bottom) to 1 (top).
#' @param legend.font.color Legend font color as a named color in character
#' format (e.g. "black") or a hex code.
#' @param legend.font.family Character; legend font family.
#' @param legend.font.size Integer; Legend font size.
#' @param legend.orientation Character; One of 'Vertical' or 'Horizontal'
#' @param margin.autoexpand Logical; Whether extra space can be added to the margins
#'      to allow space for axis/legend/data labels or other chart elements.

#' @param margin.top Margin between plot area and the top of the graphic in pixels
#' @param margin.bottom Margin between plot area and the bottom of the graphic in pixels
#' @param margin.left Margin between plot area and the left of the graphic in pixels
#' @param margin.right Margin between plot area and the right of the graphic in pixels
#' @param margin.inner.pad Padding in pixels between plot proper
#' and axis lines
#' @param y.title Character, y-axis title; defaults to chart input values;
#' to turn off set to "FALSE".
#' @param y.title.font.color y-axis title font color as a named color in
#' character format (e.g. "black") or a hex code.
#' @param y.title.font.family Character; y-axis title font family
#' @param y.title.font.size Integer; y-axis title font size
#' @param y.line.width y-axis line width in pixels (0 = no line).
#' @param y.line.color y-axis line color as a named color in character format
#' (e.g. "black") or a hex code.
#' @param y.tick.mark.length Length of tick marks in pixels. Ticks are only shown when \code{y.line.width > 0}.
#' @param y.bounds.minimum Minimum of range for plotting; For a date axis this should be supplied as a date string.
#'  For a categorical axis, the index of the category (0-based) should be used.
#' @param y.bounds.maximum Maximum of range for plotting; NULL = no manual range set.
#' @param y.tick.distance Distance between tick marks. Requires that \code{y.bounds.minimum} and \code{y.bounds.maximum} have been set.
#' @param y.tick.maxnum Maximum number of ticks shown on the axis.
#'  This setting is ignored if \code{y.tick.distance} is set or
#'  if the axis is categorical
#' @param y.zero Whether the y-axis should include zero.
#' @param y.zero.line.width Width in pixels of zero line;
#' @param y.zero.line.color Color of horizontal zero line as a named
#' color in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param y.data.reversed Logical; whether to reverse y-axis or not
#' @param y.grid.width Width of y-grid lines in pixels; 0 = no line
#' @param y.grid.color Color of y-grid lines as a named color in character
#' format (e.g. "black") or a hex code.
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
#' in character format (e.g. "black") or an a hex code.
#' @param y.tick.font.family Character; y-axis tick label font family
#' @param y.tick.font.size Integer; y-axis tick label font size
#' @param y2.title Character, y-axis title; defaults to chart input values;
#' to turn off set to "FALSE".
#' @param y2.title.font.color y-axis title font color as a named color in
#' character format (e.g. "black") or a hex code.
#' @param y2.title.font.family Character; y-axis title font family
#' @param y2.title.font.size Integer; y-axis title font size
#' @param y2.line.width y-axis line width in pixels (0 = no line).
#' @param y2.line.color y-axis line color as a named color in character format
#' (e.g. "black") or a hex code.
#' @param y2.tick.mark.length Length of tick marks in pixels. Ticks are only shown when \code{y.line.width > 0}.
#' @param y2.bounds.minimum Minimum of range for plotting; For a date axis this should be supplied as a date string.
#'  For a categorical axis, the index of the category (0-based) should be used.
#' @param y2.bounds.maximum Maximum of range for plotting; NULL = no manual range set.
#' @param y2.tick.distance Distance between tick marks. Requires that \code{y.bounds.minimum} and \code{y.bounds.maximum} have been set.
#' @param y2.tick.maxnum Maximum number of ticks shown on the axis.
#'  This setting is ignored if \code{y2.tick.distance} is set or
#'  if the axis is categorical
#' @param y2.zero Whether the y-axis should include zero.
#' @param y2.zero.line.width Width in pixels of zero line;
#' @param y2.zero.line.color Color of horizontal zero line as a named
#' color in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param y2.data.reversed Logical; whether to reverse y-axis or not
#' @param y2.grid.width Width of y-grid lines in pixels; 0 = no line
#' @param y2.grid.color Color of y-grid lines as a named color in character
#' format (e.g. "black") or a hex code.
#' @param y2.tick.show Whether to display the y-axis tick labels
#' @param y2.tick.suffix y-axis tick label suffix
#' @param y2.tick.prefix y-axis tick label prefix
#' @param y2.tick.format A string representing a d3 formatting code.
#' See https://github.com/d3/d3/blob/master/API.md#number-formats-d3-format
#' @param y2.hovertext.format A string representing a d3 formatting code
#' See https://github.com/d3/d3/blob/master/API.md#number-formats-d3-format
#' @param y2.tick.angle y-axis tick label angle in degrees.
#' 90 = vertical; 0 = horizontal
#' @param y2.tick.font.color y-axis tick label font color as a named color
#' in character format (e.g. "black") or an a hex code.
#' @param y2.tick.font.family Character; y-axis tick label font family
#' @param y2.tick.font.size Integer; y-axis tick label font size

#' @param x.title Character, x-axis title; defaults to chart input values;
#' to turn off set to "FALSE".
#' @param x.title.font.color x-axis title font color as a named color in
#' character format (e.g. "black") or an a hex code.
#' @param x.title.font.family Character; x-axis title font family
#' @param x.title.font.size Integer; x-axis title font size
#' @param x.line.width x-axis line in pixels, 0 = no line
#' @param x.line.color x-axis line color as a named color in character format
#' (e.g. "black") or a hex code.
#' @param x.tick.marks Character; whether and where to show tick marks on the
#' x-axis.  Can be "outside", "inside", "none"
#' @param x.tick.mark.length Length of tick marks in pixels.
#' @param x.bounds.minimum Minimum of range for plotting; For a date axis this should be supplied as a date string.
#'  For a categorical axis, the index of the category (0-based) should be used.
#' @param x.bounds.maximum Maximum of range for
#' plotting; NULL = no manual range set.  Must be greater than x.bounds.minimum
#' @param x.tick.distance Tick mark distance in
#' x-axis units between minimum and maximum for plotting; NULL = no manual
#' range set.
#' @param x.tick.maxnum Maximum number of ticks shown on the axis.
#'  It defaults to 11 which gives the same output from plotly as NULL.
#'  This setting is ignored if \code{x.tick.distance} is set or
#'  if the axis is categorical
#' @param x.zero Whether the x-axis should include zero.
#' @param x.zero.line.width Width in pixels of zero line.
#' @param x.zero.line.color Color of horizontal zero (origin) line as a named
#' color in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param x.data.reversed Logical; whether to reverse x-axis or not
#' @param x.grid.width Width of y-grid lines in pixels; 0 = no line
#' @param x.grid.color Color of y-grid lines as a named color in character
#' format (e.g. "black") or a hex code.
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
#' character format (e.g. "black") or an a hex code.
#' @param x.tick.font.family Character; x-axis tick label font family
#' @param x.tick.font.size Integer; x-axis tick label font size
#' @param x.tick.label.wrap Logical; whether to wrap long labels on the x-axis.
#' @param x.tick.label.wrap.nchar Integer; number of characters in each line when \code{label.wrap} is \code{TRUE}.
#' @param hovertext.font.family Font family of hover text.
#' @param hovertext.font.size Font size of hover text.
#' @param marker.border.width Width in pixels of border/line
#' around series bars; 0 is no line
#' @param marker.border.colors Character; a vector containing one or more colors specified as hex codes.
#' @param marker.border.opacity Opacity of border around bars as an alpha value (0 to 1).
#' @param tooltip.show Logical; whether to show a tooltip on hover.
#' @param modebar.show Logical; whether to show the zoom menu buttons or not.
#' @param global.font.family Character; font family for all occurrences of any
#' font attribute for the chart unless specified individually.
#' @param global.font.color Global font color as a named color in character format
#' (e.g. "black") or an a hex code.
#' @param bar.gap Chart proportion between each bar or column if using
#' bar or column charts, or between each cluster of bars or columns.
#' @param data.label.show Logical; whether to show data labels.
#' @param data.label.centered Logical; whether data labels in Stacked Column charts should have the data labels vertically centered.
#' @param data.label.font.family Character; font family for data label.
#' @param data.label.font.size Integer; Font size for data label.px.
#' @param data.label.font.color Font color as a named color
#' in character format (e.g. "black") or an a hex code. This can be a single
#' color, a vector of colors (1 for each series/column), or a comma separated list
#' of colors
#' @param data.label.font.autocolor Logical; Whether font color should be automatically determined
#' (black or white) based on the color of the background column if stacked.
#' @param data.label.format A string representing a d3 formatting code.
#' See https://github.com/mbostock/d3/wiki/Formatting#numbers
#' @param data.label.prefix Character; prefix for data values.
#' @param data.label.suffix Character; suffix for data values.
#' @param data.label.threshold The proportion of the total range below which
#' data labels should not be displayed. Only applicable for pie, bar and column
#' charts.
#' @param x2.data.label.show Logical; whether to show data labels for the secondary axis.
#' @param x2.data.label.show.at.ends Logical; show data labels at the beginning and end of each
#'      line data series. This value will override \code{x2.data.label.show}.
#' @param x2.marker.show.at.ends Logical; show markers at the begining and end of each
#'      data series. The value will override \code{x2.marker.show}.
#' @param x2.data.label.position Character; one of 'top' or 'bottom'.
#' @param x2.data.label.font.family Character; font family for data label for the secondary axis.
#' @param x2.data.label.font.size Integer; Font size for data label.px for the secondary axis.
#' @param x2.data.label.font.color Font color as a named color for the secondary axis
#' in character format (e.g. "black") or an a hex code. This can be a single
#' color, a vector of colors (1 for each series/column), or a comma separated list
#' of colors
#' @param x2.data.label.prefix Character; prefix for data values.
#' @param x2.data.label.suffix Character; suffix for data values.
#' @param x2.data.label.font.autocolor Logical; Whether font color should
#' automatically set to the series color.
#' @param x2.data.label.format A string representing a d3 formatting code.
#' See https://github.com/mbostock/d3/wiki/Formatting#numbers
#' @param x2.data.label.prefix Character; prefix for data values.
#' @param x2.shape Either "linear" for straight lines between data points or "spline" for curved lines.
#' @param x2.smoothing Numeric; smoothing if \code{shape} is "spline".
#' @param x2.line.type Character; one of 'solid', 'dot', 'dashed'.
#' @param x2.line.thickness Thickness, in pixels, of the series line for secondary data.
#' @param x2.opacity Opacity of the series line for secondary data.
#' @param x2.marker.show Logical; whether to show markers at the data points on the lines for the secondary data.
#' @param x2.marker.symbols Character; marker symbols, which are only shown if marker.show = TRUE.
#'     if a vector is passed, then each element will be applied to a data series in the secondary data set.
#' @param x2.marker.colors Character; a vector containing on/mae or more colors specified as hex codes.
#' @param x2.marker.opacity Opacity for markers as an alpha value (0 to 1).
#' @param x2.marker.size Size in pixels of marker
#' @param x2.marker.border.width Width in pixels of border/line around markers; 0 is no line
#' @param x2.marker.border.colors Character; a vector containing one or more colors specified as hex codes.
#' @param x2.marker.border.opacity Opacity of border/line around
#' markers as an alpha value (0 to 1).
#' @importFrom grDevices rgb
#' @importFrom flipChartBasics ChartColors
#' @importFrom flipTables AsTidyTabularData
#' @importFrom plotly plot_ly config toRGB add_trace add_text layout hide_colorbar
#' @importFrom stats loess loess.control lm predict
#' @examples
#' z <- structure(c(1L, 2L, 3L, 4L, 5L, 2L, 3L, 4L, 5L, 6L),  .Dim = c(5L, 2L),
#'       .Dimnames = list(c("T", "U", "V", "W", "X"), c("A", "B")))
#' Column(z, type="Stacked Column")
#' @export
Column <- function(x,
                    x2 = NULL,
                    colors = ChartColors(max(1, NCOL(x), na.rm = TRUE)),
                    opacity = NULL,
                    type = "Column",
                    annotation.list = NULL,
                    overlay.annotation.list = NULL,
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
                    background.fill.color = "transparent",
                    background.fill.opacity = 1,
                    charting.area.fill.color = background.fill.color,
                    charting.area.fill.opacity = 0,
                    legend.show = NA,
                    legend.orientation = 'Vertical',
                    legend.wrap = TRUE,
                    legend.wrap.nchar = 30,
                    legend.position.x = NULL,
                    legend.position.y = NULL,
                    legend.fill.color = background.fill.color,
                    legend.fill.opacity = 0,
                    legend.border.color = rgb(44, 44, 44, maxColorValue = 255),
                    legend.border.line.width = 0,
                    legend.font.color = global.font.color,
                    legend.font.family = global.font.family,
                    legend.font.size = 10,
                    legend.ascending = NA,
                    hovertext.font.family = global.font.family,
                    hovertext.font.size = 11,
                    margin.top = NULL,
                    margin.bottom = NULL,
                    margin.left = NULL,
                    margin.right = NULL,
                    margin.inner.pad = NULL,
                    margin.autoexpand = TRUE,
                    grid.show = TRUE,
                    x2.colors = ChartColors(max(1, NCOL(x2), na.rm = TRUE)),
                    x2.data.label.show = FALSE,
                    x2.data.label.show.at.ends = FALSE,
                    x2.line.type = "Solid",
                    x2.line.thickness = 2,
                    x2.shape = c("linear", "spline")[1],
                    x2.smoothing = 1,
                    x2.opacity = 1,
                    x2.marker.show = FALSE,
                    x2.marker.show.at.ends = FALSE,
                    x2.marker.size = 6,
                    x2.marker.symbols = "circle",
                    x2.marker.colors = x2.colors,
                    x2.marker.border.colors = x2.colors,
                    x2.marker.opacity = x2.opacity,
                    x2.marker.border.opacity = x2.opacity,
                    x2.marker.border.width = 1,
                    x2.data.label.position = "Top",
                    x2.data.label.font.autocolor = FALSE,
                    x2.data.label.font.family = global.font.family,
                    x2.data.label.font.size = 10,
                    x2.data.label.font.color = global.font.color,
                    x2.data.label.format = "",
                    x2.data.label.prefix = "",
                    x2.data.label.suffix = "",
                    y.title = "",
                    y.title.font.color = global.font.color,
                    y.title.font.family = global.font.family,
                    y.title.font.size = 12,
                    y.line.width = 0,
                    y.line.color = rgb(0, 0, 0, maxColorValue = 255),
                    y.tick.mark.length = 0,
                    y.bounds.minimum = NULL,
                    y.bounds.maximum = NULL,
                    y.tick.distance = NULL,
                    y.tick.maxnum = NULL,
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
                    y2.title = "",
                    y2.title.font.color = global.font.color,
                    y2.title.font.family = global.font.family,
                    y2.title.font.size = 12,
                    y2.line.width = 0,
                    y2.line.color = rgb(0, 0, 0, maxColorValue = 255),
                    y2.tick.mark.length = 0,
                    y2.bounds.minimum = NULL,
                    y2.bounds.maximum = NULL,
                    y2.tick.distance = NULL,
                    y2.tick.maxnum = NULL,
                    y2.zero = TRUE,
                    y2.zero.line.width = 0,
                    y2.zero.line.color = rgb(225, 225, 225, maxColorValue = 255),
                    y2.data.reversed = FALSE,
                    y2.grid.width = 0 * grid.show,
                    y2.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                    y2.tick.show = TRUE,
                    y2.tick.suffix = "",
                    y2.tick.prefix = "",
                    y2.tick.format = "",
                    y2.hovertext.format = y.tick.format,
                    y2.tick.angle = NULL,
                    y2.tick.font.color = global.font.color,
                    y2.tick.font.family = global.font.family,
                    y2.tick.font.size = 10,
                    x.title = "",
                    x.title.font.color = global.font.color,
                    x.title.font.family = global.font.family,
                    x.title.font.size = 12,
                    x.line.width = 0,
                    x.line.color = rgb(0, 0, 0, maxColorValue = 255),
                    x.tick.marks = "",
                    x.tick.mark.length = 3,
                    x.bounds.minimum = NULL,
                    x.bounds.maximum = NULL,
                    x.tick.distance = NULL,
                    x.tick.maxnum = 11,
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
                    data.label.centered = FALSE,
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
    if (bar.gap < 0.0 || bar.gap >= 1.0)
    {
        warning("Parameter 'bar gap' must be between 0 and 1. ",
                "Invalid 'bar gap' set to default value of 0.15.")
        bar.gap <- 0.15
    }

    # Store data for chart annotations
    annot.data <- x
    chart.matrix <- checkMatrixNames(x)
    if (!is.numeric(chart.matrix))
        stop("Input data should be numeric.")
    x.labels.full <- rownames(chart.matrix)

    is.stacked <- grepl("Stacked", type, fixed = TRUE)
    if (is.stacked && ncol(chart.matrix) < 2)
    {
        warning("No stacking performed for only one series.")
        is.stacked <- FALSE
    }
    is.hundred.percent.stacked <- grepl("100% Stacked", type, fixed = TRUE)
    if (any(!is.finite(as.matrix(chart.matrix))))
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
    barmode <- if (is.stacked) "relative" else "group"
    if (is.null(opacity))
        opacity <- if (fit.type == "None") 1 else 0.6
    if (is.null(marker.border.opacity))
        marker.border.opacity <- opacity
    n <- ncol(chart.matrix)
    colors <- vectorize(colors, n)
    if (fit.type != "None" && is.null(fit.line.colors))
        fit.line.colors <- colors
    if (fit.CI.show && is.null(fit.CI.colors))
        fit.CI.colors <- fit.line.colors
    if (is.null(marker.border.colors))
        marker.border.colors <- colors
    marker.border.colors <- vectorize(marker.border.colors, n)


    if (is.stacked && data.label.font.autocolor)
        dlab.color <- autoFontColor(colors)
    else
        dlab.color <- vectorize(data.label.font.color, n)
    data.label.show <- vectorize(data.label.show, n, NROW(chart.matrix))

    data.label.font = lapply(dlab.color,
        function(cc) list(family = data.label.font.family, size = data.label.font.size, color = cc))
    title.font = list(family = title.font.family, size = title.font.size, color = title.font.color)
    subtitle.font = list(family = subtitle.font.family, size = subtitle.font.size, color = subtitle.font.color)
    x.title.font = list(family = x.title.font.family, size = x.title.font.size, color = x.title.font.color)
    y.title.font = list(family = y.title.font.family, size = y.title.font.size, color = y.title.font.color)
    y2.title.font = list(family = y2.title.font.family, size = y2.title.font.size, color = y2.title.font.color)
    ytick.font = list(family = y.tick.font.family, size = y.tick.font.size, color = y.tick.font.color)
    y2.tick.font = list(family = y2.tick.font.family, size = y2.tick.font.size, color = y2.tick.font.color)
    xtick.font = list(family = x.tick.font.family, size = x.tick.font.size, color = x.tick.font.color)
    footer.font = list(family = footer.font.family, size = footer.font.size, color = footer.font.color)
    legend.font = list(family = legend.font.family, size = legend.font.size, color = legend.font.color)

    legend.show <- setShowLegend(legend.show, NCOL(chart.matrix))
    legend <- setLegend(type, legend.font, legend.ascending, legend.fill.color, legend.fill.opacity,
                        legend.border.color, legend.border.line.width,
                        legend.position.x, legend.position.y, y.data.reversed,
                        legend.orientation, y2.show = !is.null(x2))
    footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate = FALSE)

    # Format axis labels
    axisFormat <- formatLabels(chart.matrix, type, x.tick.label.wrap, x.tick.label.wrap.nchar,
                               x.tick.format, y.tick.format)

    # In most cases, if the user does not specify a range we just let plotly determine the defaults
    # But in some cases adding data labels to column charts will case the default range to expand
    use.default.range <- TRUE
    if (!is.null(x.tick.distance) || (!x.zero && axisFormat$x.axis.type != "date" && any(data.label.show)))
        use.default.range <- FALSE
    x.range <- setValRange(x.bounds.minimum, x.bounds.maximum, axisFormat, x.zero, use.default.range, is.bar = TRUE)
    y.range <- setValRange(y.bounds.minimum, y.bounds.maximum, chart.matrix, y.zero, is.null(y.tick.distance))
    xtick <- setTicks(x.range$min, x.range$max, x.tick.distance, x.data.reversed, is.bar = TRUE)
    ytick <- setTicks(y.range$min, y.range$max, y.tick.distance, y.data.reversed)

    yaxis <- setAxis(y.title, "left", axisFormat, y.title.font,
                  y.line.color, y.line.width, y.grid.width * grid.show, y.grid.color,
                  ytick, ytick.font, y.tick.angle, y.tick.mark.length, y.tick.distance, y.tick.format,
                  y.tick.prefix, y.tick.suffix,
                  y.tick.show, y.zero, y.zero.line.width, y.zero.line.color,
                  y.hovertext.format, num.maxticks = y.tick.maxnum)
    xaxis <- setAxis(x.title, "bottom", axisFormat, x.title.font,
                  x.line.color, x.line.width, x.grid.width * grid.show, x.grid.color,
                  xtick, xtick.font, x.tick.angle, x.tick.mark.length, x.tick.distance, x.tick.format,
                  x.tick.prefix, x.tick.suffix, x.tick.show, x.zero, x.zero.line.width, x.zero.line.color,
                  x.hovertext.format, axisFormat$labels, num.series = NCOL(chart.matrix), with.bars = TRUE, num.maxticks = x.tick.maxnum)

    yaxis2 <- NULL

    # Work out margin spacing
    margins <- list(t = 20, b = 20, r = if (!legend.show && !is.null(x2)) 80 else 60, l = 80, pad = 0)
    margins <- setMarginsForAxis(margins, axisFormat, xaxis)
    margins <- setMarginsForText(margins, title, subtitle, footer, title.font.size,
                                 subtitle.font.size, footer.font.size)

    legend.text <- autoFormatLongLabels(colnames(chart.matrix), legend.wrap, legend.wrap.nchar)
    margins <- setMarginsForLegend(margins, legend.show, legend, legend.text, right.axis = !is.null(x2))
    margins <- setCustomMargins(margins, margin.top, margin.bottom, margin.left,
                    margin.right, margin.inner.pad)
    margins$autoexpand <- margin.autoexpand

    ## Initiate plotly object
    p <- plot_ly(as.data.frame(chart.matrix))
    x.labels <- axisFormat$labels
    y.labels <- colnames(chart.matrix)

    # Set up numeric x-axis - this is used for data labels and hovertext
    x.all.labels <- x.labels
    if (!is.null(x2))
    {
        # Set up x-axis values for x2
        x2 <- checkMatrixNames(x2)
        x2.axis.type <- getAxisType(rownames(x2), format = x.tick.format)
        if (x2.axis.type != xaxis$type)
        {
            if (x2.axis.type == "numeric" && NROW(x2) == NROW(chart.matrix))
            {
                rownames(x2) <- rownames(chart.matrix)
                x2.axis.type <- xaxis$type
            }
            else
                stop("Rownames in data for second axis (", x2.axis.type,
                     ") do not have the same type as the input data (",
                     xaxis$type, ").")
        }
        x2.labels <- formatLabels(x2, "Column", x.tick.label.wrap, x.tick.label.wrap.nchar,
            x.tick.format, y2.tick.format)$labels
        x.all.labels <- unique(c(x.all.labels, x2.labels))

        # Force chart to used combined dataset to set x-axis range
        # But we don't touch the date axis
        if (xaxis$type != "date")
        {
            old.range.reversed <- isReversed(xaxis) && xaxis$autorange != "reversed"
            xaxis$range <- c(NA, NA)
            old.range <- x.range
            x.range <- getRange(x.all.labels, xaxis, NULL)
            xaxis$autorange <- FALSE
            xaxis$range <- x.range
            if (old.range.reversed)
                xaxis$range <- x.range <- rev(x.range)
        } else
            x.range <- getRange(x.labels, xaxis, axisFormat)
    }
    else
        x.range <- getRange(x.labels, xaxis, axisFormat)

    # Set up second x-axis for data labels
    xaxis2 <- list(overlaying = "x", fixedrange = TRUE, range = x.range,
        visible = FALSE)
    data.annotations <- dataLabelPositions(chart.matrix = chart.matrix,
                        axis.type = xaxis$type,
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
                        reversed = isReversed(yaxis),
                        font = data.label.font,
                        center.data.labels = data.label.centered)

    if (!is.null(overlay.annotation.list))
        data.overlay.annot <- dataLabelPositions(chart.matrix = chart.matrix,
                        axis.type = xaxis$type,
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
                        reversed = isReversed(yaxis),
                        font = data.label.font,
                        center.data.labels = FALSE)


    # Set up second y-axis (for secondary data)
    if (!is.null(x2))
    {
        y2.range <- setValRange(y2.bounds.minimum, y2.bounds.maximum, x2, y2.zero, is.null(y2.tick.distance))
        y2.tick  <- setTicks(y2.range$min, y2.range$max, y2.tick.distance, y2.data.reversed)
        yaxis2   <- setAxis(y2.title, "right", axisFormat, y2.title.font,
                          y2.line.color, y2.line.width, y2.grid.width * grid.show, y2.grid.color,
                          y2.tick, y2.tick.font, y2.tick.angle, y2.tick.mark.length, y2.tick.distance,
                          y2.tick.format, y2.tick.prefix, y2.tick.suffix,
                          y2.tick.show, y2.zero, y2.zero.line.width, y2.zero.line.color,
                          y2.hovertext.format, num.maxticks = y2.tick.maxnum)
        yaxis2$overlaying <- "y"

        n2 <- ncol(x2)
        m2 <- nrow(x2)
        if (x2.data.label.show.at.ends || x2.marker.show.at.ends)
        {
            ends.show <- matrix(FALSE, m2, n2)
            for (i in 1:n2)
            {
                ind <- which(is.finite(x2[,i])) # ignore NAs
                ends.show[min(ind),i] <- TRUE
                ends.show[max(ind),i] <- TRUE
            }
        }
        x2.colors <- vectorize(x2.colors, n2)
        if (is.null(x2.marker.colors))
            x2.marker.colors <- x2.colors
        x2.marker.colors <- vectorize(x2.marker.colors, n2)
        if (is.null(x2.marker.border.colors))
            x2.marker.border.colors <- x2.marker.colors
        x2.marker.border.colors <- vectorize(x2.marker.border.colors, n2)
        x2.data.label.show <- if (x2.data.label.show.at.ends) ends.show
                           else vectorize(x2.data.label.show, n2, m2)
        x2.marker.show <- if (x2.marker.show.at.ends) ends.show
                       else  vectorize(x2.marker.show, n2, m2)

        x2.line.type <- vectorize(tolower(x2.line.type), n2)
        x2.line.thickness <- readLineThickness(x2.line.thickness, n2)
        x2.opacity <- x2.opacity * rep(1, n2)
        x2.marker.symbols <- vectorize(x2.marker.symbols, n2, m2)
        x2.marker.size <- vectorize(x2.marker.size, n2, m2)
        x2.dlab.color <- if (x2.data.label.font.autocolor) x2.colors
                         else vectorize(x2.data.label.font.color, n2)
        x2.dlab.pos <- vectorize(tolower(x2.data.label.position), n2)
        x2.dlab.prefix <- vectorize(x2.data.label.prefix, n2, m2, split = NULL)
        x2.dlab.suffix <- vectorize(x2.data.label.suffix, n2, m2, split = NULL)
        x2.data.label.font = lapply(x2.dlab.color,
        function(cc) list(family = x2.data.label.font.family, size = x2.data.label.font.size, color = cc))

        if (grepl("^curved", tolower(x2.shape)))
            x2.shape <- "spline"
        if (grepl("^straight", tolower(x2.shape)))
            x2.shape <- "linear"
        x2.series.mode <- ifelse (apply(x2.marker.show, 2, any), "lines+markers", "lines")
        x2.lines <- list()
        x2.markers <- list()

        for (i in 1:n2)
        {
            x2.lines[[i]] <- list(width = x2.line.thickness[i], dash = x2.line.type[i],
                  shape = x2.shape, smoothing = x2.smoothing,
                  color = toRGB(x2.colors[i], alpha = x2.opacity[i]))

            x2.markers[[i]] <- list(NULL)
            if (any(x2.marker.show[,i]) && any(is.finite(x2[,i])))
            {
                sz.ind0 <- which(is.finite(x2[,i]))
                sz.ind <- min(sz.ind0):max(sz.ind0) # plotly ignores NAs at ends but not in the middle
                size.i <- rep(0, length(sz.ind))
                size.i[which(x2.marker.show[sz.ind,i])] <-
                    x2.marker.size[intersect(which(x2.marker.show[,i]), sz.ind),i]

                x2.markers[[i]] <- list(size = size.i,
                           color = toRGB(x2.marker.colors[i], alpha = x2.marker.opacity),
                           symbol = x2.marker.symbols[i], opacity = 1.0,
                           line = list(
                           color = toRGB(x2.marker.border.colors[i], alpha = x2.marker.border.opacity),
                           width = x2.marker.border.width))
            }
        }
    }
    # Add invisible line to force all categorical labels to be shown
    # Type "scatter" ensures y-axis tick bounds are treated properly
    # but it also adds extra space next to the y-axis
    tmp.min <- if (any(is.finite(chart.matrix))) min(chart.matrix[is.finite(chart.matrix)])
               else y.bounds.minimum
    p <- add_trace(p, x = x.all.labels,
                   y = rep(tmp.min, length(x.all.labels)),
                   mode = if (notAutoRange(yaxis)) "markers" else "lines",
                   type = "scatter", cliponaxis = TRUE,
                   hoverinfo = "skip", showlegend = FALSE, opacity = 0)


    # Plot trace for second y-axis first so that they are shown last in legend
    if (!is.null(x2) && is.stacked)
    {
        for (i in 1:ncol(x2))
            p <- add_trace(p, x = x2.labels, y = x2[,i], name = colnames(x2)[i],
                    type = "scatter", mode = x2.series.mode[i], yaxis = "y2", xaxis = "x",
                    line = x2.lines[[i]], marker = x2.markers[[i]], connectgaps = FALSE,
                    hoverlabel = list(font = list(color = autoFontColor(x2.colors[i]),
                    size = hovertext.font.size, family = hovertext.font.family)),
                    hovertemplate = setHoverTemplate(i, xaxis, x2), cliponaxis = TRUE,
                    legendgroup = NCOL(chart.matrix) + i)
    }

    ## Add a trace for each col of data in the matrix
    for (i in 1:ncol(chart.matrix))
    {
        y <- as.numeric(chart.matrix[, i])
        y.filled <- ifelse(is.finite(y), y, 0)
        x <- x.labels
        marker <- list(color = toRGB(colors[i], alpha = opacity),
                      line = list(color = toRGB(marker.border.colors[i],
                      alpha = marker.border.opacity),
                      width = marker.border.width))

        # This is the main trace for each data series
        p <- add_trace(p, x = x, y = y.filled, type = "bar",
                       orientation = "v", marker = marker, name = legend.text[i],
                       hoverlabel = list(font = list(color = autoFontColor(colors[i]),
                       size = hovertext.font.size, family = hovertext.font.family)),
                       hovertemplate = setHoverTemplate(i, xaxis, chart.matrix),
                       legendgroup = if (is.stacked && any(data.label.show)) "all" else i)


        if (fit.type != "None" && is.stacked && i == 1)
            warning("Line of best fit not shown for stacked charts.")
        if (fit.type != "None" && !is.stacked)
        {
            tmp.fit <- fitSeries(x, y, fit.type, fit.ignore.last, xaxis$type,
                fit.CI.show, fit.window.size)
            tmp.fname <- if (ncol(chart.matrix) == 1)  fit.line.name
                         else sprintf("%s: %s", fit.line.name, y.labels[i])
            p <- add_trace(p, x = tmp.fit$x, y = tmp.fit$y, type = 'scatter', mode = "lines",
                      name = tmp.fname, legendgroup = i, showlegend = FALSE, opacity = fit.line.opacity,
                      hoverlabel = list(font = list(color = autoFontColor(fit.line.colors[i]),
                      size = hovertext.font.size, family = hovertext.font.family)),
                      line = list(dash = fit.line.type, width = fit.line.width,
                      color = fit.line.colors[i], shape = 'spline'), opacity = fit.line.opacity)
            if (fit.CI.show && !is.null(tmp.fit$lb))
            {
                p <- add_trace(p, x = tmp.fit$x, y = tmp.fit$lb, type = 'scatter',
                        mode = 'lines', name = "Lower bound of 95%CI",
                        hoverlabel = list(font = list(color = autoFontColor(fit.CI.colors[i]),
                        size = hovertext.font.size, family = hovertext.font.family)),
                        showlegend = FALSE, legendgroup = i,
                        line=list(color=fit.CI.colors[i], width=0, shape='spline'))
                p <- add_trace(p, x = tmp.fit$x, y = tmp.fit$ub, type = 'scatter',
                        mode = 'lines', name = "Upper bound of 95% CI",
                        hoverlabel = list(font = list(color = autoFontColor(fit.CI.colors[i]),
                        size = hovertext.font.size, family = hovertext.font.family)),
                        fill = "tonexty", fillcolor = toRGB(fit.CI.colors[i], alpha = fit.CI.opacity),
                        showlegend = FALSE, legendgroup = i,
                        line = list(color=fit.CI.colors[i], width=0, shape='spline'))
            }
        }

        # Only used for small multiples
        if (!is.null(average.series))
            p <- add_trace(p, x = x, y = average.series, name = "Average",
                    type = "scatter", mode = "lines", showlegend = FALSE,
                    hoverlabel = list(font = list(color = autoFontColor(average.color),
                    size = hovertext.font.size, family = hovertext.font.family)),
                    line = list(color = average.color))


        # Plotly text marker positions are not spaced properly when placed to
        # the below the bar (i.e. negative values or reversed axis).
        # Adjusted by controlling the size of the marker
        # Hover must be included because this trace hides existing hover items
        if (any(data.label.show))
            p <- addDataLabelAnnotations(p, type = "Column", legend.text[i],
                    data.label.xpos = if (NCOL(chart.matrix) > 1) data.annotations$x[,i] else x,
                    data.label.ypos = data.annotations$y[,i],
                    data.label.show = data.label.show[,i],
                    data.label.text = data.annotations$text[,i],
                    data.label.sign = getSign(data.annotations$y[,i], yaxis),
                    annotation.list, annot.data, i,
                    xaxis = if (NCOL(chart.matrix) > 1) "x2" else "x", yaxis = "y",
                    data.label.font[[i]], is.stacked, data.label.centered)

        # Create annotations separately for each series
        # so they can be toggled using the legend
        for (curr.annot.ind in seq_along(overlay.annotation.list))
        {
            curr.annot <- overlay.annotation.list[[curr.annot.ind]]
            curr.annot$threshold <- parseThreshold(curr.annot$threshold)
            curr.dat <- getAnnotData(annot.data, curr.annot$data, i,
                as.numeric = !grepl("Text", curr.annot$type) && curr.annot$data != "Column Comparisons")
            ind.sel <- extractSelectedAnnot(curr.dat, curr.annot$threshold, curr.annot$threstype)
            if (length(ind.sel) == 0)
                next

            curr.annot.ypos <- if (is.stacked) data.overlay.annot$y[ind.sel,i] -
                            (chart.matrix[ind.sel,i] * (1 - curr.annot$relative.pos))
                        else data.overlay.annot$y[ind.sel,i] * curr.annot$relative.pos
            curr.annot.align <- paste(if (is.null(curr.annot$valign)) "middle" else tolower(curr.annot$valign),
                            if (is.null(curr.annot$halign)) "center" else tolower(curr.annot$halign))

            if (curr.annot$data == "Column Comparisons" && grepl("Arrow", curr.annot$type))
                curr.annot.text <- getColCmpArrowHtml(curr.dat[ind.sel], curr.annot$size)
            else if (curr.annot$type == "Text")
                curr.annot.text <- formatByD3(curr.dat[ind.sel], curr.annot$format, curr.annot$prefix, curr.annot$suffix)
            else if (curr.annot$type == "Arrow - up")
                curr.annot.text <- "&#129049;"
            else if (curr.annot$type == "Arrow - down")
                curr.annot.text <- "&#129051;"
            else
                curr.annot.text <- curr.annot$custom.symbol
            curr.annot.text <- rep(curr.annot.text, length = length(ind.sel))

            p <- add_trace(p, y = curr.annot.ypos,
                x = data.overlay.annot$x[ind.sel,i],
                type = "scatter", mode = "markers+text", hoverinfo = "skip",
                xaxis = "x2", yaxis = "y", showlegend = FALSE,
                marker = list(opacity = 0.0, size = sum(curr.annot$offset)),
                text = curr.annot.text, textposition = curr.annot.align,
                textfont = list(family = curr.annot$font.family, size = curr.annot$size,
                    color = curr.annot$color),
                legendgroup = if (is.stacked) "all" else i,
                cliponaxis = FALSE)
        }
    }

    if (!any(data.label.show) && length(annotation.list) > 0)
        warning("Annotations are ignored when data labels are not shown. ",
            "Try using 'Annotation Overlay' instead.")



    # Plot trace for second y-axis last so that they are shown last in legend
    if (!is.null(x2) && !is.stacked)
    {
        for (i in 1:ncol(x2))
        {
            p <- add_trace(p, x = x2.labels, y = x2[,i], name = colnames(x2)[i],
                    type = "scatter", mode = x2.series.mode[i], yaxis = "y2",
                    line = x2.lines[[i]], marker = x2.markers[[i]], connectgaps = FALSE,
                    hoverlabel = list(font = list(color = autoFontColor(x2.colors[i]),
                    size = hovertext.font.size, family = hovertext.font.family)),
                    hovertemplate = setHoverTemplate(i, xaxis, x2), cliponaxis = FALSE,
                    legendgroup = NCOL(chart.matrix) + i)
        }
    }

    # Add data labels for x2 as a trace
    if (!is.null(x2) && any(x2.data.label.show))
    {
        for (i in 1:ncol(x2))
        {
            if (any(x2.data.label.show[,i]))
            {
                ind.show <- which(x2.data.label.show[,i] & is.finite(x2[,i]))
                tmp.y <- as.numeric(x2[ind.show, i])
                tmp.x <- x2.labels[ind.show]
                tmp.text <- formatByD3(x2[ind.show,i], x2.data.label.format,
                    x2.dlab.prefix[ind.show,i], x2.dlab.suffix[ind.show,i])
                tmp.offset <- rep(x2.line.thickness[i]/2, length(ind.show))
                if (any(x2.marker.show[,i]))
                    tmp.offset[which(x2.marker.show[ind.show,i])] <- pmax(x2.marker.size[ind.show,i], tmp.offset)

                p <- add_trace(p, x = tmp.x, y = tmp.y, yaxis = "y2", xaxis = "x",
                       type = "scatter", cliponaxis = FALSE,
                       text = tmp.text, mode = "markers+text",
                       marker = list(size = tmp.offset, color = x2.colors[i], opacity = 0),
                       textfont = x2.data.label.font[[i]], textposition = x2.dlab.pos[i],
                       showlegend = FALSE, legendgroup = ncol(chart.matrix) + i,
                       hoverinfo = "skip")
            }
        }
    }

    # Add text elements surrounding chart
    annotations <- NULL
    n <- length(annotations)
    annotations[[n+1]] <- setTitle(title, title.font, margins)
    annotations[[n+2]] <- setFooter(footer, footer.font, margins)
    annotations[[n+3]] <- setSubtitle(subtitle, subtitle.font, margins)
    annotations <- Filter(Negate(is.null), annotations)

    p <- config(p, displayModeBar = modebar.show)
    p$sizingPolicy$browser$padding <- 0
    p <- layout(p,
        showlegend = legend.show,
        legend = legend,
        yaxis2 = yaxis2,
        yaxis = yaxis,
        xaxis2 = xaxis2,
        xaxis = xaxis,
        margin = margins,
        annotations =  annotations,
        plot_bgcolor = toRGB(charting.area.fill.color, alpha = charting.area.fill.opacity),
        paper_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity),
        hoverlabel = list(namelength = -1, bordercolor = "transparent",
            font = list(size = hovertext.font.size, family = hovertext.font.family)),
        hovermode = if (tooltip.show) "x" else FALSE,
        bargap = bar.gap,
        barmode = barmode
    )
    #attr(p, "can-run-in-root-dom") <- TRUE
    result <- list(htmlwidget = p)
    class(result) <- "StandardChart"
    attr(result, "ChartType") <- if (is.stacked) "Column Stacked" else "Column Clustered"
    result
}

