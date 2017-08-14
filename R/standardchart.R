#' Generates an interactive chart based on the plotly library.
#'
#' \code{Chart} generates standard charts from plotly library.
#'
#' @param y A table, matrix, vector or data frame.
#' @param type Character; type of chart. Can be "Area", "Stacked Area",
#'  "100\% Stacked Area", "Bar", "Stacked Bar", "100% Stacked Bar",
#'  "Column", "Stacked Column", "100% Stacked Column", "Line", "Donut",
#'  "Pie", "Labeled Scatterplot", "Labeled Bubbleplot", "Radar".
#' @param fit.type Character; type of line of best fit. Can be one of "None", "Linear" or "Smooth" (loess local polynomial fitting).
#' @param fit.ignore.last Boolean; whether to ignore the last data point in the fit.
#' @param fit.line.type Character; One of "solid", "dot", "dash, "dotdash", or length of dash "2px", "5px".
#' @param fit.line.width Numeric; Line width of line of best fit.
#' @param fit.line.name Character; Name of the line of best fit, which will appear in the hovertext.
#' @param transpose Logical; should the final output be transposed?
#' @param subset An optional vector specifying a subset of observations to be plotted. Only used when \code{scatter.var.from.matrix} is true and \code{type} is one of \code{Scatterplot, Labeled Scatterplot} or \code{Labeled Bubbleplot}.
#' @param weights Not implemented. An optional vector of sampling weights. Only used when \code{scatter.var.from.matrix} is true and \code{type} is one of \code{Scatterplot, Labeled Scatterplot} or \code{Labeled Bubbleplot}.
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
#' @param footer.wordwrap Logical; whether the footer text should be wrapped.
#' @param footer.wordwrap.nchar Number of characters (approximately) in each line of the footer when \code{footer.wordwrap} i \code{TRUE}.
#' @param colors Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' @param colors.reverse Logical; if the order of the colors should be reversed.
#' @param colors.custom.color Character; a single color which is used if \code{colors} is set to \code{"Custom color"}.
#' @param colors.custom.gradient.start Character; starting color of gradient if \code{colors} is set to \code{"Custom gradient"}.
#' @param colors.custom.gradient.end Character; last color of gradient if \code{colors} is set to \code{"Custom gradient"}.
#' @param colors.custom.palette Character; comma separated list of colors to be used if \code{colors} is set to \code{"Custom palette"}.
#' @param fit.line.colors Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' @param fit.line.colors.reverse Logical; if the order of the colors should be reversed.
#' @param fit.line.colors.custom.color Character; a single color which is used if \code{fit.line.colors} is set to \code{"Custom color"}.
#' @param fit.line.colors.custom.gradient.start Character; starting color of gradient if \code{fit.line.colors} is set to \code{"Custom gradient"}.
#' @param fit.line.colors.custom.gradient.end Character; last color of gradient if \code{fit.line.colors} is set to \code{"Custom gradient"}.
#' @param fit.line.colors.custom.palette Character; comma separated list of colors to be used if \code{fit.line.colors} is set to \code{"Custom palette"}.
#' @param opacity Opacity of area fill colors as an alpha value
#' (0 to 1).
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
#' @param y.position Character; set y-axis position; can be "left" or "right"
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
#' @param x.position Character; set x-axis position; can be "top" or "bottom"
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
#' @param x.tick.label.autoformat Logical; whether to rotate and wrap long x-axis labels.
#' @param x.tick.label.wordwrap Logical; whether to wrap long x-axis labels.
#' @param series.marker.show Can be "none", "automatic" or a vector referencing
#' the plotly symbol dictionary using either numerics or strings.
#' @param wordwrap.nchar Integer; number of characters in each line when \code{x.tick.label.wordwrap} is \code{TRUE}.
#' @param series.marker.colors Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' @param series.marker.colors.reverse Logical; if the order of the colors should
#' be reversed.
#' @param series.marker.opacity Opacity for series markers as an alpha value (0 to 1).
#' @param series.marker.colors.custom.color Character; a single color which is used if \code{series.marker.colors} is set to \code{"Custom color"}.
#' @param series.marker.colors.custom.gradient.start Character; starting color of gradient if \code{series.marker.colors} is set to \code{"Custom gradient"}.
#' @param series.marker.colors.custom.gradient.end Character; last color of gradient if \code{series.marker.colors} is set to \code{"Custom gradient"}.
#' @param series.marker.colors.custom.palette Character; comma separated list of series.marker.colors to be used if \code{series.marker.colors} is set to \code{"Custom palette"}.
#' @param series.marker.size Size in pixels of marker
#' @param series.marker.border.width Width in pixels of border/line
#' around series markers; 0 is no line
#' @param series.marker.border.colors Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' @param series.marker.border.colors.reverse Logical; if the order of the colors
#' should be reversed.
#' @param series.marker.border.colors.custom.color Character; a single color which is used if \code{series.marker.border.colors} is set to \code{"Custom color"}.
#' @param series.marker.border.colors.custom.gradient.start Character; starting color of gradient if \code{series.marker.border.colors} is set to \code{"Custom gradient"}.
#' @param series.marker.border.colors.custom.gradient.end Character; last color of gradient if \code{series.marker.border.colors} is set to \code{"Custom gradient"}.
#' @param series.marker.border.colors.custom.palette Character; comma separated list of series.marker.border.colors to be used if \code{series.marker.border.colors} is set to \code{"Custom palette"}.
#' @param series.marker.border.opacity Opacity of border/line around
#' series markers as an alpha value (0 to 1).
#' @param series.line.width Thickness, in pixels, of the series line
#' @param series.line.colors  Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' @param series.line.colors.reverse Logical; if the order of the colors
#' should be reversed.
#' @param series.line.colors.custom.color Character; a single color which is used if \code{series.line.colors} is set to \code{"Custom color"}.
#' @param series.line.colors.custom.gradient.start Character; starting color of gradient if \code{series.line.colors} is set to \code{"Custom gradient"}.
#' @param series.line.colors.custom.gradient.end Character; last color of gradient if \code{series.line.colors} is set to \code{"Custom gradient"}.
#' @param series.line.colors.custom.palette Character; comma separated list of series.line.colors to be used if \code{series.line.colors} is set to \code{"Custom palette"}.
#' @param series.line.opacity Opacity for series lines as an
#' alpha value (0 to 1)
#' @param tooltip.show Logical; whether to show a tooltip on hover.
#' @param modebar.show Logical; whether to show the zoom menu buttons or not.
#' @param global.font.family Character; font family for all occurrences of any
#' font attribute for the chart unless specified individually.
#' @param global.font.color Global font color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. #' rgb(0, 0, 0, maxColorValue = 255)).
#' @param rows.to.ignore Character; comma separated string of row headings to
#' exclude from the charting.
#' @param cols.to.ignore Character; comma separated string of column headings to
#' exclude from the charting.  Does not apply to Labeled Scatterplot or Labeled
#' Bubbleplot, which both need to have the correct columns prior to charting.
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
#' @param pie.order Character; "descending", "initial", or
#' "alphabetical".
#' @param pie.groups.order Character; "descending", "initial", or
#' "alphabetical".
#' @param pie.subslice.colors Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' @param pie.subslice.colors.reverse Logical; if the order of
#' pie.subslice.colors should be reversed.
#' @param pie.subslice.colors.custom.color Character; a single color which is used if \code{pie.subslice.colors} is set to \code{"Custom color"}.
#' @param pie.subslice.colors.custom.gradient.start Character; starting color of gradient if \code{pie.subslice.colors} is set to \code{"Custom gradient"}.
#' @param pie.subslice.colors.custom.gradient.end Character; last color of gradient if \code{pie.subslice.colors} is set to \code{"Custom gradient"}.
#' @param pie.subslice.colors.custom.palette Character; comma separated list of pie.subslice.colors to be used if \code{pie.subslice.colors} is set to \code{"Custom palette"}.
#' @param pie.subslice.colors.repeat Logical; if, when a grouped
#' pie chart is displayed, the colors of the subslices should repeat
#' by group, or be different throughout; defaults to TRUE.
#' @param pie.border.color A single color for space around pie and between
#' segments.
#' @param pie.inner.radius The size of the inner radius of pie and
#' donut charts as a proportion out of 100. defaults to 70.
#' @param pie.show.percentages Whether to show percentages in pie and donut
#' charts instead of original values.
#' @param z.title Character; title of the bubble-size legend in labeled
#' bubbleplots.
#' @param trend.lines Boolean indicating whether to plot trend lines for multiple tables.
#' @param scatter.group.indices Vector or text of comma-separated group indices
#' corresponding to each row.
#' @param scatter.group.labels Vector or text of comma-separated group labels.
#' @param scatter.var.from.matrix Indicates whether additional chart properties for Scatterplot, Labeled Scatterplot or Labeled Bubbleplot should by read from the \code{y} matrix.
#' @param scatter.x.var The index of the column of \code{y} (if \code{scatter.var.from.matrix} is \code{true}) or a numeric vector containing the x-coordinates of the scatterplot.
#' @param scatter.y.var The index of the column of \code{y} (if \code{scatter.var.from.matrix} is \code{true}) or a numeric vector containing the y-coordinates of the scatterplot.
#' @param scatter.labels.var The index of the column of \code{y} (if \code{scatter.var.from.matrix} is \code{true}) or a character vector containing the labels of the scatterplot.
#' @param scatter.sizes.var The index of the column of \code{y} (if \code{scatter.var.from.matrix} is \code{true}) or a  numeric vector. The absolute value will be used to determine the size of the points.
#' @param scatter.colors.var The index of the column of \code{y} (if \code{scatter.var.from.matrix} is \code{true}) or a  numeric vector. These shading of the points will be determined based on the value in this vector.
#' @param scatter.colors.as.group Boolean indicating whether \code{scatter.colors.var} should be treated as a grouping variable (i.e. categorical). If true, the legend will be shown and group labels will be ordered alphabetically or in the order given by \code{scatter.group.labels}.
#' @param us.date.format Whether to apply the US convention when parsing dates.
#' @param logos Optional list of images to be used to label scatterplot instead of the row names. It should be inputted as a comma-seperated list of URLs.
#' @param logo.size Numeric controlling the size of the logos.
#' @examples
#' z <- c(5, 6, 2, 1.5, 9, 2.2)
#' Chart(y = z, type = "Area")
#' @importFrom grDevices rgb
#' @importFrom flipFormat FormatAsReal
#' @importFrom flipTime PeriodNameToDate
#' @importFrom flipChartBasics ChartColors
#' @importFrom flipTransformations Factor AsNumeric TextAsVector
#' @importFrom plotly plot_ly config toRGB add_trace add_text layout hide_colorbar
#' @importFrom stats loess lm predict
#' @export
Chart <-   function(y = NULL,
                    type = "Column",
                    fit.type = "None", # can be "Smooth" or anything else
                    fit.ignore.last = FALSE,
                    fit.line.type = "dot",
                    fit.line.width = 1,
                    fit.line.name = "Fitted",
                    transpose = FALSE,
                    subset = NULL,
                    weights = NULL,
                    title = "",
                    title.font.family = NULL,
                    title.font.color = NULL,
                    title.font.size = 16,
                    subtitle = "",
                    subtitle.font.family = NULL,
                    subtitle.font.color = NULL,
                    subtitle.font.size = 12,
                    footer = "",
                    footer.font.family = NULL,
                    footer.font.color = NULL,
                    footer.font.size = 8,
                    footer.wordwrap = TRUE,
                    footer.wordwrap.nchar = 150,
                    colors = NULL,
                    colors.reverse = FALSE,
                    colors.custom.color = NA,
                    colors.custom.gradient.start = NA,
                    colors.custom.gradient.end = NA,
                    colors.custom.palette = NA,
                    fit.line.colors = NULL,
                    fit.line.colors.reverse = FALSE,
                    fit.line.colors.custom.color = NA,
                    fit.line.colors.custom.gradient.start = NA,
                    fit.line.colors.custom.gradient.end = NA,
                    fit.line.colors.custom.palette = NA,
                    opacity = NULL,
                    background.fill.color = rgb(255, 255, 255, maxColorValue = 255),
                    background.fill.opacity = 1,
                    charting.area.fill.color = rgb(255, 255, 255, maxColorValue = 255),
                    charting.area.fill.opacity = 1,
                    legend.show = TRUE,
                    legend.fill = rgb(255, 255, 255, maxColorValue = 255), # retained for backwards compatibility
                    legend.fill.color = legend.fill,
                    legend.fill.opacity = 1,
                    legend.border.color = rgb(44, 44, 44, maxColorValue = 255),
                    legend.border.line.width = 0,
                    legend.font.color = NULL,
                    legend.font.family = NULL,
                    legend.font.size = 10,
                    legend.position = "right",
                    legend.ascending = NA,
                    margin.top = NULL,
                    margin.bottom = NULL,
                    margin.left = NULL,
                    margin.right = NULL,
                    margin.inner.pad = NULL,
                    y.title = "",
                    y.title.font.color = NULL,
                    y.title.font.family = NULL,
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
                    y.grid.width = 1,
                    y.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                    y.tick.show = TRUE,
                    y.tick.suffix = "",
                    y.tick.prefix = "",
                    y.tick.decimals = NULL,
                    y.tick.format.manual = "",
                    y.hovertext.decimals = NULL,
                    y.hovertext.format.manual = "",
                    y.tick.angle = NULL,
                    y.tick.font.color = NULL,
                    y.tick.font.family = NULL,
                    y.tick.font.size = 10,
                    x.title = "",
                    x.title.font.color = NULL,
                    x.title.font.family = NULL,
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
                    x.tick.font.color = NULL,
                    x.tick.font.family = NULL,
                    x.tick.font.size = 10,
                    x.tick.label.autoformat = TRUE,
                    x.tick.label.wordwrap = TRUE,
                    wordwrap.nchar = 21,
                    series.marker.show = "none", # ignored
                    series.marker.colors = NULL,
                    series.marker.colors.reverse = FALSE,
                    series.marker.colors.custom.color = NA,
                    series.marker.colors.custom.gradient.start = NA,
                    series.marker.colors.custom.gradient.end = NA,
                    series.marker.colors.custom.palette = NA,
                    series.marker.opacity = 1,
                    series.marker.size = 6,
                    series.marker.border.width = 1,
                    series.marker.border.colors = NULL,
                    series.marker.border.colors.custom.color = NA,
                    series.marker.border.colors.custom.gradient.start = NA,
                    series.marker.border.colors.custom.gradient.end = NA,
                    series.marker.border.colors.custom.palette = NA,
                    series.marker.border.colors.reverse = FALSE,
                    series.marker.border.opacity = 1,
                    series.line.width = NULL,
                    series.line.colors = NULL,
                    series.line.colors.reverse = FALSE,
                    series.line.colors.custom.color = NULL,
                    series.line.colors.custom.gradient.start = NA,
                    series.line.colors.custom.gradient.end = NA,
                    series.line.colors.custom.palette = NA,
                    series.line.opacity = 1,
                    tooltip.show = TRUE,
                    modebar.show = FALSE,
                    global.font.family = "Arial",
                    global.font.color = rgb(44, 44, 44, maxColorValue = 255),
                    rows.to.ignore = "Total, NET, SUM",
                    cols.to.ignore = "Total, NET, SUM",
                    bar.gap = 0.15,
                    data.label.show = FALSE,
                    data.label.font.family = NULL,
                    data.label.font.size = 10,
                    data.label.font.color = NULL,
                    data.label.decimals = 2, # Ignored in Labeled Bubble and Scatterplots
                    data.label.prefix = "",
                    data.label.suffix = "",
                    data.label.threshold = NULL,
                    data.label.position = "top middle",
                    data.label.max.plot = 50,
                    pie.order = "initial",
                    pie.groups.order = "initial",
                    pie.subslice.colors = NULL,
                    pie.subslice.colors.reverse = FALSE,
                    pie.subslice.colors.custom.color = NA,
                    pie.subslice.colors.custom.gradient.start = NA,
                    pie.subslice.colors.custom.gradient.end = NA,
                    pie.subslice.colors.custom.palette = NA,
                    pie.subslice.colors.repeat = TRUE,
                    pie.border.color = rgb(255, 255, 255, maxColorValue = 255),
                    pie.inner.radius = 70,
                    pie.show.percentages = FALSE,
                    z.title = "",
                    trend.lines = FALSE,
                    scatter.group.indices = "",
                    scatter.group.labels = "",
                    scatter.var.from.matrix = TRUE,
                    scatter.colors.as.group = FALSE,
                    scatter.x.var = NULL,
                    scatter.y.var = NULL,
                    scatter.labels.var = NULL,
                    scatter.sizes.var = NULL,
                    scatter.colors.var = NULL,
                    us.date.format = NULL,
                    logos = NULL,
                    logo.size = 0.5)
{
    if (!is.null(weights))
        warning("Weights are currently not used.")
    if (length(subset) > 1 && (scatter.var.from.matrix ||
        !type %in% c("Scatterplot", "Labeled Scatterplot", "Labeled Bubbleplot")))
        warning("Filters are only used when 'Data source' is set to 'Variables'.")

    # Set undefined variables to defaults
    # This is for compatibility with R GUI controls
    if (type == "Donut")
        transpose <- FALSE
    if (type %in% c("Area", "Stacked", "100% Stacked Area"))
        series.line.width <- 0
    if (type != "Pie" || is.null(pie.subslice.colors) || pie.subslice.colors == "Group colors")
         pie.subslice.colors <- NULL
    if (type != "Labeled Bubbleplot")
        z.title <- ""
    if (!type %in% c("Scatterplot", "Labeled Scatterplot", "Labeled Bubbleplot"))
    {
        scatter.group.indices <- NULL
        scatter.group.labels <- NULL
    }
    if (type %in% c("Labeled Scatterplot", "Labeled Bubbleplot"))
    {
        data.label.decimals <- 2
        data.label.show <- TRUE
        series.marker.show <- "automatic"
    }
    if (type == "Bar" || type == "Stacked Bar")
    {
        y.tick.prefix <- ""
        y.tick.suffix <- ""
    }
    if (!type %in% c("Bar", "Stacked Bar", "Scatterplot", "Labeled Scatterplot", "Labeled Bubbleplot"))
    {
        x.tick.suffix <- ""
        x.tick.prefix <- ""
    }
    if (pie.show.percentages || type %in% c("100% Stacked Area", "100% Stacked Bar", "100% Stacked Column"))
    {
        x.tick.prefix <- ""
        x.tick.suffix <- ""
        y.tick.prefix <- ""
        y.tick.suffix <- ""
        data.label.prefix <- ""
        data.label.suffix <- ""
    }
    if (type == "Scatterplot")
    {
        series.mode <- if (is.null(series.line.width) || series.line.width == 0) "markers"
                       else "markers+lines"

        # Tick label formatting not required because it is always numeric
        x.tick.label.autoformat <- FALSE
    }
    if (type %in% c("Pie", "Donut"))
    {
        x.title <- ""
        y.title <- ""
        data.label.show <- TRUE
        if (pie.show.percentages)
            data.label.suffix <- "%"

    } else
    {
        pie.show.percentages <- FALSE
        pie.inner.radius <- NULL
    }
    if (!data.label.show)
    {
        data.label.decimals <- 2
    }



    if (!(type %in% c("Area", "Stacked Area", "100% Stacked Area", "Bar", "Stacked Bar", "100% Stacked Bar",
                "Column", "Stacked Column", "100% Stacked Column", "Line", "Pie", "Donut", "Scatterplot",
                "Labeled Scatterplot", "Labeled Bubbleplot", "Radar")))
        stop("The input chart type is not supported.")

    is.stacked <- type %in% c("Stacked Area", "100% Stacked Area",
                              "Stacked Bar", "100% Stacked Bar",
                              "Stacked Column", "100% Stacked Column")
    is.hundred.percent.stacked <- type %in% c("100% Stacked Area", "100% Stacked Bar", "100% Stacked Column")
    if (is.na(legend.ascending))
        legend.ascending <- type %in% c("Stacked Bar", "100% Stacked Bar") || !is.stacked

    swap.axes.and.data <- type %in% c("Bar", "Stacked Bar", "100% Stacked Bar")
    is.area.or.line.chart <- type %in% c("Area", "Stacked Area", "100% Stacked Area", "Line")
    is.bar.or.column.chart <- type %in% c("Bar", "Stacked Bar", "100% Stacked Bar",
                                          "Column", "Stacked Column", "100% Stacked Column")
    is.area.chart <- type %in% c("Area", "Stacked Area", "100% Stacked Area")
    is.pie.or.donut.chart <- type %in% c("Pie", "Donut")
    is.labeled.scatterplot.or.bubbleplot <-  type %in% c("Labeled Scatterplot", "Labeled Bubbleplot")
    is.scatterplot.or.bubbleplot <-  type %in% c("Scatterplot", "Labeled Scatterplot", "Labeled Bubbleplot")
    is.scatterplot <- type %in% c("Scatterplot")
    is.bar.chart <- type %in% c("Bar", "Stacked Bar", "100% Stacked Bar")
    is.column.chart <- type %in% c("Column", "Stacked Column", "100% Stacked Column")

    if (is.null(y) && !is.scatterplot.or.bubbleplot)
        stop("Input data 'y' is missing.")
    if (is.null(y) && is.null(scatter.x.var) && is.null(scatter.y.var))
        stop("Input data 'y' or 'scatter.x.var' or 'scatter.y.var' must be provided.")

    if (!is.area.chart && !is.bar.or.column.chart && !is.null(opacity))
        warning("The opacity parameter is only valid for area, bar and column charts.")
    if (is.null(opacity))
        opacity <- if (type == "Area") 0.4 else 1

    if (is.null(series.line.width))
        series.line.width <- if (is.area.chart || is.scatterplot) 0 else 3

    default.background.color <- rgb(255, 255, 255, maxColorValue = 255)
    if (((background.fill.color != default.background.color &&
         background.fill.opacity != 0) ||
         (charting.area.fill.color != default.background.color &&
         charting.area.fill.opacity != 0)) &&
         (is.pie.or.donut.chart || is.labeled.scatterplot.or.bubbleplot))
        warning("The background and charting area fill colors cannot be changed for pie charts, donut charts, labeled scatterplots or labeled bubbleplots.")

    if ((!is.null(margin.top) || !is.null(margin.bottom) || !is.null(margin.left) ||
        !is.null(margin.right) || !is.null(margin.inner.pad)) &&
        (is.pie.or.donut.chart || is.labeled.scatterplot.or.bubbleplot))
        warning("Margins cannot be set for pie charts, donut charts,
                labeled scatterplots or labeled bubbleplots.")

    if (!tooltip.show && (is.pie.or.donut.chart || is.labeled.scatterplot.or.bubbleplot))
        warning("Tooltips cannot be turned off for pie charts, donut charts,
                 labeled scatterplots or labeled bubbleplots.")

    if (modebar.show && (is.pie.or.donut.chart || is.labeled.scatterplot.or.bubbleplot))
        warning("There is no modebar for pie charts, donut charts, labeled scatterplots
                or labeled bubbleplots.")

    if (is.null(data.label.show))
        data.label.show <- is.pie.or.donut.chart || is.labeled.scatterplot.or.bubbleplot
    else if (!data.label.show && is.pie.or.donut.chart)
        warning("Data labels cannot be hidden for pie and donut charts.")

    # Handle multiple tables
    num.tables <- 1
    if (!is.null(logos) && nchar(logos) != 0 && is.null(dim(y[[1]])))
        y <- list(y)   # ensure rows are groups, so legend is plotted with labels

    if (!is.null(dim(y[[1]]))) {
        if (type != "Labeled Scatterplot")
            stop("Multiple tables can only be used with Labeled Scatterplot.")

        # Get table names
        num.tables <- length(y)
        y.names <- rep("", num.tables)
        used.names <- c()
        for (i in 1:num.tables)
        {
            if (is.null(attr(y[[i]], "name")))
            {
                attr(y[[i]], "name") <- as.character(i)
                used.names <- c(used.names, i)
            }
            y.names[i] <- attr(y[[i]], "name")[1]
        }

        # Check tables match - order of rows will match first table
        y[[1]] <- if (transpose) GetTidyTwoDimensionalArray(t(y[[1]]), rows.to.ignore, cols.to.ignore)
        else GetTidyTwoDimensionalArray(y[[1]], rows.to.ignore, cols.to.ignore)
        r.names <- rownames(y[[1]])
        c.names <- colnames(y[[1]])
        if (!is.null(r.names) && any(duplicated(r.names)) && trend.lines)
            stop("Row names of tables must be unique or NULL for trend lines to be plotted but are duplicated.")

        if (num.tables > 1) {
            for (i in 2:num.tables)
            {
                y[[i]] <- if (transpose) GetTidyTwoDimensionalArray(t(y[[i]]), rows.to.ignore, cols.to.ignore)
                            else GetTidyTwoDimensionalArray(y[[i]], rows.to.ignore, cols.to.ignore)

                if (!identical(r.names, rownames(y[[i]])))
                    stop(sprintf("Tables should have identical row names but table '%s' differs from table '%s'.",
                                 y.names[i], y.names[1]))
                if (!identical(c.names, colnames(y[[i]])))
                    stop(sprintf("Tables should have identical column names but table '%s' differs from table '%s'.",
                                 y.names[i], y.names[1]))
            }
        }
        y <- do.call(rbind, y)

        n1 <- nrow(y)/num.tables
        scatter.group.indices <- rep(seq(n1), num.tables)
        if (is.null(r.names)) {
            r.names <- paste("Row", seq(n1))
            rownames(y) <- rep(r.names, num.tables)
        }
        scatter.group.labels <- r.names
        if (transpose)     # already transposed when matching tables so do not do so later
            transpose <- FALSE
        if (is.null(logos) || nchar(logos) == 0)
            legend.show <- FALSE
    }

    # Identify logos
    logo.urls <- NULL
    if (!is.null(logos) && nchar(logos) != 0) {

        logo.urls <- try(TextAsVector(logos)) # This function gives warnings if it doesn't work
        if (!is.null(logo.urls) && !inherits(logo.urls, "try-error"))
        {
            logo.required.length <- if (num.tables > 1) n1 else {
                temp <- if (transpose) GetTidyTwoDimensionalArray(t(y), rows.to.ignore, cols.to.ignore)
                        else GetTidyTwoDimensionalArray(y, rows.to.ignore, cols.to.ignore)
                nrow(temp)
            }
            if (length(logo.urls) != logo.required.length)
                stop(sprintf("Number of URLs supplied in logos is %d but must be equal to the number of %s in the table (%d)\n",
                             length(logo.urls), ifelse(transpose, "columns", "rows"), logo.required.length))
            if (any(nchar(logo.urls) == 0))
                stop("Logos cannot be an empty string\n")
            if (num.tables > 1)
                logo.urls <- rep(logo.urls, num.tables)
            logo.size <- rep(logo.size, length(y))
        }
    }

    # Format input data
    if (is.scatterplot.or.bubbleplot)
    {
        # Get variables and variable names
        if (!scatter.var.from.matrix)
        {
            scatter.x.name <- ""
            scatter.y.name <- ""
            if (!is.null(scatter.x.var))
            {
                scatter.x.name <- deparse(substitute(scatter.x.var))
                if (!is.null(attr(scatter.x.var, "label")))
                    scatter.x.name <- attr(scatter.x.var, "label")
            }
            if (!is.null(scatter.y.var))
            {
                scatter.y.name <- deparse(substitute(scatter.y.var))
                if (!is.null(attr(scatter.y.var, "label")))
                    scatter.y.name <- attr(scatter.y.var, "label")
            }
            if (!is.null(scatter.labels.var))
            {
                scatter.labels.name <- deparse(substitute(scatter.labels.var))
                if (!is.null(attr(scatter.labels.var, "label")))
                    scatter.labels.name <- attr(scatter.labels.var, "label")
            }
            if (!is.null(scatter.colors.var))
            {
                scatter.colors.name <- deparse(substitute(scatter.colors.var))
                if (!is.null(attr(scatter.colors.var, "label")))
                    scatter.colors.name <- attr(scatter.colors.var, "label")
            }
            if (!is.null(scatter.sizes.var))
            {
                scatter.sizes.name <- deparse(substitute(scatter.sizes.var))
                if (!is.null(attr(scatter.sizes.var, "label")))
                    scatter.sizes.name <- attr(scatter.sizes.var, "label")
            }
        }
        else
        {
            if (!is.null(scatter.x.var) && scatter.x.var == 0)
                scatter.x.var <- NULL
            if (!is.null(scatter.y.var) && scatter.y.var == 0)
                scatter.y.var <- NULL
            if (!is.null(scatter.sizes.var) && scatter.sizes.var == 0)
                scatter.sizes.var <- NULL
            if (!is.null(scatter.colors.var) && scatter.colors.var == 0)
                scatter.colors.var <- NULL
            if (!is.null(scatter.labels.var) && scatter.labels.var == 0)
                scatter.labels.var <- NULL

            if (!is.null(scatter.x.var))
            {
                if (scatter.x.var < 1 || scatter.x.var > ncol(y))
                    stop("The variable for 'X-coordinates' must be an integer between 1 and ", ncol(y))
                scatter.x.var <- y[,scatter.x.var]
            }
            if (!is.null(scatter.y.var))
            {
                if (scatter.y.var < 1 || scatter.y.var > ncol(y))
                    stop("The variable for 'Y-coordinates' must be an integer between 1 and ", ncol(y))
                scatter.y.var <- y[,scatter.y.var]
            }
            if (!is.null(scatter.labels.var))
            {
                if (scatter.labels.var < 1 || scatter.labels.var > ncol(y))
                    stop("The variable for 'Labels' must be an integer between 1 and ", ncol(y))
                scatter.labels.name <- colnames(y)[scatter.labels.var]
                scatter.labels.var <- y[,scatter.labels.var]
            }
            if (!is.null(scatter.sizes.var))
            {
                if (scatter.sizes.var < 1 || scatter.sizes.var > ncol(y))
                    stop("The variable for 'Sizes' must be an integer between 1 and ", ncol(y))
                scatter.sizes.name <- colnames(y)[scatter.sizes.var]
                scatter.sizes.var <- y[,scatter.sizes.var]
            }
            if (!is.null(scatter.colors.var))
            {
                if (scatter.colors.var < 1 || scatter.colors.var > ncol(y))
                    stop("The variable 'Colors' must be an integer between 1 and ", ncol(y))
                scatter.colors.name <- colnames(y)[scatter.colors.var]
                scatter.colors.var <- y[,scatter.colors.var]
            }
        }
        if (nchar(footer) == 0)
        {
            if (!is.null(scatter.labels.var))
                footer <- sprintf("%sPoints labeled by '%s'; ", footer, scatter.labels.name)
            if (!is.null(scatter.colors.var))
                footer <- sprintf("%sPoints colored according to '%s'; ", footer, scatter.colors.name)
            if (!is.null(scatter.sizes.var))
                footer <- sprintf("%sPoints sizes are proportional to absolute value of '%s'; ", footer, scatter.sizes.name)
        }
        chart.matrix <- if (is.null(scatter.x.var) && is.null(scatter.y.var)) y
                        else cbind(if (is.null(scatter.x.var)) 0 else AsNumeric(scatter.x.var, binary=F),
                                   if (is.null(scatter.y.var)) 0 else AsNumeric(scatter.y.var, binary=F))
        if (!is.null(scatter.x.var) || !is.null(scatter.y.var))
            colnames(chart.matrix)[1:2] <- c(scatter.x.name, scatter.y.name)

        if (!is.null(scatter.sizes.var))
        {
            sc <- abs(AsNumeric(scatter.sizes.var, binary=F))
            if (inherits(scatter.sizes.var, "Date") || inherits(scatter.sizes.var, "POSIXct") ||
                inherits(scatter.sizes.var, "POSIXt"))
                sc <- sc - min(sc, na.rm=T)
            if (ncol(chart.matrix) >= 3)
                chart.matrix[,3] <- sc
            else
            {
                chart.matrix <- if (ncol(chart.matrix) == 2) cbind(chart.matrix, sc)
                                else cbind(chart.matrix, 0, sc)
            }
        }

        if (!is.null(scatter.labels.var))
            rownames(chart.matrix) <- scatter.labels.var
        if (!is.null(scatter.colors.var) && scatter.colors.as.group)
        {
            ind.na <- which(is.na(scatter.colors.var))
            tmp.factor <- Factor(scatter.colors.var)
            tmp.ordered <- is.numeric(scatter.colors.var) || is.ordered(tmp.factor) ||
                           is.factor(scatter.colors.var)
            if (all(nchar(scatter.group.labels)==0))
            {
                scatter.group.labels <- levels(tmp.factor)
                if (!tmp.ordered)
                    scatter.group.labels <- sort(scatter.group.labels)
                if (length(ind.na) > 0)
                {
                    scatter.group.labels <- c(scatter.group.labels, "NA")
                    levels(tmp.factor) <- scatter.group.labels
                    tmp.factor[ind.na] <- "NA"
                }
            }
            tmp.factor <- factor(tmp.factor, levels=scatter.group.labels)
            scatter.group.indices <- as.numeric(tmp.factor)
            scatter.colors.var <- NULL
        }
        if (anyDuplicated(chart.matrix, margin=1))
            warning("Chart contains overlapping points in the same position.")
        if (length(subset) > 1 && !scatter.var.from.matrix)
        {
            chart.matrix <- chart.matrix[subset,]
            scatter.group.indices <- scatter.group.indices[subset]
        }

        # this must be determined before the margin sizes are calculated
        scatterplot.data <- scatterplotData(chart.matrix,
                                            type = type,
                                            colors = colors,
                                            colors.reverse = colors.reverse,
                                            colors.custom.color = colors.custom.color,
                                            colors.custom.gradient.start = colors.custom.gradient.start,
                                            colors.custom.gradient.end = colors.custom.gradient.end,
                                            colors.custom.palette = colors.custom.palette,
                                            colorscale.variable = scatter.colors.var,
                                            group.labels.text = scatter.group.labels,
                                            group.indices.text = scatter.group.indices,
                                            origin = FALSE, # base on y and x.zero.line.width
                                            transpose = transpose,
                                            rows.to.ignore = rows.to.ignore,
                                            cols.to.ignore = cols.to.ignore,
                                            legend.show = legend.show,
                                            x.title = x.title,
                                            y.title = y.title,
                                            logos = logo.urls)

        if (x.title == "")
            x.title <- scatterplot.data$x.title
        if (y.title == "")
            y.title <- scatterplot.data$y.title

        y.abs.max <- max(abs(range(scatterplot.data$y, na.rm=T)), na.rm=T)
        if (!is.finite(y.abs.max) || y.abs.max == 0 || any(abs(range(scatterplot.data$y, na.rm=T))/y.abs.max < 1e-2))
            y.zero <- FALSE
    }
    else
    {
        chart.matrix <- y
    }

    # Check that chart.matrix is suitable for charting
    msg <- paste("The input data is not appropriate.",
                 "A numeric Q table, a numeric R vector, a numeric R matrix",
                 "or a data frame consisting entirely of numerics is required.")
    if (is.data.frame(chart.matrix))
    {
        if (sum(sapply(chart.matrix, is.numeric)) == ncol(chart.matrix))
            chart.matrix <- as.matrix(chart.matrix)
        else
            stop(msg)
    }
    if (scatter.var.from.matrix && !is.numeric(chart.matrix))
        stop(msg)

    if (is.null(colors))
        colors <- "Default colors"

    if (is.null(charting.area.fill.color))
        charting.area.fill.color <- background.fill.color
    if (is.null(charting.area.fill.opacity))
        charting.area.fill.opacity <- background.fill.opacity

    # Truncate dimensions if there are more than 2
    n.dimensions <- length(dim(chart.matrix))
    if (n.dimensions > 2)
    {
        chart.matrix <- eval(parse(text =
            paste0("chart.matrix[,,", paste0(rep("1", n.dimensions - 2), collapse = ","), "]")))
        warning("The input has more than 2 dimensions, only the first 2 have been displayed.")
    }

    ## Is it a Q input?
    qinput <- (!is.null(attr(chart.matrix, "statistic")))
    table.axes.labels <- c("", "")

    if (qinput)
    {
        table.axes.labels <- attr(chart.matrix, "questions")
        table.statistic <- attr(chart.matrix, "statistic")
    } else {
        if (length(names(dimnames(chart.matrix))) == 2)
            table.axes.labels <- names(dimnames(chart.matrix))
        table.statistic <- ""
    }

    ## If it's an array with an attribute of names, then assign those names to the rows
    if (is.array(chart.matrix) && length(names(chart.matrix)) != 0)
        rownames(chart.matrix) <- names(chart.matrix)


    if (!is.scatterplot.or.bubbleplot)
    {
        ## Convert arrays to matrices
        if (is.array(chart.matrix))
            chart.matrix <- as.matrix(chart.matrix)

        ## Convert vectors to matrices
        if (any(c("numeric", "integer") %in% class(chart.matrix)))
            chart.matrix <- as.matrix(chart.matrix)

        ## Transform chart.matrix based on transposition requirements.
        if (transpose && ncol(chart.matrix) == 1)
            warning("The input table cannot be transposed as it has only one column.")
        else if (transpose || (nrow(chart.matrix) == 1 && ncol(chart.matrix) > 1))
        {
            chart.matrix <- t(chart.matrix)
            table.axes.labels <- rev(table.axes.labels)
            temp <- rows.to.ignore
            rows.to.ignore <- cols.to.ignore
            cols.to.ignore <- temp
        }

        ## Use default row and column labels, if they are missing from the matrix
        if (is.null(rownames(chart.matrix)))
            rownames(chart.matrix) <- 1:nrow(chart.matrix)
        if (is.null(colnames(chart.matrix)) && ncol(chart.matrix) > 1)
            colnames(chart.matrix) <- paste0("Series", 1:ncol(chart.matrix))

        ## Ignore rows or columns, using flipData::GetTidyTwoDimensionalArray()
        chart.matrix <- flipData::GetTidyTwoDimensionalArray(chart.matrix, rows.to.ignore, cols.to.ignore)

        ## Error if there is only one series when multiple series are required
        if (ncol(chart.matrix) == 1)
        {
            if (type %in% c("Stacked Area", "100% Stacked Area"))
                stop(paste(type, "requires more than one series. Use Area charts instead for this data."))
            if (type %in% c("Stacked Bar", "100% Stacked Bar"))
                stop(paste(type, "requires more than one series. Use Bar charts instead for this data."))
            if (type %in% c("Stacked Column", "100% Stacked Column"))
                stop(paste(type, "requires more than one series. Use Column charts instead for this data."))
        }
        if (is.stacked && (any(is.na(chart.matrix)) || any(chart.matrix < 0)))
            stop("Stacked charts cannot be produced with missing or negative values.")
        if (is.hundred.percent.stacked && any(rowSums(chart.matrix) == 0))
            stop("100% stacked charts cannot be produced with rows that do not contain positive values.")

        nms <- row.names(chart.matrix)
        if (length(nms) > length(unique(nms)))
            stop("Row names of the input table must be unique.")

        original.chart.matrix <- chart.matrix

        value.title <- if (table.statistic != "")
            table.statistic
        else if (length(colnames(chart.matrix)) == 1)
            colnames(chart.matrix)
        else
            ""

        ## If no x.title or y.title provided, take defaults from data input
        if (x.title == "" || length(x.title) == 0)
            x.title <- if (swap.axes.and.data) value.title else table.axes.labels[1]

        if (x.title == "FALSE" || x.title == FALSE)
            x.title <- ""

        if (y.title == "" || length(y.title) == 0)
            y.title <- if (swap.axes.and.data) table.axes.labels[1] else value.title

        if (y.title == "FALSE" || y.title == FALSE)
            y.title <- ""
    }

    # Use global fonts if necessary
    if (is.null(title.font.family) || title.font.family == "") title.font.family <- global.font.family
    if (is.null(legend.font.family) || legend.font.family == "") legend.font.family <- global.font.family
    if (is.null(footer.font.family) || footer.font.family == "") footer.font.family <- global.font.family
    if (is.null(y.title.font.family) || y.title.font.family == "") y.title.font.family <- global.font.family
    if (is.null(y.tick.font.family) || y.tick.font.family == "") y.tick.font.family <- global.font.family
    if (is.null(x.title.font.family) || x.title.font.family == "") x.title.font.family <- global.font.family
    if (is.null(x.tick.font.family) || x.tick.font.family == "") x.tick.font.family <- global.font.family
    if (is.null(data.label.font.family) || data.label.font.family == "") data.label.font.family <- global.font.family

    # Use global colours if necessary
    if (is.null(title.font.color)) title.font.color <- global.font.color
    if (is.null(subtitle.font.color)) subtitle.font.color <- global.font.color
    if (is.null(legend.font.color)) legend.font.color <- global.font.color
    if (is.null(footer.font.color)) footer.font.color <- global.font.color
    if (is.null(y.title.font.color)) y.title.font.color <- global.font.color
    if (is.null(y.tick.font.color)) y.tick.font.color <- global.font.color
    if (is.null(x.title.font.color)) x.title.font.color <- global.font.color
    if (is.null(x.tick.font.color)) x.tick.font.color <- global.font.color
    if (is.null(data.label.font.color)) data.label.font.color <- global.font.color

    # Default margins
    is.default.margin.top <- is.null(margin.top)
    is.default.margin.bottom <- is.null(margin.bottom)
    is.default.margin.left <- is.null(margin.left)
    is.default.margin.right <- is.null(margin.right)
    if (is.null(margin.top))
    {
        margin.top <- 20
        title.nline <- 0
        if (nchar(title) > 0)
        {
            title.nline <- sum(gregexpr("<br>", title)[[1]] > -1) + 1
            margin.top <- margin.top + (title.font.size * title.nline * 1.25)
        }
        if (nchar(subtitle) > 0)
        {
            subtitle.nline <- sum(gregexpr("<br>", subtitle)[[1]] > -1) + 1.5
            margin.top <- margin.top + (subtitle.font.size * subtitle.nline * 1.25)
            subtitle.npad <- max(0, round(title.nline * subtitle.font.size/title.font.size * 0.9))
            subtitle <- paste0(paste(rep("<br>", subtitle.npad), collapse=""), subtitle)
        }
    }
    if (is.null(margin.bottom))
    {
        margin.bottom <- if (type != "Radar") 50
                         else 20
        if (nchar(x.title) > 0 && type != "Radar")
        {
            x.title.nline <- sum(gregexpr("<br>", x.title)[[1]] > -1) + 1
            margin.bottom <- margin.bottom + (x.title.font.size * x.title.nline * 1.25)
        }
        if (nchar(footer) > 0)
        {
            footer <- autoFormatLongLabels(footer, wordwrap=footer.wordwrap, n=footer.wordwrap.nchar, truncate=FALSE)
            footer.nline <- sum(gregexpr("<br>", footer)[[1]] > -1) + 2 + (type == "Radar")
            margin.bottom <- margin.bottom + (footer.font.size * footer.nline * 1.25)
            # footer position cannot be determined until after x-axis labels have been formatted
        }
    }
    if (is.null(margin.left))
        margin.left <- if (type == "Radar") 60 else 80
    if (is.null(margin.right))
        margin.right <- 60
    if (is.null(margin.inner.pad))
        margin.inner.pad <- 0

    ## Set defaults for chart specific items
    fill.bound <- ""
    legend.group <- ""
    barmode <- ""
    orientation <- NULL
    x.tickformat <- NULL
    y.tickformat <- NULL
    connectgap <- NULL

    ## Settings specific to Area Charts
    if (type == "Area" | type == "Stacked Area" | type == "100% Stacked Area")
    {
        chart.type.outputs <- areaChart(chart.matrix = chart.matrix,
                                        opacity = opacity,
                                        type = type,
                                        y.tick.format.manual = y.tick.format.manual,
                                        y.tick.suffix = y.tick.suffix,
                                        series.line.width = series.line.width,
                                        series.marker.show = series.marker.show)

        chart.matrix <- chart.type.outputs$chart.matrix
        fill.bound <- chart.type.outputs$fill.bound
        legend.group <- chart.type.outputs$legend.group
        y.tickformat <- chart.type.outputs$y.tickformat
        series.mode <- chart.type.outputs$series.mode
        opacity <- chart.type.outputs$opacity
    }

    ## Settings specific to Line Charts
    if (type == "Line")
    {
        chart.type.outputs <- lineChart(chart.matrix = chart.matrix,
                                        series.line.width = series.line.width,
                                        series.marker.show = series.marker.show)

        chart.matrix <- chart.type.outputs$chart.matrix
        series.mode <- chart.type.outputs$series.mode
        series.line.width <- chart.type.outputs$series.line.width
        y.tickformat <- ""
        connectgap <- TRUE
    }

    ## Settings specific to Column Charts
    if (type == "Column" | type == "Stacked Column" | type == "100% Stacked Column")
    {
        chart.type.outputs <- columnChart(chart.matrix = chart.matrix,
                                          type = type,
                                          y.tick.format.manual = y.tick.format.manual,
                                          series.marker.border.width = series.marker.border.width)

        chart.matrix <- chart.type.outputs$chart.matrix
        legend.group <- chart.type.outputs$legend.group
        y.tickformat <- chart.type.outputs$y.tickformat
        series.mode <- ""
        orientation <- chart.type.outputs$orientation
        barmode <- chart.type.outputs$barmode
    }

    ## Settings specific to Bar Charts
    if (type == "Bar" | type == "Stacked Bar" | type == "100% Stacked Bar")
    {
        chart.type.outputs <- barChart(chart.matrix = chart.matrix,
                                       type = type,
                                       x.tick.format.manual = x.tick.format.manual,
                                       series.marker.border.width = series.marker.border.width)

        chart.matrix <- chart.type.outputs$chart.matrix
        legend.group <- chart.type.outputs$legend.group
        x.tickformat <- chart.type.outputs$x.tickformat
        series.mode <- ""
        orientation <- chart.type.outputs$orientation
        barmode <- chart.type.outputs$barmode
    }

    ## Color inheritance - first run
    if (is.null(series.line.colors))
    {
        series.line.colors <- colors
        series.line.colors.reverse <- colors.reverse
        series.line.colors.custom.color <- colors.custom.color
        series.line.colors.custom.gradient.start <- colors.custom.gradient.start
        series.line.colors.custom.gradient.end <- colors.custom.gradient.end
        series.line.colors.custom.palette <- colors.custom.palette
    }

    if (fit.type != "None" && is.null(fit.line.colors))
    {
        fit.line.colors <- colors
        fit.line.colors.reverse <- colors.reverse
        fit.line.colors.custom.color <- colors.custom.color
        fit.line.colors.custom.gradient.start <- colors.custom.gradient.start
        fit.line.colors.custom.gradient.end <- colors.custom.gradient.end
        fit.line.colors.custom.palette <- colors.custom.palette
    }

    if (is.null(series.marker.colors))
    {
        series.marker.colors <- series.line.colors
        series.marker.colors.reverse <- series.line.colors.reverse
        series.marker.colors.custom.color <- series.line.colors.custom.color
        series.marker.colors.custom.gradient.start <- series.line.colors.custom.gradient.start
        series.marker.colors.custom.gradient.end <- series.line.colors.custom.gradient.end
        series.marker.colors.custom.palette <- series.line.colors.custom.palette
    }

    if (is.null(series.marker.border.colors))
    {
        series.marker.border.colors <- series.marker.colors
        series.marker.border.colors.reverse <- series.marker.colors.reverse
        series.marker.border.colors.custom.color <- series.marker.colors.custom.color
        series.marker.border.colors.custom.gradient.start <- series.marker.colors.custom.gradient.start
        series.marker.border.colors.custom.gradient.end <- series.marker.colors.custom.gradient.end
        series.marker.border.colors.custom.palette <- series.marker.colors.custom.palette
    }

    if (type == "Pie" || type == "Donut")
        return(pieChart(
                chart.matrix = chart.matrix,
                type = type,
                values.color = colors,
                colors.reverse = colors.reverse,
                colors.custom.color = colors.custom.color,
                colors.custom.gradient.start = colors.custom.gradient.start,
                colors.custom.gradient.end = colors.custom.gradient.end,
                colors.custom.palette = colors.custom.palette,
                title = title,
                title.font.family = title.font.family,
                title.font.size = title.font.size,
                title.font.color = title.font.color,
                pie.values.font.family = data.label.font.family,
                pie.values.font.size = data.label.font.size,
                pie.values.prefix = data.label.prefix,
                pie.values.suffix = data.label.suffix,
                pie.data.threshold = data.label.threshold,
                pie.values.order = pie.order,
                pie.values.decimals = data.label.decimals,
                pie.labels.font.family = data.label.font.family,
                pie.labels.font.size = data.label.font.size,
                pie.labels.font.color = data.label.font.color,
                pie.groups.font.family = data.label.font.family,
                pie.groups.font.size = data.label.font.size,
                pie.groups.font.color = data.label.font.color,
                pie.subslice.colors = pie.subslice.colors,
                pie.subslice.colors.custom.color = pie.subslice.colors.custom.color,
                pie.subslice.colors.custom.gradient.start = pie.subslice.colors.custom.gradient.start,
                pie.subslice.colors.custom.gradient.end = pie.subslice.colors.custom.gradient.end,
                pie.subslice.colors.custom.palette = pie.subslice.colors.custom.palette,
                pie.subslice.colors.reverse = pie.subslice.colors.reverse,
                pie.groups.order = pie.groups.order,
                pie.inner.radius = pie.inner.radius,
                pie.subslice.colors.repeat = pie.subslice.colors.repeat,
                pie.border.color = pie.border.color,
                pie.show.percentages = pie.show.percentages,
                table.statistic = table.statistic))


    ## Settings specific to labeled scatter plots
    if (is.labeled.scatterplot.or.bubbleplot)
    {
        draw.grid <- (x.grid.width != 0 && y.grid.width != 0)
        if (xor(x.grid.width != 0, y.grid.width != 0))
            warning(paste("The x-axis and y-axis grid widths cannot be separately set to zero for",
                    "Labeled Scatterplots and Labeled Bubbleplots."))
        if ((x.grid.width != 0 && x.grid.width != 1) || (y.grid.width != 0 && y.grid.width != 1))
            warning(paste("The x-axis and y-axis grid widths cannot be adjusted for",
                          "Labeled Scatterplots and Labeled Bubbleplots."))

        point.radius <- if (is.null(series.marker.show) || series.marker.show == "automatic")
            0.5 * series.marker.size
        else
            0

        label.plot <- scatterplot.data$label
        n.lab <- length(label.plot)
        if (is.finite(data.label.max.plot) && data.label.max.plot < 0)
            data.label.max.plot <- NA
        if (is.finite(data.label.max.plot) && data.label.max.plot < n.lab)
        {
            if (data.label.max.plot == 50)
                warning("By default, only the first 50 labels are shown to avoid long running times. Adjust 'Maximum data labels to plot' to show more labels. Alternatively, to show a large number of points, show as 'Hovertext' instead.")
            else
                warning("Some labels have been hidden. Adjust 'Maximum data labels to plot' to show more labels.")
            label.plot[(data.label.max.plot + 1):n.lab] <- ""
        }
        if (is.null(scatterplot.data$label))
            warning("No labels were provided for a Labeled Scatterplot. Consider trying Scatterplot instead.")

        return(rhtmlLabeledScatter::LabeledScatter(X = scatterplot.data$x,
                       Y = scatterplot.data$y,
                       Z = scatterplot.data$z,
                       label = label.plot,
                       label.alt = scatterplot.data$label.alt,
                       fixed.aspect = FALSE,
                       group = if (length(unique(scatterplot.data$group)) == 1) NULL else scatterplot.data$group,
                       grid = draw.grid,
                       origin = scatterplot.data$origin,
                       origin.align = FALSE,
                       labels.show = data.label.show,
                       legend.show = scatterplot.data$legend.show,
                       legend.bubbles.show = scatterplot.data$legend.bubbles.show,
                       legend.font.color = legend.font.color,
                       legend.font.family = legend.font.family,
                       legend.font.size = legend.font.size,
                       legend.bubble.font.color = legend.font.color,
                       legend.bubble.font.family = legend.font.family,
                       legend.bubble.font.size = legend.font.size,
                       legend.bubble.title.font.color = legend.font.color,
                       legend.bubble.title.font.family = legend.font.family,
                       legend.bubble.title.font.size = legend.font.size,
                       colors = scatterplot.data$colors,
                       y.title = scatterplot.data$y.title,
                       y.title.font.family = y.title.font.family,
                       y.title.font.color = y.title.font.color,
                       y.title.font.size = y.title.font.size,
                       axis.font.family = y.tick.font.family,
                       axis.font.color = y.tick.font.color,
                       axis.font.size = y.tick.font.size,
                       x.title = scatterplot.data$x.title,
                       x.title.font.family = x.title.font.family,
                       x.title.font.color = x.title.font.color,
                       x.title.font.size = x.title.font.size,
                       z.title = z.title,
                       x.decimals = if (is.null(x.tick.decimals)) decimalsToDisplay(scatterplot.data$x) else x.tick.decimals,
                       y.decimals = if (is.null(y.tick.decimals)) decimalsToDisplay(scatterplot.data$y) else y.tick.decimals,
                       z.decimals = if (is.null(data.label.decimals)) 1 else data.label.decimals,
                       x.prefix = x.tick.prefix,
                       y.prefix = y.tick.prefix,
                       z.prefix = data.label.prefix,
                       x.suffix = x.tick.suffix,
                       y.suffix = y.tick.suffix,
                       z.suffix = data.label.suffix,
                       title.font.family = title.font.family,
                       title.font.color = title.font.color,
                       title.font.size = title.font.size,
                       labels.font.family = data.label.font.family,
                       labels.font.color = data.label.font.color,
                       labels.font.size = data.label.font.size,
                       point.radius = point.radius,
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
                       labels.logo.scale = logo.size
                       ))
    }

    if (!is.scatterplot.or.bubbleplot)
    {
        ## Work out color ranges; n.b. some color ranges worked out in the chart specific functions.
        ## This must occur after pie/donut charts are plotted because they use a different number of colors
        ## Scatterplots, labeled scatterplots and bubblecharts also handle groups separately

        number.colors.needed <- ncol(chart.matrix)
        if (length(number.colors.needed) == 0 || number.colors.needed == 0)
            stop("Chart matrix is empty.")

        colors <- ChartColors(number.colors.needed = number.colors.needed,
                                               given.colors = colors,
                                               custom.color = colors.custom.color,
                                               custom.gradient.start = colors.custom.gradient.start,
                                               custom.gradient.end = colors.custom.gradient.end,
                                               custom.palette = colors.custom.palette,
                                               reverse = colors.reverse,
                                               trim.light.colors = TRUE)
        if (fit.type != "None")
            fit.line.colors <- ChartColors(number.colors.needed = number.colors.needed,
                                               given.colors = fit.line.colors,
                                               custom.color = fit.line.colors.custom.color,
                                               custom.gradient.start = fit.line.colors.custom.gradient.start,
                                               custom.gradient.end = fit.line.colors.custom.gradient.end,
                                               custom.palette = fit.line.colors.custom.palette,
                                               reverse = fit.line.colors.reverse,
                                               trim.light.colors = TRUE)

        series.line.colors <- suppressWarnings(ChartColors(number.colors.needed = number.colors.needed,
                                               given.colors = series.line.colors,
                                               custom.color = series.line.colors.custom.color,
                                               custom.gradient.start = series.line.colors.custom.gradient.start,
                                               custom.gradient.end = series.line.colors.custom.gradient.end,
                                               custom.palette = series.line.colors.custom.palette,
                                               reverse = series.line.colors.reverse,
                                               trim.light.colors = TRUE))
        series.marker.colors <- suppressWarnings(ChartColors(number.colors.needed = number.colors.needed,
                                               given.colors = series.marker.colors,
                                               custom.color = series.marker.colors.custom.color,
                                               custom.gradient.start = series.marker.colors.custom.gradient.start,
                                               custom.gradient.end = series.marker.colors.custom.gradient.end,
                                               custom.palette = series.marker.colors.custom.palette,
                                               reverse = series.marker.colors.reverse,
                                               trim.light.colors = TRUE))
        series.marker.border.colors <- suppressWarnings(ChartColors(number.colors.needed = number.colors.needed,
                                               given.colors = series.marker.border.colors,
                                               custom.color = series.marker.border.colors.custom.color,
                                               custom.gradient.start = series.marker.border.colors.custom.gradient.start,
                                               custom.gradient.end = series.marker.border.colors.custom.gradient.end,
                                               custom.palette = series.marker.border.colors.custom.palette,
                                               reverse = series.marker.border.colors.reverse,
                                               trim.light.colors = TRUE))


        ## Color inheritance - second run
        if (is.null(series.line.colors))
            series.line.colors <- colors

        if (is.null(series.marker.colors))
            series.marker.colors <- series.line.colors

        if (is.null(series.marker.border.colors))
            series.marker.border.colors <- series.marker.colors
    }

    is.x.axis.numeric <- swap.axes.and.data || (is.area.or.line.chart &&
                         all(!is.na(suppressWarnings(as.numeric(row.names(chart.matrix))))))

    x.axis.type <- if (is.x.axis.numeric || is.scatterplot) "linear" else "category"
    y.axis.type <- if (swap.axes.and.data) "category" else "linear"

    ## Get axes labels from the matrix labels
    x.labels <- rownames(chart.matrix)
    y.labels <- colnames(chart.matrix)

    ymd <- PeriodNameToDate(x.labels, us.format = us.date.format)
    if (!any(is.na(ymd)) && (is.area.or.line.chart ||
         (is.bar.chart && length(x.labels) > 6) ||
         (is.column.chart && length(x.labels) > 6)))
    {
        use.dates <- TRUE
        x.labels <- ymd
        if (swap.axes.and.data)
            y.axis.type <- "date"
        else
            x.axis.type <- "date"
    }
    else
    {
        ymd <- NULL
        x.labels <- autoFormatLongLabels(x.labels, wordwrap = x.tick.label.wordwrap, n = wordwrap.nchar)
    }

    # Adjust tick label orientation and margins
    if (x.tick.label.autoformat && type != "Radar")
    {
        lab.nchar <- max(nchar(unlist(strsplit(split="<br>", as.character(x.labels)))))
        font.asp <- switch(tolower(x.tick.font.family),
                              'arial'= 0.54,
                              'arial black' = 0.63,
                              'century gothic' = 0.61,
                              'courier new' = 0.63,
                              'impact' = 0.48,
                              'open sans' = 0.45,
                              'times new roman' = 0.45,
                              'tahoma' = 0.52,
                              'trebuchet' = 0.48,
                              'verdana' = 0.63,
                              0.54)
        lab.len <- if (is.bar.chart) font.asp * y.tick.font.size * lab.nchar
                   else font.asp * x.tick.font.size * lab.nchar
        lab.nline <- if (is.character(x.labels)) max(sapply(gregexpr("<br>", x.labels),
                         function(x){sum(x > -1)}))
                     else 0

        if (is.null(x.tick.angle))
        {
            x.tick.angle <- if (length(x.labels) > 9 && lab.nchar > 5 &&
                                x.tick.label.autoformat && !is.bar.chart) 90
                            else 0
        }


        if (lab.len > 50 || (!is.null(lab.nline) && lab.nline > 0))
        {
            new.margin <- lab.len
            if (is.bar.chart)
            {
                if (y.position == "right")
                {
                    if (is.default.margin.right)
                        margin.right <- new.margin + 5 + y.title.font.size*(y.title != "")*1.25
                }
                else if (is.default.margin.left)
                    margin.left <- new.margin + 5 + y.title.font.size*(y.title != "")*1.25
            }
            else if (is.default.margin.bottom)
            {
                if (x.tick.angle != 0)
                    margin.bottom <- margin.bottom + new.margin - 40
                else
                    margin.bottom <- margin.bottom + 1.25*x.tick.font.size*floor(lab.nline)
            }
        }
    }

    # Finalize footer and subtitle
    # This should be done before radarchart is called
    footer.axis <- NULL
    subtitle.axis <- NULL
    if (nchar(footer) > 0)
    {
        footer.nline <- sum(gregexpr("<br>", footer)[[1]] > -1) + 1
        footer.npad <- max(0, ceiling(margin.bottom/footer.font.size/1.25) - footer.nline - 2)
        footer <- paste0(paste(rep("<br>", footer.npad), collapse=""), footer)
        footer.axis <- list(overlaying="x", side = "bottom", anchor="free", position=0,
             visible=T, showline=F, zeroline=F, showgrid=F,
             tickfont=list(family=footer.font.family, size=footer.font.size, color=footer.font.color),
             range=c(0,1), tickvals=c(0.5), ticktext=c(footer), tickangle=0)
    }
    if (nchar(subtitle) > 0)
    {
        subtitle.axis <- list(overlaying="x", side="top", anchor="free", position=1,
             showline=F, zeroline=F, showgrid=F, showticklabels=F, title=subtitle,
             titlefont=list(family=subtitle.font.family, size=subtitle.font.size, color=subtitle.font.color))
    }


    # Bar and column chart data label annotations
    data.label.mult <- 1
    if (is.hundred.percent.stacked)
    {
        data.label.suffix <- paste0(data.label.suffix, "%")
        data.label.mult <- 100
    }
    data.annotations <- if (data.label.show && is.bar.or.column.chart)
        dataLabelAnnotation(chart.matrix = chart.matrix,
                            data.label.mult = data.label.mult,
                            bar.decimals = data.label.decimals,
                            bar.prefix = data.label.prefix,
                            bar.suffix = data.label.suffix,
                            barmode = barmode,
                            swap.axes.and.data = swap.axes.and.data,
                            bar.gap = bar.gap,
                            display.threshold = data.label.threshold,
                            dates = ymd)
    else
        list()

    ## Position legend
    legend.x.anchor <- "left"
    legend.y.anchor <- "auto"
    legend.y <- 1
    legend.x <- 1.02

    legend.sort.order <- "normal"
    if (legend.ascending == FALSE)
        legend.sort.order <- "reversed"

    ### If legend on right and there's a y-axis on the right too:
    y2 = ""
    if (legend.position == "right" && (y2 != "" || y.position == "right"))
        legend.x = 1.15

    ### If legend on the left, and there's no y-axis on the left:
    if (legend.position == "left" && y.position == "right" && y2 != "")
    {
        legend.x.anchor <- "right"
        legend.x <- -.02
    }
    else if (legend.position == "left")
    {
        legend.x.anchor <- "right"
        legend.x <- -.15
        margin.r <- 80
    }

    if (type == "Radar")
        return(radarChart(chart.matrix,
                    title,
                    title.font.family,
                    title.font.color,
                    title.font.size,
                    colors,
                    background.fill.color,
                    background.fill.opacity,
                    charting.area.fill.color,
                    charting.area.fill.opacity,
                    legend.show,
                    legend.fill.color,
                    legend.fill.opacity,
                    legend.border.color,
                    legend.border.line.width,
                    legend.font.color,
                    legend.font.family,
                    legend.font.size,
                    legend.x.anchor,
                    legend.y.anchor,
                    legend.y,
                    legend.x,
                    legend.sort.order,
                    margin.top,
                    margin.bottom,
                    margin.left,
                    margin.right,
                    margin.inner.pad,
                    series.marker.colors,
                    series.marker.size,
                    series.line.width,
                    tooltip.show,
                    modebar.show,
                    x.title.font.color,
                    x.title.font.family,
                    x.title.font.size,
                    x.grid.width,
                    x.grid.color,
                    y.bounds.minimum,
                    y.bounds.maximum,
                    y.tick.distance,
                    y.grid.width,
                    y.grid.color,
                    y.tick.show,
                    y.tick.suffix, # take from data.suffix
                    y.tick.prefix,
                    y.tick.decimals,
                    y.hovertext.decimals,
                    y.tick.font.color,
                    y.tick.font.family,
                    y.tick.font.size,
                    x.tick.label.wordwrap,
                    wordwrap.nchar,
                    data.label.show,
                    data.label.font.family,
                    data.label.font.size,
                    data.label.font.color,
                    data.label.decimals, # Ignored in Labeled Bubble and Scatterplots
                    data.label.prefix,
                    data.label.suffix,
                    subtitle.axis,
                    footer.axis))


    ## If line thickness is zero, then we shouldn't show a line; ticks only shown if there's a line (same as Excel)
    ## Tick labels only shown if there's a line.
    y.showline <- FALSE
    y.showticks <- FALSE

    ## If no line, then set to NULL to satisfy Plotly
    if (y.line.width == 0)
        y.line.width <- NULL

    if (y.line.width >= 1 && !is.null(y.line.width))
    {
        y.showline <- TRUE
        # Default to outer tick marks if they are to be shown, but have not been specified
        if (y.tick.marks == "")
        {
            y.showticks <- TRUE
            y.tick.marks <- "outside"
        }
    }
    else
        y.tick.marks <- ""

    # If specified no tick marks, then make sure they have no tick length, as this overrides showticks.
    if (y.tick.marks == "none" | y.tick.mark.length > 0)
    {
        y.showticks <- FALSE
        y.tick.mark.length <- 0
    }

    x.showline <- FALSE
    x.showticks <- FALSE

    ## If no line, then set to NULL to satisfy Plotly
    if (x.line.width == 0)
        x.line.width <- NULL

    if (x.line.width >= 1 && !is.null(x.line.width))
    {
        x.showline <- TRUE
        # Default to outer tick marks if they are to be shown, but have not been specified
        if (x.tick.marks == "")
        {
            x.showticks <- TRUE
            x.tick.marks <- "outside"
        }
    }
    else if (x.tick.marks != "")
        x.showticks <- TRUE
    else
        x.tick.marks <- ""

    # If specified no tick marks, then make sure they have no tick length, as this overrides showticks.
    if (x.tick.marks == "none" | x.tick.mark.length == 0)
    {
        x.showticks <- FALSE
        x.tick.mark.length <- 0
    }

    ## Set tick and hover formats
    if (is.null(x.hovertext.decimals))
        x.hovertext.decimals <- data.label.decimals
    if (is.null(y.hovertext.decimals))
        y.hovertext.decimals <- data.label.decimals
    if (y.tick.format.manual != "" && y.tick.format.manual != y.tickformat)
        y.tickformat <- y.tick.format.manual

    if (xor(is.null(x.bounds.minimum), is.null(x.bounds.maximum)))
        stop("Both x.bounds.minimum and x.bounds.maximum need to be supplied in order to specify a display range.")
    if (xor(is.null(y.bounds.minimum), is.null(y.bounds.maximum)))
        stop("Both y.bounds.minimum and y.bounds.maximum need to be supplied in order to specify a display range.")

    x.has.bounds <- !is.null(x.bounds.minimum) && !is.null(x.bounds.maximum)
    y.has.bounds <- !is.null(y.bounds.minimum) && !is.null(y.bounds.maximum)

    # Area chart does not display the data labels on the edge correctly, so we add padding.
    # Line chart does add padding automatically, but the amount of padding seems to change
    # between regression tests, so we add padding manually.
    if (!x.has.bounds && (is.area.or.line.chart || (type == "Bar" && data.label.show)))
    {
        not.na <- which(apply(chart.matrix, 1, function(x){any(!is.na(x))}))
        if (type == "Bar")
        {
            min.x <- min(0, min(chart.matrix))
            max.x <- max(chart.matrix)
        }
        else if (is.x.axis.numeric)
        {
            x.vals <- as.numeric(row.names(chart.matrix)[not.na])
            min.x <- min(x.vals)
            max.x <- max(x.vals)
        }
        else if (x.axis.type == "date")
        {
            x.vals <- as.numeric(x.labels[not.na]) * 1000
            min.x <- min(x.vals)
            max.x <- max(x.vals)
        }
        else
        {
            min.x <- 0
            max.x <- max(not.na) - 1
        }
        padding <- 0
        lab.len <- 1
        if (data.label.show)
            lab.len <- (nchar(data.label.prefix) + nchar(data.label.suffix) + data.label.decimals) *
                        data.label.font.size/10
        if (data.label.show || (!is.null(series.marker.show) && series.marker.show != "none"))
            padding <- (max.x - min.x) * (0.05 * lab.len/2 + (0.1 * (type == "Bar")))

        x.bounds.minimum <- min.x - (padding * (type != "Bar"))
        x.bounds.maximum <- max.x + padding
        x.has.bounds <- TRUE
        added.bounds.for.area.chart <- TRUE
    }
    else
        added.bounds.for.area.chart <- FALSE

    # Determine decimal places to show if not provided
    if (!is.scatterplot.or.bubbleplot && x.axis.type == "linear" && is.null(x.tick.decimals))
    {
            x.tick.decimals <- if (x.has.bounds)
                decimalsToDisplay(c(x.bounds.minimum, x.bounds.maximum))
            else if (is.stacked && !is.hundred.percent.stacked)
                decimalsToDisplay(rowSums(chart.matrix, na.rm = TRUE))
            else
                decimalsToDisplay(chart.matrix)
    }
    if (!is.scatterplot.or.bubbleplot && y.axis.type == "linear" && is.null(y.tick.decimals))
    {
        y.tick.decimals <- if (y.has.bounds)
            decimalsToDisplay(c(y.bounds.minimum, y.bounds.maximum))
        else if (is.stacked && !is.hundred.percent.stacked)
            decimalsToDisplay(rowSums(chart.matrix, na.rm = TRUE))
        else
            decimalsToDisplay(chart.matrix)
    }

    # Tick formats
    if ((x.tick.format.manual == "" && (is.null(x.tickformat) || x.tickformat == "") && x.axis.type != "date"))
        x.tickformat <- paste(".", x.tick.decimals, "f", sep="")
    if ((y.tick.format.manual == "" && (is.null(y.tickformat) || y.tickformat == "") && y.axis.type != "date"))
        y.tickformat <- paste(".", y.tick.decimals, "f", sep="")

    # Hover formats
    x.hoverformat <- if (x.hovertext.format.manual != "")
        x.hoverformat <- x.hovertext.format.manual
    else if (x.axis.type == "date")
        ""
    else
        paste(".", x.hovertext.decimals, "f", sep = "")
    y.hoverformat <- if (y.hovertext.format.manual != "")
        y.hoverformat <- y.hovertext.format.manual
    else if (y.axis.type == "date")
        ""
    else
        paste(".", y.hovertext.decimals, "f", sep = "")

    x.autorange <- if (x.data.reversed)
    {
        "reversed"
    }
    else if (x.has.bounds || !is.null(x.tick.distance))
    {
        if (!is.x.axis.numeric && !added.bounds.for.area.chart)
            stop("It is not possible to specify tick range or spacing as the x-axis is not numeric.")
        #if (x.data.reversed)
        #    stop("It is not possible to reverse the x-axis whilst specifying tick range or spacing.")
        FALSE
    }
    else
        TRUE

    x.tickmode <- "auto"
    x.range <- NULL
    x.tickvals <- NULL
    x.ticktext <- NULL
    if (x.has.bounds)
    {
        x.range <- c(x.bounds.minimum, x.bounds.maximum)
        if (!is.null(x.tick.distance))
        {
            x.tickmode <- "array"
            x.tickvals <- seq(x.bounds.minimum, x.bounds.maximum, by = x.tick.distance)
        }
    }

    y.autorange <- if (xor(y.data.reversed, swap.axes.and.data))
    {
        "reversed"
    }
    else if (y.has.bounds || !is.null(y.tick.distance))
    {
        if (y.data.reversed)
            stop("It is not possible to reverse the y-axis whilst specifying tick range or spacing.")
        if (swap.axes.and.data)
            stop("It is not possible to specify the tick range or spacing for this chart type.")
        FALSE
    }
    else
        TRUE

    y.tickmode <- "auto"
    y.range <- NULL
    y.tickvals <- NULL
    y.ticktext <- NULL
    if (y.has.bounds)
    {
        y.range <- c(y.bounds.minimum, y.bounds.maximum)
        if (!is.null(y.tick.distance))
        {
            y.tickmode <- "array"
            y.tickvals <- seq(y.bounds.minimum, y.bounds.maximum, by = y.tick.distance)
        }
    }

    ## Should we draw a zero line
    y.zero.line <- FALSE
    if (y.zero.line.width > 0)
        y.zero.line <- TRUE

    x.zero.line <- FALSE
    if (x.zero.line.width > 0)
        x.zero.line <- TRUE

    ## Show plot grid?
    y.grid.show <- FALSE
    if (y.grid.width > 0)
        y.grid.show <- TRUE

    x.grid.show <- FALSE
    if (x.grid.width > 0)
        x.grid.show <- TRUE

    ## Which markers to show?
    series.marker.symbols <- if (is.null(series.marker.show) ||
                                 series.marker.show == "automatic" ||
                                 series.marker.show == "none")
        rep(100, 100) # disc
    else
        series.marker.show

    ## Hide legend if only one series to plot
    if (ncol(chart.matrix) == 1)
        legend.show <- FALSE

    if (is.bar.chart && !legend.show && is.default.margin.right && y.position != "right")
        margin.right <- 20
    if (y.position == "right" && is.default.margin.left)
        margin.left <- 20
    if (x.position == "top")
    {
        if (is.default.margin.top)
            margin.top <- margin.bottom
        if (is.default.margin.bottom)
            margin.bottom <- 20
    }

    hover.mode <- if (tooltip.show) "closest" else FALSE

    ## Convert type to plotly type
    plotly.type <- if (is.bar.or.column.chart) "bar" else "scatter"

    ## Show source data points in hover text, or along series markers
    if (data.label.show)
        series.mode <- if (series.mode == "none") "text" else paste0(series.mode, "+text")

    textfont <- if (!is.null(series.mode) && regexpr('text', series.mode) >= 1)
        list(family = data.label.font.family,
             color = toRGB(data.label.font.color, alpha = 1),
             size = data.label.font.size)
    else
        NULL
    xaxis2 <- NULL
    yaxis2 <- NULL

    if (is.scatterplot)
    {
        legend.show <- scatterplot.data$legend.show
        if (x.autorange != "reversed")
            x.autorange <- FALSE

        # Fix x-axis to prevent changing chart width
        # Scatterplot data can be assumed to be always numeric
        x.range <- range(scatterplot.data$x)
        tmp.width <- diff(x.range)
        lab.len <- 1
        if (data.label.show)
            lab.len <- (nchar(data.label.prefix) + nchar(data.label.suffix) + data.label.decimals) *
                        data.label.font.size/10
        padding <- diff(x.range) * (0.05 * lab.len)
        x.range <- x.range + c(-padding, padding)

        x.prefix <- if (x.tick.prefix == "") data.label.prefix else x.tick.prefix
        x.suffix <- if (x.tick.suffix == "") data.label.suffix else x.tick.suffix
        y.prefix <- if (y.tick.prefix == "") data.label.prefix else y.tick.prefix
        y.suffix <- if (y.tick.suffix == "") data.label.suffix else y.tick.suffix

        if (is.null(y.tick.decimals))
            y.tick.decimals <- decimalsToDisplay(scatterplot.data$y)
        y.tickformat <- paste(".", y.tick.decimals, "f", sep="")
        if (is.null(x.tick.decimals))
            x.tick.decimals <- decimalsToDisplay(scatterplot.data$x)
        x.tickformat <- paste(".", x.tick.decimals, "f", sep="")

        source.text <- paste0(scatterplot.data$label, " (",
            x.prefix, FormatAsReal(scatterplot.data$x, decimals = data.label.decimals), x.suffix, ",",
            y.prefix, FormatAsReal(scatterplot.data$y, decimals = data.label.decimals), y.suffix, ")")

        if (fit.type != "None")
        {
            fit.line.colors <- if (is.null(fit.line.colors)) scatterplot.data$colors
                               else ChartColors(number.colors.needed = length(scatterplot.data$colors),
                                                given.colors = fit.line.colors,
                                                custom.color = fit.line.colors.custom.color,
                                                custom.gradient.start = fit.line.colors.custom.gradient.start,
                                                custom.gradient.end = fit.line.colors.custom.gradient.end,
                                                custom.palette = fit.line.colors.custom.palette,
                                                reverse = fit.line.colors.reverse,
                                                trim.light.colors = TRUE)
        }

        # Iteratively add each group so the order is the same as the dataframe
        g.list <- unique(scatterplot.data$group)
        if (fit.type == "None")
            num.fit <- 0
        else if (length(g.list) == length(scatterplot.data$group))
            num.fit <- 1
        else
            num.fit <- length(g.list)

        p <- plot_ly(as.data.frame(x=scatterplot.data$x, y=scatterplot.data$y))
        for (ggi in 1:length(g.list))
        {
            ind <- which(scatterplot.data$group == g.list[ggi])
            line.obj <- if (is.null(series.line.width) || series.line.width == 0) NULL
                        else list(width=series.line.width, color=scatterplot.data$colors[ggi])
            sizes <- if (!is.null(scatterplot.data$z)) scatterplot.data$z[ind]
                     else series.marker.size
            p <- add_trace(p, x=scatterplot.data$x[ind], y=scatterplot.data$y[ind],
                    name=g.list[ggi], showlegend=(length(g.list) > 1),
                    text=source.text[ind], textfont=textfont, textposition=data.label.position,
                    marker=list(size=sizes, sizemode="diameter", color=scatterplot.data$colors[ggi],
                    line=list(width=series.marker.border.width)), line=line.obj,
                    type=plotly.type, mode=series.mode, symbols=series.marker.symbols,
                    hoverinfo=if(length(g.list) > 1) "name+text" else "text")

            if (num.fit > 1)
            {
                indF <- ind[order(scatterplot.data$x[ind])]
                if (fit.ignore.last)
                    indF <- indF[-which.max(scatterplot.data$x[indF])]

                # Does not handle dates - because scatterplot does not handle dates
                tmp.dat <- data.frame(x=scatterplot.data$x[indF], y=scatterplot.data$y[indF])
                tmp.fit <- if (fit.type == "Smooth") loess(y~x, data=tmp.dat)
                           else                      lm(y~x, data=tmp.dat)

                x.fit <- seq(from=min(tmp.dat$x), to=max(tmp.dat), length=100)
                y.fit <- predict(tmp.fit, data.frame(x=x.fit))
                tmp.fname <- if (length(g.list) == 1)  fit.line.name
                             else sprintf("%s: %s", fit.line.name, g.list[ggi])

                p <- add_trace(p, x=x.fit, y=y.fit, type='scatter', mode="lines",
                          name=tmp.fname, legendgroup=ggi, showlegend=F,
                          line=list(dash=fit.line.type, width=fit.line.width, color=fit.line.colors[ggi]))
            }
        }
        if (num.fit == 1)
        {
            indF <- 1:length(scatterplot.data$x)
            if (fit.ignore.last)
                indF <- indF[-which.max(scatterplot.data$x[indF])]

            tmp.dat <- data.frame(x=scatterplot.data$x[indF], y=scatterplot.data$y[indF])
            tmp.fit <- if (fit.type == "Smooth") loess(y~x, data=tmp.dat)
                       else                      lm(y~x, data=tmp.dat)

            x.fit <- seq(from=min(tmp.dat$x), to=max(tmp.dat), length=100)
            y.fit <- predict(tmp.fit, data.frame(x=x.fit))
            p <- add_trace(p, x=x.fit, y=y.fit, type='scatter', mode="lines",
                      name=fit.line.name, showlegend=F,
                      line=list(dash=fit.line.type, width=fit.line.width, color=fit.line.colors[1]))
        }
    }
    else
    {
        ## Initiate plotly object
        p <- plot_ly(as.data.frame(chart.matrix))

        ## Add a trace for each col of data in the matrix
        for (i in 1:ncol(chart.matrix))
        {
            y <- as.numeric(chart.matrix[, i])
            x <- x.labels

            if (swap.axes.and.data == TRUE)
            {
                y.swap <- y
                x.swap <- x
                y <- x.swap
                x <- y.swap
            }

            # Used by line, area and scatter charts
            source.text <- if (is.area.or.line.chart && data.label.show)
                paste(data.label.prefix,
                      FormatAsReal(chart.matrix[, i] * data.label.mult, decimals = data.label.decimals),
                      data.label.suffix, sep = "")
            else
                ""

            ## Add trace components
            if (!is.null(series.mode) && regexpr('lines', series.mode) >= 1)
            {
                lines <- list(width = series.line.width,
                             color = toRGB(series.line.colors[i], alpha = series.line.opacity))
            } else
                lines <- NULL

            if (!is.null(series.mode) && regexpr('marker', series.mode) >= 1)
            {
                marker <- list(size = series.marker.size,
                              color = toRGB(series.marker.colors[i], alpha = series.marker.opacity),
                              symbol = series.marker.symbols[i],
                              line = list(
                                  color = toRGB(series.marker.border.colors[i], alpha = series.marker.border.opacity),
                                  width = series.marker.border.width))
            } else if (plotly.type == "bar") {
                marker <- list(size = series.marker.size,
                              color = toRGB(colors[i], alpha = opacity),
                              line = list(
                                  color = toRGB(series.marker.border.colors[i], alpha = series.marker.border.opacity),
                                  width = series.marker.border.width))
            } else
                 marker <- NULL

            if (plotly.type == "bar")
            {
                tmp.group <- if (legend.group == "") paste("group", i) else legend.group
                p <- add_trace(p,
                               type = plotly.type,
                               x = x,
                               y = y,
                               orientation = orientation,
                               line = lines,
                               name = y.labels[i],
                               legendgroup = tmp.group,
                               hoverinfo = if(ncol(chart.matrix) > 1) "x+y+name" else "x+y",
                               marker = marker)

                if (type == "Column" && fit.type != "None" && !is.stacked)
                {
                    tmp.is.factor <- x.axis.type != "linear" && x.axis.type != "date"
                    x0 <- if (!tmp.is.factor) x else 1:length(x)
                    tmp.dat <- data.frame(x=x0, y=y)
                    if (fit.ignore.last)
                        tmp.dat <- tmp.dat[-which.max(tmp.dat$x),]

                    tmp.fit <- if (fit.type == "Smooth") loess(y~I(as.numeric(x)), data=tmp.dat)
                               else                      lm(y~x, data=tmp.dat)

                    x.fit <- if (tmp.is.factor) x0
                             else seq(from=min(x), to=max(x), length=100)
                    y.fit <- predict(tmp.fit, data.frame(x=x.fit))
                    tmp.fname <- if (ncol(chart.matrix) == 1)  fit.line.name
                                 else sprintf("%s: %s", fit.line.name, y.labels[i])

                    if (tmp.is.factor)
                        x.fit <- x
                    p <- add_trace(p, x=x.fit, y=y.fit, type='scatter', mode="lines",
                              name=tmp.fname, legendgroup=tmp.group, showlegend=F,
                              line=list(dash=fit.line.type, width=fit.line.width,
                              color=fit.line.colors[i]))
                }

                if (type == "Column" && data.label.show && !is.stacked)
                {
                    if (is.null(x.range))
                    {
                        if (is.numeric(x) || !is.null(ymd))
                            x.range <- range(data.annotations$x)
                        else
                            x.range <- c(0, length(x))

                        if (!is.null(ymd))
                        {
                            #tmpd <- diff(data.annotations$x[,i])[1] * 6/24
                            tmpd <- diff(sort(data.annotations$x))[1] * 0.5
                            x.range <- x.range + c(-tmpd, tmpd)
                        }
                        if (x.autorange == "reversed")
                            x.range <- rev(x.range)
                    }
                    xaxis2 <- list(overlaying = "x",
                                   visible = FALSE,
                                   range = x.range)

                    p <- add_text(p,
                              xaxis = "x2",
                              x = data.annotations$x[,i] + 0.5,
                              y = data.annotations$y[,i],
                              text = data.annotations$text[,i],
                              textposition = "top center",
                              textfont = textfont,
                              legendgroup = tmp.group,
                              hoverinfo = "none",
                              showlegend = FALSE)
                }
                if (type == "Bar" && fit.type != "None" && !is.stacked)
                {
                    tmp.is.factor <- y.axis.type != "linear" && y.axis.type != "date"
                    y0 <- if (!tmp.is.factor) y else 1:length(y)
                    tmp.dat <- data.frame(x=x, y=y0)
                    if (fit.ignore.last)
                        tmp.dat <- tmp.dat[-which.max(tmp.dat$y),]

                    tmp.fit <- if (fit.type == "Smooth") loess(x~I(as.numeric(y)), data=tmp.dat)
                               else                      lm(x~y, data=tmp.dat)

                    y.fit <- if (tmp.is.factor) y0
                             else seq(from=min(y), to=max(y), length=100)
                    x.fit <- predict(tmp.fit, data.frame(y=y.fit))
                    tmp.fname <- if (ncol(chart.matrix) == 1)  fit.line.name
                                 else sprintf("%s: %s", fit.line.name, y.labels[i])

                    if (tmp.is.factor)
                        y.fit <- y
                    p <- add_trace(p, x=x.fit, y=y.fit, type='scatter', mode="lines",
                              name=tmp.fname, legendgroup=tmp.group, showlegend=F,
                              line=list(dash=fit.line.type, width=fit.line.width,
                              color=fit.line.colors[i]))


                }
                if (type == "Bar" && data.label.show)
                {
                    if (is.null(y.range))
                    {
                        if (is.numeric(y) || !is.null(ymd))
                            y.range <- range(data.annotations$y)
                        else
                            y.range <- c(0, length(y)) - 0.5

                        if (!is.null(ymd))
                        {
                            tmpd <- diff(sort(data.annotations$y))[1] * 0.5
                            y.range <- y.range + c(-tmpd, tmpd)
                            y.range <- rev(y.range)
                            # plotly does not reverse dates on the y-axis
                            if (y.autorange == "reversed")
                                y.range <- rev(y.range)
                        }
                        if (y.autorange == "reversed")
                            y.range <- rev(y.range)
                    }
                    yaxis2 <- list(overlaying = "y",
                                   visible = FALSE,
                                   range = y.range)
                    x.diff <- diff(range(data.annotations$x))/100
                    p <- add_text(p,
                              yaxis = "y2",
                              x = data.annotations$x[,i] + x.diff,
                              y = data.annotations$y[,i],
                              text = data.annotations$text[,i],
                              textposition = "middle right",
                              textfont = textfont,
                              legendgroup = tmp.group,
                              hoverinfo = "none",
                              showlegend = FALSE)
                }

            }
            else if (is.area.or.line.chart && !is.stacked)
            {
                y.label <- y.labels[i]
                tmp.group <- if (legend.group == "") paste("group", i) else legend.group

                p <- add_trace(p,
                               type = plotly.type,
                               x = x,
                               y = y,
                               fill = fill.bound,
                               fillcolor = toRGB(colors[i], alpha = opacity),
                               connectgaps = FALSE,
                               line = lines,
                               name = y.label,
                               legendgroup = tmp.group,
                               hoverinfo = if(ncol(chart.matrix) > 1) "x+y+name" else "x+y",
                               marker = marker,
                               mode = series.mode)

                # single points (no lines) need to be added separately
                # turn off if series present?
                not.na <- is.finite(y)
                is.single <- not.na & c(TRUE, !not.na[-nrow(chart.matrix)]) & c(!not.na[-1], TRUE)
                if (any(is.single))
                {
                    p <- add_trace(p,
                               type = "scatter",
                               mode = "markers",
                               x = x[is.single],
                               y = y[is.single],
                               legendgroup = tmp.group,
                               name = y.label,
                               marker = if (!is.null(marker)) marker
                                        else list(color = toRGB(colors[i], alpha=opacity),
                                             size = series.line.width),
                               hoverinfo = if(ncol(chart.matrix) > 1) "x+y+name" else "x+y",
                               showlegend = FALSE)
                }

                if (data.label.show)
                    p <- add_trace(p,
                               type = "scatter",
                               mode = "text",
                               x = x,
                               y = y,
                               legendgroup = tmp.group,
                               name = y.label,
                               text = source.text,
                               textfont = textfont,
                               textposition = data.label.position,
                               showlegend = FALSE)

                if (fit.type != "None")
                {
                    tmp.is.factor <- x.axis.type != "linear" && x.axis.type != "date"
                    x0 <- if (!tmp.is.factor) x else 1:length(x) # what happens with dates?
                    tmp.dat <- data.frame(x=x0, y=y)
                    if (fit.ignore.last)
                        tmp.dat <- tmp.dat[-which.max(tmp.dat$x),]

                    tmp.fit <- if (fit.type == "Smooth") loess(y~I(as.numeric(x)), data=tmp.dat)
                               else                      lm(y~x, data=tmp.dat)

                    x.fit <- if (tmp.is.factor) x0
                             else seq(from=min(x), to=max(x), length=100)
                    y.fit <- predict(tmp.fit, data.frame(x=x.fit))
                    tmp.fname <- if (ncol(chart.matrix) == 1)  fit.line.name
                                 else sprintf("%s: %s", fit.line.name, y.labels[i])

                    if (tmp.is.factor)
                        x.fit <- x
                    p <- add_trace(p, x=x.fit, y=y.fit, type='scatter', mode="lines",
                              name=tmp.fname, legendgroup=tmp.group, showlegend=F,
                              line=list(dash=fit.line.type, width=fit.line.width,
                              color=fit.line.colors[i]))
                }

            }
            else
            {
                y.label <- y.labels[i]
                p <- add_trace(p,
                               type = plotly.type,
                               x = x,
                               y = y,
                               fill = fill.bound,
                               fillcolor = toRGB(colors[i], alpha = opacity),
                               line = lines,
                               name = y.label,
                               legendgroup = legend.group,
                               text = source.text,
                               textfont = textfont,
                               textposition = data.label.position,
                               hoverinfo = if (ncol(chart.matrix) > 1) "x+y+name" else "x+y",
                               # MARKERS
                               mode = series.mode,
                               marker = marker
                )
            }
        }
    }

    ## Config options
    p <- config(p, displayModeBar = modebar.show)

    ## Set htmlwidget padding to zero (defaults to 40px)
    p$sizingPolicy$browser$padding <- 0

    y.range.mode <- if (y.zero) "tozero" else "normal"

    ## Set plotly layout styles
    p <- layout(p,
        title = title,
        ## LEGEND
        showlegend = legend.show,
        legend = list(
            bgcolor = toRGB(legend.fill.color, alpha=legend.fill.opacity),
            bordercolor = legend.border.color,
            borderwidth = legend.border.line.width,
            font = list(
                color = legend.font.color,
                family = legend.font.family,
                size = legend.font.size
            ),
            xanchor = legend.x.anchor,
            yanchor = legend.y.anchor,
            y = legend.y,
            x = legend.x,
            traceorder = legend.sort.order
        ),
        ## Y-AXIS
        yaxis = list(
            title = y.title,
            type = y.axis.type,
            titlefont = list(
                color = y.title.font.color,
                family = y.title.font.family,
                size = y.title.font.size
            ),
            tickfont = list(
                color = y.tick.font.color,
                family = y.tick.font.family,
                size = y.tick.font.size
            ),
            showline = y.showline,
            linecolor = y.line.color,
            linewidth = y.line.width,
            tickmode = y.tickmode,
            tickvals = y.tickvals,
            ticktext = y.ticktext,
            range = y.range,
            rangemode = y.range.mode,
            ticks = y.tick.marks,
            tickangle = y.tick.angle,
            ticklen = y.tick.mark.length,
            tickcolor = y.line.color,
            dtick = y.tick.distance,
            zeroline = y.zero.line,
            zerolinewidth = y.zero.line.width,
            zerolinecolor = y.zero.line.color,
            tickformat = y.tickformat,
            tickprefix = y.tick.prefix,
            ticksuffix = y.tick.suffix,
            autorange = y.autorange,
            side = y.position,
            gridwidth = y.grid.width,
            gridcolor = y.grid.color,
            showgrid = y.grid.show,
            hoverformat = y.hoverformat,
            showexponent = "all",
            showtickprefix = TRUE,
            showticksuffix = TRUE,
            showticklabels = y.tick.show
        ),
        ## X-AXIS
        xaxis4 = footer.axis,
        xaxis3 = subtitle.axis,
        xaxis2 = xaxis2,
        yaxis2 = yaxis2,
        xaxis = list(
            title = x.title,
            type = x.axis.type,
            titlefont = list(
                color = x.title.font.color,
                family = x.title.font.family,
                size = x.title.font.size
            ),
            tickfont = list(
                color = x.tick.font.color,
                family = x.tick.font.family,
                size = x.tick.font.size
            ),
            showline = x.showline,
            linecolor = x.line.color,
            linewidth = x.line.width,
            tickmode = x.tickmode,
            tickvals = x.tickvals,
            ticktext = x.ticktext,
            range = x.range,
            ticks = x.tick.marks,
            tickangle = x.tick.angle,
            ticklen = x.tick.mark.length,
            tickcolor = x.line.color,
            dtick = x.tick.distance,
            zeroline = x.zero.line,
            zerolinewidth = x.zero.line.width,
            zerolinecolor = x.zero.line.color,
            tickformat = x.tickformat,
            tickprefix = x.tick.prefix,
            ticksuffix = x.tick.suffix,
            autorange = x.autorange,
            side = x.position,
            gridwidth = x.grid.width,
            gridcolor = x.grid.color,
            showgrid = x.grid.show,
            hoverformat = x.hoverformat,
            showexponent = "all",
            showtickprefix = TRUE,
            showticksuffix = TRUE,
            showticklabels = x.tick.show
        ),
        ## MARGINS
        margin = list(
            t = margin.top,
            b = margin.bottom,
            l = margin.left,
            r = margin.right,
            pad = margin.inner.pad
        ),
        plot_bgcolor = toRGB(charting.area.fill.color, alpha = charting.area.fill.opacity),
        paper_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity),
        hovermode = hover.mode,
        titlefont = list(
            family = title.font.family,
            color = title.font.color,
            size = title.font.size
        ),
        font = list(
            family = data.label.font.family,
            color = data.label.font.color,
            size = data.label.font.size
        ),
        annotations = if (plotly.type == "bar" && is.stacked) data.annotations else NULL,
        bargap = bar.gap,
        barmode = barmode
    )

    result <- list(plotly.plot = p)
    class(result) <- "StandardChart"
    result
}

#' @export
print.StandardChart <- function(x, ...)
{
    return(x$plotly.plot)
}
