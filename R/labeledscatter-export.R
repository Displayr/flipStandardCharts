#' Labeled Scatterplot chart
#'
#' Plot scatterplot with labels - best used for small sets of data
#'
#' @param x A table matrix or data frame.
#' @param y Optional numeric vector for the y-axis positions. Should contain the same number of observations as x. If not provided, will use x instead.
#' @param colors Optional vector determining the color of each observation.
#' @param groups Alternative to using colors. Defaults to treating colors as categorical.
#' @param colors.as.categorical Whether to treat colors as a categorical groups, or a numeric scale.
#' @param color.palette A vector of colors to use in the chart. Should be the same length as the number of groups.
#' @param sizes Optional vector determining of the size of each observation.
#' @param labels Optional vector for labelling scatter points. This is used in the hovertext and data labels.
#' @param labels.name Character; Used for labelling subtitles and footers.
#' @param colors.name Character; Used for labelling subtitles and footers.
#' @param sizes.name Character; Used for labelling subtitles and footers.
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
#' @param data.label.font.family Character; font family for data label.
#' @param data.label.font.size Font size for data label.
#' @param data.label.font.color Font color as a named color
#' in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param data.label.max.plot Integer; the maximum number of labels to show on a Labeled Scatterplot.
#' @param data.label.decimals Number of decimal places to show in
#' data labels.
#' @param data.label.prefix Character; prefix for data values.
#' @param data.label.suffix Character; suffix for data values.
#' @param data.label.position Character; where to place the source data
#' value in relation to the marker icon.  Can be "top left", "top center", "top
#' right", "middle left", "middle center", "middle right", "bottom left",
#' "bottom center", "bottom right". Only applicable for line and area charts.
#' @param us.date.format Whether to apply the US convention when parsing dates.
#' @param ... Extra arguments that are ignored.
#' @importFrom grDevices rgb
#' @importFrom flipChartBasics ChartColors
#' @importFrom rhtmlLabeledScatter LabeledScatter
#' @export
LabeledScatterChart <- function(x, 
                         y = NULL, 
                         colors = NULL, 
                         groups = colors, 
                         sizes = NULL, 
                         labels = 1:length(x),
                         colors.as.categorical = !is.null(groups),
                         color.palette = flipChartBasics:::qColors, 
                         labels.name = NA, colors.name = NA, sizes.name = NA,
                    fit.type = "None",
                    fit.ignore.last = FALSE,
                    fit.line.type = "dot",
                    fit.line.width = 1,
                    fit.line.name = "Fitted",
                    fit.line.colors = color.palette,
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
                    data.label.max.plot = 50,
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
                    label.wrap = TRUE,
                    label.wrap.nchar = 21,
                    series.line.width = 0,
                    series.marker.border.width = 1,
                    series.marker.size = 6,
                    series.marker.opacity = 1,
                    series.marker.show = "none", # ignored
                    series.marker.colors = NULL,
                    us.date.format = FALSE)
{

    g.list <- unique(groups)
    colors <- color.palette
    # need to create colorRamp if colors.as.categorical

    lab.tidy <- labels
    if (length(labels) > data.label.max.plot)
        lab.tidy <- labels[(data.label.max.plot+1):(length(labels))] <- ""

    # need to pull out variables from x
    return(LabeledScatter(X = as.numeric(x), # can't handle dates?
                       Y = as.numeric(y),
                       #Z = as.numeric(sizes),
                       group = groups,
                       colors = colors,
                       label = lab.tidy, # should also handle logos
                       label.alt = labels,
                       fixed.aspect = FALSE,
                       grid = x.grid.width != 0 && y.grid.width != 0,
                       origin = FALSE,
                       origin.align = FALSE,
                       labels.show = TRUE,
                       legend.show = legend.show && length(g.list) > 1,
                       legend.bubbles.show = !is.null(sizes),
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
                       axis.font.family = y.tick.font.family,
                       axis.font.color = y.tick.font.color,
                       axis.font.size = y.tick.font.size,
                       x.title = x.title,
                       x.title.font.family = x.title.font.family,
                       x.title.font.color = x.title.font.color,
                       x.title.font.size = x.title.font.size,
                       z.title = sizes.name,
                       x.decimals = if (is.null(x.tick.decimals)) decimalsToDisplay(x) else x.tick.decimals,
                       y.decimals = if (is.null(y.tick.decimals)) decimalsToDisplay(y) else y.tick.decimals,
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
                       #point.radius = point.radius,
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
                       #trend.lines.show = trend.lines,
                       #labels.logo.scale = 0.4, # logo.size
                       debug.mode = grepl("DEBUG_MODE_ON", title)))
}
                     

