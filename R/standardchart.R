#' Generates an interactive chart based on the plotly library.
#'
#' \code{Chart} generates standard charts from plotly library.
#'
#' @param y A table, matrix, vector or data frame.
#' @param type Character; type of chart. Can be "Area", "Stacked Area",
#'  "100\% Stacked Area", "Bar", "Stacked Bar", "100% Stacked Bar",
#'  "Column", "Stacked Column", "100% Stacked Column", "Line", "Donut",
#'  "Pie", "Labeled Scatterplot", "Labeled Bubbleplot", "Radar".
#' @param transpose Logical; should the final output be transposed?
#' @param title Character; chart title.
#' @param title.font.family Character; title font family. Can be "Arial Black",
#' "Arial", "Comic Sans MS", "Courier New", "Georgia", "Impact",
#' "Lucida Console", "Lucida Sans Unicode", "Marlett", "Symbol", "Tahoma",
#' "Times New Roman", "Trebuchet MS", "Verdana", "Webdings"
#' @param title.font.color Title font color as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param title.font.size Title font size; default = 10.
#' @param colors Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' @param colors.reverse Logical; if the order of the colors should be reversed.
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
#' @param legend.ascending Logical; TRUE for ascending, FALSE for descending
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
#' @param series.marker.colors Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' @param series.marker.colors.reverse Logical; if the order of the colors should
#' be reversed.
#' @param series.marker.opacity Opacity for series markers as an alpha value (0 to 1).
#' @param series.marker.size Size in pixels of marker
#' @param series.marker.border.width Width in pixels of border/line
#' around series markers; 0 is no line
#' @param series.marker.border.colors Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' @param series.marker.border.colors.reverse Logical; if the order of the colors
#' should be reversed.
#' @param series.marker.border.opacity Opacity of border/line around
#' series markers as an alpha value (0 to 1).
#' @param series.line.width Thickness, in pixels, of the series line
#' @param series.line.colors  Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' @param series.line.colors.reverse Logical; if the order of the colors
#' should be reversed.
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
#' @param pie.order Character; "descending", "initial", or
#' "alphabetical".
#' @param pie.groups.order Character; "descending", "initial", or
#' "alphabetical".
#' @param pie.subslice.colors Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' @param pie.subslice.colors.reverse Logical; if the order of
#' pie.subslice.colors should be reversed.
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
#' @param scatter.group.indices Vector or text of comma-separated group indices
#' corresponding to each row.
#' @param scatter.group.labels Vector or text of comma-separated group labels.
#' @param us.date.format Whether to apply the US convention when parsing dates.
#' @examples
#' z <- c(5, 6, 2, 1.5, 9, 2.2)
#' Chart(y = z, type = "Area")
#' @importFrom grDevices rgb
#' @importFrom flipFormat FormatWithDecimals
#' @importFrom flipTime PeriodNameToDate
#' @importFrom flipChartBasics ChartColors
#' @importFrom plotly plot_ly config toRGB add_trace add_text layout hide_colorbar
#' @export
Chart <-   function(y,
                    type = "Column",
                    transpose = FALSE,
                    title = "",
                    title.font.family = NULL,
                    title.font.color = NULL,
                    title.font.size = 16,
                    colors = NULL,
                    colors.reverse = FALSE,
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
                    legend.ascending = TRUE,
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
                    series.marker.show = "none", # ignored
                    series.marker.colors = NULL,
                    series.marker.colors.reverse = FALSE,
                    series.marker.opacity = 1,
                    series.marker.size = 6,
                    series.marker.border.width = 1,
                    series.marker.border.colors = NULL,
                    series.marker.border.colors.reverse = FALSE,
                    series.marker.border.opacity = 1,
                    series.line.width = NULL,
                    series.line.colors = NULL,
                    series.line.colors.reverse = FALSE,
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
                    pie.order = "initial",
                    pie.groups.order = "initial",
                    pie.subslice.colors = NULL,
                    pie.subslice.colors.reverse = FALSE,
                    pie.subslice.colors.repeat = TRUE,
                    pie.border.color = rgb(255, 255, 255, maxColorValue = 255),
                    pie.inner.radius = 70,
                    pie.show.percentages = FALSE,
                    z.title = "",
                    scatter.group.indices = "",
                    scatter.group.labels = "",
                    us.date.format = NULL)
{
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
        warning("The background and charting area fill colors cannot be changed for
                 pie charts, donut charts, labeled scatterplots or labeled bubbleplots.")

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

    chart.matrix <- y

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
    if (!is.numeric(chart.matrix))
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
    qinput <- FALSE
    if (!is.null(attr(chart.matrix, "statistic")))
        qinput <- TRUE

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

        # Replace $ with html entity number otherwise plotly will treat it as LaTeX
        rownames(chart.matrix) <- gsub("\\$", "&#36;", rownames(chart.matrix))
        colnames(chart.matrix) <- gsub("\\$", "&#36;", colnames(chart.matrix))

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
    if (is.null(y.title.font.family) || y.title.font.family == "") y.title.font.family <- global.font.family
    if (is.null(y.tick.font.family) || y.tick.font.family == "") y.tick.font.family <- global.font.family
    if (is.null(x.title.font.family) || x.title.font.family == "") x.title.font.family <- global.font.family
    if (is.null(x.tick.font.family) || x.tick.font.family == "") x.tick.font.family <- global.font.family
    if (is.null(data.label.font.family) || data.label.font.family == "") data.label.font.family <- global.font.family

    # Use global colours if necessary
    if (is.null(title.font.color)) title.font.color <- global.font.color
    if (is.null(legend.font.color)) legend.font.color <- global.font.color
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
        margin.top <- if (is.null(title) || title == "") 20 else 40
    if (is.null(margin.bottom))
    {
        margin.bottom <- 70
        if (x.title == "")
            margin.bottom <- 50
        if (type == "Radar")
            margin.bottom <- 20
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

    ## Settings specific to Scatterplots
    if (type == "Scatterplot")
    {
        scatterplot.data <- scatterplotData(chart.matrix, FALSE, scatter.group.labels, scatter.group.indices, transpose,
                                rows.to.ignore, cols.to.ignore, x.title, y.title)
        if (x.title == "")
            x.title <- scatterplot.data$x.title
        if (y.title == "")
            y.title <- scatterplot.data$y.title
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
    }

    if (is.null(series.marker.colors))
    {
        series.marker.colors <- series.line.colors
        series.marker.colors.reverse <- series.line.colors.reverse
    }

    if (is.null(series.marker.border.colors))
    {
        series.marker.border.colors <- series.marker.colors
        series.marker.border.colors.reverse <- series.marker.colors.reverse
    }

    if (type == "Pie" || type == "Donut")
        return(pieChart(
                chart.matrix = chart.matrix,
                type = type,
                values.color = colors,
                colors.reverse = colors.reverse,
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
                pie.subslice.colors.reverse = pie.subslice.colors.reverse,
                pie.groups.order = pie.groups.order,
                pie.inner.radius = pie.inner.radius,
                pie.subslice.colors.repeat = pie.subslice.colors.repeat,
                pie.border.color = pie.border.color,
                pie.show.percentages = pie.show.percentages,
                table.statistic = table.statistic))

    ## Settings specific to labelled scatter plots
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

        labeled.scatterplot <- labeledScatterplot(chart.matrix = chart.matrix,
                                                  colors = colors,
                                                  colors.reverse = colors.reverse,
                                                  type = type,
                                                  group.labels.text = scatter.group.labels,
                                                  group.indices.text = scatter.group.indices,
                                                  origin = FALSE, # base on y and x.zero.line.width
                                                  transpose = transpose,
                                                  rows.to.ignore = rows.to.ignore,
                                                  cols.to.ignore = cols.to.ignore,
                                                  legend.show = legend.show,
                                                  x.title = x.title,
                                                  y.title = y.title)

        return(rhtmlLabeledScatter::LabeledScatter(X = labeled.scatterplot$X,
                       Y = labeled.scatterplot$Y,
                       Z = labeled.scatterplot$Z,
                       label = labeled.scatterplot$label,
                       fixed.aspect = FALSE,
                       group = if (length(unique(labeled.scatterplot$group)) == 1) NULL else labeled.scatterplot$group,
                       grid = draw.grid,
                       origin = labeled.scatterplot$origin,
                       origin.align = FALSE,
                       labels.show = data.label.show,
                       legend.show = labeled.scatterplot$legend.show,
                       legend.bubbles.show = labeled.scatterplot$legend.bubbles.show,
                       legend.font.color = legend.font.color,
                       legend.font.family = legend.font.family,
                       legend.font.size = legend.font.size,
                       colors = labeled.scatterplot$colors,
                       y.title = labeled.scatterplot$y.title,
                       y.title.font.family = y.title.font.family,
                       y.title.font.color = y.title.font.color,
                       y.title.font.size = y.title.font.size,
                       axis.font.family = y.tick.font.family,
                       axis.font.color = y.tick.font.color,
                       axis.font.size = y.tick.font.size,
                       x.title = labeled.scatterplot$x.title,
                       x.title.font.family = x.title.font.family,
                       x.title.font.color = x.title.font.color,
                       x.title.font.size = x.title.font.size,
                       z.title = z.title,
                       y.decimals = if (is.null(y.tick.decimals)) 1 else y.tick.decimals,
                       x.decimals = if (is.null(x.tick.decimals)) 1 else x.tick.decimals,
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
                       plot.border.show = FALSE,
                       title = title
                       ))
    }

    ## Work out color ranges; n.b. some color ranges worked out in the chart specific functions.
    number.colors.needed <- if (is.scatterplot) length(unique(scatterplot.data$group)) else ncol(chart.matrix)

    ## Calculate colors
    if (length(number.colors.needed) == 0 || number.colors.needed == 0)
        stop("Chart matrix is empty.")
    colors <- ChartColors(number.colors.needed = number.colors.needed,
                                           given.colors = colors,
                                           reverse = colors.reverse,
                                           trim.light.colors = TRUE)
    series.line.colors <- ChartColors(number.colors.needed = number.colors.needed,
                                                       given.colors = series.line.colors,
                                                       reverse = series.line.colors.reverse,
                                                       trim.light.colors = TRUE)
    series.marker.colors <- ChartColors(number.colors.needed = number.colors.needed,
                                                         given.colors = series.marker.colors,
                                                         reverse = series.marker.colors.reverse,
                                                         trim.light.colors = TRUE)
    series.marker.border.colors <- ChartColors(number.colors.needed = number.colors.needed,
                                                                given.colors = series.marker.border.colors,
                                                                reverse = series.marker.border.colors.reverse,
                                                                trim.light.colors = TRUE)

    ## Color inheritance - second run
    if (is.null(series.line.colors))
        series.line.colors <- colors

    if (is.null(series.marker.colors))
        series.marker.colors <- series.line.colors

    if (is.null(series.marker.border.colors))
        series.marker.border.colors <- series.marker.colors

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
        x.labels <- autoFormatLongLabels(x.labels, wordwrap = x.tick.label.wordwrap)
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
                    margin.bottom <- new.margin + 10 + x.title.font.size * (x.title != "") * 1.25
                else
                    margin.bottom <- 50 + 1.25 * (x.tick.font.size*floor(lab.nline/2) + 
                                                  x.title.font.size*(x.title != ""))
            }
        }
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
                    data.label.show,
                    data.label.font.family,
                    data.label.font.size,
                    data.label.font.color,
                    data.label.decimals, # Ignored in Labeled Bubble and Scatterplots
                    data.label.prefix,
                    data.label.suffix))


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
            lab.len <- nchar(data.label.prefix) + nchar(data.label.suffix) + data.label.decimals
        if (data.label.show || (!is.null(series.marker.show) && series.marker.show != "none"))
            padding <- (max.x - min.x) * (0.01 * lab.len + (0.1 * (type == "Bar")))

        x.bounds.minimum <- min.x - (padding * (type != "Bar"))
        x.bounds.maximum <- max.x + padding
        x.has.bounds <- TRUE
        added.bounds.for.area.chart <- TRUE
    }
    else
        added.bounds.for.area.chart <- FALSE

    # Determine decimal places to show if not provided
    if (swap.axes.and.data)
    {
        if (is.null(x.tick.decimals))
        {
            x.tick.decimals <- if (x.has.bounds)
                decimalsToDisplay(c(x.bounds.minimum, x.bounds.maximum))
            else if (is.stacked && !is.hundred.percent.stacked)
                decimalsToDisplay(rowSums(chart.matrix, na.rm = TRUE))
            else
                decimalsToDisplay(chart.matrix)
        }
    }
    else if (is.null(y.tick.decimals))
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
            margin.top <- if (is.null(title) || title == "") 50 else 70
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
        x.prefix <- if (x.tick.prefix == "") data.label.prefix else x.tick.prefix
        x.suffix <- if (x.tick.suffix == "") data.label.suffix else x.tick.suffix
        y.prefix <- if (y.tick.prefix == "") data.label.prefix else y.tick.prefix
        y.suffix <- if (y.tick.suffix == "") data.label.suffix else y.tick.suffix

        source.text <- paste0(scatterplot.data$row.names, " (",
            x.prefix, FormatWithDecimals(scatterplot.data$x, data.label.decimals), x.suffix, ",",
            y.prefix, FormatWithDecimals(scatterplot.data$y, data.label.decimals), y.suffix, ")")

        # Iteratively add each group so the order is the same as the dataframe
        g.list <- unique(scatterplot.data$group)
        p <- plot_ly(as.data.frame(x=scatterplot.data$x, y=scatterplot.data$y))
        for (ggi in 1:length(g.list))
        {
            ind <- which(scatterplot.data$group == g.list[ggi])
            line.obj <- if (is.null(series.line.width) || series.line.width == 0) NULL
                        else list(width=series.line.width, color=series.marker.colors[ggi])

            p <- add_trace(p, x=scatterplot.data$x[ind], y=scatterplot.data$y[ind],
                    name=g.list[ggi], showlegend=(length(g.list) > 1),
                    text=source.text[ind], textfont=textfont, textposition=data.label.position,
                    marker=list(size=series.marker.size, color=series.marker.colors[ggi],
                    line=list(width=series.marker.border.width)), line=line.obj,
                    type=plotly.type, mode=series.mode, symbols=series.marker.symbols,
                    hoverinfo=if(length(g.list) > 1) "name+text" else "text")
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
                      FormatWithDecimals(chart.matrix[, i] * data.label.mult, data.label.decimals),
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
                               marker = marker)

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
