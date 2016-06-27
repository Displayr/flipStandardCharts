#' Generates an interactive chart based on the plotly library.
#'
#' \code{Chart} generates standard charts from plotly library.
#'
#' @param y A vector, matrix, list of vectors, data frame, or table.
#' @param x A vector over which y will be aggregated. Must have the same
#' number of elements as y.
#' @param type Character; type of chart. Can be "Area", "Stacked Area",
#' or "100\% Stacked Area".
#' @param transpose Logical; should the final output be transposed?
#' @param aggregate.period Character; can be "month", "quarter", "year".
#' Only relevant when x is a vector of mode date.
#' @param y.labels Character vector, overrides chart matrix row names.
#' @param y.values Integer vector, optiona, for manually specifying the points
#' along the y-axis where the y.labels should appear.
#' @param x.labels Character vector, overrides chart matrix column names.
#' @param x.values Integer vector, optiona, for manually specifying the points
#' along the x-axis where the x.labels should appear.
#' @param title Character; chart title.
#' @param title.font.family Character; title font family.  Can be "Arial
#' Black", "Arial", "Comic Sans MS", "Courier New", "Georgia", "Impact",
#' "Lucida Console", "Lucida Sans Unicode", "Marlett", "Symbol", "Tahoma",
#' "Times New Roman", "Trebuchet MS", "Verdana", "Webdings"
#' @param title.font.color Title font color as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param title.font.size Title font size; default = 10.
#' @param colors Vector of colors in RGB format.
#' @param transparency Transparency of area fill colors as an alpha value
#' (0 to 1).
#' @param chart.fill.color Chart (borders around plot) background color as
#' a named color in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param chart.fill.transparency Chart (borders around plot) background
#' transparency as an alpha value (0 to 1).
#' @param plot.fill.color Plot (the plot area proper) background color as
#' a named color in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param plot.fill.transparency Plot (the plot area proper) background
#' transparency as an alpha value (0 to 1).
#' @param legend.show Logical; show the legend.
#' @param legend.fill Legend fill color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param legend.border.color Legend border color as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param legend.border.line.width Integer; width in pixels of the border
#' around the legend.  0 = no border.
#' @param legend.font.color Legend font color as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param legend.font.family Character; legend font family.
#' @param legend.font.size Integer; legend font size.
#' @param legend.position Where the legend will be placed; can be "left" or
#' "right" of plot.
#' @param legend.sort.order Character; can be "normal" or "reversed" (see
#' also grouping options, currently excluded from this function)
#' @param margin.top Integer; margin between plot area and the top of the
#' graphic in pixels
#' @param margin.bottom Integer; margin between plot area and the top of the
#' graphic in pixels
#' @param margin.left Integer; margin between plot area and the top of the
#' graphic in pixels
#' @param margin.right Integer; margin between plot area and the top of the
#' graphic in pixels
#' @param margin.inner.pad Integer; padding in pixels between plot proper
#' and axis lines
#' @param y.title Character, y-axis title
#' @param y.title.font.color Y-axis title font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0,
#' max = 255)).
#' @param y.title.font.family Character; Y-axis title font family
#' @param y.title.font.size Integer; y-axis title font size
#' @param y.line.width Integer; y-axis line in pixels, 0 = no line
#' @param y.line.color Y-axis line color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param y.tick.marks Character; whether and where to show tick marks on the
#' y axis.  Can be "outside", "inside", "none"
#' @param y.tick.length Integer; length of tick marks in pixels.
#' @param y.bounds.minimum Integer or NULL; set minimum of range for plotting;
#' NULL = no manual range set.  Must be less than y.bounds.maximum
#' @param y.bounds.maximum = Integer or NULL; set maximum of range for
#' plotting; NULL = no manual range set.  Must be greater than y.bounds.minimum
#' @param y.bounds.units.major Ingeger or NULL; set tick mark distance in
#' y-axis units between minimum and maximum for plotting; NULL = no manual
#' range set.
#' @param y.zero.line.width Width in pixels of zero line; 0 = no zero line
#' shown
#' @param y.zero.line.color Color of horizontal zero (origo) line as a named
#' color in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param y.position Character; set y-axis position; can be "left" or "right"
#' @param y.mirror Logical; mirror y-axis on other side?
#' @param y.values.reversed Logical; whether to reverse y-axis or not
#' @param y.grid.width Integer; width of y-grid lines in pixels; 0 = no line
#' @param y.grid.color Color of y-grid lines as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param y.tick.suffix Y-axis tick label suffix
#' @param y.tick.prefix Y-axis tick label prefix
#' @param y.tick.decimals Y-axis tick label decimal places
#' @param y.tick.format.manual Overrides tick.prefix, suffix and decimals;
#' See https://github.com/mbostock/d3/wiki/Formatting#numbers or
#' https://docs.python.org/release/3.1.3/library/string.html#formatspec
#' @param y.hovertext.suffix Y-axis hover text number suffix
#' @param y.hovertext.prefix Y-axis hover text number prefix
#' @param y.hovertext.decimals Y-axis hover text decimal places
#' @param y.hovertext.manual Overrides hovertext.prefix, suffix and decimals;
#' See https://github.com/mbostock/d3/wiki/Formatting#numbers or
#' https://docs.python.org/release/3.1.3/library/string.html#formatspec
#' @param y.tick.angle Integer, y-axis tick label angle in degrees.
#' 90 = vertical; 0 = horizontal
#' @param y.tick.font.color Y-axis tick label font color as a named color
#' in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param y.tick.font.family Character; y-axis tick label font family
#' @param y.tick.font.size Integer; y-axis tick label font size
#' @param x.title Character, x-axis title
#' @param x.title.font.color x-axis title font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param x.title.font.family Character; x-axis title font family
#' @param x.title.font.size Integer; x-axis title font size
#' @param x.line.width Integer; x-axis line in pixels, 0 = no line
#' @param x.line.color X-axis line color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param x.tick.marks Character; whether and where to show tick marks on the
#' y axis.  Can be "outside", "inside", "none"
#' @param x.tick.length Integer; length of tick marks in pixels.
#' @param x.bounds.minimum Integer or NULL; set minimum of range for plotting;
#' NULL = no manual range set.  Must be less than x.bounds.maximum
#' @param x.bounds.maximum = Integer or NULL; set maximum of range for
#' plotting; NULL = no manual range set.  Must be greater than x.bounds.minimum
#' @param x.bounds.units.major Ingeger or NULL; set tick mark distance in
#' x-axis units between minimum and maximum for plotting; NULL = no manual
#' range set.
#' @param x.tick.frequency Integer; number of ticks to show on x-axis.
#' @param x.zero.line.width Width in pixels of zero line; 0 = no zero line
#' shown
#' @param x.zero.line.color Color of horizontal zero (origo) line as a named
#' color in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param x.position Character; set x-axis position; can be "left" or "right"
#' @param x.mirror Logical; mirror x-axis on other side?
#' @param x.values.reversed Logical; whether to reverse x-axis or not
#' @param x.grid.width Integer; width of y-grid lines in pixels; 0 = no line
#' @param x.grid.color Color of y-grid lines as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param x.tick.suffix X.axis tick label suffix
#' @param x.tick.prefix X.axis tick label prefix
#' @param x.tick.decimals X.axis tick label decimal places
#' @param x.tick.format.manual Overrides tick.prefix, suffix and decimals;
#' See https://github.com/mbostock/d3/wiki/Formatting#numbers or
#' https://docs.px.hon.org/release/3.1.3/librarx.string.html#formatspec
#' @param x.hovertext.suffix X.axis hover text number suffix
#' @param x.hovertext.prefix X.axis hover text number prefix
#' @param x.hovertext.decimals X.axis hover text decimal places
#' @param x.hovertext.manual Overrides hovertext.prefix, suffix and decimals;
#' See https://github.com/mbostock/d3/wiki/Formatting#numbers or
#' https://docs.px.hon.org/release/3.1.3/librarx.string.html#formatspec
#' @param x.tick.angle Integer, x-axis tick label angle in degrees.
#' 90 = vertical; 0 = horizontal
#' @param x.tick.font.color X-axis tick label font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param x.tick.font.family Character; x-axis tick label font family
#' @param x.tick.font.size Integer; x-axis tick label font size
#' @param x.tick.label.autoformat Logical; whether to apply built-in auto-
#' formatting of long (> 15 characters) text labels on the x-axis
#' @param series.marker.show Can be "none", "automatic" or a vector referencing
#' the plotly symbol dictionary using either numerics or strings.
#' @param series.marker.color Vector of colors in RGB format to use for the
#' markers
#' @param series.marker.transparency Integer; transparency for series markers
#' as an alpha value (0 to 1)
#' @param series.marker.size Integer; size in pixels of marker
#' @param series.marker.border.width Integer; width in pixels of border/line
#' around series markers; 0 is no line
#' @param series.marker.border.color Vector of colors in RGB format for
#' border/line around series markers
#' @param series.marker.border.transparency Integer; transparency of
#' border/line around series markers as an alpha value (0 to 1)
#' @param series.marker.text Logical; whether to include data point with the
#' markers on line chart series.
#' @param series.marker.text.position Character; where to place the source data
#' value in relation to the marker icon.  Can be "top left", "top center", "top
#' right", "middle left", "middle center", "middle right", "bottom left",
#' "bottom center", "bottom right"
#' @param series.marker.text.color Vector of colors in RGB format for source
#' data label
#' @param series.marker.text.family Character; the font family of the source
#' data label
#' @param series.marker.text.size Integer; font size of the source data label
#' @param series.marker.text.percent Logical; if the source value should be
#' multiplied by 100.
#' @param series.line.width Integer; thickness, in pixels, of the series line
#' @param series.line.color Vector of colors in RGB format for series the lines
#' @param series.line.transparency Integer; transparency for series lines as an
#' alpha value (0 to 1)
#' @param hover.mode Character or logic; can be FALSE for no hover text, "x" to
#' show all x-values for the hover point, "y" to show all y-values for the
#' hover point, or "closest" to show the single, nearest, value.
#' @param hover.include.source.value Logical; Include source data point value
#' in the hover text.
#' @param hover.include.source.value.prefix Character; prefix for source data
#' point value in hover text.
#' @param hover.include.source.value.suffix Character; suffix for source data
#' point value in hover text.
#' @param hover.include.source.value.percent Logical; multiplies source data
#' point value by 100.
#' @param show.modebar Logical; whether to show the zoom menu buttons or not.
#' @examples
#' data("y.data")
#' data("x.data")
#' Chart(y = y.data, x = x.data, type = "Area", transpose = TRUE)
#' @param subtitle.text Character; text string to appear as a sub-title
#' @param subtitle.border.width Numeric; width in pixels of border around
#' sub-title.
#' @param subtitle.border.color Sub-title border color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param subtitle.background.color Sub-title background color as a named
#' color in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param subtitle.font.family Character; Sub-title font family
#' @param subtitle.font.size Integer; Sub-title font size
#' @param subtitle.font.color Sub-title font color as a named color
#' in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param global.font.family.override Character; font family to override
#' all occurrences of any font attribute for the chart instead of specifying
#' font for all font attributes individually
#' @param global.font.color.override Global font color as a named color
#' in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).  Will only work if global.font.family.override
#' is also set.
#' @param rows.to.ignore Character; comma separated string of row headings to
#' exclude from the charting.
#' @param cols.to.ignore Character; comma separated string of column headings to
#' exclude from the charting.
#' @param bar.gap Integer; chart proportion between each bar or column if using
#' bar or column charts, or between each cluster of bars or columns.
#' @param bar.group.gap Integer; chart proportion between each bar or column in
#' a cluster.
#' @param bar.data.label.offset Numeric; number of y-axis units to offset the
#' data label from the end of the column (use negative numbers for down, positive
#' for up)
#' @param bar.data.label Integer or Character; the offset from the top end of a
#' column measured in y-axis units.
#' @param bar.data.label.family Character; font family for data label.
#' @param bar.data.label.size Integer; font size for data label.
#' @param bar.data.label.color Font color as a named color
#' in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param bar.data.label.decimals Integer; number of decimal places to show in
#' bar and column chart data labels.
#' @param bar.data.label.as.percent Logical; whether to treat data labels in
#' bar or column charts as percentages or not.
#' @export
Chart <-   function(y,
                        x = NULL,
                        # weights = NULL,                                 ## Gets passed to AsChartMatrix <- add to that function first!
                        # subset = NULL,                                  ## Gets passed to AsChartMatrix <- add to that function first!
                        type = "Area",
                        transpose = FALSE,                                ## Should the inputs be transposed; TRUE or FALSE
                        aggregate.period = "month",
                        y.labels = NULL,
                        y.values = NULL,
                        x.labels = NULL,
                        x.values = NULL,
                        title = "",
                        title.font.family = "Arial",
                        title.font.color = rgb(44, 44, 44, maxColorValue = 255),
                        title.font.size = 16,
                        colors = qColors,
                        transparency = 0.4,
                        chart.fill.color = rgb(255, 255, 255, maxColorValue = 255),
                        chart.fill.transparency = 1,
                        plot.fill.color = rgb(255, 255, 255, maxColorValue = 255),
                        plot.fill.transparency = 1,
                        legend.show = TRUE,
                        legend.fill = rgb(255, 255, 255, maxColorValue = 255),
                        legend.border.color = rgb(44, 44, 44, maxColorValue = 255),
                        legend.border.line.width = 0,
                        legend.font.color = rgb(44, 44, 44, maxColorValue = 255),
                        legend.font.family = "Arial",
                        legend.font.size = 10,
                        legend.position = "right",
                        legend.sort.order = "normal",
                        margin.top = 80,
                        margin.bottom = 80,
                        margin.left = 80,
                        margin.right = 80,
                        margin.inner.pad = 0,
                        y.title = "",
                        y.title.font.color = rgb(44, 44, 44, maxColorValue = 255),
                        y.title.font.family = "Arial",
                        y.title.font.size = 12,
                        y.line.width = 0,
                        y.line.color = rgb(0, 0, 0, maxColorValue = 255),
                        y.tick.marks = "",
                        y.tick.length = 5,
                        y.bounds.minimum = NULL,
                        y.bounds.maximum = NULL,
                        y.bounds.units.major = NULL,
                        # y.number.ticks = NULL,
                        y.zero.line.width = 0,
                        y.zero.line.color = rgb(44, 44, 44, maxColorValue = 255),
                        y.position = "left",
                        y.mirror = FALSE,
                        y.values.reversed = FALSE,                       ## T/F - involves autorange and may be too complicated.
                        y.grid.width = 1,
                        y.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                        y.tick.suffix = "",
                        y.tick.prefix = "",
                        y.tick.decimals = 0,
                        y.tick.format.manual = "",
                        y.hovertext.suffix = "",
                        y.hovertext.prefix = "",
                        y.hovertext.decimals = 2,
                        y.hovertext.manual = "",
                        y.tick.angle = 0,
                        y.tick.font.color = rgb(0, 0, 0, maxColorValue = 255),
                        y.tick.font.family = "Arial",
                        y.tick.font.size = 10,
                        x.title = "",
                        x.title.font.color = rgb(44, 44, 44, maxColorValue = 255),
                        x.title.font.family = "Arial",
                        x.title.font.size = 12,
                        x.line.width = 0,
                        x.line.color = rgb(0, 0, 0, maxColorValue = 255),
                        x.tick.marks = "",
                        x.tick.length = 5,
                        x.bounds.minimum = NULL,
                        x.bounds.maximum = NULL,
                        x.bounds.units.major = NULL,
                        x.tick.frequency = NULL,
                        x.zero.line.width = 0,
                        x.zero.line.color = rgb(44, 44, 44, maxColorValue = 255),
                        x.position = "bottom",
                        x.mirror = FALSE,
                        x.values.reversed = FALSE,                       ## T/F - involves autorange and may be too complicated.
                        x.grid.width = 0,
                        x.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                        x.tick.suffix = "",
                        x.tick.prefix = "",
                        x.tick.decimals = 0,
                        x.tick.format.manual = "",
                        x.hovertext.suffix = "",
                        x.hovertext.prefix = "",
                        x.hovertext.decimals = 2,
                        x.hovertext.manual = "",
                        x.tick.angle = 0,
                        x.tick.font.color = rgb(0, 0, 0, maxColorValue = 255),
                        x.tick.font.family = "Arial",
                        x.tick.font.size = 10,
                        x.tick.label.autoformat = TRUE,
                        series.marker.show = "none",
                        series.marker.color = qColors,
                        series.marker.transparency = 1,
                        series.marker.size = 6,
                        series.marker.border.width = 1,
                        series.marker.border.color = qColors,
                        series.marker.border.transparency = 1,
                        series.marker.text = FALSE,
                        series.marker.text.position = "top middle",
                        series.marker.text.color = rgb(0, 0, 0, maxColorValue = 255),
                        series.marker.text.family = "Arial",
                        series.marker.text.size = 10,
                        series.marker.text.percent = FALSE,
                        series.line.width = 0,
                        series.line.color = qColors,
                        series.line.transparency = 1,
                        hover.mode = "closest",
                        hover.include.source.value = FALSE,
                        hover.include.source.value.prefix = "",
                        hover.include.source.value.suffix = "",
                        hover.include.source.value.percent = FALSE,
                        show.modebar = FALSE,
                        subtitle.text = "",
                        subtitle.border.width = 0,
                        subtitle.border.color = "white",
                        subtitle.background.color = "white",
                        subtitle.font.family = "Arial",
                        subtitle.font.color = rgb(0, 0, 0, maxColorValue=255),
                        subtitle.font.size = 10,
                        global.font.family.override = "",
                        global.font.color.override = rgb(0, 0, 0, maxColorValue=255),
                        rows.to.ignore = "",
                        cols.to.ignore = "",
                        bar.gap = 0.15,
                        bar.group.gap = NULL,
                        bar.data.label.offset = NULL,
                        bar.data.label.family = "Arial",
                        bar.data.label.size = 10,
                        bar.data.label.color = rgb(0, 0, 0, maxColorValue=255),
                        bar.data.label.decimals = 0,
                        bar.data.label.as.percent = FALSE
)
{
    ## Make a chart matrix
    chart.matrix <- AsChartMatrix(y, x, transpose = transpose, aggregate.period = aggregate.period)

    ## Ignore rows or columns
    if (rows.to.ignore != "" | cols.to.ignore != "")
        chart.matrix <- removeRowsAndColumns(chart.matrix, rows.to.ignore, cols.to.ignore)

    ## Set defaults for chart specific items
    fill.bound <- ""
    legend.group <- ""
    barmode <- ""
    orientation <- NULL
    swap.axes.and.data <- FALSE
    y.nticks <- NULL

    ## Settings specific to Area Charts
    if (type == "Area" | type == "Stacked Area" | type == "100% Stacked Area")
    {
        chart.type.outputs <- areaChart(chart.matrix = chart.matrix,
                                        transparency = transparency,
                                        type = type,
                                        y.tick.format.manual = y.tick.format.manual,
                                        y.tick.suffix = y.tick.suffix,
                                        y.tick.decimals = y.tick.decimals,
                                        series.line.width = series.line.width,
                                        series.marker.show = series.marker.show
                                        )

        chart.matrix <- chart.type.outputs$chart.matrix
        fill.bound <- chart.type.outputs$fill.bound
        legend.group <- chart.type.outputs$legend.group
        y.tickformat <- chart.type.outputs$y.tickformat
        series.mode <- chart.type.outputs$series.mode
        transparency <- chart.type.outputs$transparency
    }

    ## Settings specific to Line Charts
    if (type == "Line")
    {
        chart.type.outputs <- lineChart(chart.matrix = chart.matrix,
                                        transpose = transpose,
                                        series.line.width = series.line.width,
                                        series.marker.show = series.marker.show,
                                        series.marker.text = series.marker.text)

        series.mode <- chart.type.outputs$series.mode
        series.line.width <- chart.type.outputs$series.line.width
        y.tickformat <- ""
        transpose <- chart.type.outputs$transpose
    }

    ## Settings specific to Column Charts
    if (type == "Column" | type == "Stacked Column" | type == "100% Stacked Column")
    {
        chart.type.outputs <- columnChart(chart.matrix = chart.matrix,
                                        type = type,
                                        y.tick.format.manual = y.tick.format.manual,
                                        y.tick.suffix = y.tick.suffix,
                                        y.tick.decimals = y.tick.decimals,
                                        series.marker.border.width = series.marker.border.width,
                                        bar.group.gap = bar.group.gap
                                        )

        chart.matrix <- chart.type.outputs$chart.matrix
        legend.group <- chart.type.outputs$legend.group
        y.tickformat <- chart.type.outputs$y.tickformat
        series.mode <- chart.type.outputs$series.mode
        orientation <- chart.type.outputs$orientation
        type <- chart.type.outputs$type
        barmode <- chart.type.outputs$barmode
        bar.group.gap <- chart.type.outputs$bar.group.gap
    }

    ## Settings specific to Bar Charts
    if (type == "Bar" | type == "Stacked Bar" | type == "100% Stacked Bar")
    {
        chart.type.outputs <- barChart(chart.matrix = chart.matrix,
                                          type = type,
                                          y.tick.format.manual = y.tick.format.manual,
                                          y.tick.suffix = y.tick.suffix,
                                          y.tick.decimals = y.tick.decimals,
                                          series.marker.border.width = series.marker.border.width,
                                          bar.group.gap = bar.group.gap,
                                          y.bounds.minimum = y.bounds.minimum,
                                          y.bounds.maximum = y.bounds.maximum,
                                          y.bounds.units.major = y.bounds.units.major,
                                          y.nticks = length(colnames(chart.matrix))
        )

        chart.matrix <- chart.type.outputs$chart.matrix
        legend.group <- chart.type.outputs$legend.group
        y.tickformat <- chart.type.outputs$y.tickformat
        series.mode <- chart.type.outputs$series.mode
        orientation <- chart.type.outputs$orientation
        type <- chart.type.outputs$type
        barmode <- chart.type.outputs$barmode
        bar.group.gap <- chart.type.outputs$bar.group.gap
        swap.axes.and.data <- chart.type.outputs$swap.axes.and.data
        y.bounds.minimum <- chart.type.outputs$y.bounds.minimum
        y.bounds.maximum <- chart.type.outputs$y.bounds.maximum
        y.bounds.units.major <- chart.type.outputs$y.bounds.units.major
        y.nticks <- y.nticks
    }

    ## Pie Chart / Donut / multiple pie charts

    ## Waterfall (part of column charts, really...)

    ## Scatterplot

    ## Radar/Polar plot

    ## ... Any other chart types...

    # Set all fonts to global font override if required
    if (global.font.family.override != "")
    {
        title.font.family <- global.font.family.override
        legend.font.family <- global.font.family.override
        y.title.font.family <- global.font.family.override
        y.title.font.family <- global.font.family.override
        y.tick.font.family <- global.font.family.override
        x.title.font.family <- global.font.family.override
        x.tick.font.family <- global.font.family.override
        series.marker.text.family <- global.font.family.override
        subtitle.font.family <- global.font.family.override
        bar.data.label.family <- global.font.family.override

        title.font.color <- global.font.color.override
        legend.font.color <- global.font.color.override
        y.title.font.color <- global.font.color.override
        y.title.font.color <- global.font.color.override
        y.tick.font.color <- global.font.color.override
        x.title.font.color <- global.font.color.override
        x.tick.font.color <- global.font.color.override
        series.marker.text.color <- global.font.color.override
        subtitle.font.color <- global.font.color.override
        bar.data.label.color <- global.font.color.override
    }

    # Bar and column chart data label annotations
    if (!is.null(bar.data.label.offset))
        data.annotations <- dataLabelAnnotation(chart.matrix = chart.matrix,
                                                bar.offset = bar.data.label.offset,
                                                bar.family = bar.data.label.family,
                                                bar.size = bar.data.label.size,
                                                bar.color = bar.data.label.color,
                                                bar.decimals = bar.data.label.decimals,
                                                barmode = barmode,
                                                bar.data.label.as.percent = bar.data.label.as.percent,
                                                swap.axes.and.data = swap.axes.and.data)
    else
        data.annotations <- list()

    # Sort out the sub-title
    subtitle <- list()
    if (subtitle.text != "")
    {
        # Allow some extra margin space
        subtitle.text <- as.vector(subtitle.text)

        if (margin.top < 81)
            margin.top <- ifelse(length(unlist(regmatches(subtitle.text, gregexpr("<br>", subtitle.text)))) > 1, (margin.top + length(unlist(regmatches(subtitle.text, gregexpr("<br>", subtitle.text)))) * subtitle.font.size), margin.top)

        subtitle <- list(y = 1.15,
                         x = 0.5,
                         text = subtitle.text,
                         xref = "paper",
                         yref = "paper",
                         showarrow = FALSE,
                         borderwidth = subtitle.border.width,
                         bordercolor = subtitle.border.color,
                         bgcolor = subtitle.background.color,
                         font = list(
                             color = subtitle.font.color,
                             size = subtitle.font.size,
                             family = subtitle.font.family
                         )
        )
    }


    # Create text matrix of source data if required for hover
    source.matrix <- chart.matrix
    if (hover.include.source.value.percent | series.marker.text.percent)
        source.matrix <- source.matrix * 100

    if (hover.include.source.value.percent)
        source.matrix <- matrix(paste(hover.include.source.value.prefix, " ", source.matrix, hover.include.source.value.suffix, sep = ""), nrow = nrow(chart.matrix), ncol = ncol(chart.matrix))

    ## Get axes labels from the matrix labels if none manually specified
    if (is.null(x.labels))
        x.labels <- colnames(chart.matrix)

    if (is.null(y.labels))
        y.labels <- rownames(chart.matrix)

    ## If no angle set for x.tick.angle and x.labels are > 15 characters,
    tally <- sapply(x.labels, function(x) nchar(x))
    if (max(tally) > 15 && x.tick.label.autoformat == TRUE)
    {
        x.tick.angle <- 315
        x.labels <- autoFormatLongLabels(x.labels)
        if (margin.bottom == 80)
            margin.bottom <- 100
    }
    else if (sum(tally) > 100 && x.tick.label.autoformat == TRUE)
        x.tick.angle <- 315

    ## Position legend
    legend.x.anchor <- "left"
    legend.y.anchor <- "auto"
    legend.y <- 1
    legend.x <- 1.02

    ### If legend on right and there's a y-axis on the right too:
    y2 = ""
    if (legend.position == "right" && (y.mirror == TRUE || y2 != "" || y.position == "right"))
        legend.x = 1.15

    ### If legend on the left, and there's no y-axis on the left:
    if (legend.position == "left" && y.position == "right" && y.mirror == FALSE && y2 != "")
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

    ## If line thickness is zero, then we shouldn't show a line; ticks only shown if there's a line (same as Excel)
    ## Tick labels only shown if there's a line.
    y.showline <- FALSE
    # y.showticklables <- FALSE
    y.showticklabels <- TRUE
    y.showticks <- FALSE
    if (y.line.width >= 1)
    {
        y.showline <- TRUE
        # y.showticklabels <- TRUE
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
    if (y.tick.marks == "none" | y.tick.length > 0)
    {
        y.showticks <- FALSE
        y.tick.length <- 0
    }

    x.showline <- FALSE
    x.showticklabels <- TRUE
    #x.showticklabels <- FALSE
    x.showticks <- FALSE
    if (x.line.width >= 1)
    {
        x.showline <- TRUE
        #x.showticklabels <- TRUE
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
    if (x.tick.marks == "none" | x.tick.length == 0)
    {
        x.showticks <- FALSE
        x.tick.length <- 0
    }

    ## Set tick and hover formats
    ifelse((y.tick.format.manual == "" && is.null(y.tickformat)), y.tickformat <- paste(".", y.tick.decimals, "f", sep=""), y.tickformat <- y.tick.format.manual)

    ifelse(x.tick.format.manual == "", x.tickformat <- paste(".", x.tick.decimals, "f", sep=""), x.tickformat <- x.tick.format.manual)

    ifelse(y.hovertext.manual == "", y.hoverformat <- paste(".", y.hovertext.decimals, "f", sep=""), y.hoverformat <- y.hovertext.manual)

    ifelse(x.hovertext.manual == "", x.hoverformat <- paste(".", x.hovertext.decimals, "f", sep=""), x.hoverformat <- x.hovertext.manual)

    ## Resolve numeric tick values based on y.bounds.minimum and y.bounds.maximum, and y.bounds.units.major
    # y.tickmode = "auto"
    # y.tickvals = integer()
    # y.ticktext = character()
    # y.range = integer()
    # y.autorange = TRUE
    # y.rangemode = "tozero"
    #
    # if (is.null(y.bounds.minimum) | is.null(y.bounds.maximum) | is.null(y.bounds.units.major))
    #     y.bounds.manual <- FALSE
    # else
    # {
    #     y.bounds.manual <- TRUE
    #     y.range <- c(y.bounds.minimum, y.bounds.maximum)
    #     y.autorange = FALSE
    #     y.tickmode <- "array"
    #     for (a in seq(y.bounds.minimum, y.bounds.maximum, by = y.bounds.units.major))
    #     {
    #         y.tickvals <- c(y.tickvals, a)
    #     }
    #
    #     if (y.tickformat == "%")
    #         y.ticktext <- sapply(y.tickvals, function(x) paste(round(x * 100, y.tick.decimals), "%", sep = ""))
    #     else if (y.tickformat == "$")
    #         y.ticktext <- sapply(y.tickvals, function(x) ifelse(x < 0, paste("-$", -1 * round(x, y.tick.decimals), sep = ""), paste("$", round(x, y.tick.decimals), sep = "")))
    #     else
    #         y.ticktext <- sapply(y.tickvals, function(x) paste(round(x, y.tick.decimals)))
    # }

    y.tickmode <- "auto"
    y.tickvals <- integer()
    y.ticktext <- character()
    y.range <- integer()
    y.autorange <- TRUE
    # y.nticks <- length(y.labels)
    y.rangemode <- "tozero"

    if (!is.null(y.bounds.minimum) && !is.null(y.bounds.maximum) && !is.null(y.bounds.units.major))
    {
        y.tickmode <- "array"
        y.autorange <- FALSE
        y.range <- c(y.bounds.minimum, y.bounds.maximum)

        for (a in seq(y.bounds.minimum, y.bounds.maximum, by = y.bounds.units.major))
        {
            y.tickvals <- c(y.tickvals, a)
            # if (!swap.axes.and.data)
            #     y.ticktext <- y.labels[seq(1, length(y.labels), a)]
        }

        if (y.tickformat == "%")
            y.ticktext <- sapply(y.tickvals, function(x) paste(round(x * 100, y.tick.decimals), "%", sep = ""))
        else if (y.tickformat == "$")
            y.ticktext <- sapply(y.tickvals, function(x) ifelse(x < 0, paste("-$", -1 * round(x, y.tick.decimals), sep = ""), paste("$", round(x, y.tick.decimals), sep = "")))
        else
            y.ticktext <- sapply(y.tickvals, function(x) paste(round(x, y.tick.decimals)))
    }
    else if (!is.null(y.values) && !is.null(y.labels))
    {
        y.tickmode <- "array"
        # y.autorange <- TRUE
        y.tickvals <- y.values
        y.ticktext <- y.labels
        # y.range <- c(y.values[1], y.values[ncol(chart.matrix)])
    }
    else
    {
        y.tickmode <- "auto"
        y.autorange <- TRUE
        # if (!is.null(y.number.ticks))
        #     y.nticks <- y.number.ticks
    }

    x.tickmode <- "auto"
    x.tickvals <- integer()
    x.ticktext <- character()
    x.range <- integer()
    x.autorange <- TRUE
    x.nticks <- length(x.labels)
    x.dtick <- NULL
    x.tick0 <- NULL

    if (!is.null(x.bounds.minimum) && !is.null(x.bounds.maximum) && !is.null(x.bounds.units.major))
    {
        x.tickmode <- "array"
        x.autorange <- FALSE
        x.range <- c(x.bounds.minimum, x.bounds.maximum)

        for (a in seq(x.bounds.minimum, x.bounds.maximum, by = x.bounds.units.major))
        {
            x.tickvals <- c(x.tickvals, a)
            if (!swap.axes.and.data)
                x.ticktext <- x.labels[seq(1, length(x.labels), a)]
        }

        if (x.tickformat == "%")
            x.ticktext <- sapply(x.tickvals, function(x) paste(round(x * 100, x.tick.decimals), "%", sep = ""))
        else if (x.tickformat == "$")
            x.ticktext <- sapply(x.tickvals, function(x) ifelse(x < 0, paste("-$", -1 * round(x, x.tick.decimals), sep = ""), paste("$", round(x, x.tick.decimals), sep = "")))
        else
            x.ticktext <- sapply(x.tickvals, function(x) paste(round(x, x.tick.decimals)))
    }
    else if (!is.null(x.values) && !is.null(x.labels))
    {
        x.tickmode <- "array"
        # x.autorange <- TRUE
        x.tickvals <- x.values
        x.ticktext <- x.labels
        # x.range <- c(x.values[1], x.values[ncol(chart.matrix)])
    }
    else
    {
        x.tickmode <- "linear"
        if (is.null(x.tick.frequency))
        {
            # Check if the column headings are numeric, and if so, make the step the diff between item 1 and 2
            if (length(which(is.na(suppressWarnings(as.numeric(colnames(chart.matrix)))))) == 0)
            {
                numeric.cols <- as.numeric(colnames(chart.matrix))
                tick.gap <- numeric.cols[2] - numeric.cols[1]
                x.dtick <- tick.gap
                x.tick0 <- 0
            }
            else
            {
                x.dtick <- 1
                x.tick0 <- 1
            }
        }
        else
        {
            x.dtick <- x.tick.frequency # round((length(x.labels) / x.number.ticks), digits = 0)
            x.tick0 <- 1
        }
    }

    ## Should autorange be = "reverse"?
    if (y.values.reversed == TRUE)
        y.autorange = "reversed"

    if (x.values.reversed == TRUE)
        x.autorange = "reversed"

    ## Should we draw a zero line
    y.zero.line <- FALSE
    if (y.zero.line.width > 0)
        y.zero.line <- TRUE

    x.zero.line <- FALSE
    if (x.zero.line.width > 0)
        x.zero.line <- TRUE

    ## Mirror settings
    if (y.mirror == TRUE)
        y.mirror <- "allticks"

    if (x.mirror == TRUE)
        x.mirror <- "allticks"

    ## Show plot grid?
    y.grid.show <- FALSE
    if (y.grid.width > 0)
        y.grid.show <- TRUE

    x.grid.show <- FALSE
    if (x.grid.width > 0)
        x.grid.show <- TRUE

    ## Which markers to show?
    if (series.marker.show == "automatic" || series.marker.show == "none")
        series.marker.symbols <- plotlySymbols
    else if (series.marker.show != "none" && series.marker.show != "automatic")
    {
        if (length(series.marker.show) < 100)
            series.marker.symbols <- rep(series.marker.show, 100)
    }

    ## Show source data points in hover text, or along series markers
    if (series.marker.text)
        series.mode <- paste(series.mode, "+text", sep = "")

    axis.to.show <- "y"
    if (swap.axes.and.data)
        axis.to.show <- "x"

    show.series.name = "+name"
    if (nrow(chart.matrix) == 1)
        show.series.name = ""

    if (hover.include.source.value)
        hoverinfo = paste(axis.to.show, show.series.name, "+text", sep = "")
    else
        hoverinfo = paste(axis.to.show, show.series.name, sep = "")

    ## Increase number of colors in color vectors such that a max of 100 are stored, in case insufficient numbers have been specified
    colors <- rep(colors, 100/length(colors))
    series.marker.color <- rep(series.marker.color, 100/length(series.marker.color))
    series.marker.border.color <- rep(series.marker.border.color, 100/length(series.marker.border.color))
    series.line.color <- rep(series.line.color, 100/length(series.line.color))

    ## Build annotations list
    if (subtitle.text != "")
        data.annotations[[length(data.annotations) + 1]] <- subtitle

    ## Hide legend if only one series to plot
    if ((type != "Bar" & type != "Stacked Bar" & type != "100% Stacked Bar") && nrow(chart.matrix) == 1)
        legend.show <- FALSE

    if ((type == "Bar" | type == "Stacked Bar" | type == "100% Stacked Bar") && ncol(chart.matrix) == 1)
        legend.show <- FALSE

    ## Initiate plotly object
    p <- plotly::plot_ly()

    ## Config options
    p <- plotly::config(displayModeBar = show.modebar)

    ## Add a trace for each row of data in the matrix
    for (a in 1:nrow(chart.matrix))
    {
        y <- as.numeric(chart.matrix[a, ])
        x <- as.character(colnames(chart.matrix))

        if (swap.axes.and.data == TRUE)
        {
            y.swap <- y
            x.swap <- x
            y <- x.swap
            x <- y.swap
        }

        source.text <- source.matrix[a, ]

        p <- plotly::add_trace(p,
                               type = type,
                               x = x,
                               y = y,
                               evaluate = TRUE,
                               orientation = orientation,
                               fill = fill.bound,
                               fillcolor = plotly::toRGB(colors[a], alpha = transparency),
                               line = list(
                                   width = series.line.width,
                                   color = plotly::toRGB(series.line.color[a], alpha = series.line.transparency)
                               ),
                               name = y.labels[a],
                               legendgroup = legend.group,
                               text = source.text,
                               textposition = series.marker.text.position,
                               textfont = list(
                                   family = series.marker.text.family,
                                   color = plotly::toRGB(series.marker.text.color, alpha = 1),
                                   size = series.marker.text.size
                               ),
                               ## MARKERS
                               mode = series.mode,
                               marker = list(
                                   size = series.marker.size,
                                   color = plotly::toRGB(series.marker.color[a], alpha = series.marker.transparency),
                                   symbol = series.marker.symbols[a],
                                   line = list(
                                       color = plotly::toRGB(series.marker.border.color[a], alpha = series.marker.border.transparency),
                                       width = series.marker.border.width
                                   )
                               ),
                               hoverinfo = hoverinfo
        )
    }

    ## Set plotly layout styles
    p <- plotly::layout(
        title = title,
        ## LEGEND
        showlegend = legend.show,
        legend = list(
            bgcolor = legend.fill,
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
            showticklabels = y.showticklabels,
            range = y.range,
            rangemode = y.rangemode,
            ticks = y.tick.marks,
            tickangle = y.tick.angle,
            ticklen = y.tick.length,
            tickcolor = y.line.color,
            nticks = y.nticks,
            dtick = NULL,
            tick0 = NULL,
            zeroline = y.zero.line,
            zerolinewidth = y.zero.line.width,
            zerolinecolor = y.zero.line.color,
            tickformat = y.tickformat,
            tickprefix = y.tick.prefix,
            ticksuffix = y.tick.suffix,
            autorange = y.autorange,
            side = y.position,
            mirror = y.mirror,
            gridwidth = y.grid.width,
            gridcolor = y.grid.color,
            showgrid = y.grid.show,
            hoverformat = y.hoverformat,
            showexponent = "all",
            showtickprefix = TRUE,
            showticksuffix = TRUE
        ),
        ## X-AXIS
        xaxis = list(
            title = x.title,
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
            showticklabels = x.showticklabels,
            range = x.range,
            ticks = x.tick.marks,
            tickangle = x.tick.angle,
            ticklen = x.tick.length,
            tickcolor = x.line.color,
            nticks = x.nticks,
            dtick = x.dtick,
            tick0 = x.tick0,
            zeroline = x.zero.line,
            zerolinewidth = x.zero.line.width,
            zerolinecolor = x.zero.line.color,
            tickformat = x.tickformat,
            tickprefix = x.tick.prefix,
            ticksuffix = x.tick.suffix,
            autorange = x.autorange,
            side = x.position,
            mirror = x.mirror,
            gridwidth = x.grid.width,
            gridcolor = x.grid.color,
            showgrid = x.grid.show,
            hoverformat = x.hoverformat,
            showexponent = "all",
            showtickprefix = TRUE,
            showticksuffix = TRUE
        ),
        ## MARGINS
        margin = list(
            t = margin.top,
            b = margin.bottom,
            l = margin.left,
            r = margin.right,
            pad = margin.inner.pad
        ),
        plot_bgcolor = plotly::toRGB(plot.fill.color, alpha = plot.fill.transparency),
        paper_bgcolor = plotly::toRGB(chart.fill.color, alpha = chart.fill.transparency),
        hovermode = hover.mode,
        font = list(
            family = title.font.family ,
            color = title.font.color,
            size = title.font.size
        ),
        annotations = data.annotations,
        bargap = bar.gap,
        bargroupgap = bar.group.gap,
        barmode = barmode
    )

    ## Return the chart
    p
}

