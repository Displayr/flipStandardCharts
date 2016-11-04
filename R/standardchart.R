#' Generates an interactive chart based on the plotly library.
#'
#' \code{Chart} generates standard charts from plotly library.
#'
#' @param y A table or matrix.
#' @param type Character; type of chart. Can be "Area", "Stacked Area",
#' or "100\% Stacked Area".
#' @param transpose Logical; should the final output be transposed?
#' @param aggregate.period Character; can be "month", "quarter", "year".
#' Only relevant when x is a vector of mode date.
#' @param y.labels Character vector, overrides chart matrix row names.
#' @param y.data Integer vector, optiona, for manually specifying the points
#' along the y-axis where the y.labels should appear.
#' @param x.labels Character vector, overrides chart matrix column names.
#' @param x.data Integer vector, optional, for manually specifying the points
#' along the x-axis where the x.labels should appear.
#' @param title Character; chart title.
#' @param title.font.family Character; title font family.  Can be "Arial
#' Black", "Arial", "Comic Sans MS", "Courier New", "Georgia", "Impact",
#' "Lucida Console", "Lucida Sans Unicode", "Marlett", "Symbol", "Tahoma",
#' "Times New Roman", "Trebuchet MS", "Verdana", "Webdings"
#' @param title.font.color Title font color as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param title.font.size Title font size; default = 10.
#' @param colors Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' @param colors.reverse Logical; if the order of the colors should be reversed.
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
#' @param legend.ascending Logical; TRUE for ascending, FALSE for descending
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
#' @param y.title Character, y-axis title; defaults to chart input values;
#' to turn off set to "FALSE".
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
#' @param y.data.reversed Logical; whether to reverse y-axis or not
#' @param y.grid.width Integer; width of y-grid lines in pixels; 0 = no line
#' @param y.grid.color Color of y-grid lines as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param y.tick.suffix Y-axis tick label suffix
#' @param y.tick.prefix Y-axis tick label prefix
#' @param y.tick.decimals Y-axis tick label decimal places
#' @param y.tick.format.manual Overrides tick.prefix, suffix and decimals;
#' See https://github.com/mbostock/d3/wiki/Formatting#numbers or
#' https://docs.python.org/release/3.1.3/library/string.html#formatspec
#' @param y.hovertext.decimals Y-axis hover text decimal places
#' @param y.hovertext.format.manual Overrides hovertext.prefix, suffix and decimals;
#' See https://github.com/mbostock/d3/wiki/Formatting#numbers or
#' https://docs.python.org/release/3.1.3/library/string.html#formatspec
#' @param y.tick.angle Integer, y-axis tick label angle in degrees.
#' 90 = vertical; 0 = horizontal
#' @param y.tick.font.color Y-axis tick label font color as a named color
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
#' @param x.data.reversed Logical; whether to reverse x-axis or not
#' @param x.grid.width Integer; width of y-grid lines in pixels; 0 = no line
#' @param x.grid.color Color of y-grid lines as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param x.tick.suffix X.axis tick label suffix
#' @param x.tick.prefix X.axis tick label prefix
#' @param x.tick.decimals X.axis tick label decimal places
#' @param x.tick.format.manual Overrides tick.prefix, suffix and decimals;
#' See https://github.com/mbostock/d3/wiki/Formatting#numbers or
#' https://docs.px.hon.org/release/3.1.3/librarx.string.html#formatspec
#' @param x.hovertext.decimals X.axis hover text decimal places
#' @param x.hovertext.format.manual Overrides hovertext.prefix, suffix and decimals;
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
#' @param x.show.missing.data.markers Logical; whether to show markers
#' on the x axis when there is no corresponding data.
#' @param series.marker.show Can be "none", "automatic" or a vector referencing
#' the plotly symbol dictionary using either numerics or strings.
#' @param series.marker.color Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' @param series.marker.color.reverse Logical; if the order of the colors should
#' be reversed.
#' @param series.marker.transparency Integer; transparency for series markers
#' as an alpha value (0 to 1)
#' @param series.marker.size Integer; size in pixels of marker
#' @param series.marker.border.width Integer; width in pixels of border/line
#' around series markers; 0 is no line
#' @param series.marker.border.color Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' @param series.marker.border.color.reverse Logical; if the order of the colors
#' should be reversed.
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
#' @param series.line.color  Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' @param series.line.color.reverse Logical; if the order of the colors
#' should be reversed.
#' @param series.line.transparency Integer; transparency for series lines as an
#' alpha value (0 to 1)
#' @param hover.mode Character or logic; can be FALSE for no hover text, "x" to
#' show all x-values for the hover point, "y" to show all y-values for the
#' hover point, or "closest" to show the single, nearest, value.
#' @param hover.include.source.data Logical; Include source data point value
#' in the hover text.
#' @param hover.include.source.data.prefix Character; prefix for source data
#' point value in hover text.
#' @param hover.include.source.data.suffix Character; suffix for source data
#' point value in hover text.
#' @param hover.include.source.data.percent Logical; multiplies source data
#' point value by 100.
#' @param show.modebar Logical; whether to show the zoom menu buttons or not.
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
#' exclude from the charting.  Does not apply to Labeled Scatterplot or Labeled
#' Bubbleplot, which both need to have the correct columns prior to charting.
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
#' @param pie.groups.colors  Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' @param pie.groups.colors.reverse Logical; if the order of the colors
#' should be reversed.
#' @param pie.data.font.family Character; font family for label values.
#' @param pie.data.font.size Numeric; font size of label values.
#' @param pie.data.prefix Character; prefix for label values.
#' @param pie.data.suffix Character; suffix for label values.
#' @param pie.data.display.format Character; either "\%" or "original";
#' yields a percentage or the source values as specified.
#' @param pie.data.thres.percent Numeric; 0-1, the percentage value at
#' which labels should not be shown.  Default 0.3\%.
#' @param pie.data.order Character; "descending", "initial", or
#' "alphabetical"; default is "descending" sort on values; "alphabetical"
#' sorts on labels.
#' @param pie.data.decimals Numeric; number of decimal points to show;
#' defaults to zero.
#' @param pie.labels.font.family Character; font family for label text.
#' @param pie.labels.font.size Numeric; font size for label text
#' @param pie.labels.font.color A single color for label text.
#' @param pie.labels.minFontSize Numeric; minimum font size of label text.
#' @param pie.labels.inner Logical; if labels should be on top of (TRUE)
#' or next to (FALSE) the chart.
#' @param pie.groups.font.family Character; font family for group labels
#' @param pie.groups.font.size Numeric; font size of group label text
#' @param pie.groups.font.color A single color for group label text.
#' @param pie.groups.minFontSize Numeric; minimum font size of group label
#' text.
#' @param pie.groups.order Character; "descending", "initial", or
#' "alphabetical"; default is "descending" sort on values; "alphabetical"
#' sorts on labels.
#' @param pie.segment.colors.repeat.by.group Logical; if, when a grouped
#' pie chart is displayed, the colors of the segments should repeat
#' by group, or be different throughout; defaults to TRUE.
#' @param pie.border.color A single color for space around pie and between
#' segments.
#' @param pie.segment.color.gradient Logical; if no pie.segment.colors are
#' specified, a gradient will be generated.
#' @param pie.groups.radius Numeric; must be passed as a proportion out of
#' 100 (e.g. 75); specifies the radius of the groups where the data are 2D;
#' only relevant to pit charts; defaults to 70.
#' @param donut.hole.radius Numeric; must be passed as a proportion out of
#' 100 (e.g. 75); specifies the radius of the donut hole; only relevant to
#' donut charts; defaults to 70.
#' @param bubble.legend.title Character; title of the bubble-size legend.
#' @param bubble.decimals Numeric; number of decimal points to show for
#' bubble-size data values.
#' @param bubble.label.prefix Character; label prefix to bubble values.
#' @param scatter.group.labels Character vector; overrides any automatically
#' generated series/group labels.
#' @param scatter.data.label.show Logical; whether to show labels in the
#' plot or not.
#' @param scatter.marker.radius Numeric; the radius of the scatter plot
#' markers if no bubble size has been provided.
#' @param scatter.labels.font.family Character; font to use for labels in
#' in plot.
#' @param scatter.labels.font.size Numeric; font size for plot labels.
#' @param scatter.labels.font.color Color for plot labels.
#' @examples
#' data("z")
#' z <- cbind(z, z[,1])
#' colnames(z) <- LETTERS[1:2]
#' rownames(z) <- LETTERS[1:5]
#' Chart(y = z, type = "Area", transpose = TRUE)
#' @export
Chart <-   function(y,
                        # x = NULL,
                        # weights = NULL,                                 ## Gets passed to AsChartMatrix <- add to that function first!
                        # subset = NULL,                                    ## Gets passed to AsChartMatrix <- add to that function first!
                        type = "Area",
                        transpose = FALSE,                                ## Should the inputs be transposed; TRUE or FALSE
                        aggregate.period = "none",
                        y.labels = NULL,
                        y.data = NULL,
                        x.labels = NULL,
                        x.data = NULL,
                        title = "",
                        title.font.family = "Arial",
                        title.font.color = rgb(44, 44, 44, maxColorValue = 255),
                        title.font.size = 16,
                        colors = qColors,
                        colors.reverse = FALSE,
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
                        legend.ascending = TRUE,
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
                        y.data.reversed = FALSE,                       ## T/F - involves autorange and may be too complicated.
                        y.grid.width = 1,
                        y.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                        y.tick.suffix = "",
                        y.tick.prefix = "",
                        y.tick.decimals = NULL,
                        y.tick.format.manual = "",
                        # y.hovertext.suffix = NULL,
                        # y.hovertext.prefix = NULL,
                        y.hovertext.decimals = 2,
                        y.hovertext.format.manual = "",
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
                        x.data.reversed = FALSE,                       ## T/F - involves autorange and may be too complicated.
                        x.grid.width = 1,
                        x.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                        x.tick.suffix = "",
                        x.tick.prefix = "",
                        x.tick.decimals = NULL,
                        x.tick.format.manual = "",
                        # x.hovertext.suffix = NULL,
                        # x.hovertext.prefix = NULL,
                        x.hovertext.decimals = 5,
                        x.hovertext.format.manual = "",
                        x.tick.angle = 0,
                        x.tick.font.color = rgb(0, 0, 0, maxColorValue = 255),
                        x.tick.font.family = "Arial",
                        x.tick.font.size = 10,
                        x.tick.label.autoformat = TRUE,
                        x.show.missing.data.markers = TRUE,
                        series.marker.show = "none",
                        series.marker.color = NULL,
                        series.marker.color.reverse = FALSE,
                        series.marker.transparency = 1,
                        series.marker.size = 6,
                        series.marker.border.width = 1,
                        series.marker.border.color = NULL,
                        series.marker.border.color.reverse = FALSE,
                        series.marker.border.transparency = 1,
                        series.marker.text = FALSE,
                        series.marker.text.position = "top middle",
                        series.marker.text.color = rgb(0, 0, 0, maxColorValue = 255),
                        series.marker.text.family = "Arial",
                        series.marker.text.size = 10,
                        series.marker.text.percent = FALSE,
                        series.line.width = 0,
                        series.line.color = qColors,
                        series.line.color.reverse = FALSE,
                        series.line.transparency = 1,
                        hover.mode = "closest",
                        hover.include.source.data = FALSE,
                        hover.include.source.data.prefix = "",
                        hover.include.source.data.suffix = "",
                        hover.include.source.data.percent = FALSE,
                        show.modebar = FALSE,
                        # subtitle.text = NULL,
                        # subtitle.align = "left",
                        # subtitle.border.width = 0,
                        # subtitle.border.color = "white",
                        # subtitle.background.color = "white",
                        # subtitle.font.family = "Arial",
                        # subtitle.font.color = rgb(0, 0, 0, maxColorValue=255),
                        # subtitle.font.size = 10,
                        global.font.family.override = "",
                        global.font.color.override = rgb(0, 0, 0, maxColorValue=255),
                        rows.to.ignore = "Total, NET, SUM",
                        cols.to.ignore = "Total, NET, SUM",
                        bar.gap = 0.15,
                        bar.group.gap = NULL,
                        bar.data.label.offset = NULL,
                        bar.data.label.family = "Arial",
                        bar.data.label.size = 10,
                        bar.data.label.color = rgb(0, 0, 0, maxColorValue=255),
                        bar.data.label.decimals = 0,
                        bar.data.label.as.percent = FALSE,
                        pie.data.font.family = "Arial",
                        pie.data.font.size = 10,
                        pie.data.prefix = "",
                        pie.data.suffix = "",
                        pie.data.display.format = "",
                        pie.data.thres.percent = 0.3,
                        pie.data.order = "initial",
                        pie.data.decimals = 0,
                        pie.labels.font.family = "Arial",
                        pie.labels.font.size = 10,
                        pie.labels.font.color = rgb(44, 44, 44, maxColorValue = 255),
                        pie.labels.minFontSize = 8,
                        pie.labels.inner = FALSE,
                        pie.groups.font.family = "Arial",
                        pie.groups.font.size = 10,
                        pie.groups.font.color = rgb(44, 44, 44, maxColorValue = 255),
                        pie.groups.minFontSize = 8,
                        pie.groups.colors = NULL,
                        pie.groups.colors.reverse = FALSE,
                        pie.groups.order = "descending",
                        pie.groups.radius = 60,
                        pie.segment.colors.repeat.by.group = TRUE,
                        pie.border.color = rgb(255, 255, 255, maxColorValue = 255),
                        pie.segment.color.gradient = FALSE,
                        donut.hole.radius = 0,
                        bubble.legend.title = NULL,
                        bubble.decimals = 0,
                        bubble.label.prefix = "",
                        scatter.group.labels = NULL,
                        scatter.data.label.show = TRUE,
                        scatter.marker.radius = 3,
                        scatter.labels.font.family = "Arial",
                        scatter.labels.font.size = 10,
                        scatter.labels.font.color = rgb(44, 44, 44, maxColorValue = 255)
                    )
{
    chart.matrix <- y

    ## Check decimal input
    if (!(type %in% c("Labeled Scatterplot", "Labeled Bubbleplot")))
    {
        if (is.null(x.tick.decimals))
            x.tick.decimals <- 0

        if (is.null(y.tick.decimals))
            y.tick.decimals <- 2
    }

    ## If the input is 3D, then error
    if (length(dim(chart.matrix)) > 2 && is.null(attr(chart.matrix, "statistic")))
        stop("The input consists of more than one table, or has multiple statistics.  Please include only one table and/or statistic.")

    ## Is it a Q input?
    qinput <- FALSE
    if (!is.null(attr(chart.matrix, "statistic")))
        qinput <- TRUE

    table.axes.labels <- c("", "")

    if (qinput)
    {
        table.axes.labels <- attr(chart.matrix, "questions")
        table.name <- attr(chart.matrix, "name")
        table.statistic <- attr(chart.matrix, "statistic")
    } else {
        if (length(names(dimnames(chart.matrix))) == 2)
            table.axes.labels <- names(dimnames(chart.matrix))

        table.statistic <- ""
    }

    ## If it's an array with an attribute of names, then assign those names to the rows
    if (is.array(chart.matrix) && length(names(chart.matrix)) != 0)
        rownames(chart.matrix) <- names(chart.matrix)

    if (!(type %in% c("Labeled Scatterplot", "Labeled Bubbleplot")))
    {
        ## If it's a one column entity, make sure it's a matrix and that it's got a column heading.
        has.statistic <- "statistic" %in% names(attributes(chart.matrix))
        if (is.array(chart.matrix))
        {
            chart.matrix <- as.matrix(chart.matrix)
            if (ncol(chart.matrix) == 1 && has.statistic)
            {
                colnames(chart.matrix) <- table.statistic
            } else if (ncol(chart.matrix) == 1 && !has.statistic) {
                colnames(chart.matrix) <- c("n")
            }
        }

        original.row.count <- nrow(chart.matrix)
        original.col.count <- ncol(chart.matrix)

        ## Ignore rows or columns, using flipData::GetTidyTwoDimensionalArray()
        chart.matrix <- flipData::GetTidyTwoDimensionalArray(chart.matrix, rows.to.ignore, cols.to.ignore)

        ## Check if the input is labelled
        if (is.null(rownames(chart.matrix)) || is.null(colnames(chart.matrix)))
            stop("The input lacks row and/or column labels")

        ## Issue warning if any rows or columns have been removed
        if (original.row.count != nrow(chart.matrix))
            warning(paste("Any rows labelled", rows.to.ignore, "have been removed from the chart."), sep = "")

        if (original.col.count != ncol(chart.matrix))
            warning(paste("Any columns labelled", cols.to.ignore, "have been removed from the chart."), sep = "")

        ## Check if the input is a 2D object
        if (length(dim(chart.matrix)) != 2)
            stop("The input needs to be a 2 dimensional object (table or matrix)")

        ## Make sure it's not a character matrix
        if (is.character(chart.matrix))
            stop("The input must be numeric")

        ## If it's a data frame with only numerics, make it a matrix
        if (sum(sapply(chart.matrix, is.numeric)) == ncol(chart.matrix))
            chart.matrix <- as.matrix(chart.matrix)

        ## Can only take items of class matrix or table.
        if (!is.matrix(chart.matrix) && !is.table(chart.matrix))
            stop("The input needs to be either a matrix, a table, or a data frame consisting entirely of numerics")

        ## Transform chart.matrix based on transposition requirements.
        if (ncol(chart.matrix) == 1)
        {
            chart.matrix <- t(chart.matrix)
            table.axes.labels <- rev(table.axes.labels)
        }

        if (transpose)
        {
            chart.matrix <- t(chart.matrix)
            table.axes.labels <- rev(table.axes.labels)
        }

        ## If no x.title or y.title provided, take defaults from data input
        if (x.title == "" || length(x.title) == 0)
            x.title <- table.axes.labels[2]

        if (x.title == "FALSE" || x.title == FALSE)
            x.title <- ""

        if (y.title == "" || length(y.title) == 0)
            y.title <- table.statistic

        if (y.title == "FALSE" || y.title == FALSE)
            y.title <- ""

        if (type %in% c("bar", "Bar", "Stacked Bar", "100% Stacked Bar"))
        {
            x.temp <- y.title
            y.temp <- x.title
            x.title <- x.temp
            y.title <- y.temp
        }
    }

    ## Store chart type for later use
    original.type <- type



    # if (y.title == "" && !qinput)
    #     y.title <- table.axes.labels[2]

    ## Make a chart matrix
    # if (type != "Scatter Plot" || (type == "Scatter Plot" && !is.null(x)))
    #     chart.matrix <- AsChartMatrix(y, x, transpose = transpose, aggregate.period = aggregate.period, subset = subset, weights = weights)
    # else
    #     chart.matrix <- y

    ## Make sure that the provided data will work with the desired chart type
    checkDataForChartType(chart.matrix = chart.matrix, type = type)

    ## Only allow a single factor variable if it's a pie-chart; chart-specific test not appropriate for AsChartMatrix, so
    ## included here rather than in that function.
    # if (is.factor(y) && is.null(x) && type != "Pie")
    #     warning(paste("The data selected is not best displayed as an", type, "chart.  Consider changing the chart type. (Old message: Y must be either a vector, matrix, or table.  Currently it is: ", class(y), ")"))

    ## Ignore rows or columns
    # if ((rows.to.ignore != "" | cols.to.ignore != "") && !(type %in% c("Labeled Scatterplot", "Labeled Bubbleplot")))
    #     chart.matrix <- removeRowsAndColumns(chart.matrix, rows.to.ignore, cols.to.ignore)




    ## Set defaults for chart specific items
    fill.bound <- ""
    legend.group <- ""
    barmode <- ""
    orientation <- NULL
    swap.axes.and.data <- FALSE
    y.nticks <- NULL
    y.tickformat <- NULL
    connectgap <- NULL


    ## Settings specific to Area Charts
    if (type == "Area" | type == "Stacked Area" | type == "100% Stacked Area")
    {
        no.data.on.row <- rowSums(is.na(chart.matrix)) >= length(chart.matrix[1, ]) - 1

        if (any(no.data.on.row))
        {
            warning("Some of your series contain either no data or only one data point.  These will be removed from the chart.")
            chart.matrix <- chart.matrix[!no.data.on.row, ]
        }

        if (any(is.nan(as.matrix(chart.matrix))))
            warning("Your data contains NaN values; data points in gaps will be interpolated.")

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
        connectgap <- TRUE
    }

    ## Settings specific to Scatter Plot Charts
    if (type == "Scatterplot")
    {
        if (any(is.nan(as.matrix(chart.matrix))))
        {
            warning("Your data contains NaN values which will not appear in the chart.")

            chart.matrix <- chart.matrix[!is.nan(rowSums(chart.matrix)), ]
        }

        chart.type.outputs <- scatterPlotChart(chart.matrix = chart.matrix,
                                                transpose = transpose,
                                                series.marker.text = series.marker.text,
                                                x.tick.frequency = x.tick.frequency,
                                                x.tick.decimals = x.tick.decimals,
                                                x.labels = x.labels,
                                                x.bounds.minimum = x.bounds.minimum,
                                                x.bounds.maximum = x.bounds.maximum,
                                                x.bounds.units.major = x.bounds.units.major)

        chart.matrix <- chart.type.outputs$chart.matrix
        series.mode <- chart.type.outputs$series.mode
        y.tickformat <- ""
        transpose <- chart.type.outputs$transpose
        x.tick.frequency <- chart.type.outputs$x.tick.frequency
        x.tick.decimals <- chart.type.outputs$x.tick.decimals
        x.bounds.minimum <- chart.type.outputs$x.bounds.minimum
        x.bounds.maximum <- chart.type.outputs$x.bounds.maximum
        x.bounds.units.major <- chart.type.outputs$x.bounds.units.major
    }

    ## Settings specific to Column Charts
    if (type == "Column" | type == "Stacked Column" | type == "100% Stacked Column")
    {
        if (any(is.nan(as.matrix(chart.matrix))))
        {
            warning("Your data contains NaN values which have been set to zero.")
            chart.matrix[which(is.nan(chart.matrix))] <- 0
        }


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
        transparency <- 1
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
                                          y.nticks = length(colnames(chart.matrix)),
                                          x.tick.format.manual = x.tick.format.manual,
                                          x.tick.frequency = x.tick.frequency
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
        x.tick.format.manual <- chart.type.outputs$x.tick.format.manual
        x.tick.frequency <- chart.type.outputs$x.tick.frequency
        transparency <- 1
    }


    ## Waterfall (part of column charts, really...)

    ## Radar/Polar plot

    ## Heat map

    ## ... Any other chart types...


    ## Color inheritance - first run
    if (is.null(series.marker.color))
        series.marker.color <- colors

    if (is.null(series.marker.border.color))
        series.marker.border.color <- series.marker.color

    if (is.null(series.line.color))
        series.line.color <- colors

    if (is.null(pie.groups.colors))
        pie.groups.colors <- colors


    # Settings specific to Pie charts
    if (type == "Pie" || type == "Donut")
    {
        # Set any NaN to 0 so that it won't chart.
        chart.matrix <- as.matrix(chart.matrix)

        if (any(is.nan(chart.matrix)))
            warning("Your data contains NaN values which will not appear in the chart.")

        chart.matrix[is.nan(chart.matrix)] <- 0

        pie <- pieChart(chart.matrix = chart.matrix,
                transpose = transpose,
                type = type,
                values.color = colors,
                colors.reverse = colors.reverse,
                pie.values.font.family = pie.data.font.family,
                pie.values.font.size = pie.data.font.size,
                pie.values.prefix = pie.data.prefix,
                pie.values.suffix = pie.data.suffix,
                pie.values.display.format = pie.data.display.format,
                pie.values.thres.percent = pie.data.thres.percent,
                pie.values.order = pie.data.order,
                pie.values.decimals = pie.data.decimals,
                pie.labels.font.family = pie.labels.font.family,
                pie.labels.font.size = pie.labels.font.size,
                pie.labels.font.color = pie.labels.font.color,
                pie.labels.minFontSize = pie.labels.minFontSize,
                pie.labels.inner = pie.labels.inner,
                pie.groups.font.family = pie.groups.font.family,
                pie.groups.font.size = pie.groups.font.size,
                pie.groups.font.color = pie.groups.font.color,
                pie.groups.minFontSize = pie.groups.minFontSize,
                pie.groups.colors = pie.groups.colors,
                pie.groups.colors.reverse = pie.groups.colors.reverse,
                pie.groups.order = pie.groups.order,
                pie.groups.radius = pie.groups.radius,
                pie.segment.colors.repeat.by.group = pie.segment.colors.repeat.by.group,
                pie.border.color = pie.border.color,
                pie.segment.color.gradient = pie.segment.color.gradient,
                donut.hole.radius = donut.hole.radius,
                table.statistic = table.statistic,
                qinput = qinput)

        return(rhtmlDonut::Donut(values = pie$values.data,
                 labels = pie$labels,
                 values.color = pie$pie.values.color,
                 values.order = pie$pie.values.order,
                 values.font.family = pie$pie.values.font.family,
                 values.font.size = pie$pie.values.font.size,
                 values.decimal.places = pie$pie.values.decimals,
                 values.display.as = pie$values.display,
                 values.display.thres = pie$pie.values.thres.percent,
                 labels.font.family = pie$pie.labels.font.family,
                 labels.font.color = pie$pie.labels.font.color,
                 labels.font.size = pie$pie.labels.font.size,
                 labels.min.font.size = pie$pie.labels.minFontSize,
                 labels.inner = pie$pie.labels.inner,
                 groups = pie$groups,
                 groups.color = pie$pie.groups.colors,
                 groups.order = pie$pie.groups.order,
                 groups.font.family = pie$pie.groups.font.family,
                 groups.font.color = pie$pie.groups.font.color,
                 groups.font.size = pie$pie.groups.font.size,
                 groups.min.font.size = pie$pie.groups.min.font.size,
                 title = title,
                 title.font.family = title.font.family,
                 title.font.size = title.font.size,
                 title.font.color = title.font.color,
                 prefix = pie$pie.values.prefix,
                 suffix = pie$pie.values.suffix,
                 border.color = pie$pie.border.color,
                 gradient = pie$pie.segment.color.gradient,
                 inner.radius = pie$inner.radius
                 )
        )
    }

    ## Settings specific to labelled scatter plots
    if (type == "Labeled Scatterplot" || type == "Labeled Bubbleplot")
    {
        draw.grid <- (x.grid.width != 0 && y.grid.width != 0)

        if (any(is.nan(as.matrix(chart.matrix))))
        {
            warning("Your data contains NaN values which will not appear in the chart.")
            chart.matrix <- chart.matrix[!is.nan(rowSums(chart.matrix)), ]
        }

        labeled.scatterplot <- labeledScatterplot(chart.matrix = chart.matrix,
                                                  colors = colors,
                                                  colors.reverse = colors.reverse,
                                                  type = type,
                                                  group = scatter.group.labels,
                                                  grid = draw.grid, # if x and y grid are both 0; else draw grid?
                                                  origin = FALSE, # base on y and x.zero.line.width
                                                  transpose = transpose,
                                                  qinput = qinput,
                                                  rows.to.ignore = rows.to.ignore,
                                                  cols.to.ignore = cols.to.ignore,
                                                  legend.show = legend.show,
                                                  x.title = x.title,
                                                  y.title = y.title
                                                  )

        return(rhtmlLabeledScatter::LabeledScatter(X = labeled.scatterplot$X,
                       Y = labeled.scatterplot$Y,
                       Z = labeled.scatterplot$Z,
                       label = labeled.scatterplot$label,
                       fixed.aspect = FALSE,
                       group = labeled.scatterplot$group,
                       grid = labeled.scatterplot$grid,
                       origin = labeled.scatterplot$origin,
                       origin.align = FALSE,
                       labels.show = TRUE,
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
                       x.title.font.family = y.title.font.family,
                       x.title.font.color = y.title.font.color,
                       x.title.font.size = y.title.font.size,
                       # x.axis.font.family = x.tick.font.family,
                       # x.axis.font.color = x.tick.font.color,
                       # X.axis.font.size = x.tick.font.size,
                       z.title = bubble.legend.title,
                       y.decimals = y.tick.decimals,
                       x.decimals = x.tick.decimals,
                       z.decimals = bubble.decimals,
                       x.prefix = x.tick.prefix,
                       y.prefix = y.tick.prefix,
                       z.prefix = bubble.label.prefix,
                       title.font.family = title.font.family,
                       title.font.color = title.font.color,
                       title.font.size = title.font.size,
                       labels.font.family = scatter.labels.font.family,
                       labels.font.color = scatter.labels.font.color,
                       labels.font.size = scatter.labels.font.size,
                       point.radius = scatter.marker.radius,
                       y.bounds.maximum = y.bounds.maximum,
                       y.bounds.minimum = y.bounds.minimum,
                       y.bounds.units.major = y.bounds.units.major,
                       x.bounds.maximum = x.bounds.maximum,
                       x.bounds.minimum = x.bounds.minimum,
                       x.bounds.units.major = x.bounds.units.major,
                       # tooltip.font.color = y.tick.font.family,
                       # tooltip.font.size = y.tick.font.size,
                       # tooltip.font.family = y.tick.font.family,
                       title = title
                       ))
    }


    ## Work out color ranges; n.b. some color ranges worked out in the chart specific functions.
    if (type %in% c("Column", "Stacked Column", "100% Stacked Column"))
        number.colors.needed <- ncol(chart.matrix)
    else
        number.colors.needed <- nrow(chart.matrix)

    ## Calculate colors
    colors <- flipChartBasics::ChartColors(number.colors.needed = number.colors.needed, given.colors = colors, reverse = colors.reverse)
    series.marker.color <- flipChartBasics::ChartColors(number.colors.needed = number.colors.needed, given.colors = series.marker.color, reverse = series.marker.color.reverse)
    series.marker.border.color <- flipChartBasics::ChartColors(number.colors.needed = number.colors.needed, given.colors = series.marker.border.color, reverse = series.marker.border.color.reverse)
    series.line.color <- flipChartBasics::ChartColors(number.colors.needed = number.colors.needed, given.colors = series.line.color, reverse = series.line.color.reverse)

    ## Color inheritance - second run
    if (is.null(series.marker.color))
        series.marker.color <- colors

    if (is.null(series.marker.border.color))
        series.marker.border.color <- series.marker.color

    if (is.null(series.line.color))
        series.line.color <- colors



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

    # Find maximum value in chart matrix; needed for subtitle positioning and setting y-axis limits
    y.max <- max(chart.matrix)
    if (!is.null(y.bounds.maximum))
        y.max <- y.bounds.maximum

    if (original.type == "Bar" || original.type == "Stacked Bar" || original.type == "100% Stacked Bar")
        y.max <- ncol(chart.matrix)

    if (original.type == "100% Stacked Column")
        y.max <- 1

    if (original.type == "Stacked Column")
        y.max <- max(apply(chart.matrix, 2, FUN = function(x) sum(x)))


    # Specify x.axis as categorical to prevent Plotly from automatically manipulating inputs.
    # if (type != "bar")
    # {
    #     x.axis.type = "category"
    # } else {
    #     x.axis.type = "linear"
    # }

    ## This should actually be linear if the x-axis data is numeric, and category if it's not.
    x.axis.type = "category"
    if (!any(is.character(colnames(chart.matrix))))
        x.axis.type = "linear"

    # Set subtitle defaults - this should be done in signature, but sutitle function removed due to formatting no longer available in Plotly 10 10 16
    subtitle.text <- NULL
    subtitle.align <- "left"
    subtitle.border.width <- 0
    subtitle.border.color <- "white"
    subtitle.background.color <- "white"
    subtitle.font.family <- "Arial"
    subtitle.font.color <- rgb(0, 0, 0, maxColorValue=255)
    subtitle.font.size <- 10

    # Sort out the sub-title
    subtitle <- list()

    if (!is.null(subtitle.text))
        warning("Subtitles are currently not formattable and should not be used.")

    if (!is.null(subtitle.text))
    {
        # Allow some extra margin space
        subtitle.text <- as.vector(subtitle.text)

        # Manually insert line-breaks every 61 characters
        # subtitle.text <- lineBreakEveryN(subtitle.text, n = 61)

        ## Attempt to determine y-position, which varies depending on chart type and chart data.
        if (subtitle.align == "left")
            x.position = 0
        else if (subtitle.align == "right")
            x.position = 1
        else
            x.position = 0.5

        sfont <- list(color = subtitle.font.color,
                      size = subtitle.font.size,
                      family = subtitle.font.family)

        subtitle <- list(y = y.max,
                         x = x.position,
                         text = subtitle.text,
                         xref = "paper",
                         yref = "y",
                         showarrow = FALSE,
                         borderwidth = subtitle.border.width,
                         bordercolor = subtitle.border.color,
                         bgcolor = subtitle.background.color,
                         font = sfont,
                         borderpad = 10,
                         yanchor = "bottom",
                         align = subtitle.align
        )
    }

    # Create text matrix of source data if required for hover
    source.matrix <- chart.matrix
    if (hover.include.source.data.percent | series.marker.text.percent)
        source.matrix <- source.matrix * 100

    if (hover.include.source.data.percent)
        source.matrix <- matrix(paste(hover.include.source.data.prefix, " ", source.matrix, hover.include.source.data.suffix, sep = ""), nrow = nrow(chart.matrix), ncol = ncol(chart.matrix))

    ## Get axes labels from the matrix labels if none manually specified
    if (is.null(x.labels))
        x.labels <- colnames(chart.matrix)

    if (is.null(y.labels))
        y.labels <- rownames(chart.matrix)

    ## If no angle set for x.tick.angle and x.labels are > 15 characters,
    tally <- as.numeric(sapply(x.labels, function(x) nchar(x)))

    if (max(tally) > 15 && x.tick.label.autoformat == TRUE)
    {
        x.tick.angle <- 315
        x.labels <- autoFormatLongLabels(x.labels)
        if (margin.bottom == 80)
            margin.bottom <- 140
    }
    else if (sum(tally) > 100 && x.tick.label.autoformat == TRUE)
        x.tick.angle <- 315

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

    ## If no line, then set to NULL to satisfy Plotly
    if (y.line.width == 0)
        y.line.width <- NULL

    if (y.line.width >= 1 && !is.null(y.line.width))
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

    ## If no line, then set to NULL to satisfy Plotly
    if (x.line.width == 0)
        x.line.width <- NULL

    if (x.line.width >= 1 && !is.null(x.line.width))
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

    ## Resolve tick prefix and suffixes
    # if (is.null(y.hovertext.format.prefix))
    #     y.hovertext.prefix <- y.tick.prefix
    #
    # if (is.null(y.hovertext.suffix))
    #     y.hovertext.suffix <- y.tick.suffix
    #
    # if (is.null(x.hovertext.prefix))
    #     x.hovertext.prefix <- x.tick.prefix
    #
    # if (is.null(x.hovertext.suffix))
    #     x.hovertext.suffix <- x.tick.suffix

    ## Set tick and hover formats

    if (y.tick.format.manual != "" && y.tick.format.manual != y.tickformat)
        y.tickformat <- y.tick.format.manual

    ifelse((y.tick.format.manual == "" && (is.null(y.tickformat) || y.tickformat == "")), y.tickformat <- paste(".", y.tick.decimals, "f", sep=""), FALSE) #y.tickformat <- y.tick.format.manual)

    ifelse(x.tick.format.manual == "", x.tickformat <- paste(".", x.tick.decimals, "f", sep=""), x.tickformat <- x.tick.format.manual)

    ifelse(y.hovertext.format.manual == "", y.hoverformat <- paste(".", y.hovertext.decimals, "f", sep=""), y.hoverformat <- y.hovertext.format.manual)

    ifelse(x.hovertext.format.manual == "", x.hoverformat <- paste(".", x.hovertext.decimals, "f", sep=""), x.hoverformat <- x.hovertext.format.manual)

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

        y.tickvals <- seq(y.bounds.minimum, y.bounds.maximum, by = y.bounds.units.major)

        if (y.tickformat == "%")
            y.ticktext <- sapply(y.tickvals, function(x) paste(round(x * 100, 0), "%", sep = ""))
        else if (y.tickformat == "$")
            y.ticktext <- sapply(y.tickvals, function(x) ifelse(x < 0, paste("-$", -1 * round(x, y.tick.decimals), sep = ""), paste("$", round(x, y.tick.decimals), sep = "")))
        else
            y.ticktext <- sapply(y.tickvals, function(x) paste(round(x, y.tick.decimals)))
    }
    else if (!is.null(y.data) && !is.null(y.labels))
    {
        y.tickmode <- "array"
        y.tickvals <- y.data
        y.ticktext <- y.labels
    }
    else if (!is.null(subtitle.text))
    {
        y.tickmode <- "array"
        y.tickvals <- seq(0, y.max, y.max / 5)
        y.ticktext <- seq(0, y.max, y.max / 5)
        y.autorange <- TRUE
        # if (!is.null(y.number.ticks))
        #     y.nticks <- y.number.ticks
    } else {
        y.tickmode <- "auto"
        y.autorange <- TRUE
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

        x.tickvals <- seq(x.bounds.minimum, x.bounds.maximum, by = x.bounds.units.major)
        if (!swap.axes.and.data)
            x.ticktext <- as.character(x.tickvals)

        if (x.tickformat == "%")
            x.ticktext <- sapply(x.tickvals, function(x) paste(round(x * 100, 0), "%", sep = ""))
        else if (x.tickformat == "$")
            x.ticktext <- sapply(x.tickvals, function(x) ifelse(x < 0, paste("-$", -1 * round(x, x.tick.decimals), sep = ""), paste("$", round(x, x.tick.decimals), sep = "")))
        else
            x.ticktext <- sapply(x.tickvals, function(x) paste(round(x, x.tick.decimals)))
    }
    else if (!is.null(x.data) && !is.null(x.labels))
    {
        x.tickmode <- "array"
        # x.autorange <- TRUE
        x.tickvals <- x.data
        x.ticktext <- x.labels
        # x.range <- c(x.data[1], x.data[ncol(chart.matrix)])
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
                # x.tickmode <- "array"
                # x.autorange <- TRUE
                # x.tickvals <- 1:length(x.labels)
                # x.ticktext <- x.labels
                #
                # print(x.tickvals)
                # print(x.ticktext)
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
    if (y.data.reversed == TRUE)
        y.autorange = "reversed"

    if (x.data.reversed == TRUE)
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

    ###########################################
    show.series.name <- "+name"
    if (nrow(chart.matrix) == 1)
        show.series.name <- ""
    ###########################################

    if (hover.include.source.data)
        hoverinfo = paste(axis.to.show, show.series.name, "+text", sep = "")
    else
        hoverinfo = paste(axis.to.show, show.series.name, sep = "")

    ## Build annotations list
    if (!is.null(subtitle.text) && length(data.annotations) >= 1)
        data.annotations[[length(data.annotations) + 1]] <- subtitle
    # else
    #     data.annotations <- subtitle

    ## Hide legend if only one series to plot
    if ((type != "Bar" & type != "Stacked Bar" & type != "100% Stacked Bar") && nrow(chart.matrix) == 1)
        legend.show <- FALSE

    if ((type == "Bar" | type == "Stacked Bar" | type == "100% Stacked Bar") && ncol(chart.matrix) == 1)
        legend.show <- FALSE

    ## Initiate plotly object
    p <- plotly::plot_ly(as.data.frame(chart.matrix))

    ## Config options
    p <- plotly::config(p, displayModeBar = show.modebar)

    ## Make sure x.labels are actually used
    colnames(chart.matrix) <- x.labels

    ## Convert type to plotly type
    if (type %in% c("bar", "Bar", "Stacked Bar", "100% Stacked Bar", "Column", "Stacked Column", "100% Stacked Column"))
        type <- "bar"
    else
        type <- "scatter"

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

        ## Whether to show missing data markers on the x-axis or not
        x.categoryorder = "array"
        x.categoryarray = x
        if (!x.show.missing.data.markers)
        {
            x.categoryorder = "trace"
            x.categoryarray = NULL
        }

        source.text <- source.matrix[a, ]

        ## Add trace components
        if (regexpr('lines', series.mode) >= 1 && !is.null(series.mode))
        {
            lines = list(width = series.line.width,
                         color = plotly::toRGB(series.line.color[a], alpha = series.line.transparency)
                         )
        } else {
            lines <- NULL
        }

        if (regexpr('text', series.mode) >= 1 && !is.null(series.mode))
        {
            textfont <- list(family = series.marker.text.family,
                             color = plotly::toRGB(series.marker.text.color, alpha = 1),
                             size = series.marker.text.size
                             )
        } else {
            textfont <- NULL
        }

        if (regexpr('marker', series.mode) >= 1 && !is.null(series.mode))
        {
            marker = list(size = series.marker.size,
                          color = plotly::toRGB(series.marker.color[a], alpha = series.marker.transparency),
                          symbol = series.marker.symbols[a],
                          line = list(
                              color = plotly::toRGB(series.marker.border.color[a], alpha = series.marker.border.transparency),
                              width = series.marker.border.width
                              )
                          )
        } else if (type == "bar") {
            marker = list(size = series.marker.size,
                          color = plotly::toRGB(colors[a], alpha = transparency),
                          line = list(
                              color = plotly::toRGB(series.marker.border.color[a], alpha = series.marker.border.transparency),
                              width = series.marker.border.width
                              )
                          )

        } else {
             marker <- NULL
        }

        if (type != "bar")
        {
            p <- plotly::add_trace(p,
                                   type = type,
                                   x = x,
                                   y = y,
                                   # evaluate = TRUE,
                                   orientation = orientation,
                                   fill = fill.bound,
                                   fillcolor = plotly::toRGB(colors[a], alpha = transparency),
                                   line = lines,
                                   name = y.labels[a],
                                   legendgroup = legend.group,
                                   text = source.text,
                                   textposition = series.marker.text.position,
                                   # MARKERS
                                   mode = series.mode,
                                   marker = marker,
                                   hoverinfo = hoverinfo
            )
        } else {
            p <- plotly::add_trace(p,
                                   type = type,
                                   x = x,
                                   y = y,
                                   # evaluate = TRUE,
                                   orientation = orientation,
                                   line = lines,
                                   name = y.labels[a],
                                   legendgroup = legend.group,
                                   text = source.text,
                                   # MARKERS
                                   # mode = series.mode,
                                   marker = marker,
                                   hoverinfo = hoverinfo
            )
        }
    }

    ## Set plotly layout styles
    p <- plotly::layout(p,
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
            showticksuffix = TRUE,
            categoryorder = x.categoryorder,
            categoryarray = x.categoryarray
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
        # bargroupgap = bar.group.gap,  ## Apparently deprecated; bargap fills same function.
        barmode = barmode
    )

    ## Return the chart
    return(p)
}

