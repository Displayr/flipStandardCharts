#' Generates an interactive line chart based on the plotly library.
#'
#' \code{LineChart} generates an line chart.
#'
#' @param y A vector, matrix, list of vectors, data frame, or table.
#' @param x A vector over which y will be aggregated. Must have the same
#' number of elements as y.
#' @param transpose Logical; should the final output be transposed?
#' @param aggregate.period Character; can be "month", "quarter", "year".
#' Only relevant when x is a vector of mode date.
#' @param y.labels Character vector, overrides chart matrix row names.
#' @param x.labels Character vector, overrides chart matrix column names.
#' @param type Character; type of chart. Can be "Line"
#' @param title Character; chart title.
#' @param global.font.family Character; global font family.  Can be "Arial
#' Black", "Arial", "Comic Sans MS", "Courier New", "Georgia", "Impact",
#' "Lucida Console", "Lucida Sans Unicode", "Marlett", "Symbol", "Tahoma",
#' "Times New Roman", "Trebuchet MS", "Verdana", "Webdings"
#' @param global.font.color Global font color as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, max = 255)).
#' @param global.font.size Global font size; default = 10.
#' @param colors Vector of colors in RGB format.
#' @param transparency Transparency of area fill colors as an alpha value
#' (0 to 1).
#' @param chart.fill.color Chart (borders around plot) background color as
#' a named color in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, max = 255)).
#' @param chart.fill.transparency Chart (borders around plot) background
#' transparency as an alpha value (0 to 1).
#' @param plot.fill.color Plot (the plot area proper) background color as
#' a named color in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, max = 255)).
#' @param plot.fill.transparency Plot (the plot area proper) background
#' transparency as an alpha value (0 to 1).
#' @param legend.show Logical; show the legend.
#' @param legend.fill Legend fill color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, max = 255)).
#' @param legend.border.color Legend border color as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, max = 255)).
#' @param legend.border.line.width Integer; width in pixels of the border
#' around the legend.  0 = no border.
#' @param legend.font.color Legend font color as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, max = 255)).
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
#' (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, max = 255)).
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
#' rgb(0, 0, 0, max = 255)).
#' @param y.position Character; set y-axis position; can be "left" or "right"
#' @param y.mirror Logical; mirror y-axis on other side?
#' @param y.grid.width Integer; width of y-grid lines in pixels; 0 = no line
#' @param y.grid.color Color of y-grid lines as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, max = 255)).
#' @param y.ticksuffix Character; y-axis tick label suffix.
#' @param y.tickprefix Character; y-axis tick label prefix
#' @param y.tick.angle Integer, y-axis tick label angle in degrees.
#' 90 = vertical; 0 = horizontal
#' @param y.tick.format.manual Character; specify the tick format manually
#' in accordance with \url{https://github.com/mbostock/d3/wiki/Formatting#numbers}
#' or \url{https://docs.python.org/release/3.1.3/library/string.html#formatspec}
#' @param y.tick.format A vector of two character strings where the first
#' one represents a prefix and the second represents the total number of
#' digits to show (including decimals).  E.g. c("n","3").  The formats can
#' be:  "n" (number), "\%", "$", "£".
#' @param y.tick.font.color Y-axis tick label font color as a named color
#' in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, max = 255)).
#' @param y.tick.font.family Character; y-axis tick label font family
#' @param y.tick.font.size Integer; y-axis tick label font size
#' @param y.hover.format.manual Character; specify the hover format manually
#' in accordance with \url{https://github.com/mbostock/d3/wiki/Formatting#numbers}
#' or \url{https://docs.python.org/release/3.1.3/library/string.html#formatspec}
#' @param y.hover.format  A vector of two character strings where the first
#' one represents a prefix and the second represents the total number of
#' digits to show (including decimals).  E.g. c("n","3").  The formats can be:
#' "n" (number), "\%", "$", "£".
#' @param x.title Character, x-axis title
#' @param x.title.font.color x-axis title font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, max = 255)).
#' @param x.title.font.family Character; x-axis title font family
#' @param x.title.font.size Integer; x-axis title font size
#' @param x.line.width Integer; x-axis line in pixels, 0 = no line
#' @param x.line.color X-axis line color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, max = 255)).
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
#' @param x.zero.line.width Width in pixels of zero line; 0 = no zero line
#' shown
#' @param x.zero.line.color Color of horizontal zero (origo) line as a named
#' color in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, max = 255)).
#' @param x.position Character; set x-axis position; can be "left" or "right"
#' @param x.mirror Logical; mirror x-axis on other side?
#' @param x.grid.width Integer; width of y-grid lines in pixels; 0 = no line
#' @param x.grid.color Color of y-grid lines as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, max = 255)).
#' @param x.ticksuffix Character; x-axis tick label suffix.
#' @param x.tickprefix Character; x-axis tick label prefix
#' @param x.tick.angle Integer, x-axis tick label angle in degrees.
#' 90 = vertical; 0 = horizontal
#' @param x.tick.format.manual Character; specify the tick format manually
#' in accordance with \url{https://github.com/mbostock/d3/wiki/Formatting#numbers}
#' or \url{https://docs.python.org/release/3.1.3/library/string.html#formatspec}
#' @param x.tick.format A vector of two character strings where the first one
#' represents a prefix and the second represents the total number of digits to
#' show (including decimals).  E.g. c("n","3").  The formats can be:
#' "n" (number), "\%", "$", "£".
#' @param x.tick.font.color X-axis tick label font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, max = 255)).
#' @param x.tick.font.family Character; x-axis tick label font family
#' @param x.tick.font.size Integer; x-axis tick label font size
#' @param x.hover.format.manual Character; specify the hover format manually
#' in accordance with \url{https://github.com/mbostock/d3/wiki/Formatting#numbers}
#' or \url{https://docs.python.org/release/3.1.3/library/string.html#formatspec}
#' @param x.hover.format  A vector of two character strings where the first
#' one represents a prefix and the second represents the total number of
#' digits to show (including decimals).  E.g. c("n","3").  The formats can be:
#' "n" (number), "\%", "$", "£".
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
#' @param series.line.width Integer; thickness, in pixels, of the series line
#' @param series.line.color Vector of colors in RGB format for series the lines
#' @param series.line.transparency Integer; transparency for series lines as an
#' alpha value (0 to 1)
#' @param hover.mode Character or logic; can be FALSE for no hover text, "x" to
#' show all x-values for the hover point, "y" to show all y-values for the
#' hover point, or "closest" to show the single, nearest, value.
#' @param series.marker.text Logical; show data point text with or as series
#' marker.
#' @param series.marker.text.font Character; font of series marker text data
#' point.
#' @param series.marker.text.color Color of series marker text data
#' point as a named color in character format (e.g. "black") or an rgb value
#' (e.g. rgb(0, 0, 0, max = 255)).
#' @param series.marker.text.size Integer; series marker text data font size
#' @param series.marker.text.position Character; first word can be "top",
#' "middle", or "bottom".  Second word can be "left", "middle", or "right".
#' @param show.modebar Logical; whether to show the zoom menu buttons or not.
#' @examples
#' data("y.data")
#' data("x.data")
#' LineChart(y.data, x.data)
#' @export
LineChart <-   function(y,
                        x = NULL,
                        # weights = NULL,                                 ## Gets passed to AsChartMatrix <- add to that function first!
                        # subset = NULL,                                  ## Gets passed to AsChartMatrix <- add to that function first!
                        transpose = FALSE,                                ## Should the inputs be transposed; TRUE or FALSE
                        aggregate.period = "month",                       ## can be month, quarter, year
                        y.labels = NULL,                                  ## Optional, overrides named vectors et c.
                        x.labels = NULL,                                  ## Optional
                        type = "Line",                                    ## Type of chart; can be "Line", "Stacked Line", or "100% Stacked Line"
                        title = "",                                       ## Chart title
                        global.font.family = "Arial",                     ## Global font family
                        global.font.color = rgb(44, 44, 44, maxColorValue = 255),   ## Global font color
                        global.font.size = 10,                            ## Global font size
                        colors = qColors,                                 ## Vector of colors in RGB format
                        transparency = 1,                                 ## Transparency
                        chart.fill.color = rgb(255, 255, 255, maxColorValue = 255), ## Chart (borders around plot) background color
                        chart.fill.transparency = 1,                      ## Chart (borders around plot) background color transparency
                        plot.fill.color = rgb(255, 255, 255, maxColorValue = 255),  ## Plot (the plot area proper) background color
                        plot.fill.transparency = 1,                       ## Plot (the plot area proper) background color transparency
                        legend.show = TRUE,                               ## Show the legend, TRUE or FALSE
                        legend.fill = rgb(255, 255, 255, maxColorValue = 255),      ## Legend fill color
                        legend.border.color = rgb(44, 44, 44, maxColorValue = 255), ## Legend border color
                        legend.border.line.width = 0,                     ## >0 means border on; width in pixels
                        legend.font.color = rgb(44, 44, 44, maxColorValue = 255),   ## Legend font color
                        legend.font.family = "Arial",                     ## Legend font family.
                        legend.font.size = 12,                            ## Legend font size
                        legend.position = "right",                        ## Can be "left" or "right"
                        legend.sort.order = "normal",                     ## Can be "normal" or "reversed" (see also grouping options, currently excluded from this function)
                        margin.top = 80,                                  ## Margin between plot area and the top of the graphic
                        margin.bottom = 80,                               ## Margin between plot area and the bottom of the graphic
                        margin.left = 80,                                 ## Margin between plot area and the left of the graphic
                        margin.right = 80,                                ## Margin between plot area and the right of the graphic
                        margin.inner.pad = 0,                             ## Padding between graph proper and axis lines
                        y.title = "",                                     ## Y-axis title
                        y.title.font.color = rgb(44, 44, 44, maxColorValue = 255),  ## Y-axis title font color
                        y.title.font.family = "Arial",                    ## Y-axis title font family
                        y.title.font.size = 16,                           ## Y-axis title font size
                        y.line.width = 1,                                 ## Y-axis line in pixels, 0 = no line
                        y.line.color = rgb(0, 0, 0, maxColorValue = 255),           ## Y-axis line color
                        y.tick.marks = "",                                ## Can be "outside", "inside", "none"
                        y.tick.length = 5,                                ## Length in px of tick marks.
                        y.bounds.minimum = NULL,                          ## Set minimum of range for plotting; NULL = no manual range set
                        y.bounds.maximum = NULL,                          ## Set maximum of range for plotting; NULL = no manual range set
                        y.bounds.units.major = NULL,                      ## Set step between minimum and maximum for plotting; NULL = no manual range set
                        y.zero.line.width = 1,                            ## Width in pixels of zero line; 0 = no zero line shown
                        y.zero.line.color = rgb(44, 44, 44, maxColorValue = 255),   ## Color of zero line
                        y.position = "left",                              ## Select y-axis position; can be "left" or "right"
                        y.mirror = FALSE,                                 ## Mirror y-axis on other side?
                        #y.values.reversed = FALSE,                       ## T/F - involves autorange and may be too complicated.
                        y.grid.width = 1,                                 ## Width of y-grid lines in pixels; 0 = no line
                        y.grid.color = rgb(225, 225, 225, maxColorValue = 255),     ## Color of y-grid lines
                        y.ticksuffix = "",                                ## Y-axis tick label suffix
                        y.tickprefix = "",                                ## Y-axis tick label prefix
                        y.tick.angle = 0,                                 ## Y-axis tick label angle in degrees.  90 = vertical; 0 = horizontal
                        y.tick.format.manual = "",                        ## See https://github.com/mbostock/d3/wiki/Formatting#numbers or https://docs.python.org/release/3.1.3/library/string.html#formatspec
                        y.tick.format = c("",""),                         ## Uses above to set pre-specified formats, can be:  "n" (number), "%", "$", "£", second value is number of total digits showing (not decimals...)
                        y.tick.font.color = rgb(0, 0, 0, maxColorValue = 255),      ## Y-axis tick label font color
                        y.tick.font.family = "Arial",                     ## Y-axis tick label font family
                        y.tick.font.size = 10,                            ## Y-axis tick label font size
                        y.hover.format.manual = "",                       ## See https://github.com/mbostock/d3/wiki/Formatting#numbers or https://docs.python.org/release/3.1.3/library/string.html#formatspec
                        y.hover.format = c("",""),                        ## Uses above to set pre-specified formats, can be:  "n" (number), "%", "$", "£", second value is number of total digits showing (not decimals...)
                        x.title = "",                                     ## X-axis title
                        x.title.font.color = rgb(44, 44, 44, maxColorValue = 255),  ## X-axis title font color
                        x.title.font.family = "Arial",			          ## X-axis title font family
                        x.title.font.size = 16,			                  ## X-axis title font size
                        x.line.width = 1,                                 ## X-axis line in pixels, 0 = no line
                        x.line.color = rgb(0, 0, 0, maxColorValue = 255),	          ## X-axis line color
                        x.tick.marks = "",                                ## "outside", "inside", "none"
                        x.tick.length = 5,                                ## Length in px of tick marks.
                        x.bounds.minimum = 0,                             ## Set minimum of range for plotting; NULL = no manual range set
                        x.bounds.maximum = 1,                             ## Set maximum of range for plotting; NULL = no manual range set
                        x.bounds.units.major = 0.2,                       ## Set step between minimum and maximum for plotting; NULL = no manual range set
                        x.zero.line.width = 1,                            ## Width in pixels of zero line; 0 = no zero line shown
                        x.zero.line.color = rgb(44, 44, 44, maxColorValue = 255),   ## Color of zero line
                        x.position = "bottom",                            ## Select x-axis position ("top"/"bottom")
                        x.mirror = FALSE,                                 ## Mirror x-axis on other side?
                        #x.values.reversed = FALSE,                       ## T/F - involves autorange and may be too complicated.
                        x.grid.width = 0,                                 ## Width of x-grid lines in pixels; 0 = no line
                        x.grid.color = rgb(225, 225, 225, maxColorValue = 255),     ## Width of y-grid lines in pixels; 0 = no line
                        x.ticksuffix = "",				                   ## X-axis tick label suffix
                        x.tickprefix = "",				                   ## X-axis tick label prefix
                        x.tick.angle = 0,				                   ## X-axis tick label angle in degrees.  90 = vertical; 0 = horizontal
                        x.tick.format.manual = "",                        ## See https://github.com/mbostock/d3/wiki/Formatting#numbers or https://docs.python.org/release/3.1.3/library/string.html#formatspec
                        x.tick.format = c("",""),                         ## Uses above to set pre-specified formats, can be:  "n" (number), "%", "$", "£", second value is number of total digits showing (not decimals...)
                        x.hover.format.manual = "",                       ## See https://github.com/mbostock/d3/wiki/Formatting#numbers or https://docs.python.org/release/3.1.3/library/string.html#formatspec
                        x.hover.format = c("",""),                        ## Uses above to set pre-specified formats, can be:  "n" (number), "%", "$", "£", second value is number of total digits showing (not decimals...)
                        x.tick.font.color = rgb(0, 0, 0, maxColorValue = 255),	   ## Y-axis tick label font color
                        x.tick.font.family = "Arial",			           ## Y-axis tick label font family
                        x.tick.font.size = 10,				               ## Y-axis tick label font size
                        series.marker.show = "none",                      ## Can be "none", "automatic" or a vector referencing the plotly symbol dictionary using either numerics or strings.
                        series.marker.color = qColors,                    ## A vector of colors to use for the markers
                        series.marker.transparency = 1,                   ## Transparency for series markers
                        series.marker.size = 6,                           ## Size in pixels of marker
                        series.marker.border.width = 1,                   ## Width in pixels of border/line around series markers; 0 is no line
                        series.marker.border.color = qColors,             ## Color of border/line around series markers
                        series.marker.border.transparency = 1,            ## Transparency of border/line around series markers
                        series.marker.text = FALSE,                       ## Show data point as text
                        series.marker.text.font = "Arial",
                        series.marker.text.color = rgb(191, 191, 191, maxColorValue = 255),
                        series.marker.text.size = 10,
                        series.marker.text.position = "top middle",
                        series.line.width = 1,                            ## 0 = no line, else thickness of line for series.  Cannot be 0 for a line chart
                        series.line.color = qColors,                      ## A vector of colors to use for the lines
                        series.line.transparency = 1,                     ## Transparency for series lines
                        hover.mode = "closest",                           ## Can be FALSE, "x", "y", or "closest"
                        show.modebar = FALSE                              ## T/F - show the zoom menu
)
{
    ## Get dependencies
    # require(plotly)

    ## Make a chart matrix
    chart.matrix <- AsChartMatrix(y, x, transpose = transpose, aggregate.period = aggregate.period)

    ## Check that the chart matrix is a success
    if (!IsChartMatrix(chart.matrix, n.rows = nrow(chart.matrix), n.columns = ncol(chart.matrix)))
        stop(paste("Input data is not in a chart matrix format"))

    ## Check that line width is at least 1
    if (!series.line.width > 0)
        stop(paste("series.line.width must be at least 1 for a line chart, or else there would be no lines."))

    ## Create text matrix of source data if required for hover
    if (series.marker.text)
        series.marker.data <- chart.matrix
    else
        series.marker.data <- NULL



    ## Change the matrix data according to requirements of the chart type
    # if (type == "Stacked Line")
    #     chart.matrix <- cum.data(chart.matrix, "cumulative.sum")
    # else if (type == "100% Stacked Line")
    #     chart.matrix <- cum.data(chart.matrix, "cumulative.percentage")

    ## Get axes labels from the matrix labels if none manually specified
    if (is.null(x.labels))
        x.labels <- colnames(chart.matrix)

    if (is.null(y.labels))
        y.labels <- rownames(chart.matrix)

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

    ## Group legend items if it's a stacked area chart as taking individual items off makes no sense
    # legend.group <- ""
    # if (type != "Area")
    #     legend.group <- "grouped"

    ## If line thickness is zero, then we shouldn't show a line; ticks only shown if there's a line (same as Excel)
    ## Tick labels only shown if there's a line too - makes no sense either.
    y.showline <- FALSE
    y.showticklabels <- FALSE
    y.showticks <- FALSE
    if (y.line.width >= 1)
    {
        y.showline <- TRUE
        y.showticklabels <- TRUE
        # Default to outer tick marks if they are to be shown, but have not been specified
        if (y.tick.marks == "")
        {
            y.showticks <- TRUE
            y.tick.marks <- "outer"
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
    x.showticklabels <- FALSE
    x.showticks <- FALSE
    if (x.line.width >= 1)
    {
        x.showline <- TRUE
        x.showticklabels <- TRUE
        # Default to outer tick marks if they are to be shown, but have not been specified
        if (x.tick.marks == "")
        {
            x.showticks <- TRUE
            x.tick.marks <- "outer"
        }
    }
    else
        x.tick.marks <- ""

    # If specified no tick marks, then make sure they have no tick length, as this overrides showticks.
    if (x.tick.marks == "none" | x.tick.length > 0)
    {
        x.showticks <- FALSE
        x.tick.length <- 0
    }

    ## Resolve numeric tick values based on y.bounds.minimum and y.bounds.maximum, and y.bounds.units.major
    y.tickmode = "auto"
    y.tickvals = integer()
    y.ticktext = character()
    y.range = integer()
    y.autorange = TRUE

    y.bounds.manual <- TRUE
    if (is.null(y.bounds.minimum) | is.null(y.bounds.maximum) | is.null(y.bounds.units.major))
        y.bounds.manual <- FALSE

    if (y.bounds.manual == TRUE)
    {
        y.range <- c(y.bounds.minimum, y.bounds.maximum)
        y.autorange = FALSE
        y.tickmode <- "array"
        for (a in seq(y.bounds.minimum, y.bounds.maximum, by = y.bounds.units.major))
        {
            y.tickvals <- c(y.tickvals, a)
        }
    }

    x.tickmode = "auto"
    x.tickvals = integer()
    x.ticktext = character()
    x.range = integer()
    x.autorange = TRUE

    x.bounds.manual <- FALSE
    if (!is.null(x.bounds.minimum) & !is.null(x.bounds.maximum) & !is.null(x.bounds.units.major) & x.bounds.maximum > x.bounds.minimum)
        x.bounds.manual <- TRUE

    if (x.bounds.manual == TRUE)
    {
        x.range <- c(x.bounds.minimum, x.bounds.maximum)
        x.autorange = FALSE
        x.tickmode <- "array"
        for (a in seq(x.bounds.minimum, x.bounds.maximum, by = x.bounds.units.major))
        {
            x.tickvals <- c(x.tickvals, a)
        }
    }

    ## Should we draw a zero line
    y.zero.line <- FALSE
    if (y.zero.line.width > 0)
        y.zero.line <- TRUE

    x.zero.line <- FALSE
    if (x.zero.line.width > 0)
        x.zero.line <- TRUE

    ## Work out number format to use.
    if (y.tick.format.manual != "")
        y.tickformat <- y.tick.format.manual
    else if (y.tick.format[1] != "")
    {
        y.tickformat.vector <- printFormat(y.tick.format)
        y.tickformat <- y.tickformat.vector[1]
        y.tickprefix <- y.tickformat.vector[2]
    }
    else
        y.tickformat <- ""


    if (x.tick.format.manual != "")
        x.tickformat <- x.tick.format.manual
    else if (x.tick.format[1] != "")
    {
        x.tickformat.vector <- printFormat(x.tick.format)
        x.tickformat <- x.tickformat.vector[1]
        x.tickprefix <- x.tickformat.vector[2]
    }
    else
        x.tickformat <- ""

    ## Hover number formats
    if (y.hover.format.manual != "")
        y.hoverformat <- y.hover.format.manual
    else if (y.hover.format[1] != "")
    {
        y.hoverformat.vector <- printFormat(y.hover.format)
        y.hoverformat <- y.hoverformat.vector[1]
        y.hoverprefix <- y.hoverformat.vector[2]
    }
    else
        y.hoverformat <- ""

    if (x.hover.format.manual != "")
        x.hoverformat <- x.hover.format.manual
    else if (x.hover.format[1] != "")
    {
        x.hoverformat.vector <- printFormat(x.hover.format)
        x.hoverformat <- x.hoverformat.vector[1]
        x.hoverprefix <- x.hoverformat.vector[2]
    }
    else
        x.hoverformat <- ""

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

    ## Showing markers and lines
    series.mode = "lines+markers"  #default = line and marker
    # if (series.line.width == 0 && series.marker.show != "none")
    #     series.mode = "markers"

    if (series.line.width >= 1 && series.marker.show == "none")
        series.mode <- "lines"

    if (series.marker.text)
        series.mode <- paste(series.mode, "+text", sep = "")

    # else if (series.line.width == 0 && series.marker.show == "none")
    #     series.mode = "none"

    ## Which markers to show?
    if (series.marker.show == "automatic" || series.marker.show == "none")
        series.marker.symbols <- plotlySymbols
    else if (series.marker.show != "none" && series.marker.show != "automatic")
    {
        if (length(series.marker.show) < 100)
            series.marker.symbols <- rep(series.marker.show, 100)
    }

    ## Increase number of colors in color vectors such that a max of 100 are stored, in case insufficient numbers have been specified
    colors <- rep(colors, 100/length(colors))
    series.marker.color <- rep(series.marker.color, 100/length(series.marker.color))
    series.marker.border.color <- rep(series.marker.border.color, 100/length(series.marker.border.color))
    series.line.color <- rep(series.line.color, 100/length(series.line.color))

    ## Initiate plotly object
    p <- plotly::plot_ly()

    ## Config options
    p <- plotly::config(displayModeBar = show.modebar)

    ## Add a trace for each row of data in the matrix
    for (a in 1:nrow(chart.matrix))
    {
        y = as.numeric(chart.matrix[a, ])
        x <- x.labels

        if (!series.marker.text)
            series.marker.data.show <- NULL
        else
            series.marker.data.show <- series.marker.data[a, ]

        p <- plotly::add_trace(p,
                       type = type,
                       x = x,
                       y = y,
                       evaluate = TRUE,
                       fill = FALSE,
                       fillcolor = plotly::toRGB(colors[a], alpha = transparency),
                       line = list(
                           width = series.line.width,
                           color = plotly::toRGB(series.line.color[a], alpha = series.line.transparency)
                       ),
                       name = y.labels[a],
                       text = series.marker.data.show,
                       textfont = list(
                           family = series.marker.text.font,
                           color = series.marker.text.color,
                           size = series.marker.text.size
                       ),
                       textposition = series.marker.text.position,
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
                       )
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
            ticks = y.tick.marks,
            tickangle = y.tick.angle,
            ticklen = y.tick.length,
            tickcolor = y.line.color,
            zeroline = y.zero.line,
            zerolinewidth = y.zero.line.width,
            zerolinecolor = y.zero.line.color,
            tickformat = y.tickformat,
            tickprefix = y.tickprefix,
            ticksuffix = y.ticksuffix,
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
            zeroline = x.zero.line,
            zerolinewidth = x.zero.line.width,
            zerolinecolor = x.zero.line.color,
            tickformat = x.tickformat,
            tickprefix = x.tickprefix,
            ticksuffix = x.ticksuffix,
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
            family = global.font.family ,
            color = global.font.color,
            size = global.font.size
        )
    )

    ## Return the chart
    p
}


