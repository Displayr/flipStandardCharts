### Required colour vector
qColors <- c(grDevices::rgb(91, 155, 213, 255, max = 255), # blue
             grDevices::rgb(237, 125, 49, 255, max = 255), # orange
             grDevices::rgb(165, 165, 165, 255, max = 255), # grey
             grDevices::rgb(30, 192, 0, 255, max = 255), # yellow
             grDevices::rgb(68, 114, 196, 255, max = 255), # darker blue
             grDevices::rgb(112, 173, 71, 255, max = 255), # green
             grDevices::rgb(37, 94, 145, 255, max = 255), # even darker blue
             grDevices::rgb(158, 72, 14, 255, max = 255), # blood
             grDevices::rgb(99, 99, 99, 255, max = 255), # dark grey
             grDevices::rgb(153, 115, 0, 255, max = 255), # brown
             grDevices::rgb(38, 68, 120, 255, max = 255), # very dark blue
             grDevices::rgb(67, 104, 43, 255, max = 255), # darker green
             grDevices::rgb(255, 255, 255, 255, max = 255), # black
             grDevices::rgb(255, 35, 35, 255, max = 255)) # red

plotlySymbols <- plotlySymbols <- c(0,100,200,300,1,101,201,301,2,102,202,302,3,103,203,303,4,104,204,304,5,105,205,305,6,106,206,306,7,107,207,307,8,108,208,308,9,109,209,309,10,110,210,310,11,111,211,311,12,112,212,312,13,113,213,313,14,114,214,314,15,115,215,315,16,116,216,316,17,117,217,317,18,118,218,318,19,119,219,319,20,120,220,320,21,121,221,321,22,122,222,322,23,123,223,323,24,124,224,324,25,125,26,126,27,127,28,128,29,129,30,130,31,131,32,132,33,133,34,134,35,135,36,136,37,137,38,138,39,139,40,140,41,141,42,142,43,143,44,144)

######################## Auxiliary functions
## Strips out any capital X followed by a digit, as this is R-imposed in case of numeric row or col names.
clean.numeric.labels <- function(labels) {
    labels <- gsub("^X","",labels)
    return(labels)
}

### KNOWN ISSUES TO SOLVE:
# The function "clean.numeric.labels" needs to allow for text variable names that begin with X but which
# are not otherwise entirely numeric.  e.g. "Xylophone", "Xenophon", "X1Z", where the INTENT was to have
# the "X" and it hasn't been automatically added by Q.  Needs to compare against the original data file
# import to be truly accurate and not make too many guesses.

## Takes a matrix, and returns a matrix of either a cumulative sum, or a cumulative sum of percentages.
cum.data <- function(x, output = "cumulative.percentage") {
    if (output == "cumulative.sum"){
        x <- apply(x, 2, function(z) {cumsum(z)})
    } else if (output == "cumulative.percentage") {
        x <- apply(x, 2, function(z) {cumsum(prop.table(z))})
    } else if (output == "column.percentage") {
        x <- apply(x, 2, function(z) {prop.table(z)})
    }

    return(x)
}

## Function to return number format string and potential y-axis prefix
printFormat <- function (tick.format = c("%", "0"))     # Uses above to set pre-specified formats, can be:  "n" (number), "%", "$", "£", "€" second value is number of decimals for rounding, default = 0
{
    tickformat <- character()
    if (length(tick.format) != 2)
        tickformat[2] <- "0"

    if (tick.format[1] == "%")
    {
        tickformat <- paste(".", as.integer(tick.format[2]), "p", sep="")
        tick.prefix <- ""
    }
    else if (tick.format[1] == "$" || tick.format[1] == "£")
    {
        tickformat <- paste(".", as.integer(tick.format[2]), "r", sep="")
        tick.prefix <- tick.format[1]
    }
    else if (tick.format[1] == "n")
    {
        tickformat <- paste(".", as.integer(tick.format[2]), "r", sep="")
        tick.prefix <- ""
    }

    return(c(tickformat,tick.prefix))
}

# Checks if all three vars have data, and if the first is greater than the second
checkBounds <- function(min, max, maj)
{
    if (!is.null(min) & !is.null(max) & !is.null(maj) & max > min)
        TRUE

    FALSE
}

################################################################################################

## Sorts rows of a matrix in descending order (high to low) based on overall mean of each row.
#' @export
MeanRowValueDescendingSort <- function(x) {
    means <- apply(x, 1, function(z) {mean(z)})
    x <- cbind(x, means)
    x <- x[order(-means),]
    x <- x[,1:(ncol(x)-1)]
    return(x)
}

## Sorts rows of a matrix in descending alphabetical order
#' @export
AlphabeticRowNames <- function(x, desc = TRUE) {
    x <- x[order(rownames(x), decreasing = desc),]
    return(x)
}

## Gives a named-elements vector of colours for each row in a matrix,
## where the base colour is specified, and the alpha value gradually
## increases based on the row-mean of the matrix (darker = higher),
## and where the colour vector is returned sorted alphabetically
#' @export
MakeColorGradient <- function (x, red, green, blue) {
    col.vector <- ""
    number.rows <- nrow(x) + 1

    for (i in 1:nrow(x)){
        red.factor <- ((255 - red) / number.rows) * i
        green.factor <- ((255 - green) / number.rows) * i
        blue.factor <- ((255 - blue) / number.rows) * i

        col.vector <- c(col.vector, grDevices::rgb(red + red.factor, green + green.factor, blue + blue.factor, 255, max = 255))
    }

    ## Sort the matrix by mean
    ordered.matrix <- MeanRowValueDescendingSort(x)

    ## Assign matrix row names to col.vector
    col.vector <- col.vector[2:length(col.vector)]
    names(col.vector) <- rownames(ordered.matrix)

    ## Sort the colour vector
    col.vector <- AlphabeticRowNames(as.matrix(col.vector))

    return(col.vector)
}

#' @export
AreaChart <-   function(y,
                         x = NULL,
                         weights = NULL,                                   ## Gets passed to AsChartMatrix <- add to that function first!
                         # subset = NULL,                                  ## Gets passed to AsChartMatrix <- add to that function first!
                         transpose = FALSE,                                ## Should the inputs be transposed; TRUE or FALSE
                         date.aggregation = "m",                           ## can be m(onth), q(uarter), y(ear)
                         row.labels = NULL,                                ## Optional, overrides named vectors et c.
                         column.labels = NULL,                             ## Optional
                         type = "Area",                                    ## Type of char; can be "Area", "Stacked Area", or "100% Stacked Area"
                         title = "",                                       ## Chart title
                         colors = qColors,                                 ## Vector of colors in RGB format
                         transparency = 1,                                 ## Transparency of area fill colors
                         chart.fill.color = rgb(255, 255, 255, max = 255), ## Chart (borders around plot) background color
                         chart.fill.transparency = 1,                      ## Chart (borders around plot) background color transparency
                         plot.fill.color = rgb(255, 255, 255, max = 255),  ## Plot (the plot area proper) background color
                         plot.fill.transparency = 1,                       ## Plot (the plot area proper) background color transparency
                         legend.show = TRUE,                               ## Show the legend, TRUE or FALSE
                         legend.fill = rgb(255, 255, 255, max = 255),      ## Legend fill color
                         legend.border.color = rgb(44, 44, 44, max = 255), ## Legend border color
                         legend.border.line.width = 0,                     ## >0 means border on; width in pixels
                         legend.font.color = rgb(44, 44, 44, max = 255),   ## Legend font color
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
                         y.title.font.color = rgb(44, 44, 44, max = 255),  ## Y-axis title font color
                         y.title.font.family = "Arial",                    ## Y-axis title font family
                         y.title.font.size = 16,                           ## Y-axis title font size
                         y.line.width = 1,                                 ## Y-axis line in pixels, 0 = no line
                         y.line.color = rgb(0, 0, 0, max = 255),           ## Y-axis line color
                         y.tick.marks = "",                                ## Can be "outside", "inside", "none"
                         y.tick.length = 5,                                ## Length in px of tick marks.
                         y.bounds.minimum = NULL,                          ## Set minimum of range for plotting; NULL = no manual range set
                         y.bounds.maximum = NULL,                          ## Set maximum of range for plotting; NULL = no manual range set
                         y.bounds.units.major = NULL,                      ## Set step between minimum and maximum for plotting; NULL = no manual range set
                         y.zero.line.width = 1,                            ## Width in pixels of zero line; 0 = no zero line shown
                         y.zero.line.color = rgb(44, 44, 44, max = 255),   ## Color of zero line
                         y.position = "left",                              ## Select y-axis position; can be "left" or "right"
                         y.mirror = FALSE,                                 ## Mirror y-axis on other side?
                         #y.values.reversed = FALSE,                       ## T/F - involves autorange and may be too complicated.
                         y.grid.width = 1,                                 ## Width of y-grid lines in pixels; 0 = no line
                         y.grid.color = rgb(225, 225, 225, max = 255),     ## Color of y-grid lines
                         y.ticksuffix = "",                                ## Y-axis tick label suffix
                         y.tickprefix = "",                                ## Y-axis tick label prefix
                         y.tick.angle = 0,                                 ## Y-axis tick label angle in degrees.  90 = vertical; 0 = horizontal
                         y.tick.format.manual = "",                        ## See https://github.com/mbostock/d3/wiki/Formatting#numbers or https://docs.python.org/release/3.1.3/library/string.html#formatspec
                         y.tick.format = c("",""),                         ## Uses above to set pre-specified formats, can be:  "n" (number), "%", "$", "£", second value is number of total digits showing (not decimals...)
                         y.tick.font.color = rgb(0, 0, 0, max = 255),      ## Y-axis tick label font color
                         y.tick.font.family = "Arial",                     ## Y-axis tick label font family
                         y.tick.font.size = 10,                            ## Y-axis tick label font size
                         y.hover.format.manual = "",                       ## See https://github.com/mbostock/d3/wiki/Formatting#numbers or https://docs.python.org/release/3.1.3/library/string.html#formatspec
                         y.hover.format = c("",""),                        ## Uses above to set pre-specified formats, can be:  "n" (number), "%", "$", "£", second value is number of total digits showing (not decimals...)
                         x.title = "",                                     ## X-axis title
                         x.title.font.color = rgb(44, 44, 44, max = 255),  ## X-axis title font color
                         x.title.font.family = "Arial",			           ## X-axis title font family
                         x.title.font.size = 16,			               ## X-axis title font size
                         x.line.width = 1,                                 ## X-axis line in pixels, 0 = no line
                         x.line.color = rgb(0, 0, 0, max = 255),	       ## X-axis line color
                         x.tick.marks = "",                                ## "outside", "inside", "none"
                         x.tick.length = 5,                                ## Length in px of tick marks.
                         x.bounds.minimum = 0,                             ## Set minimum of range for plotting; NULL = no manual range set
                         x.bounds.maximum = 1,                             ## Set maximum of range for plotting; NULL = no manual range set
                         x.bounds.units.major = 0.2,                       ## Set step between minimum and maximum for plotting; NULL = no manual range set
                         x.zero.line.width = 1,                            ## Width in pixels of zero line; 0 = no zero line shown
                         x.zero.line.color = rgb(44, 44, 44, max = 255),   ## Color of zero line
                         x.position = "bottom",                            ## Select x-axis position ("top"/"bottom")
                         x.mirror = FALSE,                                 ## Mirror x-axis on other side?
                         #x.values.reversed = FALSE,                       ## T/F - involves autorange and may be too complicated.
                         x.grid.width = 0,                                 ## Width of x-grid lines in pixels; 0 = no line
                         x.grid.color = rgb(225, 225, 225, max = 255),     ## Width of y-grid lines in pixels; 0 = no line
                         x.ticksuffix = "",				                   ## X-axis tick label suffix
                         x.tickprefix = "",				                   ## X-axis tick label prefix
                         x.tick.angle = 0,				                   ## X-axis tick label angle in degrees.  90 = vertical; 0 = horizontal
                         x.tick.format.manual = "",                        ## See https://github.com/mbostock/d3/wiki/Formatting#numbers or https://docs.python.org/release/3.1.3/library/string.html#formatspec
                         x.tick.format = c("",""),                         ## Uses above to set pre-specified formats, can be:  "n" (number), "%", "$", "£", second value is number of total digits showing (not decimals...)
                         x.hover.format.manual = "",                       ## See https://github.com/mbostock/d3/wiki/Formatting#numbers or https://docs.python.org/release/3.1.3/library/string.html#formatspec
                         x.hover.format = c("",""),                        ## Uses above to set pre-specified formats, can be:  "n" (number), "%", "$", "£", second value is number of total digits showing (not decimals...)
                         x.tick.font.color = rgb(0, 0, 0, max = 255),	   ## Y-axis tick label font color
                         x.tick.font.family = "Arial",			           ## Y-axis tick label font family
                         x.tick.font.size = 10,				               ## Y-axis tick label font size
                         series.marker.show = "none",                      ## Can be "none", "automatic" or a vector referencing the plotly symbol dictionary using either numerics or strings.
                         series.marker.color = qColors,                    ## A vector of colors to use for the markers
                         series.marker.transparency = 1,                   ## Transparency for series markers
                         series.marker.size = 6,                           ## Size in pixels of marker
                         series.marker.border.width = 1,                   ## Width in pixels of border/line around series markers; 0 is no line
                         series.marker.border.color = qColors,             ## Color of border/line around series markers
                         series.marker.border.transparency = 1,            ## Transparency of border/line around series markers
                         series.line.width = 0,                            ## 0 = no line, else thickness of line for series.
                         series.line.color = qColors,                      ## A vector of colors to use for the lines
                         series.line.transparency = 1,                     ## Transparency for series lines
                         hover.mode = "closest"                            ## Can be FALSE, "x", "y", or "closest"
)
{
    ## Get dependencies
    require(plotly)

    ## Make a chart matrix
    chart.matrix <- AsChartMatrix(y, x, transpose = transpose, weights = weights, date.aggregation = date.aggregation)

    ## Check that the chart matrix is a success
    if (!IsChartMatrix(chart.matrix, n.rows = nrow(chart.matrix), n.columns = ncol(chart.matrix)))
        stop(paste("Input data is not in a chart matrix format"))

    ## Issue warning if transparency is = 1 and type = "Area"
    if (transparency == 1 && type == "Area")
        warning("Displaying this chart without transparent series will make it difficult to read as some data series may be obscured.")

    ## Change the matrix data according to requirements of the chart type
    if (type == "Stacked Area")
        chart.matrix <- cum.data(chart.matrix, "cumulative.sum")
    else if (type == "100% Stacked Area")
        chart.matrix <- cum.data(chart.matrix, "cumulative.percentage")

    ## Grab constants from the matrix
    if (!is.null(column.labels))
        x.labels <- column.labels
    else
        x.labels <- clean.numeric.labels(colnames(chart.matrix))

    if (!is.null(row.labels))
        series.labels <- row.labels
    else
        series.labels <- rownames(chart.matrix)

    ## Determine whether to draw to zero y (overlapping area chart) or to next y (for stacked)
    if (type == "Area")
        fill.bound <- "tozeroy"
    else
        fill.bound <- "tonexty"

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
    legend.group <- ""
    if (type != "Area")
        legend.group <- "grouped"

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

    ## Resolve numeric tick values based on y.bounds.minimum and y.bounds.maximum, and y.bounds.units.major
    y.tickmode = "auto"
    y.tickvals = integer()
    y.ticktext = character()
    y.range = integer()
    y.autorange = TRUE

    y.bounds.manual <- FALSE
    if (!is.null(y.bounds.minimum) & !is.null(y.bounds.maximum) & !is.null(y.bounds.units.major))
        y.bounds.manual <- checkBounds(y.bounds.minimum, y.bounds.maximum, y.bounds.units.major)

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
    if (!is.null(x.bounds.minimum) & !is.null(x.bounds.maximum) & !is.null(x.bounds.units.major))
        x.bounds.manual <- checkBounds(x.bounds.minimum, x.bounds.maximum, x.bounds.units.major)

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
    if (series.line.width == 0 && series.marker.show != "none")
        series.mode = "markers"

    else if (series.line.width >= 1 && series.marker.show == "none")
        series.mode = "lines"

    else if (series.line.width == 0 && series.marker.show == "none")
        series.mode = "none"

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
    p <- plot_ly()

    ## Add a trace for each row of data in the matrix
    for (a in 1:nrow(chart.matrix))
    {
        y = as.numeric(chart.matrix[a,])
        x <- x.labels

        p <- add_trace(p,
                       type = type,
                       x = x,
                       y = y,
                       evaluate = TRUE,
                       fill = fill.bound,
                       fillcolor = toRGB(colors[a], alpha = transparency),
                       line = list(
                           width = series.line.width,
                           color = toRGB(series.line.color[a], alpha = series.line.transparency)
                       ),
                       name = series.labels[a],
                       legendgroup = legend.group,
                       ## MARKERS
                       mode = series.mode,
                       marker = list(
                           size = series.marker.size,
                           color = toRGB(series.marker.color[a], alpha = series.marker.transparency),
                           symbol = series.marker.symbols[a],
                           line = list(
                               color = toRGB(series.marker.border.color[a], alpha = series.marker.border.transparency),
                               width = series.marker.border.width
                           )
                       )
        )
    }

    ## Set plotly layout styles
    p <- layout(
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
        plot_bgcolor = toRGB(plot.fill.color, alpha = plot.fill.transparency),
        paper_bgcolor = toRGB(chart.fill.color, alpha = chart.fill.transparency),
        hovermode = hover.mode
    )

    ## Return the chart
    p
}

