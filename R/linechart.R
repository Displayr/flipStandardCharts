#' @export
LineChart <-   function(y,
                        x = NULL,
                        # weights = NULL,          ## Gets passed to AsChartMatrix <- add to that function first!
                        # subset = NULL,           ## Gets passed to AsChartMatrix <- add to that function first!
                        transpose = FALSE,
                        date.aggregation = "m",    ## can be m(onth), q(uarter), y(ear)

                        row.labels = "",           ## Optional, overrides named vectors et c.
                        column.labels = "",
                        type = "Line",
                        title = "",
                        colors = qColors,
                        transparency = 1,
                        chart.fill.color = rgb(255, 255, 255, max = 255),
                        chart.fill.transparency = 1,
                        plot.fill.color = rgb(255, 255, 255, max = 255),
                        plot.fill.transparency = 1,
                        legend.show = TRUE,
                        legend.fill = rgb(255, 255, 255, max = 255),
                        legend.border.color = rgb(44, 44, 44, max = 255),
                        legend.border.line.width = 0,  #>0 means border on
                        legend.font.color = rgb(44, 44, 44, max = 255),
                        legend.font.family = "Arial",
                        legend.font.size = 12,
                        legend.position = "right",     # Local definition, not Plotly.  Can be "left" or "right"
                        legend.sort.order = "normal",   ## Can be "normal" or "reversed" (see also grouping options, currently excluded from this function)
                        margin.top = 80,
                        margin.bottom = 80,
                        margin.left = 80,
                        margin.right = 80,
                        margin.inner.pad = 0,    ## Padding between graph proper and axis lines
                        y.title = "",
                        y.title.font.color = rgb(44, 44, 44, max = 255),
                        y.title.font.family = "Arial",
                        y.title.font.size = 16,
                        y.line.width = 1,  # 0 = no line, else px thickness
                        y.line.color = rgb(0, 0, 0, max = 255),
                        y.tick.marks = "",     # "outside", "inside", "none"
                        y.tick.length = 5,         # Length in px of tick marks.
                        y.bounds.manual = FALSE,     ## T/F.  If true, then require bounds below, else "auto".
                        y.bounds.minimum = 0,
                        y.bounds.maximum = 1,
                        y.bounds.units.major = 0.2,
                        y.zero.line = FALSE,            # T/F show a line along the zero-value of axis.  May be confused with an axis line if origo is lower left corner.
                        y.zero.line.width = 1,
                        y.zero.line.color = rgb(44, 44, 44, max = 255),
                        y.position = "left",            ## Select y-axis position
                        y.mirror = FALSE,               ## Mirror y-axis on other side?
                        #y.values.reversed = FALSE,       ## T/F - involves autorange and may be too complicated.
                        y.grid.show = FALSE,
                        y.grid.width = 1,
                        y.grid.color = rgb(225, 225, 225, max = 255),
                        y.ticksuffix = "",
                        y.tickprefix = "",
                        y.tick.angle = 0,
                        y.tick.format.manual = "",   #See https://github.com/mbostock/d3/wiki/Formatting#numbers or https://docs.python.org/release/3.1.3/library/string.html#formatspec
                        y.tick.format = c("",""),     # Uses above to set pre-specified formats, can be:  "n" (number), "%", "$", "£", second value is number of total digits showing (not decimals...)
                        y.hover.format.manual = "",   #See https://github.com/mbostock/d3/wiki/Formatting#numbers or https://docs.python.org/release/3.1.3/library/string.html#formatspec
                        y.hover.format = c("",""),     # Uses above to set pre-specified formats, can be:  "n" (number), "%", "$", "£", second value is number of total digits showing (not decimals...)
                        y.tick.font.color = rgb(0, 0, 0, max = 255),
                        y.tick.font.family = "Arial",
                        y.tick.font.size = 10,
                        x.title = "",
                        x.title.font.color = rgb(44, 44, 44, max = 255),
                        x.title.font.family = "Arial",
                        x.title.font.size = 16,
                        x.line.width = 1,  # 0 = no line, else px thickness
                        x.line.color = rgb(0, 0, 0, max = 255),
                        x.tick.marks = "",     # "outside", "inside", "none"
                        x.tick.length = 5,         # Length in px of tick marks.
                        x.bounds.manual = FALSE,     ## T/F.  If true, then require bounds below, else "auto".
                        x.bounds.minimum = 0,
                        x.bounds.maximum = 1,
                        x.bounds.units.major = 0.2,
                        x.zero.line = FALSE,            # T/F show a line along the zero-value of axis.  May be confused with an axis line if origo is lower left corner.
                        x.zero.line.width = 1,
                        x.zero.line.color = rgb(44, 44, 44, max = 255),
                        x.position = "bottom",            ## Select x-axis position (top/bottom)
                        x.mirror = FALSE,               ## Mirror x-axis on other side?
                        #x.values.reversed = FALSE,       ## T/F - involves autorange and may be too complicated.
                        x.grid.show = FALSE,
                        x.grid.width = 1,
                        x.grid.color = rgb(196, 196, 196, max = 255),
                        x.ticksuffix = "",
                        x.tickprefix = "",
                        x.tick.angle = 0,
                        x.tick.format.manual = "",   #See https://github.com/mbostock/d3/wiki/Formatting#numbers or https://docs.python.org/release/3.1.3/library/string.html#formatspec
                        x.tick.format = c("",""),     # Uses above to set pre-specified formats, can be:  "n" (number), "%", "$", "£", second value is number of total digits showing (not decimals...)
                        x.hover.format.manual = "",   #See https://github.com/mbostock/d3/wiki/Formatting#numbers or https://docs.python.org/release/3.1.3/library/string.html#formatspec
                        x.hover.format = c("",""),     # Uses above to set pre-specified formats, can be:  "n" (number), "%", "$", "£", second value is number of total digits showing (not decimals...)
                        x.tick.font.color = rgb(0, 0, 0, max = 255),
                        x.tick.font.family = "Arial",
                        x.tick.font.size = 10,
                        series.marker.show = "none",    ## Can be "none", "automatic" or a vector referencing the plotly symbol dictionary using either numerics or strings.
                        series.marker.color = qColors,  ## A vector of colors to use for the markers
                        series.marker.transparency = 1, ## Transparency for series markers
                        series.marker.size = 6,         ## Size in pixels of marker
                        series.marker.border.color = qColors,
                        series.marker.border.transparency = 1,
                        series.marker.border.width = 1,
                        series.line.width = 0,          ## 0 = no line, else thickness of line for series.
                        series.line.color = qColors,    ## A vector of colors to use for the lines
                        series.line.transparency = 1,    ## Transparency for series lines
                        hover.mode = "closest"          ## Can be FALSE, "x", "y", or "closest"
)
{
    ## Get dependencies
    require(plotly)

    # stopIfNotMatrix(input.data.frame)
    ## Data Conversion
    input.data.frame <- AsChartMatrix(y, x, transpose = transpose, date.aggregation = date.aggregation)
    if (!IsChartMatrix(input.data.frame, n.rows = nrow(input.data.frame), n.columns = ncol(input.data.frame)))
        stop(paste("Input data is not in a chart matrix format"))

    ## Issue warning if transparency is = 1 and type = "Area"
    if (transparency == 1 && type == "Area")
        warning("Displaying this chart without transparent series will make it difficult to read as some data series may be obscured.")

    ## Change the matrix data according to requirements of the chart type
    if (type == "Stacked Area" || type == "Stacked Line")
        input.data.frame <- cum.data(input.data.frame, "cumulative.sum")
    else if (type == "100% Stacked Area" || type == "100% Stacked Line")
        input.data.frame <- cum.data(input.data.frame, "cumulative.percentage")
    else if (type == "100% Stacked Bar" || type == "100% Stacked Column")
        input.data.frame <- cum.data(input.data.frame, "column.percentage")

    ## Grab constants from the matrix
    x.labels <- clean.numeric.labels(colnames(input.data.frame))
    series.labels <- rownames(input.data.frame)

    ## Determine whether to draw to zero y (overlapping area chart) or to next y (for stacked)
    fill.bound = "none"
    if (regexpr("Area", type) >= 1)
    {
        # type <- "area"
        fill.bound <- "tozeroy"
        if (type != "Area") {fill.bound = "tonexty"}
    }

    ## Setting up bar and column charts
    orientation <- ""
    barmode <- "group"
    if (regexpr("Bar", type) >= 1 || regexpr("Column", type) >= 1) {
        if (regexpr("Stacked", type) >= 1)
            barmode <- "stack"

        if (regexpr("Bar", type) >= 1)
            orientation <- "h"
        else
            orientation <- "v"

        type <- "bar"
    }

    ################################################
    ##### Set variables for positioning legend #####
    ################################################
    ## Defaults:
    legend.x.anchor <- "left"
    legend.y.anchor <- "auto"
    legend.y <- 1
    legend.x <- 1.02

    ## If legend on right and there's a y-axis on the right too:
    y2 = ""
    if (legend.position == "right" && (y.mirror == TRUE || y2 != "" || y.position == "right"))
        legend.x = 1.15

    ## If legend on the left, and there's no y-axis on the left:
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
    ############## END OF LEGEND POSITIONING #######

    ## Group legend items if it's a stacked area chart as taking individual items off makes no sense
    if (type != "Area")
        legend.group <- "grouped"
    else
        legend.group <- ""

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
    if (y.bounds.manual == TRUE)
    {
        y.range <- c(y.bounds.minimum, y.bounds.maximum)
        y.autorange = FALSE
        y.tickmode <- "array"
        for (a in seq(y.bounds.minimum, y.bounds.maximum, by = y.bounds.units.major))
        {
            y.tickvals <- c(y.tickvals, a)
            # ticktext <- c(ticktext, as.character(a))
            # Ticktext, for numeric marks, is redundant and would override number format.
        }
    }

    x.tickmode = "auto"
    x.tickvals = integer()
    x.ticktext = character()
    x.range = integer()
    x.autorange = TRUE
    if (x.bounds.manual == TRUE)
    {
        x.range <- c(x.bounds.minimum, x.bounds.maximum)
        x.autorange = FALSE
        x.tickmode <- "array"
        for (a in seq(x.bounds.minimum, x.bounds.maximum, by = x.bounds.units.major))
        {
            x.tickvals <- c(x.tickvals, a)
            # ticktext <- c(ticktext, as.character(a))
            # Ticktext, for numeric marks, is redundant and would override number format.
        }
    }

    ## Draw zero line (only relevant if axis <= 0)
    if (y.bounds.minimum >= 0)
        y.zero.line = FALSE

    if (x.bounds.minimum >= 0)
        x.zero.line = FALSE

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
    for (a in 1:nrow(input.data.frame))
    {
        y.data = as.numeric(input.data.frame[a,])

        # Flip labels if it's a bar chart
        if (type == "bar" && orientation == "h") {
            x <- y.data
            y <- x.labels
        }
        else
        {
            x <- x.labels
            y <- y.data
        }

        p <- add_trace(p,
                       type = type,
                       orientation = orientation,
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
        hovermode = hover.mode,
        barmode = barmode

    )

    ## Return the chart
    p
}

