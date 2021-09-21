#' Plot data as a Streamgraph
#'
#' Create a Streamtraph, which is an area chart centered around the x-axis rather than on top of it.
#' @param x A \code{matrix}, with columns containing the dates or other numeric x-axis variable.
#' @param colors A vector of colors of the streams.
#' @param global.font.color Global font color as a named color in character format
#' (e.g. "black") or an a hex code.
#' @param global.font.family Font family of tick labels.
#' @param global.font.size Font size of tick labels in pixels.
#' @param hovertext.font.family Font family of hovertext.
#' @param hovertext.font.color Font color of hovertext as a string or hex code.
#' @param hovertext.font.size Font size of hovertext in pixels.
#' @param y.axis.show Logical; if FALSE, the y-axis is not shown.
#' @param y.tick.format A string representing a d3 formatting code for the y-axis.
#' See https://github.com/d3/d3/blob/master/API.md#number-formats-d3-format
#' @param y.number.ticks The total number of ticks on the y-axis.
#' @param x.tick.format A string representing a d3 formatting code for the x.axis.
#' See https://github.com/d3/d3/blob/master/API.md#number-formats-d3-format
#' @param x.tick.units "Automatic", "Number", "Day", "Month" or "Year".
#' @param x.tick.interval The frequency of ticks on the x-axis. Where the data crosses multiple years, re-starts at each year.
#' If this is zero, it is determined from the data.
#' @param margin.top Top margin (default should be fine, this allows for fine-tuning plot space)
#' @param margin.right Right margin (default should be fine, this allows for fine-tuning plot space)
#' @param margin.bottom Bottom margin (default should be fine, this allows for fine-tuning plot space)
#' @param margin.left Left margin (default should be fine, this allows for fine-tuning plot space)
#' @importFrom streamgraph streamgraph sg_fill_manual sg_axis_x sg_axis_y sg_colors
#' @importFrom flipTime AsDateTime
#' @importFrom verbs SumColumns
#' @importFrom janitor round_half_up
#' @export
Stream <- function(x,
                   colors = c("#5C9AD3", "#ED7D31", "#A5A5A5", "#FFC000",
                              "#4473C5", "#70AD46", "#255F91", "#9E480D",
                              "#636365", "#987300", "#26408B", "#42682B"),
                   global.font.family = "Arial",
                   global.font.color = rgb(44, 44, 44, maxColorValue = 255),
                   global.font.size = 10,
                   y.axis.show = TRUE,
                   y.tick.format = "",
                   y.number.ticks = 5,
                   x.tick.format = "%d %b %y",
                   x.tick.units = "Automatic",
                   x.tick.interval = 1,
                   hovertext.font.color = global.font.color,
                   hovertext.font.family = global.font.family,
                   hovertext.font.size = 11,
                   margin.top = 20,
                   margin.left = 50,
                   margin.bottom = 30,
                   margin.right = 40)
{
    if (isPercentData(x) && isAutoFormat(y.tick.format))
        y.tick.format <- paste0(y.tick.format, "%")
    if (!is.list(x) && (is.array(x) || is.numeric(x)))
        x <- checkMatrixNames(x)
    else if (is.data.frame(x))
        x <- as.matrix(x)
    else
        stop("Stream graphs should have a tabular input (e.g., a matrix).")
    if (!x.tick.units %in% c("Automatic", "Number", "Day", "Month", "Year"))
        stop("x.tick.units must be one of 'Automatic', 'Number', 'Day', 'Month' or 'Year'.")

    # streamgraph requires dates along the columns but for consistency with Time Series, Line, Google Trends etc
    # CChart produces dates along the rows, hence we transpose
    x <- t(x)

    colors = StripAlphaChannel(colors, "Alpha values for colors in Streamgraphs are ignored.")
    if (nrow(x) == 1)
        colors <- c(colors, colors) #  fix streamgraph bug where one color single stream is ignored and produces black chart

    ErrorIfNotEnoughData(x)
    columns <- colnames(x)
    x.axis.type <- getAxisType(columns, x.tick.format)
    if (!x.axis.type %in% c("date", "numeric"))
        stop("Stream requires the rownames of the input data (which form the x-axis) to be dates or numeric. ",
             "Change 'Chart type' to 'Table' to see the input data.")
    if (!is.numeric(x))
        stop("Stream requires numeric data to be plotted along the y-axis. ",
             "Change 'Chart type' to 'Table' to see the input data.")

    if (x.tick.units == "Automatic" && x.axis.type != "date")
        x.tick.units <- "Number"

    if (x.tick.units == "Number")
    {
        if (x.tick.format == "")
            x.tick.format = ".0f"
        if (d3FormatType(x.tick.format) != "numeric" || x.axis.type != "numeric")
            stop("x-axis tick format and units are incompatible.")
        columns <- suppressWarnings(as.numeric(columns))
        if (any(is.na((columns))))
            columns <- 1:ncol(x)

        # convert x.tick.interval to a number of ticks as required in sg_axis_x for numeric axes
        if (x.tick.interval == 0)
            x.tick.interval <- 5
        else
        {
            r <- range(columns)
            x.tick.interval <- (r[2] - r[1]) / (x.tick.interval)
        }
    }
    else
    {
        if (x.tick.format == "")
            x.tick.format = "%d %b %y"
        if (d3FormatType(x.tick.format) != "date" || x.axis.type != "date")
            stop("x-axis tick format and units are incompatible.")
        columns <- AsDateTime(columns, on.parse.failure = "silent")
        diffs <- as.numeric(diff(columns))
        med.diff <- median(abs(diffs), na.rm = TRUE)
        max.diff <- max(abs(diffs), na.rm = TRUE)
        if (max.diff > 1.1 * med.diff)
            warning("Dates are not spaced at regular intervals. Most of the dates have a difference of ",
            med.diff, " but the maximum interval is ", max.diff, ".")
        r <- range(columns)
        day.range <- r[2] - r[1]
        if (x.tick.units == "Automatic")
        {
            if (day.range < 90)
                x.tick.units <- "Day"
            else if (day.range < 367)
                x.tick.units <- "Month"
            else
                x.tick.units <- "Year"
        }
        if (x.tick.interval == 0)
        {
            if (x.tick.units == "Day")
                x.tick.interval <- day.range / 5
            else if (x.tick.units == "Month")
                x.tick.interval <- day.range / 30 / 5
            else
                x.tick.interval <- day.range / 365 / 5
        }
        x.tick.interval <- ceiling(x.tick.interval)
    }

    # Rounding off data to make hovertext legible, but should not affect y-values on the graph
    data.magnitude <- floor(log10(min(SumColumns(x, remove.rows = NULL))))
    x.round <- round(as.numeric(t(x)), max(0, 4 - data.magnitude))
    df <- data.frame(value = x.round, date = columns, key = rep(rownames(x), rep(ncol(x), nrow(x))))

    sg <- streamgraph(data = df,
                key = "key",
                value = "value",
                date = "date",
                offset = "silhouette",
                interpolate = "cardinal",
                interactive = TRUE,
                scale = if(x.tick.units == "Number") "continuous" else "date",
                top = margin.top,
                right = margin.right,
                left = margin.left,
                bottom = margin.bottom)
    sg <- sg_fill_manual(sg, values = colors)
    if (!y.axis.show)
        sg <- sg_axis_y(sg, 0)
    else
        sg <- sg_axis_y(sg, tick_count = y.number.ticks, tick_format = y.tick.format)

    sg <- sg_axis_x(sg, tick_interval = x.tick.interval, tick_units = tolower(x.tick.units),
            tick_format = x.tick.format)
    sg <- sg_colors(sg, axis_color = global.font.color, tooltip_color = hovertext.font.color)

    # Set font of hovertext and tick labels
    js <- paste0("function(){
        document.querySelector('text').style.font = '", hovertext.font.size, "px ",
            hovertext.font.family, "';
        document.querySelector('text').style.fill = '", hovertext.font.color, "';
        var ticks = document.querySelectorAll('g .tick text');
        for (var i = 0; i < ticks.length; i++) {
            ticks[i].style.font = '", global.font.size, "px ", global.font.family, "';
            ticks[i].style.fill = '", global.font.color, "';
        }}")
    sg <- onRender(sg, js)

    # Override default of fixed size widget
    sg$sizingPolicy$browser$fill <- TRUE

    result <- list(htmlwidget = sg)
    class(result) <- "StandardChart"
    attr(result, "ChartType") <- "Area Stacked"
    result
}
