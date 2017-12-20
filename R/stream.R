#' Plot data as a Streamgraph
#'
#' Create a Streamtraph, which is an area chart centered around the x-axis rather than on top of it.
#' @param x A \code{matrix}, with columns containing the dates or other numeric x-axis variable.
#' @param colors A vector of colors of the streams.
#' @param y.axis.show Logical; if FALSE, the y-axis is not shown.
#' @param y.tick.format A string representing a d3 formatting code for the y-axis.
#' See https://github.com/d3/d3/blob/master/API.md#number-formats-d3-format
#' @param y.number.ticks The total number of ticks on the y-axis.
#' @param values.hovertext.format A string representing a d3 formatting code for the hover text.
#' Only the decimal places are used.
#' @param x.tick.format A string representing a d3 formatting code for the x.axis.
#' See https://github.com/d3/d3/blob/master/API.md#number-formats-d3-format
#' @param x.tick.units "Automatic", "Number", "Month" or "Year".
#' @param x.tick.interval The frequency of ticks on the x-axis. Where the data crosses multiple years, re-starts at each year.
#' @param margin.top Top margin (default should be fine, this allows for fine-tuning plot space)
#' @param margin.right Right margin (default should be fine, this allows for fine-tuning plot space)
#' @param margin.bottom Bottom margin (default should be fine, this allows for fine-tuning plot space)
#' @param margin.left Left margin (default should be fine, this allows for fine-tuning plot space)
#' @importFrom streamgraph streamgraph sg_fill_manual sg_axis_x sg_axis_y
#' @importFrom flipTime AsDateTime
#' @export
Stream <- function(x,
                   colors = c("#5C9AD3", "#ED7D31", "#A5A5A5", "#FFC000",
                              "#4473C5", "#70AD46", "#255F91", "#9E480D",
                              "#636365", "#987300", "#26408B", "#42682B"),
                   y.axis.show = TRUE,
                   y.tick.format = "",
                   y.number.ticks = 5,
                   values.hovertext.format = "",
                   x.tick.format = "%d %b %y",
                   x.tick.units = "Automatic",
                   x.tick.interval = 1,
                   margin.top = 20,
                   margin.left = 50,
                   margin.bottom = 30,
                   margin.right = 40)
{
    if (is.null(dim(x)) || length(dim(x)) == 1L) {
        x <- as.matrix(x)
        colnames(x) <- ""
    }
    if (!is.matrix(x) && !is.data.frame(x) && !is.array(x))
        stop("Stream graphs should have a tabular input (e.g., a matrix).")

    # streamgraph requires dates along the columns but for consistency with Time Series, Line, Google Trends etc
    # CChart produces dates along the rows, hence we transpose
    x <- t(x)

    ErrorIfNotEnoughData(x)
    columns <- colnames(x)

    if (x.tick.units == "Automatic") {
        if (getAxisType(columns, x.tick.format) == "date")
            x.tick.units <- "Month"
        else
            x.tick.units <- "Number"
    }

    if (x.tick.units == "Number")
    {
        if (x.tick.format == "")
            x.tick.format = ".0f"
        if (d3FormatType(x.tick.format) != "numeric" || getAxisType(columns, x.tick.format) != "numeric")
            stop("x-axis tick format and units are incompatible.")
        columns <- suppressWarnings(as.integer(columns))
        if (any(is.na((columns))))
            columns <- 1:ncol(x)
    }
    else
    {
        if (x.tick.format == "")
            x.tick.format = "%d %b %y"
        if (d3FormatType(x.tick.format) != "date" || getAxisType(columns, x.tick.format) != "date")
            stop("x-axis tick format and units are incompatible.")
        columns <- AsDateTime(columns, on.parse.failure = "silent")
    }

    x <- round(x, decimalsFromD3(values.hovertext.format, 2))
    df <- data.frame(value = as.numeric(t(x)), date = columns, key = rep(rownames(x), rep(ncol(x), nrow(x))))

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
    sg <- sg_axis_x(sg, tick_interval = x.tick.interval, tick_units = tolower(x.tick.units), tick_format = x.tick.format)

    # Override default of fixed size widget
    sg$sizingPolicy$browser$fill <- TRUE

    sg
    }
