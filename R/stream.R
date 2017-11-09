#' Stream
#'
#' Create a Streamtraph, which is an area chart centerd around the x-axis rather than on top of it.
#' @param x A \code{matrix}, with columns containing the dates or other numeric x-axis variable.
#' @param colors The colors of the streams.
#' @param y.axis.show If FALSE, the y-axis is not shown.
#' @param y.tick.format The number format for the y-axis.
#' @param y.number.ticks The total number of ticks on the y-axis.
#' @param hover.decimals The number of decimals to show in hovers.
#' @param x.tick.format number format for the x-axis.
#' @param x.tick.units "Number", "Month" or "Year".
#' @param x.tick.interval The frequency of ticks on the x-axis. Where the data crosses multiple years, re-starts at each year.
#' @param margin.top Top margin (default should be fine, this allows for fine-tuning plot space)
#' @param margin.right Right margin (default should be fine, this allows for fine-tuning plot space)
#' @param margin.bottom Bottom margin (default should be fine, this allows for fine-tuning plot space)
#' @param margin.left Left margin (default should be fine, this allows for fine-tuning plot space)
#' @importFrom streamgraph streamgraph sg_fill_manual sg_axis_x sg_axis_y
#' @importFrom flipTime AsDate
#' @export
Stream <- function(x,
                   colors = c("#5C9AD3", "#ED7D31", "#A5A5A5", "#FFC000",
                              "#4473C5", "#70AD46", "#255F91", "#9E480D",
                              "#636365", "#987300", "#26408B", "#42682B"),
                   y.axis.show = TRUE,
                   y.tick.format = "",
                   y.number.ticks = 5,
                   hover.decimals = 2,
                   x.tick.format = "%d %b %y",
                   x.tick.units = "Month",
                   x.tick.interval = 1,
                   margin.top = 20,
                   margin.left = 50,
                   margin.bottom = 30,
                   margin.right = 40)
{
    if (!is.matrix(x) && !is.data.frame(x) && !is.array(x))
        stop("Stream graphs should have a tabular input (e.g., a matrix).")
    ErrorIfNotEnoughData(x)
    columns <- colnames(x)
    if (x.tick.format == "Number")
    {
        # x.tick.format = "%y"
        # x.tick.units = "Year"
        columns <- suppressWarnings(as.integer(columns))
        if (any(is.na((columns))))
            columns <- 1:ncol(x)
        # columns <- as.Date(paste0(columns, "/1/1"))
    }
    else
    {
        columns <- AsDate(columns, on.parse.failure = "silent")
    }
    x <- round(x, hover.decimals)
    df <- data.frame(value = as.numeric(t(x)), date = columns, key = rep(rownames(x), rep(ncol(x), nrow(x))))
    #print(df)
    sg <- streamgraph(data = df,
                key = "key",
                value = "value",
                date = "date",
                offset = "silhouette",
                interpolate = "cardinal",
                interactive = TRUE,
                scale = if(x.tick.format == "Number") "continuous" else "date",
                top = margin.top,
                right = margin.right,
                left = margin.left,
                bottom = margin.bottom)
    sg <- sg_fill_manual(sg, values = colors)
    if (!y.axis.show)
        sg <- sg_axis_y(sg, 0)
    else
        sg <- sg_axis_y(sg, tick_count = y.number.ticks, tick_format = y.tick.format)
    sg <- sg_axis_x(sg, tick_interval = x.tick.interval, tick_units = tolower(x.tick.units), tick_format = x.tick.format)#tick_format = if (is.date) x.tick.format else NULL)

    sg
    }
