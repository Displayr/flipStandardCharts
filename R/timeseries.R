#' TimeSeries
#'
#' Plots interactive time series. Either multiple series may be plotted, or a single series with high and low
#' range/error bars.
#'
#' @inherit Area
#' @param x Input data may be a matrix or a vector, wth dates as the rownames and data series along the columns.
#' @param range.bars Logical; whether the data consists of a single series with low, value, high in the columns, or
#' multiple series.
#' @param colors Character; a named color from grDevices OR a hex value color.
#' @param line.thickness Integer; The width of the lines connecting data points.
#' @param legend.width Integer; Width (in pixels) of the legend.
#' @param window.start The number of days before the end of the data series to start the range selector window.
#' @param y.hovertext.font.color Legend font color as a named color in character
#' format (e.g. "black") or a hex code.
#' @param y.hovertext.font.family Character; legend font family.
#' @param y.hovertext.font.size Integer; Legend font size.
#' @importFrom flipChartBasics ChartColors
#' @importFrom dygraphs dygraph dySeries dyCSS dyRangeSelector %>% dyOptions dyLegend dyAxis
#' @importFrom flipTime AsDate AsDateTime
#' @importFrom xts xts
#' @export
TimeSeries <- function(x = NULL,
                    range.bars = FALSE,
                    colors = ChartColors(1),
                    line.thickness = NULL,
                    legend.width = 250,
                    window.start = NULL,
                    global.font.family = "Arial",
                    global.font.color = rgb(44, 44, 44, maxColorValue = 255),
                    title = "",
                    title.font.family = global.font.family,
                    title.font.color = global.font.color,
                    title.font.size = 16,
                    x.title = "",
                    x.title.font.color = global.font.color,
                    x.title.font.family = global.font.family,
                    x.title.font.size = 12,
                    x.tick.font.color = global.font.color,
                    x.tick.font.family = global.font.family,
                    x.tick.font.size = 10,
                    y.title = "",
                    y.title.font.color = global.font.color,
                    y.title.font.family = global.font.family,
                    y.title.font.size = 12,
                    y.tick.font.color = global.font.color,
                    y.tick.font.family = global.font.family,
                    y.tick.font.size = 10,
                    y.tick.format = "",
                    y.hovertext.font.size = 12,
                    y.hovertext.font.color = global.font.color,
                    y.hovertext.font.family = global.font.family,
                    y.hovertext.format= y.tick.format)
{

    if (is.null(dim(x)) || length(dim(x)) == 1L)
        x <- as.matrix(x)

    row.names <- AsDateTime(rownames(x), on.parse.failure = "silent")
    if (all(is.na(row.names)))
        stop("Time series requires that the row names of the data must be dates",
             " but the row names cannot be interpreted as dates.")
    is.time <- !all(format(row.names, format = "%H:%M:%S") == "00:00:00")
    rownames(x) <- as.character(row.names)

    if (range.bars)
    {
        if (ncol(x) != 3)
            stop("Data must consist of 3 columns containing low, central and high values.")
        x <- x[, order(apply(x, 2, mean, na.rm = TRUE))]
        label <- colnames(x)[2]
        colors <- colors[1]
    }

    names(colors) <- NULL # Remove names because named chr is (oddly!) ignored by dygraph

    range.end <- as.POSIXct(rownames(x)[length(rownames(x))])
    if (is.null(window.start))
        range.start <- as.POSIXct(rownames(x)[1])
    else
        range.start <- max(range.end - 60 * 60 * 24 * window.start, as.POSIXct(rownames(x)[1]))

    # Convert to an xts object with UTC timezone, or else this is done within dygraph which takes the
    # system timezone, which causes a difference between the data times and the x-axis times.
    if (is.time)
        x <- xts(x, order.by = AsDateTime(rownames(x)), tzone = "UTC")

    # Controlling the formatting of the dygraphs via the CSS
    css <- paste0(".dygraph-title {
        color: ", title.font.color, ";
        font-size: ", title.font.size, "px;
        font-family: ", title.font.family, ";
        font-weight: bold;
        }
        .dygraph-legend {
        color: ", y.hovertext.font.color, ";
        font-size: ", y.hovertext.font.size, "px;
        font-family: ", y.hovertext.font.family, ";
        }
        .dygraph-label.dygraph-xlabel {
        color: ", x.title.font.color, ";
        font-size: ", x.title.font.size, "px;
        font-family: ", x.title.font.family, ";
        }
        .dygraph-label.dygraph-ylabel {
        color: ", y.title.font.color, ";
        font-size: ", y.title.font.size, "px;
        font-family: ", y.title.font.family, ";
        }
        .dygraph-axis-label.dygraph-axis-label-x {
        color: ", x.tick.font.color, ";
        font-size: ", x.tick.font.size, "px;
        font-family: ", x.tick.font.family, ";
        }
        .dygraph-axis-label.dygraph-axis-label-y {
        color: ", y.tick.font.color, ";
        font-size: ", y.tick.font.size, "px;
        font-family: ", y.tick.font.family, ";
        }")

    write(css, "dygraph.css")

    dg <- dygraph(x, main = title, xlab = x.title, ylab = y.title)
    dg <- dyAxis(dg, "y",
        valueFormatter = if (percentFromD3(y.hovertext.format)) 'function(d){return Math.round(d*100) + "%"}' else NULL,
        axisLabelFormatter = if (percentFromD3(y.tick.format)) 'function(d){return Math.round(d*100) + "%"}' else NULL)
    if (range.bars)
    {
        dg <- dySeries(dg, colnames(x), label = colnames(x)[2], color = colors, strokeWidth = line.thickness)
        dg <- dyOptions(dg, useDataTimezone = is.time)
    }
    else
        dg <- dyOptions(dg, colors = colors, strokeWidth = line.thickness, useDataTimezone = is.time)
    dg <- dyCSS(dg, "dygraph.css")

    if (!range.bars && ncol(x) != 1)
        colors <- "#888888"
    dg <- dyRangeSelector(dg, fillColor = colors, dateWindow = c(range.start, range.end))
    dg <- dyLegend(dg, width = legend.width)

    result <- list(htmlwidget = dg)
    class(result) <- "StandardChart"
    result
}
