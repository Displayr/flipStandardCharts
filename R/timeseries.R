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
#'      Deprecated. Use \code{legend.position.x} instead.
#' @param window.start The number of days before the end of the data series to start the range selector window.
#' @param hovertext.font.color Legend font color as a named color in character
#' format (e.g. "black") or a hex code.
#' @param hovertext.font.family Character; legend font family.
#' @param hovertext.font.size Integer; Legend font size.
#' @param y.hovertext.prefix String to prepend to hovertext showing y-values.
#' @param y.hovertext.suffix String to append to hovertext showing y-values.
#' @importFrom flipChartBasics ChartColors
#' @importFrom dygraphs dygraph dySeries dyCSS dyRangeSelector %>% dyOptions dyLegend dyAxis
#' @importFrom flipTime AsDate AsDateTime
#' @importFrom flipU IsRServer
#' @importFrom xts xts
#' @importFrom htmlwidgets onRender
#' @export
TimeSeries <- function(x = NULL,
                    range.bars = FALSE,
                    colors = NULL,
                    line.thickness = NULL,
                    legend.width = NULL,
                    legend.orientation = "Horizontal",
                    legend.position.x = NULL,
                    legend.fill.color = "transparent",
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
                    y.tick.prefix = "",
                    y.tick.suffix = "",
                    y.bounds.minimum = NULL,
                    y.bounds.maximum = NULL,
                    hovertext.font.size = 11,
                    hovertext.font.color = global.font.color,
                    hovertext.font.family = global.font.family,
                    y.hovertext.format = y.tick.format,
                    y.hovertext.prefix = y.tick.prefix,
                    y.hovertext.suffix = y.tick.suffix)
{

    if (!is.list(x) && (is.array(x) || is.numeric(x)))
        x <- checkMatrixNames(x)
    else if (is.data.frame(x))
        x <- as.matrix(x)

    if (is.null(colors))
        colors <- ChartColors(ncol(x))

    row.names <- AsDateTime(rownames(x), on.parse.failure = "silent")
    if (all(is.na(row.names)))
    {
        if (IsRServer())
            stop("Time series charts require dates to be supplied as the row names ",
                "if the Data source is a table; the first column if the data is pasted or typed; ",
                "or the first variable if variables are provided as the Data source.")
        else
            stop("Row names of input data could not be interpreted as dates.")
    }
    is.time <- !all(format(row.names, format = "%H:%M:%S") == "00:00:00")
    rownames(x) <- as.character(row.names)

    # Make sure input data is ordered - this is required for dygraphs
    ord <- order(row.names)
    x <- x[ord,,drop = FALSE]

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
        color: ", hovertext.font.color, ";
        font-size: ", hovertext.font.size, "px;
        font-family: ", hovertext.font.family, ";
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

    y.bounds.minimum <- charToNumeric(y.bounds.minimum)
    if (is.null(y.bounds.minimum))
        y.bounds.minimum <- min(0, x)
    y.tick.format <- checkD3Format(y.tick.format, "numeric", warning.type = "Y axis tick")
    if (nchar(y.hovertext.format) == 0)
        y.hovertext.format <- y.tick.format
    else
        y.hovertext.format <- checkD3Format(y.hovertext.format, "numeric", warning.type = "Hover text")
    medium.values <- all(as.numeric(x) > 1 && as.numeric(x) < 1e5)
    dg <- dyAxis(dg, "y",
        valueRange = c(charToNumeric(y.bounds.minimum), charToNumeric(y.bounds.maximum)),
        valueFormatter = tickFormat(y.hovertext.format, y.hovertext.prefix, y.hovertext.suffix, medium.values),
        axisLabelFormatter =  tickFormat(y.tick.format, y.tick.prefix, y.tick.suffix, medium.values)
    )

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
    dg <- dyLegend(dg, labelsSeparateLines = tolower(substr(legend.orientation,1,1)) == "v")

    top.offset <- 0
    if (sum(nchar(title), na.rm = TRUE) > 0)
        top.offset <- title.font.size + hovertext.font.size

    width.constraint <- ""
    if (!is.null(legend.position.x) && legend.position.x < 1.0)
        width.constraint <- paste0("document.querySelector('.dygraph-legend').style.left = '",
                            legend.position.x * 100, "%';")

    js <- paste0("function(){
        var elem = document.querySelector('.dygraph-legend');
        elem.removeAttribute('style', 'width');
        document.querySelector('.dygraph-legend').style.font = '", hovertext.font.size, "px ",
            hovertext.font.family, "';
        document.querySelector('.dygraph-legend').style.backgroundColor = '", legend.fill.color, "';
        document.querySelector('.dygraph-legend').style.position = 'absolute';
        document.querySelector('.dygraph-legend').style.right = '0px';
        document.querySelector('.dygraph-legend').style.top = '", top.offset, "px';",
        width.constraint, "
        }")
    dg <- onRender(dg, js)

    result <- list(htmlwidget = dg)
    class(result) <- "StandardChart"
    attr(result, "ChartType") <- "Line"
    result
}

tickFormat  <- function(format.str, prefix, suffix, default.medium.values = TRUE)
{
    # Avoid showing 200 in scientific notation
    if (sum(nchar(format.str), na.rm = TRUE) == 0 && default.medium.values)
        format.str <- ".0f"

    # Set decimal places if none supplied
    # This avoids getting scientific notation with 8 decimal places
    if (!grepl("[0-9]", format.str))
    {
        has.comma <- grepl(",", format.str, fixed = TRUE)
        format.str <- gsub(",", "", format.str)
        format.str <- paste0(".", if (grepl("%$", format.str)) 0 else 2, format.str)
        if (has.comma)
            format.str <- paste0(",", format.str)
    }

    return(sprintf("function(value) { return ('%s' + window.d3format.getOrCreate('%s')(value) + '%s'); }",
            prefix, format.str, suffix))
}
