#' TimeSeries
#'
#' Plots an interactive time series. Either one column or three columns may be provided. Three columns allows a shaded range
#' to be plotted around the central value. Columns must be in order of low, value, high.
#'
#' @param x Input data may be a matrix or a vector, wth dates as the rownames and data series along the columns.
#' @param colors Character; a named color from grDevices OR a hex value color.
#' @param global.font.family Character; font family for all occurrences of any
#' font attribute for the chart unless specified individually.
#' @param global.font.color Global font color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. #' rgb(0, 0, 0, maxColorValue = 255)).
#' @param title Character; chart title.
#' @param title.font.family Character; title font family.
#' @param title.font.color Title font color as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param title.font.size Title font size; default = 16.
#' @param x.title Character, x-axis title.
#' @param x.title.font.color x-axis title font color as a named color in character format (e.g. "black")
#' or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param x.title.font.family Character; x-axis title font family.
#' @param x.title.font.size x-axis title font size.
#' @param x.tick.font.color X-axis tick label font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param x.tick.font.family Character; x-axis tick label font family
#' @param x.tick.font.size x-axis tick label font size
#' @param y.title Character, y-axis title.
#' @param y.title.font.color y-axis title font color as a named color in character format (e.g. "black")
#' or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param y.title.font.family Character; y-axis title font family.
#' @param y.title.font.size y-axis title font size.
#' @param y.tick.font.color y-axis tick label font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param y.tick.font.family Character; y-axis tick label font family
#' @param y.tick.font.size y-axis tick label font size
#' @importFrom flipChartBasics ChartColors
#' @importFrom dygraphs dygraph dySeries dyCSS dyRangeSelector %>%
#' @export
TimeSeries <- function(x = NULL,
                    colors = ChartColors(1),
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
                    y.tick.font.size = 10
) {

    if(ncol(x) == 1)
    {
        label <- colnames(x)[1]
    }
    else if (ncol(x) == 3)
    {
        label <- colnames(x)[2]
    }
    else
        stop("Data must consist of one column or 3 columns in order of low, value, high.")

    # Controlling the formatting of the dygraphs via the CSS
    css <- paste0(".dygraph-title {
        color: ", title.font.color, ";
        font-size: ", title.font.size, "px;
        font-family: ", title.font.size, ";
        font-weight: bold;
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
        font-size", y.tick.font.size, "px;
        font-family: ", y.tick.font.family, ";
        }")

    write(css, "dygraph.css")
    start <- rownames(x)[length(rownames(x)) / 2]
    end <- rownames(x)[length(rownames(x))]

    dygraph(x, main = title, xlab = x.title, ylab = y.title) %>%
    dySeries(colnames(x), label = label, color = colors)  %>%
    dyCSS("dygraph.css")  %>%
    dyRangeSelector(fillColor = colors, dateWindow = c(as.POSIXct(start), as.POSIXct(end)))

}
