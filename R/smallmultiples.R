#' Plotting data as a small multiples
#' @description A small multiple (sometimes called trellis chart, lattice chart,
#'    grid chart, or panel chart) is a series of similar graphs or charts using
#'    the same scale and axes, allowing them to be easily compared.
#' @param x Input data as a matrix or dataframe
#' @param chart.type Can be one of "Area", "Column", "Bar" or "Line"
#' @param nrows Integer; Number of rows to arrange the charts in
#' @param x.order A vector containing the list index of the columns in the order which they are to be shown
#' @param average.show Logical; whether to show a second series in each panel containing
#'     the data averaged across all series.
#' @param average.color The color in which the average series should be displayed
#' @param titles.wrap Logical; whether the panel titles should be wrapped.
#' @param titles.wrap.nchar Number of characters (approximately) in each line
#'     of the panel titles when \code{titles.wordwrap} \code{TRUE}.
#' @param pad Numeric; spacing between each panel in the chart.
#' @param ... Extra arguments passed to the charting function
#' @inherit Column
#' @importFrom plotly subplot
#' @export
SmallMultiples <- function(x,
                           chart.type = "Area",
                           nrows = 2,
                           x.order = NULL,
                           average.show = FALSE,
                           average.color = rgb(80, 80, 80, maxColorValue = 255),
                           titles.wrap = TRUE,
                           titles.wrap.nchar = 20,
                           colors = ChartColors(max(1, ncol(x), na.rm = TRUE)),
                           global.font.family = "Arial",
                           global.font.color = rgb(44, 44, 44, maxColorValue = 255),
                           title = "",
                           title.font.family = global.font.family,
                           title.font.color = global.font.color,
                           title.font.size = 16,
                           x.title = "",
                           x.title.font.size = 12,
                           y.title = "",
                           y.title.font.size = 12,
                           grid.show = TRUE,
                           x.tick.show = TRUE,
                           data.label.show = FALSE,
                           pad = 0.01,
                           margin.left = NULL,
                           margin.right = NULL,
                           margin.top = NULL,
                           margin.bottom = NULL,
                           subtitle = "", # parameters to discard
                           subtitle.font.family = global.font.family,
                           subtitle.font.color = global.font.color,
                           subtitle.font.size = 12,
                           footer = "",
                           footer.font.family = global.font.family,
                           footer.font.color = global.font.color,
                           footer.font.size = 8,
                           footer.wrap = TRUE,
                           footer.wrap.nchar = 100,
                           ...)
{
    # Subplot has problems with the placement of GeographicMap and Radar
    # Arguments which also cannot be used: data labels, subtitle, footer
    chart <- get0(chart.type)
    eval(colors)

    # Data manipulation
    if (!is.null(x.order))
        x <- x[, x.order]
    title.list <- autoFormatLongLabels(colnames(x), titles.wrap, titles.wrap.nchar)
    average.series <- NULL
    if (average.show)
        average.series <- apply(x, 1, mean)
    else
        average.color <- NULL

    # Layout and positioning
    if (is.null(margin.top))
        margin.top <- 20 + title.font.size * (sum(nchar(title) > 0, na.rm = TRUE))
    if (is.null(margin.bottom))
        margin.bottom <- 30 + x.title.font.size * (sum(nchar(x.title) > 0, na.rm = TRUE))
    if (is.null(margin.left))
        margin.left <- 30 + y.title.font.size * (sum(nchar(y.title) > 0, na.rm = TRUE))
    if (is.null(margin.right))
        margin.right <- 20

    npanels <- ncol(x)
    if (npanels <= 1)
        stop("Multiple series are required for Small Multiples.")
    ncols <- ceiling(npanels/nrows)
    tmp <- (nrows:1)/nrows
    if (nrows >= 2)
        tmp <- tmp + c(pad, rep(0, nrows - 2), -pad)/nrows
    title.ypos <- rep(tmp, each = ncols)[1:npanels]
    tmp <- (1:ncols - 0.5)/ncols
    if (ncols >= 2)
        tmp <- tmp + c(-pad, rep(0, ncols - 2), pad)
    title.xpos <- rep(tmp, nrows)[1:npanels]

    if (chart.type == "Radar")
    {
        plot.list <- lapply(1:npanels, function(i){chart(cbind(Mean = average.series, x[,i, drop = FALSE]),
                                                     colors = c(average.color, colors[i]),
                                                     grid.show = FALSE, x.tick.show = FALSE,
                                                     data.label.show = data.label.show,
                                                     ...)$htmlwidget})
        margin.left <- 0
        margin.right <- 0
        margin.bottom <- 0
    }
    #else if (chart.type == "GeographicMap")
    #{
    #     plot.list <- lapply(1:npanels, function(i){chart(x[,i, drop = FALSE],
    #                                                 colors = c(average.color, colors[i]),
    #                                                 mapping.package = "plotly",
    #                                                 ...)$htmlwidget})
    #    margin.left <- 0
    #    margin.right <- 0
    #    margin.bottom <- 0
    #}
    else
        plot.list <- lapply(1:npanels, function(i){chart(cbind(x[,i, drop = FALSE], Mean = average.series),
                                                     colors = c(colors[i], average.color),
                                                     x.title = x.title, x.title.font.size = x.title.font.size,
                                                     y.title = y.title, y.title.font.size = y.title.font.size,
                                                     grid.show = grid.show,
                                                     x.tick.show = x.tick.show,
                                                     data.label.show = FALSE,
                                                     ...)$htmlwidget})
    res <- subplot(plot.list, nrows = nrows, margin = pad,
                   titleX = TRUE, titleY = TRUE, shareX = TRUE, shareY = TRUE)
    res <- layout(res,
                  annotations = list(text = title.list, x = title.xpos, y = title.ypos, showarrow = FALSE,
                                     xanchor = "center", yanchor = "top", xref = 'paper', yref = 'paper'),
                  showlegend = FALSE, title = title,
                  titlefont = list(family = title.font.family, color = title.font.color, size = title.font.size),
                  margin = list(l = margin.left, r = margin.right, b = margin.bottom, t = margin.top))
    res
}
