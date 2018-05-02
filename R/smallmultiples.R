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
#' @param legend.show Ignored except for with \code{GeographicMap}.
#' @param pad.top Numeric in [0,1]; Spacing above chart (between panels)
#' @param pad.bottom Numeric in [0,1]; Spacing below chart (between panels)
#' @param pad.left Numeric in [0,1]; Spacing to the left of chart (between panels)
#' @param pad.right Numeric in [0,1]; Spacing to the right chart (between panels)
#' @param ... Extra arguments passed to the charting function
#' @inherit Column
#' @importFrom plotly subplot
#' @export
SmallMultiples <- function(x,
                           chart.type = "Area",
                           nrows = 2,
                           pad.left = 0.01,
                           pad.right = 0.01,
                           pad.top = 0.01,
                           pad.bottom = 0.01,
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
                           legend.show = FALSE,
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
    {
        if (!is.numeric(x.order))
            x.order <- as.numeric(TextAsVector(x.order))
        if (any(is.na(x.order)) || any(x.order > ncol(x)))
            stop("x.order should be a comma separated list of indices (between 1 and ", ncol(x), ")")
        x <- x[, x.order]
    }
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
    title.ypos <- rep((nrows:1)/nrows, each = ncols)[1:npanels]
    title.xpos <- rep((1:ncols - 0.5)/ncols, nrows)[1:npanels]

    h.offset <- c(pad.top, rep(0, max(0, nrows - 2)), pad.bottom)[1:nrows]
    w.offset <- c(pad.left, rep(0, max(0, ncols - 2)), pad.right)[1:ncols]
    if (any(h.offset > 1/nrows))
        stop("'pad.top' and 'pad.bottom' should be between 0 and 1/nrows (", round(1/nrows, 4), ")")
    if (any(w.offset > 1/ncols))
        stop("'pad.left' and 'pad.bottom' should be between 0 and 1/ncols (", round(1/ncols, 4), ")")

    # Construct charts
    .bind_mean <- function(a, b, rev = FALSE)
    {
        if (is.null(b))
            return(a)
        else if (rev)
            return(cbind(Mean = b, a))
        else
            return(cbind(a, Mean = b))
    }

    if (chart.type == "Radar")
    {
        plot.list <- lapply(1:npanels, function(i){chart(.bind_mean(x[,i, drop = FALSE], average.series, rev = TRUE),
                                                     colors = c(average.color, colors[i]),
                                                     grid.show = FALSE, x.tick.show = FALSE,
                                                     data.label.show = data.label.show,
                                                     ...)$htmlwidget})
        margin.left <- 0
        margin.right <- 0
        margin.bottom <- 0
    }
    else if (chart.type == "GeographicMap")
    {
         plot.list <- lapply(1:npanels, function(i){chart(x[,i, drop = FALSE],
                                                     colors = colors,
                                                     mapping.package = "plotly",
                                                     legend.show = legend.show && (i == 1),
                                                     ...)$htmlwidget})
        margin.left <- 0
        margin.right <- 0
        margin.bottom <- 0
    }
    else
        plot.list <- lapply(1:npanels, function(i){chart(.bind_mean(x[,i, drop = FALSE], average.series),
                                                     colors = c(colors[i], average.color),
                                                     x.title = x.title, x.title.font.size = x.title.font.size,
                                                     y.title = y.title, y.title.font.size = y.title.font.size,
                                                     grid.show = grid.show,
                                                     x.tick.show = x.tick.show,
                                                     data.label.show = FALSE,
                                                     ...)$htmlwidget})

    is.geo <- chart.type == "GeographicMap"
    res <- subplot(plot.list, nrows = nrows, margin = c(pad.left,pad.right,pad.top,pad.bottom),
                   heights = rep(1/nrows, nrows) - h.offset, # compensate for plotly bug
                   widths = rep(1/ncols, ncols) - w.offset,
                   titleX = TRUE, titleY = TRUE, shareX = !is.geo, shareY = !is.geo)
    res <- layout(res, title = title, showlegend = is.geo,
                  annotations = list(text = title.list, x = title.xpos, y = title.ypos, showarrow = FALSE,
                                     xanchor = "center", yanchor = "top", xref = 'paper', yref = 'paper'),
                  titlefont = list(family = title.font.family, color = title.font.color, size = title.font.size),
                  margin = list(l = margin.left, r = margin.right, b = margin.bottom, t = margin.top))
    res
}