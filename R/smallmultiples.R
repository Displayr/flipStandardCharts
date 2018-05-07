#' Plotting data as a small multiples
#' @description A small multiple (sometimes called trellis chart, lattice chart,
#'    grid chart, or panel chart) is a series of similar graphs or charts using
#'    the same scale and axes, allowing them to be easily compared.
#' @param x Input data as a matrix or dataframe
#' @param chart.type Can be one of "Area", "Column", "Bar", "Line", "Radar" or "Geographic Map".
#' @param nrows Integer; Number of rows to arrange the charts in
#' @param x.order A vector containing the list index of the columns in the order which they are to be shown
#' @param share.axes Force range of the plot to be the same across all panels.
#' @param average.show Logical; whether to show a second series in each panel containing
#'     the data averaged across all series.
#' @param average.color The color in which the average series should be displayed
#' @param paneltitle.show Logical; whether to show a title for each panel.
#' @param paneltitle.font.family Font family of panel titles.
#' @param paneltitle.font.color Font color of panel titles.
#' @param paneltitle.font.size Font size of panel titles.
#' @param paneltitle.wrap Logical; whether the panel title should be wrapped.
#' @param paneltitle.wrap.nchar Number of characters (approximately) in each line
#'     of the panel title when \code{paneltitle.wordwrap} \code{TRUE}.
#' @param legend.show Ignored except for with \code{GeographicMap}.
#' @param pad.top Numeric in [0,1]; Spacing above chart (between panels)
#' @param pad.bottom Numeric in [0,1]; Spacing below chart (between panels)
#' @param pad.left Numeric in [0,1]; Spacing to the left of chart (between panels)
#' @param pad.right Numeric in [0,1]; Spacing to the right chart (between panels)
#' @param mapping.package Not used.
#' @param ... Extra arguments passed to the charting function
#' @inherit Column
#' @inherit GeographicMap
#' @importFrom plotly subplot
#' @export
SmallMultiples <- function(x,
                           chart.type = "Area",
                           nrows = 2,
                           share.axes = TRUE,
                           pad.left = 0.01,
                           pad.right = 0.01,
                           pad.top = 0.01,
                           pad.bottom = 0.01,
                           x.order = NULL,
                           average.show = FALSE,
                           average.color = rgb(230, 230, 230, maxColorValue = 255),
                           y.bounds.maximum = NULL,
                           y.bounds.minimum = NULL,
                           x.bounds.maximum = NULL,
                           x.bounds.minimum = NULL,
                           values.bounds.maximum = NULL,
                           values.bounds.minimum = NULL,
                           colors = ChartColors(max(1, ncol(x), na.rm = TRUE)),
                           global.font.family = "Arial",
                           global.font.color = rgb(44, 44, 44, maxColorValue = 255),
                           title = "",
                           title.font.family = global.font.family,
                           title.font.color = global.font.color,
                           title.font.size = 16,
                           paneltitle.show = TRUE,
                           paneltitle.font.family = global.font.family,
                           paneltitle.font.color = global.font.color,
                           paneltitle.font.size = 14,
                           paneltitle.wrap = TRUE,
                           paneltitle.wrap.nchar = 20,
                           x.title = "",
                           x.title.font.size = 12,
                           y.title = "",
                           y.title.font.size = 12,
                           data.label.show = FALSE,
                           grid.show = TRUE,
                           x.tick.show = TRUE,
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
                           mapping.package = "plotly", # discarded
                           ...)
{
    # Subplot has problems with the placement of GeographicMap and Radar
    # Arguments which also cannot be used: data labels, subtitle, footer
    chart.type <- gsub(" ", "", chart.type)
    chart <- get0(chart.type, mode = "function")
    eval(colors)

    # Data manipulation
    if (!is.null(x.order))
    {
        if (!is.numeric(x.order))
            x.order <- as.numeric(TextAsVector(x.order))
        if (any(is.na(x.order)) || any(x.order > ncol(x)))
            stop("x.order should be a comma separated list of indices (between 1 and ", ncol(x), ")")
        if (is.numeric(x.order) && length(x.order) > 0)
            x <- x[, x.order]
    }
    values.max = max(unlist(x), na.rm = TRUE)
    values.min = min(0, unlist(x), na.rm = TRUE)
    if (share.axes && chart.type == "Bar" && is.null(x.bounds.maximum))
        x.bounds.maximum <- values.max
    if (share.axes && chart.type != "Bar" && is.null(y.bounds.maximum))
        y.bounds.maximum <- values.max
    if (share.axes && chart.type == "Bar" && is.null(x.bounds.minimum))
        x.bounds.minimum <- values.min
    if (share.axes && chart.type != "Bar" && is.null(y.bounds.minimum))
        y.bounds.minimum <- values.min
    if (share.axes && chart.type == "GeographicMap" && is.null(values.bounds.maximum))
        values.bounds.maximum <- values.max
    if (share.axes && chart.type == "GeographicMap" && is.null(values.bounds.minimum))
        values.bounds.minimum <- values.min

    average.series <- NULL
    if (chart.type != "GeographicMap" && average.show)
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
    h.offset <- c(pad.top, rep(0, max(0, nrows - 2)), pad.bottom)[1:nrows]
    w.offset <- c(pad.left, rep(0, max(0, ncols - 2)), pad.right)[1:ncols]
    if (any(h.offset > 1/nrows))
        stop("'pad.top' and 'pad.bottom' should be between 0 and 1/nrows (", round(1/nrows, 4), ")")
    if (any(w.offset > 1/ncols))
        stop("'pad.left' and 'pad.bottom' should be between 0 and 1/ncols (", round(1/ncols, 4), ")")

    # Position titles for each panel
    paneltitles <- NULL
    if (paneltitle.show && !is.null(colnames(x)))
    {
        title.list <- autoFormatLongLabels(colnames(x), paneltitle.wrap, paneltitle.wrap.nchar)
        titles.ypos <- rep((nrows:1)/nrows, each = ncols)[1:npanels]
        titles.xpos <- rep((1:ncols - 0.5)/ncols, nrows)[1:npanels]
        paneltitles <- list(text = title.list, x = titles.xpos, y = titles.ypos,
                            showarrow = FALSE, xanchor = "center", yanchor = "top",
                            font = list(family = paneltitle.font.family, color = paneltitle.font.color,
                            size = paneltitle.font.size), xref = 'paper', yref = 'paper')
    }

    # Construct charts
    .bind_mean <- function(a, b, rev = FALSE)
    {
        if (is.null(b))
            return(a)
        else if (rev)
            return(cbind(Average = b, a))
        else
            return(cbind(a, Average = b))
    }

    if (chart.type == "Radar")
    {
        plot.list <- lapply(1:npanels, function(i){chart(.bind_mean(x[,i, drop = FALSE], average.series),
                                                     hovertext.show = c(TRUE, TRUE),
                                                     colors = c(colors[i], average.color),
                                                     grid.show = FALSE, x.tick.show = FALSE,
                                                     data.label.show = c(data.label.show, FALSE),
                                                     series.line.width = c(3,0),
                                                     y.bounds.maximum = y.bounds.maximum,
                                                     global.font.family = global.font.family,
                                                     global.font.color = global.font.color,
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
                                                     values.bounds.maximum = values.bounds.maximum,
                                                     values.bounds.minimum = values.bounds.minimum,
                                                     global.font.family = global.font.family,
                                                     global.font.color = global.font.color,
                                                     ...)$htmlwidget})
        margin.left <- 0
        margin.right <- 0
        margin.bottom <- 0
    } else if (chart.type == "Bar" || chart.type == "Column")
        plot.list <- lapply(1:npanels, function(i){chart(x[,i, drop = FALSE],
                                                     colors = colors[i],
                                                     average.series = average.series,
                                                     average.color = average.color,
                                                     x.title = x.title, x.title.font.size = x.title.font.size,
                                                     y.title = y.title, y.title.font.size = y.title.font.size,
                                                     grid.show = grid.show, data.label.show = data.label.show,
                                                     x.tick.show = x.tick.show,
                                                     y.bounds.maximum = y.bounds.maximum,
                                                     y.bounds.minimum = y.bounds.minimum,
                                                     x.bounds.maximum = x.bounds.maximum,
                                                     x.bounds.minimum = x.bounds.minimum,
                                                     global.font.family = global.font.family,
                                                     global.font.color = global.font.color,
                                                     ...)$htmlwidget})

    else
        plot.list <- lapply(1:npanels, function(i){chart(.bind_mean(x[,i, drop = FALSE], average.series),
                                                     colors = c(colors[i], average.color),
                                                     x.title = x.title, x.title.font.size = x.title.font.size,
                                                     y.title = y.title, y.title.font.size = y.title.font.size,
                                                     grid.show = grid.show, data.label.show = data.label.show,
                                                     x.tick.show = x.tick.show,
                                                     y.bounds.maximum = y.bounds.maximum,
                                                     y.bounds.minimum = y.bounds.minimum,
                                                     x.bounds.maximum = x.bounds.maximum,
                                                     x.bounds.minimum = x.bounds.minimum,
                                                     global.font.family = global.font.family,
                                                     global.font.color = global.font.color,
                                                     ...)$htmlwidget})

    is.geo <- chart.type == "GeographicMap"
    res <- subplot(plot.list, nrows = nrows, margin = c(pad.left,pad.right,pad.top,pad.bottom),
                   heights = rep(1/nrows, nrows) - h.offset, # compensate for plotly bug
                   widths = rep(1/ncols, ncols) - w.offset, titleX = TRUE, titleY = TRUE,
                   shareX = share.axes && !is.geo, shareY = share.axes && !is.geo)
    res <- layout(res, title = title, showlegend = is.geo, annotations = paneltitles,
                  titlefont = list(family = title.font.family, color = title.font.color, size = title.font.size),
                  margin = list(l = margin.left, r = margin.right, b = margin.bottom, t = margin.top))
    res
}
