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
#' @param scatter.groups.column The column of \code{x} which is used to aggregate
#'   the data for small multiples. By default this is the last column in \code{x}
#' @param mapping.package Not used.
#' @param ... Extra arguments passed to the charting function
#' @inherit Column
#' @inherit Scatter
#' @inherit GeographicMap
#' @importFrom plotly subplot
#' @export
#' @examples
#' x <- matrix(1:21, 7, 3, dimnames = list(letters[1:7], LETTERS[1:3]))
#' SmallMultiples(x, "Column", colors=c("red","green","blue"))
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
                           average.color = rgb(153, 153, 153, maxColorValue = 255),
                           y.bounds.maximum = NULL,
                           y.bounds.minimum = NULL,
                           x.bounds.maximum = NULL,
                           x.bounds.minimum = NULL,
                           values.bounds.maximum = NULL,
                           values.bounds.minimum = NULL,
                           colors = ChartColors(max(1, ncol(x), na.rm = TRUE)),
                           fit.line.colors = colors,
                           fit.CI.colors = colors,
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
                           x.tick.angle = NULL,
                           legend.show = TRUE,
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
                           scatter.x.column = 1,
                           scatter.y.column = 2,
                           scatter.sizes.column = 3,
                           scatter.colors.column = 4,
                           scatter.groups.column = NULL,
                           scatter.colors.as.categorical = TRUE,
                           ...)
{
    # Subplot has problems with the placement of GeographicMap and Radar
    # Arguments which also cannot be used: data labels, subtitle, footer
    chart.type <- gsub(" ", "", chart.type)
    chart <- get0(chart.type, mode = "function")
    eval(colors)
    if (is.null(fit.line.colors))
        fit.line.colors <- colors

    if (chart.type == "Scatter")
    {
        if (sum(scatter.groups.column, na.rm = TRUE) <= 0)
            scatter.groups.column <- NCOL(x)

        if (sum(scatter.groups.column, na.rm = TRUE) <= 0 || NCOL(x) < scatter.groups.column)
            scatter.groups.column <- NCOL(x)
        if (isTRUE(unname(scatter.colors.column == scatter.groups.column)))
            scatter.colors.column <- 0
        if (isTRUE(unname(scatter.sizes.column == scatter.groups.column)))
            scatter.sizes.column <- 0
        indexes <- tapply(1:nrow(x), x[,scatter.groups.column], function(ii) ii)
        npanels <- length(indexes)

    } else
        npanels <- NCOL(x)

    if (npanels <= 1)
        stop("Small Multiples can only be used for data containing multiple series.")

    # Data manipulation
    if (!is.null(x.order))
    {
        if (!is.numeric(x.order))
            x.order <- suppressWarnings(as.numeric(TextAsVector(x.order)))
        if (any(is.na(x.order)) || any(x.order > npanels))
            stop("'Order' should be a comma separated list of indices (between 1 and ", npanels, ")")
        if (is.numeric(x.order) && length(x.order) > 0)
        {
            if (chart.type == "Scatter")
                indexes <- indexes[x.order]
            else
                x <- x[, x.order]
            npanels <- length(x.order)
        }
    }
    if (length(colors) < npanels)
        colors <- paste0(rep("", npanels), colors)

    all.values <- if (chart.type == "Scatter") x[,scatter.y.column]
                  else unlist(x)
    values.max = max(0, all.values, na.rm = TRUE)
    values.min = min(0, all.values, na.rm = TRUE)
    values.bounds.minimum <- charToNumeric(values.bounds.minimum)
    values.bounds.maximum <- charToNumeric(values.bounds.maximum)
    x.bounds.minimum <- charToNumeric(x.bounds.minimum)
    x.bounds.maximum <- charToNumeric(x.bounds.maximum)
    y.bounds.minimum <- charToNumeric(y.bounds.minimum)
    y.bounds.maximum <- charToNumeric(y.bounds.maximum)

    if (share.axes)
    {
        if (chart.type == "Scatter" && is.numeric(x[,scatter.x.column]))
        {
            xvals <- x[,scatter.x.column]
            if (is.null(x.bounds.minimum))
                x.bounds.minimum <- min(xvals, na.rm = TRUE)
            if (is.null(x.bounds.maximum))
                x.bounds.maximum <- max(xvals, na.rm = TRUE)
        }
        if (chart.type == "GeographicMap")
        {
            values.bounds.maximum <- max(values.bounds.maximum, values.max)
            values.bounds.minimum <- min(values.bounds.minimum, values.min)
        }
        else if (chart.type == "Bar")
        {
            if (is.null(x.bounds.maximum))
                x.bounds.maximum <- values.max
            if (is.null(x.bounds.minimum))
                x.bounds.minimum <- values.min
        }
        else
        {
            if (is.null(y.bounds.maximum))
                y.bounds.maximum <- values.max
            if (is.null(y.bounds.minimum))
                y.bounds.minimum <- values.min
        }
    }

    if (is.null(x.tick.angle) && chart.type %in% c("Column", "Area", "Line") &&
        max(nchar(rownames(x)), 0) > 3)
        x.tick.angle <- 90

    average.series <- NULL
    if (chart.type != "GeographicMap" && average.show)
        average.series <- apply(x, 1, mean)
    else
        average.color <- NULL

    # Layout and positioning
    if (is.null(margin.top) || is.na(margin.top))
        margin.top <- 20 + title.font.size * (sum(nchar(title) > 0, na.rm = TRUE))
    if (is.null(margin.bottom) || is.na(margin.bottom))
        margin.bottom <- 30 + x.title.font.size * (sum(nchar(x.title) > 0, na.rm = TRUE))
    if (is.null(margin.left) || is.na(margin.left))
        margin.left <- 30 + y.title.font.size * (sum(nchar(y.title) > 0, na.rm = TRUE))
    if (is.null(margin.right) || is.na(margin.right))
        margin.right <- 20

    ncols <- ceiling(npanels/nrows)
    h.offset <- 0
    w.offset <- 0
    if (chart.type != "Radar")
    {
        w.offset <- c(pad.left, rep(0, max(0, ncols - 2)), pad.right)[1:ncols]
        if (any(w.offset >= 1/ncols))
            stop("'pad.left' and 'pad.bottom' should be between 0 and 1/ncols (", round(1/ncols, 4), ")")
    }
    h.offset <- c(pad.top, rep(0, max(0, nrows - 2)), pad.bottom)[1:nrows]
    if (any(h.offset >= 1/nrows))
        stop("'pad.top' and 'pad.bottom' should be between 0 and 1/nrows (", round(1/nrows, 4), ")")

    # Position titles for each panel
    paneltitles <- NULL
    titles <- if (chart.type == "Scatter") names(indexes)
              else                         colnames(x)
    if (paneltitle.show && !is.null(titles))
    {
        title.list <- autoFormatLongLabels(titles, paneltitle.wrap, paneltitle.wrap.nchar)
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


    if (chart.type == "Scatter")
    {
        if (average.show)
            warning("Averages cannot be shown for small multiples with scatterplot.")
        sz.min <- NULL
        sz.max <- NULL
        if (!is.null(scatter.sizes.column) && !is.na(scatter.sizes.column) &&
            scatter.sizes.column > 0 && scatter.sizes.column <= NCOL(x))
        {
            sc.tmp <- abs(AsNumeric(x[,scatter.sizes.column], binary = FALSE))
            sz.min <- min(sc.tmp, na.rm = TRUE)
            sz.max <- max(sc.tmp, na.rm = TRUE)
        }
        col.min <- NULL
        col.max <- NULL
        if (!is.null(scatter.colors.column) && !is.na(scatter.colors.column) &&
            scatter.colors.column > 0 && scatter.colors.column <= NCOL(x))
        {
            if (!is.numeric(x[,scatter.colors.column]))
                x[,scatter.colors.column] <- as.factor(x[,scatter.colors.column])
            col.tmp <- AsNumeric(x[,scatter.colors.column], binary = FALSE)
            col.min <- min(col.tmp, na.rm = TRUE)
            col.max <- max(col.tmp, na.rm = TRUE)
            colors <- rep(list(colors), npanels) # use the whole palette in each panel
        } else
            colors <- as.list(colors)

        plot.list <- lapply(1:npanels, function(i){chart(x[indexes[[i]],],
                                                     scatter.x.column = scatter.x.column,
                                                     scatter.y.column = scatter.y.column,
                                                     scatter.sizes.column = scatter.sizes.column,
                                                     scatter.colors.column = scatter.colors.column,
                                                     scatter.colors.as.categorical = FALSE,
                                                     colors = colors[[i]],
                                                     fit.line.colors = fit.line.colors[i],
                                                     fit.CI.colors = fit.line.colors[i],
                                                     x.title = x.title, x.title.font.size = x.title.font.size,
                                                     y.title = y.title, y.title.font.size = y.title.font.size,
                                                     grid.show = grid.show, data.label.show = data.label.show,
                                                     x.tick.show = x.tick.show, x.tick.angle = x.tick.angle,
                                                     y.bounds.maximum = y.bounds.maximum,
                                                     y.bounds.minimum = y.bounds.minimum,
                                                     x.bounds.maximum = x.bounds.maximum,
                                                     x.bounds.minimum = x.bounds.minimum,
                                                     global.font.family = global.font.family,
                                                     global.font.color = global.font.color,
                                                     legend.show = legend.show && (i == 1),
                                                     footer.show = FALSE,
                                                     sz.min = sz.min, sz.max = sz.max,
                                                     col.min = col.min, col.max = col.max,
                                                    ...)$htmlwidget})
    }
    else if (chart.type == "Radar")
    {
        plot.list <- lapply(1:npanels, function(i){chart(.bind_mean(x[,i, drop = FALSE], average.series),
                                                     hovertext.show = c(TRUE, TRUE),
                                                     colors = c(colors[i], average.color),
                                                     grid.show = FALSE, x.tick.show = FALSE,
                                                     data.label.show = c(data.label.show, FALSE),
                                                     y.bounds.maximum = y.bounds.maximum,
                                                     global.font.family = global.font.family,
                                                     global.font.color = global.font.color,
                                                     pad.left = pad.left, pad.right = pad.right,
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
                                                     fit.line.colors = fit.line.colors[i],
                                                     fit.CI.colors = fit.CI.colors[i],
                                                     x.title = x.title, x.title.font.size = x.title.font.size,
                                                     y.title = y.title, y.title.font.size = y.title.font.size,
                                                     grid.show = grid.show, data.label.show = data.label.show,
                                                     x.tick.show = x.tick.show, x.tick.angle = x.tick.angle,
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
                                                     fit.line.colors = c(fit.line.colors[i], average.color),
                                                     fit.CI.colors = c(fit.CI.colors[i], average.color),
                                                     x.title = x.title, x.title.font.size = x.title.font.size,
                                                     y.title = y.title, y.title.font.size = y.title.font.size,
                                                     grid.show = grid.show, data.label.show = data.label.show,
                                                     x.tick.show = x.tick.show, x.tick.angle = x.tick.angle,
                                                     y.bounds.maximum = y.bounds.maximum,
                                                     y.bounds.minimum = y.bounds.minimum,
                                                     x.bounds.maximum = x.bounds.maximum,
                                                     x.bounds.minimum = x.bounds.minimum,
                                                     global.font.family = global.font.family,
                                                     global.font.color = global.font.color,
                                                     ...)$htmlwidget})

    is.geo <- chart.type == "GeographicMap"
    is.radar <- chart.type == "Radar"
    res <- subplot(plot.list, nrows = nrows, margin = c(pad.left * !is.radar, pad.right * !is.radar, pad.top, pad.bottom),
                   heights = rep(1/nrows, nrows) - h.offset, # compensate for plotly bug
                   widths = rep(1/ncols, ncols) - w.offset, titleX = TRUE, titleY = TRUE,
                   shareX = share.axes && !is.geo, shareY = share.axes && !is.geo)
    res <- layout(res, title = title, showlegend = is.geo, annotations = paneltitles,
                  titlefont = list(family = title.font.family, color = title.font.color, size = title.font.size),
                  margin = list(l = margin.left, r = margin.right, b = margin.bottom, t = margin.top))
    res
}
