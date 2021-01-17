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
#' @param panel.title.show Logical; whether to show a title for each panel.
#' @param panel.title.font.family Font family of panel titles.
#' @param panel.title.font.color Font color of panel titles.
#' @param panel.title.font.size Font size of panel titles.
#' @param panel.title.wrap Logical; whether the panel title should be wrapped.
#' @param panel.title.wrap.nchar Number of characters (approximately) in each line
#'     of the panel title when \code{panel.title.wordwrap} \code{TRUE}.
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
#' @importFrom flipU CollectWarnings
#' @examples
#' x <- matrix(1:21, 7, 3, dimnames = list(letters[1:7], LETTERS[1:3]))
#' SmallMultiples(x, "Column", colors=c("red","green","blue"))
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
                           average.color = rgb(153, 153, 153, maxColorValue = 255),
                           y.bounds.maximum = NULL,
                           y.bounds.minimum = NULL,
                           x.bounds.maximum = NULL,
                           x.bounds.minimum = NULL,
                           x.tick.maxnum = NULL,
                           y.tick.maxnum = NULL,
                           values.bounds.maximum = NULL,
                           values.bounds.minimum = NULL,
                           colors = ChartColors(max(1, ncol(x), nrow(x), na.rm = TRUE)),
                           fit.line.colors = colors,
                           fit.CI.colors = colors,
                           global.font.family = "Arial",
                           global.font.color = rgb(44, 44, 44, maxColorValue = 255),
                           title = "",
                           title.font.family = global.font.family,
                           title.font.color = global.font.color,
                           title.font.size = 16,
                           subtitle = "",
                           subtitle.font.family = global.font.family,
                           subtitle.font.color = global.font.color,
                           subtitle.font.size = 12,
                           footer = "",
                           footer.font.family = global.font.family,
                           footer.font.color = global.font.color,
                           footer.font.size = 8,
                           footer.wrap = TRUE,
                           footer.wrap.nchar = 100,
                           panel.title.show = TRUE,
                           panel.title.font.family = global.font.family,
                           panel.title.font.color = global.font.color,
                           panel.title.font.size = 14,
                           panel.title.wrap = TRUE,
                           panel.title.wrap.nchar = 20,
                           x.title = "",
                           x.title.font.size = 12,
                           y.title = "",
                           y.title.font.size = 12,
                           data.label.show = FALSE,
                           data.label.prefix = "",
                           data.label.suffix = "",
                           data.label.font.color = global.font.color,
                           line.thickness = NULL,
                           grid.show = TRUE,
                           x.tick.show = TRUE,
                           x.tick.angle = NULL,
                           legend.show = TRUE,
                           margin.autoexpand = TRUE,
                           margin.left = NULL,
                           margin.right = NULL,
                           margin.top = NULL,
                           margin.bottom = NULL,
                           mapping.package = "plotly", # discarded
                           scatter.x.column = 1,
                           scatter.y.column = 2,
                           scatter.sizes.column = 3,
                           scatter.colors.column = 4,
                           scatter.groups.column = NULL,
                           scatter.colors.as.categorical = TRUE,
                           scatter.sizes.as.diameter = FALSE,
                           ...)
{
    chart.type <- gsub(" ", "", chart.type)
    chart <- get0(chart.type, mode = "function")
    eval(colors)
    if (is.null(fit.line.colors))
        fit.line.colors <- colors
    if (is.null(footer.font.family))
        footer.font.family <- global.font.family
    if (is.null(footer.font.color))
        footer.font.color <- global.font.color
    if (is.null(footer.font.size))
        footer.font.size <- 8

    if (length(dim(x)) < 2)
    {
        x <- as.matrix(x)
        warning("Unselect 'Automatically tidy the data' to avoid flattening 1-column matrices")
    }

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
        is.empty <- sapply(indexes, is.null)
        if (any(is.empty))
        {
            warning("No data observed for '", paste(names(is.empty)[is.empty], collapse = "', '"), "'")
            indexes <- indexes[!is.empty]
        }
        npanels <- length(indexes)
        scatter.chart.type <- if (is.na(scatter.sizes.column) ||
            scatter.sizes.column == 0 || scatter.sizes.column > NCOL(x))
            "X Y Scatter" else "Bubble"
    } else
    {
        npanels <- NCOL(x)
        data.label.show <- vectorize(data.label.show, NCOL(x), NROW(x))
        data.label.prefix <- vectorize(data.label.prefix, NCOL(x), NROW(x), split = NULL)
        data.label.suffix <- vectorize(data.label.suffix, NCOL(x), NROW(x), split = NULL)
    }

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
                x <- x[, x.order, drop = FALSE]
            npanels <- length(x.order)
        }
    }
    nrows <- min(npanels, nrows)
    if (npanels > 100)
        stop("Small multiples cannot show more than 100 panels (current dataset contains ", npanels, " series).\n")
    if (length(colors) < npanels && !chart.type %in% c("GeographicMap", "Pyramid", "Scatter", "BarMultiColor", "ColumnMultiColor"))
        colors <- paste0(rep("", npanels), colors)
    if (!chart.type %in% c("Pyramid", "BarMultiColor", "ColumnMultiColor"))
        data.label.font.color <- vectorize(data.label.font.color, npanels)

    all.values <- if (chart.type == "Scatter") x[,scatter.y.column]
                  else if (length(dim(x)) == 3) checkMatrixNames(x)
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
        else if (chart.type == "Radar")
        {
            bounds <- setRadarAxisBounds(y.bounds.minimum, y.bounds.maximum, all.values)
            y.bounds.minimum <- bounds$min
            y.bounds.maximum <- bounds$max
        }
        else if (chart.type %in% c("Bar", "Pyramid", "BarMultiColor"))
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
    {
        x.num <- checkMatrixNames(x)
        average.series <- apply(x.num, 1, mean)
    }
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
            stop("'Left padding' and 'Right padding' should be between 0 and 1/ncols (", round(1/ncols, 4), ")")
    }
    h.offset <- c(pad.top, rep(0, max(0, nrows - 2)), pad.bottom)[1:nrows]
    if (any(h.offset >= 1/nrows))
        stop("'Top padding' and 'Bottom padding' should be between 0 and 1/nrows (",
             round(1/nrows, 4), ")")

    # For Column charts, values for y.tick.maxnum default to 11
    # Here we try to generalise to make the axis less crowded
    # if more subplots are shown.
    if (is.null(x.tick.maxnum) &&
        !chart.type %in% c("Bar", "BarMultiColor", "Pyramid" ))
        x.tick.maxnum <- max(5, floor(11/nrows) + 1)


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
        empty.footer <- length(footer) == 0 || nchar(footer) == 0
        sz.min <- NULL
        sz.max <- NULL
        if (!is.null(scatter.sizes.column) && !is.na(scatter.sizes.column) &&
            scatter.sizes.column > 0 && scatter.sizes.column <= NCOL(x))
        {
            notNA.ind <- 1:nrow(x)
            if (sum(scatter.x.column, na.rm = TRUE) > 0)
                notNA.ind <- intersect(notNA.ind, which(!is.na(x[,scatter.x.column])))
            if (sum(scatter.y.column, na.rm = TRUE) > 0)
                notNA.ind <- intersect(notNA.ind, which(!is.na(x[,scatter.y.column])))
            if (sum(scatter.colors.column, na.rm = TRUE) > 0)
                notNA.ind <- intersect(notNA.ind, which(!is.na(x[,scatter.colors.column])))
            sc.tmp <- abs(AsNumeric(x[notNA.ind, scatter.sizes.column], binary = FALSE))
            sz.min <- min(sc.tmp, na.rm = TRUE)
            sz.max <- max(sc.tmp, na.rm = TRUE)
            if (empty.footer)
                footer <- sprintf("%s%s of points are proportional to absolute value of '%s'; ",
                              footer, if (scatter.sizes.as.diameter) "Diameter" else "Area",
                              colnames(x)[scatter.sizes.column])

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
            if (empty.footer)
                footer <- sprintf("%sPoints colored according to '%s'; ",
                              footer, colnames(x)[scatter.colors.column])
        } else
            colors <- as.list(colors)

        plot.list <- CollectWarnings(lapply(1:npanels, function(i){chart(x[indexes[[i]],],
                                                     scatter.x.column = scatter.x.column,
                                                     scatter.y.column = scatter.y.column,
                                                     scatter.sizes.column = scatter.sizes.column,
                                                     scatter.colors.column = scatter.colors.column,
                                                     scatter.sizes.as.diameter = scatter.sizes.as.diameter,
                                                     scatter.colors.as.categorical = FALSE,
                                                     colors = colors[[i]],
                                                     data.label.font.color = data.label.font.color[i],
                                                     fit.line.colors = fit.line.colors[i],
                                                     fit.CI.colors = fit.CI.colors[i],
                                                     x.title = x.title, x.title.font.size = x.title.font.size,
                                                     y.title = y.title, y.title.font.size = y.title.font.size,
                                                     grid.show = grid.show, data.label.show = data.label.show,
                                                     x.tick.show = x.tick.show, x.tick.angle = x.tick.angle,
                                                     y.bounds.maximum = y.bounds.maximum,
                                                     y.bounds.minimum = y.bounds.minimum,
                                                     y.tick.maxnum = y.tick.maxnum,
                                                     x.bounds.maximum = x.bounds.maximum,
                                                     x.bounds.minimum = x.bounds.minimum,
                                                     x.tick.maxnum = x.tick.maxnum,
                                                     global.font.family = global.font.family,
                                                     global.font.color = global.font.color,
                                                     legend.show = legend.show && (i == 1),
                                                     small.mult.index = i,
                                                     sz.min = sz.min, sz.max = sz.max,
                                                     col.min = col.min, col.max = col.max,
                                                     margin.autoexpand = margin.autoexpand,
                                                    ...)$htmlwidget}))
    }
    else if (chart.type == "Radar")
    {
        if (length(line.thickness) == 0)
            line.thickness <- ""
        if (is.character(line.thickness))
            line.thickness <- TextAsVector(line.thickness)
        line.thickness <- suppressWarnings(paste0(line.thickness, rep("", npanels)))

        plot.list <- CollectWarnings(lapply(1:npanels, function(i){chart(.bind_mean(getColumn(x, i), average.series),
                                                     hovertext.show = c(TRUE, TRUE),
                                                     line.thickness = line.thickness[i],
                                                     colors = c(colors[i], average.color),
                                                     grid.show = FALSE, x.tick.show = FALSE,
                                                     data.label.show = cbind(data.label.show[,i], FALSE),
                                                     data.label.prefix = data.label.prefix[,i],
                                                     data.label.suffix = data.label.suffix[,i],
                                                     data.label.font.color = data.label.font.color[i],
                                                     y.bounds.maximum = y.bounds.maximum,
                                                     y.bounds.minimum = y.bounds.minimum,
                                                     aspect.fixed = FALSE, # not supported with subplot
                                                     global.font.family = global.font.family,
                                                     global.font.color = global.font.color,
                                                     pad.left = pad.left, pad.right = pad.right,
                                                     ...)$htmlwidget}))
        margin.left <- 0
        margin.right <- 0
        margin.bottom <- 0
    }
    else if (chart.type == "GeographicMap")
    {
         plot.list <- CollectWarnings(lapply(1:npanels, function(i){chart(getColumn(x, i),
                                                     colors = colors,
                                                     mapping.package = "plotly",
                                                     legend.show = legend.show && (i == 1),
                                                     values.bounds.maximum = values.bounds.maximum,
                                                     values.bounds.minimum = values.bounds.minimum,
                                                     global.font.family = global.font.family,
                                                     global.font.color = global.font.color,
                                                     ...)$htmlwidget}))
        margin.left <- 0
        margin.right <- 0
        margin.bottom <- 0
    } else if (chart.type == "Bar" || chart.type == "Column")
    {
        plot.list <- CollectWarnings(lapply(1:npanels, function(i){chart(getColumn(x, i),
                                                     colors = colors[i],
                                                     average.series = average.series,
                                                     average.color = average.color,
                                                     fit.line.colors = fit.line.colors[i],
                                                     fit.CI.colors = fit.CI.colors[i],
                                                     x.title = x.title, x.title.font.size = x.title.font.size,
                                                     y.title = y.title, y.title.font.size = y.title.font.size,
                                                     grid.show = grid.show, data.label.show = data.label.show[,i],
                                                     data.label.prefix = data.label.prefix[,i],
                                                     data.label.suffix = data.label.suffix[,i],
                                                     data.label.font.color = data.label.font.color[i],
                                                     x.tick.show = x.tick.show, x.tick.angle = x.tick.angle,
                                                     y.bounds.maximum = y.bounds.maximum,
                                                     y.bounds.minimum = y.bounds.minimum,
                                                     y.tick.maxnum = y.tick.maxnum,
                                                     x.bounds.maximum = x.bounds.maximum,
                                                     x.bounds.minimum = x.bounds.minimum,
                                                     x.tick.maxnum = x.tick.maxnum,
                                                     global.font.family = global.font.family,
                                                     global.font.color = global.font.color,
                                                     margin.autoexpand = margin.autoexpand,
                                                     ...)$htmlwidget}))

        # Remove second axis which is used for positioning data labels/hovertext
        # on categorical/date axis - naming interferes with subplot
        axis.name <- if (chart.type == "Bar") "yaxis2" else "xaxis2"
        for (i in 1:npanels)
            plot.list[[i]]$x$layoutAttrs[[1]][[axis.name]] <- NULL
    } else if (chart.type == "BarMultiColor" || chart.type == "ColumnMultiColor")
    {
        color.as.matrix <- NCOL(colors) == npanels && npanels > 1
        if (NCOL(colors) != npanels && NCOL(colors) > 1)
            warning("Only the first column of 'colors' was used. ",
                    "To apply a different for each panel, 'colors' should be a table with ",
                    npanels, " columns")
        plot.list <- CollectWarnings(lapply(1:npanels, function(i){chart(getColumn(x, i),
                                                     colors = if (color.as.matrix) colors[,i] else colors,
                                                     x.title = x.title, x.title.font.size = x.title.font.size,
                                                     y.title = y.title, y.title.font.size = y.title.font.size,
                                                     grid.show = grid.show, data.label.show = data.label.show[,i],
                                                     data.label.prefix = data.label.prefix[,i],
                                                     data.label.suffix = data.label.suffix[,i],
                                                     data.label.font.color = data.label.font.color,
                                                     x.tick.show = x.tick.show, x.tick.angle = x.tick.angle,
                                                     y.bounds.maximum = y.bounds.maximum,
                                                     y.bounds.minimum = y.bounds.minimum,
                                                     y.tick.maxnum = y.tick.maxnum,
                                                     x.bounds.maximum = x.bounds.maximum,
                                                     x.bounds.minimum = x.bounds.minimum,
                                                     x.tick.maxnum = x.tick.maxnum,
                                                     global.font.family = global.font.family,
                                                     global.font.color = global.font.color,
                                                     margin.autoexpand = margin.autoexpand,
                                                     ...)$htmlwidget}))
    } else if (chart.type == "Pyramid")
    {
        color.as.matrix <- NCOL(colors) == npanels && npanels > 1
        if (NCOL(colors) != npanels && NCOL(colors) > 1)
            warning("Only the first column of 'colors' was used. ",
                    "To apply a different for each panel, 'colors' should be a table with ",
                    npanels, " columns")
        plot.list <- CollectWarnings(lapply(1:npanels, function(i){chart(getColumn(x, i),
                                                     colors = if (color.as.matrix) colors[,i] else colors,
                                                     x.title = x.title, x.title.font.size = x.title.font.size,
                                                     y.title = y.title, y.title.font.size = y.title.font.size,
                                                     data.label.show = data.label.show[,i],
                                                     data.label.prefix = data.label.prefix[,i],
                                                     data.label.suffix = data.label.suffix[,i],
                                                     data.label.font.color = data.label.font.color,
                                                     x.tick.show = FALSE, x.tick.angle = x.tick.angle,
                                                     x.bounds.maximum = x.bounds.maximum,
                                                     x.tick.maxnum = x.tick.maxnum,
                                                     y.tick.maxnum = y.tick.maxnum,
                                                     global.font.family = global.font.family,
                                                     global.font.color = global.font.color,
                                                     margin.autoexpand = margin.autoexpand,
                                                     ...)$htmlwidget}))
    } else
    {
        # Line or Area chart
        if (length(line.thickness) == 0)
            line.thickness <- ""
        if (is.character(line.thickness))
            line.thickness <- TextAsVector(line.thickness)
        line.thickness <- suppressWarnings(paste0(line.thickness, rep("", npanels)))

        plot.list <- CollectWarnings(lapply(1:npanels, function(i){chart(.bind_mean(getColumn(x, i), average.series),
                                                     colors = c(colors[i], average.color),
                                                     line.thickness = line.thickness[i],
                                                     fit.line.colors = c(fit.line.colors[i], average.color),
                                                     fit.CI.colors = c(fit.CI.colors[i], average.color),
                                                     x.title = x.title, x.title.font.size = x.title.font.size,
                                                     y.title = y.title, y.title.font.size = y.title.font.size,
                                                     grid.show = grid.show,
                                                     data.label.show = cbind(data.label.show[,i], FALSE),
                                                     data.label.prefix = cbind(data.label.prefix[,i], ""),
                                                     data.label.suffix = cbind(data.label.suffix[,i], ""),
                                                     data.label.font.color = data.label.font.color[i],
                                                     x.tick.show = x.tick.show, x.tick.angle = x.tick.angle,
                                                     y.bounds.maximum = y.bounds.maximum,
                                                     y.bounds.minimum = y.bounds.minimum,
                                                     x.bounds.maximum = x.bounds.maximum,
                                                     x.bounds.minimum = x.bounds.minimum,
                                                     x.tick.maxnum = x.tick.maxnum,
                                                     y.tick.maxnum = y.tick.maxnum,
                                                     global.font.family = global.font.family,
                                                     global.font.color = global.font.color,
                                                     margin.autoexpand = margin.autoexpand,
                                                     ...)$htmlwidget}))
    }

    is.geo <- chart.type == "GeographicMap"
    is.radar <- chart.type == "Radar"
    res <- subplot(plot.list, nrows = nrows, margin = c(pad.left * !is.radar, pad.right * !is.radar, pad.top, pad.bottom),
                   heights = rep(1/nrows, nrows) - h.offset, # compensate for plotly bug
                   widths = rep(1/ncols, ncols) - w.offset, titleX = TRUE, titleY = TRUE,
                   shareX = share.axes && !is.geo, shareY = share.axes && !is.geo)

    # Titles and margin text
    title.font <- list(family = title.font.family, size = title.font.size, color = title.font.color)
    panel.title.font <- list(family = panel.title.font.family, color = panel.title.font.color,
                             size = panel.title.font.size)
    subtitle.font <- list(family = subtitle.font.family, size = subtitle.font.size,
                          color = subtitle.font.color)
    footer.font <- list(family = footer.font.family, size = footer.font.size, color = footer.font.color)
    footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate = FALSE)
    margins <- list(l = margin.left, r = margin.right, b = margin.bottom, t = margin.top)
    margins <- setMarginsForText(margins, title, subtitle, footer, title.font.size,
                                 subtitle.font.size, footer.font.size)
    if (sum(nchar(subtitle)) > 0)
        subtitle <- paste0(subtitle, "<br>&nbsp;<br>")
    annotations <- list(setSubtitle(subtitle, subtitle.font, margins),
                        setTitle(title, title.font, margins),
                        setFooter(footer, footer.font, margins))
    titles <- if (chart.type == "Scatter") names(indexes)
              else                         colnames(x)
    if (panel.title.show && !is.null(titles))
    {
        title.list <- autoFormatLongLabels(titles, panel.title.wrap, panel.title.wrap.nchar)
        titles.ypos <- rep((nrows:1)/nrows, each = ncols)[1:npanels]
        titles.xpos <- rep((1:ncols - 0.5)/ncols, nrows)[1:npanels]
        for (i in 1:npanels)
            annotations[[i+3]] <- list(text = title.list[i], showarrow = FALSE,
                            x = titles.xpos[i], y = titles.ypos[i], font = panel.title.font,
                            xanchor = "center", yanchor = "top", xref = 'paper', yref = 'paper')
    }
    res$sizingPolicy$browser$padding <- if (margin.autoexpand) 40 # so existing charts don't move
                                        else                   0
    margins$autoexpand <- margin.autoexpand
    res <- layout(res, showlegend = is.geo, margin = margins,
                  annotations = annotations)
    #attr(res, "can-run-in-root-dom") <- TRUE
    result <- list(htmlwidget = res)
    class(result) <- "StandardChart"
    attr(result, "ChartType") <- switch(chart.type,
        GeographicMap = "Filled Map",
        Scatter = scatter.chart.type,
        Bar = "Bar Clustered",
        Pyramid = "Bar Clustered",
        BarMultiColor = "Bar Clustered",
        Column = "Column Clustered",
        ColumnMultiColor = "Column Clustered",
        chart.type) # e.g. Area, Line, Radar
    result
}

