#' Scatter
#'
#' Scatter plot
#' @inherit LabeledScatter
#' @inherit Column
#' @param x A numeric vector for the x-axis coordinates (which may be named); or a matrix or datarame; or a list of matrices where each matrix share the same row and column names.
#' @param scatter.labels.as.hovertext Logical; if TRUE, labels are shown has hovers; otherwise, as a labeled scatterplot.
#' @param scatter.sizes.as.diameter Whether to show the points with diameter (instead of area, which is the default) proportional to the sizes variable.
#' @param line.thickness Thickness, in pixels, of the series line
#' @param line.colors  Character; a vector containing one or more named
#' @param marker.size Size in pixels of marker. This is overriden
#' if \code{scatter.sizes} is provided, but used for the legend
#' if \code{scatter.colors.as.categorical}.
#' @param data.label.position Character; where to place the source data value in relation
#' to the marker icon. Can be "top left", "top center", "top right", "middle left", "middle center",
#' "middle right", "bottom left", "bottom center", "bottom right".
#' @param marker.border.width Width in pixels of border/line around markers; 0 is no line.
#' @param marker.border.colors Character; a vector containing one or more colors specified as hex codes.
#' @param marker.border.opacity Opacity of border/line around markers as an alpha value (0 to 1).
#' @param swap.x.and.y Swap the x and y axis around on the chart.
#' @param small.mult.index Used by Small Multiples to add prefixes to warnings.
#' @param sz.min Parameter to control scaling of scatter.sizes, used by SmallMultiples
#' @param sz.max Parameter to control scaling of scatter.sizes, used by SmallMultiples
#' @param sz.scale Parameter to control scaling of scatter.sizes, used by SmallMultiples
#' @param col.min Parameter to control scaling of scatter.colors, used by SmallMultiples
#' @param col.max Parameter to control scaling of scatter.colors, used by SmallMultiples
#' @param ... Extra arguments that are ignored.
#' @importFrom grDevices rgb col2rgb
#' @importFrom flipChartBasics ChartColors
#' @importFrom flipTime AsDateTime
#' @importFrom flipTransformations AsNumeric
#' @importFrom plotly plot_ly config toRGB add_trace add_text layout hide_colorbar
#' @importFrom stats loess loess.control lm predict
#' @export
Scatter <- function(x = NULL,
                         y = NULL,
                         scatter.x.column = 1,
                         scatter.y.column = 2,
                         scatter.labels = NULL,
                         scatter.labels.name = NULL,
                         scatter.sizes = NULL,
                         scatter.sizes.name = NULL,
                         scatter.sizes.column = 3,
                         scatter.sizes.as.diameter = FALSE,
                         scatter.colors = NULL,
                         scatter.colors.name = NULL,
                         scatter.colors.column = 4,
                         scatter.colors.as.categorical = TRUE,
                         scatter.labels.as.hovertext = TRUE,
                         scatter.max.labels = 50,
                         colors = ChartColors(12),
                         trend.lines = FALSE,
                         logos = NULL,
                         logo.size = 0.5,
                         fit.type = "None",
                         fit.ignore.last = FALSE,
                         fit.line.type = "dot",
                         fit.line.width = 1,
                         fit.line.name = "Fitted",
                         fit.line.colors = colors,
                         fit.line.opacity = 1,
                         fit.CI.show = FALSE,
                         fit.CI.colors = fit.line.colors,
                         fit.CI.opacity = 0.4,
                         legend.show = TRUE,
                         legend.wrap = TRUE,
                         legend.wrap.nchar = 30,
                         tooltip.show = TRUE,
                         modebar.show = FALSE,
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
                         data.label.show = FALSE,
                         data.label.font.family = global.font.family,
                         data.label.font.color = global.font.color,
                         data.label.font.autocolor = FALSE,
                         data.label.font.size = 10,
                         data.label.position = "top middle",
                         opacity = NULL,
                         background.fill.color =  "transparent",
                         background.fill.opacity = 1,
                         charting.area.fill.color =  background.fill.color,
                         charting.area.fill.opacity = 1,
                         legend.fill.color = background.fill.color,
                         legend.fill.opacity = 1,
                         legend.border.color = rgb(44, 44, 44, maxColorValue = 255),
                         legend.border.line.width = 0,
                         legend.font.color = global.font.color,
                         legend.font.family = global.font.family,
                         legend.font.size = 10,
                         legend.position.y = 1,
                         legend.position.x = 1.02,
                         legend.ascending = NA,
                         margin.top = NULL,
                         margin.bottom = NULL,
                         margin.left = NULL,
                         margin.right = NULL,
                         margin.inner.pad = NULL,
                         grid.show = TRUE,
                         y.title = "",
                         y.title.font.color = global.font.color,
                         y.title.font.family = global.font.color,
                         y.title.font.size = 12,
                         y.line.width = 0,
                         y.line.color = rgb(0, 0, 0, maxColorValue = 255),
                         y.tick.mark.length = 5,
                         y.bounds.minimum = NULL,
                         y.bounds.maximum = NULL,
                         y.tick.distance = NULL,
                         y.zero = FALSE,
                         y.zero.line.width = 0,
                         y.zero.line.color = rgb(225, 225, 225, maxColorValue = 255),
                         y.data.reversed = FALSE,
                         y.grid.width = 1 * grid.show,
                         y.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                         y.tick.show = TRUE,
                         y.tick.suffix = "",
                         y.tick.prefix = "",
                         y.tick.format = "",
                         y.hovertext.format = "",
                         y.tick.angle = NULL,
                         y.tick.font.color = global.font.color,
                         y.tick.font.family = global.font.family,
                         y.tick.font.size = 10,
                         x.title = "",
                         x.title.font.color = global.font.color,
                         x.title.font.family = global.font.family,
                         x.title.font.size = 12,
                         x.line.width = 0,
                         x.line.color = rgb(0, 0, 0, maxColorValue = 255),
                         x.tick.mark.length = 5,
                         x.bounds.minimum = NULL,
                         x.bounds.maximum = NULL,
                         x.tick.distance = NULL,
                         x.zero = FALSE,
                         x.zero.line.width = 0,
                         x.zero.line.color = rgb(225, 225, 225, maxColorValue = 255),
                         x.data.reversed = FALSE,
                         x.grid.width = 1 * grid.show,
                         x.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                         x.tick.show = TRUE,
                         x.tick.suffix = "",
                         x.tick.prefix = "",
                         x.tick.format = "",
                         x.hovertext.format = "",
                         x.tick.angle = NULL,
                         x.tick.font.color = global.font.color,
                         x.tick.font.family = global.font.family,
                         x.tick.font.size = 10,
                         x.tick.label.wrap = TRUE,
                         x.tick.label.wrap.nchar = 21,
                         hovertext.font.family = global.font.family,
                         hovertext.font.size = 11,
                         line.thickness = 0,
                         line.colors = colors,
                         marker.border.width = 1,
                         marker.border.colors = colors,
                         marker.border.opacity = NULL,
                         marker.size = if (is.null(scatter.sizes)) 6 else 12,
                         swap.x.and.y = FALSE,
                         small.mult.index = NULL,
                         sz.min = NULL,
                         sz.max = NULL,
                         sz.scale = 50,
                         col.min = NULL,
                         col.max = NULL)
{
    # Use labeled scatterplots if multiple tables are provided
    if ((is.list(x) && !is.data.frame(x)) || !scatter.labels.as.hovertext)
    {
        if (is.list(x) || !is.null(rownames(x))|| (length(dim(x)) < 2 && !is.null(names(x))))
        {
            cl <- as.list(match.call())
            cl <- cl[-1]
            cl$scatter.labels.as.hovertext <- NULL
            return(do.call(LabeledScatter, cl))
        }
        else if (!scatter.labels.as.hovertext)
            warning("Labels not provided.")
    }

    # Adjust some of the the default default tick formats
    tmp.stat <- attr(x, "statistic")
    if (!is.null(tmp.stat) && grepl("%$", tmp.stat))
    {
        if (nchar(x.tick.format) == 0 || grepl("[0-9]$", x.tick.format))
            x.tick.format = paste0(x.tick.format, "%")
    }
    if (sum(nchar(x.hovertext.format)) == 0)
        x.hovertext.format <- x.tick.format
    if (sum(nchar(y.hovertext.format)) == 0)
        y.hovertext.format <- y.tick.format
    warning.prefix <- if (!is.null(small.mult.index)) paste0("Chart ", small.mult.index, ": ") else ""

    # Grouping font attributes to simplify passing to plotly
    title.font = list(family = title.font.family, size = title.font.size, color = title.font.color)
    subtitle.font = list(family = subtitle.font.family, size = subtitle.font.size, color = subtitle.font.color)
    x.title.font = list(family = x.title.font.family, size = x.title.font.size, color = x.title.font.color)
    y.title.font = list(family = y.title.font.family, size = y.title.font.size, color = y.title.font.color)
    ytick.font = list(family = y.tick.font.family, size = y.tick.font.size, color = y.tick.font.color)
    xtick.font = list(family = x.tick.font.family, size = x.tick.font.size, color = x.tick.font.color)
    footer.font = list(family = footer.font.family, size = footer.font.size, color = footer.font.color)
    legend.font = list(family = if (!is.null(legend.font.family)) legend.font.family else global.font.family,
                       color = if (!is.null(legend.font.color)) legend.font.color else global.font.family,
                       size = if (!is.null(legend.font.size)) legend.font.size else data.label.font.size)

    # Try to store name of variables
    scatter.mult.yvals <- isTRUE(attr(x, "scatter.mult.yvals"))
    if (!is.null(scatter.sizes) && is.null(scatter.sizes.name))
        scatter.sizes.name <- deparse(substitute(scatter.sizes))
    if (!is.null(scatter.labels) && is.null(scatter.labels.name))
        scatter.labels.name <- deparse(substitute(scatter.labels))
    if (!is.null(scatter.colors) && is.null(scatter.colors.name))
        scatter.colors.name <- deparse(substitute(scatter.colors))

    if (is.matrix(x) || is.data.frame(x))
    {
        .isValidColumnIndex <- function(n) {return (!is.null(n) && !is.na(n) && n > 0 && n <= ncol(x))}
        if (is.null(scatter.labels) && !is.null(rownames(x)))
            scatter.labels <- rownames(x)
        if (is.null(y) && .isValidColumnIndex(scatter.y.column))
        {
            if (sum(nchar(y.title), na.rm = TRUE) == 0 && !is.null(colnames(x)) && !scatter.mult.yvals)
                y.title <- colnames(x)[scatter.y.column]
            y <- x[,scatter.y.column]
        }
        if (is.null(scatter.sizes) && .isValidColumnIndex(scatter.sizes.column))
        {
            if (is.null(scatter.sizes.name) && !is.null(colnames(x)))
                scatter.sizes.name <- colnames(x)[scatter.sizes.column]
            scatter.sizes <- x[,scatter.sizes.column]
        }
        if (is.null(scatter.colors) && .isValidColumnIndex(scatter.colors.column))
        {
            if (is.null(scatter.colors.name) || nchar(scatter.colors.name) == 0)
                scatter.colors.name <- colnames(x)[scatter.colors.column]
            scatter.colors <- x[,scatter.colors.column]
        }
        if (sum(nchar(x.title), na.rm = TRUE) == 0 && (!is.null(colnames(x))) &&
            .isValidColumnIndex(scatter.x.column) && !scatter.mult.yvals)
            x.title <- colnames(x)[scatter.x.column]
        if (!.isValidColumnIndex(scatter.x.column))
            x <- NULL
        else
            x <- x[,scatter.x.column]
    }
    if (is.null(scatter.labels) && !is.null(names(x)))
        scatter.labels <- names(x)

    # Warning if non-default selected but corresponding data is missing
    if (is.null(small.mult.index) && is.null(scatter.sizes) && scatter.sizes.as.diameter)
        warning("'Sizes' variable not provided.")
    if (is.null(small.mult.index) && is.null(scatter.colors) && !scatter.colors.as.categorical)
        warning("'Colors' variable not provided.")
    qualitative.palettes <- c("Default colors", "Primary colors",
        "Light colors", "Strong colors", "Colorblind safe colors")
    if (!scatter.colors.as.categorical && !is.null(attr(colors, "palette.type"))
        && attr(colors, "palette.type") %in% qualitative.palettes)
        warning("For a numeric 'colors' variable, a qualitative palette should not be used. The colorscale is created by interpolating the colors.")

    # Basic data checking
    if (is.null(x) && is.null(y))
        stop("At least one of x or y must be supplied.")
    if (is.null(x))
        x <- rep(0, length(y))
    n <- length(x)
    if (is.null(y))
        y <- rep(0, n)
    if (swap.x.and.y)
    {
        tmp <- x
        x <- y
        y <- tmp

        tmp <- x.title
        x.title <- y.title
        y.title <- tmp
    }
    if (any(duplicated(cbind(x, y))))
        warning(warning.prefix, "Chart contains overlapping points in the same position.")

    # Remove NAs
    not.na <- !is.na(x) & !is.na(y)
    if (sum(not.na) != n)
        warning(warning.prefix, "Data points with missing values have been omitted.")
    n <- length(x)
    if (!is.null(scatter.sizes))
    {
        if (length(scatter.sizes) != n)
            stop("'scatter.sizes' should be a numeric vector with the same number of observations as 'x'.")
        if (any(!is.finite(suppressWarnings(AsNumeric(scatter.sizes, binary = FALSE)))))
        {
            warning(warning.prefix, "Some points omitted due to missing values in 'scatter.sizes'.")
            not.na <- not.na & is.finite(suppressWarnings(AsNumeric(scatter.sizes, binary = FALSE)))
        }
    }
    if (!is.null(scatter.colors))
    {
        if (length(scatter.colors) != n)
            stop("'scatter.colors' should be a vector with the same number of observations as 'x'.")
        if (any(is.na(scatter.colors)))
        {
            warning(warning.prefix, "Some points omitted due to missing values in 'scatter.colors'")
            not.na <- not.na & !is.na(scatter.colors)
        }
    } else
        scatter.colors.as.categorical <- FALSE

    if (sum(not.na) == 0)
        stop("No non-NA points to plot.")
    if (any(not.na))
    {
        if (!is.null(scatter.labels))
            scatter.labels <- scatter.labels[which(not.na)]
        if (!is.null(x))
            x <- x[which(not.na)]
        if (!is.null(y))
            y <-y[which(not.na)]
        if (!is.null(scatter.sizes))
            scatter.sizes <- scatter.sizes[which(not.na)]
        if (!is.null(scatter.colors))
            scatter.colors <- scatter.colors[which(not.na)]
    }

    n <- sum(not.na)
    if (!is.null(scatter.sizes))
    {
        sz.tmp <- abs(AsNumeric(scatter.sizes, binary = FALSE))
        if (is.null(sz.min))
            sz.min <- min(sz.tmp, na.rm = TRUE)
        if (is.null(sz.max))
            sz.max <- max(sz.tmp, na.rm = TRUE)
        if (!scatter.sizes.as.diameter)
        {
            sz.tmp <- sqrt(sz.tmp)
            sz.min <- sqrt(sz.min)
            sz.max <- sqrt(sz.max)
        }

        if (any(class(scatter.sizes) %in% c("Date", "POSIXct", "POSIXt")))
            scatter.sizes.scaled <- (sz.tmp - sz.min)/(sz.max - sz.min) * sz.scale
        else
            scatter.sizes.scaled <- sz.tmp/sz.max * sz.scale

        if (is.null(opacity))
            opacity <- 0.4
    }
    if (is.null(opacity))
        opacity <- if (fit.type == "None") 1 else 0.4
    if (is.null(marker.border.opacity))
        marker.border.opacity <- opacity
    if (data.label.font.autocolor)
        data.label.font.color <- colors

    scatter.colors.as.numeric <- 0
    colorbar <- NULL
    groups <- rep("Series 1", n)
    if (!is.null(scatter.colors) && !scatter.colors.as.categorical)
    {
        # make colorscalebar
        col.fun <- colorRamp(unique(colors))  # undo recycling in PrepareColors
        cc.orig <- rgb(col.fun((0:5)/5), maxColorValue = 255) # hex values of opaque colors
        cc.rgb <- col2rgb(cc.orig)
        bg.rgb <- c(255, 255, 255)
        conv.alpha <- function(xx, alpha) {                   # fake alpha transparency
            yy <- (xx * alpha) + (bg.rgb * (1 - alpha))
            return(rgb(yy[1], yy[2], yy[3], maxColorValue = 255))}
        bg.rgb <- col2rgb(conv.alpha(col2rgb(background.fill.color), background.fill.opacity))
        cc.alpha <- apply(cc.rgb, 2, conv.alpha, alpha = opacity)
        cc.vals <- seq(from = 0, to = 1, length = length(cc.orig))
        col.scale <- mapply(function(a,b)c(a,b), a = cc.vals, b = toRGB(cc.alpha), SIMPLIFY = FALSE)

        # getting labels for all types
        if (is.character(scatter.colors))
            scatter.colors <- as.factor(scatter.colors)

        scatter.colors.as.numeric <- 1
        groups <- 1:n
        col.tmp <- AsNumeric(scatter.colors, binary = FALSE)
        if (is.null(col.min))
            col.min <- min(col.tmp, na.rm = TRUE)
        if (is.null(col.max))
            col.max <- max(col.tmp, na.rm = TRUE)
        scatter.colors.scaled <- (col.tmp - col.min)/(col.max - col.min)
        scatter.colors.labels <- col.tmp
        if (any(class(scatter.colors) == "factor") ||
            any(class(scatter.colors) %in% c("Date", "POSIXct", "POSIXt")))
                scatter.colors.labels <- scatter.colors.scaled
        colors <- rgb(col.fun(scatter.colors.scaled), maxColorValue=255)

        if (any(class(scatter.colors) %in% c("Date", "POSIXct", "POSIXt")))
        {
            col.min <- 0
            col.max <- 1
            tmp.seq <- seq(0, 1, length=5)
            colorbar <- list(tickmode="array", tickvals=tmp.seq,
                             ticktext=c(min(scatter.colors) + diff(range(scatter.colors)) * tmp.seq),
                             outlinewidth=0, tickfont=legend.font)
        }
        else if (any(class(scatter.colors) == "factor"))
        {
            col.min <- 0
            col.max <- 1
            tmp.seq <- seq(from = 0, to = 1, length = nlevels(scatter.colors))
            colorbar <- list(tickmode="array", tickvals = tmp.seq,
                             ticktext=levels(scatter.colors), outlinewidth=0, tickfont=legend.font)
        }
        else
            colorbar <- list(outlinewidth = 0, tickfont=legend.font)
    }

    if (!is.null(scatter.colors) && scatter.colors.as.categorical)
        groups <- as.factor(scatter.colors)
    if (is.factor(groups))
        g.list <- levels(groups) # fix legend order
    else
        g.list <- unique(groups)

    num.groups <- length(g.list)
    num.series <- if (scatter.colors.as.numeric) 1 else num.groups
    data.label.font.color <- vectorize(data.label.font.color, length(g.list))
    data.label.font = lapply(data.label.font.color,
        function(cc) list(family = data.label.font.family, size = data.label.font.size, color = cc))


    # hovertext
    source.text <- paste0(scatter.labels, " (", formatByD3(x, x.hovertext.format, x.tick.prefix, x.tick.suffix), ", ",
                          formatByD3(y, y.hovertext.format, y.tick.prefix, y.tick.suffix), ")")
    if (!is.null(scatter.colors.name) && !scatter.mult.yvals)
    {
        colors.str <- if (is.numeric(scatter.colors)) FormatAsReal(scatter.colors, decimals = decimalsFromD3(x.hovertext.format)) else as.character(scatter.colors)
        source.text <- paste0(source.text, "<br>", scatter.colors.name, ": ", colors.str)
    }
    if (!is.null(scatter.sizes.name) && !scatter.mult.yvals)
    {
        sizes.str <- if (is.numeric(scatter.sizes)) FormatAsReal(scatter.sizes, decimals = decimalsFromD3(x.hovertext.format)) else as.character(scatter.sizes)
        source.text <- paste0(source.text, "<br>", scatter.sizes.name, ": ", sizes.str)
    }


    # other constants
    colorbar.show <- legend.show
    legend.show <- legend.show && num.series > 1
    series.mode <- if (is.null(line.thickness) || line.thickness == 0) "markers"
                   else "markers+lines"
    if (data.label.show)
        series.mode <- paste0(series.mode, "+text")
    marker.symbols <-  rep(100, 100) # disc

    type <- "Scatterplot"
    legend <- setLegend("Scatterplot", legend.font, legend.ascending,
                        legend.fill.color, legend.fill.opacity,
                        legend.border.color, legend.border.line.width,
                        legend.position.x, legend.position.y)
    if (length(footer) == 0 || nchar(footer) == 0)
    {
        footer <- ""
        if (!is.null(scatter.labels.name))
            footer <- sprintf("%sPoints labeled by '%s'; ",
                               footer, scatter.labels.name[1])
        if (!is.null(scatter.colors.name) && !scatter.mult.yvals)
            footer <- sprintf("%sPoints colored according to '%s'; ",
                              footer, scatter.colors.name[1])
        if (!is.null(scatter.sizes.name) && !scatter.mult.yvals)
            footer <- sprintf("%s%s of points are proportional to absolute value of '%s'; ",
                              footer,
                              if (scatter.sizes.as.diameter) "Diameter" else "Area",
                              scatter.sizes.name[1])
    }
    footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate=FALSE)

    # Format axis labels
    #if (is.null(y.tick.decimals))
    #    y.tick.decimals <- decimalsToDisplay(as.numeric(y))
    x.range <- setValRange(x.bounds.minimum, x.bounds.maximum, x)
    y.range <- setValRange(y.bounds.minimum, y.bounds.maximum, y)
    xtick <- setTicks(x.range$min, x.range$max, x.tick.distance, x.data.reversed)
    ytick <- setTicks(y.range$min, y.range$max, y.tick.distance, y.data.reversed)

    xlab.tmp <- if (!is.numeric(x)) as.character(x)
                else FormatAsReal(x, decimals=2) #x.tick.decimals)
    ylab.tmp <- if (!is.numeric(y)) as.character(y)
                else FormatAsReal(y, decimals=2) #y.tick.decimals)

    # Avoid points being trimmed off if they are too close to zero
    if (x.zero && is.numeric(x))
    {
        x.abs.max <- max(abs(range(x, na.rm=T)), na.rm=T)
        if (!is.finite(x.abs.max) || x.abs.max == 0 || any(abs(range(x, na.rm=T))/x.abs.max < 1e-2))
            x.zero <- FALSE
    }
    if (y.zero && is.numeric(y))
    {
        y.abs.max <- max(abs(range(y, na.rm=T)), na.rm=T)
        if (!is.finite(y.abs.max) || y.abs.max == 0 || any(abs(range(y, na.rm=T))/y.abs.max < 1e-2))
            y.zero <- FALSE
    }

    axisFormat <- formatLabels(list(x=xlab.tmp, y=ylab.tmp), type,
                       x.tick.label.wrap, x.tick.label.wrap.nchar,
                       x.tick.format, y.tick.format)
    yaxis <- setAxis(y.title, "left", axisFormat, y.title.font,
                  y.line.color, y.line.width, y.grid.width * grid.show, y.grid.color,
                  ytick, ytick.font, y.tick.angle, y.tick.mark.length,
                  y.tick.distance, y.tick.format, y.tick.prefix, y.tick.suffix,
                  y.tick.show, y.zero, y.zero.line.width, y.zero.line.color,
                  y.hovertext.format)
    xaxis <- setAxis(x.title, "bottom", axisFormat, x.title.font,
                  x.line.color, x.line.width, x.grid.width * grid.show, x.grid.color,
                  xtick, xtick.font, x.tick.angle, x.tick.mark.length,
                  x.tick.distance, x.tick.format, x.tick.prefix, x.tick.suffix, x.tick.show,
                  x.zero, x.zero.line.width, x.zero.line.color,
                  x.hovertext.format, axisFormat$labels)

    if (xaxis$type == "date")
        x <- AsDateTime(as.character(x), on.parse.failure = "silent")
    if (yaxis$type == "date")
        y <- AsDateTime(as.character(y), on.parse.failure = "silent")
    if (is.factor(x))
        levels(x) <- autoFormatLongLabels(levels(x), x.tick.label.wrap, x.tick.label.wrap.nchar)
    if (is.character(x))
        x <- autoFormatLongLabels(x, x.tick.label.wrap, x.tick.label.wrap.nchar)

    # Work out margin spacing
    margins <- list(t = 20, b = 20, r = 60, l = 80, pad = 0)
    margins <- setMarginsForAxis(margins, axisFormat, xaxis)
    margins <- setMarginsForAxis(margins, ylab.tmp, yaxis)
    margins <- setMarginsForText(margins, title, subtitle, footer, title.font.size,
                                 subtitle.font.size, footer.font.size)

    legend.text <- autoFormatLongLabels(g.list, legend.wrap, legend.wrap.nchar, remove.empty = FALSE)
    margins <- setMarginsForLegend(margins, legend.show || scatter.colors.as.numeric,
                    legend, legend.text)
    margins <- setCustomMargins(margins, margin.top, margin.bottom, margin.left,
                    margin.right, margin.inner.pad)

    ## START PLOTTING
    p <- plot_ly(data.frame(x = x,y = y))
    for (ggi in 1:num.groups)
    {
        ind <- which(groups == g.list[ggi])
        tmp.size <- if (!is.null(scatter.sizes)) scatter.sizes.scaled[ind]
                 else marker.size

        # initialise marker/line settings
        line.obj <- if (is.null(line.thickness) || line.thickness == 0) NULL
                    else list(width = line.thickness, color = line.colors[ggi])
        if (ggi == 1 && scatter.colors.as.numeric)
            marker.obj <- list(size = tmp.size, sizemode = "diameter", opacity = opacity,
                            line = list(width = marker.border.width,
                            color = toRGB(marker.border.colors[ggi], alpha = marker.border.opacity)),
                            color = colors[ggi], colorscale = col.scale, cmin = col.min, cmax = col.max,
                            showscale = colorbar.show, colorbar = colorbar)
        else
            marker.obj <- list(size = tmp.size, sizemode = "diameter", opacity = opacity,
                            color = colors[ggi],
                            line = list(width = marker.border.width,
                            color = toRGB(marker.border.colors[ggi], alpha = marker.border.opacity)))

        # add invisible trace to force correct order
        if (ggi == 1)
        {
            tmp.x <- NULL
            tmp.y <- NULL
            if (is.factor(x))
            {
                tmp.x <- levels(x)
                tmp.y <- minPosition(y, nlevels(x))
            }
            if (is.factor(y))
            {
                tmp.x <- c(tmp.x, minPosition(x, nlevels(y)))
                tmp.y <- c(tmp.y, levels(y))
            }
            if (!is.null(tmp.x))
                p <- add_trace(p, x = tmp.x, y = tmp.y, type = "scatter",
                       mode = "lines", hoverinfo = "none", showlegend = F, opacity = 0)
        }

        # Main trace
        separate.legend <- legend.show && scatter.colors.as.categorical && !is.null(scatter.sizes)
        p <- add_trace(p, x = x[ind], y = y[ind],
                #name  =  autoFormatLongLabels(paste0(g.list[ggi], " "), legend.wrap, legend.wrap.nchar),
                name = legend.text[ggi],
                showlegend = (legend.show && !separate.legend),
                legendgroup = if (num.series > 1) ggi else 1,
                textposition = data.label.position, cliponaxis = FALSE,
                textfont = if (data.label.show) data.label.font[[ggi]] else NULL,
                marker = marker.obj, line = line.obj, text = source.text[ind],
                hoverinfo = if (num.series == 1) "text" else "name+text",
                hoverlabel = list(font = list(color = autoFontColor(colors[ggi]),
                size = hovertext.font.size, family = hovertext.font.family)),
                type = "scatter", mode = series.mode, symbols = marker.symbols)

        # Getting legend with consistently sized markers
        if (separate.legend)
            p <- add_trace(p, x = list(NULL), y = list(NULL), name = legend.text[ggi],
                showlegend = TRUE, legendgroup = ggi, visible = TRUE,
                line = line.obj, marker = list(size = marker.size,
                opacity = opacity, color = colors[ggi]),
                type = "scatter", mode = series.mode, symbols = marker.symbols)


        if (fit.type != "None" && num.series > 1)
        {
            tmp.fit <- fitSeries(x[ind], y[ind], fit.type, fit.ignore.last, xaxis$type, fit.CI.show, warning.prefix)
            tmp.fname <- sprintf("%s: %s", fit.line.name, g.list[ggi])
            p <- add_trace(p, x = tmp.fit$x, y = tmp.fit$y, type = 'scatter', mode = "lines",
                      name = tmp.fname, legendgroup = ggi, showlegend = FALSE,
                      line = list(dash = fit.line.type, width = fit.line.width, shape = 'spline',
                      color = fit.line.colors[ggi]), opacity = fit.line.opacity)
            if (fit.CI.show && !is.null(tmp.fit$lb))
            {
                p <- add_trace(p, x = tmp.fit$x, y = tmp.fit$lb, type = 'scatter',
                        mode = 'lines', name = "Lower bound of 95%CI",
                        showlegend = FALSE, legendgroup = ggi,
                        line=list(color=fit.CI.colors[ggi], width=0, shape='spline'))
                p <- add_trace(p, x = tmp.fit$x, y = tmp.fit$ub, type = 'scatter',
                        mode = 'lines', name = "Upper bound of 95% CI",
                        fill = "tonexty", fillcolor = toRGB(fit.CI.colors[ggi], alpha = fit.CI.opacity),
                        showlegend = FALSE, legendgroup = ggi,
                        line = list(color=fit.CI.colors[ggi], width=0, shape='spline'))
            }
        }
    }
    if (fit.type != "None" && num.series == 1)
    {
        tmp.fit <- fitSeries(x, y, fit.type, fit.ignore.last, xaxis$type, fit.CI.show, warning.prefix)
        p <- add_trace(p, x = tmp.fit$x, y = tmp.fit$y, type = 'scatter', mode = 'lines',
                    name = fit.line.name, showlegend = FALSE, line = list(dash = fit.line.type,
                    width = fit.line.width, shape = 'spline',
                    color = fit.line.colors[1]), opacity = fit.line.opacity)
        if (fit.CI.show && !is.null(tmp.fit$lb))
        {
            p <- add_trace(p, x = tmp.fit$x, y = tmp.fit$lb, type = 'scatter',
                    mode = 'lines', name = "Lower bound of 95%CI", showlegend = FALSE,
                    line=list(color=fit.CI.colors[1], width=0, shape='spline'))
            p <- add_trace(p, x = tmp.fit$x, y = tmp.fit$ub, type = 'scatter',
                    mode = 'lines', name = "Upper bound of 95% CI", showlegend = FALSE,
                    fill = "tonexty", fillcolor = toRGB(fit.CI.colors[1], alpha = fit.CI.opacity),
                    line = list(color=fit.CI.colors[1], width=0, shape='spline'))
        }
    }
    annot <- list(setSubtitle(subtitle, subtitle.font, margins),
                  setTitle(title, title.font, margins),
                  if (is.null(small.mult.index)) setFooter(footer, footer.font, margins) else NULL)
    annot <- Filter(Negate(is.null), annot)

    p <- config(p, displayModeBar = modebar.show)
    p$sizingPolicy$browser$padding <- 0
    p <- layout(p,
        margin = margins,
        showlegend = legend.show,
        legend = legend,
        yaxis = yaxis,
        xaxis = xaxis,
        margin = margins,
        plot_bgcolor = toRGB(charting.area.fill.color, alpha = charting.area.fill.opacity),
        paper_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity),
        annotations = annot,
        hovermode = if (tooltip.show) "closest" else FALSE,
        hoverlabel = list(namelength = -1, bordercolor = charting.area.fill.color,
            font = list(size = hovertext.font.size, family = hovertext.font.family))
    )
    result <- list(htmlwidget = p)
    class(result) <- "StandardChart"
    result
}
