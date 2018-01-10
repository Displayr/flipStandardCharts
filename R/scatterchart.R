#' Scatter
#'
#' Scatter plot
#'
#' @inherit LabeledScatter
#' @param scatter.labels.as.hovertext Logical; if TRUE, labels are shown has hovers; otherwise, as a labeled scatterplot.
#' @param scatter.sizes.as.diameter Whether to show the points with diameter (instead of area, which is the default) proportional to the sizes variable.
#' @param fit.type Character; type of line of best fit. Can be one of "None", "Linear" or "Smooth" (loess local polynomial fitting).
#' @param fit.ignore.last Boolean; whether to ignore the last data point in the fit.
#' @param fit.line.type Character; One of "solid", "dot", "dash, "dotdash", or length of dash "2px", "5px".
#' @param fit.line.width Numeric; Line width of line of best fit.
#' @param fit.line.name Character; Name of the line of best fit, which will appear in the hovertext.
#' @param opacity Opacity of area fill colors as an alpha value (0 to 1).
#' @param fit.line.colors Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' @param background.fill.color Background color in character format
#' (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param background.fill.opacity Background opacity as an alpha value
#' (0 to 1).
#' @param charting.area.fill.color Charting area background color as
#' a named color in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param charting.area.fill.opacity Charting area background
#' opacity as an alpha value (0 to 1).
#' @param legend.fill Same as \code{legend.fill.color}. Retained for backwards compatibility.
#' @param legend.fill.color Legend fill color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param legend.fill.opacity Legend fill opacity as an alpha value
#' (0 to 1).
#' @param legend.border.color Legend border color as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param legend.border.line.width Width in pixels of the border
#' around the legend.  0 = no border.
#' @param legend.position.x A numeric controlling the position of the legend.
#'   Values range from -0.5 (left) to 1.5 (right).
#' @param legend.position.y A numeric controlling the position of the legend.
#'   Values range from 0 (bottom) to 1 (top).
#' @param legend.ascending Logical; TRUE for ascending, FALSE for descending.
#' By default, we set it to to FALSE if the chart is stacked and TRUE otherwise.
#' @param margin.top Margin between plot area and the top of the
#' graphic in pixels
#' @param margin.bottom Margin between plot area and the bottom of the
#' graphic in pixels
#' @param margin.left Margin between plot area and the left of the
#' graphic in pixels
#' @param margin.right Margin between plot area and the right of the
#' graphic in pixels
#' @param margin.inner.pad Padding in pixels between plot proper
#' and axis lines
#' @param y.zero Whether the y-axis should include zero.
#' @param y.zero.line.width Width in pixels of zero line; 0 = no zero line
#' shown
#' @param y.zero.line.color Color of horizontal zero line as a named
#' color in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param y.data.reversed Logical; whether to reverse y-axis or not
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param y.hovertext.format A string representing a d3 formatting code.
#' See https://github.com/d3/d3/blob/master/API.md#number-formats-d3-format
#' @param y.tick.angle y-axis tick label angle in degrees.
#' 90 = vertical; 0 = horizontal
#' @param y.tick.mark.length Length of tick marks in pixels.
#' @param x.zero Whether the x-axis should include zero.
#' @param x.zero.line.width Width in pixels of zero line.
#' @param x.zero.line.color Color of horizontal zero (origo) line as a named
#' color in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param x.data.reversed Logical; whether to reverse x-axis or not
#' @param x.hovertext.format A string representing a d3 formatting code.
#' See https://github.com/mbostock/d3/wiki/Formatting#numbers
#' @param x.tick.angle x-axis tick label angle in degrees.
#' 90 = vertical; 0 = horizontal
#' @param x.tick.mark.length Length of tick marks in pixels.
#' @param x.tick.font.color X-axis tick label font color as a named color in
#' @param x.tick.label.wrap Logical; whether to wrap long labels on the x-axis.
#' @param x.tick.label.wrap.nchar Integer; number of characters in each
#' line when \code{label.wrap} is \code{TRUE}.
#' @param line.thickness Thickness, in pixels, of the series line
#' @param line.colors  Character; a vector containing one or more named
#' @param marker.size Size in pixels of marker. This is overriden
#' if \code{scatter.sizes} is provided, but used for the legend
#' if \code{scatter.colors.as.categorical}.
#' @param marker.border.width Width in pixels of border/line
#' @param marker.border.colors Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' @param marker.border.opacity Opacity of border/line around
#' markers as an alpha value (0 to 1).

#' around markers; 0 is no line
#' @param tooltip.show Logical; whether to show a tooltip on hover.
#' @param modebar.show Logical; whether to show the zoom menu buttons or not.
#' @param global.font.family Character; font family for all occurrences of any
#' font attribute for the chart unless specified individually.
#' @param global.font.color Global font color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. #' rgb(0, 0, 0, maxColorValue = 255)).
#' @param data.label.show Logical; whether to show data labels.
#' @param data.label.position Character; where to place the source data
#' value in relation to the marker icon.  Can be "top left", "top center", "top
#' right", "middle left", "middle center", "middle right", "bottom left",
#' "bottom center", "bottom right". Only applicable for line and area charts.
#' @param swap.x.and.y Swap the x and y axis around on the chart.
#' @param ... Extra arguments that are ignored.
#' @importFrom grDevices rgb
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
                         scatter.colors.as.categorical = FALSE,
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
                         legend.show = TRUE,
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
                         data.label.font.size = 10,
                         data.label.position = "top middle",
                         opacity = NULL,
                         background.fill.color = rgb(255, 255, 255, maxColorValue = 255),
                         background.fill.opacity = 0,
                         charting.area.fill.color = rgb(255, 255, 255, maxColorValue = 255),
                         charting.area.fill.opacity = 0,
                         legend.fill.color = rgb(255, 255, 255, maxColorValue = 255),
                         legend.fill.opacity = 0,
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
                         y.hovertext.format = y.tick.format,
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
                         x.hovertext.format = x.tick.format,
                         x.tick.angle = NULL,
                         x.tick.font.color = global.font.color,
                         x.tick.font.family = global.font.family,
                         x.tick.font.size = 10,
                         x.tick.label.wrap = TRUE,
                         x.tick.label.wrap.nchar = 21,
                         line.thickness = 0,
                         line.colors = colors,
                         marker.border.width = 1,
                         marker.border.colors = colors,
                         marker.border.opacity = 1,
                         marker.size = if (is.null(scatter.sizes)) 6 else 12,
                         swap.x.and.y = FALSE)
{
    # Use labeled scatterplots if multiple tables are provided
    if (is.list(x) && !is.data.frame(x) |     # Use labeled scatterplots if labels are provided in (row)names
         !scatter.labels.as.hovertext &&
        (!is.null(rownames(x))|| (length(dim(x)) < 2 && !is.null(names(x)))))
    {
        cl <- as.list(match.call())
        cl <- cl[-1]
        cl$scatter.labels.as.hovertext <- NULL
        return(do.call(LabeledScatter, cl))
    }

    # Grouping font attributes to simplify passing to plotly
    title.font = list(family = title.font.family, size = title.font.size, color = title.font.color)
    subtitle.font = list(family = subtitle.font.family, size = subtitle.font.size, color = subtitle.font.color)
    x.title.font = list(family = x.title.font.family, size = x.title.font.size, color = x.title.font.color)
    y.title.font = list(family = y.title.font.family, size = y.title.font.size, color = y.title.font.color)
    ytick.font = list(family = y.tick.font.family, size = y.tick.font.size, color = y.tick.font.color)
    xtick.font = list(family = x.tick.font.family, size = x.tick.font.size, color = x.tick.font.color)
    footer.font = list(family = footer.font.family, size = footer.font.size, color = footer.font.color)
    legend.font = list(family = legend.font.family, size = legend.font.size, color = legend.font.color)
    data.label.font = list(family = data.label.font.family, size=data.label.font.size, color = data.label.font.color)

    # Try to store name of variables
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
            if ((is.na(y.title) || nchar(y.title) == 0) && !is.null(colnames(x)))
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
        if (((is.na(x.title) || nchar(x.title) == 0) && !is.null(colnames(x))) && .isValidColumnIndex(scatter.x.column))
            x.title <- colnames(x)[scatter.x.column]
        if (!.isValidColumnIndex(scatter.x.column))
            x <- NULL
        else
            x <- x[,scatter.x.column]
    }
    if (is.null(scatter.labels) && !is.null(names(x)))
        scatter.labels <- names(x)

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
        warning("Chart contains overlapping points in the same position.")

    # Remove NAs
    not.na <- !is.na(x) & !is.na(y)
    if (sum(not.na) != n)
        warning("Data points with missing values have been omitted.")
    n <- length(x)
    if (!is.null(scatter.sizes))
    {
        if (length(scatter.sizes) != n)
            stop("'scatter.sizes' should be a numeric vector with the same number of observations as 'x'.")
        if (any(!is.finite(suppressWarnings(AsNumeric(scatter.sizes, binary = FALSE)))))
        {
            warning("Some points omitted due to missing values in 'scatter.sizes'.")
            not.na <- not.na & is.finite(suppressWarnings(AsNumeric(scatter.sizes, binary = FALSE)))
        }
    }
    if (!is.null(scatter.colors))
    {
        if (length(scatter.colors) != n)
            stop("'scatter.colors' should be a vector with the same number of observations as 'x'.")
        if (any(is.na(scatter.colors)))
        {
            warning("Some points omitted due to missing values in 'scatter.colors'")
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

    opacity <- 1
    n <- sum(not.na)
    if (!is.null(scatter.sizes))
    {
        sc.tmp <- abs(AsNumeric(scatter.sizes, binary = FALSE))
        if (!scatter.sizes.as.diameter)
            sc.tmp <- sqrt(sc.tmp)
        if (any(class(scatter.sizes) %in% c("Date", "POSIXct", "POSIXt")))
            scatter.sizes.scaled <- (sc.tmp - min(sc.tmp, na.rm=T))/diff(range(sc.tmp, na.rm=T)) * 50
        else
            scatter.sizes.scaled <- sc.tmp/max(sc.tmp, na.rm=T) * 50
        opacity <- 0.4
    }

    scatter.colors.as.numeric <- 0
    colorbar <- NULL
    groups <- rep("Series 1", n)
    if (!is.null(scatter.colors) && !scatter.colors.as.categorical)
    {
        # make colorscalebar
        col.fun <- colorRamp(unique(colors))  # undo recycling in PrepareColors
        c.tmp <- rgb(col.fun((0:5)/5), maxColorValue=255)
        v.tmp <- seq(from=0, to=1, length=length(c.tmp))
        col.scale <- mapply(function(a,b)c(a,b), a=v.tmp, b=c.tmp, SIMPLIFY=F)

        # getting labels for all types
        if (is.character(scatter.colors))
            scatter.colors <- as.factor(scatter.colors)
        if (any(class(scatter.colors) %in% c("Date", "POSIXct", "POSIXt")))
        {
            tmp.seq <- seq(0, 1, length=5)
            colorbar <- list(tickmode="array", tickvals=tmp.seq,
                             ticktext=c(min(scatter.colors) + diff(range(scatter.colors)) * tmp.seq),
                             outlinewidth=0, tickfont=legend.font)
        }
        else if (any(class(scatter.colors) == "factor"))
            colorbar <- list(tickmode="array", tickvals=seq(0, 1, length=nlevels(scatter.colors)),
                             ticktext=levels(scatter.colors), outlinewidth=0, tickfont=legend.font)
        else
            colorbar <- list(outlinewidth = 0, tickfont=legend.font)

        scatter.colors.as.numeric <- 1
        groups <- 1:n
        opacity <- 1
        col.tmp <- AsNumeric(scatter.colors, binary=FALSE)
        scatter.colors.scaled <- (col.tmp - min(col.tmp, na.rm=T))/diff(range(col.tmp, na.rm=T))
        scatter.colors.labels <- col.tmp
        if (any(class(scatter.colors) == "factor") || any(class(scatter.colors) %in% c("Date", "POSIXct", "POSIXt")))
            scatter.colors.labels <- scatter.colors.scaled
        colors <- rgb(col.fun(scatter.colors.scaled), maxColorValue=255)
    }
    if (!is.null(scatter.colors) && scatter.colors.as.categorical)
        groups <- as.factor(scatter.colors)
    if (is.factor(groups))
        g.list <- levels(groups) # fix legend order
    else
        g.list <- unique(groups)

    num.groups <- length(g.list)
    num.series <- if (scatter.colors.as.numeric) 1 else num.groups

    # hovertext
    x.str <- if (is.numeric(x)) FormatAsReal(x, decimals = decimalsFromD3(x.hovertext.format)) else as.character(x)
    y.str <- if (is.numeric(y)) FormatAsReal(y, decimals = decimalsFromD3(y.hovertext.format)) else as.character(y)
    source.text <- paste0(scatter.labels, " (", x.tick.prefix, x.str, x.tick.suffix, ", ",
                          y.tick.prefix, y.str, y.tick.suffix, ")")
    if (!is.null(scatter.colors.name))
    {
        colors.str <- if (is.numeric(scatter.colors)) FormatAsReal(scatter.colors, decimals = decimalsFromD3(x.hovertext.format)) else as.character(scatter.colors)
        source.text <- paste0(source.text, "<br>", scatter.colors.name, ": ", colors.str)
    }
    if (!is.null(scatter.sizes.name))
    {
        sizes.str <- if (is.numeric(scatter.sizes)) FormatAsReal(scatter.sizes, decimals = decimalsFromD3(x.hovertext.format)) else as.character(scatter.sizes)
        source.text <- paste0(source.text, "<br>", scatter.sizes.name, ": ", sizes.str)
    }


    # other constants
    hover.mode <- if (tooltip.show) "closest" else FALSE
    colorbar.show <- legend.show
    legend.show <- legend.show && num.series > 1
    scatter.opacity <- if (!is.null(scatter.sizes)) 0.4 else 1
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
                               footer, scatter.labels.name)
        if (!is.null(scatter.colors.name))
            footer <- sprintf("%sPoints colored according to '%s'; ",
                              footer, scatter.colors.name)
        if (!is.null(scatter.sizes.name))
            footer <- sprintf("%s%s of points are proportional to absolute value of '%s'; ",
                              footer,
                              if (scatter.sizes.as.diameter) "Diameter" else "Area",
                              scatter.sizes.name)
    }
    footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate=FALSE)

    # Format axis labels
    #if (is.null(y.tick.decimals))
    #    y.tick.decimals <- decimalsToDisplay(as.numeric(y))
    xtick <- setTicks(x.bounds.minimum, x.bounds.maximum, x.tick.distance, x.data.reversed)
    ytick <- setTicks(y.bounds.minimum, y.bounds.maximum, y.tick.distance, y.data.reversed)

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

    # Work out margin spacing
    margins <- list(t = 20, b = 50, r = 60, l = 80, pad = 0)
    margins <- setMarginsForAxis(margins, axisFormat, xaxis)
    margins <- setMarginsForAxis(margins, ylab.tmp, yaxis)
    margins <- setMarginsForText(margins, title, subtitle, footer, title.font.size,
                                 subtitle.font.size, footer.font.size)
    margins <- setMarginsForLegend(margins, legend.show || scatter.colors.as.numeric,
                    legend, scatter.colors)
    margins <- setCustomMargins(margins, margin.top, margin.bottom, margin.left,
                    margin.right, margin.inner.pad)
    footer.axis <- setFooterAxis(footer, footer.font, margins)

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
                            color = scatter.colors.labels, colorscale = col.scale,
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
        p <- add_trace(p, x = x[ind], y = y[ind], name = paste0(g.list[ggi], " "),
                showlegend = (legend.show && !separate.legend),
                legendgroup = if (num.series > 1) ggi else 1,
                textposition = data.label.position,
                marker = marker.obj, line = line.obj, text = source.text[ind],
                hoverinfo = if (num.series == 1) "text" else "name+text",
                type="scatter", mode=series.mode, symbols = marker.symbols)

        # Getting legend with consistently sized markers
        if (separate.legend)
            p <- add_trace(p, x = list(NULL), y = list(NULL), name = g.list[ggi],
                showlegend = TRUE, legendgroup = ggi, visible = TRUE,
                line = line.obj, marker = list(size = marker.size,
                opacity = opacity, color = colors[ggi]),
                type = "scatter", mode = series.mode, symbols = marker.symbols)


        if (fit.type != "None" && num.series > 1)
        {
            tmp.fit <- fitSeries(x[ind], y[ind], fit.type, fit.ignore.last, xaxis$type)
            tmp.fname <- sprintf("%s: %s", fit.line.name, g.list[ggi])
            p <- add_trace(p, x = tmp.fit$x, y = tmp.fit$y, type = 'scatter', mode = "lines",
                      name = tmp.fname, legendgroup = ggi, showlegend = F,
                      line = list(dash = fit.line.type, width = fit.line.width,
                      color = fit.line.colors[ggi], shape = 'spline'))
        }
    }
    if (fit.type != "None" && num.series == 1)
    {
        tmp.fit <- fitSeries(x, y, fit.type, fit.ignore.last, xaxis$type)
        p <- add_trace(p, x = tmp.fit$x, y = tmp.fit$y, type = 'scatter', mode = 'lines',
                    name = fit.line.name, showlegend = F, line = list(dash = fit.line.type,
                    width = fit.line.width, color = fit.line.colors[1], shape = 'spline'))
    }
    p <- addSubtitle(p, subtitle, subtitle.font, margins)
    p <- config(p, displayModeBar = modebar.show)
    p$sizingPolicy$browser$padding <- 0
    p <- layout(p,
        margin = margins,
        xaxis4 = footer.axis,
        title = title,
        showlegend = legend.show,
        legend = legend,
        yaxis = yaxis,
        xaxis = xaxis,
        margin = margins,
        plot_bgcolor = toRGB(charting.area.fill.color, alpha = charting.area.fill.opacity),
        paper_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity),
        hovermode = hover.mode,
        titlefont = title.font,
        font = data.label.font
    )
    result <- list(plotly.plot = p)
    class(result) <- "StandardChart"
    result
}
