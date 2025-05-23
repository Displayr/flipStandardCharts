#' Distribution
#'
#' Distribution Chart.
#'
#' @param x A \code{\link{list}}, where each vector contains data to be plotted. If the data is not numeric
#' it is coerced to be numeric.
#' Input data may be a matrix or a vector, containing the height of the columns
#' to be plotted, with the name/rownames used as the column names of the chart. Numeric and date labels
#' will be parsed automatically.
#' @param weights An optional \code{\link{list}}, where each element is a vector containing weights corresponding to
#' the values of \code{x}, or, a vector where the weights is assumed applicable for each element in \code{x}.
#' @param vertical Display the densities vertically.
#' @param show.mean Displays the mean of the data.
#' @param show.median Displays the median of the data.
#' @param show.quartiles Displays the quartiles of the data.
#' @param show.range Displays the range of the data.
#' @param show.density Show the left or top (if \code{vertical} is FALSE) of the violin plot.
#' @param show.mirror.density Show the right or bottom (if \code{vertical} is FALSE) of the violin plot.
#' @param show.values Produces a rug plot of individual values.
#' @param density.type Plot the density as a \code{"Density"} plot, \code{"Histogram"} plot, or \code{"Box"} plot. With box plots, the
#' 'whiskers' are drawn at the the most outlying point within 1.5 IQR (inter-quaritle range) below the first quarter and 1.5 IQR above the third quartile.
#' @param bw The smoothing bandwidth to be used when creating a Density,
#' Bean, or Violin plot. This defaults to \code{"nrd0"}, whereas \code{"SJ"} may often be superior (see \code{\link{density}}).
#' The default is to \code{"nrd0"} as \code{"SJ"} fails with trivial categorical cases.
#' @param adjust A scaling factor for the bandwidth when creating a Density, Bean, or Violin plot. E.g., a value of 0.5 sets the bandwidth to have of that computed using \code{bw}.
#' @param kernel The kernel used when when creating a Density, Bean, or Violin plot. One of "gaussian" (the default), "epanechnikov", "rectangular", "triangular", "biweight", "cosine", "optcosine".
#' @param n The number of equally-sapced points at which the density is to be estimated when creating a Density, Bean, or Violin plot. If greater than 512, it is rounded to a power of 2 (see \code{link{density}}).
#' @param from The left-most point of the grid used when creating a Density, Bean, or Violin plot.
#' @param to The right-most point of the grid used when creating a Density, Bean, or Violin plot.
#' @param cut By default, the values of \code{from} and \code{to} are \code{cut} bandwidths beyond the extremes of the data.
#' @param automatic.lower.density When \code{TRUE}, which is the default, \code{from} is set to the lowest value in the data.
#' @param histogram.cumulative Plots the cumulative histogram, if \code{histogram} is set to TRUE.
#' @param histogram.counts Displays the counts in tooltips of a histogram, rather than the proportions.
#' @param maximum.bins The maximum number of bins of the histogram. If \code{NULL}, this is generated automatically.
#' @param box.points How outliers are displayed boxplots. \code{"All"} plots all the points. \code{"Suspected outliers"} plots points
#' between 1.5 and 3 IQR from the 1st and 3rd quartile with un-filled circles. \code{"Outliers"} does not plot points between 1.5 and 3 IQR from the 1st and 3rd quartiles.
#' @param mean.color Defaults to "white"
#' @param median.color Defaults to "black"
#' @param quartile.color Defaults to "black",
#' @param range.color Defaults to "black"
#' @param values.color Defaults to "Green"
#' @param density.color Defaults to "Green". This can be a single color or a vector of colors, with the same length as the number of variables
#' @param title Character; chart title.
#' @param title.font.family Character; title font family. Can be "Arial Black",
#' "Arial", "Comic Sans MS", "Courier New", "Georgia", "Impact",
#' "Lucida Console", "Lucida Sans Unicode", "Marlett", "Symbol", "Tahoma",
#' "Times New Roman", "Trebuchet MS", "Verdana", "Webdings"
#' @param title.font.color Title font color as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param title.font.size Title font size; default = 10.
#' @param title.align Horizontal alignment of title
#' @param subtitle Character
#' @param subtitle.font.color subtitle font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param subtitle.font.family Character; subtitle font family
#' @param subtitle.font.size Integer; subtitle font size
#' @param subtitle.align Horizontal alignment of subtitle
#' @param footer Character
#' @param footer.font.color footer font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param footer.font.family Character; footer font family
#' @param footer.font.size Integer; footer font size
#' @param footer.align Horizontal alignment of footer
#' @param footer.wrap Logical; whether the footer text should be wrapped.
#' @param footer.wrap.nchar Number of characters (approximately) in each line of the footer when \code{footer.wordwrap} \code{TRUE}.

#' @param grid.show Logical; whether to show grid lines.
#' @param background.fill.color Background color in character format
#' (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param background.fill.opacity Background opacity as an alpha value
#' (0 to 1).
#' @param charting.area.fill.color Charting area background color as
#' a named color in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param charting.area.fill.opacity Charting area background
#' opacity as an alpha value (0 to 1).
#' @param margin.top Margin between plot area and the top of the
#' graphic in pixels
#' @param margin.bottom Margin between plot area and the bottom of the
#' graphic in pixels
#' @param margin.left Margin between plot area and the left of the
#' graphic in pixels
#' @param margin.right Margin between plot area and the right of the
#' graphic in pixels
#' @param values.title Character, y-axis title; defaults to chart input values;
#' to turn off set to "FALSE".
#' @param values.title.font.color y-axis title font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0,
#' max = 255)).
#' @param values.title.font.family Character; y-axis title font family
#' @param values.title.font.size y-axis title font size
#' @param values.line.width y-axis line in pixels, 0 = no line
#' @param values.line.color y-axis line color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param values.tick.mark.length Length of tick marks in pixels.
#' @param values.tick.mark.color Color of tick marks.
#' @param values.bounds.minimum Minimum of range for plotting;
#' NULL = no manual range set.  Must be less than values.bounds.maximum
#' @param values.bounds.maximum Maximum of range for
#' plotting; NULL = no manual range set.  Must be greater than values.bounds.minimum
#' @param values.tick.distance The distance between the ticks. Requires that \code{values.bounds.minimum}
#' and \code{values.bounds.maximum} have been set.
#' @param values.tick.maxnum Maximum number of ticks shown on the values axis.
#' @param values.zero Logical; whether a line should be shown when at values = 0.
#' @param values.zero.line.width Width in pixels of zero line; 0 = no zero line
#' shown
#' @param values.zero.line.color Color of horizontal zero line as a named
#' color in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param values.zero.line.dash Line type of zero line. One of "Solid", "Dot", "Dash".
#' @param values.grid.width Width of y-grid lines in pixels; 0 = no line
#' @param values.grid.color Color of y-grid lines as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param values.tick.show Whether to display the y-axis tick labels
#' @param values.tick.suffix y-axis tick label suffix
#' @param values.tick.prefix y-axis tick label prefix
#' @param values.tick.format d3 formatting string applied to the tick labels.
#' See https://github.com/mbostock/d3/wiki/Formatting#numbers
#' @param values.hovertext.format d3 formatting string applied to the hover text.
#' https://github.com/mbostock/d3/wiki/Formatting#numbers or
#' @param values.tick.angle y-axis tick label angle in degrees.
#' 90 = vertical; 0 = horizontal
#' @param values.tick.font.color y-axis tick label font color as a named color
#' in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param values.tick.font.family Character; y-axis tick label font family
#' @param values.tick.font.size y-axis tick label font size
#' @param categories.tick.font.color X-axis tick label font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param categories.tick.font.family Character; x-axis tick label font family
#' @param categories.tick.font.size x-axis tick label font size
#' @param categories.tick.label.wrap Logical; whether to wrap long labels on the x-axis.
#' @param categories.tick.label.wrap.nchar Integer; number of characters in each line when \code{categories.tick.label.wrap} is \code{TRUE}.
#' @param categories.tick.angle Angle of the categories tick label.
#' @param categories.tick.mark.length Distance between tick labels (variable names)
#'  and axis. Note that this parameter name is chosen to be analgous to
#'  the same parameter in other charts, but it is not a a true "tick" label
#'  so categories.tick.mark.color is set as transparent.
#' @param modebar.show Logical; whether to show the zoom menu buttons or not.
#' @param zoom.enable Logical; whether to enable zoom on the chart.
#'  For Bar and Column charts with data labels it may be useful to turn off zoom
#'  because data labels and annotations can be misplace on zoom.

#' @param global.font.family Character; font family for all occurrences of any
#' font attribute for the chart unless specified individually.
#' @param global.font.color Global font color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. #' rgb(0, 0, 0, maxColorValue = 255)).
#' @param hovertext.font.family Font family of hover text.
#' @param hovertext.font.size Font size of hover text.
#' @param hover.on Only used for boxplot. If "all", then all the hovertext (for median,
#   quartiles, range) will be shown simultaneously. If "points", then only the hovertext
#'  for the element near the cursor will be shown.
#' @param tooltip.show Logical; whether to show a tooltip on hover.
#' @return A \code{plotly} chart.
#' @examples
#' Distribution(rnorm(100))
#' Distribution(list(rnorm(100), rexp(100)))
#' @importFrom grDevices rgb nclass.FD
#' @importFrom plotly plot_ly config toRGB add_trace add_text layout hide_colorbar
#' @importFrom stats loess loess.control lm predict sd
#' @importFrom flipTransformations AsNumeric
#' @importFrom flipU StopForUserError
#' @importFrom verbs Sum
#' @export
Distribution <-   function(x,
    weights = NULL,
    vertical = TRUE,
    show.mean = TRUE,
    show.median = TRUE,
    show.quartiles  = TRUE,
    show.range = TRUE,
    show.density = TRUE,
    show.mirror.density = TRUE,
    show.values = FALSE,
    density.type = "Density",
    bw = "nrd0",
    adjust = 1,
    kernel = c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight", "cosine", "optcosine"),
    n = 512,
    from = NULL,
    to = NULL,
    cut = 3,
    automatic.lower.density = TRUE,
    histogram.cumulative = FALSE,
    histogram.counts = FALSE,
    maximum.bins = NULL,
    box.points = "Suspected outliers",
    mean.color = "White",
    median.color = "Black",
    quartile.color = "Black",
    range.color = "Black",
    values.color = "#008000",
    density.color = "#008000",
    global.font.family = "Arial",
    global.font.color = rgb(44, 44, 44, maxColorValue = 255),
    title = "",
    title.font.family = global.font.family,
    title.font.color = global.font.color,
    title.font.size = 16,
    title.align = "center",
    subtitle = "",
    subtitle.font.family = global.font.family,
    subtitle.font.color = global.font.color,
    subtitle.font.size = 12,
    subtitle.align = "center",
    footer = "",
    footer.font.family = global.font.family,
    footer.font.color = global.font.color,
    footer.font.size = 8,
    footer.align = "center",
    footer.wrap = TRUE,
    footer.wrap.nchar = 100,
    background.fill.color = "transparent",
    background.fill.opacity = 1,
    charting.area.fill.color = background.fill.color,
    charting.area.fill.opacity = 0,
    margin.top = NULL,
    margin.bottom = NULL,
    margin.left = NULL,
    margin.right = NULL,
    grid.show = FALSE,
    values.title = "",
    values.title.font.color = global.font.color,
    values.title.font.family = global.font.family,
    values.title.font.size = 12,
    values.line.width = 0,
    values.line.color = rgb(0, 0, 0, maxColorValue = 255),
    values.tick.mark.color = "transparent",
    values.tick.mark.length = 0,
    values.bounds.minimum = NULL,
    values.bounds.maximum = NULL,
    values.tick.distance = NULL,
    values.tick.maxnum = NULL,
    values.zero = FALSE,
    values.zero.line.width = 0,
    values.zero.line.color = rgb(44, 44, 44, maxColorValue = 255),
    values.zero.line.dash = "Solid",
    values.grid.width = 1 * grid.show,
    values.grid.color = rgb(225, 225, 225, maxColorValue = 255),
    values.tick.show = TRUE,
    values.tick.suffix = "",
    values.tick.prefix = "",
    values.tick.format = "",
    values.hovertext.format = "",
    values.tick.angle = NULL,
    values.tick.font.color = global.font.color,
    values.tick.font.family = global.font.family,
    values.tick.font.size = 10,
    categories.tick.angle = NULL,
    categories.tick.mark.length = 20,
    categories.tick.font.color = global.font.color,
    categories.tick.font.family = global.font.family,
    categories.tick.font.size = 10,
    categories.tick.label.wrap = TRUE,
    categories.tick.label.wrap.nchar = 21,
    hovertext.font.family = global.font.family,
    hovertext.font.size = 11,
    hover.on = c("all", "points")[1],
    zoom.enable = TRUE,
    tooltip.show = TRUE,
    modebar.show = FALSE)
{
    # Extracting and wrapping labels
    ErrorIfNotEnoughData(x, require.tidy = FALSE, require.notAllMissing = TRUE)
    if (length(x) == 1 && is.list(x) && NCOL(x[[1]]) > 1)
        x <- x[[1]]
    if (!is.list(x) && (is.array(x) || inherits(x, "QTable")))
        x <- as.data.frame(checkMatrixNames(x))
    else if (is.numeric(x))
        x <- as.data.frame(x) # not a Q table
    else if (!is.list(x))
    {
        if (is.array(x) && length(dim(x)) == 1)
        {
            x <- list(x)
            names(x) <- attributes(x[[1]])$name
        }
        else if (NCOL(x) == 1)
            x <- list(x)
    }
    if (!is.list(x))
        StopForUserError("Input data should be a list of numeric vectors or a matrix.")

    # Checking for categories with no data.
    all.missing <- sapply(x, function(x) all(is.na(x)))
    if (any(all.missing))
    {
        warning("The following categories contain only missing values: ",
                paste(names(all.missing)[all.missing], sep = ","))
        x <- x[!all.missing]
    }
    # Adding in a title based on name if only 1 statistic.
    if (length(x) == 1 && values.title == "")
    {
        table.name <- attributes(x[[1]])$label
        if (is.null(table.name))
            table.name <- attributes(x[[1]])$name
        if(!is.null(table.name))
            values.title <- table.name
    }
    # Extracting labels
    labels <- names(x)
    if (length(labels) == 1)
        labels = ""
    else
        labels <- autoFormatLongLabels(labels, categories.tick.label.wrap, categories.tick.label.wrap.nchar)
    x <- AsNumeric(x, FALSE)
    # Warnings for chart types that cannot deal with weights.
    if (!is.null(weights))
    {
        if (density.type == "Box")
        {
            warning("Weights are ignored in box plots.")
        }
        else if (density.type == "Histogram")
        {
            if (sd(weights) != 0)
                warning("Weights are ignored in histograms. To create a weighted histogram, either (1), ",
                        "create a Histogram Chart in Q or Displayr from the menus or R by code, or (2) ",
                        "manually create the categories and create a column chart.")
        }
    }

    # Checking inputs.
    if (density.type != "Density" && show.mirror.density)
    {
        warning("Mirror densities are only shown with 'density.type' set to 'Density'.")
        show.mirror.density = FALSE
    }
    if (density.type == "Box")
    {
        if (show.values)
        {
            show.values <- FALSE
            box.points <- "All"
        }
        if (any(show.mean || show.range || show.median || show.quartiles))
            warning("Means, medians, quartiles, and values, will often cause problems when added to a box plot (as the box plot already shows this information).")
    }
    # Titles and footers
    if (!any(nzchar(values.title)))
        values.title.font.size = 0
    title.font = list(family = title.font.family, size = title.font.size, color = title.font.color)
    subtitle.font = list(family = subtitle.font.family, size = subtitle.font.size, color = subtitle.font.color)
    footer.font = list(family = footer.font.family, size = footer.font.size, color = footer.font.color)
    values.title.font = list(family = values.title.font.family, size = values.title.font.size, color = values.title.font.color)
    values.tick.font = list(family = values.tick.font.family, size = values.tick.font.size, color = values.tick.font.color)
    categories.tick.font = list(family = categories.tick.font.family, size = categories.tick.font.size, color = categories.tick.font.color)
    footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate = FALSE)

    # Work out margin spacing
    if (vertical)
        margins <- list(t = 20, b = 30, r = 60,
                        l = 60 + values.title.font.size, pad = 0)
    else
        margins <- list(t = 20, b = 30 + values.tick.font.size + values.title.font.size,
                        r = 60, l = 0, pad = 0)
    if (is.null(categories.tick.angle))
        categories.tick.angle <- if (vertical) 0 else 270

    if (vertical || categories.tick.angle == 0)
        margins <- setMarginsForAxis(margins, labels, list(tickfont = categories.tick.font,
                     side = if (vertical) "bottom" else "left", tickangle = categories.tick.angle))
    else    # to avoid affecting old charts
        margins$l <- 80
    margins <- setMarginsForText(margins, title, subtitle, footer, title.font.size,
                                 10, 10)
    margins <- setCustomMargins(margins, margin.top, margin.bottom, margin.left,
                    margin.right, 0)
    if (!values.tick.show)
        margins$autoexpand <- FALSE

    ## Initiate plotly object
    p <- plot_ly()
    p <- config(p, displayModeBar = modebar.show)
    p$sizingPolicy$browser$padding <- 0

    n.variables <- length(x)
    if (length(density.color) < n.variables)
        density.color <- rep(density.color, n.variables)
    if (length(density.color) != n.variables)
        warning("The number of colors provided for shading the densities is not consistent with the number of variables.")
    # Histograms can be specified using 'maximum.bins' or 'bin.size' in plotly.
    # The user, however, only has access to 'maximum.bins'
    # Note however, that in plotly, 'maximum.bins' is ignored when 'bin.size'
    # is provided, and is sometimes still ignored if it is not
    x.vals <- unlist(x)
    x.vals <- x.vals[!is.na(x.vals)]
    x.sorted <- sort(unique(x.vals))
    rng <- x.sorted[c(1, length(x.sorted))]
    if (density.type == "Histogram")
    {
        bin.min.size <- suppressWarnings(min(diff(x.sorted)))
        if (!is.finite(bin.min.size))
            bin.min.size <- 1
        if (bin.min.size < sqrt(.Machine$double.eps))
            bin.min.size <- (rng[2] - rng[1]) * 1e-6
        rng <- rng  + c(-1, 1) * bin.min.size/2 # expand range if values are integers

        default.bins <- is.null(maximum.bins) || is.na(maximum.bins)
        if (default.bins) {
            # Use a lower estimate of the bin size to identify when
            # the default plotly algorithm runs into trouble
            maximum.bins = (rng[2] - rng[1])/bin.min.size
            bin.size <- NULL
            if (maximum.bins > 1000)
            {
                # Force a ceiling on the number of bins used to avoid browser freesing
                num.bins <- min(1000, nclass.FD(x.vals))
                bin.size = (rng[2] - rng[1])/num.bins
                default.bins <- FALSE
            }
            else if (length(x.sorted) < 10)
            {
                # Use smaller bins when there are only a few values. This avoids grouping
                # values together if they are unevenly spaced inside larger range.
                bin.size <- bin.min.size
                default.bins <- FALSE
            }
            else if (rng[2] <= 2.0)
            {
                # Address a bug in plotly when dealing with small numbers
                bin.size <- bin.min.size
                default.bins <- FALSE
            }
            # else leave bin.size = NULL, which will be determined by plotly
        }
        else
            bin.size = (rng[2] - rng[1])/maximum.bins
        bins <- list(start = rng[1], end = rng[2], size = bin.size)
    }

    # Creating the violin plot
    for (v in 1:n.variables)
    {
        category.axis <- axisName(vertical, v, 1)
        value.axis <- axisName(vertical, v, 2)
        values <- x[[v]]

        # When values is only a single value we duplicate the value
        # This makes the density functions work properly and also
        # avoid errors from formatLabels
        if (length(values) == 1) {
            values <- rep(values, length = 2)
        }
        wgt <- if (is.null(weights)) rep(1, length(values)) else
                (if (is.list(weights)) weights[[v]] else weights)
        if (length(wgt) != length(values))
            StopForUserError("The data and the weights do not have the same number of observations.")
        # Removing missing values
        not.missing <- !is.na(values)
        values <- values[not.missing]
        values.names <- if (is.null(rownames(x))) which(not.missing) else rownames(x)[which(not.missing)]
        wgt <- wgt[not.missing]
        wgt <- prop.table(wgt) # Rebasing the weight (Required by the density function)
        from <- if (automatic.lower.density) rng[1] else from
        p <- addDensities(p, values, wgt, labels[v], vertical, show.density, show.mirror.density, density.type, histogram.cumulative, histogram.counts, bins, maximum.bins, box.points, category.axis, value.axis, density.color[v], values.color, bw, adjust, kernel, n, from, to, cut, hover.on)
        p <- addSummaryStatistics(p, values, wgt, vertical, show.density, show.mirror.density, show.mean, show.median, show.quartiles, show.range, show.values,
                                 mean.color, median.color, quartile.color, range.color, values.color,
                                 category.axis, value.axis, values.names)

    }
    # Finalizing the layout
    # Format axis labels
    values.range <- setValRange(values.bounds.minimum, values.bounds.maximum, rng)
    values.tick <- setTicks(values.range$min, values.range$max, values.tick.distance, FALSE)

    axisFormat <- formatLabels(values, "Area", categories.tick.label.wrap, categories.tick.label.wrap.nchar, "", values.tick.format) #ignored
    if (is.null(values.bounds.minimum))
        values.bounds.minimum <- rng[1]
    if (is.null(values.bounds.maximum))
        values.bounds.maximum <- rng[2]
    values.axis <- setAxis(values.title, if (vertical) "left" else "bottom",
         axisFormat, values.title.font,
         values.line.color, values.line.width, values.grid.width, values.grid.color,
         values.tick, values.tick.font, values.tick.angle, values.tick.mark.length,
         values.tick.distance, values.tick.format, values.tick.prefix,
         values.tick.suffix, values.tick.show, values.zero, values.zero.line.width,
         values.zero.line.color, values.hovertext.format, num.maxticks = values.tick.maxnum,
         tickcolor = values.tick.mark.color, zoom.enable = zoom.enable)
    hover.mode <- if (tooltip.show) "'closest'" else "FALSE"
    annotations <- setCategoriesAxesTitles(vertical, labels, categories.tick.font, categories.tick.angle, categories.tick.mark.length)
    n <- length(annotations)
    annotations[[n+1]] <- setTitle(title, title.font, margins, title.align)
    annotations[[n+2]] <- setFooter(footer, footer.font, margins, footer.align)
    annotations[[n+3]] <- setSubtitle(subtitle, subtitle.font, margins, subtitle.align)
    annotations <- Filter(Negate(is.null), annotations)
    value.zeroline <- if (vertical) zerolines(FALSE, 0, "", "Solid", values.zero, values.zero.line.width, values.zero.line.color, values.zero.line.dash)
        else zerolines(values.zero, values.zero.line.width, values.zero.line.color, values.zero.line.dash, FALSE, 0, "")

    txt <- paste0("p <- layout(p,
        autosize = TRUE,
        font = list(size = 11),
        hovermode = ", hover.mode, ",",
        "showlegend = FALSE,
        showlegend = FALSE,",
        if (vertical) "y" else "x", "axis = values.axis,",
        violinCategoriesAxes(vertical, n.variables, gsub("'", "\\\\'", labels)),
        "margin = margins,
        annotations = annotations,
        shapes = value.zeroline,
        hoverlabel = list(namelength = -1,
            font = list(size = hovertext.font.size, family = hovertext.font.family)),
        plot_bgcolor = toRGB(charting.area.fill.color, alpha = charting.area.fill.opacity),
        paper_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity))")
    eval(parse(text = txt))

    attr(p, "can-run-in-root-dom") <- TRUE
    result <- list(htmlwidget = p)
    class(result) <- "StandardChart"
    if (n.variables == 1)
        attr(result, "ChartType") <- "Histogram"
    else
        attr(result, "ChartType") <- "Box & Whisker"
    result
}







axisName <- function(vertical, n.variables, axis.number, secondary.category = FALSE)
{
    if ( n.variables == 1 && !secondary.category || vertical & axis.number == 2 || !vertical & axis.number == 1)
        return(if (vertical) "x" else "y")
    paste0(if (vertical) "x" else "y", n.variables * 2 - if (secondary.category) 0 else 1)
}



addDensities <- function(p,
                         values,
                         weights,
                         label,
                         vertical,
                         show.density,
                         show.mirror.density,
                         density.type,
                         histogram.cumulative,
                         histogram.counts,
                         bins,
                         maximum.bins,
                         box.points,
                         category.axis,
                         value.axis,
                         density.color,
                         values.color,
                         # Density parameters
                         bw, adjust, kernel, n, from, to, cut,
                         hover.on)
{
    # Comuting the density Also used in plotting other graphical elements.
    d.args <- list(x = values, na.rm = TRUE, bw = bw, adjust = adjust, kernel = kernel, cut = cut, weights = weights)
    if (!is.null(from))
        d.args$from = from
    if (!is.null(to))
        d.args$from = to
    values.density <- do.call(density, d.args)
    attr(p, "values.density") <- values.density
    if (!show.density && !show.mirror.density)
        return(p)
    if (density.type == "Box")
    {
        p <-add_trace(p,
                      boxpoints  = switch(box.points, "Outliers" = "outliers", "All" = "all", "Suspected outliers" = "suspectedoutliers"),
                      x = if (vertical) NULL else values,
                      y = if (vertical) values else NULL,
                      fillcolor = rgb(t(col2rgb(density.color[1])), alpha = 128, maxColorValue = 255),
                      marker = list(color = values.color),
                      name = label,
                      line = list(color = density.color),
                      hoverinfo = if (hover.on == "points") "skip" else if (vertical) "y" else "x",
                      type = "box",
                      xaxis = category.axis,
                      yaxis = value.axis)

        if (hover.on == "points")
        {
            # Unlike violin plots, box plots do not accept weights
            # For consistency with plotly we use type = 5 (midpoints)
            # this differs from violin plot quantiles (type = 6; i/n+1)
            five.num <- quantile(values, type = 5)
            names(five.num) <- c("Minimum:", "Lower quartile:", "Median:", "Upper quartile:", "Maximum:")
            five.pos <- rep(0, length(five.num))

            p <- add_trace(p,
                           x = if (vertical) five.pos else five.num,
                           y = if (vertical) five.num else five.pos,
                           type = "scatter",
                           mode = "markers", cliponaxis = FALSE,
                           marker = list(color = "transparent"),
                           hoverlabel = list(bgcolor = density.color[1]),
                           hoverinfo = "text",
                           text = paste(names(five.num), round_half_up(five.num, 2)))
        }
    } else if (density.type == "Histogram")
    {
        p <- add_trace(p,
                      xbins = if (!vertical) bins else NULL,
                      ybins = if (vertical) bins else NULL,
                      nbinsx = maximum.bins,
                      nbinsy = maximum.bins,
                      x = if (vertical) NULL else values,
                      y = if (vertical) values else NULL ,
                      marker = list(color = density.color[1]),
                      histnorm = if(histogram.counts) "" else "probability",
                      cumulative = list(enabled = histogram.cumulative),
                      name = label,
                      type = "histogram",
                      xaxis = category.axis,
                      yaxis = value.axis)
        return(p)
    } else # Density
        for (x.product in c(if (show.density) 1 else NULL, if (show.mirror.density) -1 else NULL))
            p <- add_trace(p,
               y = if (vertical) values.density$x else x.product * values.density$y,
               x = if (vertical) x.product * values.density$y else values.density$x,
               fill = if (vertical) "tozerox" else "tozeroy",
               fillcolor = density.color[1],
               hoverinfo = "none",
               line = list(shape = "spline", width = 0),
               mode = "lines",
               name = label,
               type = "scatter",
               xaxis = category.axis,
               yaxis = value.axis)
    p
}



#' @importFrom stats density weighted.mean quantile
#' @importFrom Hmisc wtd.quantile
addSummaryStatistics <- function(p, values, weights, vertical, show.density, show.mirror.density,
                                 show.mean, show.median, show.quartiles, show.range, show.values,
                                 mean.color, median.color, quartile.color, range.color, values.color,
                                 category.axis, value.axis, value.names)
{
    # Rug plot of values
    if (show.values)
    {
        v.max <- max(abs(range(attr(p, "values.density")$y)))
        if (show.density && show.mirror.density)
            rug.pos <- 0
        else if (show.density)
            rug.pos <- -0.1 * v.max
        else if (show.mirror.density)
            rug.pos <- 0.1 * v.max

        vpos <- rep(rug.pos, length(values))
        v.tmp <- tapply(value.names, values, paste, collapse = ", ")
        v2 <- as.numeric(names(v.tmp))
        v1 <- rep(rug.pos, length(v2))

        # Trace showing rugs
        p <- add_trace(p, hoverinfo = "none",
              x = if (vertical) vpos else values, y = if (vertical) values else vpos,
              marker = list(color = values.color, symbol = if (vertical) "line-ew-open" else "line-ns-open"),
              type = "scatter", mode = "markers",
              xaxis = category.axis, yaxis = value.axis, showlegend = FALSE)

        # Invisible trace for hovertext only
        p <- add_trace(p, hoverinfo = "text",
              x = if (vertical) v1 else v2, y = if (vertical) v2 else v1,
              text = autoFormatLongLabels(paste0("<b>", trimws(round_half_up(v2, 2)), "</b>: ", v.tmp),
                            wordwrap = TRUE, n = 30),
              type = "scatter", mode = "markers", marker = list(color = values.color, opacity = 0.0),
              xaxis = category.axis, yaxis = value.axis, showlegend = FALSE)

    }
    ### Violin plot
    if (show.median || show.quartiles || show.range)
    {
        # Quantiles are computed in the same way as SPSS
        # This corresponds to type = 6 (default is type = 7); see Rdocs for stats::quantile
        five.num <- wtd.quantile(values, weights = weights, type = "i/(n+1)", normwt = TRUE)
        names(five.num) <- c("Minimum:", "Lower quartile:", "Median:", "Upper quartile:", "Maximum:")

    }
    mn <- if(show.mean)  c("Mean:" = weighted.mean(values, w = weights)) else NULL
    # Function for adding components of boxplot to plot
    .addBox <- function(p, y, x, name, line = NULL, marker = NULL)
    {
        p <- add_trace(p,
                       x = x,
                       y = y,
                       line = line,
                       marker = marker,
                       name = name,
                       hoverinfo = paste0("name+", if (vertical) "y" else "x"),
                       mode = if (is.null(line)) "markers" else "lines",
                       type = "scatter", cliponaxis = FALSE,
                       xaxis = category.axis,
                       yaxis = value.axis
        )
    }
    # Adding box plot components
    if (show.range)
    {
        v1 <- c(0, 0)
        v2 <- five.num[c(1, 5)]
        p <- .addBox(p, x = if (vertical) v1 else v2, y = if (vertical) v2 else v1, "Range", line = list(width = 1.5, color = range.color))
    }
    if (show.quartiles)
    {
        v1 <- c(0, 0)
        v2 <- five.num[c(2, 4)]
        p <- .addBox(p, x = if (vertical) v1 else v2, y = if (vertical) v2 else v1, "Quartiles", line = list(width = 8, color = quartile.color))
    }
    if (show.median)
    {
        half.mean.width = 0.2 * max(abs(range(attr(p, "values.density")$y)))
        v1 <- c(-half.mean.width, half.mean.width)
        v2 <- rep(five.num[3], 2)
        p <- .addBox(p,  x = if (vertical) v1 else v2, y = if (vertical) v2 else v1, "Median", line = list(width = 4, color = median.color))
    }
    if (show.mean)
    {
        v1 <- 0
        v2 <- mn
        p <- .addBox(p,  x = if (vertical) v1 else v2, y = if (vertical) v2 else v1, "Mean", marker = list(color = mean.color, symbol = "square"))
    }
    p

}


violinCategoryAxis <- function(i, label, n.variables, vertical, show.values, show.density, show.mirror.density, family,
                               size, color, tickangle, values.hovertext.format)
{
    if (i > n.variables)
        return(NULL)
    if (!show.mirror.density)
        domain = c(if (show.values) .12 else 0, .95)
    else if (!show.density)
        domain = c(0, .9)
    else
        domain = c(0, 1)
    list(autorange = TRUE,
         domain =  domain / n.variables + (i - 1) / n.variables,
         hoverformat = values.hovertext.format,
         showgrid = FALSE,
         showticklabels = FALSE,
         ticks = "",
         #title = label,
         #titlefont = list(family = family, size = size, color = color),
         type = "linear",
         zeroline = FALSE)

}


violinCategoriesAxes <- function(vertical, n.variables, labels)
{
    standard.parameters <- "n.variables, vertical, show.values, show.density, show.mirror.density, categories.tick.font.family, categories.tick.font.size, categories.tick.font.color, categories.tick.angle, values.hovertext.format"
    axes <- paste0("xaxis = violinCategoryAxis(1, '", labels[1], "',", standard.parameters, "),")
    if (n.variables > 1)
    {
        sq <- seq(4, n.variables * 2 , 2)
        violin <- paste0("xaxis", sq - 1, " = violinCategoryAxis(", 2:n.variables, ", '", labels[-1], "',", standard.parameters, "), ", collapse = "")
        axes <- paste0(axes, violin)
    }
    if (!vertical)
        axes <- gsub("xaxis", "yaxis", axes)
    axes
}


setCategoriesAxesTitles <- function(vertical, labels, font, angle, ticklen)
{
    if (is.null(angle))
        angle <- 0
    n <- length(labels)
    sq <- seq(1, n * 2 , 2)
    if (!vertical)
        axes <- lapply(1:n, function(i)
            return(list(text = labels[i], showarrow = FALSE, font = font, textangle = angle,
                xref = "paper", x = -0.01, xanchor = "right", xshift = -ticklen,
                yref = paste0("y", sq[i], " domain"), y = 0.5)))

    else
        axes <- lapply(1:n, function(i)
            return(list(text = labels[i], showarrow = FALSE, font = font, textangle = angle,
                yref = "paper", y = 0.0, yanchor = "top", yshift = -ticklen,
                xref = paste0("x", sq[i], " domain"), x = 0.5)))
    return(axes)
}


distributionArgs <- function(call, chart.function, arguments)
{
    args <- modifyList(as.list(args(chart.function)), arguments)
    nms <- names(args)
    nms <- nms[nms != ""]
    nms <- nms[!nms %in% names(call)]
    args <- args[nms]
    args <- args[!sapply(args, is.null)]
    call[[1]] <- Distribution
    call <- modify_call(call, args)
    as.list(call[-1])
}
