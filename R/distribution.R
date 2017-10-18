#' Distribution
#'
#' Distribution Chart.
#'
#' @param x A \code{\link{list}}, where each vector contains data to be plotted. If the data is not numeric
#' it is coerced to be numeric.
#' Input data may be a matrix or a vector, containing the height of the columns
#' to be plotted, with the name/rownames used as the column names of the chart. Numeric and date labels
#' will be parsed automatically.
#' @param weights An optional \code{\link{list}}, where each element is a vecytor containing weights corresponding to
#' the3 values of \code{x}.
#' @param vertical Display the densities vertically.
#' @param show.mean Displays the mean of the data.
#' @param show.median Displays the median of the data.
#' @param show.quartiles Displays the quartiles of the data.
#' @param show.range Displays the rage of the data.
#' @param show.density Show the left or top (if \code{vertical} is FALSE) of the violin plot.
#' @param show.mirror.density Show the right or bottom (if \code{vertical} is FALSE) of the violin plot.
#' @param show.values Produces a rug plot of individual values.
#' @param density.type Plot the density as a \code{"Density"} plot, \code{"Histogram"} plot, or \code{"Box"} plot. With box plots, the
#' 'whiskers' are drawn at the the most outlying point within 1.5 IQR (inter-quaritle range) below the first quarter and 1.5 IQR above the third quartile.
#' @param automatic.lower.density When \code{TRUE}, which is the default, the empirical lowest value is assumed to be the lowest value when estimating the density.
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
#' @param density.color Defaults to "Green"
#' @param title Character; chart title.
#' @param title.font.family Character; title font family. Can be "Arial Black",
#' "Arial", "Comic Sans MS", "Courier New", "Georgia", "Impact",
#' "Lucida Console", "Lucida Sans Unicode", "Marlett", "Symbol", "Tahoma",
#' "Times New Roman", "Trebuchet MS", "Verdana", "Webdings"
#' @param title.font.color Title font color as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param title.font.size Title font size; default = 10.
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
#' @param values.tick.marks Character; whether and where to show tick marks on the
#' y axis.  Can be "outside", "inside", "none"
#' @param values.tick.mark.length Length of tick marks in pixels.
#' @param values.bounds.minimum Minimum of range for plotting;
#' NULL = no manual range set.  Must be less than values.bounds.maximum
#' @param values.bounds.maximum Maximum of range for
#' plotting; NULL = no manual range set.  Must be greater than values.bounds.minimum
#' @param values.tick.distance The distance between the ticks. Requires that \code{values.bounds.minimum} and \code{values.bounds.maximum} have been set.
#' @param values.zero.line.width Width in pixels of zero line; 0 = no zero line
#' shown
#' @param values.zero.line.color Color of horizontal zero line as a named
#' color in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param values.grid.width Width of y-grid lines in pixels; 0 = no line
#' @param values.grid.color Color of y-grid lines as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param values.tick.show Whether to display the y-axis tick labels
#' @param values.tick.suffix y-axis tick label suffix
#' @param values.tick.prefix y-axis tick label prefix
#' @param values.tick.decimals y-axis tick label decimal places
#' @param values.tick.format.manual Overrides tick.prefix, suffix and decimals;
#' See https://github.com/mbostock/d3/wiki/Formatting#numbers or
#' https://docs.python.org/release/3.1.3/library/string.html#formatspec
#' @param values.hovertext.decimals y-axis hover text decimal places
#' @param values.hovertext.format.manual Overrides hovertext decimals;
#' See https://github.com/mbostock/d3/wiki/Formatting#numbers or
#' https://docs.python.org/release/3.1.3/library/string.html#formatspec
#' @param values.tick.angle y-axis tick label angle in degrees.
#' 90 = vertical; 0 = horizontal
#' @param values.tick.font.color y-axis tick label font color as a named color
#' in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param values.tick.font.family Character; y-axis tick label font family
#' @param values.tick.font.size y-axis tick label font size
#' @param categories.font.color X-axis tick label font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param categories.font.family Character; x-axis tick label font family
#' @param categories.font.size x-axis tick label font size
#' @param categories.label.wrap Logical; whether to wrap long labels on the x-axis.
#' @param categories.label.wrap.nchar Integer; number of characters in each line when \code{categories.label.wrap} is \code{TRUE}.
#' @param modebar.show Logical; whether to show the zoom menu buttons or not.
#' @param global.font.family Character; font family for all occurrences of any
#' font attribute for the chart unless specified individually.
#' @param global.font.color Global font color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. #' rgb(0, 0, 0, maxColorValue = 255)).
#' @param us.date.format Whether to apply the US convention when parsing dates.
#' @param ... Extra arguments that are ignored.
#' @return A \code{plotly} chart.
#' @examples
#' Distribution(list(rnorm(100)))
#' @importFrom grDevices rgb
#' @importFrom plotly plot_ly config toRGB add_trace add_text layout hide_colorbar
#' @importFrom stats loess loess.control lm predict
#' @importFrom flipTransformations AsNumeric
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
    automatic.lower.density = TRUE,
    histogram.cumulative = FALSE,
    histogram.counts = FALSE,
    maximum.bins = NULL,
    box.points = "Suspected outliers",
    mean.color = "White",
    median.color = "Black",
    quartile.color = "Black",
    range.color = "Black",
    values.color = "Green",
    density.color = "Green",
    global.font.family = "Arial",
    global.font.color = rgb(44, 44, 44, maxColorValue = 255),
    title = "",
    title.font.family = global.font.family,
    title.font.color = global.font.color,
    title.font.size = 16,
    background.fill.color = rgb(255, 255, 255, maxColorValue = 255),
    background.fill.opacity = 1,
    charting.area.fill.color = background.fill.color,
    charting.area.fill.opacity = 1,
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
    values.tick.marks = "",
    values.tick.mark.length = 5,
    values.bounds.minimum = NULL,
    values.bounds.maximum = NULL,
    values.tick.distance = NULL,
    values.zero.line.width = 0,
    values.zero.line.color = rgb(44, 44, 44, maxColorValue = 255),
    values.grid.width = 1 * grid.show,
    values.grid.color = rgb(225, 225, 225, maxColorValue = 255),
    values.tick.show = TRUE,
    values.tick.suffix = "",
    values.tick.prefix = "",
    values.tick.decimals = NULL,
    values.tick.format.manual = "",
    values.hovertext.decimals = NULL,
    values.hovertext.format.manual = "",
    values.tick.angle = NULL,
    values.tick.font.color = global.font.color,
    values.tick.font.family = global.font.family,
    values.tick.font.size = 10,
    categories.font.color = global.font.color,
    categories.font.family = global.font.family,
    categories.font.size = 10,
    categories.label.wrap = TRUE,
    categories.label.wrap.nchar = 21,
    modebar.show = FALSE,
    us.date.format = NULL)
{
    # Extracting and wrapping labels
    labels <- names(x)
    labels <- autoFormatLongLabels(labels, categories.label.wrap, categories.label.wrap.nchar)

    if (!is.list(x))
        stop("Input data should be a list of numeric vectors.")
    x <- AsNumeric(x, FALSE)
    if (density.type == "Box" && !is.null(weights))
    {
        warning("Weights are ignored in box plots.")
    }
    weights <- createWeights(x, weights)
    # Checking inputs.
    if (density.type != "Density" && show.mirror.density)
    {
        warning("Mirror densities are only shown with 'density.type' set to 'Density'.")
        show.mirror.density = FALSE
    }
    if (density.type == "Box" && any(show.values || show.mean || show.range || show.median || show.quartiles))
    {
        warning("Means, medians, quartiles, and values, will often cause problems when added to a box plot (as the box plot already shows this information).")
    }

    # Titles and footers
    title.font=list(family=title.font.family, size=title.font.size, color=title.font.color)

    values.title.font=list(family=values.title.font.family, size=values.title.font.size, color=values.title.font.color)
    values.tick.font=list(family=values.tick.font.family, size=values.tick.font.size, color=values.tick.font.color)
    categories.font=list(family=categories.font.family, size=categories.font.size, color=categories.font.color)

    # Work out margin spacing
    margins <- list(t = 20, b = 50, r = 60, l = 80, pad = 0)
    if (!is.null(margin.top))
        margins$t <- margin.top
    if (!is.null(margin.bottom))
        margins$b <- margin.bottom
    if (!is.null(margin.left))
        margins$l <- margin.left
    if (!is.null(margin.right))
        margins$r <- margin.right

    ## Initiate plotly object
    p <- plot_ly()
    p <- config(p, displayModeBar = modebar.show)
    p$sizingPolicy$browser$padding <- 0

    n.variables <- length(x)
    if (length(density.color) < n.variables)
        density.color <- rep(density.color, n.variables)
    if (length(density.color) != n.variables)
        warning("The number of colors provided for shading the densities is not consistent with the number of variables.")
    # working out the range of the data
    rng <- range(unlist(x), na.rm = TRUE)
    # Creating the violin plot
    for (v in 1:n.variables)
    {
        category.axis <- axisName(vertical, v, 1)
        value.axis <- axisName(vertical, v, 2)
        category.axis.2 <- axisName(vertical, v, 1, TRUE)
        value.axis.2 <- axisName(vertical, v, 2, TRUE)
        values <- x[[v]]
        # Removing missing values
        not.missing <- !is.na(values)
        wgt <- weights[[v]][not.missing]
        values <- values[not.missing]

        #
        density.from <- if (automatic.lower.density) rng[1] else NULL
        p <- addDensities(p, values, labels[v], vertical, show.density, show.mirror.density, density.type, histogram.cumulative, histogram.counts, maximum.bins, box.points, category.axis, value.axis, density.color, density.from)
        p <- addSummaryStatistics(p, values, wgt, vertical,  show.mean, show.median, show.quartiles, show.range, show.values,
                                 mean.color, median.color, quartile.color, range.color, values.color,
                                 category.axis, axisName(vertical, v, 1, TRUE), value.axis, value.axis.2)

    }
    # Finalizing the layout
    # Format axis labels
    if (is.null(values.tick.decimals))
        values.tick.decimals <- decimalsToDisplay(values)
    #categories.tick <- setTicks(categories.bounds.minimum, categories.bounds.maximum, categories.distance, FALSE)
    values.tick <- setTicks(values.bounds.minimum, values.bounds.maximum, values.tick.distance, FALSE)
    axisFormat <- formatLabels(values, "Area", categories.label.wrap, categories.label.wrap.nchar, us.date.format) #ignored

    if (is.null(values.bounds.minimum))
        values.bounds.minimum <- rng[1]
    if (is.null(values.bounds.maximum))
        values.bounds.maximum <- rng[2]
    values.axis <- setAxis(values.title, "left", axisFormat, values.title.font,
                  values.line.color, values.line.width, values.grid.width, values.grid.color,
                  values.tick, values.tick.font, values.tick.angle, values.tick.mark.length, values.tick.distance, values.tick.format.manual,
                  values.tick.decimals, values.tick.prefix, values.tick.suffix,
                  values.tick.show, FALSE, values.zero.line.width, values.zero.line.color,
                  values.hovertext.format.manual, values.hovertext.decimals)
    txt <- paste0("p <- layout(p, autosize=TRUE,
        font=list(size = 11),
        hovermode = 'closest',
        showlegend=FALSE,
        title = title,
        titlefont = title.font,
        showlegend = FALSE,", violinCategoriesAxes(vertical, n.variables, labels), "
", if (vertical) "y" else "x", "axis = values.axis,",
violinCategoriesAxes(vertical, n.variables, values.axis), "margin = margins,
plot_bgcolor = toRGB(charting.area.fill.color, alpha = charting.area.fill.opacity),
paper_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity),
hovermode = 'text',
titlefont = title.font)")
    eval(parse(text = txt))
   p
}







axisName <- function(vertical, n.variables, axis.number, secondary.category = FALSE)
{
    if ( n.variables == 1 && !secondary.category || vertical & axis.number == 2 || !vertical & axis.number == 1)
        return(if (vertical) "x" else "y")
    paste0(if (vertical) "x" else "y", n.variables * 2 - if (secondary.category) 0 else 1)
}



addDensities <- function(p, values, label, vertical, show.density, show.mirror.density, density.type, histogram.cumulative, histogram.counts, maximum.bins, box.points, category.axis, value.axis, density.color, density.from)
{
    # Comuting the density Also used in plotting other graphical elements.
    values.density <- if (is.null(density.from)) density(values, na.rm = TRUE) else density(values, from = density.from, na.rm = TRUE)
    attr(p, "values.density") <- values.density
    if (!show.density && !show.mirror.density)
        return(p)
    if (density.type == "Box")
    {
        p <-add_trace(p,
                      #orientation = if (vertical) "h" else "v",
                      boxpoints  = switch(box.points, "Outliers" = "outliers", "All" = "all", "Suspected outliers" = "suspectedoutliers"),
                      x = if (vertical) NULL else values,
                      y = if (vertical) values else NULL ,
                      marker = list(color = density.color),
                      name = label,
                      line = list(color = density.color),
                      hoverinfo = if (vertical) "y" else "x",
                      type = "box",
                      xaxis = category.axis,
                      yaxis = value.axis)
    } else if (density.type == "Histogram")
    {
        p <-add_trace(p,
                      #orientation = if (vertical) "h" else "v",
                      nbinsx = maximum.bins,
                      x = if (vertical) NULL else values,
                      y = if (vertical) values else NULL ,
                      marker = list(color = rep(density.color, max(100, maximum.bins))), # Hacking past a plotly bug
                      histnorm = if(histogram.counts) "" else "probability",
                      hoverinfo = if (vertical) "y" else "x",
                      cumulative = list(enabled = histogram.cumulative),
                      name = label,
                      cumulative = list(enabled = histogram.cumulative),
                      type = "histogram",
                      xaxis = category.axis,
                      yaxis = value.axis)
        return(p)
    } else # Density
        for (x.product in c(if (show.density) 1 else NULL, if (show.mirror.density) -1 else NULL))
            p <- add_trace(p,
               y = if (vertical) values.density$x else x.product * values.density$y,
               x = if (vertical) x.product * values.density$y else values.density$x,
               fill= if (vertical) "tozerox" else "tozeroy",
               fillcolor = density.color,
               hoverinfo= "none",
               line=list(shape = "spline", width = 0),
               mode = "lines",
               name = label,
               type = "scatter",
               xaxis = category.axis,
               yaxis = value.axis)
    p
}

createWeights <- function(x, weights)
{
    if (is.null(weights))
        group.sizes <- sapply(x, length)
    weights <- rep(1, sum(group.sizes))
    groups <- rep(1:length(x), group.sizes)
    tapply(weights, groups, c)
}

#' @importFrom stats density weighted.mean
#' @importFrom Hmisc wtd.quantile
addSummaryStatistics <- function(p, values, weights, vertical, show.mean, show.median, show.quartiles, show.range, show.values,
                                 mean.color, median.color, quartile.color, range.color, values.color,
                                 category.axis, category.axis.2, value.axis, value.axis.2)
{
    # Rug plot of values
    if (show.values)
    {
        v2 <- values
        v1 <- rep("rugplot", length(values))

        p <- add_trace(p,
              x = if (vertical) v1 else v2,
              y = if (vertical) v2 else v1,
              hoverinfo = "text",
              marker = list(color = values.color, symbol = if (vertical) "line-ew-open" else "line-ns-open"),
              mode = "markers",
              name = "",
              showlegend = FALSE,
              text = round(values, 2),
              type = "scatter",
              xaxis = category.axis.2,
              yaxis = value.axis.2)

    }
    ### Box plot
    if (show.median || show.quartiles || show.range)
    {
        five.num <- wtd.quantile(values, weights = weights, type = "i/(n+1)")
        names(five.num) <- c("Minimum:", "Lower quartile:", "Median:", "Upper quartile:", "Maximum:")

    }
    mn <- if(show.mean)  c("Mean:" = weighted.mean(values, w = weights)) else NULL
    # Function for adding components of boxplot to plot
    .addBox <- function(p, y, x, line = NULL, marker = NULL)
    {
        p <- add_trace(p,
                       x = x,
                       y = y,
                       line = line,
                       marker = marker,
                       hoverinfo = "text",
                       text = paste(names(y), round(y)),
                       mode = if (is.null(line)) "markers" else "lines",
                       name = "",
                       type = "scatter",
                       xaxis = category.axis,
                       yaxis = value.axis
        )
    }
    # Adding box plot components
    if (show.range)
    {
        v1 <- c(0, 0)
        v2 <- five.num[c(1, 5)]
        p <- .addBox(p, x = if (vertical) v1 else v2, y = if (vertical) v2 else v1, line = list(width = 1.5, color = range.color))
    }
    if (show.quartiles)
    {
        v1 <- c(0, 0)
        v2 <- five.num[c(2, 4)]
        p <- .addBox(p, x = if (vertical) v1 else v2, y = if (vertical) v2 else v1, line = list(width = 8, color = quartile.color))
    }
    if (show.median)
    {
        half.mean.width = 0.2 * max(abs(range(attr(p, "values.density")$y)))
        v1 <- c(-half.mean.width, half.mean.width)
        v2 <- rep(five.num[3], 2)
        p <- .addBox(p,  x = if (vertical) v1 else v2, y = if (vertical) v2 else v1, line = list(width = 4, color = median.color))
    }
    if (show.mean)
    {
        v1 <- 0
        v2 <- mn
        p <- .addBox(p,  x = if (vertical) v1 else v2, y = if (vertical) v2 else v1, marker = list(color = mean.color, symbol = "square"))
    }
    p

}


violinCategoryAxis <- function(i, label, n.variables, vertical, show.values, show.density, show.mirror.density, family, size, color)
{
    if (i > n.variables)
        return(NULL)
    if (!show.mirror.density)
        domain = c(if (show.values) .13 else 0, .95)
    else if (!show.density)
        domain = c(0, .9)
    else
        domain = c(0, 1)
    list(autorange = TRUE,
         domain =  domain / n.variables + (i - 1) / n.variables,
         showgrid = FALSE,
         showticklabels = FALSE,
         ticks = "",
         title = label,
         titlefont = list(family = family, size = size, color = color),
         type = "linear",
         zeroline = FALSE)

}

rugCategoryAxis <- function(i, n.variables, vertical, show.density, show.mirror.density, show.values)
{
    if(i > n.variables ||!show.values)
        return(NULL)
    if (show.density && show.mirror.density)
        domain = c(.45, .55)
    else if (show.density)
        domain = c(0, 0.1)
    else if (show.mirror.density)
        domain = c(.9, 1)

    list(autorange = TRUE,
         domain = domain / n.variables + (i - 1) / n.variables,
            autorange = TRUE,
            range = c(-1, 1),
            showgrid = FALSE,
            showticklabels = FALSE,
            title = "",
            type = "category")}

violinCategoriesAxes <- function(vertical, n.variables, labels)
{
    standard.parameters <- "n.variables, vertical, show.values, show.density, show.mirror.density, categories.font.family, categories.font.size, categories.font.color"
    axes <- paste0("xaxis = violinCategoryAxis(1, '", labels[1], "',", standard.parameters, "), xaxis2 = rugCategoryAxis(1, n.variables, vertical, show.density, show.mirror.density, show.values), ")
    if (n.variables > 1)
    {
        sq <- seq(4, n.variables * 2 , 2)
        violin <- paste0("xaxis", sq - 1, " = violinCategoryAxis(", 2:n.variables, ", '", labels[-1], "',", standard.parameters, "), ", collapse = "")
        rug <- paste0("xaxis", sq, " = rugCategoryAxis(", 2:n.variables, ", n.variables, vertical, show.density, show.mirror.density, show.values), ", collapse = "")
        axes <- paste0(axes, violin, rug)
    }
    if (!vertical)
        axes <- gsub("xaxis", "yaxis", axes)
    axes
}