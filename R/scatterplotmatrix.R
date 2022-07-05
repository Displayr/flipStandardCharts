#' Scatterplot matrix
#'
#' Creates a matrix of scatterplots showing the distribution of variable pairs
#' @param x A dataframe containing the variables to be plotted.
#' @param weights A numeric vector with length equal to the number of rows in \code{x}.
#' @param seed A seed used to generate random variables for jitter.
#' @param modifications One of "None", "Jitter", or "Enlarge points with multiple observations".
#' @param fit.type Character; type of line of best fit to show in the scatterplot in the
#'   lower triangle of the graphic. One of "None", "LOESS", or "Straight".
#' @param fit.line.type Character; One of "solid", "dot", "dash, "dotdash", or length of dash "2px", "5px".
#' @param fit.line.width Numeric; Line width of line of best fit.
#' @param fit.line.color Color of line of best fit.
#' @param fit.line.opacity Opacity of trend line as an alpha value (0 to 1).
#' @param point.symbol Character; symbol used in scatterplots in the lower triangle of the graphic
#'  See \url{https://plotly-r.com/working-with-symbols.html} for a list of symbol names.
#' @param point.size Size of points in scatterplots in pixel. If point sizes vary by frequency, then
#'  this parameter specfies the maximum size of the points.
#' @param point.color Color of the points in the scatterplots.
#' @param point.opacity Opacity of the points in the scatterplots as an alpha value (0 to 1).
#' @param histogram.color Color of the histogram bars shown in the diagonal panels.
#' @param histogram.opacity Opeacity of the histogram bars as an alpha value (0 to 1).
#' @param correlation.decimals Number of decimals used to show correlation values in the upper trangle.
#' @param correlation.font.family Font family of correlation values shown in the upper triangle.
#' @param correlation.font.color Font color of correlation values.
#' @param correlation.font.size Font size of correlation values in pixels.
#' @param label.font.family Font family of the variable name labels shown in the diagonal panels.
#' @param label.font.color Font color of the variable name labels.
#' @param label.font.size Font size of the variable name labels in units of pixels.
#' @param hovertext.font.family Font family of hover text (tooltips). Font color will be automatically
#'  determined based on the color of the points or histograms.
#' @param hovertext.font.size Font size of hover text in pixels.
#' @param tick.font.family Font family of ticks labels (across all panel).
#' @param tick.font.color Font color of the tick labels.
#' @param tick.font.size Font size tick labels in pixels.
#' @param tick.length Length of tick labels in pixels.
#' @param tick.format Format of the ticklabels in D3 (e.g. ".1f"). Leave blank for plotly to
#'  automatically set format based on values.
#' @param panel.outline Logical; whether of not to show outline around each of the panels.
#' @param panel.line.width Line width of panel.outllne in pixels.
#' @param panel.line.color Color of panel lines.
#' @param global.font.family Character; font family for all occurrences of any
#' font attribute for the chart unless specified individually.
#' @param global.font.color Global font color as a named color in character format
#' (e.g. "black") or an a hex code.
#' @param title Character; chart title.
#' @param title.font.family Character; title font family. Can be "Arial Black",
#' "Arial", "Comic Sans MS", "Courier New", "Georgia", "Impact",
#' "Lucida Console", "Lucida Sans Unicode", "Marlett", "Symbol", "Tahoma",
#' "Times New Roman", "Trebuchet MS", "Verdana", "Webdings"
#' @param title.font.color Title font color as a named color in character
#' format (e.g. "black") or a hex code.
#' @param title.font.size Integer; Title font size; default = 10.
#' @param subtitle Character
#' @param subtitle.font.color subtitle font color as a named color in
#' character format (e.g. "black") or an a hex code.
#' @param subtitle.font.family Character; subtitle font family
#' @param subtitle.font.size Integer; subtitle font size
#' @param footer Character
#' @param footer.font.color footer font color as a named color in
#' character format (e.g. "black") or an a hex code.
#' @param footer.font.family Character; footer font family
#' @param footer.font.size Integer; footer font size
#' @param footer.wrap Logical; whether the footer text should be wrapped.
#' @param footer.wrap.nchar Number of characters (approximately) in each
#' line of the footer when \code{footer.wrap} \code{TRUE}.
#' @param background.fill.color Background color in character format (e.g. "black") or a hex code.
#' @param background.fill.opacity Background opacity as an alpha value (0 to 1).
#' @param charting.area.fill.color Charting area background color as
#' a named color in character format (e.g. "black") or a hex code.
#' @param charting.area.fill.opacity Charting area background opacity as an alpha value (0 to 1).
#' @param panel.gap Space between the panels of the scatterplot matrix as a proportion of the total graphic.
#' @param panel.extend The extent to which the range in each panel should extend past the range of the variable.
#' @param margin.top Margin between charting area and the top of the graphic in pixels.
#' @param margin.bottom Margin between charting area and the bottom of the graphic in pixels.
#' @param margin.left Margin between charting area and the left of the graphic in pixels.
#' @param margin.right Margin between charting area and the right of the graphic in pixels.
#' @param margin.autoexpand Logical; Whether extra space can be added to the margins
#'      to allow space for axis labels or other chart elements.
#' @param tooltip.show Logical; whether to show a tooltip on hover.
#' @param modebar.show Logical; whether to show the zoom menu buttons or not.
#' @param zoom.enable Logical; whether to enable zoom on the chart.
#' @param axis.drag.enable Logical; whether to enable the user to drag along axes.
#'  This interaction is available when the cursor shows up as a double-headed arrow
#'  when hovering over an axis. It is turned off by default because it can sometimes
#'  cause problems with data labels and annotations. Also, is only used when
#'  \code{zoom.enable = TRUE}. Note that in similar functionality is already available
#'  using zoom.

#' @importFrom plotly plot_ly add_trace layout subplot toRGB config
#' @importFrom stats lm loess rnorm
#' @importFrom hash hash has.key values
#' @importFrom flipTransformations AsNumeric
#' @export
ScatterplotMatrix <- function(x, weights = 1:NROW(x), seed = 123,
                              modifications = "Enlarge points with multiple observations",
                              fit.type = "None",
                              fit.line.type = "solid",
                              fit.line.width = 2,
                              fit.line.color = "#990000",
                              fit.line.opacity = 1,
                              point.symbol = "circle",
                              point.size = 10,
                              point.color = "#5C9AD3",
                              point.opacity = 0.4,
                              histogram.color = "#5C9AD3",
                              histogram.opacity = 1.0,
                              correlation.decimals = 2,
                              background.fill.color = "transparent",
                              background.fill.opacity = 1,
                              charting.area.fill.color = "transparent",
                              charting.area.fill.opacity = 1,
                              global.font.family = "Arial",
                              global.font.color = "#222222",
                              label.font.family = global.font.family,
                              label.font.color = global.font.color,
                              label.font.size = 12,
                              correlation.font.family = global.font.family,
                              correlation.font.color = global.font.color,
                              correlation.font.size = 12,
                              hovertext.font.family = global.font.family,
                              hovertext.font.size = 10,
                              tick.font.family = global.font.family,
                              tick.font.color = global.font.color,
                              tick.font.size = 9,
                              tick.length = 3,
                              tick.format = "",
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
                              panel.outline = TRUE,
                              panel.line.width = 1,
                              panel.line.color = "#BBBBBB",
                              panel.extend = 0.1,
                              panel.gap = 0.01,
                              margin.left = NULL,
                              margin.right = NULL,
                              margin.top = NULL,
                              margin.bottom = NULL,
                              margin.autoexpand = TRUE,
                              modebar.show = FALSE,
                              tooltip.show = TRUE,
                              axis.drag.enable = FALSE,
                              zoom.enable = FALSE)

{
    set.seed(seed)
    if (is.null(ncol(x)) || ncol(x) < 2)
        stop("Input data must contain at least two variables.")
    x <- suppressWarnings(AsNumeric(x, binary = FALSE))
    n <- ncol(x)
    labels <- colnames(x)
    label.font <- list(family = label.font.family, color = label.font.color, size = label.font.size)
    correlation.font <- list(family = correlation.font.family, color = correlation.font.color,
        size = correlation.font.size)
    blank.axis <- list(range = c(0, 1), showgrid = FALSE, showticklabels = FALSE, mirror = TRUE,
        showline = panel.outline, linewidth = panel.line.width, linecolor = panel.line.color,
        zeroline = FALSE, fixedrange = !zoom.enable)
    tick.font <- list(family = tick.font.family, color = tick.font.color, size = tick.font.size)
    hist.hover <- list(font = list(family = hovertext.font.family, size = hovertext.font.size,
        color = autoFontColor(histogram.color)), bgcolor = histogram.color)
    point.hover <- list(font = list(family = hovertext.font.family, size = hovertext.font.size,
        color = autoFontColor(point.color)), bgcolor = point.color)
    fit.hover <- list(font = list(family = hovertext.font.family, size = hovertext.font.size,
        color = autoFontColor(fit.line.color)), bgcolor = fit.line.color)
    point.marker <- list(symbol = point.symbol, size = point.size,
        color = toRGB(point.color, alpha = point.opacity),
        line = list(color = toRGB(point.color, alpha = point.opacity)))
    fit.line.marker <- list(dash = fit.line.type, width = fit.line.width, shape = 'spline',
         color = fit.line.color, opacity = fit.line.opacity)

    panels <- list()
    k <- 1
    for (i in 1:n)
        for (j in 1:n)
        {
            xi.rng <- range(x[,i], na.rm = TRUE, finite = TRUE)
            xi.diff <- xi.rng[2] - xi.rng[1]
            xj.rng <- range(x[,j], na.rm = TRUE, finite = TRUE)
            xj.diff <- xj.rng[2] - xj.rng[1]

            if (i == j)
                panels[[k]] <- panel_hist(x[,i], weights = weights,
                    label = labels[i], label.font = label.font,
                    color = histogram.color, opacity = histogram.opacity,
                    hover.style = hist.hover, axis = blank.axis,
                    x.range = xj.rng + xj.diff * panel.extend * c(-1,1))
            else if (j > i)
                panels[[k]] <- panel_cor(x[,j], x[,i], weights = weights,
                    decimals = correlation.decimals, font = correlation.font, axis = blank.axis)
            else
                panels[[k]] <- panel_scatter(x[,j], x[,i], x.name = labels[j], y.name = labels[i],
                    weights, modifications,
                    fit.type, fit.line.marker, fit.hover,
                    point.marker, point.hover,
                    xaxis = list(range = xj.rng + xj.diff * panel.extend * c(-1,1),
                        showgrid = FALSE, tickfont = tick.font, tickformat = tick.format,
                        showline = panel.outline, mirror = TRUE, ticks = "outside", ticklen = tick.length,
                        linewidth = panel.line.width, linecolor = panel.line.color, zeroline = FALSE,
                        showticklabels = i == n, tickcolor = panel.line.color, fixedrange = !zoom.enable),
                    yaxis = list(range = xi.rng + xi.diff * panel.extend * c(-1,1),
                        showgrid = FALSE, tickfont = tick.font, tickformat = tick.format,
                        showline = panel.outline, mirror = TRUE, ticks = "outside", ticklen = tick.length,
                        linewidth = panel.line.width, linecolor = panel.line.color, zeroline = FALSE,
                        showticklabels = j == 1, tickcolor = panel.line.color, fixedrange = !zoom.enable))
            k <- k + 1
        }

    # Adjust spacing between panels
    h.offset <- c(panel.gap, rep(0, max(0, n - 2)), panel.gap)[1:n]
    w.offset <- c(panel.gap, rep(0, max(0, n - 2)), panel.gap)[1:n]
    if (panel.gap >= 1/(2*n))
        stop("'Panel gap' should be between 0 and 1/(2n) (",
             round(1/(2*n), 4), ")")
    res <- subplot(panels, nrows = n, margin = panel.gap,
        heights = rep(1/n, n) - h.offset,
        widths  = rep(1/n, n) - w.offset)

    # Title, subtitle and footer
    annotations <- list()
    title.font <- list(family = title.font.family, size = title.font.size, color = title.font.color)
    subtitle.font <- list(family = subtitle.font.family, size = subtitle.font.size, color = subtitle.font.color)
    footer.font <- list(family = footer.font.family, size = footer.font.size, color = footer.font.color)
    footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate = FALSE)
    margins <- list(l = 20, r = 20, b = 20, t = 20)
    margins <- setMarginsForText(margins, title, subtitle, footer, title.font.size,
                                 subtitle.font.size, footer.font.size)
    margins <- setCustomMargins(margins, margin.top, margin.bottom, margin.left, margin.right, 0)
    margins$autoexpand <- margin.autoexpand
    
    if (nzchar(title))
        annotations[[length(annotations)+1]] <- setTitle(title, title.font, margins)
    if (nzchar(subtitle))
        annotations[[length(annotations)+1]] <- setSubtitle(subtitle, subtitle.font, margins)
    if (nzchar(footer))
        annotations[[length(annotations)+1]] <- setFooter(footer, footer.font, margins) 

    res <- config(res, displayModeBar = modebar.show, showAxisDragHandles = axis.drag.enable)
    res$sizingPolicy$browser$padding <- 0
    res <- layout(res, showlegend = FALSE, margin = margins, annotations = annotations,
        plot_bgcolor = toRGB(charting.area.fill.color, alpha = charting.area.fill.opacity),
        paper_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity),
        hovermode = if (tooltip.show) "closest" else FALSE,
        hoverlabel = list(namelength = -1, bordercolor = "transparent",
            font = list(size = hovertext.font.size, family = hovertext.font.family, color ="#222222")))
    attr(res, "can-run-in-root-dom") <- TRUE
    class(res) <- c(class(res), "visualization-selector")
    return(res)
}

#' @importFrom weights wtd.cors wtd.hist
panel_cor <- function(x, y, weights, decimals, font, axis)
{
    r <- wtd.cors(x, y, weight = weights)
    txt <- FormatAsReal(r, decimals = decimals)
    pp <- plot_ly()
    pp <- add_trace(pp, x = 0.5, y = 0.5, text = txt, type = "scatter", mode = "text",
            hoverinfo = "skip", textfont = font)
    pp <- layout(pp,
        xaxis = axis, yaxis = axis)
    return(pp)
}


panel_hist <- function(x, weights, label, label.font, color, opacity, hover.style, axis, x.range)
{
    h <- wtd.hist(x, plot = FALSE, weight = weights)
    breaks <- h$breaks
    nB <- length(breaks)
    x0 <- (breaks[-nB] + breaks[-1])/2
    y <- h$counts
    y <- y / sum(y)
    hover.text = sprintf("P(%.2f < x < %.2f) = %.2f", breaks[-nB], breaks[-1], y)
    xaxis <- axis
    yaxis <- axis
    xaxis$range <- x.range
    yaxis$range <- c(0, 1.5 * max(y))

    pp <- plot_ly()
    pp <- add_trace(pp, x = x0, y = y, type = "bar", showlegend = FALSE,
        marker = list(color = toRGB(color, alpha = opacity)), textposition = "none",
        name = label, hoverinfo = "name+text", text = hover.text, hoverlabel = hover.style)
    pp <- layout(pp, xaxis = xaxis, yaxis = yaxis,
        annotations = list(text = label, showarrow = FALSE, font = label.font,
        x = 0.5, y = 0.85, xref = "paper", yref = "paper", xanchor = "center", yanchor = "middle"))
    return(pp)
}


panel_scatter <- function(x, y, x.name, y.name, weights, modifications,
    fit.type, fit.line.marker, fit.hover.style,
    point.marker, point.hover.style, xaxis, yaxis)
{
    sz.pts <- 1
    if (modifications == "None")
    {
        x.pts <- x
        y.pts <- y
    }
    else if (modifications == "Jitter")
    {
        x.pts <- x + rnorm(length(x)) * (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 0.01
        y.pts <- y + rnorm(length(y)) * (max(y, na.rm = TRUE) - min(y, na.rm = TRUE)) * 0.01
    }
    else if (modifications == "Enlarge points with multiple observations")
    {
        c.hash <- hash()
        x.hash <- hash()
        y.hash <- hash()
        for (i in seq(x))
        {
            key <- paste(x[i], y[i], collapse = ",")

            if (has.key(key, c.hash))
                c.hash[[key]] <- c.hash[[key]] + weights[i]
            else
            {
                c.hash[[key]] <- weights[i]
                x.hash[[key]] <- x[i]
                y.hash[[key]] <- y[i]
            }
        }
        x.pts <- values(x.hash)
        y.pts <- values(y.hash)
        sz.const <- point.marker$size
        sz.raw <- sqrt(values(c.hash) / sum(weights))
        sz.min <- min(sz.raw, na.rm = TRUE)
        sz.max <- max(sz.raw, na.rm = TRUE)
        sz.denom <- sz.max - sz.min
        sz.scaled <- if (sz.denom > 0) (sz.raw - sz.min)/sz.denom * sz.const
                     else              rep(sz.const, length(sz.raw))
        point.marker$sizemode = "Area"
        point.marker$size <- pmax(1, sz.scaled)
    }
    else
        stop(paste("Point setting not handled:", modifications))

    pp <- plot_ly(x = x.pts, y = y.pts, type = "scatter", mode = "markers",
        marker = point.marker, hoverinfo = "x+y", hoverlabel = point.hover.style,
        hovertemplate = paste0(x.name, ": %{x}<br>", y.name, ": %{y}<extra></extra>"),
        cliponaxis = FALSE)

    if (fit.type == "Straight")
    {
        reg <- lm(y ~ x, weights = weights)
        pp <- add_trace(pp, x = x, y = reg$fitted.values, type = "scatter", mode = "markers+lines",
                marker = list(color = fit.line.marker$color, opacity = 0),
                line = fit.line.marker, name = "Fitted",
                hovertemplate = paste0(x.name, ": %{x}<br>", y.name, ": %{y}"),
                hoverinfo = "all", hoverlabel = fit.hover.style)
    }
    else if (fit.type == "LOESS")
    {
        ok <- is.finite(x) & is.finite(y)
        if (any(ok))
        {
            xok <- x[ok]
            yok <- y[ok]
            j <- order(xok)
            suppressWarnings(fitted.y <- loess(yok ~ xok, weights = weights[ok])$fitted)
            pp <- add_trace(pp, x = xok[j], y = fitted.y[j], type = "scatter", mode = "markers+lines",
                marker = list(color = fit.line.marker$color, opacity = 0),
                line = fit.line.marker, name = "Fitted",
                hovertemplate = paste0(x.name, ": %{x}<br>", y.name, ": %{y}"),
                hoverinfo = "all", hoverlabel = fit.hover.style)
        }
    }
    pp <- layout(pp, xaxis = xaxis, yaxis = yaxis)
    return(pp)
}
