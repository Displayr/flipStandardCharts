#' Sparkline charts
#'
#' Create small charts for displaying inline with text
#' @param x A numeric input vector to plot. Note that because sparklines is designed to be minimal 
#'  there is no support for vector names, and the x-axis of these charts is always the numeric index.
#' @param type The chart output type. One of 'Area', 'Line' (straight lines connecting points), 'Curve',
#'  'Column' or 'Box'.
#' @param fill.color Character; Color of chart
#' @param fill.opacity Opacity of \code{fill.color} as an alpha value (0 to 1).
#' @param line.color Character; Line color in Area, Line and Curve chart. For a line or curve chart, 
#'  setting either \code{fill.color} or \code{line.color} while retaining default values can be used.
#'  but if both arguments are used, then \code{fill.color} will be ignored in favour of \code{line.color}.
#' @param line.opacity Opacity of \code{line.color} as an alpha value (0 to 1).
#' @param line.thickness Thickness of line (in pixels) for Area, Line or Curve chart.
#' @param end.points.show Logical; whether to show markers at either end of the series. This only applies for
#   charts with \code{type} "Area", "Line", or "Curve".
#' @param end.points.symbol Character; marker symbol used at end points. See \url{https://plot.ly/r/reference/#scatter-marker-symbol} for options.
#' @param end.points.size Numeric; marker size in pixels.
#' @param end.points.color Character; color of end point markers.
#' @param end.points.opacity Opacity of \code{end.points.color} as an alpha value (0 to 1).
#' @param end.labels.position Character; Whether to put labels above, below or next to end points.
#' @param end.labels.decimals Numeric; The number of decimals to be shown in the end labels.
#' @param x.axis.show Logical; Whether to show the x-axis.
#' @param x.tick.show Logical; Whether to show ticklabels. This only has an effect is \code{x.tick.show} is true.
#' @param y.axis.show Logical; Whether to show the y-axis.
#' @param y.tick.show Logical; Whether to show ticklabels. This only has an effect is \code{y.tick.show} is true
#' @param background.fill.color Background color in character format (e.g. "black") or a hex code.
#' @param background.fill.opacity Background opacity as an alpha value (0 to 1).
#' @param margin.top Margin between plot area and the top of the graphic in pixels
#' @param margin.bottom Margin between plot area and the bottom of the graphic in pixels
#' @param margin.left Margin between plot area and the left of the graphic in pixels
#' @param margin.right Margin between plot area and the right of the graphic in pixels
#' @examples
#' xx <- rnorm(10)
#' Sparkline(xx, background.fill.color = "black", background.fill.opacity = 1)
#' Sparkline(xx, type = "Curve", end.points.show = TRUE, end.labels.position = "Next", 
#'      end.labels.decimals = 5)
#' Sparkline(xx, type = "Line", x.axis.show = TRUE, y.axis.show = TRUE, 
#'      x.tick.show = FALSE, y.tick.show = FALSE)
#' @importFrom plotly plot_ly config layout add_trace
#' @export
Sparkline <- function(x, 
        type = c("Area", "Line", "Curve", "Column", "Box")[1],
        fill.color = "red",
        fill.opacity = 0.5,
        line.thickness = 3,
        line.color = fill.color,
        line.opacity = 1,
        end.points.show = FALSE,
        end.points.symbol = "circle",
        end.points.size = 20,
        end.points.color = fill.color,
        end.points.opacity = 1,
        end.labels.position = c("None", "Above", "Below", "Next")[1],
        end.labels.decimals = 2,
        background.fill.color = rgb(255, 255, 255, maxColorValue = 255),
        background.fill.opacity = 0,
        x.axis.show = FALSE,
        x.tick.show = x.axis.show,
        y.axis.show = FALSE,
        y.tick.show = y.axis.show,
        margin.left = 0,
        margin.right = 0,
        margin.top = 0,
        margin.bottom = 0)
{
    if (type == "Box")
        return(Box(x, background.fill.color = background.fill.color, 
        background.fill.opacity = background.fill.opacity, vertical = FALSE,
        margin.bottom = margin.bottom, margin.top = margin.top, 
        margin.left = margin.left, margin.right = margin.right))

    if (is.null(line.color))
    {
        line.color <- fill.color
        line.opacity <- fill.opacity
    }

    n <- length(x)
    x0 <- 1:n
    annot <- NULL
    p <- plot_ly()    

    if (type == "Area")
        p <- add_trace(p, x = x0, y = x, fill = "tozeroy", type = "scatter", mode = "lines",
                fillcolor = toRGB(fill.color, alpha = fill.opacity),
                hoverinfo = "y",
                line = list(width = line.thickness, color = toRGB(line.color, alpha = line.opacity)))
    else if (type %in% c("Line", "Curve"))
        p <- add_trace(p, x = x0, y = x, type = "scatter", mode = "lines",
                hoverinfo = "y",
                line = list(width = line.thickness, 
                            shape = if (type == "Curve") "spline" else "linear",
                            color = toRGB(line.color, alpha = line.opacity)))
    else if (type == "Column")
        p <- add_trace(p, x = x0, y = x, type = "bar", orientation = "v",
                hoverinfo = "y",
                marker = list(color = toRGB(fill.color, alpha = fill.opacity)))
    else
        stop("Unknown chart type. Please set type to one of 'Area', 'Line', 'Curve', 'Column', 'Bar'.")

    if (type %in% c("Line", "Curve", "Area"))
    {
        if (end.points.show)
            p <- add_trace(p, x = x0[c(1,n)], y = x[c(1,n)], type = "scatter", mode = "markers",
                marker = list(color = end.points.color, opacity = end.points.opacity,
                symbol = end.points.symbol, size = end.points.size), 
                hoverinfo = "skip", cliponaxis = FALSE)
        else
            end.points.size <- 1

        if (end.labels.position != "None")
            annot <- list(setLabel(x[1], x0[1], decimals = end.labels.decimals, 
                shift = end.points.size * 0.75, position = end.labels.position, index = 1),
                setLabel(x[n], x0[n], decimals = end.labels.decimals, 
                shift = end.points.size * 0.75, position = end.labels.position, index = n))
    }
            

    xaxis <- list(side = "bottom", showgrid = FALSE, showline = x.axis.show, zeroline = FALSE,
                showticklabels = x.axis.show, ticks = if (x.axis.show) "outside" else "",
                tickfont = list(size = if (x.tick.show) 10 else 1,
                                color = if (x.tick.show) "red" else "blue"),
                linewidth = 1, linecolor = "red")
    yaxis <- list(side = "left", showgrid = FALSE, showline = y.axis.show, zeroline = FALSE,
                showticklabels = y.axis.show, ticks = if (y.axis.show) "outside" else "",
                tickfont = list(size = if (x.tick.show) 10 else 1),
                linewidth = 1, linecolor = "red")

    p <- config(p, displayModeBar = FALSE)
    p$sizingPolicy$browser$padding <- 0
    p <- layout(p,
        xaxis = xaxis, yaxis = yaxis,
        annotations = annot,
        hovermode = "x", showlegend = FALSE,
        margin = list(t = margin.top, b = margin.bottom, l = margin.left, r = margin.right, 
                autoexpand = x.axis.show | y.axis.show),
        plot_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity),
        paper_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity))
    result <- list(htmlwidget = p)
    class(result) <- "StandardChart"
    result
}


setLabel <- function(y, xpos, decimals = 2, shift = 0, position = "Above", index = 1)
{
    if (grepl("^Above", position))
    {
        xanchor = "center"
        yanchor = "bottom"
        yshift = shift
        xshift = 0
    } else if (grepl("^Below", position))
    {
        xanchor = "center"
        yanchor = "top"
        yshift = -shift
        xshift = 0
    } else
    {
        yanchor = "middle"
        yshift = 0
        xanchor = if (index == 1) "right" else "left"
        xshift = if (index == 1) -shift else shift
    }
    return(list(x = xpos, y = y, text = round(y, decimals),
            showarrow = FALSE, xshift = xshift, yshift = yshift,
            xref = "x", yref = "y", yanchor = yanchor, xanchor = xanchor))
}

