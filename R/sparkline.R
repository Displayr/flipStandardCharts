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
#' @param end.points.show Logical; whether to show markers at either end of the series.
#' 	This only applies for charts with \code{type} "Area", "Line", or "Curve".
#' @param end.points.symbol Character; marker symbol used at end points. See \url{https://plot.ly/r/reference/#scatter-marker-symbol} for options.
#' @param end.points.size Numeric; marker size in pixels.
#' @param end.points.color Character; color of end point markers.
#' @param end.points.opacity Opacity of \code{end.points.color} as an alpha value (0 to 1).
#' @param end.labels.position Character; Whether to put labels above, below or next to end points.
#' @param x.axis.show Logical; Whether to show the x-axis.
#' @param x.axis.color Character; Color of the x-axis line.
#' @param x.axis.width Integer; Width of the x-axis line in pixels.
#' @param x.tick.show Logical; Whether to show ticklabels. This only has an effect is \code{x.axis.show} is true.
#' @param y.axis.show Logical; Whether to show the y-axis.
#' @param y.tick.show Logical; Whether to show ticklabels. This only has an effect is \code{y.tick.show} is true.
#' @param y.axis.color Character; Color of the y-axis line.
#' @param y.axis.width Integer; Width of the y-axis line in pixels.
#' @param end.labels.format A string representing a d3 formatting code.
#' 	See https://github.com/d3/d3/blob/master/API.md#number-formats-d3-format
#' @param end.labels.prefix Optional text to prepend to end labels.
#' @param end.labels.suffix Optional text to append to end labels.
#' @param end.labels.font.color label font color as a named color
#' in character format (e.g. "black") or an a hex code.
#' @param end.labels.font.family Character; label font family
#' @param end.labels.font.size Integer; label font size
#' @param hover.bg.color Color of the background text box showing the hover text.
#' @param hover.format A string representing a d3 formatting code.
#' See https://github.com/d3/d3/blob/master/API.md#number-formats-d3-format
#' @param hover.font.color hover text font color as a named color
#' in character format (e.g. "black") or an a hex code.
#' @param hover.font.family Character; hover text font family
#' @param hover.font.size Integer; hover text font size.
#' @param y.tick.format A string representing a d3 formatting code.
#' See https://github.com/d3/d3/blob/master/API.md#number-formats-d3-format
#' @param y.tick.font.color y-axis tick label font color as a named color
#' in character format (e.g. "black") or an a hex code.
#' @param y.tick.length Numeric; length of tick marks if \code{y.tick.show}.
#' @param y.tick.color Color of tick marks on y-axis
#' @param y.tick.font.family Character; y-axis tick label font family
#' @param y.tick.font.size Integer; y-axis tick label font size
#' @param x.tick.format A string representing a d3 formatting code.
#' See https://github.com/d3/d3/blob/master/API.md#number-formats-d3-format
#' @param x.tick.font.color x-axis tick label font color as a named color
#' in character format (e.g. "black") or an a hex code.
#' @param x.tick.font.family Character; x-axis tick label font family
#' @param x.tick.font.size Integer; x-axis tick label font size
#' @param x.tick.length Numeric; length of tick marks if \code{x.axis.show}.
#' @param x.tick.color Color of tick marks on x-axis.
#' @param background.fill.color Background color in character format (e.g. "black") or a hex code.
#' @param background.fill.opacity Background opacity as an alpha value (0 to 1).
#' @param margin.top Margin between plot area and the top of the graphic in pixels
#' @param margin.bottom Margin between plot area and the bottom of the graphic in pixels
#' @param margin.left Margin between plot area and the left of the graphic in pixels
#' @param margin.right Margin between plot area and the right of the graphic in pixels
#' @param global.font.family Character; font family for all occurrences of any
#' font attribute for the chart unless specified individually.
#' @param global.font.color Global font color as a named color in character format
#' (e.g. "black") or an a hex code.
#' @param font.unit Set to 'pt' (default) to get font sizing consistent with textboxes.
#' Otherwise fonts will be taken to be specified in pixels.
#' @importFrom plotly plot_ly config layout add_trace
#' @examples
#' xx <- rnorm(10)
#' Sparkline(xx, background.fill.color = "black", background.fill.opacity = 1)
#' Sparkline(xx, type = "Curve", end.points.show = TRUE, end.labels.position = "Next",
#'      end.labels.format = ".1%")
#' Sparkline(xx, type = "Line", x.axis.show = TRUE, y.axis.show = TRUE,
#'      x.tick.show = FALSE, y.tick.show = FALSE)
#' @export
Sparkline <- function(x,
        type = c("Area", "Line", "Curve", "Column", "Box")[1],
        fill.color = "red",
        fill.opacity = 0.5,
        line.thickness = 3,
        line.color = fill.color,
        line.opacity = 1,
		global.font.family = "Arial",
		global.font.color = rgb(44, 44, 44, maxColorValue = 255),
		font.unit = "px",
        end.points.show = FALSE,
        end.points.symbol = "circle",
        end.points.size = 20,
        end.points.color = fill.color,
        end.points.opacity = 1,
        end.labels.position = c("None", "Above", "Below", "Next")[1],
		end.labels.font.family = global.font.family,
		end.labels.font.color = global.font.color,
		end.labels.font.size = 10,
		end.labels.format = "",
		end.labels.prefix = "",
		end.labels.suffix = "",
		hover.bg.color = rgb(0.5,0.5,0.5),
		hover.font.family = global.font.family,
		hover.font.color = rgb(1,1,1),
		hover.font.size = 11,
		hover.format = "",
        background.fill.color = "transparent",
        background.fill.opacity = 1,
        x.axis.show = FALSE,
		x.axis.color = rgb(44, 44, 44, maxColorValue = 255),
		x.axis.width = 1,
        x.tick.show = x.axis.show,
        x.tick.length = 3,
        x.tick.color = x.axis.color,
		x.tick.font.family = global.font.family,
		x.tick.font.color = global.font.color,
		x.tick.font.size = 10,
		x.tick.format = "",
        y.axis.show = FALSE,
		y.axis.color = rgb(44, 44, 44, maxColorValue = 255),
		y.axis.width = 1,
        y.tick.show = y.axis.show,
        y.tick.length = 3,
        y.tick.color = y.axis.color,
		y.tick.font.family = global.font.family,
		y.tick.font.color = global.font.color,
		y.tick.font.size = 10,
		y.tick.format = "",
        margin.left = 0,
        margin.right = 0,
        margin.top = 0,
        margin.bottom = 0)
{
    ErrorIfNotEnoughData(x, require.tidy = type != "Box")
    data.is.percent <- isTRUE(grepl("%)?$", attr(x, "statistic")))
    if (type == "Box")
    {
        if (length(x) == 1 && is.list(x) && NCOL(x[[1]]) > 1)
            x <- x[[1]]
        if (is.matrix(x))
            x <- as.data.frame(x)
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
            stop("Input data should be a list of numeric vectors or a matrix.")
        if (length(x) > 1)
        {
            warning("Sparkline charts only show a single series ",
               "(", names(x)[1], "). ",
               "Use COLUMN MANIPULATIONS to choose a different data series.")
            x <- x[[1]]
        }
    } else
    {
        x <- checkMatrixNames(x)
        tmp.name <- "Column 1"
        if (!is.null(colnames(x)))
            tmp.name <- colnames(x)[1]
        if (is.list(x) && !is.null(attr(x[[1]], "label")))
            tmp.name <- attr(x[[1]], "label")
        if (NCOL(x) > 1)
        {
            warning("Sparkline charts only show a single series ",
               "(", tmp.name, "). ",
               "Use COLUMN MANIPULATIONS to choose a different data series.")
            x <- x[,1,drop = FALSE]
        }
    }

    if (is.null(line.color))
    {
        line.color <- fill.color
        line.opacity <- fill.opacity
    }
	if (!any(nzchar(y.tick.format)) || grepl("[0-9]$", y.tick.format))
		y.tick.format <- if (data.is.percent) paste0(y.tick.format, "%") else paste0(y.tick.format, "f")
	if (!any(nzchar(hover.format)) || grepl("[0-9]$", hover.format))
		hover.format <- if (data.is.percent) paste0(hover.format, "%") else paste0(hover.format, "f")

    if (tolower(font.unit) %in% c("pt", "point", "points"))
    {
        fsc <- 1.3333
        end.labels.font.size = round(fsc * end.labels.font.size, 0)
        x.tick.font.size = round(fsc * x.tick.font.size, 0)
        y.tick.font.size = round(fsc * y.tick.font.size, 0)
        hover.font.size = round(fsc * hover.font.size, 0)
    }

	if (!x.axis.show)
		x.axis.width = 0
    if (type == "Box")
    {
        res <- Box(x, hover.on = "points",
            values.title = " ", density.color = fill.color,
    		values.tick.show = x.tick.show, values.tick.format = x.tick.format,
    		values.tick.font.color = x.tick.font.color, values.tick.font.size = x.tick.font.size,
    		values.line.width = x.axis.width, values.line.color = x.axis.color,
    		values.tick.font.family = x.tick.font.family, values.tick.mark.length = x.tick.length,
    		background.fill.color = background.fill.color,
            background.fill.opacity = background.fill.opacity, vertical = FALSE,
            margin.bottom = margin.bottom, margin.top = margin.top,
            margin.left = margin.left, margin.right = margin.right)
        class(res) <- c(class(res), "visualization-selector")
        return(res)
    }

    n <- length(x)
    annot <- NULL

    axisFormat <- formatLabels(x, type = if (type == "Curve") "Line" else type,
                        label.wrap = FALSE, label.wrap.nchar = NULL,
                        x.format = x.tick.format, y.format = y.tick.format)
    x.labels <- axisFormat$labels
    x <- as.numeric(x)
    xaxis <- list(side = "bottom", type = axisFormat$x.axis.type, categoryorder = "trace",
                showgrid = FALSE, showline = x.axis.show, zeroline = FALSE, automargin = type != "Box",
                showticklabels = x.axis.show, ticklabelposition = "outside",
                tickfont = list(size = if (x.tick.show) x.tick.font.size else 1,
						   		color = if (x.tick.show) x.tick.font.color else "transparent",
				family = x.tick.font.family), tickformat = x.tick.format,
                ticklen = if (x.tick.show) x.tick.length else 0,
                linewidth = x.axis.width, linecolor = x.axis.color, tickcolor = x.tick.color)
    yaxis <- list(side = "left", showgrid = FALSE, showline = y.axis.show, zeroline = FALSE,
                automargin = type != "Box",
                showticklabels = y.axis.show, ticklabel.position = "outside",
                tickfont = list(size = if (y.tick.show) y.tick.font.size else 1,
							 	color = if (y.tick.show) y.tick.font.color else "transparent",
				family = y.tick.font.family), ticklen = if (y.tick.show) y.tick.length else 0,
				hoverformat = hover.format, tickformat = y.tick.format,
                linewidth = y.axis.width, linecolor = y.axis.color, tickcolor = y.tick.color)

    p <- plot_ly()
    if (type == "Area")
        p <- add_trace(p, x = x.labels, y = x, fill = "tozeroy", type = "scatter", mode = "lines",
                fillcolor = toRGB(fill.color, alpha = fill.opacity),
                hoverinfo = "x+y",
                line = list(width = line.thickness, color = toRGB(line.color, alpha = line.opacity)))
    else if (type %in% c("Line", "Curve"))
        p <- add_trace(p, x = x.labels, y = x, type = "scatter", mode = "lines",
                hoverinfo = "x+y",
                line = list(width = line.thickness,
                            shape = if (type == "Curve") "spline" else "linear",
                            color = toRGB(line.color, alpha = line.opacity)))
    else if (type == "Column")
        p <- add_trace(p, x = x.labels, y = x, type = "bar", orientation = "v",
                hoverinfo = "x+y",
                marker = list(color = toRGB(fill.color, alpha = fill.opacity)))
    else
        stop("Unknown chart type. Please set type to one of 'Area', 'Line', 'Curve', 'Column', 'Bar'.")

    if (type %in% c("Line", "Curve", "Area"))
    {
        if (end.points.show)
            p <- add_trace(p, x = x.labels[c(1,n)], y = x[c(1,n)], type = "scatter", mode = "markers",
                marker = list(color = end.points.color, opacity = end.points.opacity,
                symbol = end.points.symbol, size = end.points.size),
                hoverinfo = "skip", cliponaxis = FALSE)
        else
            end.points.size <- 1

        if (end.labels.position != "None")
		{
			end.lab.font = list(family = end.labels.font.family, color = end.labels.font.color,
				size = end.labels.font.size)
            annot <- list(setLabel(x[1], x.labels[1],
				text = formatByD3(x[1], end.labels.format, end.labels.prefix,
				end.labels.suffix, data.is.percent),
                shift = end.points.size * 0.75, position = end.labels.position,
				font = end.lab.font, index = 1),
                setLabel(x[n], x.labels[n],
				text = formatByD3(x[n], end.labels.format, end.labels.prefix,
				end.labels.suffix, data.is.percent),
                shift = end.points.size * 0.75, position = end.labels.position,
				font = end.lab.font, index = n))
		}
    }


    p <- config(p, displayModeBar = FALSE, showAxisDragHandles = FALSE)
    p$sizingPolicy$browser$padding <- 0
    p <- layout(p,
        xaxis = xaxis, yaxis = yaxis,
        annotations = annot,
        hovermode = "closest", showlegend = FALSE,
        margin = list(t = margin.top, b = margin.bottom, l = margin.left, r = margin.right,
          	autoexpand = x.axis.show | y.axis.show),
		hoverlabel = list(bgcolor = hover.bg.color, bordercolor = hover.bg.color,
			font = list(color = hover.font.color, size = hover.font.size,
			family = hover.font.family)),
        plot_bgcolor = "transparent",
        paper_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity))
    attr(p, "can-run-in-root-dom") <- TRUE
    result <- list(htmlwidget = p)
    class(result) <- c("StandardChart", "visualization-selector")
    result
}


setLabel <- function(y, xpos, text, shift = 0, position = "Above", font, index = 1)
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
    return(list(x = xpos, y = y, text = text, font = font,
            showarrow = FALSE, xshift = xshift, yshift = yshift,
            xref = "x", yref = "y", yanchor = yanchor, xanchor = xanchor))
}

