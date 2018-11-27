#' Parallel coordinates
#'
#' Create a parallel coordinates plot to show multivariate data
#' @param x A dataframe containing multiple variables (as columns)
#' @param group An optional variable which is used to color the lines in plot.
#'  It should be a vector with length equal to the number of rows in \code{x}.
#'  Can be numeric or categorical.
#' @param colors A vector of colors for the color in lines in plot. Note that only 6-digit
#'  hex codes are accepted (8-digit hex results in black lines). If no \code{group}
#'  is provided, then the first color will be used for all the lines. If it is provided,
#'  \code{colors} will be interpolated (linearly) to create a color scalebar.
#' @param opacity Opacity of the line colors as an alpha value (0 to 1).
#' @param global.font.family Character; font family for all occurrences of any
#' font attribute for the chart unless specified individually.
#' @param global.font.color Global font color as a named color in character format
#' (e.g. "black") or an a hex code.
#' @param font.unit One of "px" of "pt". By default all font sizes are specified in terms of
#'  pixels ("px"). But changing this to "pt" will mean that the font sizes will be in terms
#'  points ("pt"), which will be consistent with font sizes in text boxes.
#' @param label.font.color Label font color as a named color in character
#' format (e.g. "black") or a hex code.
#' @param label.font.family Character; label font family.
#' @param label.font.size Integer; Label font size.
#' @param label.rotate Boolean; whether to rotate the variable names on the chart. 
#' @param tick.font.color Tick label font color as a named color in character
#' format (e.g. "black") or a hex code.
#' @param tick.font.family Character; tick label font family.
#' @param tick.font.size Integer; Tick label font size.
#' @param margin.top Margin between plot area and the top of the
#' graphic in pixels
#' @param margin.bottom Margin between plot area and the bottom of the
#' graphic in pixels
#' @param margin.left Margin between plot area and the left of the
#' graphic in pixels
#' @param margin.right Margin between plot area and the right of the
#' graphic in pixels
#' @param auto.resize Whether the chart should resize automatically. It does not appear to work.
#' @param interactive Whether the lines should be hidden/shown depending on mouse.
#' @param queue Logical; whether lines should be added slowly to the plot
#' @param queue.rate Specifies the speed if \code{queue = TRUE}. This specifies the number
#'  of lines drawn per second.
#' @param width Numeric; Width of chart in pixels.
#' @param height Numeric; Height of chart in pixels.
#' @param ... Parameters which are ignored. This is included so calls expecting the
#'     plotly implementation which had more parameters will not cause errors 
#' @importFrom parcoords parcoords
#' @importFrom htmlwidgets JS
#' @importFrom jsonlite toJSON
#' @export
ParallelCoordinates <- function(x,
                                opacity = 0.4,
                                group = NULL,
                                colors = ChartColors(5, "Spectral"),
                                global.font.family = "Arial",
                                global.font.color = rgb(44, 44, 44, maxColorValue = 255),
                                label.font.family = global.font.family,
                                label.font.color = global.font.color,
                                label.font.size = 12,
                                label.rotate = FALSE,
                                tick.font.family = global.font.family,
                                tick.font.color = global.font.color,
                                tick.font.size = 10,
                                margin.top = 0,
                                margin.bottom = 0,
                                margin.left = 0,
                                margin.right = 0,
                                auto.resize = TRUE,
                                interactive = TRUE,
                                queue = FALSE,
                                queue.rate = NULL,
                                font.unit = "px",
                                width = NULL,
                                height = NULL,
                                ...)

{
    # For the other chart types, the font size conversion
    # happens inside flipChart::CChart but ParallelCoordinates is called separately.
    if (tolower(font.unit) %in% c("pt", "point", "points"))
    {
        fsc <- 1.3333
        label.font.size = round(fsc * label.font.size, 0)
        tick.font.size = round(fsc * tick.font.size, 0)
    }

    # Reduce the number of ticks for date variables
	tasks <- NULL
    dimlist <- list()
    for (i in 1:ncol(x))
    {
        tmp.name0 <- colnames(x)[i]
        if (any(class(x[[i]]) %in% c("Date", "POSIXct", "POSIXt")))
            x[,i] <- as.numeric(x[,i])
        tmp.name1 <- colnames(x)[i]
		dimlist[[tmp.name1]] <- list(title = tmp.name0)
		
        #if (any(class(x[[i]]) %in% c("Date", "POSIXct", "POSIXt")))
		#		tasks <- c(tasks, JS(orderDateTicks(tmp.name1, x[[i]])))
		if (is.factor(x[[i]]))
			tasks <- c(tasks, JS(orderCategoricalTicks(tmp.name1, levels(x[[i]]))))
    }

    # some JS function if group is a variable
    if (!is.null(group))
    {
        if (length(group) != nrow(x))
            stop("Length of color variable must be the same as x")

        if (is.factor(group) || is.character(group))
        {
            x <- data.frame(x, tmp.color = as.factor(group), check.names = FALSE)
            if (length(colors) <= 1)
                colorScale <- JS('d3.scale.category20()')           
            else
            { 
                colors <- paste0(colors, rep("", nlevels(x$tmp.color)))[1:nlevels(x$tmp.color)]
                colorScale = JS(sprintf('d3.scale.ordinal().range(%s).domain(%s)',
                    toJSON(colors), toJSON(levels(x$tmp.color))))
            }
        } else
        {
            x <- data.frame(x, tmp.color = as.numeric(group))
            if (length(colors) <= 1)
                colors <- c("blue", "red")
            seq.len <- length(colors)
            seq.val <- seq(from = min(x$tmp.color, na.rm = T), 
                           to = max(x$tmp.color, na.rm = T),
                           length = seq.len)
            colorScale = JS(sprintf(
            'd3.scale.linear().domain(%s).range(%s).interpolate(d3.interpolateRgb)',
            toJSON(seq.val), toJSON(colors)))
        }
        colors <- list(colorBy = "tmp.color", colorScale = colorScale)
        tasks <- c(tasks, JS(removeColVar()))

    } else
        colors <- colors[1]

    # Margins
    margin.bottom <- margin.bottom + tick.font.size
    margin.top <- margin.top + 2 * label.font.size
    if (label.rotate)
        margin.top <- margin.top + max(nchar(colnames(x))) * 0.5 * label.font.size

    # Apply formatting
    tasks <- c(tasks, JS(formatD3Text(tick.font.family,
                    tick.font.color, tick.font.size))) 
    tasks <- c(tasks, JS(formatD3Labels(label.font.family,
                    label.font.color, label.font.size, label.rotate)))
    tasks <- c(tasks, JS(removeD3CanvasMargin()))
    tasks <- c(tasks, JS(removeD3BodyPadding()))

    #lapply(tasks, cat)
    parcoords(x, alpha = opacity, dimensions = dimlist, tasks = tasks,
        rownames = FALSE, composite = "darken", color = colors,
        brushMode = if (interactive) "1D-axes-multi" else NULL,
        margin = list(top = margin.top, bottom = margin.bottom,
            left = margin.left, right = margin.right),
        width = width, height = height, reorderable = interactive,
        autoresize = auto.resize, queue = queue, rate = queue.rate)
}

setD3Margin <- function()
    return('
function(){
    d3.select("canvas") .attr("translate(0, -50)")
}
')


# Formatting is applied after initial rendering
# Tick labels are applied first because we can't select the exact text elements
# Copied from https://github.com/timelyportfolio/parcoords/issues/8

formatD3Text <- function(family, color, size)
    return(paste0('
function(){
      d3.select(this.el).selectAll("svg text")
        .style("font", "', size, 'px ', family, '") .style("fill", "', color, '")
    }
'))

formatD3Labels <- function(family, color, size, rotate)
    return(paste0('
function(){
      d3.select(this.el).selectAll(".dimension:nth-child(n+1) > .axis > text")
        .style("font", "', size, 'px ', family, '") .style("fill", "', color,
        '") .attr("transform", "translate(0,-', size, ')',
        if (rotate) ' rotate(-90)") .attr("text-anchor", "start' else '', '")
    }
'))

# Javascript function to remove variable used to create colorscale
# Copied from https://github.com/timelyportfolio/parcoords/issues/17
removeColVar <- function()
    return ("
function(){
  // supply an array of columns to hide
  this.parcoords.hideAxis(['names', 'tmp.color'])

  this.parcoords.removeAxes();
  this.parcoords.render();

  // duplicated from the widget js code
  //  to make sure reorderable and brushes work
  if( this.x.options.reorderable ) {
    this.parcoords.reorderable();
  } else {
    this.parcoords.createAxes();
  }

  if( this.x.options.brushMode ) {
  // reset the brush with None
    this.parcoords.brushMode('None')
    this.parcoords.brushMode(this.x.options.brushMode);
    this.parcoords.brushPredicate(this.x.options.brushPredicate);
  }
}
")

# The following two functions add javascript code to remove some margins and padding
# Not sure what they are doing or where they come from but
# inspecting objects in chrome show these are always present
removeD3CanvasMargin <- function()
    return ('
function(){
    d3.select("canvas") .attr("translate(0, -50)")
}
')

removeD3BodyPadding <- function()
    return('
function(){
    d3.select("body") .style("padding", "0px")
}
')

orderCategoricalTicks <- function(varname, varlevels)
	return(paste0("
function(){
	this.parcoords.dimensions()['", varname, "']
	.yscale = d3.scale.ordinal()
	.domain(['", paste(varlevels, collapse = "','"), "'])
	.rangePoints([
	1,
	this.parcoords.height()-this.parcoords.margin().top - this.parcoords.margin().bottom
])
}
"))


orderDateTicks <- function(varname, varlevels)
	return(paste0("
function(){
	this.parcoords.dimensions()['", varname, "']
	.yscale = d3.scale.time()
	.domain([new Date(2018,1,1), new Date(2019, 2, 1)])
	.rangePoints([
	1,
	this.parcoords.height()-this.parcoords.margin().top - this.parcoords.margin().bottom
])
}
"))
