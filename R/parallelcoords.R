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
#'  \code{colors} will be interpolated (linearly) to create a color scalebar
#' @param scale.show Logical; whether to show the color scalebar
#' @param scale.reverse Logical; whether to reverse the color scalebar
#' @param scale.nticks; Integer; number of ticks to show on colorscalebar
#' @param global.font.family Character; font family for all occurrences of any
#' font attribute for the chart unless specified individually.
#' @param global.font.color Global font color as a named color in character format
#' (e.g. "black") or an a hex code.
#' @param font.unit One of "px" of "pt". By default all font sizes are specified in terms of
#'  pixels ("px"). But changing this to "pt" will mean that the font sizes will be in terms
#'  points ("pt"), which will be consistent with font sizes in text boxes.
#' @param range.decimals The number of decimals to shown in the range labels (for numeric
#'  variables) and the tick labels.
#' @param label.font.color Label font color as a named color in character
#' format (e.g. "black") or a hex code.
#' @param label.font.family Character; label font family.
#' @param label.font.size Integer; Label font size.
#' @param range.font.color Range label font color as a named color in character
#' format (e.g. "black") or a hex code.
#' @param range.font.family Character; range label font family.
#' @param range.font.size Integer; Range label font size.
#' @param tick.font.color Tick label font color as a named color in character
#' format (e.g. "black") or a hex code.
#' @param tick.font.family Character; tick label font family.
#' @param tick.font.size Integer; Tick label font size.
#' @param background.fill.color Background color in character format (e.g. "black") or a hex code.
#' @param background.fill.opacity Background opacity as an alpha value (0 to 1).
#' @param charting.area.fill.color Charting area background color as
#' a named color in character format (e.g. "black") or a hex code.
#' @param charting.area.fill.opacity Charting area background opacity as an alpha value (0 to 1).

#' @param margin.top Margin between plot area and the top of the
#' graphic in pixels
#' @param margin.bottom Margin between plot area and the bottom of the
#' graphic in pixels
#' @param margin.left Margin between plot area and the left of the
#' graphic in pixels
#' @param margin.right Margin between plot area and the right of the
#' graphic in pixels
#' @importFrom plotly plot_ly layout
#' @export
ParallelCoordinates <- function(x,
                                group = NULL,
                                colors = ChartColors(5, "Spectral"),
                                scale.show = TRUE,
                                scale.reverse = TRUE,
                                scale.nticks = 10,
                                global.font.family = "Arial",
                                global.font.color = rgb(44, 44, 44, maxColorValue = 255),
                                label.font.family = global.font.family,
                                label.font.color = global.font.color,
                                label.font.size = 12,
                                range.font.family = global.font.family,
                                range.font.color = global.font.color,
                                range.font.size = 10,
                                range.decimals = 0,
                                tick.font.family = global.font.family,
                                tick.font.color = global.font.color,
                                tick.font.size = 10,
                                background.fill.color = rgb(255, 255, 255, maxColorValue = 255),
                                background.fill.opacity = 0,
                                charting.area.fill.color = rgb(255, 255, 255, maxColorValue = 255),
                                charting.area.fill.opacity = 0,
                                margin.top = 40,
                                margin.bottom = 40,
                                margin.left = 50,
                                margin.right = 40,
                                font.unit = "px")


{
    # For the other chart types, the font size conversion
    # happens inside flipChart::CChart but ParallelCoordinates is called separately.
    if (tolower(font.unit) %in% c("pt", "point", "points"))
    {
        fsc <- 1.3333
        label.font.size = round(fsc * label.font.size, 0)
        range.font.size = round(fsc * range.font.size, 0)
        tick.font.size = round(fsc * tick.font.size, 0)
    }

    # Set up data for plotting
    dimlist <- list()
    for (i in 1:ncol(x))
    {
        if (is.numeric(x[[i]]))
        {
            rr <- range(x[[i]], na.rm = TRUE) * 10^{range.decimals}
            rr <- c(floor(rr[1]), ceiling(rr[2])) / 10^{range.decimals}
            dimlist[[i]] <- list(label = colnames(x)[i], values = as.numeric(x[[i]]), 
                tickformat = paste0(".", range.decimals, "f"), range = rr)
        
        } else
        {
            ff <- as.factor(x[[i]])
            dimlist[[i]] <- list(label = colnames(x)[i], values = as.numeric(ff),
                 tickvals = 1:nlevels(ff), ticktext = levels(ff))
        }
    }

    # Set colors
    eval(colors)
    if (length(group) == 0)
    {
        group <- colors[1]
        col.scale <- NULL
    
    } else if (length(group) != nrow(x))
    {
        stop("Variable 'group' should have the same number of values as the number of observations in 'x'")
    } 
    else
    {
        colorbar = list(outlinewidth = 0)
        if (!is.null(group) && !is.numeric(group))
        {
            ff <- as.factor(group)
            colorbar = list(outlinewidth = 0, tickmode = "array", ticktext = levels(ff),
                    tickvals = 1:nlevels(ff))
            group <- as.numeric(ff)
        } 
        cc.vals <- seq(from = 0, to = 1, length = scale.nticks)
        col.fun <- colorRamp(unique(colors))  # undo recycling in PrepareColors
        cc.cols <- rgb(col.fun(cc.vals), maxColorValue = 255) # hex values of opaque colors
        col.scale <- mapply(function(a,b)c(a,b), a = cc.vals, b = cc.cols, SIMPLIFY = FALSE)
    }

    p <- x %>% 
        plot_ly(type = 'parcoords', 
        line = list(color = group, colorscale = col.scale,
            showscale = scale.show, reversescale = scale.reverse, colorbar = colorbar),
            labelfont = list(family = label.font.family, color = label.font.color, size = label.font.size),
            rangefont = list(family = range.font.family, color = range.font.color, size = range.font.size),
            tickfont = list(family = tick.font.family, color = tick.font.color, size = tick.font.size),
        dimensions = dimlist)
    
    p <- config(p, displayModeBar = FALSE)
    p$sizingPolicy$browser$padding <- 0
    p <- layout(p,
        margin = list(t = margin.top, b = margin.bottom, l = margin.left, r = margin.right),
        plot_bgcolor = toRGB(charting.area.fill.color, alpha = charting.area.fill.opacity),
        paper_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity))

    result <- list(htmlwidget = p)
    class(result) <- "StandardChart"
    result

}
