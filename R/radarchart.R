#' Radar
#'
#' Radar chart, also known as web chart, spider chart, star chart, star plot, cobweb chart, irregular polygon, polar chart, or Kiviat diagram
#'
#' @param x Input data in the form of a vector or matrix. The categories used
#' to create the radar (i.e. the x-axis) is taken from the names/rownames of x.
#' @param title Character; chart title.
#' @param title.font.family Character; title font family. Can be "Arial Black",
#' "Arial", "Comic Sans MS", "Courier New", "Georgia", "Impact",
#' "Lucida Console", "Lucida Sans Unicode", "Marlett", "Symbol", "Tahoma",
#' "Times New Roman", "Trebuchet MS", "Verdana", "Webdings"
#' @param title.font.color Title font color as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param title.font.size Title font size; default = 10.
#' @param subtitle Character
#' @param subtitle.font.color subtitle font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param subtitle.font.family Character; subtitle font family
#' @param subtitle.font.size subtitle font size
#' @param footer Character
#' @param footer.font.color footer font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param footer.font.family Character; footer font family
#' @param footer.font.size footer font size
#' @param footer.wrap Logical; whether the footer text should be wrapped.
#' @param footer.wrap.nchar Number of characters (approximately) in each line of the footer when \code{footer.wordwrap} \code{TRUE}.
#' @param opacity Opacity of area fill colors as an alpha value (0 to 1).
#' @param colors Character; a vector containing one or more named
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
#' @param grid.show Logical; Whether to show grid lines.
#' opacity as an alpha value (0 to 1).
#' @param legend.show Logical; show the legend.
#' @param legend.position.x A numeric controlling the position of the legend.
#'   Values range from -0.5 (left) to 1.5 (right).
#' @param legend.position.y A numeric controlling the position of the legend.
#'   Values range from 0 (bottom) to 1 (top).
#' @param legend.fill.color Legend fill color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param legend.fill.opacity Legend fill opacity as an alpha value
#' (0 to 1).
#' @param legend.border.color Legend border color as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param legend.border.line.width Width in pixels of the border
#' around the legend.  0 = no border.
#' @param legend.font.color Legend font color as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param legend.font.family Character; legend font family.
#' @param legend.font.size Legend font size.
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
#' @param y.bounds.maximum Maximum of range for
#' plotting; NULL = no manual range set.  The minimum is always 0.
#' @param y.tick.distance Tick mark distance.
#' @param y.grid.width Width of y-grid lines in pixels; 0 = no line
#' @param y.grid.color Color of y-grid lines as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param y.tick.show Whether to display the y-axis tick labels
#' (i.e. radial distance from center)
#' @param y.tick.suffix y-axis tick label suffix
#' @param y.tick.prefix y-axis tick label prefix
#' @param y.tick.format A string representing a d3 formatting code.
#' See https://github.com/mbostock/d3/wiki/Formatting#numbers
#' @param y.hovertext.format A string representing a d3 formatting code.
#' See https://github.com/mbostock/d3/wiki/Formatting#numbers
#' @param y.tick.font.color y-axis tick label font color as a named color
#' in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param y.tick.font.family Character; y-axis tick label font family
#' @param y.tick.font.size y-axis tick label font size
#' @param x.tick.show  Whether to display the x-axis tick labels
#' (i.e. labels around the sides of the radar chart)
#' @param x.tick.font.color x-axis title font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param x.tick.font.family Character; x-axis title font family
#' @param x.tick.font.size x-axis title font size
#' @param x.grid.width Width of y-grid lines in pixels; 0 = no line
#' @param x.grid.color Color of y-grid lines as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param x.tick.label.wrap Logical; whether to wrap long labels on the x-axis.
#' @param x.tick.label.wrap.nchar Integer; number of characters in each line when \code{label.wrap} is \code{TRUE}.
#' @param series.line.width Width of outline of radar polygons.
#' @param tooltip.show Logical; whether to show a tooltip on hover.
#' @param modebar.show Logical; whether to show the zoom menu buttons or not.
#' @param global.font.family Character; font family for all occurrences of any
#' font attribute for the chart unless specified individually.
#' @param global.font.color Global font color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. #' rgb(0, 0, 0, maxColorValue = 255)).
#' @param data.label.show Logical; whether to show data labels.
#' @param data.label.font.family Character; font family for data label.
#' @param data.label.font.size Font size for data label.
#' @param data.label.font.color Font color as a named color
#' in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param data.label.format A string representing a d3 formatting code.
#' See https://github.com/mbostock/d3/wiki/Formatting#numbers
#' @param data.label.prefix Character; prefix for data values.
#' @param data.label.suffix Character; suffix for data values.
#' @param ... Extra arguments that are ignored.
#' @importFrom grDevices rgb
#' @importFrom flipChartBasics ChartColors
#' @importFrom plotly plot_ly layout config add_annotations
#' @importFrom flipFormat FormatAsReal
#' @export
Radar <- function(x,
                    title = "",
                    title.font.family = global.font.family,
                    title.font.color = global.font.color,
                    title.font.size = 16,
                    colors = ChartColors(max(1, ncol(x), na.rm = TRUE)),
                    opacity = 0.4,
                    background.fill.color = rgb(255, 255, 255, maxColorValue = 255),
                    background.fill.opacity = 0,
                    charting.area.fill.color = background.fill.color,
                    charting.area.fill.opacity = 0,
                    legend.show = TRUE,
                    legend.fill.color = background.fill.color,
                    legend.fill.opacity = 0,
                    legend.border.color = rgb(44, 44, 44, maxColorValue = 255),
                    legend.border.line.width = 0,
                    legend.font.color = global.font.color,
                    legend.font.family = global.font.family,
                    legend.font.size = 10,
                    legend.ascending = NA,
                    legend.position.y = 1,
                    legend.position.x = 1.02,
                    margin.top = NULL,
                    margin.bottom = NULL,
                    margin.left = NULL,
                    margin.right = NULL,
                    margin.inner.pad = NULL,
                    series.line.width = 3,
                    tooltip.show = TRUE,
                    modebar.show = FALSE,
                    global.font.family = "Arial",
                    global.font.color = rgb(44, 44, 44, maxColorValue = 255),
                    grid.show = TRUE,
                    x.tick.show = TRUE,
                    x.tick.font.color = global.font.color,
                    x.tick.font.family = global.font.family,
                    x.tick.font.size = 12,
                    x.grid.width = 1 * grid.show,
                    x.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                    y.bounds.maximum = NULL,
                    y.tick.distance = NULL,
                    y.grid.width = 1 * grid.show,
                    y.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                    y.tick.show = TRUE,
                    y.tick.suffix = "",
                    y.tick.prefix = "",
                    y.tick.format = "",
                    y.hovertext.format = "",
                    y.tick.font.color = global.font.color,
                    y.tick.font.family = global.font.family,
                    y.tick.font.size = 10,
                    x.tick.label.wrap = TRUE,
                    x.tick.label.wrap.nchar = 21,
                    data.label.show = FALSE,
                    data.label.font.family = global.font.family,
                    data.label.font.size = 10,
                    data.label.font.color = global.font.color,
                    data.label.format = "",
                    data.label.prefix = "",
                    data.label.suffix = "",
                    subtitle = "",
                    subtitle.font.family = global.font.family,
                    subtitle.font.color = global.font.color,
                    subtitle.font.size = 12,
                    footer = "",
                    footer.font.family = global.font.family,
                    footer.font.color = global.font.color,
                    footer.font.size = 8,
                    footer.wrap = TRUE,
                    footer.wrap.nchar = 100)
{
    # Check data
    ErrorIfNotEnoughData(x)
    chart.matrix <- checkMatrixNames(x)
    if (any(!is.finite(chart.matrix)))
        stop("Radar charts cannot contain missing or non-finite values.\n")
    if (any(chart.matrix < 0))
        stop("Radar charts cannot have negative values.\n")
    n <- nrow(chart.matrix)
    m <- ncol(chart.matrix)

    if (is.null(n) || n == 1 || m == 1)
    {
        # only 1 series
        chart.matrix <- data.frame(x=chart.matrix)
        n <- nrow(chart.matrix)
        m <- ncol(chart.matrix)
        legend.show <- FALSE
    }

    if (n <= 2)
    {
        warning("Radar chart only has two or less spokes. ",
                "It may be more appropriate to use another chart type.")
    }

    title.font = list(family = title.font.family, size = title.font.size, color = title.font.color)
    subtitle.font = list(family = subtitle.font.family, size = subtitle.font.size, color = subtitle.font.color)
    x.tick.font = list(family = x.tick.font.family, size = x.tick.font.size, color = x.tick.font.color)
    y.tick.font = list(family = y.tick.font.family, size = y.tick.font.size, color = y.tick.font.color)
    footer.font = list(family = footer.font.family, size = footer.font.size, color = footer.font.color)
    legend.font = list(family = legend.font.family, size = legend.font.size, color = legend.font.color)
    legend <- setLegend("Radar", legend.font, legend.ascending, legend.fill.color, legend.fill.opacity,
                        legend.border.color, legend.border.line.width, legend.position.x, legend.position.y)

    # Figure out positions of y-ticks (i.e. radial axis)
    tick.vals <- NULL
    if (is.null(y.bounds.maximum))
    {
        offset <- 1.0
        if (data.label.show)
            offset <- 1.1 + data.label.font.size/100
        y.bounds.maximum <- offset * max(chart.matrix)
    }
    if (is.null(y.tick.distance))
    {
        base <- 10^round(log10(y.bounds.maximum) - 1)
        mult <- max(1, floor((y.bounds.maximum/base)/5))
        y.tick.distance <- base * mult
    }
    tick.vals <- seq(from = 0, to = y.bounds.maximum, by = y.tick.distance)
    r.max <- y.bounds.maximum

    # Extract formatting from d3
    stat <- attr(x, "statistic")
    #if (!is.null(stat) && grepl("%", stat, fixed = TRUE))
    #{
    #    if (hover.format.function == "") hover.format.function <- ".0%"
    #    if (tick.format.function == "") tick.format.function <- ".0%"
    #    if (data.label.format.function == "") data.label.format.function <- ".0%"
    #}
    hover.format.function <- ifelse(percentFromD3(y.hovertext.format), FormatAsPercent, FormatAsReal)
    tick.format.function <- ifelse(percentFromD3(y.tick.format), FormatAsPercent, FormatAsReal)
    data.label.format.function <- ifelse(percentFromD3(data.label.format), FormatAsPercent, FormatAsReal)

    if (y.tick.format == "")
        y.tick.decimals <- max(0, -floor(log10(min(diff(tick.vals)))))
    else
        y.tick.decimals <- decimalsFromD3(y.tick.format)
    y.hovertext.decimals <- decimalsFromD3(y.hovertext.format, y.tick.decimals)
    data.label.decimals <- decimalsFromD3(data.label.format)

    # Convert data (polar) into x, y coordinates
    pos <- do.call(rbind, lapply(as.data.frame(chart.matrix), getPolarCoord))
    pos <- data.frame(pos,
                      Name = rep(rownames(chart.matrix)[c(1:n,1)], m),
                      Group = rep(colnames(chart.matrix),each = n+1),
                      stringsAsFactors  =  T, check.names = F)
    chart.matrix <- rbind(chart.matrix, chart.matrix[1,])
    tmp.group <- if (ncol(chart.matrix) == 1) ""
                 else paste0(pos$Group, ":", " ")

    pos <- cbind(pos,
            HoverText=sprintf("%s%s: %s%s%s", tmp.group, pos$Name, y.tick.prefix,
                hover.format.function(unlist(chart.matrix), decimals = y.hovertext.decimals,
                                      comma.for.thousands = commaFromD3(y.hovertext.format)), y.tick.suffix),
            DataLabels=sprintf("%s: %s%s%s", rownames(chart.matrix), data.label.prefix,
                data.label.format.function(unlist(chart.matrix), decimals = data.label.decimals),
                data.label.suffix))


    # Set margins
    footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate = FALSE)
    margins <- list(b = 20, l = 0, r = 0, t = 20, inner = 0)
    if (sum(nchar(subtitle)) > 0)
        subtitle <- paste0("<br>&nbsp;", subtitle, "<br>&nbsp;") # extra vertical space
    margins <- setMarginsForText(margins, title, subtitle, footer, title.font.size,
                                 subtitle.font.size, footer.font.size)
    footer.axis <- setFooterAxis(footer, footer.font, margins)
    xaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F,
               categoryorder = "array", categoryarray = unique(pos$Group))
    yaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F)
    margins <- setMarginsForLegend(margins, legend.show, legend, colnames(chart.matrix), type = "radar")
    margins <- setCustomMargins(margins, margin.top, margin.bottom, margin.left,
                    margin.right, margin.inner.pad)

    # Initialise plot
    p <- plot_ly(pos)
    g.list <- unique(pos$Group)
    for (ggi in 1:length(g.list))
    {
        ind <- which(pos$Group == g.list[ggi])
        p <- add_trace(p, x = pos$x[ind], y = pos$y[ind], name = g.list[ggi],
                    type = "scatter", mode = "lines", fill = "toself",
                    fillcolor = toRGB(colors[ggi], alpha = opacity),
                    legendgroup = g.list[ggi], showlegend = TRUE,
                    hoverinfo = "all+text", text = pos$HoverText[ind],
                    line = list(width = series.line.width, color = toRGB(colors[ggi])))
    }

    # Markers are added as a separate trace to allow overlapping hoverinfo
    for (ggi in 1:length(g.list))
    {
        ind <- which(pos$Group == g.list[ggi])
        ind <- ind[-length(ind)] # remove last duplicated point
        p <- add_trace(p, x = pos$x[ind], y = pos$y[ind], type = "scatter", mode = "markers+lines", fill = "none",
                    name = g.list[ggi], legendgroup = g.list[ggi],
                    showlegend = FALSE, hoverinfo = "text", text = pos$HoverText[ind],
                    marker = list(size = 1, color = toRGB(colors[ggi])), line = list(width = 0))

        if (data.label.show)
        {
            x.offset <- sign(pos$x[ind]) * 0.1 * abs(max(pos$x[ind]))
            y.offset <- sign(pos$y[ind]) * 0.1 * abs(max(pos$y[ind]))
            p <- add_trace(p, x = pos$x[ind] + x.offset, y = pos$y[ind] + y.offset,
                    type = "scatter", mode = "text", legendgroup = g.list[ggi],
                    showlegend = FALSE, hoverinfo = "none", text = pos$DataLabels[ind],
                    textfont = list(family = data.label.font.family, size = data.label.font.size,
                    color = data.label.font.color))
        }
    }

    # Radial grid lines
    outer <- getPolarCoord(rep(r.max, n))
    grid <- apply(outer, 1, function(zz){
        return(list(type = "line", x0 = 0, y0 = 0, x1 = zz[1], y1 = zz[2], layer = "below",
                    line = list(width = x.grid.width * grid.show, color = x.grid.color)))})

    # Hexagonal grid
    for (tt in tick.vals)
    {
        gpos <- getPolarCoord(rep(tt, n))
        for (i in 1:n)
            grid[[length(grid)+1]] <- list(type = "line", layer = "below",
                 x0 = gpos[i,1], x1 = gpos[i+1,1], y0 = gpos[i,2], y1 = gpos[i+1,2],
                 line = list(width = y.grid.width * grid.show, dash = "dot", color = y.grid.color))
    }

    # Position of labels (x-axis)
    xlabels <- NULL
    if (x.tick.show)
    {
        xanch <- rep("center", n)
        xanch[which(abs(outer[,2]) < r.max/100 & sign(outer[,1]) < 0)] <- "right"
        xanch[which(abs(outer[,2]) < r.max/100 & sign(outer[,1]) > 0)] <- "left"

        xlab <- autoFormatLongLabels(rownames(chart.matrix)[1:n],
                    x.tick.label.wrap, x.tick.label.wrap.nchar)
        font.asp <- fontAspectRatio(x.tick.font.family)

        # X-axis label widths are fixed to avoid the chart width changing in
        # Standard R regression tests. We avoided fixing the x-axis range
        # because autorange handles variation in the xaxis labels quite well
        xlab.width <- min(500,(font.asp + 0.5) * x.tick.font.size *
                        max(nchar(unlist(strsplit(split="<br>", as.character(xlab))))))
        xlabels <- list(x = outer[,1], y = outer[,2], text = xlab,
                    width = xlab.width, font = x.tick.font,
                    showarrow = F, yshift = outer[1:n,2]/r.max * 15,
                    xanchor = xanch, xshift = outer[1:n,1]/r.max)
    }
    p <- layout(p, margin = margins, title = title, titlefont = title.font,
            plot_bgcolor = toRGB(charting.area.fill.color, alpha = charting.area.fill.opacity),
            paper_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity),
            hovermode = if (tooltip.show) "closest" else FALSE,
            xaxis2 = footer.axis, xaxis = xaxis, yaxis = yaxis,
            legend = legend, showlegend = legend.show, shapes = grid, annotations = xlabels)

    if (grid.show && y.grid.width > 0 && y.tick.show && !is.null(tick.vals))
        p <- add_annotations(p, x = rep(0, length(tick.vals)), y = tick.vals,
                font = y.tick.font, showarrow = F, xanchor = "right", xshift = -5,
                text = paste0(y.tick.prefix, tick.format.function(tick.vals,
                              decimals = y.tick.decimals), y.tick.suffix))

    p <- addSubtitle(p, subtitle, subtitle.font, margins)
    p <- config(p, displayModeBar = modebar.show)
    p$sizingPolicy$browser$padding <- 0
    result <- list(htmlwidget = p)
    class(result) <- "StandardChart"
    result
}

