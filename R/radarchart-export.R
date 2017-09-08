#' Radar chart
#'
#' Plot radar chart, also known as web chart, spider chart, star chart, star plot, cobweb chart, irregular polygon, polar chart, or Kiviat diagram
#'
#' @param y A table, matrix, vector or data frame.
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
#' opacity as an alpha value (0 to 1).
#' @param legend.show Logical; show the legend.
#' @param legend.fill Same as \code{legend.fill.color}. Retained for backwards compatibility.
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
#' @param y.bounds.minimum Minimum of range for plotting;
#' NULL = no manual range set.  Must be less than y.bounds.maximum
#' @param y.bounds.maximum Maximum of range for
#' plotting; NULL = no manual range set.  Must be greater than y.bounds.minimum
#' @param y.tick.distance Tick mark distance.
#' @param y.grid.width Width of y-grid lines in pixels; 0 = no line
#' @param y.grid.color Color of y-grid lines as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param y.tick.show Whether to display the y-axis tick labels
#' @param y.tick.suffix y-axis tick label suffix
#' @param y.tick.prefix y-axis tick label prefix
#' @param y.tick.decimals y-axis tick label decimal places
#' @param y.hovertext.decimals y-axis hover text decimal places
#' @param y.tick.font.color y-axis tick label font color as a named color
#' in character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param y.tick.font.family Character; y-axis tick label font family
#' @param y.tick.font.size y-axis tick label font size
#' @param x.title Character, x-axis title; defaults to chart input values;
#' to turn off set to "FALSE".
#' @param x.title.font.color x-axis title font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param x.title.font.family Character; x-axis title font family
#' @param x.title.font.size x-axis title font size
#' @param x.grid.width Width of y-grid lines in pixels; 0 = no line
#' @param x.grid.color Color of y-grid lines as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param label.wrap Logical; whether to wrap long labels on the x-axis.
#' @param label.wrap.nchar Integer; number of characters in each line when \code{label.wrap} is \code{TRUE}.
#' @param series.marker.colors Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' be reversed.
#' @param series.marker.size Size in pixels of marker
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
#' @param data.label.decimals Number of decimal places to show in data labels.
#' @param data.label.prefix Character; prefix for data values.
#' @param data.label.suffix Character; suffix for data values.
#' @param ... Extra arguments that are ignored.
#' @importFrom grDevices rgb
#' @importFrom flipChartBasics ChartColors
#' @importFrom plotly plot_ly layout config add_annotations
#' @importFrom flipFormat FormatAsReal
#' @export
RadarChart <- function(y,
                    title = "",
                    title.font.family = global.font.family,
                    title.font.color = global.font.color,
                    title.font.size = 16,
                    colors = ChartColors(max(1, ncol(y))),
                    background.fill.color = rgb(255, 255, 255, maxColorValue = 255),
                    background.fill.opacity = 1,
                    charting.area.fill.color = rgb(255, 255, 255, maxColorValue = 255),
                    charting.area.fill.opacity = 1,
                    legend.show = TRUE,
                    legend.fill.color = rgb(255, 255, 255, maxColorValue = 255),
                    legend.fill.opacity = 1,
                    legend.border.color = rgb(44, 44, 44, maxColorValue = 255),
                    legend.border.line.width = 0,
                    legend.font.color = global.font.color,
                    legend.font.family = global.font.family,
                    legend.font.size = 10,
                    legend.ascending = NA,
                    #legend.x.anchor = "left",
                    #legend.y.anchor = "auto",
                    #legend.y = 1,
                    #legend.x = 1.02,
                    margin.top = NULL,
                    margin.bottom = NULL,
                    margin.left = NULL,
                    margin.right = NULL,
                    margin.inner.pad = NULL,
                    series.marker.colors = NULL,
                    series.marker.size = 6,
                    series.line.width = 3,
                    tooltip.show = TRUE,
                    modebar.show = FALSE,
                    global.font.family = "Arial",
                    global.font.color = rgb(44, 44, 44, maxColorValue = 255),
                    x.title = "", # ignored but avoids matching problem
                    x.title.font.color = global.font.color,
                    x.title.font.family = global.font.family,
                    x.title.font.size = 12,
                    x.grid.width = 1,
                    x.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                    y.bounds.minimum = NULL,
                    y.bounds.maximum = NULL,
                    y.tick.distance = NULL,
                    y.grid.width = 1,
                    y.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                    y.tick.show = TRUE,
                    y.tick.suffix = "",
                    y.tick.prefix = "",
                    y.tick.decimals = NULL,
                    y.hovertext.decimals = NULL,
                    y.tick.font.color = NULL,
                    y.tick.font.family = NULL,
                    y.tick.font.size = 10,
                    label.wrap = TRUE,
                    label.wrap.nchar = 21,
                    data.label.show = FALSE,
                    data.label.font.family = global.font.family,
                    data.label.font.size = 10,
                    data.label.font.color = global.font.color,
                    data.label.decimals = 2,
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
                    footer.wrap.nchar = 100,
                    ...)

{
    # Check data
    chart.matrix <- as.matrix(y)
    if (any(!is.finite(chart.matrix)))
        stop("Missing charts cannot contain missing or non-finite values.\n")
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
    } else if (n == 2)
    {
        warning("Radar chart only has two spokes. It may be more appropriate to use another chart type.")
    }
    
    title.font = list(family = title.font.family, size = title.font.size, color = title.font.color)
    subtitle.font = list(family = subtitle.font.family, size = subtitle.font.size, color = subtitle.font.color)
    x.title.font = list(family = x.title.font.family, size = x.title.font.size, color = x.title.font.color)
    y.tick.font = list(family = y.tick.font.family, size = y.tick.font.size, color = y.tick.font.color)
    footer.font = list(family = footer.font.family, size = footer.font.size, color = footer.font.color)
    legend.font = list(family = legend.font.family, size = legend.font.size, color = legend.font.color)
    legend <- setLegend("Radar", legend.font, legend.ascending, legend.fill.color, legend.fill.opacity,
                        legend.border.color, legend.border.line.width)
    footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate=FALSE)

    # Figure out positions of y-ticks (i.e. radial axis)
    tick.vals <- NULL
    if (is.null(y.bounds.minimum))
        y.bounds.minimum <- 0
    if (is.null(y.bounds.maximum))
    {
        offset <- 1.0
        if (data.label.show)
            offset <- 1 + data.label.font.size/100
        y.bounds.maximum <- offset * max(chart.matrix)
    }
    if (is.null(y.tick.distance))
    {
        base <- 10^round(log10(y.bounds.maximum) - 1)
        mult <- max(1, floor((y.bounds.maximum/base)/5))
        y.tick.distance <- base * mult
    }
    tick.vals <- seq(from = y.bounds.minimum, to = y.bounds.maximum, by = y.tick.distance)
    r.max <- y.bounds.maximum

    if (is.null(y.tick.decimals))
        y.tick.decimals <- max(0, -floor(log10(min(diff(tick.vals)))))
    if (is.null(y.hovertext.decimals))
        y.hovertext.decimals <- y.tick.decimals

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
                FormatAsReal(unlist(chart.matrix), decimals = y.hovertext.decimals), y.tick.suffix),
            DataLabels=sprintf("%s%s%s", data.label.prefix,
                FormatAsReal(unlist(chart.matrix), decimals = data.label.decimals),
                data.label.suffix))

    
    # Set margins
    footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate = FALSE)
    margins <- list(b = 20, l = 0, r = 0, t = 20, inner = 0)
    margins <- setMarginsForText(margins, title, subtitle, footer, title.font.size, 
                                 subtitle.font.size, footer.font.size)
    subtitle.axis <- setSubtitleAxis(subtitle, subtitle.font, title, title.font)
    footer.axis <- setFooterAxis(footer, footer.font, margins)
    xaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F,
               categoryorder = "array", categoryarray = unique(pos$Group))
    yaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F,
               domain = c(0, 0.95+(0.05*is.null(subtitle.axis))))
    if (!is.null(margin.top))
        margins$t <- margin.top
    if (!is.null(margin.left))
        margins$l <- margin.left
    if (!is.null(margin.bottom))
        margins$b <- margin.bottom
    if (!is.null(margin.right))
        margins$r <- margin.right

    # Initialise plot
    p <- plot_ly(pos)
    g.list <- unique(pos$Group)
    for (ggi in 1:length(g.list))
    {
        ind <- which(pos$Group == g.list[ggi])
        p <- add_trace(p, x = pos$x[ind], y = pos$y[ind], type = "scatter", mode = "lines", fill = "toself",
                    name = g.list[ggi], legendgroup = g.list[ggi],
                    showlegend = TRUE, hoverinfo = "all+text", text = pos$HoverText[ind],
                    line = list(width = series.line.width, color = toRGB(series.marker.colors[ggi])))
    }

    # Markers are added as a separate trace to allow overlapping hoverinfo
    for (ggi in 1:length(g.list))
    {
        ind <- which(pos$Group == g.list[ggi])
        p <- add_trace(p, x = pos$x[ind], y = pos$y[ind], type = "scatter", mode = "markers+lines", fill = "none",
                    name = g.list[ggi], legendgroup = g.list[ggi],
                    showlegend = FALSE, hoverinfo = "text", text = pos$HoverText[ind],
                    marker = list(size = 1, color = toRGB(colors[ggi])), line = list(width = 0))

        if (data.label.show)
            p <- add_trace(p, x = pos$x[ind]*1.12, y = pos$y[ind]*1.06,
                    type = "scatter", mode = "text", legendgroup = g.list[ggi],
                    showlegend = FALSE, hoverinfo = "none", text = pos$DataLabels[ind],
                    textfont = list(family = data.label.font.family, size = data.label.font.size,
                    color = data.label.font.color))
    }

    # Radial grid lines
    outer <- getPolarCoord(rep(r.max, n))
    grid <- apply(outer, 1, function(zz){
        return(list(type = "line", x0 = 0, y0 = 0, x1 = zz[1], y1 = zz[2], layer = "below",
                    line = list(width = x.grid.width, color = x.grid.color)))})

    # Hexagonal grid
    for (tt in tick.vals)
    {
        gpos <- getPolarCoord(rep(tt, n))
        for (i in 1:n)
            grid[[length(grid)+1]] <- list(type = "line", layer = "below",
                 x0 = gpos[i,1], x1 = gpos[i+1,1], y0 = gpos[i,2], y1 = gpos[i+1,2],
                 line = list(width = y.grid.width, dash = "dot", color = y.grid.color))
    }

    # Position of labels (x-axis)
    xanch <- rep("center", n)
    xanch[which(abs(outer[,2]) < r.max/100 & sign(outer[,1]) < 0)] <- "right"
    xanch[which(abs(outer[,2]) < r.max/100 & sign(outer[,1]) > 0)] <- "left"

    xlab <- autoFormatLongLabels(rownames(chart.matrix)[1:n], label.wrap, label.wrap.nchar)
    font.asp <- fontAspectRatio(x.title.font.family)

    # X-axis label widths are fixed to avoid the chart width changing in regression tests.
    # We avoided fixing the x-axis range because autorange handles variation in the xaxis labels quite well
    xlab.width <- (font.asp + 0.5) * x.title.font.size * 
                    max(nchar(unlist(strsplit(split="<br>", as.character(xlab)))))
    xlabels <- list(x = outer[,1], y = outer[,2], text = xlab, 
                width = xlab.width, font = x.title.font,
                showarrow = F, yshift = outer[1:n,2]/r.max * 15,
                xanchor = xanch, xshift = outer[1:n,1]/r.max)

    p <- layout(p, margin = margins, title = title, titlefont = title.font, 
            plot_bgcolor = toRGB(charting.area.fill.color, alpha = charting.area.fill.opacity),
            paper_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity),
            hovermode = if (tooltip.show) "closest" else FALSE,
            xaxis2 = footer.axis, xaxis3 = subtitle.axis, xaxis = xaxis, yaxis = yaxis,
            legend = legend, showlegend = legend.show, shapes = grid, annotations = xlabels)

    if (y.grid.width > 0 && y.tick.show && !is.null(tick.vals))
        p <- add_annotations(p, x=rep(0, length(tick.vals)), y = tick.vals, 
                font = y.tick.font, showarrow = F, xanchor = "right", xshift = -5,
                text = paste0(y.tick.prefix, FormatAsReal(tick.vals, decimals = y.tick.decimals), y.tick.suffix))

    p <- config(p, displayModeBar = modebar.show)
    p$sizingPolicy$browser$padding <- 0
    return(p)
}

