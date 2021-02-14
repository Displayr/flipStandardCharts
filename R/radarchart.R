#' Radar
#'
#' Radar chart, also known as web chart, spider chart, star chart, star plot, cobweb chart, irregular polygon, polar chart, or Kiviat diagram
#'
#' @inherit Column
#' @inherit Line
#' @param x Input data in the form of a vector or matrix. The categories used
#' to create the radar (i.e. the x-axis) is taken from the names/rownames of x.
#' @param opacity Opacity of area fill colors as an alpha value (0 to 1).
#' @param pad.left Spacing on the left of the chart. Mainly used by SmallMultiples.
#' @param pad.right Spacing on the right of the chart. Mainly used by SmallMultiples.
#' @param y.tick.show Whether to display the y-axis tick labels (i.e. radial distance from center)
#' @param x.tick.show  Whether to display the x-axis tick labels (i.e. labels around the sides of the radar chart)
#' @param line.thickness Thickness of outline of radar polygons.
#' @param data.label.values.only Logical; whether to show only the values in the
#'  datal labels instead of the default category label and values.
#' @param data.label.offset Numeric; controls the distance between the data points to
#'  the data label.
#' @param hovertext.show Logical; whether to show hovertext.
#'      making a particular category look larger than the others. However, it is not supported with small-multiples
#' @param small.mult An internal parameter used to indicate the chart is being called as a small multiple.
#' @importFrom grDevices rgb
#' @importFrom flipChartBasics ChartColors
#' @importFrom plotly plot_ly layout config
#' @importFrom flipFormat FormatAsReal
#' @export
Radar <- function(x,
                    annotation.list = NULL,
                    small.mult = FALSE,
                    overlay.annotation.list = NULL,  
                    title = "",
                    title.font.family = global.font.family,
                    title.font.color = global.font.color,
                    title.font.size = 16,
                    colors = ChartColors(max(1, ncol(x), na.rm = TRUE)),
                    opacity = NULL,
                    background.fill.color =  "transparent",
                    background.fill.opacity = 1,
                    charting.area.fill.color = background.fill.color,
                    charting.area.fill.opacity = 0,
                    legend.show = NA,
                    legend.orientation = "Vertical",
                    legend.wrap = TRUE,
                    legend.wrap.nchar = 30,
                    legend.fill.color = background.fill.color,
                    legend.fill.opacity = 0,
                    legend.border.color = rgb(44, 44, 44, maxColorValue = 255),
                    legend.border.line.width = 0,
                    legend.font.color = global.font.color,
                    legend.font.family = global.font.family,
                    legend.font.size = 10,
                    legend.ascending = NA,
                    legend.position.y = NULL,
                    legend.position.x = NULL,
                    hovertext.font.family = global.font.family,
                    hovertext.font.size = 11,
                    margin.autoexpand = TRUE,
                    margin.top = NULL,
                    margin.bottom = NULL,
                    margin.left = NULL,
                    margin.right = NULL,
                    margin.inner.pad = NULL,
                    pad.left = 0,
                    pad.right = 0,
                    line.thickness = 3,
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
                    y.bounds.minimum = NULL,
                    y.tick.distance = NULL,
                    y.grid.width = 1 * grid.show,
                    y.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                    y.tick.show = TRUE,
                    y.tick.suffix = "",
                    y.tick.prefix = "",
                    y.tick.format = "",
                    hovertext.show = TRUE,
                    y.hovertext.format = "",
                    y.tick.font.color = global.font.color,
                    y.tick.font.family = global.font.family,
                    y.tick.font.size = 10,
                    x.tick.label.wrap = TRUE,
                    x.tick.label.wrap.nchar = 21,
                    marker.show = FALSE,
                    marker.symbols = "circle",
                    marker.size = 6,
                    marker.colors = colors,
                    marker.opacity = 1.0,
                    data.label.show = FALSE,
                    data.label.offset = 0.1,
                    data.label.font.family = global.font.family,
                    data.label.font.size = 10,
                    data.label.font.color = global.font.color,
                    data.label.format = "",
                    data.label.prefix = "",
                    data.label.suffix = "",
                    data.label.values.only = FALSE,
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
    if (isPercentData(x))
    {
        if (isAutoFormat(y.tick.format))
            y.tick.format <- paste0(y.tick.format, "%")
        if (isAutoFormat(y.hovertext.format))
            y.hovertext.format <- paste0(y.hovertext.format, "%")
        if (isAutoFormat(data.label.format))
            data.label.format <- paste0(data.label.format, "%")

        sfx <- checkSuffixForExtraPercent(c(y.tick.suffix, data.label.suffix),
            c(y.tick.format, data.label.format))
        y.tick.suffix <- sfx[1]
        data.label.suffix <- sfx[2]
    }

    annot.data <- x
    chart.matrix <- checkMatrixNames(x)
    if (any(!is.finite(chart.matrix)))
        stop("Radar charts cannot contain missing or non-finite values.\n")
    m <- nrow(chart.matrix)
    n <- ncol(chart.matrix)

    legend.show <- setShowLegend(legend.show, NCOL(chart.matrix))
    if (is.null(n) || n == 1 || m == 1)
    {
        # only 1 series
        chart.matrix <- data.frame(x = chart.matrix, check.names = FALSE)
        m <- nrow(chart.matrix)
        n <- ncol(chart.matrix)
    }

    if (m <= 2)
    {
        warning("Radar chart only has two or less spokes. ",
                "It may be more appropriate to use another chart type.")
    }
    if (is.null(opacity))
        opacity <- 0.4
    if (is.null(marker.show))
        marker.show <- FALSE
    if (is.null(marker.opacity))
        marker.opacity <- opacity

    # Set colors
    colors <- vectorize(colors, n)
    if (is.null(marker.colors))
        marker.colors <- colors
    marker.colors <- vectorize(marker.colors, n)

    title.font = list(family = title.font.family, size = title.font.size, color = title.font.color)
    subtitle.font = list(family = subtitle.font.family, size = subtitle.font.size, color = subtitle.font.color)
    x.tick.font = list(family = x.tick.font.family, size = x.tick.font.size, color = x.tick.font.color)
    y.tick.font = list(family = y.tick.font.family, size = y.tick.font.size, color = y.tick.font.color)
    footer.font = list(family = footer.font.family, size = footer.font.size, color = footer.font.color)
    legend.font = list(family = legend.font.family, size = legend.font.size, color = legend.font.color)
    legend <- setLegend("Radar", legend.font, legend.ascending, legend.fill.color, legend.fill.opacity,
                        legend.border.color, legend.border.line.width, legend.position.x, legend.position.y,
                        FALSE, legend.orientation)

    y.axis.offset <- if (any(data.label.show)) 1.1 + data.label.font.size/100
                     else                      1.0
    y.bounds <- setRadarAxisBounds(y.bounds.minimum, y.bounds.maximum,
                                   chart.matrix, y.axis.offset)
    y.bounds.minimum <- y.bounds$min
    y.bounds.maximum <- y.bounds$max

    if (is.null(y.tick.distance))
    {
        y.diff <- y.bounds.maximum - y.bounds.minimum
        base <- 10^round(log10(abs(y.diff)) - 1)
        mult <- max(1, floor((abs(y.diff)/base)/5))
        y.tick.distance <- sign(y.diff) * base * mult
    }
    tick.vals <- seq(from = y.bounds.minimum, to = y.bounds.maximum, by = sum(y.tick.distance, na.rm = TRUE))
    r.max <- abs(y.bounds.maximum - y.bounds.minimum)

    hover.format.function <- ifelse(percentFromD3(y.hovertext.format), FormatAsPercent, FormatAsReal)
    tick.format.function <- ifelse(percentFromD3(y.tick.format), FormatAsPercent, FormatAsReal)
    data.label.format.function <- ifelse(percentFromD3(data.label.format), FormatAsPercent, FormatAsReal)

    if (y.tick.format == "")
        y.tick.decimals <- max(0, -floor(log10(min(abs(diff(tick.vals))))))
    else
        y.tick.decimals <- decimalsFromD3(y.tick.format)
    y.hovertext.decimals <- decimalsFromD3(y.hovertext.format, y.tick.decimals)
    data.label.decimals <- decimalsFromD3(data.label.format)
    data.label.prefix <- rbind(vectorize(data.label.prefix, n, m, split = NULL), "")
    data.label.suffix <- rbind(vectorize(data.label.suffix, n, m, split = NULL), "")
    marker.show <- vectorize(marker.show, n, m)
    marker.symbols <- vectorize(marker.symbols, n, m)
    marker.size <- vectorize(marker.size, n, m)

    # Convert data (polar) into x, y coordinates
    pos <- do.call(rbind, lapply(as.data.frame(chart.matrix), calcPolarCoord,
                    r0 = y.bounds.minimum))
    pos <- data.frame(pos,
                      Name = rep(rownames(chart.matrix)[c(1:m,1)], n),
                      Group = if (NCOL(chart.matrix) == 1 && colnames(chart.matrix)[1] == "Series.1") ""
                              else rep(colnames(chart.matrix), each = m+1),
                      row = rep(c(1:m,1), n),
                      stringsAsFactors  =  T, check.names = F)

    chart.matrix <- rbind(chart.matrix, chart.matrix[1,])
    if (data.label.values.only)
        tmp.labels <- sprintf("%s%s%s", data.label.prefix,
                data.label.format.function(unlist(chart.matrix), decimals = data.label.decimals),
                data.label.suffix)
    else
        tmp.labels <- sprintf("%s: %s%s%s", rownames(chart.matrix), data.label.prefix,
                data.label.format.function(unlist(chart.matrix), decimals = data.label.decimals),
                data.label.suffix)
    pos <- cbind(pos,
            HoverText = sprintf("%s: %s%s%s", pos$Name, y.tick.prefix,
                hover.format.function(unlist(chart.matrix), decimals = y.hovertext.decimals,
                                      comma.for.thousands = commaFromD3(y.hovertext.format)), y.tick.suffix),
            DataLabels = tmp.labels)

    # Set margins
    g.list <- unique(pos$Group)
    footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate = FALSE)
    margins <- list(b = 20, l = 0, r = 0, t = 20, inner = 0)
    if (sum(nchar(subtitle)) > 0)
        subtitle <- paste0("<br>&nbsp;", subtitle, "<br>&nbsp;") # extra vertical space
    margins <- setMarginsForText(margins, title, subtitle, footer, title.font.size,
                                 subtitle.font.size, footer.font.size)
    xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE,
               categoryorder = "array", categoryarray = g.list, 
               constrain = "domain", scaleanchor = "y", scaleratio = 1)
    yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE,
               constrain = "domain", scaleanchor = "x", scaleratio = 1)

    legend.text <- autoFormatLongLabels(colnames(chart.matrix), legend.wrap, legend.wrap.nchar)
    margins <- setMarginsForLegend(margins, legend.show, legend, legend.text, type = "radar")
    margins <- setCustomMargins(margins, margin.top, margin.bottom, margin.left,
                    margin.right, margin.inner.pad)
    margins$autoexpand <- margin.autoexpand

    # Initialise plot (ensure chart area reaches y.bounds.maximum)
    p <- plot_ly(pos)
    outer <- calcPolarCoord(rep(y.bounds.maximum, m), r0 = y.bounds.minimum)
    x.offset <- rep(0, nrow(outer))
    x.offset[which.min(outer[,1])] <- -pad.left
    x.offset[which.max(outer[,1])] <- pad.right
    p <- add_trace(p, x = outer[,1] + x.offset, y = outer[,2], name = "Outer", showlegend = FALSE,
                   type = "scatter", mode = "markers", opacity = 0, hoverinfo = "skip")

    # Grid lines
    grid <- NULL
    if (grid.show)
    {
        # Spokes
        grid <- apply(outer, 1, function(zz){
        return(list(type = "line", x0 = 0, y0 = 0, x1 = zz[1], y1 = zz[2], layer = "below",
                    line = list(width = x.grid.width * grid.show, color = x.grid.color)))})

        # Hexagonal grid
        for (tt in tick.vals)
        {
            gpos <- calcPolarCoord(rep(tt, m), r0 = y.bounds.minimum)
            for (i in 1:m)
                grid[[length(grid)+1]] <- list(type = "line", layer = "below",
                     x0 = gpos[i,1], x1 = gpos[i+1,1], y0 = gpos[i,2], y1 = gpos[i+1,2],
                     line = list(width = y.grid.width * grid.show, dash = "dot",
                     xref = "x", yref = "y", color = y.grid.color))
        }
    }

    # Initialise x-axis labels
    xlab <- if (x.tick.show) autoFormatLongLabels(rownames(chart.matrix)[1:m],
                                x.tick.label.wrap, x.tick.label.wrap.nchar)
            else             rep("", m)


    # Create annotations separately for each series
    # so they can be toggled using the legend
    for (ggi in 1:n)
    {
        for (curr.annot.ind in seq_along(overlay.annotation.list))
        {
            curr.annot <- overlay.annotation.list[[curr.annot.ind]]
            curr.annot$threshold <- parseThreshold(curr.annot$threshold)
            curr.dat <- getAnnotData(annot.data, curr.annot$data, ggi,
                as.numeric = !grepl("Text", curr.annot$type) && curr.annot$data != "Column Comparisons")
            ind.sel <- extractSelectedAnnot(curr.dat, curr.annot$threshold, curr.annot$threstype)
            if (is.null(curr.annot$color))
                curr.annot$color <- colors[ggi]
            for (ii in ind.sel)
                xlab[ii] <- addAnnotToDataLabel(xlab[ii], curr.annot, curr.dat[ii],
                    prepend = calcXAlign(ii, m) == "left") 

        }
    }

    # Add x-axis labels
    # If x-axis label are not shown, annotations may still be present
    annotations <- NULL
    if (any(nchar(xlab) > 0))
    {

        # We use annotations rather than a text trace because
        # plotly will automatically expand margins to keep annotations visible
        annotations <- lapply(1:m, function(ii) list(text = xlab[ii], font = x.tick.font,
                        x = outer[ii,1], y = outer[ii,2], xref = "x", yref = "y",
                        xanchor = calcXAlign(ii, m, return.anchor = TRUE), 
                        yanchor = calcYAlign(ii, m, return.anchor = TRUE),
                        showarrow = FALSE, ax = 0, ay = 0))
    }

    n <- length(g.list)
    if (is.null(line.thickness))
        line.thickness <- 3

    if (small.mult && length(data.label.show) > 1 && n == 2) # small multiples
    {
        line.thickness <- c(line.thickness, 0)
        opacity <- c(opacity, if (opacity == 0.0) 0.2 else opacity)
    }
    else
    {
        line.thickness <- vectorize(line.thickness, n)
        opacity <- vectorize(opacity, n)
    }

    line.thickness <- vectorize(line.thickness, n)
    opacity <- vectorize(opacity, n)
    hovertext.show <- vectorize(hovertext.show, n)
    data.label.show <- rbind(vectorize(data.label.show, n, m), FALSE)
    data.label.offset <- sapply(vectorize(data.label.offset, n), charToNumeric)
    data.label.font.color <- vectorize(data.label.font.color, n)
    data.label.font = lapply(data.label.font.color,
        function(cc) list(family = data.label.font.family, size = data.label.font.size, color = cc))

    # Main trace
    for (ggi in 1:n)
    {
        series.mode <- "lines"
        series.marker <- NULL
        if (any(marker.show[,ggi]))
        {
            series.mode <- "lines+markers"
            marker.size[which(!marker.show[,ggi]),ggi] <- 0
            series.marker <- list(size = marker.size[,ggi],
                color = toRGB(marker.colors[ggi], alpha = marker.opacity),
                line = list(color = marker.colors[ggi], alpha = marker.opacity,
                    width = 0),
                symbol = marker.symbols[,ggi], opacity = 1.0)
        }

        ind <- which(pos$Group == g.list[ggi])
        p <- add_trace(p, x = pos$x[ind], y = pos$y[ind], name = legend.text[ggi],
                    type = "scatter", mode = series.mode, fill = "toself",
                    marker = series.marker,
                    fillcolor = toRGB(colors[ggi], alpha = opacity[ggi]),
                    legendgroup = g.list[ggi], showlegend = TRUE,
                    hoverinfo = "skip", hoveron = "points",
                    line = list(width = line.thickness[ggi], color = toRGB(colors[ggi])))
    }

    # Markers are added as a separate trace to allow overlapping hoverinfo
    for (ggi in n:1)
    {
        ind <- which(pos$Group == g.list[ggi])
        ind <- ind[-length(ind)] # remove last duplicated point
        p <- add_trace(p, x = pos$x[ind], y = pos$y[ind], type = "scatter", mode = "markers",
                    name = g.list[ggi], legendgroup = g.list[ggi], opacity = 0,
                    showlegend = FALSE, text = pos$HoverText[ind],
                    hovertemplate = paste0("%{text}<extra>", pos$Group[ind], "</extra>"),
                    hoverlabel = list(font = list(color = autoFontColor(colors[ggi]),
                    size = hovertext.font.size, family = hovertext.font.family)),
                    marker = list(size = 5, color = toRGB(colors[ggi])))

        if (any(data.label.show[,ggi]))
        {
            # Sequentially apply annotations
            for (j in seq_along(annotation.list))
            {
                if (!checkAnnotType(annotation.list[[j]]$type, "Line"))
                    next
                annotation.list[[j]]$threshold <- parseThreshold(annotation.list[[j]]$threshold)
                a.tmp <- annotation.list[[j]]
                tmp.dat <- getAnnotData(annot.data, a.tmp$data, ggi, as.numeric = !grepl("Text", a.tmp$type))
                ind.sel <- extractSelectedAnnot(tmp.dat, a.tmp$threshold, a.tmp$threstype)
                pos$DataLabels[ind[ind.sel]] <- addAnnotToDataLabel(pos$DataLabels[ind[ind.sel]], a.tmp, tmp.dat[ind.sel])
            }

            ind2 <- intersect(ind, which(data.label.show))
            
            # For single-series or small multiples we prefer to use annotations
            # because they can be dragged, and are less likely to be truncated
            # by plotly. However, annotations do not toggle with the legend
            # so they are not used for multiple overlapping data series 
            if (n == 1 || small.mult)
            {
                annotations <- c(annotations,
                    lapply(ind2, function(ii) list(text = pos$DataLabels[ii],
                        font = data.label.font[[ggi]], x = pos$x[ii], 
                        y = pos$y[ii], xref = "x", yref = "y",
                        xshift = 2, yshift = 2,
                        xanchor = calcXAlign(pos$row[ii], m, return.anchor = TRUE),
                        yanchor = calcYAlign(pos$row[ii], m, return.anchor = TRUE),
                        showarrow = TRUE, ax = 0, ay = 0, arrowsize = 0.3)))
            } else
            {
                p <- add_trace(p, x = pos$x[ind2], y = pos$y[ind2],
                    type = "scatter", mode = "markers+text", legendgroup = g.list[ggi],
                    textposition = paste(calcXAlign(pos$row[ind2], m), 
                    calcYAlign(pos$row[ind2], n)), showlegend = FALSE, 
                    hoverinfo = "skip", marker = list(opacity = 0, size = 2, 
                    color = toRGB(colors[ggi])),
                    text = pos$DataLabels[ind2],
                    textfont = data.label.font[[ggi]], cliponaxis = FALSE)
            }
        }
    }

    annot.len <- length(annotations)
    annotations[[annot.len+1]] <- setFooter(footer, footer.font, margins)
    annotations[[annot.len+2]] <- setTitle(title, title.font, margins)
    annotations[[annot.len+3]] <- setSubtitle(subtitle, subtitle.font, margins)

    if (grid.show && y.grid.width > 0 && y.tick.show && !is.null(tick.vals))
    {
        for (i in 1:length(tick.vals))
            annotations[[annot.len+3+i]] <- list(x = 0,
                y = tick.vals[i] - y.bounds.minimum,
                font = y.tick.font, showarrow = FALSE, xanchor = "right",
                xshift = -5, xref = "x", yref = "y",
                text = paste0(y.tick.prefix, tick.format.function(tick.vals[i],
                             decimals = y.tick.decimals), y.tick.suffix))
    }
    p <- layout(p, margin = margins,
            annotations = annotations,
            plot_bgcolor = toRGB(charting.area.fill.color, alpha = charting.area.fill.opacity),
            paper_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity),
            hovermode = if (tooltip.show) "closest" else FALSE,
            hoverlabel = list(namelength = -1, bordercolor = "transparent",
                font = list(size = hovertext.font.size, family = hovertext.font.family)),
            xaxis = xaxis, yaxis = yaxis, shapes = grid,
            legend = legend, showlegend = legend.show)

    # allow data labels to be movable (annotations with showarrow = TRUE)
    # but turn off editing to other parts of the text
    p <- config(p, editable = TRUE, 
                edits = list(annotationPosition = FALSE, annotationTail = TRUE,
                annotationText = FALSE, shapePosition = FALSE,
                axisTitleText = FALSE, titleText = FALSE, legendText = FALSE))


    p <- config(p, displayModeBar = modebar.show)
    p$sizingPolicy$browser$padding <- 0
    #attr(p, "can-run-in-root-dom") <- TRUE
    result <- list(htmlwidget = p)
    class(result) <- "StandardChart"
    attr(result, "ChartType") <- "Radar Filled"
    result
}

calcPolarCoord <- function(r, r0 = 0)
{
    # Get starting angle and angle increments
    theta <- 0.5 * pi
    dtheta <- -2 * pi / length(r)

    # Get polar coordinates
    x <- c()
    y <- c()

    for(i in 1:length(r)){

        x <- c(x, (r[i] - r0) * cos(theta))
        y <- c(y, (r[i] - r0) * sin(theta))

        theta <- theta + dtheta
    }

    # Return to initial point to complete circle
    x[length(x) + 1] <- x[1]
    y[length(y) + 1] <- y[1]

    return(cbind(x, y))
}

# Y axis bounds that are within the data range are ignored with a warning
# Radar charts cannot handle the chart/axis falling outside the plot area
# If only the min/max is specified, we automatically determine whether
# the axis should be reversed (i.e. the largest values shown in the center
# of the radar chart

setRadarAxisBounds <- function(y.bounds.minimum,
                               y.bounds.maximum,
                               chart.matrix,
                               offset = 1.0)
{
    if (is.character(y.bounds.maximum))
        y.bounds.maximum <- charToNumeric(y.bounds.maximum)
    if (is.character(y.bounds.minimum))
        y.bounds.minimum <- charToNumeric(y.bounds.minimum)
    range0 <- range(chart.matrix, na.rm = TRUE)
    if (length(y.bounds.maximum) == 0)
        y.bounds.maximum <- if (isTRUE(y.bounds.minimum >= range0[2])) range0[1]
                            else                                       offset * range0[2]
    if (length(y.bounds.minimum) == 0)
        y.bounds.minimum <- if (length(chart.matrix) == 1)                  min(0, min(chart.matrix))
                            else if (isTRUE(y.bounds.maximum <= range0[1])) range0[2]
                            else                                            min(0, min(chart.matrix))

    y.inside.bounds <- NULL
    if ((y.bounds.minimum > range0[1] && y.bounds.minimum < range0[2]) ||
        (y.bounds.minimum > range0[2] && y.bounds.minimum < range0[1]))
    {
        y.inside.bounds <- c(y.inside.bounds, "minimum")
        y.bounds.minimum <- if (y.bounds.maximum <= range0[1]) range0[2] else range0[1]
    }
    if ((y.bounds.maximum > range0[1] && y.bounds.maximum < range0[2]) ||
        (y.bounds.maximum > range0[2] && y.bounds.maximum < range0[1]))
    {
        y.inside.bounds <- c(y.inside.bounds, "maximum")
        y.bounds.maximum <- if (y.bounds.minimum >= range0[2]) range0[1] else range0[2]
    }
    if (length(y.inside.bounds) > 0)
        warning("The ", paste(y.inside.bounds, collapse = " and "),
                " of the radial axis was ignored because axis bounds must be outside the range of the input data. ",
                "Please specify a value outside [", range0[1], ", ", range0[2], "].")

    return(list(min = y.bounds.minimum, max = y.bounds.maximum))
}


# return.anchor indicates that the return values
# will be used for annotations, otherwise they
# will be used in textposition of a text trace
# note that they have the oppositie meaning
calcXAlign <- function(index, length, return.anchor = FALSE)
{
    theta <- (0.5 - 2 * (index - 1)/length) * pi
    x.align <- rep("center", length(theta))
   
    if (return.anchor)
    {
        x.align[cos(theta) > 0.3] <- "left"
        x.align[cos(theta) < -0.3] <- "right"

    } else
    {
        x.align[cos(theta) > 0.3] <- "right"
        x.align[cos(theta) < -0.3] <- "left"
    }

    return(x.align)
}

calcYAlign <- function(index, length, return.anchor = FALSE)
{
    theta <- (0.5 - 2 * (index - 1)/length) * pi
    y.align <- rep("middle", length(theta))
   
    if (return.anchor)
    {
        y.align[sin(theta) > 0.5] <- "bottom"
        y.align[sin(theta) < -0.5] <- "top"

    } else
    {
        y.align[sin(theta) > 0.5] <- "top"
        y.align[sin(theta) < -0.5] <- "bottom"
    }

    return(y.align)
}

