#' @importFrom plotly plot_ly layout config add_annotations
#' @importFrom flipFormat FormatAsReal
#' @importFrom flipU StopForUserError
radarChart <- function(chart.matrix,
                    title = "",
                    title.font.family = NULL,
                    title.font.color = NULL,
                    title.font.size = 16,
                    colors = NULL,
                    background.fill.color = rgb(255, 255, 255, maxColorValue = 255),
                    background.fill.opacity = 1,
                    charting.area.fill.color = rgb(255, 255, 255, maxColorValue = 255),
                    charting.area.fill.opacity = 1,
                    legend.show = TRUE,
                    legend.fill.color = rgb(255, 255, 255, maxColorValue = 255),
                    legend.fill.opacity = 1,
                    legend.border.color = rgb(44, 44, 44, maxColorValue = 255),
                    legend.border.line.width = 0,
                    legend.font.color = NULL,
                    legend.font.family = NULL,
                    legend.font.size = 10,
                    legend.x.anchor = "left",
                    legend.y.anchor = "auto",
                    legend.y = 1,
                    legend.x = 1.02,
                    legend.sort.order = "normal",
                    margin.top = NULL,
                    margin.bottom = NULL,
                    margin.left = NULL,
                    margin.right = NULL,
                    margin.inner.pad = NULL,
                    series.marker.colors = NULL,
                    series.marker.size = 6,
                    series.line.width = NULL,
                    tooltip.show = TRUE,
                    modebar.show = FALSE,
                    x.title.font.color = NULL,
                    x.title.font.family = NULL,
                    x.title.font.size = 12,
                    x.grid.width,
                    x.grid.color,
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
                    x.tick.label.wordwrap = TRUE,
                    wordwrap.nchar = 21,
                    data.label.show = FALSE,
                    data.label.font.family = NULL,
                    data.label.font.size = 10,
                    data.label.font.color = NULL,
                    data.label.decimals = 2,
                    data.label.prefix = "",
                    data.label.suffix = "",
                    subtitle.axis = NULL,
                    footer.axis = NULL)
#' @importFrom flipU StopForUserError

{
    ErrorIfNotEnoughData(chart.matrix)
    if (any(!is.finite(chart.matrix)))
        StopForUserError("Missing charts cannot contain missing or non-finite values.")
    if (any(chart.matrix < 0))
        StopForUserError("Radar charts cannot have negative values.")

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
    tick.vals <- seq(from=y.bounds.minimum, to=y.bounds.maximum, by=y.tick.distance)
    r.max <- y.bounds.maximum

    if (is.null(y.tick.decimals))
        y.tick.decimals <- max(0, -floor(log10(min(diff(tick.vals)))))
    if (is.null(y.hovertext.decimals))
        y.hovertext.decimals <- y.tick.decimals


    # Convert data (polar) into x, y coordinates
    pos <- do.call(rbind, lapply(as.data.frame(chart.matrix), getPolarCoord))
    pos <- data.frame(pos,
                      Name=rep(rownames(chart.matrix)[c(1:n,1)], m),
                      Group=rep(colnames(chart.matrix),each=n+1),
                      stringsAsFactors = T, check.names=F)
    chart.matrix <- rbind(chart.matrix, chart.matrix[1,])
    tmp.group <- if (ncol(chart.matrix) == 1) ""
                 else paste0(pos$Group, ":", " ")

    pos <- cbind(pos,
            HoverText=sprintf("%s%s: %s%s%s", tmp.group, pos$Name, y.tick.prefix,
                FormatAsReal(unlist(chart.matrix), decimals = y.hovertext.decimals), y.tick.suffix),
            DataLabels=sprintf("%s%s%s", data.label.prefix,
                FormatAsReal(unlist(chart.matrix), decimals = data.label.decimals),
                data.label.suffix))

    # Initialise plot
    p <- plot_ly(pos)
    g.list <- unique(pos$Group)
    for (ggi in 1:length(g.list))
    {
        ind <- which(pos$Group == g.list[ggi])
        p <- add_trace(p, x=pos$x[ind], y=pos$y[ind], type="scatter", mode="lines", fill="toself",
                    name=g.list[ggi], legendgroup=g.list[ggi], fillcolor=toRGB(series.marker.colors[ggi], alpha=0.4),
                    showlegend=TRUE, hoverinfo="skip", #evaluation=TRUE,
                    line=list(width=series.line.width, color=toRGB(series.marker.colors[ggi])))
    }

    # Markers are added as a separate trace to allow overlapping hoverinfo
    for (ggi in 1:length(g.list))
    {
        ind <- which(pos$Group == g.list[ggi])
        p <- add_trace(p, x=pos$x[ind], y=pos$y[ind], type="scatter", mode="markers+lines", fill="none",
                    name=g.list[ggi], legendgroup=g.list[ggi],
                    showlegend=FALSE, hoverinfo="text", text=pos$HoverText[ind],
                    marker=list(size=1, color=toRGB(series.marker.colors[ggi])), line=list(width=0))

        if (data.label.show)
            p <- add_trace(p, x=pos$x[ind]*1.12, y=pos$y[ind]*1.06,
                    type="scatter", mode="text", legendgroup=g.list[ggi],
                    showlegend=FALSE, hoverinfo="none", text=pos$DataLabels[ind],
                    textfont=list(family=data.label.font.family, size=data.label.font.size,
                        color=data.label.font.color))


    }

    # Radial grid lines
    outer <- getPolarCoord(rep(r.max, n))
    grid <- apply(outer, 1, function(zz){
        return(list(type="line", x0=0, y0=0, x1=zz[1], y1=zz[2], layer="below",
                    line=list(width=x.grid.width, color=x.grid.color)))})

    # Hexagonal grid
    for (tt in tick.vals)
    {
        gpos <- getPolarCoord(rep(tt, n))
        for (i in 1:n)
            grid[[length(grid)+1]] <- list(type="line", layer="below",
                 x0=gpos[i,1], x1=gpos[i+1,1], y0=gpos[i,2], y1=gpos[i+1,2],
                 line=list(width=y.grid.width, dash="dot", color=y.grid.color))
    }

    # Positioning of labels
    xanch <- rep("center", n)
    xanch[which(abs(outer[,2]) < r.max/100 & sign(outer[,1]) < 0)] <- "right"
    xanch[which(abs(outer[,2]) < r.max/100 & sign(outer[,1]) > 0)] <- "left"

    xlab <- autoFormatLongLabels(rownames(chart.matrix)[1:n], x.tick.label.wordwrap, wordwrap.nchar)
    font.asp <- switch(tolower(x.title.font.family),
                              'arial'= 0.54,
                              'arial black' = 0.63,
                              'century gothic' = 0.61,
                              'courier new' = 0.63,
                              'impact' = 0.48,
                              'open sans' = 0.45,
                              'times new roman' = 0.45,
                              'tahoma' = 0.52,
                              'trebuchet' = 0.48,
                              'verdana' = 0.63,
                              0.54)

    # X-axis label widths are fixed to avoid the chart width changing in regression tests.
    # We avoided fixing the x-axis range because autorange handles variation in the xaxis labels quite well
    xlab.width <- (font.asp + 0.5) * x.title.font.size * max(nchar(unlist(strsplit(split="<br>", as.character(xlab)))))
    p <- layout(p, title=list(text=title, font=list(family=title.font.family,color=title.font.color,size=title.font.size)),
        margin = list(t=margin.top, b=margin.bottom, l = margin.left, r = margin.right, pad = margin.inner.pad),
        plot_bgcolor = toRGB(charting.area.fill.color, alpha = charting.area.fill.opacity),
        paper_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity),
        hovermode = if (tooltip.show) "closest" else FALSE,
        xaxis2=footer.axis,
        xaxis3=subtitle.axis,
        xaxis=list(title="", showgrid=F, zeroline=F, showticklabels=F,
            categoryorder="array", categoryarray=unique(pos$Group)),
        yaxis=list(title="", showgrid=F, zeroline=F, showticklabels=F,
            domain=c(0, 0.95+(0.05*is.null(subtitle.axis)))),
        legend=list(bgcolor=toRGB(legend.fill.color, alpha=legend.fill.opacity),
            bordercolor=legend.border.color, borderwidth=legend.border.line.width,
            font=list(color=legend.font.color, family=legend.font.family, size=legend.font.size),
            xanchor=legend.x.anchor, yanchor=legend.y.anchor, y=legend.y, x=legend.x,
            traceorder=legend.sort.order), showlegend=legend.show,
        shapes=grid,
        annotations=list(x=outer[,1], y=outer[,2], text=xlab, width=xlab.width,
             showarrow=F, yshift=outer[1:n,2]/r.max * 15,
             font=list(family=x.title.font.family, color=x.title.font.color, size=x.title.font.size),
             xshift=outer[1:n,1]/r.max * 5, xanchor=xanch))

    if (y.grid.width > 0 && y.tick.show && !is.null(tick.vals))
        p <- add_annotations(p, x=rep(0, length(tick.vals)), y=tick.vals, showarrow=F, xanchor="right", xshift=-5,
                text=paste0(y.tick.prefix, FormatAsReal(tick.vals, decimals = y.tick.decimals), y.tick.suffix),
                font=list(family=y.tick.font.family, color=y.tick.font.color, size=y.tick.font.size))

    p <- config(p, displayModeBar=modebar.show)
    p$sizingPolicy$browser$padding <- 0
    return(p)
}

getPolarCoord <- function(r, matrix = T, na = F){
    # Get starting angle and angle increments
    theta <- 0.5 * pi
    dtheta <- -360 / length(r)
    dtheta <- (pi / 180) * dtheta  # in radians

    # Get polar coordinates
    x <- c()
    y <- c()

    for(i in 1:length(r)){

        x <- c(x, r[i] * cos(theta))
        y <- c(y, r[i] * sin(theta))

        theta <- theta + dtheta
    }

    x[length(x) + 1] <- x[1]
    y[length(y) + 1] <- y[1]

    if(na == T){
        x[length(x) + 1] <- NA
        y[length(y) + 1] <- NA
    }


    if(matrix == T){
        return(cbind(x, y))
    }else{
        return(list(x = x,
                    y = y))
    }

}
