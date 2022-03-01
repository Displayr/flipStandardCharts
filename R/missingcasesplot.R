#' MissingCasesPlot
#'
#' Show which cases in a data frame are missing
#' @inherit Column
#' @param raw.data Matrix or data frame.
#' @param fill.color Color to show the missing value.
#' @param fill.opacity Alpha transparence between 0 and 1 (or \code{NULL}).
#'  By default automatically tries to adjust opacity depending on the number of
#'  missing values. However if there is a very high density of missing values, you may need to manually
#'  reduce the fill opacity to observe differences in saturated regions.
#' @param base.color Background color of chart.
#' @param base.opacity Opacity of background color.
#' @param show.counts.missing Include the number of cases missing in the variable label.
#' @param show.percentages.missing Include the percentage of cases missing in the variable label.
#' @param subset Logical vector indicating whether each row of the data frame should be included.
#'  This vector should be the same length as the number of rows in \code{raw.data}. Missing data
#'  with rows where \code{subset} is \code{false} will not be shown.
#' @param hovertext.font.color Color of hovertext as a string or hex code.
#' @param x.tick.label.wrap.nchar Number of characters wide the x-axis labels should be before wrapping.
#' @param font.unit One of "px" of "pt". By default all font sizes are specified in terms of
#'  pixels ("px"). But changing this to "pt" will mean that the font sizes will be in terms
#'  points ("pt"), which will be consistent with font sizes in text boxes.
#' @param data.label.position One of "center", "above" or "below".
#' @param data.label.bg.opacity Numeric between 0 (tranparent) to 1 (opaque), specifying the opacity of the data label background (grey).
#' @param tooltip.show Logical, indicating whether or not to show a tooltip on hover. The default is off, because
#'  turning on tooltips with large data sets can make the chart slow to render.
#' @param enable.zoom Logical, indicating whether of not to allow the chart to be zoomable.
#' @importFrom verbs SumEachColumn
#' @export
MissingCasesPlot <- function(raw.data,
    fill.color = "#5C9AD3",
    fill.opacity = NULL,
    base.color = "#E6E6E6",
    base.opacity = 1.0,
    show.counts.missing = TRUE,
    show.percentages.missing = FALSE,
    subset = NULL,
    title = "Missing values by case",
    subtitle = "",
    footer = "",
    global.font.family = "Arial",
    global.font.color = "#2C2C2C",
    data.label.show = FALSE,
    data.label.position = "center",
    data.label.font.family = global.font.family,
    data.label.font.color = global.font.color,
    data.label.font.size = 10,
    data.label.bg.opacity = 0.5,
    title.font.family = global.font.family,
    title.font.color = global.font.color,
    title.font.size = 16,
    subtitle.font.family = global.font.family,
    subtitle.font.color = global.font.color,
    subtitle.font.size = 12,
    footer.font.family = global.font.family,
    footer.font.color = global.font.color,
    footer.font.size = 8,
    footer.wrap = TRUE,
    footer.wrap.nchar = 100,
    x.tick.font.family = global.font.family,
    x.tick.font.color = global.font.color,
    x.tick.font.size = 11,
    x.tick.label.wrap = NULL,
    x.tick.label.wrap.nchar = 20,
    x.tick.angle = NULL,
    y.tick.font.family = global.font.family,
    y.tick.font.color = global.font.color,
    y.tick.font.size = 10,
    tooltip.show = FALSE,
    enable.zoom = TRUE,
    hovertext.font.family = global.font.family,
    hovertext.font.color = "#FFFFFF",
    hovertext.font.size = 11,
    margin.top = NULL,
    margin.bottom = NULL,
    margin.left = NULL,
    margin.right = NULL,
    font.unit = "px")
{
    dat <- as.matrix(is.na(raw.data) * 1)
    index <- 1:nrow(dat)
    if (length(subset) > 1 && length(subset) != nrow(dat))
        stop("Filters must be from the same data set as the input variables.")
    if (length(subset) == nrow(dat))
    {
        index <- which(subset)
        dat <- dat[index,]
    }
    if (is.null(x.tick.angle))
        x.tick.angle <- if (ncol(dat) >= 10) 90 else 0
    if (is.null(x.tick.label.wrap))
        x.tick.label.wrap <- x.tick.angle == 0

    # For the other chart types, the font size conversion
    # happens inside flipChart::CChart but MissingCasesPlot is called separately.
    if (tolower(font.unit) %in% c("pt", "point", "points"))
    {
        fsc <- 1.3333
        title.font.size = round(fsc * title.font.size, 0)
        subtitle.font.size = round(fsc * subtitle.font.size, 0)
        footer.font.size = round(fsc * footer.font.size, 0)
        y.tick.font.size = round(fsc * y.tick.font.size, 0)
        x.tick.font.size = round(fsc * x.tick.font.size, 0)
        hovertext.font.size = round(fsc * hovertext.font.size, 0)
        data.label.font.size = round(fsc * data.label.font.size, 0)
    }


    title.font = list(family = title.font.family, size = title.font.size, color = title.font.color)
    subtitle.font = list(family = subtitle.font.family, size = subtitle.font.size, color = subtitle.font.color)
    y.tick.font = list(family = y.tick.font.family, size = y.tick.font.size, color = y.tick.font.color)
    x.tick.font = list(family = x.tick.font.family, size = x.tick.font.size, color = x.tick.font.color)
    footer.font = list(family = footer.font.family, size = footer.font.size, color = footer.font.color)
    hovertext.font = list(family = hovertext.font.family, size = hovertext.font.size, color = hovertext.font.color)
    data.label.font = list(family = data.label.font.family, size = data.label.font.size, color = data.label.font.color)

    if (is.null(colnames(dat)))
        colnames(dat) <- paste("Variable", 1:ncol(dat))
    x.labels <- paste0("<b>", colnames(dat), "</b>")
    if (show.counts.missing || show.percentages.missing)
    {
        x.labels <- paste0(x.labels, if (x.tick.label.wrap) "<br>(" else " (")
        if (show.counts.missing)
            x.labels <- paste0(x.labels, SumEachColumn(dat, remove.rows = NULL, remove.missing = FALSE), " cases ")
        if (show.counts.missing && show.percentages.missing)
            x.labels <- paste(x.labels, "or ")
        if (show.percentages.missing)
            x.labels <- paste0(x.labels, round_half_up(SumEachColumn(dat, remove.rows = NULL, remove.missing = FALSE)/nrow(dat)*100), "%")
        x.labels <- paste0(x.labels, " missing)")
    }
    x.labels <- autoFormatLongLabels(x.labels, x.tick.label.wrap, x.tick.label.wrap.nchar, truncate = FALSE)
    footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate = FALSE)
    xaxis <- list(side = "bottom", ticklen = 0, tickangle = x.tick.angle, tickfont = x.tick.font,
                  tickvals = 0:(ncol(dat)-1), ticktext = x.labels,
                  showgrid = FALSE, zeroline = FALSE, fixedrange = !enable.zoom)
    yaxis <- list(side = "left", ticklen = 0, tickfont = y.tick.font, range = rev(range(index)),
                  tickmode = "auto", nticks = min(nrow(dat) + 1, 11),
                  showgrid = FALSE, zeroline = FALSE, fixedrange = !enable.zoom)

    margins <- list(t = 20, r = 60, l = 80, b = 20, pad = 0)
    margins <- setMarginsForAxis(margins, x.labels, xaxis)
    margins <- setMarginsForText(margins, title, subtitle, footer, title.font.size,
                                 subtitle.font.size, footer.font.size)
    margins <- setCustomMargins(margins, margin.top, margin.bottom, margin.left, margin.right, 0)

    annotations <- NULL
    annotations[[1]] <- setTitle(title, title.font, margins)
    annotations[[2]] <- setFooter(footer, footer.font, margins)
    annotations[[3]] <- setSubtitle(subtitle, subtitle.font, margins)
    annotations <- Filter(Negate(is.null), annotations)

    n <- length(annotations)
    if (data.label.show)
    {
        ind <- which(dat > 0, arr.ind = TRUE)
        for (ii in 1:nrow(ind))
        {
            ypos <- index[ind[ii,1]]
            annotations[[n+ii]] <- list(text = ypos, x = ind[ii,2] - 1, y = ypos,
            yanchor = switch(tolower(data.label.position), above = "bottom",
                below = "top", "center"),
            bgcolor = rgb(t(col2rgb(base.color)), maxColorValue = 255,
                alpha = data.label.bg.opacity*255),
            xref = "x", yref = "y", showarrow = FALSE, font = data.label.font)
        }
    }

    # Avoid strange color scale if the range is not (0,1)
    base.col.alpha <- rgb(t(col2rgb(base.color)), maxColorValue = 255, alpha = 255 * base.opacity)
    if (all(dat == 0))
        fill.color <- base.col.alpha
    if (all(dat == 1))
        base.col.alpha <- fill.color

    if (is.null(fill.opacity) || is.na(fill.opacity))
    {
        if (max(SumEachColumn(dat, remove.rows = NULL, remove.missing = FALSE)) > 1000)
            fill.opacity <- 0.2
        else if (max(SumEachColumn(dat, remove.rows = NULL, remove.missing = FALSE)) > 200)
            fill.opacity <- 0.5
        else
            fill.opacity <- 1.0
    }
    fill.col.alpha <- rgb(t(col2rgb(fill.color)), maxColorValue = 255, alpha = 255 * fill.opacity)

    # Main plot
    if (nrow(dat) > 200)
    {
        p <- plot_ly(x = c(-0.5, ncol(dat)-0.5), y = range(index),
                type = "scatter", mode = "none")
        line.obj <- list(width = 0.5, color = fill.col.alpha)

    } else
    {
        # If there is only a small number of points use heatmap to ensure space is filled
        p <- plot_ly(z = dat, x = (1:ncol(dat)) - 1, y = index, type = "heatmap",
                 colors = c(base.col.alpha, fill.col.alpha),
                 zmin = 0, zmax = 1, hoverinfo = "skip",
                 zsmooth = FALSE, connectgaps = FALSE, showscale = FALSE)
        line.obj <- list(width = 1, color = fill.col.alpha)
    }

    # Add lines in case heatmap does not show missing values.
    # If tooltips are shown, an extra data point is added
    # so that hover responds to the middle of the line
    na.ind <- which(dat > 0, arr.ind = TRUE)
    if (nrow(na.ind) > 0)
    {
        pt.xpos <- if (tooltip.show) c(-1.5, -1, -0.5, NA) else c(-1.5, -0.5, NA)
        pt.ypos <- if (tooltip.show) c(0, 0, 0, NA) else c(0, 0, NA)
        pt.len <- length(pt.xpos)
        xvals <- rep(na.ind[,2], each = pt.len) + pt.xpos
        yvals <- rep(index[na.ind[,1]], each = pt.len) + pt.ypos
        hover.text <- NULL
        if (tooltip.show)
            hover.text <- rep(autoFormatLongLabels(paste("Case", index[na.ind[,1]], "missing from",
                    paste0("<b>", colnames(dat)[na.ind[,2]], "</b>")), TRUE, 50), each = pt.len)
        hover.mode <- if (tooltip.show) "text" else "skip"

        p <- add_trace(p, x = xvals, y = yvals, line = line.obj,
                hoverinfo = hover.mode, text = hover.text,
                type = "scatter", mode = "lines", showlegend = FALSE,
                z = NULL, zmin = NULL, zmax = NULL, zsmooth = NULL, showscale = NULL)
    }

    p <- config(p, displayModeBar = FALSE)
    p$sizingPolicy$browser$padding <- 0
    p <- layout(p, xaxis = xaxis, yaxis = yaxis,
                plot_bgcolor = toRGB(base.color),
                paper_bgcolor = toRGB("white", alpha = 0),
                hoverlabel = list(namelength = -1, font = hovertext.font,
                    bordercolor = "transparent", bgcolor = rgb(0.05,0.05,0.05, alpha = 0.8)),
                hovermode = if (tooltip.show) "closest" else FALSE,
                dragmode = if (enable.zoom) "zoom" else FALSE,
                annotations = annotations,
                margin = margins)
    p
}
