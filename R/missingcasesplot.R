#' MissingCasesPlot
#'
#' Show which cases in a data frame are missing
#' @inherit Column
#' @param raw.data Matrix or data frame.
#' @param show.counts.missing Include the number of cases missing in the variable label.
#' @param show.percentages.missing Include the percentage of cases missing in the variable label.
#' @param subset Logical vector indicating whether each row of the data frame should be included
#' @param hovertext.font.color Color of hovertext as a string or hex code.
#' @param x.tick.label.nchar Number of characters wide the x-axis labels should be before wrapping.
#' @param font.unit One of "px" of "pt". By default all font sizes are specified in terms of
#'  pixels ("px"). But changing this to "pt" will mean that the font sizes will be in terms
#'  points ("pt"), which will be consistent with font sizes in text boxes.
#' @param data.label.position One of "center", "above" or "below".
#' @param data.label.bg.opacity Numeric between 0 (tranparent) to 1 (opaque), specifying the opacity of the data label background (grey).
#' @export
MissingCasesPlot <- function(raw.data,
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
    data.label.bg.opacity = 1.0,
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
    x.tick.label.nchar = 20,
    x.tick.angle = NULL,
    y.tick.font.family = global.font.family,
    y.tick.font.color = global.font.color,
    y.tick.font.size = 10,
    hovertext.font.family = global.font.family,
    hovertext.font.color = "#FFFFFF",
    hovertext.font.size = 11,
    font.unit = "px")
{
    dat <- as.matrix(is.na(raw.data) * 1)
    index <- 1:nrow(dat)
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


    if (is.null(colnames(dat)))
        colnames(dat) <- paste("Variable", 1:ncol(dat))
    tmp <- paste("Missing:<b>",rep(colnames(dat), each=nrow(dat)), "</b>case", rep(index, ncol(dat)))
    info.text <- ifelse(dat > 0, tmp, "")

    title.font = list(family = title.font.family, size = title.font.size, color = title.font.color)
    subtitle.font = list(family = subtitle.font.family, size = subtitle.font.size, color = subtitle.font.color)
    y.tick.font = list(family = y.tick.font.family, size = y.tick.font.size, color = y.tick.font.color)
    x.tick.font = list(family = x.tick.font.family, size = x.tick.font.size, color = x.tick.font.color)
    footer.font = list(family = footer.font.family, size = footer.font.size, color = footer.font.color)
    hovertext.font = list(family = hovertext.font.family, size = hovertext.font.size, color = hovertext.font.color)
    data.label.font = list(family = data.label.font.family, size = data.label.font.size, color = data.label.font.color)

    x.labels <- paste0("<b>", colnames(dat), "</b>")
    if (show.counts.missing || show.percentages.missing)
    {
        x.labels <- paste0(x.labels, if (x.tick.label.wrap) "<br>(" else " (")
        if (show.counts.missing)
            x.labels <- paste0(x.labels, colSums(dat), " cases ")
        if (show.counts.missing && show.percentages.missing)
            x.labels <- paste(x.labels, "or")
        if (show.percentages.missing)
            x.labels <- paste0(x.labels, " ", round(colSums(dat)/nrow(dat)*100), "%")
        x.labels <- paste0(x.labels, " missing)")
    }
    x.labels <- autoFormatLongLabels(x.labels, x.tick.label.wrap, x.tick.label.nchar, truncate = FALSE)
    footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate = FALSE)
    xaxis <- list(side = "bottom", ticklen = 0, tickangle = x.tick.angle, tickfont = x.tick.font)
    yaxis <- list(side = "left", ticklen = 0, tickfont = y.tick.font,
                  tickmode = "auto", nticks = min(nrow(dat) + 1, 11))

    margins <- list(t = 20, r = 60, l = 80, b = 20, pad = 0)
    margins <- setMarginsForAxis(margins, x.labels, xaxis)
    margins <- setMarginsForText(margins, title, subtitle, footer, title.font.size,
                                 subtitle.font.size, footer.font.size)

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
            bgcolor = rgb(220, 220, 220, maxColorValue = 255, 
                alpha = data.label.bg.opacity*255),
            xref = "x", yref = "y", showarrow = FALSE, font = data.label.font)
        }
    }

    p <- plot_ly(z = dat, x = x.labels, y = index, type = "heatmap",
                 zmin = 0, zmax = 1, colorscale = list(c('0', 'rgba(0,0,255)'), c('1','rgba(55,0,0)')),
                 hoverinfo = "text", text = info.text, colorscale = FALSE,
                 zsmooth = FALSE, connectgaps = FALSE, showscale = FALSE)

    p <- config(p, displayModeBar = FALSE)
    p$sizingPolicy$browser$padding <- 0
    p <- layout(p, xaxis = xaxis, yaxis = yaxis,
                plot_bgcolor = toRGB("white", alpha = 0),
                paper_bgcolor = toRGB("white", alpha = 0),
                hoverlabel = list(namelength = -1, font = hovertext.font),
                hovermode = "y",
                annotations = annotations,
                margin = margins)
    p
}
