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
#' @export
MissingCasesPlot <- function(raw.data,
    subset = NULL,
    title = "Missing values by case",
    subtitle = "",
    footer = "",
    global.font.family = "Arial",
    global.font.color = "#2C2C2C",
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
    x.tick.label.wrap = TRUE,
    x.tick.label.nchar = 20,
    x.tick.angle = 0,
    y.tick.font.family = global.font.family,
    y.tick.font.color = global.font.color,
    y.tick.font.size = 10,
    hovertext.font.family = global.font.family,
    hovertext.font.color = "#FFFFFF",
    hovertext.font.size = 11, 
    show.counts.missing = FALSE,
    show.percentages.missing = FALSE)
{
    dat <- as.matrix(is.na(raw.data) * 1)
    index <- 1:nrow(dat)
    if (length(subset) == nrow(dat))
    {
        index <- which(subset)
        dat <- dat[index,]
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

    x.labels <- paste0("<b>", colnames(dat), "</b>")
    if (show.counts.missing || show.percentages.missing)
    {
        x.labels <- paste0(x.labels, "<br>(")
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
    yaxis <- list(side = "left", ticklen = 0, tickfont = y.tick.font, tick0 = 1, 
                dtick = max(1, floor(nrow(dat)/100)*10))
    
    margins <- list(t = 10, r = 0, l = 10, b = 10, pad = 0)
    margins <- setMarginsForAxis(margins, x.labels, xaxis)
    margins <- setMarginsForText(margins, title, subtitle, footer, title.font.size,
                                 subtitle.font.size, footer.font.size)

    annotations <- NULL
    annotations[[1]] <- setTitle(title, title.font, margins)
    annotations[[2]] <- setFooter(footer, footer.font, margins)
    annotations[[3]] <- setSubtitle(subtitle, subtitle.font, margins)
    annotations <- Filter(Negate(is.null), annotations)

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
                annotations = annotations,
                margin = margins)
    p
}
