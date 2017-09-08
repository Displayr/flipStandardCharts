getRange <- function(x, positions, axis, axisFormat)
{
    range <- axis$range
    if (is.null(range))
    {
        range <- if (is.numeric(x) || !is.null(axisFormat$ymd)) range(positions)
                 else c(-0.5, length(x)-0.5)

        if (!is.null(axisFormat$ymd))
        {
            tmpd <- diff(sort(positions))[1] * 0.5
            range <- range + c(-tmpd, tmpd)
        }
        ignored <- axis$side %in% c("left", "right") && !is.null(axisFormat$ymd)
        if (axis$autorange == "reversed" && !ignored)
            range <- rev(range) 
    }
    range
}

fitSeries <- function(x, y, fit.type, ignore.last, axis.type)
{
    tmp.is.factor <- axis.type != "linear" && axis.type != "date"
    x0 <- if (!tmp.is.factor) x else 1:length(x)
    tmp.dat <- data.frame(x=x0, y=y)
    if (ignore.last)
        tmp.dat <- tmp.dat[-which.max(tmp.dat$x),]
    tmp.fit <- if (fit.type == "Smooth" && nrow(tmp.dat) > 7) loess(y~I(as.numeric(x)), data=tmp.dat)
               else lm(y~x, data=tmp.dat)

    x.fit <- if (tmp.is.factor) x0
             else seq(from = min(x), to = max(tmp.dat$x), length = 100)
    if (!tmp.is.factor && max(x.fit) < max(tmp.dat$x))
        x.fit <- c(x.fit, max(tmp.dat$x))
    y.fit <- predict(tmp.fit, data.frame(x = x.fit))
    if (tmp.is.factor)
        x.fit <- x
    return(list(x=x.fit, y=y.fit))
}

setLegend <- function(type, font, ascending, fill.color, fill.opacity, border.color, border.line.width)
{
    if (is.na(ascending))
        ascending <- !grepl("Stacked", type) || type %in% c("Stacked Bar", "100% Stacked Bar")
    order <- if (!ascending) "reversed" else "normal"
    return(list(bgcolor = toRGB(fill.color, alpha=fill.opacity),
            bordercolor = border.color,
            borderwidth = border.line.width,
            font = font,
            xanchor = "left",
            yanchor = "auto",
            y = 1,
            x = 1.02,
            traceorder = order))
}



formatLabels <- function(chart.matrix, type, label.wrap, label.wrap.nchar, us.date.format)
{
    is.bar <- type %in% c("Bar", "Stacked Bar", "100% Stacked Bar")
    x.labels <- rownames(chart.matrix)
    y.labels <- colnames(chart.matrix)

    y.axis.type <- "linear"
    x.axis.type <- "linear"
    if (!is.bar && any(is.na(suppressWarnings(as.numeric(x.labels)))))
        x.axis.type <- "category"
    if (is.bar && any(is.na(suppressWarnings(as.numeric(x.labels)))))
        y.axis.type <- "category"
    
    # labels are only processed for independent x-axis (or y-axis in bar charts)
    # the only axis is always numeric
    labels <- x.labels
    ymd <- PeriodNameToDate(labels, us.format = us.date.format)
    if (!any(is.na(ymd)) && length(labels) > 6)
    {
        use.dates <- TRUE
        labels <- ymd
        if (is.bar)
            y.axis.type <- "date"
        else
            x.axis.type <- "date"
    }
    else
    {
        ymd <- NULL
        labels <- autoFormatLongLabels(labels, label.wrap, label.wrap.nchar)
    }
    return(list(ymd=ymd, labels=labels, labels.on.x=!is.bar, 
                x.axis.type=x.axis.type, y.axis.type=y.axis.type))
}

setAxis <- function(title, side, axisLabels, titlefont, linecolor, linewidth, gridwidth, gridcolor,
                    ticks, tickfont, tickangle, ticklen, tickdistance,
                    tickformatmanual, tickdecimals, tickprefix, ticksuffix, tickshow,
                    show.zero, zero.line.width, zero.line.color, 
                    hovertext.format.manual, hovertext.decimals, labels=NULL)
{
    axis.type <- if (side %in% c("bottom", "top")) axisLabels$x.axis.type else axisLabels$y.axis.type
    has.line <- !is.null(linewidth) && linewidth > 0

    if (!is.null(labels) && is.null(tickangle) && side %in% c("bottom", "top"))
    {
        lab.nchar <- max(nchar(unlist(strsplit(split="<br>", as.character(labels)))))
        tickangle <- if (length(labels) > 9 && lab.nchar > 5) 90
                          else 0
    }

    tickformat <- ""
    if (axis.type == "linear" && nchar(tickformatmanual) == 0)
        tickformat <- paste0(".", tickdecimals, "f")
    else
        tickformat <- tickformatmanual
    
    hoverformat <- ""
    if (!is.null(hovertext.format.manual) && nchar(hovertext.format.manual))
        hoverformat <- hovertext.format.manual
    else if (axis.type == "date")
        hoverformat <- ""
    else
        hoverformat <- paste0(".", hovertext.decimals, "f")
    
    rangemode <- "normal"
    if (axis.type == "linear" && show.zero)
        rangemode <- "tozero"

    return (list(title=title, side=side, type=axis.type, titlefont=titlefont, tickfont=tickfont, 
                 showline=has.line, linecolor=linecolor, linewidth=if (has.line) NULL else linewidth, 
                 showgrid=gridwidth > 0, gridwidth=gridwidth, gridcolor=gridcolor,
                 tickmode=ticks$mode, tickvals=ticks$tickvals, ticktext=ticks$ticktext,
                 ticks=if (has.line) "outside" else "", tickangle=tickangle, ticklen=ticklen,
                 tickcolor=linecolor, tickfont=tickfont, dtick=tickdistance, tickformat=tickformat,
                 tickprefix=tickprefix, ticksuffix=ticksuffix, hoverformat=hoverformat,
                 autorange=ticks$autorange, range=ticks$range, rangemode=rangemode,
                 zeroline=zero.line.width > 0, zerolinewidth=zero.line.width, zerolinecolor=zero.line.color,
                 showexponent="all", showtickprefix=TRUE, showticksuffix=TRUE, showticklabels=tickshow))
}

fontAspectRatio <- function(font)
{
    font <- as.character(font)
    font.asp <- switch(tolower(font),
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
    font.asp
}


setMarginsForAxis <- function(margins, axisLabels, axis)
{
    labels <- axisLabels$labels
    lab.nchar <- max(nchar(unlist(strsplit(split="<br>", as.character(labels)))))
    font.asp <- fontAspectRatio(axis$tickfont$family)
    lab.len <- font.asp * axis$tickfont$size * lab.nchar
    lab.nline <- if (is.character(labels)) max(sapply(gregexpr("<br>", labels),
                     function(x){sum(x > -1)}))
                 else 0

    new.margin <- 0
    if (lab.len > 50 || (!is.null(lab.nline) && lab.nline > 0))
        new.margin <- lab.len

    title.nline <- 0
    if (nchar(axis$title) > 0)
        title.nline <- sum(gregexpr("<br>", axis$title)[[1]] > -1) + 1
    title.pad <- axis$titlefont$size * title.nline * 1.25 + 5

    if (axis$side == "right")
        margins$r <- new.margin + title.pad 
    else if (axis$side == "left")
        margins$l <- new.margin + title.pad 
    else if (axis$side == "bottom")
    {
        # tickangle is changed in side setAxis
        lab.nchar <- max(nchar(unlist(strsplit(split="<br>", as.character(labels)))))
        tickangle <- if (length(labels) > 9 && lab.nchar > 5) 90
                          else 0
        
        if (tickangle != 0)
            margins$b <- margins$b + new.margin - 40 + title.pad
        else
            margins$b <- margins$b + 1.25*axis$tickfont$size*floor(lab.nline) + title.pad
    }
    return(margins)
}

setMarginsForText <- function(margins, title, subtitle, footer, 
                        title.font.size, subtitle.font.size, footer.font.size)
{
    title.nline <- 0
    if (nchar(title) > 0)
    {
        title.nline <- sum(gregexpr("<br>", title)[[1]] > -1) + 1
        margins$t <- margins$t + (title.font.size * title.nline * 1.25)
    }
    if (nchar(subtitle) > 0)
    {
        subtitle.nline <- sum(gregexpr("<br>", subtitle)[[1]] > -1) + 1.5
        margins$t <- margins$t + (subtitle.font.size * subtitle.nline * 1.25)
    }
    if (nchar(footer) > 0)
    {
        footer.nline <- sum(gregexpr("<br>", footer)[[1]] > -1) + 2
        margins$b <- margins$b + (footer.font.size * footer.nline * 1.25)
    }
    margins
}

setMarginsForLegend <- function(margins, showlegend, legend)
{
    if (!showlegend)
        margins$r <- 20
    margins
}

# No wordwrap - subtitles should not be too long
setSubtitleAxis <- function(subtitle, subtitle.font, title, title.font)
{
    res <- NULL
    if (nchar(subtitle) > 0)
    {
        title.nline <- sum(gregexpr("<br>", title)[[1]] > -1) + 1
        subtitle.npad <- max(0, round(title.nline * subtitle.font$size/title.font$size * 0.9))
        subtitle <- paste0(paste(rep("<br>", subtitle.npad), collapse=""), subtitle)
        res <- list(overlaying="x", side="top", anchor="free", position=1,
             showline=F, zeroline=F, showgrid=F, showticklabels=F, title=subtitle,
             titlefont=subtitle.font)
    }
    res
}

# footer.font and margins are lists
# footer.font = list(family, size, color)
# margins = list(top, bottom, left, right, inner)
setFooterAxis <- function(footer, footer.font, margins)
{
    res <- NULL
    if (nchar(footer) > 0)
    {
        footer.nline <- sum(gregexpr("<br>", footer)[[1]] > -1) + 1
        footer.npad <- max(0, ceiling(margins$b/footer.font$size/1.25) - footer.nline - 2) 
        footer <- paste0(paste(rep("<br>", footer.npad), collapse=""), footer)
        res <- list(overlaying="x", side = "bottom", anchor="free", position=0,
             visible=T, showline=F, zeroline=F, showgrid=F, tickfont=footer.font,
             range=c(0,1), tickvals=c(0.5), ticktext=c(footer), tickangle=0)
    }
    res
}

setTicks <- function(minimum, maximum, distance, reversed = FALSE, 
                data = NULL, labels = NULL, type="scatter", label.font.size = 10)
{
    # starting values
    mode <- "auto"
    range <- NULL
    tickvals <- NULL
    ticktext <- NULL
    autorange <- TRUE

    if (reversed)
        autorange <- "reversed"

    if (!is.null(data))
    {
        if (is.null(minimum))
            minimum <- min(0, min(data))
        if (is.null(maximum))
            maximum <- max(data)

        pad <- 0
        lab.len <- 1
        if (!is.null(labels))
        {
            lab.len <- max(nchar(as.character(unlist(labels)))) * label.font.size/10
            pad <- (maximum - minimum) * (0.05 * lab.len/4 + (0.1 * (type=="Bar")))
        }
        if (type != "Bar")
            minimum <- minimum - pad
        maximum <- maximum + pad
    }

    if (!is.null(minimum) && !is.null(maximum))
    {
        autorange <- FALSE
        range <- c(minimum, maximum)
    }
    if (!is.null(distance))
    {
        # error msg if axis is not numeric
        autorange <- FALSE
        mode <- "array"
        tickvals <- seq(minimum, maximum, by=distance)
    }
    return (list(autorange=autorange, mode=mode, range=range, 
                 tickvals=tickvals, ticktext=ticktext))
}



## Takes a matrix, and returns a matrix of either a cumulative sum,
## or a cumulative sum of percentages, over each row.
cum.data <- function(x, output = "cumulative.percentage")
{
    result <- if (output == "cumulative.sum")
        apply(x, 1, function(z) cumsum(z))
    else if (output == "cumulative.percentage")
        apply(x, 1, function(z) cumsum(prop.table(z)))
    else if (output == "column.percentage")
        apply(x, 1, function(z) prop.table(z))
    t(result)
}

## Takes a single string and puts <br> in place of the closest space preceding the n= value character.
## E.g. if n= 20 then count 20 characters.  The space preceding character 20 is replaced by "<br>".
lineBreakEveryN <- function(x, n = 21)
{
    if (n <= 0)
        stop("Wrap line length cannot be smaller than 1")

    w.list <- strsplit(x, " ")[[1]]
    final <- w.list[1]
    c.len <- nchar(final)
    for (ww in w.list[-1])
    {
        new.len <- c.len + nchar(ww) + 1
        if (new.len > n)
        {
            final <- paste0(final, "<br>", ww)
            c.len <- nchar(ww)
        } else
        {
            final <- paste0(final, " ", ww)
            c.len <- new.len
        }
    }
    final
}

autoFormatLongLabels <- function(x, wordwrap = FALSE, n = 21, truncate = TRUE)
{
    output.text <- if (truncate) output.text <- sapply(x, function(x)
                        {ifelse(nchar(x) > 60, paste(substr(x, 1, 57),"...", sep = ""), x)})
                   else x

    if (wordwrap && length(output.text) > 0)
        output.text <- sapply(output.text, function(x) lineBreakEveryN(x, n))
    if (is.na(output.text) || length(output.text) == 0)
        output.text <- ""

    attributes(output.text) <- NULL
    output.text
}

stripClassAndCallFromXtabs <- function(chart.matrix)
{
    if (class(chart.matrix) == "xtabs" || class(chart.matrix) == "table")
    {
        attr(chart.matrix, "class") <- NULL
        attr(chart.matrix, "call") <- NULL
        return(chart.matrix)
    }
    else
        return(chart.matrix)
}

decimalsToDisplay <- function(x)
{
    mx <- max(x, na.rm = TRUE)
    mn <- min(x, na.rm = TRUE)
    rng <- max(c(mx - mn, abs(mx), abs(mn)))
    if (!is.na(rng) && rng > 0)
        max(-floor(log10(rng)) + 1, 0)
    else
        NULL
}
