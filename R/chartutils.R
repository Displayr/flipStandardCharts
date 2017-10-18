setHoverText <- function(axis, chart.matrix, is.bar = FALSE)
{
    formatStr <- if (axis$type == "linear") "x+y"
                 else                       "text+y"
    if (is.bar && axis$type != "linear")
        formatStr <- "text+x"

    if (ncol(chart.matrix) > 1)
        formatStr <- paste0("name+", formatStr)
}



minPosition <- function(x, n)
{
    if (is.factor(x) || is.character(x))
        return(rep(0, n))
    else
        return(rep(min(x, na.rm=T), n))
}


checkMatrixNames <- function(x)
{
    x <- as.matrix(x)
    if (is.null(rownames(x)))
        rownames(x) <- 1:nrow(x)
    if (is.null(colnames(x)))
        colnames(x) <- sprintf("Series %d", 1:ncol(x))
    if (any(duplicated(rownames(x))))
        stop("Row names of the input table must be unique.")
    return(x)
}

checkTableList <- function(y, trend.lines)
{
    num.tables <- length(y)
    y.names <- rep("", num.tables)
    used.names <- c()
    for (i in 1:num.tables)
    {
        if (is.null(attr(y[[i]], "name")))
        {
            attr(y[[i]], "name") <- as.character(i)
            used.names <- c(used.names, i)
        }
        y.names[i] <- attr(y[[i]], "name")[1]
    }
    if (!trend.lines && length(used.names) > 0)
        warning(sprintf("Tables have been automatically assigned names '%s'. You can name tables using R code: 'attr(table.name, \"name\") <- \"Description\"'", paste(used.names, collapse="', '")))
    if (any(duplicated(y.names)) & !trend.lines)
        warning(sprintf("Tables have duplicate names: '%s'. Points from duplicated tables cannot be distinguised.", paste(y.names[duplicated(y.names)], collapse = "', '")))

    # Check tables match - order of rows will match first table
    r.names <- rownames(y[[1]])
    c.names <- colnames(y[[1]])
    if (!is.null(r.names) && any(duplicated(r.names)) && length(y) > 1)
        stop("Row names of tables must be unique or NULL for multiple tables to be plotted but are duplicated.")

    if (num.tables > 1) {
        for (i in 2:num.tables)
        {
            if (!setequal(r.names, rownames(y[[i]])))
                stop(sprintf("Tables should have identical row names but table '%s' differs from table '%s'.",
                             y.names[i], y.names[1]))
            if (!setequal(c.names, colnames(y[[i]])))
                stop(sprintf("Tables should have identical column names but table '%s' differs from table '%s'.",
                             y.names[i], y.names[1]))
            if (!is.null(r.names))
                y[[i]] <- y[[i]][r.names, ]
            if (!is.null(c.names))
                y[[i]] <- y[[i]][, c.names]
        }
    }
    return(y)
}


getRange <- function(x, axis, axisFormat)
{
    range <- axis$range
    if (is.null(range))
    {
        if (!is.null(axisFormat$ymd))
        {
            tmp.dates <- as.numeric(axisFormat$ymd) * 1000
            diff <- min(diff(tmp.dates), na.rm=T)
            range <- range(tmp.dates) + c(-1, 1) * diff
        }
        else if (is.numeric(x))
            range <- range(x) + c(-0.5, 0.5)
        else if (all(!is.na(as.numeric(x))))
            range <- range(as.numeric(x)) + c(-0.5, 0.5)
        else
            range <- c(-0.5, length(x)-0.5)

        if (axis$autorange == "reversed")
            range <- rev(range)
    }
    range
}

fitSeries <- function(x, y, fit.type, ignore.last, axis.type)
{
    tmp.is.factor <- axis.type != "linear" #&& axis.type != "date"
    x0 <- if (!tmp.is.factor) as.numeric(x) else 1:length(x)
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

setLegend <- function(type, font, ascending, fill.color, fill.opacity, border.color, border.line.width,
                      x.pos=1.02, y.pos=1.00)
{
    if (is.na(ascending))
        ascending <- !grepl("Stacked", type) || grepl("Stacked Bar", type)
    order <- if (!ascending) "reversed" else "normal"
    return(list(bgcolor = toRGB(fill.color, alpha=fill.opacity),
            bordercolor = border.color,
            borderwidth = border.line.width,
            orientation = 'v',
            font = font,
            xanchor = "left",
            yanchor = "auto",
            y = y.pos,
            x = x.pos,
            traceorder = order))
}

getAxisType <- function(labels, us.date.format)
{
    type <- "linear"
    if (any(is.na(suppressWarnings(as.numeric(labels)))))
        type <- "category"
    ymd <- PeriodNameToDate(labels, us.format = us.date.format)
    if ((length(labels) > 6 && !any(is.na(ymd))) || class(labels) %in% c("Date", "POSIXct", "POSIXt"))
        type <- "date"
    return(type)
}


formatLabels <- function(dat, type, label.wrap, label.wrap.nchar, us.date.format)
{
    is.bar <- grepl("Bar", type, fixed=T)
    if (is.matrix(dat))
    {
        x.labels <- rownames(dat)
        y.labels <- NULL
    }
    else
    {
        x.labels <- unique(dat[[1]])
        y.labels <- unique(dat[[2]])
    }

    if (!is.bar)
    {
        x.axis.type <- getAxisType(x.labels, us.date.format)
        y.axis.type <- getAxisType(y.labels, us.date.format)

    } else
    {
        x.axis.type <- getAxisType(y.labels, us.date.format)
        y.axis.type <- getAxisType(x.labels, us.date.format)
    }

    # labels are only processed for independent x-axis (or y-axis in bar charts)
    # the other axis is always numeric
    labels <- x.labels
    ymd <- PeriodNameToDate(labels, us.format = us.date.format)
    if ((!any(is.na(ymd)) && length(labels) > 6) || class(labels) %in% c("Date", "POSIXct", "POSIXt"))
    {
        use.dates <- TRUE
        if (all(!is.na(ymd)))
            labels <- ymd
    }
    else
    {
        ymd <- NULL
        if (is.character(labels))
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

    if (!is.null(labels) && !is.na(labels) && any(nchar(labels) > 0) &&
         is.null(tickangle) && side %in% c("bottom", "top"))
    {
        lab.nchar <- max(c(0, nchar(unlist(strsplit(split="<br>", as.character(labels))))))
        tickangle <- if (length(labels) > 9 && lab.nchar > 5) 90
                          else 0
    }

    autorange <- ticks$autorange
    range <- ticks$range
    if ((!axisLabels$labels.on.x) && side %in% c("left","right"))
    {
        autorange <- FALSE
        if (axis.type == "date")
        {
            tmp.dates <- as.numeric(axisLabels$ymd) * 1000
            diff <- min(diff(tmp.dates), na.rm=T)
            range <- rev(range(tmp.dates, na.rm=T)) + c(1, -1) * diff
        }
        else if (axis.type == "linear")
            range <- rev(range(as.numeric(axisLabels$labels))) + c(0.5, -0.5)
        else
            range <- c(length(axisLabels$labels)-0.5, -0.5)
    }
    else if (axis.type == "date" && !is.null(axisLabels$ymd))
    {
        autorange <- FALSE
        tmp.dates <- as.numeric(axisLabels$ymd) * 1000
        diff <- min(diff(tmp.dates), na.rm=T)
        range <- range(tmp.dates, na.rm=T) + c(-1, 1) * diff
    }

    tickformat <- ""
    if (axis.type == "linear" && nchar(tickformatmanual) == 0)
        tickformat <- paste0(".", tickdecimals, "f")
    if (axis.type == "date" && nchar(tickformatmanual) > 0 &&
        substr(tickformatmanual, nchar(tickformatmanual), nchar(tickformatmanual)) %in% c("e", "f", "%", "s"))
        tickformat <- ""
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
                 autorange=autorange, range=range, rangemode=rangemode, layer="below traces",
                 zeroline=zero.line.width > 0, zerolinewidth=zero.line.width, zerolinecolor=zero.line.color,
                 showexponent="all", showtickprefix=TRUE, showticksuffix=TRUE, showticklabels=tickshow))
}

fontAspectRatio <- function(font)
{
    if (length(font) == 0)
        return (0.54)
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


setMarginsForAxis <- function(margins, labels, axis)
{
    if (!is.character(labels) && !is.null(labels$labels))
        labels <- labels$labels
    lab.len <- 0
    lab.nline <- 0
    lab.nchar <- 1

    lab.nchar <- max(c(0,nchar(unlist(strsplit(split="<br>", as.character(labels))))))
    font.asp <- fontAspectRatio(axis$tickfont$family)
    lab.len <- font.asp * axis$tickfont$size * lab.nchar * 1.25
    lab.nline <- if (is.character(labels)) max(sapply(gregexpr("<br>", labels),
                     function(x){sum(x > -1)}))
                 else 0

    new.margin <- 0
    if (lab.len > 2 || (!is.null(lab.nline) && lab.nline > 0))
        new.margin <- lab.len

    title.nline <- 0
    if (nchar(axis$title) > 0)
        title.nline <- sum(gregexpr("<br>", axis$title)[[1]] > -1) + 1
    title.pad <- axis$titlefont$size * title.nline * 1.25

    if (axis$side == "right")
        margins$r <- max(margins$r, new.margin + title.pad)
    else if (axis$side == "left")
        margins$l <- max(margins$l, new.margin + title.pad)
    else if (axis$side == "bottom")
    {
        # tickangle is changed in function setAxis
        lab.nchar <- max(c(0,nchar(unlist(strsplit(split="<br>", as.character(labels))))))
        if (is.null(axis$tickangle))
            axis$tickangle <- 0
        if (axis$tickangle != 0)
            margins$b <- margins$b + new.margin * 0.5 + title.pad
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
    if (!showlegend && legend$x > 1)
        margins$r <- 20
    margins
}

# No wordwrap - subtitles should not be too long
setSubtitleAxis <- function(subtitle, subtitle.font, title, title.font, overlaying = "x")
{
    res <- NULL
    if (nchar(subtitle) > 0)
    {
        title.nline <- sum(gregexpr("<br>", title)[[1]] > -1) + 1
        subtitle.npad <- max(0, round(title.nline * subtitle.font$size/title.font$size * 0.9))
        subtitle <- paste0(paste(rep("<br>", subtitle.npad), collapse=""), subtitle)
        res <- list(overlaying = overlaying, side="top", anchor="free", position = 1,
             showline = F, zeroline = F, showgrid = F, showticklabels = F, title = subtitle,
             domain = c(0, 1),
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
    if ((is.null(minimum) || is.null(maximum)) && !is.null(distance))
        stop("If specifying the distance between ticks on an axis, you must also specify the minimum and maximum values.")
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
        is.bar <- grepl("Bar", type) && !grepl("Stacked", type)
        if (is.null(minimum))
            minimum <- min(0, min(data))
        if (is.null(maximum))
            maximum <- max(data)

        # Add horizontal space for data labels
        pad <- 0
        lab.len <- 1
        if (!is.null(labels))
        {
            lab.len <- max(nchar(as.character(unlist(labels)))) * label.font.size/10
            pad <- (maximum - minimum) * (0.05 * lab.len/4 + (0.1 * is.bar))
        }
        if (!is.bar || min(data) < 0)
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
