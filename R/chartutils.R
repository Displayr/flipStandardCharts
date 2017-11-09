#' ErrorIfNotEnoughData
#'
#' Returns an error if there is not enough data for charting/
#' @param x The data to be plotted.
#' @param require.tidy The data is assumed to be a numeric vector, matrix, array, or data frame.
#' @export
ErrorIfNotEnoughData <- function(x, require.tidy = TRUE)
{
    .stop <- function()
        { stop("There is not enough data to create a plot.") }
    .possiblyTidy <- function(x)
        {is.numeric(x) || is.matrix(x) || is.data.frame(x) || is.array(x)}
    .noData <- function(x)
        {NROW(x) == 0 || NCOL(x) == 0 || all(is.na(x))}

    if (!require.tidy && is.list(x))
        x <- x[[1]]
    if (require.tidy && !.possiblyTidy(x))
        stop("The data is not in an appropriate format.")
    if (.noData(x))
        .stop()
}


setHoverText <- function(axis, chart.matrix, is.bar = FALSE)
{
    formatStr <- if (axis$type == "category") "text+y"
                 else                         "x+y"
    if (is.bar && axis$type == "category")
        formatStr <- "text+x"

    if (ncol(chart.matrix) > 1)
        formatStr <- paste0("name+", formatStr)
    
    return(formatStr)
}



minPosition <- function(x, n)
{
    if (is.factor(x) || is.character(x))
        return(rep(x[1], n))
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
    range <- NULL
    if (!is.null(axis))
        range <- axis$range
    if (is.null(range))
    {
        if (!is.null(axisFormat) && !is.null(axisFormat$ymd))
        {
            tmp.dates <- as.numeric(axisFormat$ymd) * 1000
            diff <- min(diff(tmp.dates), na.rm=T)
            range <- range(tmp.dates) + c(-1, 1) * diff
        }
        else if (is.numeric(x))
            range <- range(x) + c(-0.5, 0.5)
        else if (all(!is.na(suppressWarnings(as.numeric(x)))))
            range <- range(as.numeric(x)) + c(-0.5, 0.5)
        else if (all(!is.na(AsDate(x, on.parse.failure = "silent"))))
            range <- range(AsDate(x))
        else
            range <- c(-0.5, length(x)-0.5)

        if (!is.null(axis) && axis$autorange == "reversed")
            range <- rev(range)
    }
    range
}

fitSeries <- function(x, y, fit.type, ignore.last, axis.type)
{
    if (!is.numeric(y))
    {
        warning("Line of best fit cannot handle non-numeric y-values.")
        return(list(x = NULL, y = NULL))
    }

    tmp.is.factor <- axis.type != "numeric" 
    x0 <- if (!tmp.is.factor) as.numeric(x) else 1:length(x)
    tmp.dat <- data.frame(x=x0, y=y)
    if (ignore.last)
        tmp.dat <- tmp.dat[-which.max(tmp.dat$x),]
    if (nrow(tmp.dat) < 2)
    {
        warning("Not enough data to constuct line of best fit.")
        return(list(x = NULL, y = NULL))
    }

    tmp.fit <- if (fit.type == "Smooth" && nrow(tmp.dat) > 7) loess(y~I(as.numeric(x)), data=tmp.dat)
               else lm(y~x, data=tmp.dat)

    x.fit <- if (tmp.is.factor) x0
             else seq(from = min(tmp.dat$x), to = max(tmp.dat$x), length = 100)
    if (!tmp.is.factor && max(x.fit) < max(tmp.dat$x))
        x.fit <- c(x.fit, max(tmp.dat$x))
    y.fit <- predict(tmp.fit, data.frame(x = x.fit))
    if (tmp.is.factor)
        x.fit <- x
    return(list(x = x.fit, y = y.fit))
}

setLegend <- function(type, font, ascending, fill.color, fill.opacity, border.color, border.line.width,
                      x.pos=1.02, y.pos=1.00, reversed = FALSE)
{
    if (is.na(ascending))
        ascending <- !(grepl("Stacked", type) && !reversed) || grepl("Stacked Bar", type)
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

getAxisType <- function(labels, format)
{
    d3.type <- d3FormatType(format)

    if (d3.type == "category")
        return("category")

    if (d3.type == "date")
    {
        ymd <- PeriodNa(labels)
        if (!any(is.na(ymd)))
            return("date")
    }
    if (d3.type == "numeric")
    {
        if (!any(is.na(suppressWarnings(as.numeric(gsub(",", "", labels))))))
            return("numeric")
    }

    # Try to find default format based only on labels
    if (!any(is.na(suppressWarnings(as.numeric(gsub(",", "", labels))))))
        return("numeric")
    ymd <- AsDate(labels, on.parse.failure = "silent")
    if (all(!is.na(ymd)))
        return("date")
    else
        return("category")
}

d3FormatType <- function(format)
{
    if (is.null(format) || is.na(format) || format == "")
        return("")
    if (format == "Category")
        return("category")

    if (grepl("%[aAbBcdefHIJmMLpQsSuUVwWxXyYz]", format))
        return("date")
    else
        return("numeric")
}

formatLabels <- function(dat, type, label.wrap, label.wrap.nchar, x.format, y.format)
{
    is.bar <- grepl("Bar", type, fixed=T)
    if (is.matrix(dat))
    {
        x.labels <- rownames(dat)
        y.labels <- NULL

        x.axis.type <- "numeric"
        y.axis.type <- "numeric"
        if (!is.bar)
            x.axis.type <- getAxisType(x.labels, x.format)
        else
            y.axis.type <- getAxisType(x.labels, x.format)
    }
    else
    {
        x.labels <- unique(dat[[1]])
        y.labels <- unique(dat[[2]])
        x.axis.type <- getAxisType(x.labels, x.format)
        y.axis.type <- getAxisType(y.labels, y.format)
    }

    # labels are only processed for independent x-axis (or y-axis in bar charts)
    # the other axis is always numeric
    labels <- x.labels
    axis.type <- if (is.bar) y.axis.type else x.axis.type
    if (axis.type == "date")
    {
        ymd <- AsDate(labels, on.parse.failure = "silent") # currently cannot switch between US/international inputs, amend to ParseDateTime see DS-1607
        labels <- ymd
    }
    else
    {
        ymd <- NULL
        if (is.character(labels))
            labels <- autoFormatLongLabels(labels, label.wrap, label.wrap.nchar)
    }
    return(list(ymd = ymd, labels = labels, labels.on.x = !is.bar,
                x.axis.type = x.axis.type, y.axis.type = y.axis.type))
}

setAxis <- function(title, side, axisLabels, titlefont,
                    linecolor, linewidth, gridwidth, gridcolor,
                    ticks, tickfont, tickangle, ticklen, tickdistance,
                    tickformatmanual, tickprefix, ticksuffix, tickshow,
                    show.zero, zero.line.width, zero.line.color,
                    hovertext.format.manual, labels = NULL)
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
        else if (axis.type == "numeric")
        {
            range <- rev(range(as.numeric(axisLabels$labels))) + c(0.5, -0.5)
            if (show.zero)
            {
                range[2] <- min(range[2], 0)
                range[1] <- max(range[1], 0)
            }
        }
        else
            range <- c(length(axisLabels$labels)-0.5, -0.5)
        if (ticks$autorange != "reversed")
            range <- rev(range)
    }
    else if (axis.type == "date" && !is.null(axisLabels$ymd))
    {
        autorange <- FALSE
        tmp.dates <- as.numeric(axisLabels$ymd) * 1000
        diff <- min(diff(tmp.dates), na.rm=T)
        range <- range(tmp.dates, na.rm=T) + c(-1, 1) * diff
        if (ticks$autorange == "reversed")
            range <- rev(range)
    }

    tickformat <- ""
    if (sum(nchar(tickformatmanual)) > 0 && d3FormatType(tickformatmanual) %in% c("", axis.type))
        tickformat <- tickformatmanual
    else if (sum(nchar(tickformatmanual)) > 0)
        warning("Axis label format of type '", d3FormatType(tickformatmanual),
                "' incompatible with axis type '", axis.type, "'")

    hoverformat <- ""
    if (sum(nchar(hovertext.format.manual)) > 0 && d3FormatType(hovertext.format.manual) %in% c("", axis.type))
        hoverformat <- hovertext.format.manual
    else if (sum(nchar(hovertext.format.manual)))
        warning("Hovertext label format of type '", d3FormatType(hovertext.format.manual),
                "' incompatible with axis type '", axis.type, "'")

    rangemode <- "normal"
    if (axis.type == "numeric" && show.zero)
        rangemode <- "tozero"

    return (list(title = title, side = side, type = axis.type,
                 titlefont = titlefont, tickfont = tickfont,
                 showline = has.line, linecolor = linecolor, 
                 linewidth = if (!has.line) NULL else linewidth,
                 showgrid = gridwidth > 0, gridwidth = gridwidth, 
                 gridcolor = gridcolor, tickmode = ticks$mode, 
                 tickvals = ticks$tickvals, ticktext = ticks$ticktext,
                 ticks = if (has.line) "outside" else "", tickangle = tickangle, 
                 ticklen = ticklen, tickcolor = linecolor, tickfont = tickfont, 
                 dtick = tickdistance, tickformat = tickformat,
                 tickprefix = tickprefix, ticksuffix = ticksuffix, 
                 hoverformat = hoverformat, layer = "below traces",
                 autorange = autorange, range = range, rangemode = rangemode, 
                 zeroline = show.zero, zerolinewidth = zero.line.width, 
                 zerolinecolor = zero.line.color,
                 showexponent="all", showtickprefix=TRUE, showticksuffix=TRUE, 
                 showticklabels=tickshow))
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
        stop("If specifying the distance between ticks on an axis,",
             "you must also specify the minimum and maximum values.")
    
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
            minimum <- min(0, min(data, na.rm = TRUE))
        if (is.null(maximum))
            maximum <- max(data, na.rm = TRUE)

        # Add horizontal space for data labels in column charts
        pad <- 0
        lab.len <- 1
        if (!is.null(labels))
        {
            lab.len <- max(nchar(as.character(unlist(labels)))) * label.font.size/10
            pad <- (maximum - minimum) * (0.05 * lab.len/4 + (0.1 * is.bar))
        }
        if (!is.bar || min(data, na.rm = TRUE) < 0)
            minimum <- minimum - pad
        maximum <- maximum + pad
    }

    if (!is.null(minimum) && !is.null(maximum))
    {
        autorange <- FALSE
        range <- c(minimum, maximum)
        if (reversed)
            range <- rev(range)
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

#' Force evaluation of variables in an environment
#'
#' Used by some plotting functions before calling Distribution
#' important for unit testing with testthat.
#' Also evaluates variables that have been assigned
#' another variable in the signature (relevant if
#' try eval(cl, parent.frame()) )
#' @noRd
#' @param x Component of a \code{\link{call}}
#' @param env Environment to evaluate \code{x} in
#' @return \code{x} or if it has class \code{"name"}
#' or \code{"call"}, its value when evaluated in \code{env}.
evalc <- function(x, env)
{
    if (inherits(x, c("name", "call")))
        return(eval(x, env))
    x
}
