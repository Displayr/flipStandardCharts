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



minPosition <- function(x, n = 1)
{
    if (is.character(x))
        return(rep(x[1], n))
    else if (is.factor(x))
        return(rep(levels(x)[1], n))
    else
        return(rep(min(x, na.rm=T), n))
}

#' @importFrom flipU CopyAttributes
checkMatrixNames <- function(x, assign.col.names = TRUE)
{
    tInfo <- attr(x, "tsp")
    if (length(tInfo) == 3)    # time-series object
    {
        t.seq <- seq(from = tInfo[1], to = tInfo[2], by = 1/tInfo[3])
        x <- as.matrix(x)
        rownames(x) <- t.seq
        return(x)
    }
    x <- CopyAttributes(as.matrix(x), x)
    if (is.null(rownames(x)))
        rownames(x) <- 1:nrow(x)
    if (is.null(colnames(x)) && assign.col.names)
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
                y[[i]] <- y[[i]][r.names, , drop = FALSE]
            if (!is.null(c.names))
                y[[i]] <- y[[i]][, c.names, drop = FALSE]
        }
    }
    return(y)
}


#' Returns range of axis or vector in format readable for plotly
#'
#' @param axis If \code{axis} object is provided and \code{range} is
#'   specified in it (i.e. not \code{auto.range}), then this range will
#'   be returned
#' @param axisFormat This is the output of \code{formatLabels}. This object
#'   contains information if the axis is a date.
#' @param x If \code{axis$range} is not provided, and \code{axisFormat} does
#'   not contain dates, then range is determined
#'   from the range of the data \code{x}. Offsets are added so that
#'   bar and column charts are not truncated
#' @importFrom flipTime AsDateTime
#' @noRd
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
        else if (is.numeric(x)) # this can contain NAs
        {
            diff <- if (length(x) == 1) 1
                    else abs(min(diff(sort(x)), na.rm = TRUE))
            range <- range(x) + c(-0.5, 0.5) * diff
        }
        else if (all(!is.na(suppressWarnings(as.numeric(x)))))
        {
            tmp <- as.numeric(x)
            diff <- if (length(x) == 1) 1
                    else abs(min(diff(sort(tmp)), na.rm = TRUE))
            range <- range(tmp) + c(-0.5, 0.5) * diff
        }
        else if (all(!is.na(suppressWarnings(AsDateTime(x, on.parse.failure = "silent")))))
            range <- range(AsDateTime(x))
        else
            range <- c(-0.5, length(x)-0.5)

        if (!is.null(axis) && axis$autorange == "reversed")
            range <- rev(range)
    }
    range
}


#' Construct line of best fit
#'
#' @param x Independent (predictor) data for fitting (this is actually
#'   the y-axis in bar charts). May be vector of numeric, dates or factor.
#' @param y Dependent (predicted data). This must be a numeric vector.
#' @param fit.type May be \code{Smooth}, which creates a loess smooth, or
#'   any other value will use a \code{Linear} regression to create the line
#'   of best fit
#' @param axis.type The axis type of the dependent axis. This is used
#'   to provide extra information about how to display the \code{x} variable.
#' @param ignore.last Whether to ignore the last observation in \code{x}
#'   and \code{y}.
#' @importFrom stats supsmu
#' @importFrom mgcv gam
#' @noRd
fitSeries <- function(x, y, fit.type, ignore.last, axis.type, CI.show = FALSE)
{
    if (!is.numeric(y))
    {
        warning("Line of best fit cannot handle non-numeric y-values.")
        return(list(x = NULL, y = NULL))
    }

    tmp.is.factor <- axis.type != "numeric"
    x0 <- if (tmp.is.factor) suppressWarnings(AsNumeric(x, binary = FALSE))
          else as.numeric(x)
    tmp.dat <- data.frame(xorig = x, x = x0, y = y)
    if (ignore.last)
        tmp.dat <- tmp.dat[-which.max(tmp.dat$x),]
    ind.na <- which(is.na(tmp.dat$x) | is.na(tmp.dat$y))
    if (length(ind.na) > 0)
        tmp.dat <- tmp.dat[-ind.na,]
    if (nrow(tmp.dat) < 2)
    {
        warning("Not enough data to constuct line of best fit.")
        return(list(x = NULL, y = NULL))
    }
    ord <- order(tmp.dat$x)
    tmp.dat <- tmp.dat[ord,]

    if (grepl("(friedman|super|supsmu)", fit.type, ignore.case = TRUE))
    {
        if (CI.show)
            warning("Confidence intervals cannot be computed for trend lines of this type.")
        indU <- which(!duplicated(tmp.dat$x))
        if (length(indU) < nrow(tmp.dat))
            warning("Multiple points at the same x-coordinate ignored for estimating line of best fit.\n")
        tmp.fit <- supsmu(tmp.dat$x[indU], tmp.dat$y[indU])
        return(list(x = x[ord[indU]], y = tmp.fit$y))
    }
    else if (grepl("(smooth|loess)", fit.type, ignore.case = TRUE) && nrow(tmp.dat) > 7)
        tmp.fit <- loess(y~x, data = tmp.dat)
    else if (grepl("(cubic|spline|gam)", fit.type, ignore.case = TRUE))
        tmp.fit <- gam(y~s(x, bs = "cr"), data = tmp.dat)
    else
        tmp.fit <- lm(y~x, data=tmp.dat)

    x.fit <- if (tmp.is.factor) tmp.dat$x
             else seq(from = min(tmp.dat$x), to = max(tmp.dat$x), length = 100)
    if (!tmp.is.factor && max(x.fit) < max(tmp.dat$x))
        x.fit <- c(x.fit, max(tmp.dat$x))
    y.fit <- if ("gam" %in% class(tmp.fit)) predict(tmp.fit, data.frame(x = x.fit), se = CI.show, type = "response")
             else                           predict(tmp.fit, data.frame(x = x.fit), se = CI.show)
    if (tmp.is.factor)
        x.fit <- tmp.dat$xorig
    if (CI.show)
    {
        lb <- as.numeric(y.fit$fit - (1.96 * y.fit$se))
        ub <- as.numeric(y.fit$fit + (1.96 * y.fit$se))
        return(list(x = x.fit, y = y.fit$fit, lb = lb, ub = ub))
    }
    return(list(x = x.fit, y = y.fit))
}

setLegend <- function(type, font, ascending, fill.color, fill.opacity, border.color, border.line.width,
                      x.pos = 1.02, y.pos = 1.00, reversed = FALSE)
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
            y = max(-0.5, min(1.0, y.pos)),
            x = max(-0.5, min(1.02, x.pos)),
            traceorder = order))
}

#' @importFrom flipTime AsDateTime
getAxisType <- function(labels, format)
{
    d3.type <- d3FormatType(format)

    if (d3.type == "category")
        return("category")

    if (d3.type == "date")
    {
        ymd <- suppressWarnings(AsDateTime(labels, on.parse.failure = "silent"))
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
    ymd <- suppressWarnings(AsDateTime(labels, on.parse.failure = "silent"))
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

#' @importFrom flipTime AsDateTime
#' @noRd
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
        ymd <- AsDateTime(labels, on.parse.failure = "silent")
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

getDateAxisRange <- function(label.dates)
{
    tmp.dates <- as.numeric(label.dates) * 1000
    day.len <- 60 * 60 * 24 * 1000  # length of days in seconds
    diff <- min(abs(diff(tmp.dates)), na.rm = T)

    # plotly handles different time-scales differently
    if (diff < day.len)
        range <- range(tmp.dates, na.rm = T) - day.len*11/24 + c(-0.5, 0.5) * diff
    else if (diff < 5 * day.len)
        range <- range(tmp.dates, na.rm = T) + c(-1.0, 0.1) * day.len
    else
        range <- range(tmp.dates, na.rm = T) + c(-0.5, 0.5) * diff
    range
}

setAxis <- function(title, side, axisLabels, titlefont,
                    linecolor, linewidth, gridwidth, gridcolor,
                    ticks, tickfont, tickangle, ticklen, tickdistance,
                    tickformatmanual, tickprefix, ticksuffix, tickshow,
                    show.zero, zero.line.width, zero.line.color,
                    hovertext.format.manual, labels = NULL, num.series = 1)
{
    axis.type <- if (side %in% c("bottom", "top")) axisLabels$x.axis.type else axisLabels$y.axis.type
    has.line <- !is.null(linewidth) && linewidth > 0
    if (!tickshow)
        tickfont$size <- 0

    if (!is.null(labels) && !is.na(labels) && any(nchar(labels) > 0) &&
         is.null(tickangle) && side %in% c("bottom", "top"))
    {
        lab.nchar <- max(c(0, nchar(unlist(strsplit(split = "<br>", as.character(labels))))), na.rm = TRUE)
        tickangle <- if ((!axis.type %in% c("numeric", "linear", "date")) && lab.nchar > 2 &&
                        length(labels) * num.series * lab.nchar > 50) 90
                     else 0
    }

    autorange <- ticks$autorange
    range <- ticks$range
    day.len <- 60 * 60 * 24 * 1000
    if ((!axisLabels$labels.on.x) && side %in% c("left","right"))
    {
        autorange <- FALSE
        if (axis.type == "date")
            range <- rev(getDateAxisRange(axisLabels$ymd))
        else if (axis.type == "numeric")
        {
            range <- rev(getRange(axisLabels$labels, NULL, NULL))
            if (show.zero)
            {
                range[2] <- min(range[2], 0)
                range[1] <- max(range[1], 0)
            }
        }
        else
            range <- c(length(axisLabels$labels) - 0.5, -0.5)
        if (ticks$autorange != "reversed")
            range <- rev(range)
    }
    else if (axis.type == "date" && !is.null(axisLabels$ymd))
    {
        autorange <- FALSE
        range <- getDateAxisRange(axisLabels$ymd)
        if (ticks$autorange == "reversed")
            range <- rev(range)
    }
    tickformat <- checkD3Format(tickformatmanual, axis.type)
    hoverformat <- checkD3Format(hovertext.format.manual, axis.type, "Hovertext")

    rangemode <- "normal"
    if (axis.type == "numeric" && show.zero)
        rangemode <- "tozero"
    if (gridwidth == 0)
        zero.line.color <- rgb(1, 1, 1, alpha = 0) # invisible

    # Specify max number of ticks but not for distribution charts
    nticks <- NULL
    independ.axis <- side %in% c("top", "bottom") == axisLabels$labels.on.x
    if (axis.type != "category" && independ.axis &&
        !(length(axisLabels$labels) == 1 && is.numeric(axisLabels$labels)))
        nticks <- min(length(axisLabels$labels) + 1, 11)

    return (list(title = title, side = side, type = axis.type,
                 titlefont = titlefont, tickfont = tickfont,
                 showline = has.line, linecolor = linecolor,
                 linewidth = if (!has.line) NULL else linewidth,
                 showgrid = gridwidth > 0, gridwidth = gridwidth,
                 gridcolor = gridcolor, tickmode = ticks$mode, nticks = nticks,
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

    lab.nchar <- max(c(0, nchar(unlist(strsplit(split="<br>", as.character(labels))))), na.rm = TRUE)
    font.asp <- fontAspectRatio(axis$tickfont$family)
    lab.len <- font.asp * axis$tickfont$size * lab.nchar * 1.25
    lab.nline <- if (is.character(labels)) max(sapply(gregexpr("<br>", labels),
                     function(x){sum(x > -1)}), na.rm = TRUE)
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
        lab.nchar <- max(c(0,nchar(unlist(strsplit(split = "<br>", as.character(labels))))), na.rm = TRUE)
        if (is.null(axis$tickangle))
            axis$tickangle <- 0
        if (axis$tickangle != 0)
            margins$b <- margins$b + 0.65 * new.margin + title.pad
        else
            margins$b <- margins$b + 1.25 * axis$tickfont$size*(floor(lab.nline)+1) + title.pad
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
        # We leave twice the space for subtitles, because titles are always
        # positioned halfway down the top margin
        subtitle.nline <- sum(gregexpr("<br>", subtitle)[[1]] > -1) + 1.5
        margins$t <- margins$t + (subtitle.font.size * subtitle.nline) * 0.8 * 2
    }
    if (nchar(footer) > 0)
    {
        footer.nline <- sum(gregexpr("<br>", footer)[[1]] > -1) + 4
        margins$b <- margins$b + (footer.font.size * footer.nline * 1.25)
    }
    margins
}

setMarginsForLegend <- function(margins, showlegend, legend, text, type = "")
{
    if (showlegend && legend$x > 0.99)
    {
        len <- if (is.factor(text)) nchar(levels(text))
               else                 nchar(text)
        margins$r <- min(300, 70 + (legend$font$size * max(0, len) * 0.7))
    } else if (type != "radar")
        margins$r <- 20
    margins
}

setCustomMargins <- function(margins, margin.top, margin.bottom, margin.left,
    margin.right, margin.inner.pad)
{
    if (!is.null(margin.top))
        margins$t <- margin.top
    if (!is.null(margin.bottom))
        margins$b <- margin.bottom
    if (!is.null(margin.left))
        margins$l <- margin.left
    if (!is.null(margin.right))
        margins$r <- margin.right
    if (!is.null(margin.inner.pad))
        margins$pad <- margin.inner.pad
    margins
}

addSubtitle <- function(p, subtitle, subtitle.font, margins)
{
    if (sum(nchar(subtitle)) > 0)
        p <- add_annotations(p, text = subtitle, font = subtitle.font,
                xref = "paper", x = 0.5, xshift = (margins$r - margins$l)/2,
                yref = "paper", y = 1.0, yanchor = "bottom", showarrow = FALSE)
    p
}

# footer.font and margins are lists
# footer.font = list(family, size, color)
# margins = list(top, bottom, left, right, inner)
setFooterAxis <- function(footer, footer.font, margins, overlay = "x")
{
    # overlay = FALSE is needed for the distribution chart with no x axis
    # but in other cases, setting to FALSE may do ugly things with transparencies

    res <- NULL
    if (nchar(footer) > 0)
    {
        footer.nline <- sum(gregexpr("<br>", footer)[[1]] > -1) + 1
        footer.npad <- max(0, ceiling(margins$b/footer.font$size/1.25) - footer.nline - 2)
        footer <- paste0(paste(rep("<br>", footer.npad), collapse = ""), footer)
        res <- list(overlaying = overlay, side = "bottom", anchor = "free",
             position = 0, domain = c(0,1.0), visible = TRUE, layer = "below traces",
             showline = FALSE, zeroline = FALSE, showgrid = FALSE,
             tickfont = footer.font, ticktext = c(footer), tickangle = 0,
             range = c(0,1), tickvals = c(0.5))
    }
    res
}

# This differs from as.numeric in that it returns NULL
# instead of NA if there is no valid output
# Also so basic substitution such as removing commas
charToNumeric <- function(x)
{
    if (!is.character(x))
        return(x)

    x <- gsub(" ", "", x)
    x <- gsub(",", "", x) # e.g. '5,000'
    xnum <- suppressWarnings(as.numeric(x))
    xnum <- xnum[!is.na(xnum)]
    if (length(xnum) != 1)
        return(NULL)
    return(xnum)
}

# This is only applied to the values axis which is always numeric
setValRange <- function(min, max, values, use.defaults = TRUE)
{
    min <- charToNumeric(min)
    max <- charToNumeric(max)

    # If no range is specified, then use defaults
    if (use.defaults && is.null(min) && is.null(max))
        return(list(min = NULL, max = NULL))
    if  (is.null(min))
        min <- min(unlist(values), na.rm = TRUE)
    if  (is.null(max))
        max <- max(unlist(values), na.rm = TRUE)
    return(list(min = min, max = max))
}


setTicks <- function(minimum, maximum, distance, reversed = FALSE,
                data = NULL, labels = NULL, type="scatter", label.font.size = 10)
{
    if (is.null(minimum) != is.null(maximum))
        warning("To specify the range of an axis, you must specify both the minimum and maximum values.")
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

        # Add horizontal space for data labels in bar charts
        pad <- 0
        lab.len <- 1
        if (!is.null(labels) && is.bar)
        {
            lab.len <- max(nchar(as.character(unlist(labels))))
            pad <- (maximum - minimum) * (lab.len+2) * label.font.size / 200
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

autoFormatLongLabels <- function(x, wordwrap = FALSE, n = 21, truncate = FALSE)
{
    if (truncate)
        warning("autoFormatLongLabels: truncate not longer does anything.")

    # Check for zero-length strings which are ignored by plotly
    if (length(x) > 1)
    {
        ind <- which(sapply(x, nchar) == 0)
        if (length(ind) > 0)
            x[ind] <- " "
    }

    output.text <- x
    if (wordwrap && length(output.text) > 0)
        output.text <- sapply(output.text, function(x) lineBreakEveryN(x, n))
    if (is.null(output.text) || is.na(output.text) || length(output.text) == 0)
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

#' Extract the number of decimal places from a d3 format string.
#'
#' All chart functions should accept d3 formats. This is used by functions
#' that do not handle d3 to find the number of decimal places used to
#' format labels and hovertext.
#' #' @noRd
#' @param format d3 formatting string
#' @param default The number of decimal places if \code{format} is
#' not provided (usually signifying automatic formatting).
#' @return integer
decimalsFromD3 <- function(format, default = 0)
{
    if (length(format) == 0 || format == "")
        return(default)
    return(sum(as.numeric(regmatches(format, regexpr("\\d+", format)))))
}

#' Whether to format as percentages based on a d3 format string.
#'
#' All chart functions should accept d3 formats. This is used by functions
#' that do not handle d3 to determine whether to format labels and hovertext as percentages.
#' #' @noRd
#' @param format d3 formatting string
#' @return logical
percentFromD3 <- function(format)
{
    return(substr(format, nchar(format), nchar(format)) == "%")
}

#' Output data in D3-formatting
#'
#' Returns a strings according to the d3 format specified
#' @noRd
#' @param x Input data (may be a vector) to format
#' @param format d3 formatting string
#' @param prefix Optional string to prepend to output
#' @param suffix Optional string to append to output
formatByD3 <- function(x, format, prefix = "", suffix = "")
{
    x.str <- as.character(x)
    if (is.numeric(x))
    {
        if (percentFromD3(format))
            x.str <- paste0(FormatAsReal(x*100, decimals = decimalsFromD3(format)), "%")
        else
            x.str <- FormatAsReal(x, decimals = decimalsFromD3(format))
    }
    x.str <- paste0(prefix, x.str, suffix)
    return(x.str)
}

#' Whether to format numbers with comma separation of thousands based on a d3 format string.
#'
#' All chart functions should accept d3 formats. This is used by functions
#' that do not handle d3 to determine how to format labels and hovertext.
#' #' @noRd
#' @param format d3 formatting string
#' @return logical
commaFromD3 <- function(format)
{
    return(grepl(",", format, fixed = TRUE))
}


# Gives a warning if the axis.type is incompatible
# Will also specify a numeric format if none is supplied
checkD3Format <- function(format, axis.type, warning.type = "Axis label")
{
    if (sum(nchar(format), na.rm = TRUE) == 0)
        return("")
    if (substr(format, nchar(format), nchar(format)) %in% c("", 0:9)) # automatic formatting
    {
        if (axis.type == "numeric")
            return(paste0(format, "f"))
        else
            return(format)
    }
    if (d3FormatType(format) != axis.type)
        warning(warning.type, " format of type '", d3FormatType(format),
                "' incompatible with axis type '", axis.type, "'")
    return(format)
}



