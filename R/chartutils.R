#' ErrorIfNotEnoughData
#'
#' Returns an error if there is not enough data for charting.
#' @param x The data to be plotted.
#' @param require.tidy The data is assumed to be a numeric vector, matrix, array, or data frame.
#' @param require.notAllMissing The data is required to contain at least one non-NA value.
#' @importFrom flipU StopForUserError
#' @export
ErrorIfNotEnoughData <- function(x, require.tidy = TRUE, require.notAllMissing = FALSE)
{
    .stop <- function()
        { StopForUserError("There is not enough data to create a plot.") }
    .possiblyTidy <- function(x)
        {is.numeric(x) || is.matrix(x) || is.data.frame(x) || is.array(x)}
    .noData <- function(x)
        {NROW(x) == 0 || NCOL(x) == 0}

    if (!require.tidy && is.list(x))
        x <- x[[1]]
    if (require.tidy && !.possiblyTidy(x))
        StopForUserError("The data is not in an appropriate format.")
    if (.noData(x))
        .stop()
    if (require.notAllMissing && all(is.na(x)))
        .stop()
}

# hovertemplate is preferrable to hoverinfo because it allows better control
# of formatting. Specifically, we can re-order the x/y and control separators
# and newlines. But the format is slightly more complex
setHoverTemplate <- function(i, axis, chart.matrix, template = NULL, is.bar = FALSE)
{
    # if no template is defined, set default base on axis type
    if (all(!nzchar(template)))
    {
        if (axis$type == "category")
            template <- if (is.bar) "%{y}: %{x}" else "%{x}: %{y}"
        else
            template <- "(%{x}, %{y})"
    }
    if (length(template) > 1)
        template <- matrix(template, nrow(chart.matrix), ncol(chart.matrix))[,i]

    # Set label shown in secondary box with series name if defined
    label <- if (ncol(chart.matrix) == 1 && colnames(chart.matrix)[1] %in% c("Series.1", "Series 1")) ""
             else colnames(chart.matrix)[i]
    template <- paste0(template, "<extra>", label, "</extra>")

    ind.na <- which(!is.finite(chart.matrix[,i]))
    if (length(ind.na) > 0)
    {
        template <- rep(template, length = nrow(chart.matrix))
        val.pattern <- if (is.bar) "%{x}" else "%{y}"
        na.template <- gsub(val.pattern, "", template[ind.na], fixed = TRUE)
        template[ind.na] <- na.template
    }
    return(template)
}


# Used by barcharts and radar chart to because x and y coords do not
# match values in data
evalHoverTemplate <- function(template, x, x.hovertext.format, x.tick.prefix, x.tick.suffix,
    y, y.hovertext.format, y.tick.prefix, y.tick.suffix)
{
    x.match <- regexpr("%{x}", template, fixed = TRUE)
    y.match <- regexpr("%{y}", template, fixed = TRUE)

    if (all(x.match == -1) && all(y.match == -1))
        return(template)

    x.txt <- formatByD3(x, x.hovertext.format, x.tick.prefix, x.tick.suffix)
    y.txt <- formatByD3(y, y.hovertext.format, y.tick.prefix, y.tick.suffix)
    template <- gsub("%", "%%", template, fixed = TRUE) # comment out other '%' signs

    if (all(y.match == -1))
    {
        template <- gsub("{x}", "s", template, fixed = TRUE)
        template <- gsub("%%s", "%s", template, fixed = TRUE)
        return(sprintf(template, x.txt))

    } else if (all(x.match == -1))
    {
        template <- gsub("{y}", "s", template, fixed = TRUE)
        template <- gsub("%%s", "%s", template, fixed = TRUE)
        return(sprintf(template, y.txt))

    } else
    {
        template <- gsub("{y}", "s", template, fixed = TRUE)
        template <- gsub("{x}", "s", template, fixed = TRUE)
        template <- gsub("%%s", "%s", template, fixed = TRUE)

        if (all(y.match < x.match))
            return(sprintf(template, y.txt, x.txt))
        else
            return(sprintf(template, x.txt, y.txt))
    }
}


#' Checks input data and converts it into a matrix for charting
#'
#' Converts input data into a matrix with 2 dimensions
#'   and row and columns names which can be used for charting.
#' @param x Input in the form of vector, matrix, array, time-series object
#'   or a Q table. If input is a Q table, then only the first statistic is returned
#' @param assign.col.names Whether to assign column names. This defaults to true,
#'   It is set to false (for example in \code{Heat}) because the columns do not
#'   make up separate series.
#' @return Numeric matrix with the first statistic in \code{x}.
#' @importFrom flipU CopyAttributes
#' @noRd
checkMatrixNames <- function(x, assign.col.names = TRUE)
{
    # Check for special case where x is a time-series object
    tInfo <- attr(x, "tsp", exact = TRUE)
    if (length(tInfo) == 3)
    {
        t.seq <- seq(from = tInfo[1], to = tInfo[2], by = 1/tInfo[3])
        x <- as.matrix(x)
        rownames(x) <- t.seq
        return(x)
    }

    # If x is a 1-d slice from a multi-stat crosstab
    # Then label matrix as Q-table so that secondary statistics are not plotted
    # But can still be used for annotations
    # We assume its a 1-d slice if at least 2 colnames match something in the list of cell statistics
    is.slice <- FALSE
    q.cell.statnames <- c("Average", "Standard Deviation", "Minimum", "5th Percentile", "25th Percentile",
       "Median", "75th Percentile", "95th Percentile", "Maximum", "Mode",
       "Trimmed Average", "Interquartile Range", "Sum", "% Share", "Column Sample Size",
       "%", "% Excluding NaN", "Row %", "Column %", "Cumulative %", "Expected %", "Residual %",
       "Sample Size", "Missing Count", "Effective Sample Size", "Count",
       "Weighted Column Sample Size", "Weighted Sample Size", "Weighted Count", "t-Statistic",
       "d.f.", "z-Statistic", "Standard Error", "p", "Corrected p", "Index",
       "Multiple Comparison Adjustment", "Not Duplicate", "Column Names",
       "Columns Compared", "Column Comparisons")
    if (inherits(x, "matrix") && !is.null(colnames(x)) && is.null(attr(x, "name")) && is.null(attr(x, "questions")) &&
        ncol(x) > 1 && all(colnames(x) %in% q.cell.statnames))
    {
        attr(x, "name") <- " "
        attr(x, "questions") <- " "
        is.slice <- TRUE
    }

    # Convert into a matrix format and extract primary statistic
    old.names <- c(dimnames(x), NA, NA) # ensure there are at least 2 elements
    new.x <- if (length(dim(x)) == 3) matrix(x[,,1], nrow(x), ncol(x), dimnames = old.names[1:2]) # explicitly specify dimensions
             else as.matrix(suppressWarnings(AsTidyTabularData(x))) # handles 1d data + statistic properly

    # Try to convert character matrix to numeric
    # This may occur in Q-tables with a character statistic (e.g. 'Column Comparisons')
    old.dim <- dim(new.x)
    old.names <- dimnames(new.x)
    if (is.character(new.x))
    {
        new.x <- as.numeric(new.x)
        dim(new.x) <- old.dim
        dimnames(new.x) <- old.names
    }

    # Convert percentage data to decimal form
    if (isPercentData(x) && !is.slice)
    {
        new.x <- new.x/100
        attr(new.x, "statistic") <- NULL
    }

    # Assign row/column names if missing
    if (is.null(rownames(new.x)))
        rownames(new.x) <- 1:NROW(new.x)
    if (is.null(colnames(new.x)) && assign.col.names)
        colnames(new.x) <- sprintf("Series %d", 1:NCOL(new.x))
    ind.na <- which(is.na(rownames(new.x)))
    if (length(ind.na) > 0)
        rownames(new.x)[ind.na] <- "NA"
    row.names <- trimws(rownames(new.x))
    ind.dup <- which(duplicated(row.names))
    if (length(ind.dup) > 0)
    {
        warning("Row names of the input table are not unique: ",
                paste(unique(row.names[ind.dup]), collapse = ", "), " at rows ",
                paste(ind.dup, collapse = ", "))
        # Non-space suffix is needed to stop plotly merging the duplicated rows
        # We use the html entity for 0-length space to keep it centered
        rownames(new.x) <- MakeUniqueNames(row.names, suffix = "&#8203;")
    }
    attr(new.x, "sorted.rows") <- attr(x, "sorted.rows")
    return(new.x)
}

#' @importFrom flipU StopForUserError
checkTableList <- function(y, trend.lines)
{
    num.tables <- length(y)
    y.names <- rep("", num.tables)
    used.names <- c()
    for (i in 1:num.tables)
    {
        if (is.null(attr(y[[i]], "name", exact = TRUE)))
        {
            attr(y[[i]], "name") <- as.character(i)
            used.names <- c(used.names, i)
        }
        y.names[i] <- attr(y[[i]], "name", exact = TRUE)[1]
    }
    if (!trend.lines && length(used.names) > 0)
        warning(sprintf("Tables have been automatically assigned names '%s'. You can name tables using R code: 'attr(table.name, \"name\") <- \"Description\"'", paste(used.names, collapse="', '")))
    if (any(duplicated(y.names)) & !trend.lines)
        warning(sprintf("Tables have duplicate names: '%s'. Points from duplicated tables cannot be distinguised.", paste(y.names[duplicated(y.names)], collapse = "', '")))

    # Check tables match - order of rows will match first table
    r.names <- rownames(y[[1]])
    c.names <- colnames(y[[1]])
    if (!is.null(r.names) && any(duplicated(r.names)) && length(y) > 1)
        warning("Row names of tables must be unique or NULL for multiple tables to be plotted but are duplicated.")

    if (num.tables > 1) {
        for (i in 2:num.tables)
        {
            if (!setequal(r.names, rownames(y[[i]])))
                StopForUserError(sprintf("Tables should have identical row names but table '%s' differs from table '%s'.",
                                         y.names[i], y.names[1]))
            if (!setequal(c.names, colnames(y[[i]])))
                StopForUserError(sprintf("Tables should have identical column names but table '%s' differs from table '%s'.",
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
    tozero <- FALSE
    range <- NULL
    if (!is.null(axis) && !any(is.na(axis$range)))
    {
        # in setAxis, date ranges must be a character, but when we copy an
        # axis to add data labels, we need it as a numeric
        if (axis$type == "date" && is.character(axis$range))
            range <- as.numeric(AsDateTime(axis$range)) * 1000
        else
            range <- axis$range

        if (is.null(axis$range) && axis$rangemode == "tozero")
            tozero <- TRUE
    }

    # Use axis type if numeric or categorical specified
    # For date axis, we do some more checking
    if (is.null(range) && !is.null(axis) && !is.null(axis$type))
    {
        if (axis$type == "numeric")
            range <- calcRangeNumeric(x)
        else if (axis$type == "category")
            range <- calcRangeCategorical(x)

        if (isTRUE(axis$rangemode == "tozero"))
            range <- c(min(0, range[1]), max(0, range[2]))
        if (isTRUE(axis$autorange == "reversed"))
            range <- rev(range)
    }

    # Unspecified axis type or with a date range
    if (is.null(range))
    {
        # In most cases axisFormat$ymd is specified if axis is a date
        if (!is.null(axisFormat) && length(axisFormat$ymd) >= 2)
        {
            tmp.dates <- sort(unique(as.numeric(axisFormat$ymd))) * 1000
            diff <- min(diff(tmp.dates), na.rm = TRUE)
            range <- range(tmp.dates) + c(-0.5, 0.5) * diff
        }
        else if (is.numeric(x))
        {
            range <- calcRangeNumeric(x)
            if (tozero)
                range <- c(min(0, range[1]), max(0, range[2]))
        }
        else if (all(!is.na(suppressWarnings(AsDateTime(x, on.parse.failure = "silent")))) &&
                 !(!is.null(axis) && axis$type == "numeric"))
        {
            tmp.dates <- sort(unique(as.numeric(AsDateTime(x)))) * 1000
            diff <- min(diff(tmp.dates), na.rm = TRUE)
            range <- range(tmp.dates) + c(-0.5, 0.5) * diff

        }
        else if (all(!is.na(suppressWarnings(as.numeric(x)))))
        {
            range <- calcRangeNumeric(x)
            if (tozero)
                range <- c(min(0, range[1]), max(0, range[2]))
        }
        else
            range <- calcRangeCategorical(x)

        if (!is.null(axis) && axis$autorange == "reversed")
            range <- rev(range)
    }
    range
}

calcRangeNumeric <- function(x, offset = 0.5)
{
    tmp <- as.numeric(unique(x))
    diff <- if (length(x) == 1) 1
            else abs(min(diff(sort(tmp)), na.rm = TRUE))
    return(range(tmp) + c(-offset, offset) * diff)
}

calcRangeCategorical <- function(x)
{
    tmp <- unique(x)
    return(c(-0.5, length(tmp)-0.5))
}


isReversed <- function(axis)
{
    if (is.null(axis$range))
        return(axis$autorange == "reversed")
    return(xor(axis$range[1] > axis$range[2], axis$autorange == "reversed"))
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
#' @param warning.prefix Used by small multiples for nice warning messages.
#' @importFrom stats supsmu filter
#' @importFrom mgcv gam
#' @importFrom flipU StopForUserError
#' @noRd
fitSeries <- function(x, y, fit.type, ignore.last, axis.type, CI.show = FALSE,
                      fit.window.size = 3, warning.prefix = "")
{
    if (!is.numeric(y))
    {
        warning("Line of best fit cannot handle non-numeric y-values.")
        return(list(x = NULL, y = NULL))
    }

    tmp.is.factor <- axis.type != "numeric"
    if (tolower(axis.type) == "category" && !is.factor(x))
        x0 <- (1:length(x)) - 1
    else if (tmp.is.factor)
        x0 <- suppressWarnings(AsNumeric(x, binary = FALSE))
    else
        x0 <- as.numeric(x)
    tmp.dat <- data.frame(xorig = x, x = x0, y = y)
    if (ignore.last)
        tmp.dat <- tmp.dat[-which.max(tmp.dat$x),]
    if (nrow(tmp.dat) < 2)
    {
        warning(warning.prefix, "Not enough data to constuct line of best fit.")
        return(list(x = NULL, y = NULL))
    }
    ord <- order(tmp.dat$x)
    tmp.dat <- tmp.dat[ord,]

    if (grepl("(friedman|super|supsmu|moving average)", fit.type, perl = TRUE, ignore.case = TRUE))
    {
        if (CI.show)
            warning("Confidence intervals cannot be computed for trend lines of this type.")
        if (!grepl("moving average", fit.type, perl = TRUE, ignore.case = TRUE))
        {
            ind.na <- which(!is.finite(tmp.dat$x) | !is.finite(tmp.dat$y))
            if (length(ind.na) > 0)
                tmp.dat <- tmp.dat[-ind.na,]
        }
        indU <- which(!duplicated(tmp.dat$x))
        if (length(indU) < nrow(tmp.dat))
            warning(warning.prefix, "Multiple points at the same x-coordinate ignored for estimating line of best fit.\n")
        if (grepl("moving average", fit.type, perl = TRUE, ignore.case = TRUE))
        {
            if (!is.finite(fit.window.size) && fit.window.size <= 0)
                StopForUserError("Moving average must have a positive window size")
            if (length(indU) <= fit.window.size)
            {
                warning("Trend line could not be shown. The length of the data series ",
                        "must be larger than the window size (", fit.window.size, ")")
                return(list(x = NULL, y = NULL))
            }
            tmp.gaps <- unique(diff(tmp.dat$x[indU]))
            if (any(tmp.gaps > min(tmp.gaps) * 1.2)) # allow some variation for dates
                warning("Moving averages do not account for the different intervals between values.")
            if (grepl("center|centre", fit.type, perl = TRUE, ignore.case = TRUE))
                tmp.avg <- rev(filter(tmp.dat$y[rev(indU)], rep(1/fit.window.size, fit.window.size), sides = 2))
            else
                tmp.avg <- filter(tmp.dat$y[indU], rep(1/fit.window.size, fit.window.size), sides = 1)
            return(list(x = tmp.dat$xorig[indU], y = as.numeric(tmp.avg)))
        }
        else
        {
            tmp.fit <- suppressWarnings(try(supsmu(tmp.dat$x[indU], tmp.dat$y[indU]), silent = TRUE))
            if (!inherits(tmp.fit, "try-error"))
                return(list(x = tmp.dat$xorig[indU], y = tmp.fit$y))
        }
    }
    else if (grepl("(smooth|loess)", fit.type, ignore.case = TRUE) && nrow(tmp.dat) > 7)
        tmp.fit <- suppressWarnings(try(loess(y~x, data = tmp.dat), silent = TRUE))
    else if (grepl("(cubic|spline|gam)", fit.type, ignore.case = TRUE))
        tmp.fit <- suppressWarnings(try(gam(y~s(x, bs = "cr"), data = tmp.dat), silent = TRUE))
    else
        tmp.fit <- suppressWarnings(try(lm(y~x, data=tmp.dat), silent = TRUE))

    if (inherits(tmp.fit, "try-error"))
    {
        warning(warning.prefix, "Could not fit trend line using ", fit.type, ".")
        return(list(x = NULL, y = NULL))
    }
    x.fit <- if (tmp.is.factor) tmp.dat$x
             else seq(from = min(x0, na.rm = TRUE), to = max(x0, na.rm = TRUE), length = 100)
    if (!tmp.is.factor && max(x.fit) < max(tmp.dat$x))
        x.fit <- c(x.fit, max(tmp.dat$x))
    y.fit <- if (inherits(tmp.fit, "gam")) suppressWarnings(try(predict(tmp.fit, data.frame(x = x.fit), se = CI.show, type = "response")))
             else                          suppressWarnings(try(predict(tmp.fit, data.frame(x = x.fit), se = CI.show)))
    if (inherits(y.fit, "try-error"))
    {
        warning("Could not fit trend line to data. Check that you expect to map a single x-value to a single y-value.")
        y.fit <- NULL
        lb <- NULL
        ub <- NULL
    }
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

# This function deals with both logical or string inputs
setShowLegend <- function(legend.show, num.series = 2)
{
    auto.show <- num.series > 1

    # Default response is to only show legend if it there is more than 1 series
    if (isTRUE(legend.show) || is.na(legend.show) || grepl("Auto", legend.show))
        return(auto.show)
    if (isTRUE(legend.show == FALSE))
        return(FALSE)

    # If legend.show is not boolean, check for string inputs
    # Note that legend is shown even if there is only 1 series
    # For legacy reasons, the boolean inputs do not give the same result.
    if (grepl(legend.show, "Show"))
        return(TRUE)
    if (grepl(legend.show, "Hide"))
        return(FALSE)

    # If legend.show is unknown, then return default
    return(auto.show)
}

setLegend <- function(type, font, ascending, fill.color, fill.opacity, border.color, border.line.width,
                      x.pos = NULL, y.pos = NULL, reversed = FALSE, orientation = 'Vertical', y2.show = FALSE)
{
    if (tolower(substr(orientation, 1, 1)) == "h")
        orientation <- "h"
    else
        orientation <- "v"

    if (y2.show)
    {
        if (is.null(x.pos))
            x.pos <- if (orientation == "v") 1.15 else 0.5
        if (is.null(y.pos))
            y.pos <- if (orientation == "v") 1.0 else -0.3
    } else
    {
        if (is.null(x.pos))
            x.pos <- if (orientation == "v") 1.02 else 0.5
        if (is.null(y.pos))
            y.pos <- if (orientation == "v") 1.0 else -0.2
    }

    if (is.na(ascending))
        ascending <- !(grepl("Stacked", type) && !reversed) || grepl("Stacked Bar", type)
    order <- if (!ascending) "reversed" else "normal"
    return(list(bgcolor = toRGB(fill.color, alpha=fill.opacity),
            bordercolor = border.color,
            borderwidth = border.line.width,
            orientation = orientation,
            font = font,
            xanchor = if (x.pos == 0.5) "center" else "left",
            yanchor = if (y.pos > 1.0) "bottom" else "auto",
            y = max(-2, min(3, y.pos)),
            x = max(-2, min(3, x.pos)),
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
        if (inherits(labels, "Date") || inherits(labels, "POSIXt") || inherits(labels, "POSIXct"))
            return("date")
        ymd <- try(suppressWarnings(AsDateTime(as.character(labels), on.parse.failure = "silent")), silent = TRUE)
        if (!inherits(ymd, "try-error") && !any(is.na(ymd)))
            return("date")
    }
    if (d3.type == "numeric")
    {
        # gsub acts on the labels of a factor (instead of the factor values),
        # which means that as.numeric applies to the labels too
        if (!any(is.na(suppressWarnings(as.numeric(gsub(",", "", labels))))))
            return("numeric")
    }

    # Try to find default format based only on labels
    if (!any(is.na(suppressWarnings(as.numeric(gsub(",", "", labels))))))
        return("numeric")
    ymd <- try(suppressWarnings(AsDateTime(labels, on.parse.failure = "silent")), silent = TRUE)
    if (!inherits(ymd, "try-error") && all(!is.na(ymd)) && length(ymd) > 1)
        return("date")
    else
        return("category")
}

# This is only used in labeledscatter
convertAxis <- function(values, axis.type)
{
    if (axis.type == "date")
        return(AsDateTime(values, on.parse.failure = "silent"))
    if (axis.type == "category")
    {
        # Do not use flipTransformations::Factor because we don't want to aggregate by QDate
        if (inherits(values, "factor"))
            return(values)
        return(factor(values, levels = unique(as.character(values))))
    }
    if (axis.type == "numeric")
    {
        # gsub acts on the labels of a factor (instead of the factor values),
        # which means that as.numeric applies to the labels too
        return(as.numeric(gsub(",", "", values)))
    }
    # not sure what type this is?
    return(as.vector(values))
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
#' @importFrom verbs Sum
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

    categorical.axis.type <- if (is.bar) y.axis.type else x.axis.type
    if (isTRUE(attr(dat, "sorted.rows")) && categorical.axis.type != "category")
        warning("Sorting has been applied to the rows of the input table but the positioning of ",
            "elements in a numeric or date axis is not affected by the order in the table. ",
            "To use the order of the table rows, set the axis number type to 'Category' in ",
            "Chart > CATEGORIES AXIS > Axis number type")

    # labels are only processed for independent x-axis (or y-axis in bar charts)
    # the other axis is always numeric
    labels <- x.labels
    axis.type <- if (is.bar) y.axis.type else x.axis.type
    if (axis.type == "date")
    {
        ymd <- AsDateTime(labels, on.parse.failure = "silent")
        n.dup <- Sum(duplicated(ymd), remove.missing = FALSE)
        if (n.dup > 0)
            warning("Date axis has ", n.dup, " duplicated values. There may have been an error parsing dates.")
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

getDateAxisRange <- function(label.dates, new.range = NULL)
{
    if (length(new.range) == 2)
        return(as.character(new.range))

    tmp.dates <- as.numeric(label.dates)
    diff <- min(abs(diff(sort(unique(tmp.dates)))), na.rm = TRUE)

    # Always return date-ranges as characters since there
    # seems to be more problems with using milliseconds since plotly v4.8.0
    range <- as.character(range(label.dates) + c(-1,1) * ceiling(0.5 * diff))
    return(as.character(range))
}

# This function is used to override the default plotly settings for
# a date/time axis. The return value is used to set the 'dtick' parameter.
# It can be one of:
#       - time between ticks in units of milliseconds
#       - intervals in months, e.g. 'M3' for 3 months, allows for different number of days per month
#       - NULL, i.e revert back to plotly defaults
# Where date intervals are reasonably regular we prefer to use plotly defaults
# because it will do try to simplify labels depending on the timescale
# e.g. only show years at the beginning of each year instead of each month
# or hide day of month.
setDateTickDistance <- function(date.labels, num.maxticks)
{
    # For a reasonably long series of dates, plotly does reasonably well
    # But for a short series, plotly will place ticks on every Sunday,
    # which usually does not align with the data points
    n <- length(date.labels)
    if (n < 2 || n > 10)
        return(NULL)
    tmp.dist <- as.numeric(difftime(date.labels[n], date.labels[1], units = "secs"))/(n-1)

    # We check if the date labels are on the first or each month
    # or the first day of each year, because these are likely
    # internally generated. They occur when the user does not give
    # a fully specified date, e.g. "Jan 2017", or "2019".
    use.auto.ticks <- TRUE

    if (tmp.dist <= 86400) # time scale is less than a day
        use.auto.ticks <- FALSE
    else if (tmp.dist <= 0.9 * 31536000) { # on the scale of a year
        # Check for monthly intervals which accounts for different number of days per month
        seconds.in.day <- 86400
        nmonth <- round(tmp.dist/(seconds.in.day * 30))
        if (!is.null(num.maxticks) && n > num.maxticks)
            nmonth <- (floor(n/num.maxticks) + 1) * nmonth
        day.of.month <- as.numeric(format(date.labels, "%d"))
        # use auto generated ticks to default to not showing the day of month
        if (all(day.of.month == 1))
            return (NULL)
        if (length(unique(day.of.month)) == 1)
            return(paste0("M", nmonth))
        # Check for monthly intervals which are fixed relative to the end of the month
        offset <- 31 - as.numeric(min(day.of.month))
        if (inherits(date.labels, "Date") &&
            length(unique(format(date.labels + offset, "%d"))) == 1)
                return(paste0("M", nmonth))
        if (inherits(date.labels, "POSIXct") &&
            length(unique(format(date.labels + (offset * seconds.in.day), "%d"))) == 1)
                return(paste0("M", nmonth))
        use.auto.ticks <- FALSE
    } else # use auto generated tick to default to not showing month or day
        use.auto.ticks <- all(as.numeric(format(date.labels, "%j")) == 1)

    # If axis range is considerable larger than the intervals between ticks
    # use plotly defaults
    if (difftime(max(date.labels), min(date.labels), units = "secs") >
        (n+1) * tmp.dist)
        use.auto.ticks <- TRUE

    # Override plotly defaults by specifying tickdistance
    if (!use.auto.ticks)
    {
        if (!is.null(num.maxticks) && n > num.maxticks)
        {
            tmp.mult <- floor(n/num.maxticks) + 1
            tmp.dist <- tmp.dist * tmp.mult
        }
        tickdistance <- tmp.dist * 1000 # in units of milliseconds
        return(tickdistance)
    }
    return(NULL)
}

setAxis <- function(title, side, axisLabels, titlefont,
                    linecolor, linewidth, gridwidth, gridcolor,
                    ticks, tickfont, tickangle, ticklen, tickdistance,
                    tickformatmanual, tickprefix, ticksuffix, tickshow,
                    show.zero, zero.line.width, zero.line.color,
                    hovertext.format.manual, labels = NULL, num.series = 1,
                    with.bars = FALSE, tickcolor = "transparent", num.maxticks = NULL,
                    zoom.enable = TRUE)
{
    axis.type <- if (side %in% c("bottom", "top")) axisLabels$x.axis.type else axisLabels$y.axis.type
    has.line <- !is.null(linewidth) && linewidth > 0
    if (!tickshow)
        tickfont$size <- 0

    if (!is.null(labels) && any(!is.na(labels)) && any(nzchar(labels) > 0) &&
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
    tickmode <- ticks$mode
    tick0 <- NULL
    tickformat <- checkD3Format(tickformatmanual, axis.type)
    hoverformat <- checkD3Format(hovertext.format.manual, axis.type, "Hovertext")

    # Categorical axis of a bar chart
    if ((!axisLabels$labels.on.x) && side %in% c("left","right"))
    {
        autorange <- FALSE
        if (axis.type == "date")
        {
            is.autorange <- is.null(ticks$range)
            range <- rev(getDateAxisRange(axisLabels$ymd, range))

            # Check whether or not to override tick positions
            if (with.bars && is.autorange)
            {
                tmp.dist <- setDateTickDistance(axisLabels$ymd, num.maxticks)
                if (!is.null(tmp.dist))
                {
                    tickmode <- "linear"
                    tick0 <- axisLabels$ymd[1]
                    tickdistance <- tmp.dist
                }
            }
        }
        else if (axis.type == "numeric")
        {
            # Create a fake axis to specify axis type
            if (is.null(range))
                range <- calcRangeNumeric(axisLabels$labels)
            range <- rev(range)
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
    else if (axis.type == "date" && length(axisLabels$ymd) >= 2)
    {
        is.autorange <- is.null(ticks$range)
        autorange <- FALSE
        rev <- length(range) == 2 && range[2] < range[1]
        range <- getDateAxisRange(axisLabels$ymd, range)
        if (ticks$autorange == "reversed" || rev)
            range <- rev(range)

        # Check whether or not to override tick positions
        if (is.autorange && length(axisLabels$labels) <= 10)
        {
            tmp.dist <- setDateTickDistance(axisLabels$ymd, num.maxticks)
            if (!is.null(tmp.dist))
            {
                tickmode <- "linear"
                tick0 <- axisLabels$ymd[1]
                tickdistance <- tmp.dist
            }
        }
    }
    else if (axis.type == "category" && !is.null(axisLabels$ymd))
    {
        if (!is.null(range))
        {
            rev <- range[1] > range[2]
            range <- calcRangeCategorical(as.character(axisLabels$ymd))
            if (rev)
                range <- rev(range)
        }
    }

    rangemode <- "normal"
    if (axis.type == "numeric" && show.zero)
        rangemode <- "tozero"
    if (zero.line.width == 0)
    {
        if (gridwidth == 0)
            zero.line.color <- "transparent"
        else
            zero.line.color <- gridcolor # if no zero line, draw a normal grid line at zero
    } else
        zero.line.color <- "transparent" # hide plotly zero line so we can draw one on top of the traces

    # Specify max number of ticks
    nticks <- NULL
    independ.axis <- side %in% c("top", "bottom") == axisLabels$labels.on.x
    if (axis.type != "category" && tickmode == "auto" && independ.axis &&
        !(length(axisLabels$labels) == 1 && is.numeric(axisLabels$labels)))
        nticks <- min(length(axisLabels$labels) + 1, 11)

    if (any(nzchar(title)))
        title <- list(text = title, font = titlefont)
    else
        title <- NULL

    if (!is.null(num.maxticks) && tickmode == "auto" && axis.type != "category")
    {
        # plotly only uses nticks when tickmode is auto
        if (is.null(nticks) || nticks > num.maxticks)
            nticks <- num.maxticks
    }
    return (list(title = title, fixedrange = !zoom.enable,
                 side = side, type = axis.type,
                 tickfont = tickfont,
                 showline = has.line, linecolor = linecolor,
                 linewidth = if (!has.line) NULL else linewidth,
                 showgrid = gridwidth > 0, gridwidth = gridwidth,
                 gridcolor = gridcolor, tickmode = tickmode, nticks = nticks,
                 tickvals = ticks$tickvals, ticktext = ticks$ticktext,
                 ticklabelposition = "outside", tickangle = tickangle,
                 ticklen = ticklen, tickfont = tickfont,
                 tickcolor = tickcolor,
                 dtick = tickdistance, tickformat = tickformat, tick0 = tick0,
                 tickprefix = tickprefix, ticksuffix = ticksuffix,
                 hoverformat = hoverformat, layer = "below traces",
                 autorange = autorange, range = range, rangemode = rangemode,
                 zeroline = show.zero, zerolinewidth = gridwidth,
                 zerolinecolor = zero.line.color,
                 showexponent="all", showtickprefix=TRUE, showticksuffix=TRUE,
                 showticklabels=tickshow))
}

zerolines <- function(x.zero, x.zero.line.width, x.zero.line.color, x.zero.line.dash, y.zero, y.zero.line.width, y.zero.line.color, y.zero.line.dash)
{
    result <- NULL
    if (isTRUE(x.zero))
        result <- list(type = "line", layer = "above",
            x0 = 0, x1 = 0, xref = "x", y0 = 0, y1 = 1, yref = "paper",
            line = list(color = x.zero.line.color, width = x.zero.line.width, dash = tolower(x.zero.line.dash)))
    if (isTRUE(y.zero))
    {
        y.zeroline <- list(type = "line", layer = "above",
            y0 = 0, y1 = 0, yref = "y", x0 = 0, x1 = 1, xref = "paper",
            line = list(color = y.zero.line.color, width = y.zero.line.width, dash = tolower(y.zero.line.dash)))
        if (is.null(result))
            return (y.zeroline)
        else
            result <- list(result, y.zeroline)
    }
    return (result)
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

#' @importFrom verbs Sum
setMarginsForAxis <- function(margins, labels, axis)
{
    if (!is.character(labels) && !is.null(labels$labels))
        labels <- labels$labels
    lab.len <- 0
    lab.nline <- 0
    lab.nchar <- 1

    # remove space added to de-dup labels
    labels <- gsub("&#8203;", "", labels, fixed = TRUE)

    lab.nchar <- max(c(0, nchar(unlist(strsplit(split="<br>", as.character(labels))))), na.rm = TRUE)
    font.asp <- fontAspectRatio(axis$tickfont$family)
    lab.len <- font.asp * axis$tickfont$size * lab.nchar * 1.25
    lab.nline <- if (is.character(labels) && any(nzchar(labels))) max(0, sapply(gregexpr("<br>", labels),
                     function(x) Sum(x > -1, remove.missing = FALSE)), na.rm = TRUE)
                 else 0

    new.margin <- 0
    if (lab.len > 2 || (!is.null(lab.nline) && lab.nline > 0))
        new.margin <- lab.len

    title.nline <- 0
    if (any(nzchar(axis$title)) && !isTRUE(axis$title == " "))
        title.nline <- Sum(gregexpr("<br>", axis$title)[[1]] > -1, remove.missing = FALSE) + 1
    title.pad <- max(0, axis$title$font$size) * title.nline * 1.25

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

#' @importFrom verbs Sum
setMarginsForText <- function(margins, title, subtitle, footer,
                        title.font.size, subtitle.font.size, footer.font.size)
{
    title.nline <- 0
    if (any(nzchar(title)))
    {
        title.nline <- Sum(gregexpr("<br>", title)[[1]] > -1, remove.missing = FALSE) + 1
        margins$t <- margins$t + (title.font.size * title.nline * 1.25)
    }
    if (any(nzchar(subtitle)))
    {
        # We leave twice the space for subtitles, because titles are always
        # positioned halfway down the top margin
        subtitle.nline <- Sum(gregexpr("<br>", subtitle)[[1]] > -1, remove.missing = FALSE) + 1.5
        margins$t <- margins$t + (subtitle.font.size * subtitle.nline) * 0.8 * 2
    }
    if (any(nzchar(footer)) && !isTRUE(footer == " "))
    {
        footer.nline <- Sum(gregexpr("<br>", footer)[[1]] > -1) + 4
        margins$b <- margins$b + (footer.font.size * footer.nline * 1.25)
    }
    margins
}

setMarginsForLegend <- function(margins, showlegend, legend, text,
                                type = "", right.axis = FALSE)
{
    if (showlegend && legend$x > 0.99)
    {
        # Needed to preserve subtitle alignment
        if (is.factor(text))
            text <- levels(text)
        len <- max(c(0, nchar(unlist(strsplit(split = "<br>", text)))), na.rm = TRUE)
        margins$r <- min(300, 70 + (legend$font$size * max(0, len) * 0.7))
    } else if (type != "radar" && !right.axis)
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


# setTitle, setSubtitle and setFooter return a list which can be used
# in plotly to create an annotation. The allowed names of the
# components in the list are described in
# https://plotly.com/r/reference/layout/annotations/
setTitle <- function(title, title.font, margins, x.align = "center")
{
    if (!any(nzchar(title)))
        return(NULL)
    x.align <- tolower(x.align)
    xpos <- switch(x.align, center = 0.5, left = 0, right = 1)
    return(list(text = title, font = title.font, align = x.align,
                xref = "paper", x = xpos, xanchor = x.align,
                yref = "paper", y = 1.0, yanchor = "middle",
                yshift = margins$t * 0.5, showarrow = FALSE))
}

setSubtitle <- function(subtitle, subtitle.font, margins, x.align = "center")
{
    if (!any(nzchar(subtitle)))
        return(NULL)
    x.align <- tolower(x.align)
    xpos <- switch(x.align, center = 0.5, left = 0, right = 1)
    return(list(text = subtitle, font = subtitle.font,
                xref = "paper", x = xpos, xanchor = x.align,
                yref = "paper", y = 1.0, yanchor = "bottom", showarrow = FALSE))
}

setFooter <- function(footer, footer.font, margins, x.align = "center")
{
    if (!any(nzchar(footer)))
        return(NULL)
    x.align <- tolower(x.align)
    xpos <- switch(x.align, center = 0.5, left = 0, right = 1)

    footer.nline <- Sum(gregexpr("<br>", footer)[[1]] > -1, remove.missing = FALSE) + 1
    footer.npad <- max(0, ceiling(margins$b/footer.font$size/1.25) - footer.nline - 2)
    footer <- paste0("&nbsp;", paste(rep("<br>", footer.npad), collapse = ""), footer)
    return(list(text = footer, font = footer.font, align = x.align,
                xref = "paper", x = xpos, yref = "paper", y = 0.0,
                yanchor = "top", xanchor = x.align, showarrow = FALSE))
}

#' Convert text to numeric data
#'
#' This differs from as.numeric in that it returns NULL
#' instead of NA if there is no valid output.
#' Also performs basic substitution such as removing spaces and commas.
#' @param x The text to be converted to numbers
#' @export
charToNumeric <- function(x)
{
    if (!is.character(x))
        return(x)
    if (length(x) == 0 || is.na(x))
        return(NULL)
    if (nchar(x) == 0)
        return(NULL)

	x.orig <- x
    x <- gsub(" ", "", x)
    x <- gsub(",", "", x) # e.g. '5,000'
    xnum <- suppressWarnings(as.numeric(x))
    xnum <- xnum[!is.na(xnum)]
    if (length(xnum) != 1)
	{
		warning("Value '", x.orig, "' is not numeric.")
        return(NULL)
	}
    return(xnum)
}

# Modified warning message
charToDate <- function(x)
{
	x <- as.character(x)
    if (length(x) == 0 || is.na(x))
        return(NULL)
    if (nchar(x) == 0)
        return(NULL)

	res <- AsDate(x, on.parse.failure = FALSE)
	if (any(is.na(res)))
		warning("Value '", x, "' could not be parsed as a date.")
	return(res)
}

charToDateTime <- function(x)
{
	x <- as.character(x)
	res <- AsDateTime(x, on.parse.failure = FALSE)
	if (any(is.na(res)))
		warning("Value '", x, "' could not be parsed as a date.")
	return(res)
}

isBlank <- function(x)
{
    if (is.null(x))
        return(TRUE)
    if (is.na(x))
        return(TRUE)
    if (x == "")
        return(TRUE)
    return(FALSE)
}

# This is only applied to the values axis.
# It can handle categorical and date axes types but only for the values axis
# (date categorical axis range is set using getDateAxisRange in setAxis)
setValRange <- function(min, max, values, show.zero = FALSE, use.defaults = TRUE, is.bar = FALSE)
{
    if (is.null(min) || is.na(min) || min == "")
        min <- NULL
    if (is.null(max) || is.na(max) || max == "")
        max <- NULL

    # If no range is specified, then use defaults
    if (use.defaults && is.null(min) && is.null(max))
        return(list(min = NULL, max = NULL))

    if (is.list(values) && !is.null(values$labels.on.x))
    {
        axis.type <- if (values$labels.on.x) values$x.axis.type else values$y.axis.type
        if (axis.type == "date")
            values <- values$ymd
        else if (axis.type %in% c("numeric", "linear"))
            values <- suppressWarnings(as.numeric(values$labels))
        else
            values <- 0:(length(values$labels)-1)
    }

    if (length(values) == 1 && is.bar && is.null(min) && is.null(max))
        return(list(min = NULL, max = NULL))
    if (is.factor(values) || is.character(values))
        values <- as.numeric(as.factor(values)) - 1
    if (inherits(values, "POSIXct"))
    {
		if (!is.null(min) && !is.na(min))
			min <- charToDateTime(min)
		if (!is.null(max) && !is.na(max))
			max <- charToDateTime(max)

    } else if (inherits(values, "Date") || inherits(values, "POSIXct"))
    {
		if (!is.null(min) && !is.na(min))
			min <- charToDate(min)
		if (!is.null(max) && !is.na(max))
			max <- charToDate(max)

    } else
    {
        min <- charToNumeric(min)
        max <- charToNumeric(max)
    }
    # When values is only a single value don't use value to determine range
    if (length(values) == 1)
        return(list(min = min, max = max))

    if  (length(min) == 0 || is.na(min))
        min <- min(unlist(values), if (show.zero) 0 else NULL, na.rm = TRUE)
    if  (length(max) == 0 || is.na(max))
        max <- max(unlist(values), na.rm = TRUE)

    if (is.bar && length(values) > 1)
    {
        diff <- if (length(values) > 1) min(abs(diff(values)), na.rm = TRUE)/2
                else                    0.5
        min <- min - diff
        max <- max + diff
    }
    return(list(min = min, max = max))
}


#' @importFrom flipU StopForUserError
setTicks <- function(minimum, maximum, distance, reversed = FALSE,
                data = NULL, labels = NULL, type="scatter", label.font.size = 10, is.bar = FALSE)
{
    #if (is.null(minimum) != is.null(maximum))
    #    warning("To specify the range of an axis, you must specify both the minimum and maximum values.")
    if ((is.null(minimum) || is.null(maximum)) && !is.null(distance))
        StopForUserError("If specifying the distance between ticks on an axis,",
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
        dat_min <- min(0, data, na.rm = TRUE)
        dat_max <- max(data, na.rm = TRUE)
        if (dat_min == dat_max)
            dat_max <- 0

        # Add horizontal space for data labels in bar charts
        pad <- 0
        lab.len <- 1
        if (!is.null(labels) && is.bar)
        {
            lab.len <- max(nchar(as.character(unlist(labels))))
            pad <- (dat_max - dat_min) * (lab.len+2) * label.font.size / 200
        }
        if (!is.bar || dat_min < 0)
            dat_min <- dat_min - pad
        if (dat_max > 0)
            dat_max <- dat_max + pad
        if (is.null(minimum))
            minimum <- dat_min
        if (is.null(maximum))
            maximum <- dat_max
    }

    if (!is.null(minimum) && !is.null(maximum))
    {
        autorange <- FALSE
        range <- c(minimum, maximum)
        if (reversed)
            range <- rev(range)
    }
    if (!is.null(distance) && is.numeric(minimum) && is.numeric(maximum))
    {
        # error msg if axis is not numeric
        autorange <- FALSE
        mode <- "array"
        if (is.bar && distance == round(distance))
        {
            minimum <- ceiling(minimum)
            maximum <- floor(maximum)
        }
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

# Takes a single string and puts <br> in place of the closest space
# preceding the word at the max.nchar position.
# E.g. if n = 20 then count 20 characters.  The space preceding character 20
# is replaced by "<br>".
#' @importFrom flipU StopForUserError
#' @noRd
lineBreakEveryN <- function(x, max.nchar = 21, remove.empty = TRUE)
{
    if (max.nchar <= 0)
        StopForUserError("Wrap line length cannot be smaller than 1")

    patt <- if (remove.empty) "\\s+"
            else              " "

    tot.nchar <- nchar(x)
    next.wb <- regexpr(patt, x, perl = TRUE)
    if (next.wb == -1 || is.na(next.wb))
        return(x)
    x <- TrimWhitespace(x)
    final <- ""
    cur.nchar <- 0
    while (next.wb != -1)
    {
        # Find position of next wordbreak or html tag
        next.wb <- regexpr(patt, x, perl = TRUE)
        next.html <- regexpr("<.*?>", x, perl = TRUE)
        line.is.empty <- cur.nchar == 0
        if (next.wb == -1)
            break

        # Decide whether to place a line break before the
        # next html tag or word (whichever comes first)
        if (next.html != -1 && next.html < next.wb)
        {
            tmp.text <- substr(x, 1, next.html + attr(next.html, "match.length") - 1)
            if (tmp.text == "<br>")
            {
                final <- paste0(final, tmp.text)
                cur.nchar <- 0

            } else if (next.html == 1)
            {
                final <- paste0(final, tmp.text)
                # cur.nchar and unchanged

            } else if (!line.is.empty && cur.nchar + next.html - line.is.empty > max.nchar)
            {
                final <- paste0(final, "<br>", tmp.text)
                cur.nchar <- next.html - 1

            } else
            {
                final <- paste(final, tmp.text, sep = if (line.is.empty) "" else " ")
                cur.nchar <- cur.nchar + next.html - line.is.empty
            }
            x <- substr(x, next.html + attr(next.html, "match.length"), tot.nchar)

        } else
        {
            tmp.text <- substr(x, 1, next.wb - 1)
            if (next.wb == 1)
            {
                # final and cur.nchar unchanged

            } else if (!line.is.empty && cur.nchar + next.wb > max.nchar)
            {
                final <- paste0(final, "<br>", tmp.text)
                cur.nchar <- next.wb - 1

            } else
            {
                final <- paste(final, tmp.text, sep = if (line.is.empty) "" else " ")
                cur.nchar <- cur.nchar + next.wb - line.is.empty
            }
            x <- substr(x, next.wb + attr(next.wb, "match.length"), tot.nchar)
        }
    }
    if (nchar(final) == 0)
        final <- TrimWhitespace(x)
    else if (nchar(x) > 0)
        final <- paste(final, x, sep = if (cur.nchar + nchar(x) + 1 > max.nchar) "<br>" else " ")
    return(final)
}


#' Format long labels for html by truncating and wrapping
#'
#' @param x Character; the text to format.
#' @param wordwrap Logical; whether line breaks ('<br>') should be inserted.
#' @param n Integer; the maximum number of characters before a line breaks is added. The break is added at the nearest word boundary.
#' @param truncate Logical; whether long labels should be truncated and appended with '...' instead.
#' @param remove.empty Logical; whether to remove zero-length strings
#' @export
autoFormatLongLabels <- function(x, wordwrap = FALSE, n = 21, truncate = FALSE, remove.empty = TRUE)
{
    if (truncate)
        warning("autoFormatLongLabels: truncate not longer does anything.")
    if (is.null(x))
        return("")
    if (!is.character(x))
        x <- as.character(x)

    # Check for zero-length strings which are ignored by plotly
    if (length(x) > 1)
    {
        ind <- which(sapply(x, nchar) == 0)
        if (length(ind) > 0)
            x[ind] <- " "
    }

    output.text <- x
    if (wordwrap && length(output.text) > 0)
        output.text <- sapply(output.text, function(x) lineBreakEveryN(x, n, remove.empty = remove.empty))
    if (length(output.text) == 0 || all(is.na(output.text)))
        output.text <- ""

    attributes(output.text) <- NULL
    output.text
}

stripClassAndCallFromXtabs <- function(chart.matrix)
{
    if (inherits(chart.matrix, "xtabs") || inherits(chart.matrix, "table"))
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
#' @importFrom verbs SumEmptyHandling
#' @return integer
decimalsFromD3 <- function(format, default = 0)
{
    if (length(format) == 0 || format == "")
        return(default)
    # If no matches, the result will be a length 0 numeric value
    return(SumEmptyHandling(as.numeric(regmatches(format, regexpr("\\d+", format))), remove.missing = FALSE))
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
#' Returns a strings according to the d3 format specified.
#' @noRd
#' @param x Input data (may be a vector) to format
#' @param format D3 formatting string. Accepts percentages, numeric and scientific notation
#' @param prefix Optional string to prepend to output
#' @param suffix Optional string to append to output
#' @param decimals Default number of decimals shown; used if not specified in \code{format}
formatByD3 <- function(x, format, prefix = "", suffix = "", percent = FALSE, decimals = 2)
{
    n <- length(x)
    is.vectorized <- FALSE
    if (n > 1 && length(format) > 1)
    {
        is.vectorized <- TRUE
        format <- vectorize(format, n)
        prefix <- vectorize(prefix, n)
        suffix <- vectorize(suffix, n)
        return(sapply(1:n, function(i)
            formatByD3(x[i], format[i], prefix[i], suffix[i], percent = percent, decimals = decimals)))
    }

    x.str <- as.character(x)
    if (format == "Category")
        return(x.str)
    if (is.numeric(x))
    {
        use.comma <- commaFromD3(format) || format == ""
        big.mark <- if (use.comma) "," else ""
        tmp.fmt <- gsub("[^deEfgGs]", "", format)
        tmp.fmt <- gsub("s", "g", tmp.fmt) # switch d3 SI-prefix format to scientific
        num.decimals <- decimalsFromD3(format, decimals)

        if (percentFromD3(format) || percent)
        {
            num.decimals <- decimalsFromD3(format, 0)
            x.str <- paste0(formatC(round_half_up(x*100, num.decimals),
                format = "f", digits = num.decimals, big.mark = big.mark), "%")
        }
        else if (!any(nzchar(tmp.fmt)) || tmp.fmt == "f")
            x.str <- FormatAsReal(x, decimals = num.decimals, comma.for.thousands = use.comma)
        else
            x.str <- formatC(x, format = tmp.fmt, digits = num.decimals, big.mark = big.mark)
    }
    if (inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXt"))
        x.str <- format(x, format)
    x.str <- paste0(prefix, x.str, suffix)

    x.missing <- !is.finite(x)
    if (is.vectorized && any(x.missing))
        x.str[x.missing] <- ""
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
# convert is only used by LabeledScatter
checkD3Format <- function(format, axis.type, warning.type = "Axis label", convert = FALSE)
{
    # This is to avoid LabeledScatter's problem with date formatting
    if (convert && axis.type == "date" && !any(nzchar(format)))
        return("%b %d %Y")

    if (!any(nzchar(format)))
        return("")
    if (substr(format, nchar(format), nchar(format)) %in% c("", 0:9)) # automatic formatting
    {
        if (axis.type == "numeric")
            return(paste0(format, "f"))
        else
            return(format)
    }
    mismatch <- FALSE
    if (d3FormatType(format) != axis.type)
    {
        warning(warning.type, " format of type '", d3FormatType(format),
                "' incompatible with axis type '", axis.type, "'")
        mismatch <- TRUE
    }
    if (format == "%")
        return ("0%")
    if (convert && axis.type == "date" && d3FormatType(format) != axis.type)
        return("%b %d %Y")
    if (mismatch)
        return ("")
    if (convert)
        format <- sub("^~", "", format)
    return(format)
}

notAutoRange <- function(axis)
{
    return(!isTRUE(axis$autorange) && length(axis$range) > 0 &&
            all(!is.na(axis$range)) && min(abs(axis$range)) > 0)
}

getSign <- function(values, axis)
{
    res <- sign(values)
    if (any(is.na(res)))
        res[which(is.na(res))] <- 0
    if (length(axis$range) >= 2 && isTRUE(axis$range[2] < axis$range[1]))
        res <- -res
    else if (axis$autorange == "reversed")
        res <- -res
    return(res)
}

#' use black or white for good contrast against colors
#'
#' @param colors vector of colors which will be the background color of the
#' @importFrom grDevices col2rgb rgb2hsv
autoFontColor <- function (colors)
{
    tmp.rgb <- col2rgb(colors)
    tmp.lum <- apply(tmp.rgb, 2, function(x) return(0.299*x[1] + 0.587*x[2] + 0.114*x[3]))
    return(ifelse(tmp.lum > 126, "#2C2C2C", "#FFFFFF"))
}

# By default vectorize will convert x into a vector of length n
# However, if nrow is specified, it will convert to a matrix of nrow x n columns
vectorize <- function(x, n, nrow = NULL, split = ",")
{
    input.is.matrix <- length(dim(x)) >= 2
    if (!is.null(nrow) && is.finite(nrow))
        n <- n * nrow

    if (is.logical(x))
        res <- suppressWarnings(rep(TRUE, n) & x)
    else if (is.numeric(x))
        res <- suppressWarnings(rep(0, n) + x)
    else
    {
        if (!is.null(split))
            x <- TextAsVector(x, split = split)
        res <- suppressWarnings(paste0(x, rep("", n)))
    }
    if (!is.null(nrow))
        res <- matrix(res, nrow = nrow, byrow = !input.is.matrix)
    return(res)
}


getColumn <- function(x, i)
{
    res <- x
    if (length(dim(x)) == 2)
        res <- x[,i,drop = FALSE]
    if (length(dim(x)) == 3)
        res <- x[,i, , drop = FALSE]
    if (!is.null(attr(x, "statistic", exact = TRUE)))
        attr(res, "statistic") <- attr(x, "statistic")
    return(res)
}


readLineThickness <- function(line.thickness, n)
{
    if (is.character(line.thickness))
    {
        tmp.txt <- TextAsVector(line.thickness)
        line.thickness <- suppressWarnings(as.numeric(tmp.txt))
        na.ind <- which(is.na(line.thickness))
        if (length(na.ind) == 1)
            warning("Non-numeric line thickness value '", tmp.txt[na.ind], "' was ignored.")
        if (length(na.ind) > 1)
            warning("Non-numeric line thickness values '",
            paste(tmp.txt[na.ind], collapse = "', '"), "' were ignored.")
    }
    line.thickness <- suppressWarnings(line.thickness * rep(1, n)) # suppress warnings about recyling
    return(line.thickness)
}

# Returns true if the d3 format corresponds to the output
# of PrepareNumbers when the user has set Number Type to "Automatic"
isAutoFormat <- function(x)
{
    if (isTRUE(nchar(x) == 0))
        return(TRUE)
    if (isTRUE(x == ""))
        return(TRUE)
    if (isTRUE(grepl("\\d$", x)))
        return(TRUE)
    return(FALSE)
}

isPercentData <- function(data)
{
    stat <- attr(data, "statistic", exact = TRUE)
    if (isTRUE(grepl("%", stat)))
        return(TRUE)
    ndim <- length(dim(data))
    if (is.null(stat) && !is.null(dimnames(data)))
    {
        stat <- dimnames(data)[[ndim]][1]
        if (ndim >= 3 || all(c("questions", "name") %in% names(attributes(data))))
            return(isTRUE(grepl("%", stat)))
    }
    return(FALSE)
}

# y.tick.format, y.hovertext.format, y2.tick.format, y2.hovertext.format
# data.label.format
checkSuffixForExtraPercent <- function(suffix, format)
{
    new.suffix <- suffix
    for (i in seq_along(suffix))
    {
        if (isTRUE(grepl("%", format[i])) && isTRUE(grepl("%", suffix[i])))
            new.suffix[i] <- sub("%", "", suffix[i])
    }
    if (any(new.suffix != suffix))
        warning("A percentage sign is automatically added to percent data. ",
            "The first '%' in the suffix will be ignored.")
    return(new.suffix)
}

validNonNegativeInteger <- function(x)
    is.integer(x) && !is.na(x) && x > 0
