#' This function adds traces for data labels and annotations to bar/column charts
#' Because these type of charts show bars that up space surrounding the
#' position of the data point, it is necessary to be more careful about
#' the offset of the data labels (from the data point). Also, circle
#' annotations can be added to the plot. These are added as separate traces
#' to allow for more flexible positioning
#' @param p the plotly plot object to which the trace is added
#' @param type the type of the chart (i.e. Bar or Column)
#' @param name the name to be given to the data label trace. This is visible in the hover text
#' @param data.label.xpos The position of the data label trace on the xaxis.
#'  This is usually a component of the output from \code{dataLabelPositions}.
#' @param data.label.ypos The position of the data label trace on the yaxis.
#'  This is usually a component of the output from \code{dataLabelPositions}.
#' @param data.label.show A logical vector of the same length as data.label.xpos
#'  indicating whether a data label will be shown at each point.
#' @param data.label.text A text vector of the same length as data.label.xpos
#'  containing the text to show on the data label.
#' @param data.label.sign The sign indicating whether the data point is positive or negative.
#' @param data.label.nchar The maximum number of characters in the data labels.
#'  This is used to position the circle annotations.
#' @param annotation.list A list of annotations as given to the charting function.
#' @param annot.data The data (usually a 3d array) used create the annotations.
#' @param i The index of the data series. Used to control legend.group
#' @param xaxis The name of the xaxis
#' @param yaxis The neme of the yaxis
#' @param data.label.font A list specifying the font to use.
#' @param is.stacked A logical indicating if the chart is stackeed.
#' @param stackgroupname For stacked charts, the current trace will be added on top of
#'  the previous trace with the same stackgroupname
#' @param data.label.centered A logical indicating if data label is placed at the center of bar.
#'  Only used in Stacked Column charts.
#' @param data.label.horizontal.align Text to control the horizontal alignment
#'  of labels on the column chart annotations


#' @importFrom verbs SumEmptyHandling
#' @keywords internal
addTraceForBarTypeDataLabelAnnotations <- function(p, type, name,
        data.label.xpos, data.label.ypos,
        data.label.show, data.label.text, data.label.sign, data.label.nchar,
        annotation.list, annot.data, i,
        xaxis, yaxis, data.label.font, is.stacked, data.label.centered,
        data.label.horizontal.align = "center", stackgroupname = "datalabel")
{
    if (type == "Column")
    {
        if (is.stacked)
            data.label.sign <- -1 * data.label.sign
        if (is.stacked && data.label.centered)
            textalign <- paste("middle", data.label.horizontal.align)
        else
            textalign <- paste(ifelse(data.label.sign >= 0, "top", "bottom"), data.label.horizontal.align)
        data.label.pos <- ifelse(data.label.sign < 0, 3, 0 + (is.stacked & !data.label.centered))
    } else
    {
        textalign <- if (is.stacked || data.label.centered) "middle center"
                     else            ifelse(data.label.sign >= 0, "middle right", "middle left")
        data.label.pos <- if (is.stacked) 0
                          else            ifelse(data.label.xpos < 0, 7, 3)
    }
    n <- length(data.label.xpos)

    # Find space to leave for circles
    max.diam <- 0
    for (j in seq_along(annotation.list))
    {
        annotation.list[[j]]$threshold <- parseThreshold(annotation.list[[j]]$threshold)
        a.tmp <- annotation.list[[j]]
        if (grepl("Circle", a.tmp$type))
        {
            if (a.tmp$type != "Circle - filled")
            {
                a.tmp$size <- a.tmp$size + 5
                annotation.list[[j]]$size <- a.tmp$size
            }
            if (a.tmp$size > max.diam)
                max.diam <- a.tmp$size + 0.01
        }
    }

    # Add trace adding circle annotations
    for (j in seq_along(annotation.list))
    {
        a.tmp <- annotation.list[[j]]
        if (grepl("Circle", a.tmp$type))
        { 
            # shiftleft and shiftright elements could be NULL or NA and should have zero padding then.
            tmp.dat <- getAnnotData(annot.data, a.tmp$data, i)
            ind.sel <- extractSelectedAnnot(tmp.dat, a.tmp$threshold, a.tmp$threstype)
            tmp.text <- rep("", n)
            left.pad <- paste(rep(" ", SumEmptyHandling(a.tmp$shiftright)), collapse = "")
            right.pad <- paste(rep(" ", SumEmptyHandling(a.tmp$shiftleft)), collapse = "")
            tmp.text[ind.sel] <- paste0(left.pad, switch(a.tmp$type,
                "Circle - thick outline" = "<b>&#11096;</b>",
                "Circle - thin outline" = "&#11096;",
                "Circle - filled" = "&#11044;"), right.pad)
            tmp.font <- list(family = data.label.font$family, color = a.tmp$color, size = a.tmp$size)

            # Adjusting circle position
            tmp.pos <- 0.01         # setting to 0 will result in default = 3 being used
            if (!is.stacked)
                tmp.pos <- max(0.01, (max.diam - a.tmp$size))
            if (type == "Bar" && !is.stacked)
                tmp.pos <- tmp.pos + (data.label.nchar * data.label.font$size * 0.3)
            if (type == "Column" && !is.stacked)
                tmp.pos <- tmp.pos + (data.label.sign < 0) * 5

            p <- addAnnotScatterTrace(p, xpos = data.label.xpos, ypos = data.label.ypos,
                  text = tmp.text, textfont = tmp.font, textposition = textalign,
                  marker = list(opacity = 0.0, color = "red", size = tmp.pos),
                  xaxis = xaxis, yaxis = yaxis, hoverinfo = "skip", 
                  stackgroup = if (is.stacked) paste0("circle", j) else "",
                  orientation = if (type == "Bar") "h" else "v", legendgroup = i)

            # Add other half of the trace to center the data labels
            if (is.stacked && (data.label.centered || type == "Bar"))
            p <- addAnnotScatterTrace(p, xpos = data.label.xpos, ypos = data.label.ypos, text = "",
                    yaxis = yaxis, xaxis = xaxis, stackgroup = paste0("circle", j),
                    hoverinfo = "skip", marker = list(opacity = 0.0), 
                    orientation = if (type == "Bar") "h" else "v", legendgroup = i)
        }
    }

    # Add data annotations
    tmp.offset <- if (!is.stacked) max(0, (max.diam - data.label.font$size))
                  else             0.01
    data.label.pos <- data.label.pos + tmp.offset
    data.label.text[!data.label.show] <- ""

    # Add data labels for positive or non-stacked values
    p <- addAnnotScatterTrace(p, name = name,
              xpos = data.label.xpos, ypos = data.label.ypos, text = data.label.text,
              marker = list(opacity = 0.0, size = data.label.pos),
              xaxis = xaxis, yaxis = yaxis, textfont = data.label.font,
              textposition = textalign, hoverinfo = "skip",
              stackgroup = if (is.stacked) stackgroupname else "",
              orientation = if (type == "Bar") "h" else "v", legendgroup = i)

    # Add other half of the trace to center the data labels
    if (is.stacked && (data.label.centered || type == "Bar"))
        p <- addAnnotScatterTrace(p, xpos = data.label.xpos, ypos = data.label.ypos, text = "",
                yaxis = yaxis, xaxis = xaxis, stackgroup = stackgroupname,
                hoverinfo = "skip", marker = list(opacity = 0.0), 
                orientation = if (type == "Bar") "h" else "v", legendgroup = i)

    return(p)
}

addAnnotScatterTrace <- function(p, orientation, xpos, ypos, text, stackgroup, ...)
{
    # If no stacking is performed, then just create scatter trace as usual
    tmp.fill <- "none"
    if (any(nzchar(stackgroup)))
    { 
        tmp.fill <- if (orientation == "h") "tonextx" else "tonexty"

        # Separate out positive and negative values into separate traces
        # So that datalabels can be added in the same way as barmode = relative
        ind.neg <- NULL
        neg.text <- NULL
        if (orientation == "v")
        {
            ind.neg <- which(ypos < 0)
            if (length(ind.neg) > 0)
            {
                neg.ypos <- ifelse(ypos < 0, ypos, 0)
                neg.xpos <- xpos
                if (any(nzchar(text)))
                {
                    neg.text <- ifelse(ypos < 0, text, "")
                    text[ind.neg] <- ""
                }
                ypos[ind.neg] <- 0
            }
        } else
        {
            ind.neg <- which(xpos < 0)
            if (length(ind.neg) > 0)
            {
                neg.xpos <- ifelse(xpos < 0, xpos, 0)
                neg.ypos <- ypos
                if (any(nzchar(text)))
                {
                    neg.text <- ifelse(xpos < 0, text, "")
                    text[ind.neg] <- " "
                }
                xpos[ind.neg] <- 0
            }
        }

        if (length(ind.neg) > 0)
            p <- add_trace(p, x = neg.xpos, y = neg.ypos, cliponaxis = FALSE,
                    text = neg.text, mode = if (is.null(neg.text)) "markers+text" else "markers+text",
                    type = "scatter", fillcolor = "transparent", fill = tmp.fill,
                    orientation = orientation, showlegend = FALSE, 
                    stackgroup = paste0("neg", stackgroup), ...)
    }

    # Normal scatter trace
    if (length(xpos) == 1)
    {
        # Trying to avoid plotly bug with adding a single point
        xpos <- rep(xpos, 2)
        ypos <- rep(ypos, 2)
        text <- rep(text, 2)
    }
    p <- add_trace(p, x = xpos, y = ypos, cliponaxis = FALSE,
            text = text, mode = if (!is.null(text)) "markers+text" else "markers",
            type = "scatter", fillcolor = "transparent", fill = tmp.fill,
            orientation = orientation, showlegend = FALSE, stackgroup = stackgroup, ...)
    p
} 




getAnnotData <- function(data, name, series, as.numeric = TRUE)
{
    # If no annotation data is specified use chart data
    if (all(!nzchar(name)))
    {
        new.dat <- checkMatrixNames(data)
        if (length(dim(new.dat)) >= 2)
            return(new.dat[,series])
        else
            return(new.dat)
    }

    if (is.null(data))
        stop("No data has been provided for annotations")
    if (is.null(dim(data)))
        data <- as.matrix(data)

    d.dim <- dim(data)
    d.len <- length(d.dim)
    if (!is.null(attr(data, "statistic")))
        d.names <- attr(data, "statistic")
    else
    {
        d.names <- dimnames(data)[[d.len]]
        if (is.null(d.names))
            d.names <- as.character(1:d.len)
    }
    ind <- match(paste0("", name), d.names)
    if (is.na(ind))
    {
        # Check that statistic name has not been changed in PrepareData
        name2 <- gsub("%", "Percent", name)
        ind <- match(paste0("", name2), d.names)
        if (is.na(ind))
          stop("Annotation data does not contain a statistic named '", name, "'. ",
                "Allowable names are: '", paste(d.names, collapse = "', '"),
                "'. Check that DATA MANIPULATIONS > Automatically tidy the data ",
                "is not selected.")
        else
          name <- name2
    }

    match.single.stat <- isTRUE(attr(data, "statistic") == name)
    if (match.single.stat && d.len == 2)
        new.dat <- data[,series]
    else if (match.single.stat)
        new.dat <- data
    else if (d.len == 3)
        new.dat <- data[,series, ind]
    else
        new.dat <- data[,ind]
    if (as.numeric)
        new.dat <- suppressWarnings(as.numeric(new.dat))
    return(new.dat)
}

extractSelectedAnnot <- function(data, threshold, threstype)
{
    n <- NROW(data)
    if (is.null(threstype) || is.null(threshold))
        return(1:n)
    else if (threstype == "above threshold")
        return(which(data > threshold))
    else if (threstype == "below threshold")
        return(which(data < threshold))
    else
        return(which(is.na(data)))
}


#' Adds html code to the data labels include the annotation
#' @return The modified character vector \code{data.label.text}.
#' @param data.label.text A character vector containing the original data labels
#'  which is to be annotated
#' @param annotation An element of the \code{annotation.list} passed to the
#' top level charting function. The is usually a list with named elements
#' such as "type", "size", "font.family", "format". Note that this
#' function will not handle annotation of type "Circle - xxx" or "Marker border"
#' because these are implemented as additional traces.
#' @param tmp.dat A slice of \code{annot.dat} which matches data.label.text
#' It is used when \code{annotation$type} is "Text".
#' @param prepend Logical; when true, the annotation will be added to the
#  beginning of data.label.text instead of the end.
#' @importFrom verbs Sum
#' @keywords internal
addAnnotToDataLabel <- function(data.label.text, annotation, tmp.dat, prepend = FALSE)
{
    # Fix font size so that the units do not change in size when the font size increases
    left.pad <- ""
    n.shift.right <- annotation$shiftright
    if (is.null(n.shift.right) || is.na(n.shift.right))
        n.shift.right <- 0
    if (n.shift.right > 0)
        left.pad <- paste0("<span style='font-size: 2px'>",
                    paste(rep(" ", n.shift.right), collapse = ""),
                    "</span>")

    if (annotation$type == "Shadow")
        data.label.text <- paste0(left.pad, "<span style='text-shadow: 1px 1px ",
            annotation$size, "px ", annotation$color, ", -1px -1px ",
            annotation$size, "px ", annotation$color, ";'>", data.label.text, "</span>")
    else if (annotation$type == "Border")
        data.label.text <- paste0(left.pad, "<span style='outline: ", annotation$width, "px solid ",
            annotation$color, "; outline-offset: ", annotation$offset, "px;'>", data.label.text, "</span>")
    else
    {
        new.style <- ""
        if (!is.null(annotation$color))
            new.style <- paste0(new.style, "color:", annotation$color, ";")
        if (!is.null(annotation$size))
            new.style <- paste0(new.style, "font-size:", annotation$size, ";")
        if (!is.null(annotation$font.family))
            new.style <- paste0(new.style, "font-family:", annotation$font.family, ";")
        if (!is.null(annotation$font.weight))
            new.style <- paste0(new.style, "font-weight:", annotation$font.weight, ";")
        if (!is.null(annotation$font.style))
            new.style <- paste0(new.style, "font-style:", annotation$font.style, ";")

        new.text <- ""
        if (annotation$data == "Column Comparisons" && grepl("Arrow", annotation$type))
            new.text <- paste0(" ", getColCmpArrowHtml(tmp.dat, annotation$size, " ", "&#8593;"), " ")
        else if (annotation$data == "Column Comparisons" && grepl("Caret", annotation$type))
            new.text <- paste0(" ", getColCmpArrowHtml(tmp.dat, annotation$size, " ", "&#9650;"), " ")
        else if (annotation$type == "Arrow - up")
            new.text <- "&#8593;"
        else if (annotation$type == "Arrow - down")
            new.text <- "&#8595;"
        else if (annotation$type == "Caret - up")
            new.text <- "&#9650;"
        else if (annotation$type == "Caret - down")
            new.text <- "&#9660;"
        else if (annotation$type == "Custom text")
            new.text <- annotation$custom.symbol
        else if (grepl("Text", annotation$type))
            new.text <- formatByD3(tmp.dat, annotation$format, annotation$prefix, annotation$suffix)
        else if (annotation$type == "Hide")
            new.text <- ""
        if (any(nzchar(new.style)))
            new.text <- paste0("<span style='", new.style, "'>", new.text, "</span>")

        if (annotation$type == "Hide")
            data.label.text <- ""
        else if (annotation$type == "Text - before data label" || prepend)
            data.label.text <- paste0(left.pad, new.text, data.label.text)
        else
            data.label.text <- paste0(data.label.text, left.pad, new.text)
    }
    return(data.label.text)
}

# This function in used in Bar/Column/Line and only converts
# text input into numeric values because the y-axis is always numeric
# Scatterplot uses a slightly more complicated function because
# the y-axis can also be a date or categorical so the
# threshold needs to be converted accordingly.

parseThreshold <- function(x)
{
    if (is.null(x))
        return(x)

    # Tries to convert string to numeric where possible
    # Attaches statistic if a percent sign is observed
    res <- ParseText(x)
    if (is.numeric(res) && isTRUE(grepl("%", attr(res, "statistic"))))
        res <- as.numeric(res)/100

    return(res)
}

checkAnnotType <- function(annot.type, chart.type)
{
    if (is.null(annot.type))
    {
        warning("Annotation does not have a specified type and will be ignored\n")
        return(FALSE)
    }

    # These annotation types are implemented for all charts
    # which support annotations e.g. Line
    allowed.types <- c('Arrow - up', 'Arrow - down', 'Border',
       'Caret - up', 'Caret - down', "Custom text",
       'Hide', 'Shadow', 'Text - after data label', 'Text - before data label')

    # Additional annotation types only implemented on some chart types
    if (chart.type == "Bar")
        allowed.types <- c(allowed.types,
           'Circle - filled', 'Circle - thick outline', 'Circle - thin outline')
    else if (chart.type == "Scatter")
        allowed.types <- c(allowed.types, 'Marker border')

    if (!annot.type %in% allowed.types)
    {
        warning("Unknown annotation type: '", annot.type, "'. ",
                "Valid types are '", paste(allowed.types, collapse = "', '"), "'.")
        return(FALSE)

    } else
        return(TRUE)
}

getColCmpArrowHtml <- function(cell.text, arrow.size, sep = " ", arrow.code = "&#8593;")
{
    res <- rep("", length(cell.text))

    if (is.null(arrow.size)) # no html styling
    {
        prefix <- ""
        suffix <- arrow.code
    } else
    {
        prefix <- paste0("<span style='font-size:", arrow.size - 3, "px'>")
        suffix <- paste0("</span>", arrow.code)
    }

    for (i in 1:length(cell.text))
    {
        tmp <- paste0(prefix, unlist(strsplit(cell.text[i], split = "\\s+")), suffix)
        res[i] <- paste(tmp, collapse = sep)
    }
    return(res)
}


# applyAllAnnotationsToDataLabels is basically a wrapper to apply multiple annotations
# it updates both data.label.text to add html to add arrows/style elements
# and applies getPointSegmentsForPPT to update the "customPoints" attribute
# so the annotation can be correctly exported to PowerPoint.
# The circle annotations are handled in a separate trace, so they do not
# affect data.label.text, but they are still added to "customPoints".
# The customPoints attribute consists of a list where each element corresponds
# to a data point in the series with the structure described in
# https://wiki.q-researchsoftware.com/wiki/PptPointLabel
# Each point can contain a list of segments which make up different components
# of the data label, with potentially field types (text, value or category name)
# and different font/styling.

# When the "customPoints" attribute (or pt.segs) is first initialised, the entire data label
# is defined in terms of a list of segments. This makes it easier to iteratively apply
# annotations. When it is called for the last time, \code{clean.pt.segs} should be set to TRUE
# to indicate that pt.segs can be summarised to only describe differences from
# the default data labels for the whole series (typically a data label containing
# only the value of the data point).

# applyAllAnnotationsToDataLabels applies all annotations in \code{annotation.list}
# to all points in the data series indicated by \code{series.index}.
# That is it takes a slice of the annot.data[,series.index,annot$data] and
# compares this against \code{annot$threshold} to identify the data points to which
# each annotation should be applied. Data points which are not included in \code{rows.to.show}
# will be unmodified because those are points which are set to data.label.show = FALSE.

# Note that \code{annot.data} is the full 3d array of data from the charting function.
# The relevant sections of the data is extracted and passed to getPointSegmentsForPPT.
# The row index of the selected annotation data is expected
# to correspond to rows in \code{chart.matrix}
# To specify that annotations are only applied to a subset of rows, use \code{rows.to.show}.

applyAllAnnotationsToDataLabels <- function(data.label.text, annotation.list,
    annot.data, series.index, rows.to.show,
    chart.type, clean.pt.segs = FALSE)
{
    pt.segs <- attr(data.label.text, "customPoints")
    for (j in seq_along(annotation.list))
    {
        if (!checkAnnotType(annotation.list[[j]]$type, chart.type))
            return(data.label.text)
        annotation.list[[j]]$threshold <- parseThreshold(annotation.list[[j]]$threshold)
        a.tmp <- annotation.list[[j]]
        tmp.dat <- getAnnotData(annot.data, a.tmp$data, series.index,
            as.numeric = !grepl("Text", a.tmp$type) && a.tmp$data != "Column Comparisons")
        ind.sel <- intersect(rows.to.show,
                        extractSelectedAnnot(tmp.dat, a.tmp$threshold, a.tmp$threstype))
        if (length(ind.sel) > 0)
        {
            if (!grepl("Circle", a.tmp$type))
                data.label.text[ind.sel] <- addAnnotToDataLabel(data.label.text[ind.sel],
                    a.tmp, tmp.dat[ind.sel])
            pt.segs <- getPointSegmentsForPPT(pt.segs, ind.sel, a.tmp, tmp.dat[ind.sel])
        }
    }
    if (clean.pt.segs && !is.null(pt.segs))
    {
        pt.segs <- tidyPointSegments(pt.segs, length(data.label.text))
    }
    attr(data.label.text, "customPoints") <- pt.segs
    return(data.label.text)
}

# Updates \code{points} to reflect annotations in \code{annot} being applied
# at \code{index}.
# \code{points} is a list of CustomPoints which can contain fields as described in
# https://wiki.q-researchsoftware.com/wiki/PptPointLabel
# While creating this list, we assume that there is one element for each
# data point in the data series (i.e. each row of chart.matrix)
# It is only after tidyPointSegments is called, that empty elements in
# \code{points} are removed.
getPointSegmentsForPPT <- function(points, index, annot, dat)
{
    # Shape-type annotation are added in separate function
    if (!grepl("Circle|Border|Shadow|Hide", annot$type))
    {
        tmp.seg <- list(list(Font = setFontForPPT(annot),
                        Text = setTextForPPT(annot)))
    }

    for (i in 1:length(index))
    {
        # Set text only if it depends on the data
        # Note that we use i to select elements of dat because it is assumed that we are only
        # passing the relevant sections of the data (i.e. dat is already subsetted by index)
        if (grepl("^Text", annot$type))
            tmp.seg[[1]]$Text <- formatByD3(dat[i], annot$format, annot$prefix, annot$suffix)
        else if (annot$data == "Column Comparisons" && grepl("Arrow", annot$type))
            tmp.seg[[1]]$Text <- unescape_html(getColCmpArrowHtml(dat[i], NULL, " ", "&#8593;"))
        else if (annot$data == "Column Comparisons" && grepl("Caret", annot$type))
            tmp.seg[[1]]$Text <- unescape_html(getColCmpArrowHtml(dat[i], NULL, " ", "&#9650;"))

        # Update points to reflect the change specified by annot
        # note that the element of points corresponds to the row in chart.matrix
        # so ii = index[i] is used instead of i
        ii <- index[i]
        if (annot$type == "Hide") # segments still has to be appendable
            points[[ii]]$Segments <- list()
        else if (grepl("Circle|Border|Shadow", annot$type))
            points[[ii]] <- setShapeForPPT(points[[ii]], annot) # overrides previous shapes
        else if (annot$type == "Text - before data label")
            points[[ii]]$Segments <- c(tmp.seg, points[[ii]]$Segments)
        else
            points[[ii]]$Segments <- c(points[[ii]]$Segments, tmp.seg)
    }
    return(points)
}

# Tidy up empty segments and points where possible
tidyPointSegments <- function(points, num.points, show.categoryname = FALSE, index.map = NULL)
{
    if (length(points) == 0)
        return(points)
    pt.info <- integer(num.points)  # 0 = no label; 1 = value-only label; 2 = has modification

    # Use index.map for scatterplots where CustomPoint$Index uses global position
    .posFromIndex <- if (is.null(index.map)) function(x) return(x+1)
                     else                    function(x) return(which.max(index.map == x+1))

    for (i in length(points):1)     # traverse backwards so smaller indexes still valid
    {
        # Simplify value-only segments to enable toggling in powerpoint
        if (length(points[[i]]$Segments) == 1 && isTRUE(points[[i]]$Segments[[1]]$Field == "Value"))
        {
            points[[i]]$ShowValue <- TRUE
            points[[i]]$Segments <- NULL
            if (show.categoryname)
                points[[i]]$ShowCategoryName <- TRUE

            if (is.null(points[[i]]$OutlineColor) && is.null(points[[i]]$BackgroundColor))
                pt.info[.posFromIndex(points[[i]]$Index)] <- 1L # value-only label
            else
                pt.info[.posFromIndex(points[[i]]$Index)] <- 2L

        } else if (length(points[[i]]$Segments) > 0)
            pt.info[.posFromIndex(points[[i]]$Index)] <- 2L

        if (show.categoryname && length(points[[i]]$Segments) > 0)
            points[[i]]$Segments <- c(list(list(Field="CategoryName")), points[[i]]$Segments)

        # Remove empty points - empty label cannnot have outline anyway
        if (length(points[[i]]$Segments) == 0 && !isTRUE(points[[i]]$ShowValue))
        {
            pt.info[.posFromIndex(points[[i]]$Index)] <- 0L
            points[[i]] <- NULL
        }
    }

    # Switch default point from ShowValue = FALSE to ShowValue = TRUE
    # This tries to preserve series-level toggling in Excel
    # when there is more than 1 value-only points
    if (any(pt.info == 1L) && length(points) > 0)
    {
        new.points <- list()
        jj <- 1
        for (j in 1:length(pt.info))
        {
            if (pt.info[j] == 0L)
            {
                new.index <- if (is.null(index.map)) j-1 else index.map[j]-1
                new.points <- c(new.points, list(
                    if (show.categoryname) list(Index = new.index, ShowValue = FALSE, ShowCategoryName = FALSE)
                    else                   list(Index = new.index, ShowValue = FALSE)
                ))
            }
            else if (pt.info[j] == 1L)
                jj <- jj + 1 # no new point to add because this is represented by SeriesLabels
            else
            {
                new.points <- c(new.points, points[jj])
                jj <- jj + 1
            }
        }
        attr(new.points, "SeriesShowValue") <- TRUE
        return(new.points)
    }
    return(points)
}

setFontForPPT <- function(annotation)
{
    font <- list()
    if (!is.null(annotation$color))
        font$color <- annotation$color[1]
    if (!is.null(annotation$size))
        font$size <- annotation$size[1] / 1.333 # convert px to pt
    if (!is.null(annotation$font.family))
        font$family <- annotation$font.family[1]
    if (!is.null(annotation$font.weight))
        font$bold <- isTRUE(tolower(annotation$font.weight[1]) == "bold")
    if (!is.null(annotation$font.style))
        font$italic <- isTRUE(tolower(annotation$font.style[1]) == "italic")

    if (length(font) == 0)
        return(NULL)
    return (font)
}

setShapeForPPT <- function(pt, annotation)
{
    if (annotation$type == "Border")
    {
        pt$OutlineStyle <- "Solid"  # shape defaults to rectangle
        pt$OutlineColor <- annotation$color
        pt$OutlineWidth <- annotation$width/1.3333 # convert px to pt
    }
    else if (annotation$type == "Shadow")
    {
        pt$BackgroundShadow <- TRUE
        pt$BackgroundColor <- annotation$color
    }
    else if (annotation$type == "Circle - filled")
    {
        pt$OutlineShape <- "Ellipse"
        pt$OutlineStyle <- "Solid"
        if (is.null(pt$OutlineColor))
            pt$OutlineColor <- "#FFFFFF00"
        pt$BackgroundColor <- annotation$color
    }
    else
    {
        pt$OutlineShape <- "Ellipse"
        pt$OutlineStyle <- "Solid"
        pt$OutlineColor <- annotation$color
        pt$OutlineWidth <- if (annotation$type == "Circle - thin outline") 1
                           else                                            2
    }
    return(pt)
}


setTextForPPT <- function(annot)
{
    # We use unescape_html rather than directly supplying unicode because
    # Users might have added some other custom html entities that need coverting
    symbol <- switch(annot$type, "Arrow - up" = "&#8593;", "Arrow - down" = "&#8595;",
        "Caret - up" = "&#9650;", "Caret - down" = "&#9660;", annot$custom.symbol)
    return(unescape_html(symbol))
}


# From https://stackoverflow.com/questions/5060076/convert-html-character-entity-encoding-in-r
unescape_html <- function(str){
  xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
}


