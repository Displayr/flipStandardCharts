# This is only used for Bar/Column type charts
#' @importFrom verbs Sum
addBarTypeChartLabelAnnotTrace <- function(p, type, name, data.label.xpos, data.label.ypos,
        data.label.show, data.label.text, data.label.sign, data.label.nchar,
        annotation.list, annot.data, i,
        xaxis, yaxis, data.label.font, is.stacked, data.label.centered,
        data.label.horizontal.align = "center")
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
        textalign <- if (is.stacked) "middle center"
                     else            ifelse(data.label.sign >= 0, "middle right", "middle left")
        data.label.pos <- if (is.stacked) 0
                          else            ifelse(data.label.xpos < 0, 7, 3)
    }
    if (length(textalign) > 1)
        textalign <- textalign[data.label.show]

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
            tmp.dat <- getAnnotData(annot.data, a.tmp$data, i)
            ind.sel <- extractSelectedAnnot(tmp.dat, a.tmp$threshold, a.tmp$threstype)
            tmp.text <- rep("", n)
            left.pad <- paste(rep(" ", Sum(a.tmp$shiftright)), collapse = "")
            right.pad <- paste(rep(" ", Sum(a.tmp$shiftleft)), collapse = "")
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

            p <- add_trace(p, x = data.label.xpos, y = data.label.ypos, cliponaxis = FALSE,
                  type = "scatter", mode = "markers+text",
                  text = tmp.text, textfont = tmp.font,
                  marker = list(opacity = 0.0, color = "red", size = tmp.pos),
                  xaxis = xaxis, yaxis = yaxis,
                  textposition = textalign,
                  showlegend = FALSE, hoverinfo = "skip",
                  legendgroup = if (is.stacked) "all" else i)
        }
    }

    # Add data annotations
    tmp.offset <- if (!is.stacked) max(0, (max.diam - data.label.font$size))
                  else             0.01
    data.label.pos <- data.label.pos + tmp.offset
    p <- add_trace(p, name = name,
              x = data.label.xpos[data.label.show], y = data.label.ypos[data.label.show],
              cliponaxis = FALSE, type = "scatter", mode = "markers+text",
              marker = list(opacity = 0.0, size = data.label.pos),
              xaxis = xaxis, yaxis = yaxis,
              text = data.label.text[data.label.show], textfont = data.label.font,
              textposition = textalign, showlegend = FALSE, hoverinfo = "skip",
              legendgroup = if (is.stacked) "all" else i)
    return(p)
}


# Returns data labels after annotations are applied
# The 'customPoints' attribute is a component of the ChartLabels attribute for powerpoint
# They are assumed to be attached to the data labels, and an updated version is
# re-attached to the data labels returned by this function.
# This function can be called iteratively, but clean.pt.segs should only TRUE on the last call
# Note that the full column of data should always be supplied because
# the index of the data which satistfies the conditions given in the annotations
# is passed to getPointSegmentsForPPT which assumes the index
# indicates the row in \code{chart.matrix} (starting from 1 not 0).
# To specify that annotations are only applied to a subset of rows, use \code{rows.to.show} instead.

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


getAnnotData <- function(data, name, series, as.numeric = TRUE)
{
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
        stop("Annotation data does not contain a statistic named '", name, "'. ",
                "Allowable names are: '", paste(d.names, collapse = "', '"), "'. ")

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
    else
        return(which(data < threshold))
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
    if ((n.shift.right <- Sum(annotation$shiftright)) > 0)
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

    # Convert string to numeric where possible
    tmp <- suppressWarnings(as.numeric(x))
    if (!is.na(tmp))
        return(tmp)

    # If not possible, return original for string comparison
    return(x)
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
tidyPointSegments <- function(points, num.points, show.categoryname = FALSE)
{
    if (length(points) == 0)
        return(points)
    pt.info <- rep(0L, num.points) # 0 = no label; 1 = value-only label; 2 = has modification
    for (i in length(points):1) # traverse backwards so smaller indexes still valid
    {
        # Simplify value-only segments to enable toggling in powerpoint
        if (length(points[[i]]$Segments) == 1 && isTRUE(points[[i]]$Segments[[1]]$Field == "Value"))
        {
            points[[i]]$ShowValue <- TRUE
            points[[i]]$Segments <- NULL
            if (show.categoryname)
                points[[i]]$ShowCategoryName <- TRUE

            if (is.null(points[[i]]$OutlineColor) && is.null(points[[i]]$BackgroundColor))
                pt.info[points[[i]]$Index + 1] <- 1L # value-only label
            else
                pt.info[points[[i]]$Index + 1] <- 2L

        } else if (length(points[[i]]$Segments) > 0)
            pt.info[points[[i]]$Index + 1] <- 2L

        if (show.categoryname && length(points[[i]]$Segments) > 0)
            points[[i]]$Segments <- c(list(list(Field="CategoryName")), points[[i]]$Segments)

        # Remove empty points - empty label cannnot have outline anyway
        if (length(points[[i]]$Segments) == 0 && !isTRUE(points[[i]]$ShowValue))
        {
            pt.info[points[[i]]$Index + 1] <- 0L
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
                new.points <- c(new.points, list(
                    if (show.categoryname) list(Index = j-1, ShowValue = FALSE, ShowCategoryName = FALSE)
                    else                   list(Index = j-1, ShowValue = FALSE)
                ))
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


