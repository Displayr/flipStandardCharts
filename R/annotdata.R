
# This is only used for Bar/Column type charts
addDataLabelAnnotations <- function(p, type, name, data.label.xpos, data.label.ypos,
        data.label.show, data.label.text, data.label.sign, 
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
    data.label.nchar <- nchar(data.label.text) # get length before adding html tags
    max.diam <- 0
    # add arrow and text annotations as a prefix/suffix to existing data labels
    for (j in seq_along(annotation.list))
    {
        if (!checkAnnotType(annotation.list[[j]]$type, "Bar"))
            return(p)
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
        } else
        {
            tmp.dat <- getAnnotData(annot.data, a.tmp$data, i, as.numeric = !grepl("Text", a.tmp$type))
            ind.sel <- extractSelectedAnnot(tmp.dat, a.tmp$threshold, a.tmp$threstype)
            data.label.text[ind.sel] <- addAnnotToDataLabel(data.label.text[ind.sel], a.tmp, tmp.dat[ind.sel])
        }
    }

    # Circle annotations
    for (j in seq_along(annotation.list))
    {
        a.tmp <- annotation.list[[j]]
        if (grepl("Circle", a.tmp$type))
        {
            tmp.dat <- getAnnotData(annot.data, a.tmp$data, i)
            ind.sel <- extractSelectedAnnot(tmp.dat, a.tmp$threshold, a.tmp$threstype)
            tmp.text <- rep("", n)
            left.pad <- paste(rep(" ", sum(a.tmp$shiftright, na.rm = TRUE)), collapse = "")
            right.pad <- paste(rep(" ", sum(a.tmp$shiftleft, na.rm = TRUE)), collapse = "")
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

getAnnotData <- function(data, name, series, as.numeric = TRUE)
{
    if (is.null(data))
        stop("No data has been provided for annotations")
    if (is.null(dim(data)))
        data <- as.matrix(data)

    d.dim <- dim(data)
    d.len <- length(d.dim)
    d.names <- dimnames(data)[[d.len]]
    if (is.null(d.names))
        d.names <- as.character(1:d.len)
    ind <- match(paste0("", name), d.names)
    if (is.na(ind))
        stop("Annotation data does not contain a statistic named '", name, "'. ",
                "Allowable names are: '", paste(d.names, collapse = "', '"), "'. ")
    if (length(d.dim) == 3)
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

addAnnotToDataLabel <- function(data.label.text, annotation, tmp.dat)
{
    # Fix font size so that the units do not change in size when the font size increases
    left.pad <- ""
    if (sum(annotation$shiftright, na.rm = TRUE) > 0)
        left.pad <- paste0("<span style='font-size: 2px'>",
                    paste(rep(" ", sum(annotation$shiftright, na.rm = TRUE)), collapse = ""),
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
        if (annotation$type == "Arrow - up")
            new.text <- "&#129049;"
        else if (annotation$type == "Arrow - down")
            new.text <- "&#129051;"
        else if (grepl("Text", annotation$type))
            new.text <- formatByD3(tmp.dat, annotation$format, annotation$prefix, annotation$suffix)
        else if (annotation$type == "Hide")
            new.text <- ""
        if (nchar(new.style) > 0)
            new.text <- paste0("<span style='", new.style, "'>", new.text, "</span>")

        if (annotation$type == "Hide")
            data.label.text <- ""
        else if (annotation$type == "Text - before data label")
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
       'Circle - filled', 'Circle - thick outline', 'Circle - thin outline',
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

getColCmpArrowHtml <- function(cell.text, arrow.size)
{
    labels <- unlist(strsplit(cell.text, split = "\\s"))
    arrow.code <- "&#129049;" # always use up-arrow
    
    res <- paste0("<span style='font-size:", arrow.size - 3,
        "px'>", labels, "</span>", arrow.code)
    res <- paste(res, collapse = "<br>")
    return(res)
}

