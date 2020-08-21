#' Generates a heatmap from a table of data.
#'
#' This function wraps the Heatmap function in the rhtmlHeatmap package.
#'
#' @param x A matrix of data to be displayed with values in the cells and optiona row and column labels.
#' @param sort.rows Whether to sort rows by their averages or link as a dendrogram. Options are \code{"None"},
#' \code{"Sort by averages (ascending)"}, \code{"Sort by averages (descending)"} and \code{"Dendrogram"}.
#' @param sort.columns Whether to sort columns by their averages or link as a dendrogram. Options are \code{"None"},
#' \code{"Sort by averages (ascending)"}, \code{"Sort by averages (descending)"} and \code{"Dendrogram"}.
#' @param colors A vector of 2 colors representing the maximum and minimum values.
#' @param standardization Whether to standardize the shading of rows or columns. Options are \code{"None"},
#' \code{"Standardize rows"} and \code{"Standardize columns"}.
#' @param global.font.family Character; font family for all occurrences of any
#' font attribute for the chart unless specified individually.
#' @param global.font.color Global font color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. #' rgb(0, 0, 0, maxColorValue = 255)).
#' @param title Character; chart title.
#' @param title.font.family Character; title font family. Can be "Arial Black",
#' "Arial", "Comic Sans MS", "Courier New", "Georgia", "Impact",
#' "Lucida Console", "Lucida Sans Unicode", "Marlett", "Symbol", "Tahoma",
#' "Times New Roman", "Trebuchet MS", "Verdana", "Webdings"
#' @param title.font.color Title font color as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param title.font.size Integer; Title font size
#' @param subtitle Character
#' @param subtitle.font.color subtitle font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param subtitle.font.family Character; subtitle font family
#' @param subtitle.font.size Integer; subtitle font size
#' @param footer Character
#' @param footer.font.color footer font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param footer.font.family Character; footer font family
#' @param footer.font.size Integer; footer font size
#' @param x.title Character, x-axis title
#' @param x.title.font.color x-axis title font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param x.title.font.family Character; x-axis title font family
#' @param x.title.font.size Integer; x-axis title font size
#' @param y.title Character, y-axis title
#' @param y.title.font.color y-axis title font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0,
#' max = 255)).
#' @param y.title.font.family Character; y-axis title font family
#' @param y.title.font.size Integer; y-axis title font size
#' @param x.tick.show Whether to display the x-axis tick labels
#' @param x.tick.font.family Character; x-axis tick label font family
#' @param x.tick.font.color X-axis tick label font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param x.tick.font.size Integer; x-axis tick label font size
#' @param y.tick.show Whether to display the y-axis tick labels
#' @param y.tick.font.family Character; y-axis tick label font family
#' @param y.tick.font.color y-axis tick label font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param y.tick.font.size Integer; y-axis tick label font size
#' @param values.bounds.minimum Numeric; lower bound of the color range.
#' @param values.bounds.maximum Numeric; upper bound of the color range.
#' @param legend.show Whether to display the legend
#' @param legend.font.family Character; legend font family.
#' @param legend.font.color Legend font color as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param legend.font.size Integer; Legend font size.
#' @param data.label.show Whether to display the data values (note that they are
#' always shown as hover text)
#' @param data.label.font.size Integer; Font size for data label.
#' @param data.label.font.family Character; font family for data label.
#' @param data.label.format A string representing a d3 formatting code.
#' See https://github.com/mbostock/d3/wiki/Formatting#numbers
#' @param data.label.prefix Character; prefix for data values.
#' @param data.label.suffix Character; suffix for data values.
#' @param hovertext.font.size Integer; Font size for hovertext (tooltips).
#' @param hovertext.font.family Character; font family for hovertext.
#' @param y.hovertext.format A string representing a d3 formatting code.
#' See https://github.com/mbostock/d3/wiki/Formatting#numbers
#' @param y.hovertext.prefix Character; prefix for hovertext.
#' @param y.hovertext.suffix Character; suffix for hovertext.

#' @param left.columns An optional list of vectors or matrices to be appended to the left
#' of the heatmap.
#' @param left.column.headings An optional comma separated string containing headings for
#' \code{left.columns}. If not supplied, colnames of the items in \code{left.columns} are used.
#' @param right.columns An optional list of vectors or matrices to be appended to the right
#' of the heatmap.
#' @param right.column.headings An optional comma separated string containing headings for
#' \code{right.columns}. If not supplied, colnames of the items in \code{right.columns} are used.
#'
#' @importFrom flipFormat FormatAsReal FormatAsPercent
#' @importFrom flipU ConvertCommaSeparatedStringToVector
#' @importFrom flipTables Reorder Cbind
#' @importFrom stringr str_trim
#' @export
#'
Heat <- function(x,
                    sort.rows = "None",
                    sort.columns = "None",
                    colors = c("#0066ff", "#ff0000"),
                    standardization = "None",
                    global.font.family = "Arial",
                    global.font.color = "#000000",
                    values.bounds.minimum = NULL,
                    values.bounds.maximum = NULL,
                    title = "",
                    title.font.family = global.font.family,
                    title.font.color = global.font.color,
                    title.font.size = 16,
                    subtitle = "",
                    subtitle.font.family = global.font.family,
                    subtitle.font.color = global.font.color,
                    subtitle.font.size = 12,
                    footer = "",
                    footer.font.family = global.font.family,
                    footer.font.color = global.font.color,
                    footer.font.size = 12,
                    x.title = "",
                    x.title.font.family = global.font.family,
                    x.title.font.color = global.font.color,
                    x.title.font.size = 12,
                    y.title = "",
                    y.title.font.family = global.font.family,
                    y.title.font.color = global.font.color,
                    y.title.font.size = 12,
                    x.tick.show = TRUE,
                    x.tick.font.family = global.font.family,
                    x.tick.font.color = global.font.color,
                    x.tick.font.size = 10,
                    y.tick.show = TRUE,
                    y.tick.font.family = global.font.family,
                    y.tick.font.color = global.font.color,
                    y.tick.font.size = 10,
                    legend.show = TRUE,
                    legend.font.family = global.font.family,
                    legend.font.color = global.font.color,
                    legend.font.size = 10,
                    data.label.show = TRUE,
                    data.label.font.size = 10,
                    data.label.font.family = global.font.family,
                    data.label.format = "",
                    data.label.prefix = "",
                    data.label.suffix = "",
                    hovertext.font.family = global.font.family,
                    hovertext.font.size = 11,
                    y.hovertext.format = data.label.format,
                    y.hovertext.prefix = data.label.prefix,
                    y.hovertext.suffix = data.label.suffix,
                    left.columns = NULL,
                    left.column.headings = "",
                    right.columns = NULL,
                    right.column.headings = "") {

    mat <- checkMatrixNames(stripClassAndCallFromXtabs(x), assign.col.names = FALSE)
    ErrorIfNotEnoughData(mat)

    if (nchar(gsub("\\s", "", x.title, perl = TRUE)) == 0)
        x.title <- ""

    if (nrow(mat) > 500 || ncol(mat) > 500)
        stop("Heatmap cannot be plotted with more than 500 rows or columns.")

    if (!is.numeric(mat[1, 1]))
        stop("The input data must contain only numeric values.")

    mat <- if (sort.rows == "Sort by averages (ascending)") {
        Reorder(mat, rows = "Ascending", columns = "None")
    } else if (sort.rows == "Sort by averages (descending)") {
        Reorder(mat, rows = "Descending", columns = "None")
    } else
        mat

    mat <- if (sort.columns == "Sort by averages (ascending)") {
        Reorder(mat, rows = "None", columns = "Ascending")
    } else if (sort.columns == "Sort by averages (descending)") {
        Reorder(mat, rows = "None", columns = "Descending")
    } else
        mat

    default.range <- range(mat, na.rm = TRUE)
    color.range <- unlist(setValRange(values.bounds.minimum, values.bounds.maximum, mat))
    if (!is.null(color.range[1]) && color.range[1] > default.range[1])
    {
        warning("Minimum value cannot be larger than ", default.range[1], ".")
        color.range[1] <- default.range[1]
    }
    if (!is.null(color.range[2]) && color.range[2] < default.range[2])
    {
        warning("Maximum value cannot be smaller than ", default.range[2], ".")
        color.range[2] <- default.range[2]
    }

    n.row <- nrow(mat)
    n.col <- ncol(mat)

    stat <- ifelse(is.null(attr(x, "statistic")), "", attr(x, "statistic"))
    pct <- percentFromD3(y.hovertext.format) || grepl("%)?$", stat)
    data.label.text <- formatByD3(mat, data.label.format, data.label.prefix, data.label.suffix, percent = pct)
    dim(data.label.text) <- dim(mat)
    data.label.text[!is.finite(mat)] <- "NA"

    hovertext.text <- formatByD3(mat, y.hovertext.format, y.hovertext.prefix, y.hovertext.suffix, percent = pct)
    dim(hovertext.text) <- dim(mat)
    hovertext.text[!is.finite(mat)] <- "NA"

    if (y.title == stat)
        y.title <- ""

    rowv <- sort.rows == "Dendrogram"
    colv <- sort.columns == "Dendrogram"
    dendrogram <- if (rowv) {
        if (colv) "both" else "row"
    } else {
        if (colv) "column" else "none"
    }

    scale <- if (standardization == "Standardize rows") {
        "row"
    } else if (standardization == "Standardize columns") {
        "column"
    } else
        "none"

    cell.decimals <- decimalsFromD3(data.label.format, if (pct) 0 else 2)
    row.order <- if(is.null(rownames(mat))) seq(nrow(mat)) else str_trim(rownames(mat))
    left.appended <- appendColumns(left.columns, mat, cell.decimals, left.column.headings, row.order)
    right.appended <- appendColumns(right.columns, mat, cell.decimals, right.column.headings, row.order)

    heatmap <- rhtmlHeatmap::Heatmap(mat,
                                     Rowv = rowv,
                                     Colv = colv,
                                     scale = scale,
                                     dendrogram = dendrogram,
                                     xaxis_location = "top",
                                     yaxis_location = "left",
                                     colors = colors,
                                     color_range = color.range,

                                     # Data labels and hovertext
                                     cellnote = hovertext.text,
                                     show_cellnote_in_cell = data.label.show,
                                     cellnote_in_cell = data.label.text,

                                     # Left and right additional columns
                                     left_columns = left.appended$columns.append,
                                     left_columns_subtitles = left.appended$column.subtitles,
                                     right_columns = right.appended$columns.append,
                                     right_columns_subtitles = right.appended$column.subtitles,

                                     # Titles and fonts
                                     title = title,
                                     title_font_family = title.font.family,
                                     title_font_color = title.font.color,
                                     title_font_size = title.font.size,
                                     subtitle = subtitle,
                                     subtitle_font_size = subtitle.font.size,
                                     subtitle_font_family = subtitle.font.family,
                                     subtitle_font_color = subtitle.font.color,
                                     footer = footer,
                                     footer_font_size = footer.font.size,
                                     footer_font_family = footer.font.family,
                                     footer_font_color = footer.font.color,
                                     xaxis_title = x.title,
                                     xaxis_title_font_family = x.title.font.family,
                                     xaxis_title_font_color = x.title.font.color,
                                     xaxis_title_font_size = x.title.font.size,
                                     yaxis_title = y.title,
                                     yaxis_title_font_family = y.title.font.family,
                                     yaxis_title_font_color = y.title.font.color,
                                     yaxis_title_font_size = y.title.font.size,
                                     xaxis_hidden = !x.tick.show,
                                     xaxis_font_family = x.tick.font.family,
                                     xaxis_font_color = x.tick.font.color,
                                     xaxis_font_size = x.tick.font.size,
                                     yaxis_hidden = !y.tick.show,
                                     yaxis_font_family = y.tick.font.family,
                                     yaxis_font_color = y.tick.font.color,
                                     yaxis_font_size = y.tick.font.size,
                                     show_legend = legend.show,
                                     legend_font_family = legend.font.family,
                                     legend_font_color = legend.font.color,
                                     legend_font_size = legend.font.size,
                                     legend_label_format = if (pct) "percentage" else "normal",
                                     cell_font_family = data.label.font.family,
                                     cell_font_size = data.label.font.size,
                                     tip_font_family = hovertext.font.family,
                                     tip_font_size = hovertext.font.size,
                                     left_columns_font_size = data.label.font.size,
                                     left_columns_font_family = data.label.font.family,
                                     left_columns_font_color = global.font.color,
                                     left_columns_subtitles_font_size = x.tick.font.size,
                                     left_columns_subtitles_font_family = x.tick.font.family,
                                     left_columns_subtitles_font_color = x.tick.font.color,
                                     right_columns_font_size = data.label.font.size,
                                     right_columns_font_family = data.label.font.family,
                                     right_columns_font_color = global.font.color,
                                     right_columns_subtitles_font_size = x.tick.font.size,
                                     right_columns_subtitles_font_family = x.tick.font.family,
                                     right_columns_subtitles_font_color = x.tick.font.color)
    result <- list(htmlwidget = heatmap)
    class(result) <- "StandardChart"
    attr(result, "ChartType") <- "Surface Top View"
    result
}


appendColumns <- function(to.append, mat, cell.decimals, column.headings, row.order) {

    columns.append <- NULL
    column.subtitles <- character(0)
    if (!is.null(to.append)) {
        n <- length(to.append)
        for (i in seq(n)) {
            if (length(dim(to.append[[i]])) != 2)        # coerce to 2D matrix with NULL colnames
                to.append[[i]] <- as.matrix(to.append[[i]])
            to.append[[i]] <- formatNumeric(to.append[[i]], cell.decimals)
            if (is.null(colnames(to.append[[i]])))    # label with colnames if set or else ""
            {
                column.subtitles <- c(column.subtitles, rep("", ncol(to.append[[i]])))
            }
            else
            {
                column.subtitles <- c(column.subtitles, colnames(to.append[[i]]))
            }
        }
        # Bind each additonal item to its own copy of mat, then extract the bound columns
        # and bind them together.
        mats <- rep(list(mat), n)
        cbinds <- mapply(Cbind, mats, to.append, SIMPLIFY = FALSE)
        cbinds <- lapply(cbinds, '[', row.order, -seq(1:ncol(mat)), drop = FALSE)
        columns.append <- do.call(cbind, cbinds)
        if (column.headings != "")
        {
            column.headings <- ConvertCommaSeparatedStringToVector(column.headings)
            if (length(column.headings) != length(column.subtitles))
                stop("Number of additional column headings is different from number of additional columns.")
            column.subtitles <- column.headings
        }
    }
    return(list(columns.append = columns.append, column.subtitles = column.subtitles))
}

# Format numeric left and right columns with same decimals as heatmap
formatNumeric <- function(x, decimals) {
    if (is.numeric(x))
        return(apply(x, c(1, 2), FormatAsReal, decimals = decimals))
    if (is.data.frame(x))
    {
        numeric.cols <- sapply(x, is.numeric)
        x[numeric.cols] <- lapply(x[numeric.cols], FormatAsReal, decimals = decimals)
    }
    return(x)
}


