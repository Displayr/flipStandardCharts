#' Generates a heatmap from a table of data.
#'
#' This function wraps the Heatmap function in the rhtmlHeatmap package and performs additional data preparation.
#'
#' @param table A matrix of data to be displayed.
#' @param data.type Whether \code{table} values are numeric or strings.
#' Options are \code{"Use an existing table"} or \code{"Type or paste data"}.
#' @param ignore.rows A string of rows to ignore, delimited by commas.
#' @param ignore.columns A string of rows to ignore, delimited by commas.
#' @param transpose Whether to transpose the table. \code{TRUE} or \code{FALSE}.
#' @param sort.rows Whether to sort rows by their averages or link as a dendrogram. Options are \code{"None"},
#' \code{"Sort by averages (ascending)"}, \code{"Sort by averages (descending)"} and \code{"Dendrogram"}.
#' @param sort.columns Whether to sort columns by their averages or link as a dendrogram. Options are \code{"None"},
#' \code{"Sort by averages (ascending)"}, \code{"Sort by averages (descending)"} and \code{"Dendrogram"}.
#' @param color Options are \code{"Blues"}, \code{"Reds"}, \code{"Greens"}, \code{"Greys"},
#' \code{"Purples"}, \code{"Oranges"}, \code{"Heat"}, \code{"Blues and reds"} or \code{"Greys and reds"}.
#' @param standardization Whether to standardize the shading of rows or columns. Options are \code{"None"},
#' \code{"Standardize rows"} and \code{"Standardize columns"}.
#' @param show.cell.values Whether to show values in cells. Options are \code{"Yes"}, \code{"No"} or
#' \code{"Automatic"} (<= 20 rows and <= 10 columns).
#' @param cell.decimals The number of decimal points to use for formatting cell values.
#' @param show.row.labels Whether to label the rows. \code{"Yes"} or \code{"No"}.
#' @param show.column.labels Whether to label the columns. \code{"Yes"} or \code{"No"}.
#' @param show.legend Whether to show the legend.
#' @param chart.title Title of the chart.
#' @param x.axis.title Title of the x-axis.
#' @param y.axis.title Title of the y-axis.
#' @param font.family Font family to be used for all titles, axes, labels and values.
#' @param font.color Font color to be used for all titles, axes and labels.
#' @param title.font.size Font size of the title.
#' @param xaxis.title.font.size Font size of the x-axis title.
#' @param yaxis.title.font.size Font size of the y-axis title.
#' @param legend.font.size Font size of the legend.
#' @param value.font.size Font size of the cell values and tooltips.
#' @param axis.label.font.size Font size of the axis labels.
#' @param left.columns An optional list of vectors or matrices to be appended to the left
#' of the heatmap.
#' @param left.column.headings An optional comma separated string containing headings for
#' \code{left.columns}. If not supplied, colnames of the items in \code{left.columns} are used.
#' @param right.columns An optional list of vectors or matrices to be appended to the right
#' of the heatmap.
#' @param right.column.headings An optional comma separated string containing headings for
#' \code{right.columns}. If not supplied, colnames of the items in \code{right.columns} are used.
#' @importFrom flipFormat FormatAsReal
#' @importFrom flipU ConvertCommaSeparatedStringToVector
#' @importFrom flipTables Reorder Cbind TidyTabularData
#' @importFrom stringr str_trim
#' @export
#'
HeatMap <- function(table,
                    data.type = "Use an existing table",
                    ignore.rows = "NET, Total, SUM",
                    ignore.columns = "NET, Total, SUM",
                    transpose = FALSE,
                    sort.rows = "None",
                    sort.columns = "None",
                    color = "Blues",
                    standardization = "None",
                    show.cell.values = "Automatic",
                    cell.decimals = 2,
                    show.row.labels = "Yes",
                    show.column.labels = "Yes",
                    show.legend = TRUE,
                    chart.title = "",
                    x.axis.title = "",
                    y.axis.title = "",
                    font.family = "sans-serif",
                    font.color = "#000000",
                    title.font.size = 24,
                    xaxis.title.font.size = 14,
                    yaxis.title.font.size = 14,
                    legend.font.size = 11,
                    value.font.size = 11,
                    axis.label.font.size = 11,
                    left.columns = NULL,
                    left.column.headings = "",
                    right.columns = NULL,
                    right.column.headings = "") {

    mat <- TidyTabularData(table, row.names.to.remove = ignore.rows,
                      col.names.to.remove = ignore.columns, transpose = transpose)
    ErrorIfNotEnoughData(mat)
    if (!is.matrix(mat)) {
        rownames <- names(mat)
        mat <- matrix(mat) # create single column matrix from vector
        rownames(mat) <- rownames
        # Until VIS-362 is fixed, rhtmlHeatmap cannot handle vectors
        stop("Input must be two-dimensional.")
    }
    if (!is.numeric(mat[1, 1]))
        stop("The input table must contain only numeric values.")

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

    color <- if (color == "Heat") {
        "YlOrRd"
    } else if (color == "Blues and reds") {
        "RdBu"
    } else if (color == "Greys and reds") {
        "RdGy"
    } else
        color

    color.range <- if (color %in% c("Blues and reds", "Greys and reds") &&
                       standardization == "None") {
        mx <- max(abs(mat))
        c(-mx, mx)
    } else
        NULL

    n.row <- nrow(mat)
    n.col <- ncol(mat)
    cellnote <- apply(mat, c(1, 2), FormatAsReal, decimals = cell.decimals)
    show.cellnote.in.cell <- (n.row <= 20 && n.col <= 10 && show.cell.values != "No") || show.cell.values == "Yes"
    show.x.axes.labels <- show.column.labels == "Yes"
    show.y.axes.labels <- show.row.labels == "Yes"

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

    row.order <- if(is.null(rownames(mat))) seq(nrow(mat)) else str_trim(rownames(mat))

    left.columns.append <- NULL
    if (!is.null(left.columns)) {
        n <- length(left.columns)
        left.column.subtitles <- character(0)
        for (i in seq(n)) {
            if (length(dim(left.columns[[i]])) != 2)        # coerce to 2D matrix with NULL colnames
                left.columns[[i]] <- as.matrix(left.columns[[i]])
            if (is.null(colnames(left.columns[[i]]))) {     # label with colnames if set or else ""
                left.column.subtitles <- c(left.column.subtitles, rep("", ncol(left.columns[[i]])))
            } else {
                left.column.subtitles <- c(left.column.subtitles, colnames(left.columns[[i]]))
            }
        }
        mats <- rep(list(mat), n)
        cbinds <- mapply(Cbind, mats, left.columns, SIMPLIFY = FALSE)
        cbinds <- lapply(cbinds, '[', row.order, -seq(1:ncol(mat)), drop = FALSE)
        left.columns.append <- do.call(cbind, cbinds)
        if (left.column.headings != "")
        {
            left.column.headings <- ConvertCommaSeparatedStringToVector(left.column.headings)
            if (length(left.column.headings) != length(left.column.subtitles))
                stop("Number of left column headings is different from number of left columns.")
            left.column.subtitles <- left.column.headings
        }
    }

    right.columns.append <- NULL
    if (!is.null(right.columns)) {
        n <- length(right.columns)
        right.column.subtitles <- character(0)
        for (i in seq(n)) {
            if (length(dim(right.columns[[i]])) != 2)
                right.columns[[i]] <- as.matrix(right.columns[[i]])
            if (is.null(colnames(right.columns[[i]]))) {
                right.column.subtitles <- c(right.column.subtitles, rep("", ncol(right.columns[[i]])))
            } else {
                right.column.subtitles <- c(right.column.subtitles, colnames(right.columns[[i]]))
            }
        }
        mats <- rep(list(mat), n)
        cbinds <- mapply(Cbind, mats, right.columns, SIMPLIFY = FALSE)
        cbinds <- lapply(cbinds, '[', row.order, -seq(1:ncol(mat)), drop = FALSE)
        right.columns.append <- do.call(cbind, cbinds)
        if (right.column.headings != "")
        {
            right.column.headings <- ConvertCommaSeparatedStringToVector(right.column.headings)
            if (length(right.column.headings) != length(right.column.subtitles))
                stop("Number of right column headings is different from number of right columns.")
            right.column.subtitles <- right.column.headings
        }
    }

    heatmap <- rhtmlHeatmap::Heatmap(mat,
                       Rowv = rowv,
                       Colv = colv,
                       scale = scale,
                       dendrogram = dendrogram,
                       xaxis_location = "top",
                       yaxis_location = "left",
                       colors = color,
                       color_range = NULL,
                       cellnote = cellnote,
                       show_cellnote_in_cell = show.cellnote.in.cell,
                       xaxis_hidden = !show.x.axes.labels,
                       yaxis_hidden = !show.y.axes.labels,
                       show_legend = show.legend,
                       title = chart.title,
                       xaxis_title = x.axis.title,
                       yaxis_title = y.axis.title,
                       cell_font_family = font.family,
                       tip_font_family = font.family,
                       legend_font_family = font.family,
                       title_font_family = font.family,
                       xaxis_font_family = font.family,
                       xaxis_title_font_family = font.family,
                       yaxis_font_family = font.family,
                       yaxis_title_font_family = font.family,
                       legend_font_color = font.color,
                       title_font_color = font.color,
                       xaxis_font_color = font.color,
                       xaxis_title_font_color = font.color,
                       yaxis_font_color = font.color,
                       yaxis_title_font_color = font.color,
                       cell_font_size = value.font.size,
                       tip_font_size = value.font.size,
                       legend_font_size = legend.font.size,
                       title_font_size = title.font.size,
                       xaxis_font_size = axis.label.font.size,
                       xaxis_title_font_size = xaxis.title.font.size,
                       yaxis_font_size = axis.label.font.size,
                       yaxis_title_font_size = yaxis.title.font.size,
                       left_columns = left.columns.append,
                       left_columns_subtitles = left.column.subtitles,
                       right_columns = right.columns.append,
                       right_columns_subtitles = right.column.subtitles,
                       left_columns_font_size = value.font.size,
                       left_columns_font_family = font.family,
                       left_columns_subtitles_font_size = axis.label.font.size,
                       left_columns_subtitles_font_family = font.family,
                       left_columns_subtitles_font_color = font.color,
                       right_columns_font_size = value.font.size,
                       right_columns_font_family = font.family,
                       right_columns_subtitles_font_size = axis.label.font.size,
                       right_columns_subtitles_font_family = font.family,
                       right_columns_subtitles_font_color = font.color)
}


