#' Generates a heatmap from a table of data.
#'
#' This function wraps the Heatmap function in the rhtmlHeatmap package.
#'
#' @param table A matrix of data to be displayed.
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
Heat <- function(table,
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

    mat <- table
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
    left.appended <- appendColumns(left.columns, mat, cell.decimals, left.column.headings, row.order)
    right.appended <- appendColumns(right.columns, mat, cell.decimals, right.column.headings, row.order)

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
                                     left_columns = left.appended$columns.append,
                                     left_columns_subtitles = left.appended$column.subtitles,
                                     right_columns = right.appended$columns.append,
                                     right_columns_subtitles = right.appended$column.subtitles,
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


appendColumns <- function(to.append, mat, cell.decimals, column.headings, row.order) {

    columns.append <- NULL
    column.subtitles <- character(0)
    if (!is.null(to.append)) {
        n <- length(to.append)
        for (i in seq(n)) {
            if (length(dim(to.append[[i]])) != 2)        # coerce to 2D matrix with NULL colnames
                to.append[[i]] <- as.matrix(to.append[[i]])
            to.append[[i]] <- formatNumeric(to.append[[i]], cell.decimals)
            if (is.null(colnames(to.append[[i]]))) {     # label with colnames if set or else ""
                column.subtitles <- c(column.subtitles, rep("", ncol(to.append[[i]])))
            } else {
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

# Format numeric columns with same decimals as heatmap
formatNumeric <- function(x, decimals) {
    if (is.numeric(x))
        return(apply(x, c(1, 2), FormatAsReal, decimals = decimals))
    if (is.data.frame(x)) {
        numeric.cols <- sapply(x, is.numeric)
        x[numeric.cols] <- lapply(x[numeric.cols], FormatAsReal, decimals = decimals)
    }
    return(x)
}


