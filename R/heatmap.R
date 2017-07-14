#' \code{HeatMap} generates a heatmap from a table of data. This function wraps the Heatmap function
#' in the rhtmlHeatmap package and performs additional data preparation.
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
#' @param show.row.labels Whether to label the rows. \code{"Yes"} or \code{"No"}.
#' @param show.column.labels Whether to label the columns. \code{"Yes"} or \code{"No"}.
#' @importFrom flipData GetTidyTwoDimensionalArray
#' @importFrom flipFormat FormatWithDecimals
#' @importFrom flipTables Reorder
#' @importFrom flipTransformations ParseEnteredData
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
                    show.row.labels = "Yes",
                    show.column.labels = "Yes") {

    t <- if (data.type == "Use an existing table") {
        table
    } else {
        ParseEnteredData(table)
    }

    mat <- GetTidyTwoDimensionalArray(t, ignore.rows, ignore.columns)
    if (!is.numeric(mat[1, 1]))
        stop("The input table must contain only numeric values.")

    if (transpose)
        mat <- t(mat)

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
    cellnote <- matrix("", n.row, n.col)
    for (i in 1:n.row)
        for (j in 1:n.col)
            cellnote[i, j] <- FormatWithDecimals(mat[i, j], 2)
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

    heatmap <- rhtmlHeatmap::Heatmap(mat,
                       Rowv = rowv,
                       Colv = colv,
                       scale = scale,
                       dendrogram = dendrogram,
                       xaxis_location = "top",
                       yaxis_location = "left",
                       colors = color,
                       color_range = NULL,
                       cexRow = 0.79,
                       cellnote = cellnote,
                       show_cellnote_in_cell = show.cellnote.in.cell,
                       xaxis_hidden = !show.x.axes.labels,
                       yaxis_hidden = !show.y.axes.labels)
}
