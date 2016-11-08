#' \code{labeledScatterplot}
#'
#' @param chart.matrix Input data with 2 or 3 columns.
#' @param colors Group colors.
#' @param colors.reverse Whether to reverse the colors.
#' @param type Either "Labeled Scatterplot" or "Labeled Bubbleplot".
#' @param group.labels.text Text of comma-separated group labels.
#' @param group.indices.text Text of comma-separated group indices corresponding to each row.
#' @param grid Whether to display the grid.
#' @param origin Whether to display the origin.
#' @param transpose Whether to switch the first and second columns before plotting.
#' @param rows.to.ignore Text of comma-separated row labels to omit.
#' @param cols.to.ignore Text of comma-separated column labels to omit.
#' @param legend.show Whether to show the legend, if there is more than one group.
#' @param x.title If this is text, this overrides the x-axis title. If this is FALSE, the title is hidden.
#' @param y.title If this is text, this overrides the y-axis title. If this is FALSE, the title is hidden.
#' @importFrom flipData GetTidyTwoDimensionalArray
#' @importFrom flipChartBasics StripAlphaChannel ChartColors
labeledScatterplot <- function(chart.matrix,
                               colors = qColors,
                               colors.reverse = FALSE,
                               type = "Labeled Scatterplot",
                               group.labels.text = "",
                               group.indices.text = "",
                               grid = TRUE,
                               origin = FALSE,
                               transpose = FALSE,
                               rows.to.ignore = "",
                               cols.to.ignore = "",
                               legend.show = TRUE,
                               x.title = "",
                               y.title = "")
{
    is.bubble <- type == "Labeled Bubbleplot"

    # Converts a comma separated string to a vector of strings
    .parseCommaSeparatedText <- function(t)
    {
        as.vector(sapply(strsplit(t, ","), function(x) gsub("^\\s+|\\s+$", "", x)))
    }

    # Remove rows and columns to ignore
    rows.to.ignore <- .parseCommaSeparatedText(rows.to.ignore)
    cols.to.ignore <- .parseCommaSeparatedText(cols.to.ignore)
    if (length(rows.to.ignore[[1]]) == 0)
        rows.to.ignore <- NULL
    if (length(cols.to.ignore[[1]]) == 0)
        cols.to.ignore <- NULL
    chart.matrix <- GetTidyTwoDimensionalArray(chart.matrix,
                                               row.names.to.remove = rows.to.ignore,
                                               column.names.to.remove = cols.to.ignore)

    if (is.bubble && ncol(chart.matrix) != 3)
        stop("The number of columns in the input table (after excluding ignored columns) must be 3 for a bubbleplot.")
    if (!is.bubble && ncol(chart.matrix) != 2)
        stop("The number of columns in the input table (after excluding ignored columns) must be 2 for a scatterplot.")

    if (group.labels.text != "")
    {
        if (group.indices.text != "")
        {
            group.labels <- .parseCommaSeparatedText(group.labels.text)
            group.indices <- as.numeric(.parseCommaSeparatedText(group.indices.text))

            if (length(group.labels) == 1)
                stop(paste0("Only one group has been specified: ", group.labels[1]))

            if (length(group.indices) != nrow(chart.matrix))
                stop(paste0("The number of group indices (", length(group.indices), ") needs to equal the number of rows in the table (", nrow(chart.matrix), ")."))

            permitted.indices <- 1:length(group.labels)
            if (any(is.na(group.indices)) || !all(group.indices %in% permitted.indices))
                stop(paste0("The group indices are not in the correct format."))

            group <- group.labels[group.indices]
        }
        else
            stop("Group labels were provided but group indices are missing.")
    }
    else
    {
        if (group.indices.text != "")
            stop("Group indices were provided but group labels are missing.")
        else
            group <- rep("Group", nrow(chart.matrix))
    }

    X <- if (transpose) as.numeric(chart.matrix[, 2]) else as.numeric(chart.matrix[, 1])
    Y <- if (transpose) as.numeric(chart.matrix[, 1]) else as.numeric(chart.matrix[, 2])
    Z <- if (is.bubble) as.numeric(chart.matrix[, 3]) else NULL

    if (is.bubble && any(Z < 0))
        stop("Negative values are present in the third column. No bubbles are shown for such cases.")

    colors <- StripAlphaChannel(ChartColors(number.colors.needed = length(unique(group)),
                                            given.colors = colors,
                                            reverse = colors.reverse))

    # Resolve axes labels if none specified manually
    if (x.title == "" || length(x.title) == 0)
        x.title <- colnames(chart.matrix)[1]

    if (x.title == "FALSE" || x.title == FALSE)
        x.title <- ""

    if (y.title == "" || length(y.title) == 0)
        y.title <- colnames(chart.matrix)[2]

    if (y.title == "FALSE" || y.title == FALSE)
        y.title <- ""

    output <- list(X = X,
                   Y = Y,
                   Z = Z,
                   colors = colors,
                   label = rownames(chart.matrix),
                   group = group,
                   grid = grid,
                   origin = origin,
                   legend.show = length(unique(group)) > 1 && legend.show,
                   legend.bubbles.show = is.bubble,
                   x.title = x.title,
                   y.title = y.title)

    return(output)
}
