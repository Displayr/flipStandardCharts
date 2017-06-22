#' \code{labeledScatterplot}
#'
#' @param chart.matrix Input data with 2 or 3 columns.
#' @param colors Group colors.
#' @param colors.reverse Whether to reverse the colors.
#' @param type Either "Labeled Scatterplot" or "Labeled Bubbleplot".
#' @param group.labels.text Vector or text of comma-separated group labels.
#' @param group.indices.text Vector or text of comma-separated group indices corresponding to each row.
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
                               colors = NULL,
                               colors.reverse = FALSE,
                               type = "Labeled Scatterplot",
                               group.labels.text = "",
                               group.indices.text = "",
                               origin = FALSE,
                               transpose = FALSE,
                               rows.to.ignore = "",
                               cols.to.ignore = "",
                               legend.show = TRUE,
                               x.title = "",
                               y.title = "")
{
    is.bubble <- type == "Labeled Bubbleplot"

    data <- scatterplotData(chart.matrix,
                            is.bubble,
                            group.labels.text,
                            group.indices.text,
                            transpose,
                            rows.to.ignore,
                            cols.to.ignore,
                            x.title,
                            y.title)

    if (is.null(colors))
        colors <- "Default colors"

    colors <- StripAlphaChannel(ChartColors(number.colors.needed = length(unique(data$group)),
                                            given.colors = colors,
                                            reverse = colors.reverse,
                                            trim.light.colors = TRUE))

    output <- list(X = data$x,
                   Y = data$y,
                   Z = data$z,
                   colors = colors,
                   label = data$row.names,
                   group = data$group,
                   origin = origin,
                   legend.show = length(unique(data$group)) > 1 && legend.show,
                   legend.bubbles.show = is.bubble,
                   x.title = data$x.title,
                   y.title = data$y.title)

    return(output)
}

#' @importFrom flipTransformations TextAsVector
scatterplotData <- function(chart.matrix, is.bubble, group.labels.text, group.indices.text, transpose,
                            rows.to.ignore, cols.to.ignore, x.title, y.title)
{
    if (any(is.na(as.matrix(chart.matrix))))
    {
        warning("Data points with missing values have been omitted.")
        chart.matrix <- chart.matrix[!is.na(rowSums(chart.matrix)), ]
    }

    # Remove rows and columns to ignore
    rows.to.ignore <- TextAsVector(rows.to.ignore)
    cols.to.ignore <- TextAsVector(cols.to.ignore)
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

    pt.ord <- NULL
    if (!is.null(group.labels.text) && group.labels.text[1] != "")
    {
        if (!is.null(group.indices.text) && any(group.indices.text != ""))
        {
            group.labels <- TextAsVector(group.labels.text)
            group.indices <- if (is.numeric(group.indices.text)) group.indices.text
                             else as.numeric(TextAsVector(group.indices.text))

            if (length(group.labels) == 1)
                stop(paste0("Only one group has been specified: ", group.labels[1]))

            if (length(group.indices) != nrow(chart.matrix))
                stop(paste0("The number of group indices (", length(group.indices), ") needs to equal the number of rows in the table (", nrow(chart.matrix), ")."))

            permitted.indices <- 1:length(group.labels)
            if (any(is.na(group.indices)) || !all(group.indices %in% permitted.indices))
                stop(paste0("The group indices are not in the correct format."))

            group <- group.labels[group.indices]
            pt.ord <- order(group.indices)
        }
        else
            stop("Group labels were provided but group indices are missing.")
    }
    else
    {
        if (!is.null(group.indices.text) && any(group.indices.text != ""))
            stop("Group indices were provided but group labels are missing.")
        else
            group <- rep("Group", nrow(chart.matrix))
    }

    result <- list()

    # order data points so that the color of groups are ordered
    if (!is.null(pt.ord))
    {
        chart.matrix <- chart.matrix[pt.ord,]
        group <- group[pt.ord]
    }

    result$x <- if (transpose) as.numeric(chart.matrix[, 2]) else as.numeric(chart.matrix[, 1])
    result$y <- if (transpose) as.numeric(chart.matrix[, 1]) else as.numeric(chart.matrix[, 2])
    result$z <- if (is.bubble) as.numeric(chart.matrix[, 3]) else NULL

    if (is.bubble && any(result$z < 0))
        stop("Negative values are present in the third column. No bubbles are shown for such cases.")

    result$row.names <- rownames(chart.matrix)
    result$group <- group

    # Resolve axes labels if none specified manually
    if (x.title == "" || length(x.title) == 0)
        x.title <- colnames(chart.matrix)[1]
    if (is.null(x.title) || x.title == "FALSE" || x.title == FALSE)
        x.title <- ""

    if (y.title == "" || length(y.title) == 0)
        y.title <- colnames(chart.matrix)[2]
    if (is.null(y.title) || y.title == "FALSE" || y.title == FALSE)
        y.title <- ""

    result$x.title <- if (transpose) y.title else x.title
    result$y.title <- if (transpose) x.title else y.title

    result
}

