#' \code{labeledScatterplot}
#'
#' @param chart.matrix Input data with 2 or 3 columns.
#' @param colors Group colors.
#' @param colors.reverse Whether to reverse the colors.
#' @param type Either "Labeled Scatterplot" or "Labeled Bubbleplot".
#' @param group.labels.text Text of comma-separated group labels.
#' @param group.indices.text Text of comma-separated group indices corresponding to each row.
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
        colors <- qColors

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
