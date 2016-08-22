## Color gradient function

#' Sorts a matrix in descending order by the mean row value.
#'
#' \code{MeanRowValueDescendingSort} returns a matrix where the rows
#' have been sorted in descdening order based on the mean value of
#' each row.
#'
#' @param x A matrix.
#' @return A sorted matrix.
#' @examples
#' data("z")
#' MeanRowValueDescendingSort(z)
#' @export
MeanRowValueDescendingSort <- function(x) {
    means <- apply(x, 1, function(z) {mean(z)})
    x <- cbind(x, means)
    x <- x[order(-means),]
    x <- x[, 1:(ncol(x) - 1)]
    return(as.matrix(x))
}

#' Sorts a matrix by row names.
#'
#' \code{AlphabeticRowNames} returns a matrix where the rows have been sorted
#' alphabetically by the row names
#'
#' @param x A matrix.
#' @param desc Logic; if TRUE then sort order is descending, if FALSE ascending.
#' @return A sorted matrix.
#' @examples
#' data("z")
#' AlphabeticRowNames(z, desc = FALSE)
#' @export
AlphabeticRowNames <- function(x, desc = TRUE) {
    if (length(rownames(x)) > 0 || length(names(x)) > 0)
        ifelse(is.matrix(x), x <- x[order(rownames(x), decreasing = desc), ], x <- x[order(names(x), decreasing = desc)])
    else
        stop("The input does not have names")

    x
}

#' Generates a vector of colors based on the number of rows in a matrix
#'
#' \code{MakeColorGradient} Gives a named-elements vector of colours for
#' each row in a matrix, where the base colour is specified, and the alpha
#' value gradually decreases based on the row-mean of the matrix,
#' and where the colour vector is returned sorted alphabetically.
#'
#' The parameters y, x, and transpose should be the same as those passed to
#' the standard R function "Charts" and this function will calculate, from there
#' how many colours are needed.  Alternatively, if you know the number of series,
#' you can specify the number of series for which you wish to generate colors.
#'
#' The function will calculate the specified number of colours, using the base
#' color provided (in rgb format to the parameters base.red, base.green, and
#' base.blue) in progressively lighter tones.  Thus, the base color should be
#' the darkest version of the colour you wish to see.
#'
#' Specifying the by values (either "series" or "mean") will allow you to
#' determine if the gradient should go in the order of the series, or in order
#' of row mean.  This is only relevant if y, x, and transpose have been
#' specified, and will not work if only the number of series has been given.
#'
#' Finally, base.first (defaults to TRUE), should be used for the first item
#' to be colored, with the subsequent items being lighter, or (FALSE) when
#' the first item will use the lightest color and the last item the base color.
#'
#' @param chart.matrix Matrix; the tabulated data passed to the chart.
#' @param base.red An integer between 0 and 255.
#' @param base.green An integer between 0 and 255.
#' @param base.blue An integer between 0 and 255.
#' @param by A string indicating what feature of the data determines how the
#' gradient should be ordered.  "series" makes the first series the lightest,
#' and then progresses to darkest for the last series.  "mean" generates
#' gradients by the row mean of each series, with the lowest value the lightest.
#' @param base.first Logical; if the specified base colour should come first or
#' last.
#' @return A named elements vector of colors with decreasing alpha values.
#' @examples
#' data("z")
#' MakeColorGradient(chart.matrix = z, base.red = 192, base.green = 35, base.blue = 220)
#' @export
MakeColorGradient <- function (chart.matrix = NULL,
                               base.red = 0,
                               base.green = 0,
                               base.blue = 0,
                               by = "series",
                               base.first = TRUE)
{
    number.rows <- nrow(chart.matrix)

    col.vector <- character()

    for (i in 1:number.rows) {
        red.factor <- ((255 - base.red) / number.rows) * i
        green.factor <- ((255 - base.green) / number.rows) * i
        blue.factor <- ((255 - base.blue) / number.rows) * i

        col.vector <- c(col.vector, grDevices::rgb(base.red + red.factor, base.green + green.factor, base.blue + blue.factor, 255, maxColorValue = 255))
    }

    col.vector <- col.vector[1:number.rows]

    if (!base.first)
        col.vector <- rev(col.vector)

    if (by == "mean" && !is.null(chart.matrix))
    {
        ## Sort the matrix by mean
        ordered.matrix <- MeanRowValueDescendingSort(chart.matrix)

        ## Assign matrix row names to col.vector
        names(col.vector) <- rownames(ordered.matrix)

        ## Sort the colour vector, by name, in the same order as the source data matrix.
        ## Turn the color vector into a data frame with named columns
        col.df <- cbind(series = names(col.vector), colors = col.vector)

        ## Get the original order, also as a data frame.
        original.order.names <- data.frame(series = rownames(chart.matrix))

        ## Join the original order df to the sorted col.df
        col.vector2 <- dplyr::left_join(original.order.names, col.df, by = "series", copy = TRUE)

        ## Take the second column, now with the colours sorted appropriately
        col.vector <- col.vector2[, 2]
    }

    col.vector
}

# # Assigns a vector of colours of as many members as the passed-in vector x, in increasing lightness from a given source colour
# SetColors <- function (x, red, green, blue) {
#     col.vector <- ""
#     number.rows <- nrow(x) + 1
#
#     for (i in nrow(x):1){
#         red.factor <- ((255 - red) / number.rows) * i
#         green.factor <- ((255 - green) / number.rows) * i
#         blue.factor <- ((255 - blue) / number.rows) * i
#
#         col.vector <- c(col.vector, grDevices::rgb(red + red.factor, green + green.factor, blue + blue.factor, 255, maxColorValue = 255))
#     }
#
#     return(col.vector[-1, drop = FALSE])
# }
