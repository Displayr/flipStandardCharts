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
#' and where the colour vector is returned sorted alphabetically
#'
#' @param x A matrix.
#' @param red An integer between 0 and 255.
#' @param green An integer between 0 and 255.
#' @param blue An integer between 0 and 255.
#' @return A named elements vector of colors with decreasing alpha values.
#' @examples
#' data("z")
#' MakeColorGradient(z, red = 192, green = 35, blue = 220)
#' @export
MakeColorGradient <- function (x, red, green, blue) {
    if (!is.matrix(x))
        stop("Input is not a matrix")

    col.vector <- ""
    number.rows <- nrow(x) + 1

    for (i in 1:nrow(x)) {
        red.factor <- ((255 - red) / number.rows) * i
        green.factor <- ((255 - green) / number.rows) * i
        blue.factor <- ((255 - blue) / number.rows) * i

        col.vector <- c(col.vector, grDevices::rgb(red + red.factor, green + green.factor, blue + blue.factor, 255, maxColorValue = 255))
    }

    ## Sort the matrix by mean
    ordered.matrix <- MeanRowValueDescendingSort(x)

    ## Assign matrix row names to col.vector
    col.vector <- col.vector[2:length(col.vector)]
    names(col.vector) <- rownames(ordered.matrix)

    ## Sort the colour vector
    col.vector <- AlphabeticRowNames(col.vector)

    col.vector
}
