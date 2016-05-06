## Takes a matrix, and returns a matrix of either a cumulative sum, or a cumulative sum of percentages.
cum.data <- function(x, output = "cumulative.percentage") {
    if (output == "cumulative.sum"){
        x <- apply(x, 2, function(z) {cumsum(z)})
    } else if (output == "cumulative.percentage") {
        x <- apply(x, 2, function(z) {cumsum(prop.table(z))})
    } else if (output == "column.percentage") {
        x <- apply(x, 2, function(z) {prop.table(z)})
    }

    return(x)
}
