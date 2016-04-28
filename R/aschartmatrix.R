# 'AsChartMatrix' checks if data is in the appropriate format and attempts to coerce it to the appropriate
# format if so required.

# A 'ChartMatrix' is/has
# - A 'matrix' of type 'integer' or 'numeric'
# - Columns represent the X-axis
# - Column names
# - Row names if there are more than one rows

textPeriodFromDate <- function(x, period = "month")
{
    year.two.digits <- strftime(x, "%y")

    if (period == "month")
        return(paste(substr(months(x), 1, 3), year.two.digits, sep = "-"))

    if (period == "quarter")
        return(paste(quarters(x), year.two.digits, sep = "-"))

    if (period == "year")
        return(strftime(x, "%y"))
}

isNumericOrInteger <- function(y)
{
    if (!is.integer(y) && !is.numeric(y))
    {
        vector.class <- class(y)
        vector.mode <- mode(y)
        stop(paste("Vectors passed to the y-parameter and/or weights must be of the mode numeric or integer.  The entity passed is a",vector.class,"of mode: ",vector.mode))
    }
    TRUE
}

numberOfRows <- function(x)
{
    if (is.vector(x) | is.factor(x) | inherits(x, "POSIXct"))
        return(length(x))
    nrow(x)
}

equalNumberOfRows <- function(y, x) {numberOfRows(y) == numberOfRows(x)}

#' @export
IsChartMatrix <- function(x, n.rows, n.columns)
{
    if (nrow(x) != n.rows)
        return(FALSE)
    if (ncol(x) != n.columns)
        return(FALSE)
    if (is.null(colnames(x)))
        return(FALSE)
    if (is.null(rownames(x)) & n.rows > 1)
        return(FALSE)
    isNumericOrInteger(x)
}

# assignNames <- function(y)
# {
#     if (is.null(rownames(y)))
#         return(paste("Series", 1:nrow(y)))
#     else
#         return(paste("Series", 1:ncol(y)))
# }

#' @export
AsChartMatrix <- function(y,
                          x = NULL,
                          # weights = NULL,
                          # subset = NULL,
                          transpose = FALSE,
                          aggregate.period = "month")  ## can be m(onth), q(uarter), y(ear)
{
    if (is.null(x)) # Aggregating data over X.
    {
        if (!is.vector(y) && !is.table(y) && !is.matrix(y))
            stop(paste("Y must be a vector"))

        y <- t(as.matrix(y))

        if(transpose)
            return(t(y))

        # if (is.null(rownames(y)))
        #     rownames(y) <- assignNames(y)
        #
        # if (is.null(colnames(y)))
        #     colnames(y) <- assignNames(y)

        return(y)
    }

    if (is.logical(x))
        stop(paste("X cannot be a logical vector"))

    if (is.list(x) | is.data.frame(x))
        stop(paste("X cannot take data frames or lists. You have passed a ",class(x), sep=""))

    if (is.list(y))
        y <- as.data.frame(y)

    if (!equalNumberOfRows(y, x))
        stop("The length of all the elements in a list must be the same, but your Y input is ",
             numberOfRows(y), " and your X input is ", numberOfRows(x))

    if (is.factor(y) | is.character(y))
        return(xtabs(~ x + y))

    y <- aggregate(y, list(x), mean)

    if (inherits(x, "POSIXct"))
        rownames(y) <- textPeriodFromDate(y[, 1],aggregate.period)
    else
        rownames(y) <- y[, 1]

    return(t(y[, -1, drop = FALSE]))
}
