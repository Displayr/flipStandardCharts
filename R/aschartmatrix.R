# 'AsChartMatrix' checks if data is in the appropriate format and attempts to coerce it to the appropriate
# format if so required.

# A 'ChartMatrix' is/has
# - A 'matrix' of type 'integer' or 'numeric'
# - Columns represent the X-axis
# - Column names
# - Row names if there are more than one rows

# textPeriodsFromDate <- function(x, period = "month")
# {
#     year.two.digits <- strftime(x, "%y")
#
#     df <- cbind(strftime(x, "%Y"), quarters(x), paste(strftime(x, "%Y"), quarters(x), sep = ""), substr(months(x), 1, 3), strftime(x,"%m"), strftime(x,"%Y%m"))
#
#     return(df)
# }

aggregatePeriodFromDate <- function(x, period = "month")
{
    year.two.digits <- strftime(x, "%y")

    if (period == "month")
        return(strftime(x,"%Y%m"))
        #

    if (period == "quarter")
        return(paste(strftime(x, "%Y"), quarters(x), sep = ""))
        # return(paste(quarters(x), year.two.digits, sep = "-"))

    if (period == "year")
        return(strftime(x, "%Y"))
}

dateLabelling <- function(y, period = "month")
{
    year.two.digits <- substr(y[, 1], 3,4)

    mth <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

    if (period == "month")
        y[, 1] <- paste(mth[as.integer(substr(y[, 1], 5, 6))], year.two.digits, sep = "-")

    if (period == "quarter")
        y[, 1] <- paste(substr(y[, 1], 5, 6), year.two.digits, sep = "-")

    y
}

formatDateRowNames <- function(y, period = "month")
{
    format.names <- rownames(y)

    year.two.digits <- substr(format.names, 3,4)

    mth <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

    if (period == "month")
        format.names <- paste(mth[as.integer(substr(format.names, 5, 6))], year.two.digits, sep = "-")

    if (period == "quarter")
        format.names <- paste(substr(format.names, 5, 6), year.two.digits, sep = "-")

    rownames(y) <- format.names
    y
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
    if (!is.matrix(x) && !is.data.frame(x) && !is.list(x))
        attributes(x) <- NULL

    if (is.vector(x) | inherits(x, "POSIXct"))
        return(length(x))
    nrow(x)
}

equalNumberOfRows <- function(y, x) {numberOfRows(y) == numberOfRows(x)}


#' Test if a matrix is appropriate for charting.
#'
#' \code{IsChartMatrix} returns TRUE if the given matrix is suitable for charting, else FALSE.
#'
#' @param x A matrix.
#' @param n.rows The number of rows in the matrix.
#' @param n.columns The number of columns in the matrix.
#' @return Logical; if matrix is a chart matrix.
#' @examples
#' data("z")
#' IsChartMatrix(z, nrow(z), ncol(z))
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

#' Converts one or more objects to a chart matrix.
#'
#' \code{AsChartMatrix} checks if data is in the appropriate format and
#' attempts to coerce it to the appropriate format if so required.
#'
#' @param y A vector, matrix, list of vectors, data frame, or table.
#' @param x A vector over which y will be aggregated. Must have the same
#' number of elements as y.
#' @param transpose Logical; should the final output be transposed?
#' @param aggregate.period Period over which date varaibles passed to the x
#' argument will be aggregated. Defaults to "month", and can also be "quarter"
#' or "year".
#' @return A chart matrix with named rows and columns.
#' @examples
#' data("y.data")
#' data("x.data")
#' data("var1")
#' data("x.dates")
#' AsChartMatrix(y = y.data, x = x.data, transpose = FALSE)
#' AsChartMatrix(y = var1, x = x.dates, transpose = TRUE, aggregate.period = "year")
#' @export
AsChartMatrix <- function(y,
                          x = NULL,
                          # weights = NULL,
                          # subset = NULL,
                          transpose = FALSE,
                          aggregate.period = "month")  ## can be m(onth), q(uarter), y(ear)
{
    if (is.logical(x) && length(x) == 1)
        x <- NULL

    if (is.null(x)) # Aggregating data over X.
    {
        if (!is.vector(y) && !is.table(y) && !is.matrix(y))
            stop(paste("Y must be a vector"))

        y <- t(as.matrix(y))

        if(transpose)
            return(t(y))

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

    ## Set dates to numeric values before aggregating to ensure correct sort order
    date.labelling <- FALSE
    if (inherits(x, "POSIXct"))
    {
        x <- aggregatePeriodFromDate(x, period = aggregate.period)
        date.labelling <- TRUE
    }

    if (is.factor(y) | is.character(y))
    {
        y <- xtabs(~ x + y)

        if (date.labelling <- TRUE)
        {
            y <- formatDateRowNames(y, period = aggregate.period)
            y <- t(y)
        }

        if (transpose)
            return(t(y))

        return(y)
    }

    ## Aggregate (all cases)
    y <- aggregate(y, list(x), mean)

    rownames(y) <- y[, 1]

    ## Fix up labelling for aggregated dates
    if (date.labelling == TRUE)
        y <- formatDateRowNames(y, period = aggregate.period)

    y <- y[, -1, drop = FALSE]

    if (ncol(y) == 1)
        transpose <- TRUE

    if (!transpose)
        return(y)

    return(t(y))
}
