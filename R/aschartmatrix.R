# 'AsChartMatrix' checks if data is in the appropriate format and attempts to coerce it to the appropriate
# format if so required.

# A 'ChartMatrix' is/has
# - A 'matrix' of type 'integer' or 'numeric'
# - Columns represent the X-axis
# - Column names
# - Row names if there are more than one rows



isNumericOrInteger <- function(y)
{
    if (!is.integer(y) && !is.numeric(y))
    {
        vector.class <- class(y)
        vector.mode <- mode(y)
        stop(paste("Vectors passed to the y-parameter must be of the mode numeric or integer.  The vector passed is a",vector.class,"of mode: ",vector.mode))
    }
    TRUE
}

numberOfRows <- function(x)
{
    if (is.vector(x) | is.factor(x))
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

#' @export
AsChartMatrix <- function(y,
                          x = NULL,
                          # weights = NULL,
                          # subset = NULL,
                          transpose = FALSE)
{
    if (!is.null(x)) # Aggregating data over X.
    {
        if (is.list(y))
            y <- as.data.frame(y)

        if (!equalNumberOfRows(y, x))
            stop("The length of all the elements in a list must be the same, but your Y input is ",
                 numberOfRows(y), " and your X input is ", numberOfRows(x))

        if (is.factor(y) | is.character(y))
            return(xtabs(~ x + y))

        y <- aggregate(y, list(x), mean)
        rownames(y) <- y[, 1]
        return(t(y[, -1, drop = FALSE]))
    }
    else # Manipulating aggregated data
    {
        if (is.vector(y))
            y <- t(as.matrix(y))
    }
    if(transpose)
        return(t(y))
    y
}





#
#
#
#
#
#
#
#
#
#     # 1. Work out what data format y is.
#     ## Is y a named vector?
#     if (is.vector(y) && is.null(names(y)) && !is.list(y))
#     {
#         data.type <- "Named vector"
#         y.length <- length(y)
#     }
#
#     ## Is y an unnamed vector?
#     # if (is.vector(y) && length(names(y)) == 0) data.type <- "Unnamed vector"
#
#     ## Is y a table with one (or more) rows?  Well, if it's a table, then it's a df or a matrix...
#     if (is.data.frame(y))
#     {
#         data.type <- "Data frame"
#         y.length <- nrows(y)
#     }
#
#     if (is.matrix(y))
#     {
#         data.type <- "Matrix"
#         y.length <- nrows(y)
#     }
#
#     ## Is y a list?
#     if (is.list(y) && !is.data.frame(y))
#     {
#         data.type <- "List"
#         y.length <- length(y[[1]])
#     }
#
#     ## Is y a factor?
#     if (is.factor(y))
#     {
#         data.type <- "Factor"
#         acceptable.input <- TRUE
#         y.length <- length(y)
#     }
#
#     # 2. Check for numeric and integer types in vectors, matrices, data frames and lists.
#     ## If y is a vector, then make sure it is either an integer or numeric vector
#     if (data.type == "Named vector" || data.type == "Matrix")
#     {
#
#     }
#
#     # 3. If it's a data frame or a list then all columns/list elements must be either integer or numeric
#     if (data.type == "Data frame" || data.type == "List")
#     {
#         temp.list.element.length <- integer()
#         for (i in 1:length(y))
#         {
#             if (data.type == "List") temp.mode <- mode(y[[i]])
#             # Store lengths of list elements
#             temp.list.element.length <- c(temp.list.element.length, length(y[[i]]))
#
#             if (data.type == "Data frame") temp.mode <- mode(y[,i])
#
#             if (temp.mode != "numeric" && temp.mode != "integer")
#             {
#                 stop("List and data frame elements must all be integer or numeric vectors.")
#             }
#         }
#         # If the list elements have variable lengths, then stop.
#         if (length(unique(temp.list.element.length) != 1))
#         {
#             stop("The length of all the elements in a list must be the same.")
#         }
#         acceptable.input <- TRUE
#     }
#
#     # A. Check if there is an x.  If there isn't, go to 6(?), else 8.
#     if (x != NULL) x.length <- length(x)
#
#
#     # 4. Check what data type x is.  Can be date, character, (Factor?  If it's numeric factor???); what if it's people's age? 0 to 100?
#
#     # 5. Check length of x vs. y
#
#     # 6. Check length of weights vs x/y
#
#     # 7. If tabluated data, would we weight it?  Else weight raw data.
#
#     # 8. Tabulate data that isn't already in a table (vectors, factors, vectors in lists)
#
#     # 9. How do we identify if a table needs to be transposed or not?
#
#     # 10. Transpose tables that need to be transposed
#
#     # 11. Double-check that our tables are matrices with named rows and columns.
#
#     # 12. Return the matrix to AreaCharts()
#
#     return(data.type)
# }
