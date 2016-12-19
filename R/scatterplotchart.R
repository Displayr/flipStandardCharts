scatterPlotChart <- function(chart.matrix,
                             x.bounds.minimum,
                             x.bounds.maximum,
                             x.tick.distance)
{
    if (any(is.na(as.matrix(chart.matrix))))
    {
        warning("Data points with missing values have been omitted.")
        chart.matrix <- chart.matrix[!is.na(rowSums(chart.matrix)), ]
    }

    if (!is.matrix(chart.matrix))
        chart.matrix <- as.matrix(chart.matrix)

    ## Showing markers and lines
    series.mode = "markers"  #default = markers

    ## If chart.matrix has no colnames, and only one row, and y is a single numerical vector, then:
    ## colnames become the index value
    ## but xlabels becomes every x-label inteval value (if none, then 10 evenly spaced values)
    ## and xvalues also get set...?

    divide.by <- 10
    if (ncol(chart.matrix) <= 10)
        divide.by <- ncol(chart.matrix)

    colname.upperRange <- ncol(chart.matrix) - 1

    if (is.null(colnames(chart.matrix)) ) #&& nrow(chart.matrix) == 1
    {
        colnames(chart.matrix) <- 0:colname.upperRange

        if (is.null(x.bounds.minimum))
            x.bounds.minimum <- 0

        if (is.null(x.bounds.maximum))
            x.bounds.maximum <- ncol(chart.matrix)

        if (is.null(x.tick.distance))
            x.tick.distance <- ncol(chart.matrix) / divide.by
    }

    ## If there are no column names at this stage, then assign them:
    if (is.null(colnames(chart.matrix)))
        colnames(chart.matrix) <- paste("trace ", 1:ncol(chart.matrix))

    return(list(chart.matrix = chart.matrix,
                series.mode = series.mode,
                x.bounds.minimum = x.bounds.minimum,
                x.bounds.maximum = x.bounds.maximum,
                x.tick.distance = x.tick.distance))
}
