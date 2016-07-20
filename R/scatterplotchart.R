scatterPlotChart <- function(chart.matrix,
                             transpose,
                             series.marker.text,
                             x.tick.frequency,
                             x.tick.decimals,
                             x.labels,
                             x.bounds.minimum,
                             x.bounds.maximum,
                             x.bounds.units.major)
{

    if (!is.matrix(chart.matrix))
        chart.matrix <- as.matrix(chart.matrix)

    ## if source data only has one column with multiple rows, then transpose
    if (ncol(chart.matrix) == 1)
        chart.matrix <- t(chart.matrix)

    ## Showing markers and lines
    series.mode = "markers"  #default = markers

    if (series.marker.text)
        series.mode <- paste(series.mode, "+text", sep = "")

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
        #x.tick.frequency <- ifelse(is.null(x.tick.frequency), ncol(chart.matrix) / divide.by, x.tick.frequency)
        #x.tick.decimals <- 0 #ifelse(is.null(x.tick.decimals), 0, x.tick.decimals)

        if (is.null(x.bounds.minimum))
            x.bounds.minimum <- 0

        if (is.null(x.bounds.maximum))
            x.bounds.maximum <- ncol(chart.matrix)

        if (is.null(x.bounds.units.major))
            x.bounds.units.major <- ncol(chart.matrix) / divide.by

        transpose <- FALSE
    }

    if (transpose)
        chart.matrix <- t(chart.matrix)

    ## If there are no column names at this stage, then assign them:
    if (is.null(colnames(chart.matrix)))
        colnames(chart.matrix) <- paste("trace ", 1:ncol(chart.matrix))

    return(list(chart.matrix = chart.matrix,
                series.mode = series.mode,
                transpose = transpose,
                x.tick.frequency = x.tick.frequency,
                x.tick.decimals = x.tick.decimals,
                x.bounds.minimum = x.bounds.minimum,
                x.bounds.maximum = x.bounds.maximum,
                x.bounds.units.major = x.bounds.units.major))
}
