scatterPlotChart <- function(chart.matrix,
                             transpose,
                             series.marker.text)
{

    ## if source data only has one column with multiple rows, then transpose
    if (ncol(chart.matrix) == 1)
        transpose <- TRUE
    else
        transpose <- transpose

    ## Showing markers and lines
    series.mode = "markers"  #default = markers

    if (series.marker.text)
        series.mode <- paste(series.mode, "+text", sep = "")

    return(list(series.mode = series.mode,
                transpose = transpose))
}
