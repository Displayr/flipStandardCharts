lineChart <- function(chart.matrix,
                      transpose,
                      series.line.width,
                      series.marker.show,
                      series.marker.text)
{
    ## Check that line width is at least 1
    if (series.line.width < 1)
        series.line.width <- 1

    ## if source data only has one column with multiple rows, then transpose
    if (ncol(chart.matrix) == 1)
        transpose <- TRUE
    else
        transpose <- transpose

    ## Showing markers and lines
    series.mode = "lines+markers"  #default = line and marker

    if (series.line.width >= 1 && series.marker.show == "none")
        series.mode <- "lines"

    if (series.marker.text)
        series.mode <- paste(series.mode, "+text", sep = "")

    return(list(series.mode = series.mode,
                series.line.width = series.line.width,
                transpose = transpose))
}
