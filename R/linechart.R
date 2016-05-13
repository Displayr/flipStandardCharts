lineChart <- function(series.line.width,
                      series.marker.show,
                      series.marker.text)
{
    ## Check that line width is at least 1
    if (series.line.width < 1)
        series.line.width <- 1

    ## Showing markers and lines
    series.mode = "lines+markers"  #default = line and marker

    if (series.line.width >= 1 && series.marker.show == "none")
        series.mode <- "lines"

    if (series.marker.text)
        series.mode <- paste(series.mode, "+text", sep = "")

    return(list(series.mode = series.mode,
                series.line.width = series.line.width))
}
