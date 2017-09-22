#' @rdname ColumnChart
#' @export
StackedColumnChart <- function(...)
{
    return(ColumnChart(type="Stacked Column", ...))
}

#' @rdname AreaChart
#' @export
StackedAreaChart <- function(...)
{
    return(AreaChart(type="Stacked Area", ...))
}

#' @rdname BarChart
#' @export
StackedBarChart <- function(...)
{
    return(BarChart(type="Stacked Bar", ...))
}
