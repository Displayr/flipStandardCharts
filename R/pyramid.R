#' Pyramid
#'
#' Bar charts with a centred axis
#' @inherit Column
#' @inherit Bar
#' @importFrom flipChartBasics ChartColors
#' @importFrom plotly plot_ly layout
#' @export
Pyramid <- function(x,
                    annotation.list = NULL,
                    colors = ChartColors(max(1, length(x))),
                    opacity = NULL,
                    global.font.family = "Arial",
                    global.font.color = rgb(44, 44, 44, maxColorValue = 255),
                    title = "",
                    title.font.family = global.font.family,
                    title.font.color = global.font.color,
                    title.font.size = 16,
                    title.align = "center",
                    subtitle = "",
                    subtitle.font.family = global.font.family,
                    subtitle.font.color = global.font.color,
                    subtitle.font.size = 12,
                    subtitle.align = "center",
                    footer = "",
                    footer.font.family = global.font.family,
                    footer.font.color = global.font.color,
                    footer.font.size = 8,
                    footer.align = "center",
                    footer.wrap = TRUE,
                    footer.wrap.nchar = 100,
                    background.fill.color = "transparent",
                    background.fill.opacity = 1,
                    charting.area.fill.color = background.fill.color,
                    charting.area.fill.opacity = 0,
                    margin.top = NULL,
                    margin.bottom = NULL,
                    margin.left = NULL,
                    margin.right = NULL,
                    margin.inner.pad = NULL,
                    margin.autoexpand = TRUE,
                    y.title = "",
                    y.title.font.color = global.font.color,
                    y.title.font.family = global.font.family,
                    y.title.font.size = 12,
                    y.line.width = 0,
                    y.line.color = rgb(0, 0, 0, maxColorValue = 255),
                    y.tick.mark.length = 3,
                    y.tick.mark.color = "transparent",
                    y.bounds.minimum = NULL,
                    y.bounds.maximum = NULL,
                    y.tick.distance = NULL,
                    y.tick.maxnum = 11,
                    y.zero = FALSE,
                    y.zero.line.width = 0,
                    y.zero.line.color = rgb(225, 225, 225, maxColorValue = 255),
                    y.data.reversed = FALSE,
                    y.grid.width = 0,
                    y.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                    y.tick.show = TRUE,
                    y.tick.suffix = "",
                    y.tick.prefix = "",
                    y.tick.format= "",
                    y.hovertext.format= y.tick.format,
                    y.tick.angle = NULL,
                    y.tick.font.color = global.font.color,
                    y.tick.font.family = global.font.family,
                    y.tick.font.size = 10,
                    x.title = "",
                    x.title.font.color = global.font.color,
                    x.title.font.family = global.font.family,
                    x.title.font.size = 12,
                    x.line.width = 0,
                    x.line.color = rgb(0, 0, 0, maxColorValue = 255),
                    x.tick.marks = "",
                    x.tick.mark.length = 0,
                    x.tick.mark.color = "transparent",
                    x.bounds.maximum = NULL,
                    x.tick.distance = NULL,
                    x.tick.maxnum = NULL,
                    x.zero = TRUE,
                    x.zero.line.width = 0,
                    x.zero.line.color = rgb(225, 225, 225, maxColorValue = 255),
                    x.data.reversed = FALSE,
                    x.grid.width = 0,
                    x.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                    x.tick.show = FALSE,
                    x.tick.suffix = "",
                    x.tick.prefix = "",
                    x.tick.format = "",
                    x.tick.angle = NULL,
                    x.tick.font.color = global.font.color,
                    x.tick.font.family = global.font.family,
                    x.tick.font.size = 10,
                    y.tick.label.wrap = TRUE,
                    y.tick.label.wrap.nchar = 21,
                    hovertext.font.family = global.font.family,
                    hovertext.font.size = 11,
                    hovertext.template = NULL,
                    marker.border.width = 1,
                    marker.border.colors = colors,
                    marker.border.opacity = NULL,
                    data.label.show = FALSE,
                    data.label.font.autocolor = FALSE,
                    data.label.font.family = global.font.family,
                    data.label.font.size = 10,
                    data.label.font.color = global.font.color,
                    data.label.format = "",
                    data.label.prefix = "",
                    data.label.suffix = "",
                    data.label.threshold = NULL,
                    x.hovertext.format = data.label.format,
                    tooltip.show = TRUE,
                    modebar.show = FALSE,
                    axis.drag.enable = FALSE,
                    bar.corner.radius = 0.0,
                    bar.gap = 0.15)
{
    ErrorIfNotEnoughData(x)
    chart.matrix <- checkMatrixNames(x)
    if (NROW(chart.matrix) == 1 && NCOL(chart.matrix) > 1)
       chart.matrix <- t(chart.matrix)
    if (NCOL(chart.matrix) > 1)
    {
        warning("'Pyramid' showing only show a single series. To show multiple series use Small Multiples")
        chart.matrix <- chart.matrix[,1, drop = FALSE]
    }
    ss <- sign(chart.matrix[is.finite(chart.matrix)])
    if (any(ss * ss[1] < 0))
        stop("'Pyramid' charts cannot show a mixture of positive and negative values.")

    cl <- match.call()
    cl <- c(cl[1], lapply(cl[-1], evalc, env = parent.frame()))
    cl <- as.call(cl)
    cl <- cl[-1]
    cl$colors <- colors
    cl$pyramid <- TRUE
    cl$multi.colors.within.series <- TRUE
    cl$x.tick.show <- x.tick.show
    cl$x.grid.width <- x.grid.width
    do.call(Bar, as.list(cl))

}

