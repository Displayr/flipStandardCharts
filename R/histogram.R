#' Histogram.

#' Plots a histogram of data.
#'
#' @inherit Distribution
#' @examples
#' Histogram(rnorm(100))
#' Histogram(list(rnorm(100), rexp(100)))
#' @export
Histogram <- function(x,
                 weights = NULL,
                 vertical = FALSE,
                 density.color = "#008000",
                 values.color = "#008000",
                 show.values = FALSE,
                 histogram.cumulative = FALSE,
                 histogram.counts = FALSE,
                 maximum.bins = NULL,
                 global.font.family = "Arial",
                 global.font.color = rgb(44, 44, 44, maxColorValue = 255),
                 title = "",
                 title.font.family = global.font.family,
                 title.font.color = global.font.color,
                 title.font.size = 16,
                 subtitle = "",
                 subtitle.font.family = global.font.family,
                 subtitle.font.color = global.font.color,
                 subtitle.font.size = 12,
                 footer = "",
                 footer.font.family = global.font.family,
                 footer.font.color = global.font.color,
                 footer.font.size = 8,
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
                 grid.show = FALSE,
                 values.title = "",
                 values.title.font.color = global.font.color,
                 values.title.font.family = global.font.family,
                 values.title.font.size = 12,
                 values.line.width = 0,
                 values.line.color = rgb(0, 0, 0, maxColorValue = 255),
                 values.tick.mark.length = 5,
                 values.bounds.minimum = NULL,
                 values.bounds.maximum = NULL,
                 values.tick.distance = NULL,
                 values.zero.line.width = 0,
                 values.zero.line.color = rgb(44, 44, 44, maxColorValue = 255),
                 values.grid.width = 1 * grid.show,
                 values.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                 values.tick.show = TRUE,
                 values.tick.suffix = "",
                 values.tick.prefix = "",
                 values.tick.format = "",
                 values.hovertext.format = "",
                 values.tick.angle = NULL,
                 values.tick.font.color = global.font.color,
                 values.tick.font.family = global.font.family,
                 values.tick.font.size = 10,
                 categories.tick.font.color = global.font.color,
                 categories.tick.font.family = global.font.family,
                 categories.tick.font.size = 10,
                 categories.tick.label.wrap = TRUE,
                 categories.tick.label.wrap.nchar = 21,
                 hovertext.font.family = global.font.family,
                 hovertext.font.size = 11,
                 modebar.show = FALSE)
{
    cl <- match.call()
    cl <- c(cl[1], lapply(cl[-1], evalc, env = parent.frame()))
    cl <- as.call(cl)
    args <- distributionArgs(cl, Histogram, list(density.type = "Histogram",
                                                                       show.mean = FALSE,
                                                                       show.median = FALSE,
                                                                       show.quartiles = FALSE,
                                                                       show.range = FALSE,
                                                                       show.mirror.density = FALSE))
    do.call(Distribution, args)
}
