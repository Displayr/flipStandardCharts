#' Box
#'
#' Box plot.
#'
#' @inherit Distribution
#' @examples
#' Box(rnorm(100))
#' Box(list(rnorm(100), rexp(100)))

#' @export
Box <- function(x,
                 weights = NULL,
                 vertical = TRUE,
                 show.values = FALSE,
                 density.color = "Green",
                 values.color = "Green",
                 box.points = "Suspected outliers",
                 global.font.family = "Arial",
                 global.font.color = rgb(44, 44, 44, maxColorValue = 255),
                 title = "",
                 title.font.family = global.font.family,
                 title.font.color = global.font.color,
                 title.font.size = 16,
                 background.fill.color = rgb(255, 255, 255, maxColorValue = 255),
                 background.fill.opacity = 1,
                 charting.area.fill.color = background.fill.color,
                 charting.area.fill.opacity = 1,
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
                 modebar.show = FALSE)
{
   args <- distributionArgs(match.call(), Box, list(density.type = "Box",
                                                                 show.mean = FALSE,
                                                                 show.median = FALSE,
                                                                 show.quartiles = FALSE,
                                                                 show.range = FALSE,
                                                    show.mirror.density = FALSE))
    do.call(Distribution, args)
}
