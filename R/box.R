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
                 density.color = "#008000",
                 values.color = "#008000",
                 box.points = "Suspected outliers",
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
                 grid.show = FALSE,
                 values.title = "",
                 values.title.font.color = global.font.color,
                 values.title.font.family = global.font.family,
                 values.title.font.size = 12,
                 values.line.width = 0,
                 values.line.color = rgb(0, 0, 0, maxColorValue = 255),
                 values.tick.mark.length = 0,
                 values.bounds.minimum = NULL,
                 values.bounds.maximum = NULL,
                 values.tick.distance = NULL,
                 values.tick.maxnum = NULL,
                 values.tick.mark.color = "transparent",
                 values.zero = FALSE,
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
                 categories.tick.angle = NULL,
                 categories.tick.label.wrap = TRUE,
                 categories.tick.label.wrap.nchar = 21,
                 categories.tick.mark.length = 20,
                 hovertext.font.family = global.font.family,
                 hovertext.font.size = 11,
                 hover.on = c("all", "points")[1],
                 modebar.show = FALSE)
{
    cl <- match.call()
    cl <- c(cl[1], lapply(cl[-1], evalc, env = parent.frame()))
    cl <- as.call(cl)
   args <- distributionArgs(cl, Box, list(density.type = "Box",
                                                                 show.mean = FALSE,
                                                                 show.median = FALSE,
                                                                 show.quartiles = FALSE,
                                                                 show.range = FALSE,
                                                    show.mirror.density = FALSE))
    do.call(Distribution, args)
}
