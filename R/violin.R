#' Violin
#'
#' Violin plot.
#'
#' @inherit Distribution
#' @examples
#' Distribution(list(rnorm(100)))

#' @export
Violin <- function(x,
    weights = NULL,
    vertical = TRUE,
    show.mean = TRUE,
    show.median = TRUE,
    show.quartiles = TRUE,
    show.range = TRUE,
    mean.color = "White",
    median.color = "Black",
    quartile.color = "Black",
    range.color = "Black",
    density.color = "Green",
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
    values.tick.marks = "",
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
    values.tick.decimals = NULL,
    values.tick.format.manual = "",
    values.hovertext.decimals = NULL,
    values.hovertext.format.manual = "",
    values.tick.angle = NULL,
    values.tick.font.color = global.font.color,
    values.tick.font.family = global.font.family,
    values.tick.font.size = 10,
    categories.font.color = global.font.color,
    categories.font.family = global.font.family,
    categories.font.size = 10,
    categories.label.wrap = TRUE,
    categories.label.wrap.nchar = 21,
    modebar.show = FALSE,
    us.date.format = NULL)
{
    call <- match.call()
    call[[1]] <- Distribution
    call$density.type <- "Density"
    call$automatic.lower.density <- TRUE
    call$histogram.cumulative <- FALSE
    call$histogram.counts <- FALSE
    call$maximum.bins <- NULL
    call$show.density <- TRUE
    call$values.color <- "Green"
    call$box.points <- "Shouldn't do anything"
    call$show.mirror.density <- TRUE
    call$show.values <- FALSE
    eval(call)
}
