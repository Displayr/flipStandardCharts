#' Violin
#'
#' Violin plot.
#'
#' @inherit Distribution
#' @examples
#' Violin(rnorm(100))
#' Violin(list(rnorm(100), rexp(100)))

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
    bw = "nrd0",
    adjust = 1,
    kernel = c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight", "cosine", "optcosine"),
    n = 512,
    from = NULL,
    to = NULL,
    cut = 3,
    automatic.lower.density = TRUE,
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
    background.fill.color = rgb(255, 255, 255, maxColorValue = 255),
    background.fill.opacity = 0,
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
    modebar.show = FALSE)
{
    cl <- match.call()
    cl <- c(cl[1], lapply(cl[-1], evalc, env = parent.frame()))
    cl <- as.call(cl)
    args <- distributionArgs(cl, Violin, list(density.type = "Density"))
    do.call(Distribution, args)
}









