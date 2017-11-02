#' Bean
#'
#' Bean plot.
#'
#' @inherit Distribution
#' @examples
#' Bean(rnorm(100))
#' Bean(list(rnorm(100), rexp(100)))
#' @importFrom utils modifyList
#' @importFrom pryr modify_call
#' @export
Bean <- function(x,
                   weights = NULL,
                   vertical = FALSE,
                   density.color = "Green",
                   values.color = "Green",
                   automatic.lower.density = TRUE,
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
    cl <- match.call()
    cl <- c(cl[1], lapply(cl[-1], evalc, env = parent.frame()))
    cl <- as.call(cl)
    args <- distributionArgs(cl, Bean, list(density.type = "Density",
                                                                 show.mean = FALSE,
                                                                 show.median = FALSE,
                                                                 show.quartiles = FALSE,
                                                                 show.range = FALSE,
                                                     show.values = TRUE))

    do.call(Distribution, args)
}
#
#     args <- modifyList(as.list(args(Bean)), list(density.type = "Density",
#                                                                  show.mean = FALSE,
#                                                                  show.median = FALSE,
#                                                                  show.quartiles = FALSE,
#                                                                  show.range = FALSE,
#                                                                   show.values = TRUE))
#     call <- match.call()
#     nms <- names(args)
#     nms <- nms[nms != ""]
#     nms <- nms[!nms %in% names(call)]
#     args <- args[nms]
#     args <- args[!sapply(args, is.null)]
#     call[[1]] <- Distribution
#     call <- modify_call(call, args)
#    # eval(call)
#     do.call(Distribution, (as.list(call[-1])))




