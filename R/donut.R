#' Donut chart
#' @inherit Pie
#' @export
Donut <- function(x,
                  colors = NULL,
                  pie.subslice.colors = NULL,
                  pie.subslice.colors.repeat = TRUE,
                  title = "",
                  title.font.family = global.font.family,
                  title.font.size = 16,
                  title.font.color = global.font.color,
                  subtitle = "",
                  subtitle.font.family = global.font.family,
                  subtitle.font.size = 12,
                  subtitle.font.color = global.font.color,
                  footer = "",
                  footer.font.family = global.font.family,
                  footer.font.size = 8,
                  footer.font.color = global.font.color,
                  footer.wrap = TRUE,
                  footer.wrap.nchar = 100,
                  pie.data.threshold = NULL,
                  pie.values.order = "initial",
                  data.label.format = "",
                  data.label.prefix = "",
                  data.label.suffix = "",
                  data.label.font.size = 10,
                  data.label.font.color = global.font.color,
                  data.label.font.family = global.font.family,
                  pie.groups.font.size = data.label.font.size,
                  pie.groups.font.color = data.label.font.color,
                  pie.groups.font.family = data.label.font.family,
                  pie.groups.order = "initial",
                  pie.inner.radius = 70,
                  pie.border.color = rgb(255, 255, 255, maxColorValue = 255),
                  global.font.family = "Arial",
                  global.font.color = rgb(44, 44, 44, maxColorValue = 255))
{
    cl <- match.call()
    cl <- c(cl[1], lapply(cl[-1], evalc, env = parent.frame()))
    cl <- as.call(cl)
    cl <- cl[-1]
    cl$type <- "Donut"
    if (is.null(cl$pie.inner.radius))
        cl$pie.inner.radius <- 70
    return(do.call(Pie, as.list(cl)))
}

