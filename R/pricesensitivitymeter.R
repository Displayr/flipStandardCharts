#' Plots chart analysing price sensitivity
#'
#' @inherit Line
#' @param x Input table containing survey responses in 4 columns
#'   At what price would you consider this product/brand to be 
#'   1) Very cheap, 2) Cheap, 3) Expensive, 4) Very expensive.
#' @param resolution Controls how grid points on the x-axis.
#' @param intersection.show Logical; Whether to show labels to the intersection points of the lines.
#' @param intersection.arrow.color Color of the arrows to the intersection points.
#' @param intersection.arrow.size Size of the arrows to the intersection points.
#' @param intersection.arrow.width Width of the arrow heads.
#' @param intersection.arrow.length Scaling factor controlling length of arrows.
#' @param intersection.arrow.standoff Distance between arrowhead and intersection point.
#' @param intersection.label.font.color intersection.label font color as a named color in
#' character format (e.g. "black") or an a hex code.
#' @param intersection.label.font.family Character; intersection.label font family
#' @param intersection.label.font.size Integer; intersection label font size
#' @param intersection.label.wrap Logical; whether the intersection label text should be wrapped.
#' @param intersection.label.wrap.nchar Number of characters (approximately) in each
#' line of the intersection label when \code{intersection.label.wrap} \code{TRUE}.
#' @param ... Other charting parameters passed to \link{Line}.
#' @importFrom plotly layout
#' @export

PriceSensitivityMeter <- function(x,
                                  colors = c("#FF0000", "#FF0000", "#008000", "#008000"), 
                                  line.type = c("dot", "solid", "solid", "dot"),
                                  line.thickness = c(1, 2, 2, 1),
                                  global.font.family = "Arial",
                                  global.font.color = rgb(44, 44, 44, maxColorValue = 255),
                                  x.title = "Price",
                                  x.tick.prefix = "$",
                                  x.hovertext.format = ".2f",
                                  y.title = "Proportion of respondents",
                                  y.tick.format = "%", 
                                  resolution = 0.05,
                                  intersection.show = TRUE,
                                  intersection.arrow.color = global.font.color,
                                  intersection.arrow.size = 1.6,
                                  intersection.arrow.width = 0.7,
                                  intersection.arrow.length = 10,
                                  intersection.arrow.standoff = 3,
                                  intersection.label.font.family = global.font.family,
                                  intersection.label.font.color = global.font.color,
                                  intersection.label.font.size = 10,
                                  intersection.label.wrap = TRUE,
                                  intersection.label.wrap.nchar = 21, 
                                  ...)
{
    if (ncol(x) < 4)
        stop("Price sensitivity meter needs input data containing 4 columns: ",
            "'Very cheap', 'Cheap', 'Expensive', 'Very expensive'")
    rg <- range(x, na.rm = TRUE)
    xpts <- seq(from = rg[1], to = rg[2], by = resolution)

    # Compute proportions - cannot use ecdf because we want '>=' not '>'
    pcm.dat <- matrix(NA, nrow = length(xpts), ncol = 4,
                      dimnames = list(xpts, c("Less than 'Very cheap'",
                      "Less than 'Cheap'", "More than 'Expensive'", "More than 'Very expensive'")))
    pcm.dat[,1] <- sapply(xpts, function(xx) mean(x[,1] <= xx, na.rm = TRUE))
    pcm.dat[,2] <- sapply(xpts, function(xx) mean(x[,2] <= xx, na.rm = TRUE))
    pcm.dat[,3] <- sapply(xpts, function(xx) mean(x[,3] >= xx, na.rm = TRUE))
    pcm.dat[,4] <- sapply(xpts, function(xx) mean(x[,4] >= xx, na.rm = TRUE))

    # Find intersection points
    ind.intersect <- rep(NA, 4)
    ind.intersect[1] <- which.min(abs(pcm.dat[,1] - pcm.dat[,3]))
    ind.intersect[2] <- which.min(abs(pcm.dat[,1] - pcm.dat[,4]))
    ind.intersect[3] <- which.min(abs(pcm.dat[,2] - pcm.dat[,3]))
    ind.intersect[4] <- which.min(abs(pcm.dat[,2] - pcm.dat[,4]))

    pp <- Line(pcm.dat, colors = colors, line.type = line.type, line.thickness = line.thickness,
               global.font.family = global.font.family, global.font.color = global.font.color,
               x.title = x.title, x.tick.prefix = x.tick.prefix, x.hovertext.format = x.hovertext.format,
               y.title = y.title, y.tick.format = y.tick.format, ...)
    
    if (intersection.show)
        pp$htmlwidget <- layout(pp$htmlwidget,
                            annotations = list(xref = "x", yref = "y",
                                x = xpts[ind.intersect],
                                y = c(pcm.dat[ind.intersect[1:2],1],pcm.dat[ind.intersect[3:4],2]),
                                arrowsize = intersection.arrow.size, arrowwidth = intersection.arrow.width,
                                arrowcolor = intersection.arrow.color, standoff = intersection.arrow.standoff,
                                axref = "pixel", ax = c(-10, 0, 0, 10) * intersection.arrow.length,
                                ayref = "pixel", ay = c(2, -5, 5, 2) * intersection.arrow.length,
                                font = list(family = intersection.label.font.family,
                                    color = intersection.label.font.color, size = intersection.label.font.size),
                                text = autoFormatLongLabels(sprintf("%s $%.2f", c("Point of marginal cheapness",
                                    "Optimal price point", "Indifference point price",
                                    "Point of marginal expensiveness"), xpts[ind.intersect]), 
                                     wordwrap = intersection.label.wrap, intersection.label.wrap.nchar)))
   return(pp)
}


