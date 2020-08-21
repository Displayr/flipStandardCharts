#' Generates a palm tree plot from a table of data.
#'
#' This function wraps the PalmTrees function in the rhtmlPalmTrees package and performs additional data preparation.
#'
#' @inherit Column
#' @param table A matrix of data to be displayed. Each row is represented by a palm tree, the value in each column
#' determines the area of a leaf on each palm tree.
#' @param y.axis.show Logical; whether to show the y-axis.
#' @param legend.border.color Color of the legend border.
#' @param legend.background.color Background color of the legend.
#' @examples
#' z <- matrix(runif(20), nrow = 4)
#' rownames(z) <- c("oak", "elm", "birch", "fir")
#' colnames(z) <- c("leaf", "frond", "twig", "branch", "stick")
#' Palm(z)
#' @importFrom rhtmlPalmTrees PalmTrees
#' @export

Palm <- function(table,
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
                 x.tick.font.family = global.font.family,
                 x.tick.font.color = global.font.color,
                 x.tick.font.size = 11,
                 x.title = "",
                 x.title.font.family = global.font.family,
                 x.title.font.color = global.font.color,
                 x.title.font.size = 12,
                 legend.font.family = global.font.family,
                 legend.font.color = global.font.color,
                 legend.font.size = 11,
                 legend.background.color = "transparent",
                 legend.border.color = "#000000",
                 y.axis.show = TRUE,
                 tooltip.show = TRUE,
                 y.tick.format = "",
                 y.tick.font.family = global.font.family,
                 y.tick.font.color = global.font.color,
                 y.tick.font.size = 11,
                 y.title = "",
                 y.title.font.family = global.font.family,
                 y.title.font.color = global.font.color,
                 y.title.font.size = 12,
                 y.tick.prefix = NULL,
                 y.tick.suffix = NULL,
                 hovertext.font.family = global.font.family,
                 hovertext.font.size = 11,
                 colors = NULL)
{
    ErrorIfNotEnoughData(table)
    stat <- attr(table, "statistic")
    #  Automatic formatting with statistic of '%'
    if (y.tick.format == "" && !is.null(stat) && grepl("%)?$", stat))
        y.tick.format <- ".0%"

    if (is.null(dim(table)) || length(dim(table)) == 1L)
        table <- as.matrix(table)

    # Convert from d3 formatting
    y.decimals <- decimalsFromD3(y.tick.format, decimalsToDisplay(table))
    if (percentFromD3(y.tick.format)) {
        table <- table * 100
        y.tick.suffix <- paste("%", y.tick.suffix)
        if (identical(y.title, "%"))
            y.title <- NULL
    }

    # Must have a legend
    if (is.null(legend.font.size))
        legend.font.size = 11

    palm <- rhtmlPalmTrees::PalmTrees(data = table,
                                      weights = NULL,                               # Numeric vector; length must equal to ncol(data)
                                      row.names = NULL,                             # Default values are rownames(data)
                                      row.font.size = x.tick.font.size,
                                      row.font.family = x.tick.font.family,
                                      row.font.color = x.tick.font.color,
                                      row.heading = x.title,
                                      row.heading.font.size = x.title.font.size,
                                      row.heading.font.color = x.title.font.color,
                                      row.heading.font.family = x.title.font.family,
                                      col.names = NULL,                             # Shown on legend. Default values are colnames(data)
                                      col.font.size = legend.font.size,             # Shown in legend
                                      col.font.family = legend.font.family,         # Shown in legend
                                      col.font.color = legend.font.color,
                                      col.heading = "",                             # Legend heading, defaults to "Columns" if NULL
                                      col.heading.font.size = 0,
                                      col.heading.font.family = legend.font.family,
                                      col.heading.font.color = legend.font.color,
                                      legend.background.color = legend.background.color,
                                      legend.border.color = legend.border.color,
                                      title = title,
                                      title.font.size = title.font.size,
                                      title.font.family = title.font.family,
                                      title.font.color = title.font.color,
                                      subtitle = subtitle,
                                      subtitle.font.size = subtitle.font.size,
                                      subtitle.font.family = subtitle.font.family,
                                      subtitle.font.color = subtitle.font.color,
                                      footer = footer,
                                      footer.font.size = footer.font.size,
                                      footer.font.family = footer.font.family,
                                      footer.font.color = footer.font.color,
                                      tooltips = tooltip.show,
                                      tooltips.font.size = hovertext.font.size,
                                      tooltips.font.family = hovertext.font.family,
                                      tooltips.heading.font.size = x.title.font.size,
                                      tooltips.heading.font.family = legend.font.family,
                                      y.show = y.axis.show,
                                      y.digits = y.decimals,
                                      y.font.size = y.tick.font.size,
                                      y.font.family = y.tick.font.family,
                                      y.font.color = y.tick.font.color,
                                      y.lab = y.title,
                                      y.lab.font.size = y.title.font.size,
                                      y.lab.font.color = y.title.font.color,
                                      y.lab.font.family = y.title.font.family,
                                      prefix = y.tick.prefix,                       # prefix of numbers in the tooltips and y axis
                                      suffix = y.tick.suffix,                       # suffix of numbers in the tooltips and y axis
                                      colors = unname(unlist(colors)),              # colors of the leaves
                                      digits = y.decimals,                          # number of decimal places in the tooltips
                                      order = "original")                           # not exposed since user can change by amending input data

    result <- list(htmlwidget = palm)
    class(result) <- "StandardChart"
    attr(result, "ChartType") <- "Radar"
    result
}



