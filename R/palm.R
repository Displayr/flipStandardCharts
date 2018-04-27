#' Generates a palm tree plot from a table of data.
#'
#' This function wraps the PalmTrees function in the rhtmlPalmTrees package and performs additional data preparation.
#'
#' @param table A matrix of data to be displayed. Each row is represented by a palm tree, the value in each column
#' determines the area of a leaf on each palm tree.
#' @param x.tick.font.family Character; x-axis tick label font family
#' @param x.tick.font.size x-axis tick label font size
#' @param x.title Character; x-axis title
#' @param x.title.font.family Character; x-axis title font family
#' @param x.title.font.size x-axis title font size
#' @param legend.font.family Character; legend font family
#' @param legend.font.size Legend font size
#' @param y.axis.show Logical; whether to show the y-axis
#' @param tooltip.show Logical; whether to show a tooltip on hover.
#' @param y.tick.format A string representing a d3 formatting code.
#' See https://github.com/d3/d3/blob/master/API.md#number-formats-d3-format
#' @param y.tick.font.family Character; y-axis tick label font family
#' @param y.tick.font.size y-axis tick label font size
#' @param y.title Character, y-axis title
#' @param y.title.font.family Character; y-axis title font family
#' @param y.title.font.size y-axis title font size
#' @param y.tick.prefix y-axis tick label prefix
#' @param y.tick.suffix y-axis tick label suffix
#' @param colors Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' @examples
#' z <- matrix(runif(20), nrow = 4)
#' rownames(z) <- c("oak", "elm", "birch", "fir")
#' colnames(z) <- c("leaf", "frond", "twig", "branch", "stick")
#' Palm(z)
#' @importFrom rhtmlPalmTrees PalmTrees
#' @export

Palm <- function(table,
                 x.tick.font.family = "",
                 x.tick.font.size = 11,
                 x.title = "",
                 x.title.font.family = "",
                 x.title.font.size = 12,
                 legend.font.family = "",
                 legend.font.size = 11,
                 y.axis.show = TRUE,
                 tooltip.show = TRUE,
                 y.tick.format = "",
                 y.tick.font.family = "",
                 y.tick.font.size = 11,
                 y.title = "",
                 y.title.font.family = "",
                 y.title.font.size = 12,
                 y.tick.prefix = NULL,
                 y.tick.suffix = NULL,
                 colors = NULL) {


    stat <- attr(table, "statistic")
    #  Automatic formatting with statistic of '%'
    if (y.tick.format == "" && !is.null(stat) && grepl("%", stat, fixed = TRUE))
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
                                      weights = NULL,           # Numeric vector specifying weights. Length must equal to ncol(data)
                                      row.names = NULL,         # Default values are rownames(data)
                                      row.font.size = x.tick.font.size,
                                      row.font.family = x.tick.font.family,
                                      row.heading = x.title,
                                      row.heading.font.size = x.title.font.size,
                                      row.heading.font.family = x.title.font.family,
                                      col.names = NULL,         # Shown on legend. Default values are colnames(data)
                                      col.font.size = legend.font.size,            # Shown in legend
                                      col.font.family = legend.font.family,        # Shown in legend
                                      col.heading = "",                            # Legend heading, defaults to "Columns" if NULL
                                      col.heading.font.size = 0,
                                      col.heading.font.family = legend.font.family,
                                      tooltips = tooltip.show,
                                      tooltips.font.size = legend.font.size,
                                      tooltips.font.family = legend.font.family,
                                      tooltips.heading.font.size = x.title.font.size,
                                      tooltips.heading.font.family = legend.font.family,
                                      y.show = y.axis.show,
                                      y.digits = y.decimals,           # number of decimal places of the y axis
                                      y.font.size = y.tick.font.size,
                                      y.font.family = y.tick.font.family,
                                      y.lab = y.title,          # y axis title
                                      y.lab.font.size = y.title.font.size,
                                      y.lab.font.family = y.title.font.family,
                                      prefix = y.tick.prefix,                   # prefix of numbers in the tooltips and y axis
                                      suffix = y.tick.suffix,                   # suffix of numbers in the tooltips and y axis
                                      colors = colors,          # colors of the leaves
                                      digits = y.decimals,      # number of decimal places in the tooltips
                                      order = "original")       # not exposed since user can change by amending input data

    result <- list(htmlwidget = palm)
    class(result) <- "StandardChart"
    result
}



