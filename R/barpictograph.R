#' Create bar chart composed of icons
#'
#' @inherit Distribution
#' @inherit Column
#' @param image Name of icon to use.
#' @param custom.image URL of icon to use. If this parameter is supplied \code{image} is ignored.
#' @param fill.direction Direction in which icons are filled. One of \code{"From left", "From right", "From top", "From bottom"}.
#' @param data.label.position When \code{show.label.data}, the position of the data labels can be one of \code{"Above icons", "Below icons"} (all modes) or \code{"Next to bar", "Above row label", "Below row label"} (bar mode only). Note that the last two options will overrride \code{sublabel.left} and \code{sublabel.right}.
#' @param data.label.format A string representing a d3 formatting code.
#' @param categories.tick.show Logical; Whether to show tick labels for each bar
#' @param categories.tick.align.horizontal Horizontal alignment of tick labels for each bar. One of "left", "right", "middle".
#' @param base.image URL of image to use as base image. Only used if \code{is.custom.url = TRUE} and \code{hide.base.image = FALSE}.
#' @param hide.base.image Turns off background image (on by default). In general, the base image should only be shown if the input data is a proportion.
#' @param base.icon.color Color of base image, supplied as a hex code or string. This is only used if the built-in icons are used.
#' @param total.icons Maximum number of icons in each table cell. By default, it will be determine based on \code{ceiling(x)}.
#' @param scale Value of one icon. If \code{scale  =  0}, the value is automatically determined from the data so that the largest entry is represented by 10 icons.
#' @param fix.icon.nrow When \code{mode="bar" and hide.base.image=T}, set to \code{FALSE} to allow the bars to contain varying number of rows.
#' @param icon.ncol Configuration of icons in each table cell. Can be a single value or a vector with length equal to the number of columns.
#' @param label.color.asIcon When set to \code{TRUE}, row and data labels are shown in the same color as the icons.
#' @param pad.row Single numeric specifying vertical spacing between graphic cells in the table.
#' @param graphic.width.inch Horizontal dimension of the chart output in inches. If these dimensions are not specified, the width-to-height ratio of the chart output may not match the desired dimensions.
#' @param graphic.height.inch Verical dimension of the chart output in inches.
#' @importFrom flipPictographs PictographChart
#' @importFrom flipChartBasics ChartColors
#' @export
#' @examples
#' BarPictograph(1:5, image = "Sick person")
#' BarPictograph(1:5,
#'     custom.image = "https://upload.wikimedia.org/wikipedia/commons/a/a8/Nuvola_apps_kmoon.png")
BarPictograph <- function(x,
                       image = "Stickman",
                       custom.image = NULL,
                       base.image = "",
                       hide.base.image = FALSE,
                       base.icon.color = "",
                       scale = NA,
                       total.icons = NA,
                       icon.ncol = NA,
                       fix.icon.nrow = TRUE,
                       label.color.asIcon = FALSE,
                       pad.row = 2,
                       graphic.width.inch = NA,
                       graphic.height.inch = NA,
                       colors = ChartColors(length(x)),
                       fill.direction = "From left",
                       data.label.show = FALSE,
                       data.label.position = "No",
                       data.label.format = "",
                       data.label.prefix = "",
                       data.label.suffix = "",
                       data.label.font.family = global.font.family,
                       data.label.font.color = global.font.color,
                       data.label.font.size = 8,
                       global.font.family = "Arial",
                       global.font.color = rgb(44, 44, 44, maxColorValue = 255),
                       categories.tick.show = TRUE,
                       categories.tick.font.family = global.font.family,
                       categories.tick.font.color = global.font.color,
                       categories.tick.font.size = 12,
                       categories.tick.align.horizontal = "right")
{
    if (!is.vector(x))
        stop("Input data for Bar Pictographs must be a vector.")
    x <- checkMatrixNames(x)[,1]

    is.custom.url <- FALSE
    if (!is.null(custom.image))
    {
        is.custom.url <- TRUE
        image <- custom.image
    }

    # Currently importing from flipPictographs to get signature right
    # But eventually should directly call from rhtmlPictographs
    if (!categories.tick.show)
    {
        hide.label.right <- TRUE
        hide.label.left <- TRUE

    } else
    {
        hide.label.right <- fill.direction == "From left"
        hide.label.left <- fill.direction == "From right"
    }

    # Options: No, Above icons, Below icons, Next to bar, Above row label, Below row label
    show.label.data <- data.label.position != "No"
    if (show.label.data)
    {
        label.data.100prc <- FALSE
        if (percentFromD3(data.label.format))
        {
            data.label.suffix <- paste0("%", data.label.suffix)
            #label.data.100prc <- TRUE
        }
        label.data.digits <- decimalsFromD3(data.label.format)
    }

    return(PictographChart(x, mode = "bar", fill.direction = fill.direction,
        image = image, is.custom.url = is.custom.url,
        base.image = base.image, hide.base.image = hide.base.image,
        base.icon.color = base.icon.color, scale = scale,
        total.icons = total.icons, icon.ncol = icon.ncol, 
        fix.icon.nrow = fix.icon.nrow, label.color.asIcon = label.color.asIcon,
        icon.palette = "User-specified", icon.colors = colors,
        hide.label.right = hide.label.right, hide.label.left = hide.label.left,
        show.label.data = show.label.data, label.data.position = data.label.position,
        label.data.prefix = data.label.prefix, label.data.suffix = data.label.suffix,
        #label.data.100prc = label.data.100prc,
        label.data.digits = label.data.digits,
        label.data.font.family = data.label.font.family,
        label.data.font.color = data.label.font.color,
        label.data.font.size = data.label.font.size,
        label.font.family = global.font.family, label.font.color = global.font.color,
        label.left.font.family = categories.tick.font.family,
        label.left.font.color = categories.tick.font.color,
        label.left.font.size = categories.tick.font.size,
        label.left.align.horizontal = categories.tick.align.horizontal,
        label.right.font.family = categories.tick.font.family,
        label.right.font.color = categories.tick.font.color,
        label.right.font.size = categories.tick.font.size,
        label.right.align.horizontal = categories.tick.align.horizontal,
        pad.row = pad.row, graphic.width.inch = graphic.width.inch,
        graphic.height.inch = graphic.height.inch))
}