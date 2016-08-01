#' Generates a vector of colors for the number of rows in the passed-in
#' df or matrix chart.matrix; where colors can be either a single named
#' color or a hex color, any code that generates a vector of named colors
#' or a vector of hex colors (but not a mixed vector), or a single named
#' color palette from either of the packages grDevices, RColorBrewer,
#' colorspace, or colorRamps.  If a single color is provided, and more are
#' needed, then a gradient will be calculated towards white. If more than
#' one but fewer than needed are provided, a gradient will be calculated
#' between the provided colors.
#'
#' Alpha channels are ignored and set to 255/FF.
#'
#' \code{ChartColors} generates a vector of colors.
#'
#' @param chart.matrix Any item with a positive non-zero integer returned by
#' the function nrow.
#' @param given.colors Character; a vector containing one or more named
#' colors from grDevices OR one or more specified hex value colors OR a single
#' named palette from grDevices, RColorBrewer, colorspace, or colorRamps.
#' @param reverse Logical; if the output color vector shour be reversed.
#' @examples
#' chart.matrix <- rbind(var1, var2, var3, var4, var5)
#' ChartColors(chart.matrix = chart.matrix, given.colors = c("blue", "orange",
#' "green"))
#' ChartColors(chart.matrix = chart.matrix, given.colors = "blue")
#' ChartColors(chart.matrix = chart.matrix, given.colors = "#9CFF73")
#' ChartColors(chart.matrix = chart.matrix, given.colors = "Set3", reverse = TRUE)
#' @export
ChartColors <- function(chart.matrix = NULL, given.colors = qColors, reverse = FALSE) {
    # Count the number of supplied colors
    number.colors <- length(given.colors)

    # Count the number of required colors
    number.series <- nrow(chart.matrix)

    # Can be a single named color, or a vector of named colors, e.g. "blue", or c("blue", "red", "green")
    all.colors <- grDevices::colors()

    # init some vars
    chart.colors <- character()
    color.type.named.R <- FALSE
    grcolor.palette <- FALSE
    brewer.palette <- FALSE
    ramp.palette <- FALSE
    space.palette <- FALSE
    hex.colors <- FALSE

    # Check that provided named colors are part of the native R colors
    valid.color.names <- sapply(given.colors, function(x) length(all.colors[all.colors == x]))

    if (sum(valid.color.names) == number.colors)
        color.type.named.R <- TRUE

    # Can be a single named gr palette
    all.grcolor.palettes <- c("rainbow", "heat.colors", "terrain.colors", "topo.colors", "cm.colors")

    if (number.colors == 1 && length(all.grcolor.palettes[all.grcolor.palettes == given.colors[1]]) == 1)
        grcolor.palette <- TRUE

    # Can be a single named RColorBrewer color/range
    all.brewer.palettes <- rownames(RColorBrewer::brewer.pal.info)

    if (number.colors == 1 && length(all.brewer.palettes[all.brewer.palettes == given.colors[1]]) == 1)
        brewer.palette <- TRUE

    # Can be a single specified colorRamp palette
    all.ramp.palettes <- c("ygobb", "primary.colors", "matlab.like2", "matlab.like", "magenta2green", "cyan2yellow", "blue2yellow", "green2red", "blue2green", "blue2red")

    if (number.colors == 1 && length(all.ramp.palettes[all.ramp.palettes == given.colors[1]]) == 1)
        ramp.palette <- TRUE

    # Can be a single specified colorSpace palette
    all.space.palettes <- c("diverge_hsv", "diverge_hcl", "terrain_hcl", "heat_hcl", "sequential_hcl", "rainbow_hcl")

    if (number.colors == 1 && length(all.space.palettes[all.space.palettes == given.colors[1]]) == 1)
        space.palette <- TRUE

    # Can be a single hex color, or a vector of hex colors, or anything that returns the same
    # which is useful if the users want to have their own colour brewer, colorRamp, or colorspace palette or the like
    if (grepl("#", given.colors[1], fixed = TRUE))
        hex.colors <- TRUE

    ## usage - for colorSpace, colorRamp, and grDevices colors, use the colname(n), where the n is the number of colors needed.
    if (grcolor.palette || ramp.palette || space.palette)
        chart.colors <- get(given.colors)(number.series)

    ## for RColorBrewer, work out the number of available colors in the palette, and if less than needed, then use
    ## colorRampPalette(brewer.pal(11,"Spectral"))(100)) where the 11 is the max number of items in the RCB palette, and the 100
    ## is the number of colors needed.
    if (brewer.palette)
    {
        max.brewer.colors <- RColorBrewer::brewer.pal.info[given.colors, 1]

        if (number.series <= max.brewer.colors)
            chart.colors <- RColorBrewer::brewer.pal(number.series, given.colors)
        else
            chart.colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(max.brewer.colors, given.colors))(number.series)
    }

    ## IF 1 color is specified, and more are needed, then that color is used as the starting point for a gradient,
    ## which gets progressively lighter.
    ## If more than 1 color is specified, then a grandient is created between the specified colors until we have
    ## enough to cover the number of series.
    ## If number of colors is the same as the series needed, then we just take the given colors.
    if (hex.colors || color.type.named.R)
    {
        if (number.colors == 1 && number.series > number.colors)
        {
            base.colors <- grDevices::col2rgb(given.colors[1])

            col.vector <- grDevices::rgb(base.colors[1], base.colors[2], base.colors[3], 255, maxColorValue = 255)

            for (i in 1:number.series + 1) {
                red.factor <- ((255 - base.colors[1]) / (number.series + 1)) * i
                green.factor <- ((255 - base.colors[2]) / (number.series + 1)) * i
                blue.factor <- ((255 - base.colors[3]) / (number.series + 1)) * i

                col.vector <- c(col.vector, grDevices::rgb(base.colors[1] + red.factor, base.colors[2] + green.factor, base.colors[3] + blue.factor, 255, maxColorValue = 255))
            }

            chart.colors <- col.vector[1:number.series]

        }
        else if (number.colors >= 2)
            chart.colors <- grDevices::colorRampPalette(given.colors)(number.series)
        else if (number.colors == number.series)
            chart.colors <- given.colors
    }

    if (reverse)
        chart.colors <- rev(chart.colors)

    return(chart.colors)
}
