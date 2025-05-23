#' Create bar chart composed of icons
#'
#' @inherit Distribution
#' @inherit Column
#' @param image Name of icon to use.
#' @param custom.image URL of icon to use. If this parameter is supplied \code{image} is ignored.
#' @param fill.direction Direction in which icons are filled. One of \code{"From left", "From right", "From top", "From bottom"}.
#' @param data.label.position When \code{show.label.data}, the position of the data labels can be one of \code{"Above icons", "Below icons"} (all modes) or \code{"Next to bar", "Above row label", "Below row label"} (bar mode only). Note that the last two options will overrride \code{sublabel.left} and \code{sublabel.right}.
#' @param data.label.format A string representing a d3 formatting code.
#' @param data.label.bigmark Option to prettify large numbers. By default a comma is placed after a thousand.
#' @param data.label.align.horizontal Horizontal alignment of data labels labels. One of "left", "right", "middle" or "Default".
#' @param categories.tick.show Logical; Whether to show tick labels for each bar (i.e. rownames).
#' @param categories.tick.align.horizontal Horizontal alignment of tick labels for each bar. One of "left", "right", "middle" or "Default".
#' @param categories.tick.align.vertical Vertical alignment of tick labels for each bar. One of "top", "center", "bottom" or "Default".
#' @param categories.tick.pad Horizontal space between the row labels and the icons.
#' @param base.image URL of image to use as base image. Only used if \code{custom.image} is supplied.
#' @param hide.base.image Turns off background image. In general, the base image should only be shown if the input data is a proportion.
#' @param base.icon.color Color of base image, supplied as a hex code or string. This is only used if the built-in icons are used.
#' @param total.icons Maximum number of icons in each table cell. By default, it will be determine based on \code{ceiling(x)}.
#' @param scale Value of one icon. If \code{scale  =  0}, the value is automatically determined from the data so that the largest entry is represented by 10 icons.
#' @param fix.icon.nrow When \code{mode="bar" and hide.base.image=T}, set to \code{FALSE} to allow the bars to contain varying number of rows.
#' @param icon.ncol Configuration of icons in each table cell. Can be a single value or a vector with length equal to the number of columns.
#' @param label.color.asIcon When set to \code{TRUE}, row and data labels are shown in the same color as the icons.
#' @param pad.row Single numeric specifying vertical spacing between graphic cells in the table.
#' @param pad.col Vertical spacing between cells in table.
#' @param pad.icon.row Numeric specifying vertical spacing between icons inside each table cell. May be a single value or a numeric matrix of the same dimensions as \code{x}.
#' @param pad.icon.col Horizontal spacing between icons inside each table cell.
#' @param maximum.number.icons Maximum allowed number of icons. Note that increasing this value may cause the browser to crash.
#' @param graphic.width.inch Horizontal dimension of the chart output in inches. If these dimensions are not specified, the width-to-height ratio of the chart output may not match the desired dimensions.
#' @param graphic.height.inch Verical dimension of the chart output in inches.
#' @param graphic.resolution Number of pixels per inch. Should not have an actual effect but rounding errors sometimes occur if this is chosen poorly.
#' @param print.config Print configuration string based to \code{rhtmlPictographs::graphic}. This is useful for debugging.
#' @importFrom rhtmlPictographs graphic
#' @importFrom flipChartBasics ChartColors
#' @importFrom httr GET
#' @importFrom verbs Sum
#' @importFrom flipU StopForUserError
#' @export
#' @examples
#' BarPictograph(1:5, image = "Sick person")
BarPictograph <- function(x,
                       image = "Stickman",
                       custom.image = NULL,
                       base.image = "",
                       hide.base.image = !any(nzchar(custom.image)),
                       base.icon.color = "",
                       scale = NA,
                       total.icons = NA,
                       icon.ncol = NA,
                       fix.icon.nrow = TRUE,
                       label.color.asIcon = FALSE,
                       colors = ChartColors(length(x)),
                       fill.direction = c("From left", "From right")[1],
                       global.font.family = "Arial",
                       global.font.color = rgb(44, 44, 44, maxColorValue = 255),
                       data.label.show = FALSE,
                       data.label.position = "No",
                       data.label.format = "",
                       data.label.prefix = "",
                       data.label.suffix = "",
                       data.label.bigmark = ",",
                       data.label.font.family = global.font.family,
                       data.label.font.color = global.font.color,
                       data.label.font.size = 8,
                       data.label.align.horizontal = "Default",
                       categories.tick.show = TRUE,
                       categories.tick.font.family = global.font.family,
                       categories.tick.font.color = global.font.color,
                       categories.tick.font.size = 12,
                       categories.tick.pad = 5,
                       categories.tick.align.horizontal = "Default",
                       categories.tick.align.vertical = "Default",
                       background.fill.color = "transparent",
                       pad.row = 2,
                       pad.col = 2,
                       pad.icon.row = 0.0,
                       pad.icon.col = 0.0,
                       graphic.width.inch = NA,
                       graphic.height.inch = NA,
                       graphic.resolution = 72,
                       maximum.number.icons = 10000,
                       print.config = FALSE)
{
    ErrorIfNotEnoughData(x)
    stat <- attr(x, "statistic")
    if (isPercentData(x))
    {
        if (isAutoFormat(data.label.format))
            data.label.format <- paste0(data.label.format, "%")
    }
    x <- checkMatrixNames(x)

    # Transpose if data is the wrong way around
    if (NROW(x) == 1 && NCOL(x) > 1)
        x <- t(x)
    if (NCOL(x) > 1)
    {
        warning("Only the first series will be shown.")
        x <- x[,1, drop = FALSE]
    }
    x <- x[,1, drop = FALSE]
    n <- NROW(x)
    if (n > 100)
        StopForUserError("Input data containing ", n, " rows is too large to show (maximum 100 rows).")

    # Set default values
    if (is.na(scale) && all(is.na(x)))
        scale <- 1
    else if (is.na(scale))
    {
        scale <- 10^(floor(log10(min(x[which(x != 0)]))))
        # ensure we don't get so many icons that we crash
        while (max(x, na.rm = TRUE)/scale > 100)
            scale <- scale * 10
    }
    if (is.na(total.icons))
    {
        if (all(is.na(x)))
            total.icons <- 1
        else
            total.icons <- ceiling(max(x, na.rm = TRUE)/scale)
    }
    raw.x <- x
    x <- x/scale

    # Icon layout
    if (any(nzchar(custom.image)) && !any(nzchar(base.image)))
        hide.base.image <- TRUE
    icon.nrow <- NA # only icon.ncol is used for bar charts
    if (!is.na(icon.ncol) && any(icon.ncol > total.icons))
        icon.ncol <- max(total.icons)
    if (length(icon.ncol) != 1 || !is.na(icon.ncol))
    {
        if (length(icon.ncol) != 1)
            icon.ncol <- NA
        else if (icon.ncol < 1)
        {
            warning("'Maximum icons per row' should be a positive integer. The current value is ignored.")
            icon.ncol <- NA
        }
        else if (icon.ncol != floor(icon.ncol))
            warning("'Maximum icons per row' should be an integer. The floor of the current value is used")
    }
    layout.str <- if (is.na(icon.ncol)) "\"numRows\": 1"
                  else paste("\"numCols\":", icon.ncol)

    icon.nrow <- rep(1, n)
    if (!fix.icon.nrow && hide.base.image && !is.na(icon.ncol))
    {
        icon.nrow <- pmax(ceiling(x/icon.ncol), 1)
        total.icons <- icon.nrow * icon.ncol

    } else if (!is.na(icon.ncol))
        icon.nrow <- rep(ceiling(total.icons/icon.ncol), n)
    if (is.na(icon.ncol))
        icon.ncol <- total.icons

    # 'prop' affects only what is plotted (not data-labels)
    # It should be computed after scaling, and changes total.icons
    prop <- as.vector(unlist(x))/unlist(total.icons)
    prop[total.icons == 0] <- 0
    if (any(is.na(prop)))
    {
        warning("Non-numeric values set to zero\n")
        prop[is.na(prop)] <- 0
    }
    if (any(prop < 0))
        StopForUserError("Input data cannot be negative\n")
    if (any(prop > 1))
        StopForUserError("Input data is too large. Try increasing the scale or total icons\n")

    # Check parameters
    if (!totalIconsAreIntegers(total.icons))
        StopForUserError("Total icons must be a whole number\n")
    if (!totalIconsArePositive(total.icons))
        StopForUserError("Total icons must be greater than zero\n")
    if (!is.na(scale) && scale <= 0)
        StopForUserError("Scale must be greater than zero\n")
    if (length(icon.nrow) != 1 && length(icon.nrow) != n)
        StopForUserError("icon.nrow should be a single integer or a vector of length ", n, "\n")
    if (length(icon.ncol) != 1)
        StopForUserError("icon.ncol should be a single integer or a vector of length ", 1, "\n")
    if (pad.icon.row < 0 || pad.icon.row >= 1)
        StopForUserError("pad.icon.row must be smaller than 1 and greater or equal to 0\n")
    if (pad.icon.col < 0 || pad.icon.col >= 1)
        StopForUserError("pad.icon.col must be smaller than 1 and greater or equal to 0\n")
    if (length(colors) > 1 && length(colors) != n)
        colors <- paste0(colors, rep("", n))[1:n]

    if (label.color.asIcon)
    {
        data.label.font.color <- colors
        categories.tick.font.color <- colors
    }
    if (categories.tick.align.vertical == "Default")
    {
        categories.tick.align.vertical <- if (icon.ncol == max(total.icons)) "center"
                                          else                               "top"
    }

    # Data labels
    # Options: No, Above icons, Below icons, Next to bar, Above row label, Below row label
    data.label.str <- ""
    floating.label.str <- ""
    if (data.label.position != "No")
    {
        data.label.mult100 <- FALSE
        if (data.label.format == "" && !is.null(stat) && grepl("%)?$", stat))
            data.label.format <- ".0%"
        if (percentFromD3(data.label.format))
        {
            data.label.suffix <- paste0("%", data.label.suffix)
            data.label.mult100 <- TRUE
        }
        data.label.digits <- decimalsFromD3(data.label.format)
        data.label.values <- unlist(raw.x) * (1 + (99 * data.label.mult100))
        data.label.text <- sprintf("%s%s%s", data.label.prefix,
            formatC(round_half_up(data.label.values, data.label.digits),
                digits = data.label.digits, format = "f",
                big.mark = data.label.bigmark), data.label.suffix)

        if (data.label.position %in% c("Above icons", "Below icons"))
        {
            if (data.label.align.horizontal == "Default")
                data.label.align.horizontal <- "right"
            data.label.pos <- if (data.label.position == "Above icons") "header"
                              else                                      "footer"
            data.label.str <- paste0("\"text-", data.label.pos,
                                     "\":{\"text\":\"", data.label.text,
                                     "\", \"font-size\":\"", data.label.font.size,
                                     "px\",\"font-family\":\"", data.label.font.family,
                                     "\", \"font-color\":\"", data.label.font.color,
                                     "\", \"font-weight\":\"normal",
                                     "\", \"horizontal-align\":\"",
                                     tolower(data.label.align.horizontal), "\"},")
        }
        if (data.label.position == "Next to bar")
        {
            x.tmp <- if (hide.base.image) x else rep(total.icons, length(x))
            x.tmp[!is.finite(x.tmp)] <- 0
            i.pos <- floor(x.tmp/icon.ncol)
            j.pos <- x.tmp %% icon.ncol
            ind.outside <- which(x.tmp >= icon.ncol)
            if (length(ind.outside) > 0)
            {
                i.pos[ind.outside] <- 0
                j.pos[ind.outside] <- icon.ncol
            }
            pad.dir <- ifelse(fill.direction == "From right", "padding-right", "padding-left")
            i.pos <- i.pos + 0.5
            j.pos <- j.pos

            if (data.label.align.horizontal == "Default")
                data.label.align.horizontal <- ifelse(fill.direction == "From right", "right", "left")
            floating.label.position <- sprintf("%.2f:%.2f", i.pos, j.pos)
            floating.label.str <- paste0("\"floatingLabels\":[{\"position\":\"",
                floating.label.position, "\", \"text\":\"", data.label.text,
                "\", \"font-size\":\"", data.label.font.size, "px\", \"",
                pad.dir, "\": \"4em\", \"font-family\":\"", data.label.font.family,
                "\", \"font-color\":\"", data.label.font.color, "\",\"",
                "horizontal-align\":\"", tolower(data.label.align.horizontal),
                "\", \"font-weight\":\"normal",
                "\", \"vertical-align\":\"", "center", "\"}],")
        }
    }

    # Row labels and data labels above and below row labels
    rowlabel.cells <- NULL
    if (categories.tick.show)
    {
        label.opp.pos <- if (fill.direction == "From right") "left"
                         else                                "right"
        if (categories.tick.align.horizontal == "Default")
            categories.tick.align.horizontal <- label.opp.pos

        row.labels <- if (is.null(rownames(x))) names(x) else rownames(x)
        label.str <- paste0("\"text\": \"", cleanPictographLabels(row.labels),
            "\" ,\"horizontal-align\": \"", tolower(categories.tick.align.horizontal),
            "\" ,\"font-weight\":\"normal",
            "\" ,\"font-family\": \"", categories.tick.font.family,
            "\" ,\"font-color\": \"", categories.tick.font.color,
            "\" ,\"font-size\": \"", categories.tick.font.size, "px\"")
        if (data.label.position %in% c("Above row label", "Below row label"))
        {
            if (data.label.align.horizontal == "Default")
                data.label.align.horizontal <- categories.tick.align.horizontal
            sublabel.str <- paste0("\"text\": \"", data.label.text,
                "\" ,\"horizontal-align\": \"", tolower(data.label.align.horizontal),
                "\" ,\"font-weight\":\"normal",
                "\" ,\"font-family\": \"", data.label.font.family,
                "\" ,\"font-color\": \"", data.label.font.color,
                "\" ,\"font-size\": \"", data.label.font.size, "px\"")
            if (data.label.position == "Below row label")
               label.str <- paste0("\"labels\": [{", label.str, "},{", sublabel.str, "}]")
            if (data.label.position == "Above row label")
               label.str <- paste0("\"labels\": [{", sublabel.str, "},{", label.str, "}]")
        }
        rowlabel.cells <- paste0("{\"type\":\"label\", \"value\":{", label.str,
            ", \"vertical-align\":\"", categories.tick.align.vertical,
            "\", \"padding-", label.opp.pos, "\":", categories.tick.pad, "}}")
    }

    # Icons and color
    image.type <- if (image %in% c("circle", "square")) image else "url"
    if (!is.null(custom.image))
    {
        image.url <- custom.image

        # Some tests in case image.url is invalid (rhtmlPictograph gives no warning)
        response <- try(GET(image.url), silent = TRUE)
        if (inherits(response, "try-error"))
            StopForUserError("Could not retrieve image from '", image.url, "'. Check that url is correct.")
        if(response$status_code != 200)
            StopForUserError("Error (status code ", response$status_code, ") retrieving image ", image.url)
        tmp.type <- response$headers$'content-type'
        if (any(grepl("text/html", tmp.type, fixed = TRUE)))
            StopForUserError("The url content type is 'text/html'. Ensure the image url is correct and not redirected.")
        # Give warning because sometimes chrome can fix this, but will show as blank in IE
        unknown.type <- !any(grepl("image", tmp.type, fixed = TRUE))
        if (unknown.type)
            warning("URL content type is '", tmp.type,
            "'. This may not display properly in all browsers.")
    }
    else
        image.url <- sprintf("https://displayrcors.displayr.com/images/%s_grey.svg",
                              gsub(" ", "", tolower(image)))

    if (!hide.base.image && is.null(custom.image))
        base.image <- image.url
    base.image.str <- ""
    if (any(nzchar(base.image)))
    {
        base.icon.color.str <- ifelse(nchar(base.icon.color) > 0, paste0(base.icon.color, ":"), "")
        base.image.str <- ifelse(nchar(base.image) > 0, paste("\"baseImage\":\"", image.type, ":", base.icon.color.str, base.image, "\",", sep = ""), "")
    }

    if (!is.null(custom.image))
        colors <- ""
    fill.icon.color.str <- ifelse(nchar(colors) > 0, paste0(colors, ":"), "")
    fill.direction <- gsub(" ", "", tolower(fill.direction))

    # Exact dimensions should not matter as long as aspect ratio is correct
    # But rounding errors can happen if graphic.resolution is not chosen well
    dim.str <- ""
    row.height <- paste0("\"proportion:", floor(icon.nrow/Sum(icon.nrow, remove.missing = FALSE)*1000)/1000, "\"")
    column.width <- "\"flexible:graphic\""

    # Setting up graphic cells (bars of icons)
    json.cells <- sprintf(paste0("{\"type\":\"graphic\", \"value\":{\"proportion\":%f,\"numImages\":%d, ",
                          "\"variableImage\":\"%s:%s:%s%s\", %s %s, %s %s",
                          "\"columnGutter\":%f, \"rowGutter\":%f, \"padding\":\"%f %f %f %f\"}}"),
                          prop, total.icons, image.type, fill.direction, fill.icon.color.str,
                          image.url, base.image.str, layout.str, data.label.str,
                          floating.label.str, pad.icon.col, pad.icon.row, 0, 0, 0, 0)
    json.cells <- matrix(json.cells, ncol = 1)
    if (categories.tick.show && !grepl("right", fill.direction))
    {
        column.width <- c("\"flexible:label\"", column.width)
        json.cells <- cbind(rowlabel.cells, json.cells)
    }
    if (categories.tick.show && grepl("right", fill.direction))
    {
        json.cells <- cbind(json.cells, rowlabel.cells)
        column.width <- cbind(column.width, "\"flexible:label\"")
    }
    if (!is.na(graphic.width.inch) && !is.na(graphic.height.inch))
        dim.str <- paste0("\"width\":", graphic.width.inch * graphic.resolution,
                          ", \"height\":", graphic.height.inch * graphic.resolution, ",")

    # Sticking everything together
    json.cells <- apply(json.cells, 1, paste, collapse = ",")
    json.str <- paste("{", dim.str,
             "\"background-color\":\"", background.fill.color, "\",",
             "\"table\":{\"rowHeights\":[", paste(row.height, collapse = ","), "],",
             "\"rowGutterLength\":", pad.row, ",\"columnGutterLength\":", pad.col, ",",
             "\"colWidths\":[", paste(column.width, collapse = ","), "],",
             sep = "")
    json.str <- paste(json.str, "\"rows\":[[", sep = "")
    json.str <- paste(json.str, paste(json.cells, collapse = "],["), sep = "")
    json.str <- paste(json.str, "]]}}", sep = "")

    if (print.config)
        cat(json.str, "\n")

    # Check that the number of icons is not too big
    if (hide.base.image)
        num.icons <- Sum(prop * total.icons, remove.missing = FALSE)
    else
        num.icons <- length(prop) * total.icons
    if (num.icons > maximum.number.icons)
        StopForUserError("Cannot create a chart with ", num.icons,
                         " icons (maximum allowed is ", maximum.number.icons,
                         "). Try increasing the 'scale' parameter.")

    result <- list(htmlwidget = graphic(json.str))
    class(result) <- "StandardChart"
    attr(result, "ChartType") <- "Bar Clustered"
    result

}

cleanPictographLabels <- function(x)
{
    # New line characters were causing errors in the JSON
    # Note these can be coded as \n or \r
    x <- gsub("\\s", " ", x)

    # These characters used to be shown as text but that is
    # probably not what the user wants to see
    x <- gsub("<br>", "\\\\n", x)
    x <- gsub("&nbsp;", " ", x)
    x <- gsub('"', '\\"', x, fixed = TRUE)
    return(x)
}

totalIconsAreIntegers <- function(total.icons) {
    all(vapply(total.icons,
               FUN = function(x) { is.na(x) || x == ceiling(x)},
               FUN.VALUE = logical(1)))
}
totalIconsArePositive <- function (total.icons) {
    all(vapply(total.icons,
               FUN = function(x) { is.na(x) || x > 0},
               FUN.VALUE = logical(1)))
}
