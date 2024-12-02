#' Pie
#'
#' Pie chart
#'
#' @inherit Column
#' @param x Input data in the form of a vector, matrix or dataframe. If a dataframe is given, the first column is expected to contain the label names, the second column contains numeric values and the third (optional) column are the group labels.
#' @param type One of "Pie" or "Donut"
#' @param pie.subslice.colors A vector containing hex value colors for the outer ring of the pie chart. If not supplied will default to the same colors as the inner ring.
#' @param pie.subslice.colors.repeat Logical; if, when a grouped
#' pie chart is displayed, the colors of the subslices should repeat
#' by group, or be different throughout; defaults to TRUE.
#' @param pie.values.order Order of the labels shown. Can be one of 'descending', 'alphabetical' or 'initial'.
#' @param pie.groups.order Order of the groups shown. Can be one of 'descending', 'alphabetical' or 'initial'.
#' @param pie.groups.font.family Character; font family for group labels.
#' @param pie.groups.font.size Font size for group labels.
#' @param pie.groups.font.color Font color as a named color
#' @param pie.data.threshold Labels with values smaller than the theshold are not shown.
#' @param data.label.prefix Character; prefix for data values.
#' @param data.label.suffix Character; suffix for data values.
#' @param hovertext.bg.color Color of the hover text background. If \code{NULL},
#'   the background will be the same color as the segment.
#' @param hovertext.bg.opacity Opacity of the hover text background.
#' @param hovertext.font.color Font color of hover text. If \code{NULL}, this will be
#'   black or white depending on the color of the hover text background.
#' @param pie.border.color A single color for space around pie and between segments.
#' @param pie.inner.radius The size of the inner radius of pie and
#' donut charts as a proportion out of 100. Defaults to 70.
#' @examples
#' dat <- data.frame(labels=rep(LETTERS[24:26], each=3), vals=abs(rnorm(9)),
#'      groups=rep(LETTERS[1:3], 3), stringsAsFactors = FALSE)
#' Pie(dat)
#' Pie(dat, pie.subslice.colors=rainbow(9), pie.subslice.colors.repeat = FALSE)
#' @importFrom flipChartBasics ChartColors
#' @importFrom rhtmlDonut Donut
#' @export
Pie <- function(x,
                     type = "Pie",
                     colors = NULL,
                     pie.subslice.colors = NULL,
                     pie.subslice.colors.repeat = TRUE,
                     global.font.family = "Arial",
                     global.font.color = rgb(44, 44, 44, maxColorValue = 255),
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
                     hovertext.font.family = global.font.family,
                     hovertext.font.size = 10,
                     hovertext.font.color = NULL,
                     hovertext.bg.color = NULL,
                     hovertext.bg.opacity = 0.8,
                     pie.groups.font.size = data.label.font.size,
                     pie.groups.font.color = data.label.font.color,
                     pie.groups.font.family = data.label.font.family,
                     pie.groups.order = "initial",
                     pie.inner.radius = NULL,
                     pie.border.color = rgb(255, 255, 255, maxColorValue = 255))
{
    ErrorIfNotEnoughData(x)
    if (isPercentData(x) && isAutoFormat(data.label.format))
        data.label.format <- paste0(data.label.format, "%")
    groups <- NULL

    if (is.null(dim(x)) || length(dim(x)) == 1 || is.data.frame(x) && nrow(x) == 1)
    {
        x.labels <- names(x)
        if (is.null(x.labels))
            x.labels <- as.character(1:length(x))
        y.values <- as.numeric(x)
        if (isTRUE(grepl("%", attr(x, "statistic"))))
            y.values <- y.values/100
    }
    else if (is.array(x) || is.matrix(x) && is.numeric(x) || all(unlist(lapply(x, is.numeric))))
    {
        x <- checkMatrixNames(x)
        x.labels <- rep(rownames(x), ncol(x))
        if (!is.numeric(x))
            warning("Non-numeric values coerced to numeric")
        y.values <- as.numeric(x)
        groups <- rep(colnames(x), each = nrow(x))
    }
    else
    {
        # dataframe (with group data)
        if (!is.null(ncol(x)) && ncol(x) >= 3)
            x <- x[order(x[,3]),]   # must be ordered by group
        x.labels <- as.character(x[,1])
        if (!is.numeric(x[,2]))
            warning("Non-numeric values coerced to numeric")
        y.values <- as.numeric(x[,2])
        if (!is.null(ncol(x)) && ncol(x) >= 3)
            groups <- as.character(x[,3])
    }

    if (is.null(pie.data.threshold))
        pie.data.threshold <- 0.003

    # Check input data and parameters
    if (length(unique(groups)) > 1 && type == "Donut")
        stop("The table supplied is two-dimensional and cannot be displayed as a donut chart.  Please change the chart type to 'Pie' and update.")

    # Give warning about missing values
    ind.missing <- which(!is.finite(y.values) | y.values <= 0)
    if (length(ind.missing) > 0)
    {

        missing.msg <- "Missing and non-positive values have been omitted."
        if (is.null(groups))
            missing.msg <- paste(missing.msg, "The color palette may not be shown in the way expected.",
                "To remove values before assigning colors use 'Inputs > ROW MANIPULATIONS > Hide empty rows'.")
        else if (length(dim(x)) == 2 && (any(apply(x, 1, function(u) all(is.na(u)))) ||
            any(apply(x, 2, function(u) all(is.na(u))))))
            missing.msg <- paste(missing.msg, "The color palette may not be shown in the way expected.",
                "To remove values before assigning colors use 'Inputs > ROW/COLUMN MANIPULATIONS > Hide empty rows/columns'.")
        warning(missing.msg)
        y.values[ind.missing] <- 0 # Needed for missing values in 2d tables
    }

    if (is.null(groups)) # 1-d data
    {
        pie.values.colors <- colors
        pie.groups.colors <- NULL

        if (length(pie.values.colors) > 1 && length(pie.values.colors) < length(x.labels))
        {
            warning("'Colors' does have the number of colors required (",
            length(x.labels), "). Colors will be recycled to make up the required number.")
            pie.values.colors <- paste0(rep("", length(x.labels)), pie.values.colors)
        }
    } else              # 2-d data
    {
        pie.groups.colors <- if (!is.null(colors)) colors else NULL
        pie.values.colors <- if (!is.null(pie.subslice.colors)) pie.subslice.colors else NULL
        x.labels[which(nchar(x.labels)==0)] <- " "

        # We allow the number of groups to be 1
        # For dynamic data, one or more of the group categories may disappear occasionally
        num.groups <- length(unique(groups))
        if (length(pie.groups.colors) > 1 && length(pie.groups.colors) < num.groups)
            warning("'Colors' does not have length equal to the number of groups (", num.groups, "). Colors will be recycled to make up the required number.")

        num.values <- if (!pie.subslice.colors.repeat) max(tapply(x.labels, groups, length))
                      else                             length(unique(x.labels))
        if (length(pie.values.colors) > 1 && length(pie.values.colors) < num.values)
            warning("'Outer ring colors' should be a vector of colors of length ", num.values, ".")

        # Ensure that subslices with the same names have the same color
        if (pie.subslice.colors.repeat)
        {
            v.list <- unique(x.labels)
            if (!is.null(pie.values.colors))
            {
                pie.values.colors <- paste0(rep("", length(v.list)), pie.values.colors)
                names(pie.values.colors) <- v.list
                pie.values.colors <- pie.values.colors[x.labels]
                pie.values.colors
            }
        }
    }

    # Set default radius for 1d data
    if (type == "Pie" && is.null(groups))
        pie.inner.radius <- 0

    # Set default radius in other cases
    if (is.null(pie.inner.radius))
    {
        if (type == "Pie" && is.null(groups))
            pie.inner.radius <- 0
        else
            pie.inner.radius <- 70
    }

    if (pie.inner.radius < 0 || pie.inner.radius > 100)
        stop("Pie radius should be a number between 0 and 100.")

    # Data label formatting extract from d3 format
    as.percentages <- percentFromD3(data.label.format)
    data.label.decimals <- decimalsFromD3(data.label.format)

    # Convert pie.inner.radius to character
    inner.radius <- pie.inner.radius / 100
    if (as.percentages)
    {
        data.label.suffix <- paste0("%", data.label.suffix)
        y.values <- y.values * 100
    }

    if (length(pie.values.colors) > 0)
        pie.values.colors <- rep(pie.values.colors, length = length(y.values))
    donut <- CollectWarnings(rhtmlDonut::Donut(values = y.values,
                  labels = x.labels,
                  values.color = pie.values.colors,
                  values.order = pie.values.order,
                  values.font.family = data.label.font.family,
                  values.font.size = data.label.font.size,
                  values.decimal.places = data.label.decimals,
                  values.display.as = "original",
                  values.display.thres = pie.data.threshold,
                  labels.font.family = data.label.font.family,
                  labels.font.color = data.label.font.color,
                  labels.font.size = data.label.font.size,
                  groups = groups,
                  groups.color = pie.groups.colors,
                  groups.order = pie.groups.order,
                  groups.font.family = pie.groups.font.family,
                  groups.font.color = pie.groups.font.color,
                  groups.font.size = pie.groups.font.size,
                  title = title,
                  title.font.family = title.font.family,
                  title.font.size = title.font.size,
                  title.font.color = title.font.color,
                  subtitle = subtitle,
                  subtitle.font.family = subtitle.font.family,
                  subtitle.font.size = subtitle.font.size,
                  subtitle.font.color = subtitle.font.color,
                  footer = autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate=FALSE),
                  footer.font.family = footer.font.family,
                  footer.font.size = footer.font.size,
                  footer.font.color = footer.font.color,
                  tooltips.font.family = hovertext.font.family,
                  tooltips.font.size = hovertext.font.size,
                  tooltips.font.color = hovertext.font.color,
                  tooltips.bg.color = hovertext.bg.color,
                  tooltips.bg.opacity = hovertext.bg.opacity,
                  prefix = data.label.prefix,
                  suffix = data.label.suffix,
                  border.color = StripAlphaChannel(pie.border.color, "Alpha values for pie chart borders are ignored"),
                  inner.radius = inner.radius))

    result <- list(htmlwidget = donut)
    class(result) <- "StandardChart"
    if (is.null(groups))
        attr(result, "ChartType") <- if (type == "Pie") "Pie" else "Doughnut"
    else
        attr(result, "ChartType") <- "Sunburst"

    attr(result, "ChartLabels") <- list(SeriesLabels=list(list(ShowValue = TRUE, Separator = ": ")))
    if (nzchar(data.label.prefix) || nzchar(data.label.suffix))
    {
        tmp.suffix <- if (percentFromD3(data.label.format)) sub("%", "", data.label.suffix)
                      else                                               data.label.suffix
        pt.segs <- lapply((1:NROW(x)),
            function(ii) list(Index = ii-1, Segments = c(
                list(list(Field = "CategoryName")),
                list(list(Text = paste0(": ", unescape_html(data.label.prefix)))),
                list(list(Field = "Value")),
                if (nzchar(tmp.suffix)) list(list(Text = unescape_html(tmp.suffix))) else NULL)))
        attr(result, "ChartLabels")$SeriesLabels[[1]]$CustomPoints <- pt.segs
    }
    result
}
