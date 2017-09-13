#' Pie chart
#'
#' Plot pie or donut chart
#'
#' @param df Input data frame. The first column is expected to contain the label names, the second column contains numeric values and the third (optional) column are the group labels.
#' @param type One of "Pie" or "Donut"
#' @param colors A vector containing a hex value colors.
#' @param pie.subslice.colors A vector containing hex value colors for the outer ring of the pie chart. If not supplied will default to the same colors as the inner ring.
#' @param pie.subslice.colors.repeat Logical; if, when a grouped
#' pie chart is displayed, the colors of the subslices should repeat
#' by group, or be different throughout; defaults to TRUE.

#' @param title Character; chart title.
#' @param title.font.family Character; title font family. Can be "Arial Black",
#' "Arial", "Comic Sans MS", "Courier New", "Georgia", "Impact",
#' "Lucida Console", "Lucida Sans Unicode", "Marlett", "Symbol", "Tahoma",
#' "Times New Roman", "Trebuchet MS", "Verdana", "Webdings"
#' @param title.font.color Title font color as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param title.font.size Title font size; default = 10.
#' @param pie.values.font.family Character; value labels font family
#' @param pie.values.font.size value labels font size
#' @param pie.labels.font.color value labels font color as a named color in
#' character format (e.g. "black") or an rgb value (e.g.
#' rgb(0, 0, 0, maxColorValue = 255)).
#' @param pie.labels.font.family Character; font family for data label.
#' @param pie.labels.font.size Font size for data label.
#' @param pie.values.decimals Number of decimal places to show in
#' data labels.
#' @param pie.values.order Order of the labels shown. Can be one of 'descending', 'ascending' or 'initial'.
#' @param pie.groups.order Order of the groups shown. Can be one of 'descending', 'ascending' or 'initial'.
#' @param pie.groups.font.family Character; font family for group labels.
#' @param pie.groups.font.size Font size for group labels.
#' @param pie.groups.font.color Font color as a named color
#' @param pie.data.threshold Labels with values smaller than the theshold are not shown.
#' @param pie.values.prefix Character; prefix for data values.
#' @param pie.values.suffix Character; suffix for data values.
#' @param pie.border.color A single color for space around pie and between segments.
#' @param pie.inner.radius The size of the inner radius of pie and
#' donut charts as a proportion out of 100. defaults to 70.
#' @param pie.show.percentages Whether to show percentages in pie and donut
#' charts instead of original values.
#' @param global.font.family Character; font family for all occurrences of any
#' font attribute for the chart unless specified individually.
#' @param global.font.color Global font color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. #' rgb(0, 0, 0, maxColorValue = 255)).
#' @param ... Extra arguments that are ignored.
#' @examples
#' dat <- data.frame(labels=rep(LETTERS[24:26], each=3), abs(rnorm(9)),
#'      groups=rep(LETTERS[1:3], 3), stringsAsFactors = FALSE)
#' PieChart(dat)
#' PieChart(dat, pie.subslice.colors=rainbow(9), pie.subslice.colors.repeat = FALSE)
#' @importFrom flipChartBasics ChartColors StripAlphaChannel
#' @importFrom rhtmlDonut Donut
#' @export
PieChart <- function(df,
                     type = "Pie",
                     colors = NULL,
                     pie.subslice.colors = NULL,
                     pie.subslice.colors.repeat = TRUE,
                     title = "",
                     title.font.family = global.font.family,
                     title.font.size = 16,
                     title.font.color = global.font.color,
                     pie.values.font.family = global.font.family,
                     pie.values.font.size = 10,
                     pie.values.prefix = "",
                     pie.values.suffix = "",
                     pie.data.threshold = NULL,
                     pie.values.order = "initial",
                     pie.values.decimals = 2,
                     pie.labels.font.family = global.font.family,
                     pie.labels.font.size = 10,
                     pie.labels.font.color = global.font.color,
                     pie.groups.font.family = global.font.family,
                     pie.groups.font.size = 10,
                     pie.groups.font.color = 10,
                     pie.groups.order = "initial",
                     pie.inner.radius = 70,
                     pie.border.color = rgb(255, 255, 255, maxColorValue = 255),
                     pie.show.percentages = TRUE,
                     #table.statistic = "",  # this warning should be handled in AsBasicTables
                     global.font.family = "Arial",
                     global.font.color = rgb(44, 44, 44, maxColorValue = 255))
{
    # data is expected to already formatted as a dataframe
    df[,1] <- as.character(df[,1])
    ind.missing <- which(!is.finite(df[,2]) | df[,2] < 0)
    if (length(ind.missing) > 0)
    {
        # Fill with zeros so the coloring does not change
        warning("Missing and negative values have been omitted.")
        df[ind.missing,2] <- 0
    }
    groups <- NULL
    if (ncol(df) >= 3)
        groups <- as.character(df[,3])

    if (is.null(pie.data.threshold))
        pie.data.threshold <- 0.003

    # Check input data and parameters
    if (is.null(groups) && type == "Donut")
        stop("The table supplied is two-dimensional and cannot be displayed as a donut chart.  Please change the chart type to 'Pie' and update.")

    if (is.null(groups))
    {
        pie.values.colors <- colors
        pie.groups.colors <- NULL

        if (!is.null(pie.values.colors) && length(pie.values.colors) != nrow(df))
            warning("'Colors' does not have length equal to the number of slices (", nrow(df), ").")
    } else
    {
        pie.groups.colors <- if (!is.null(colors)) StripAlphaChannel(colors) else NULL
        pie.values.colors <- if (!is.null(pie.subslice.colors)) StripAlphaChannel(pie.subslice.colors) else NULL

        # We allow the number of groups to be 1
        # For dynamic data, one or more of the group categories may disappear occasionally
        num.groups <- length(unique(groups))
        if (!is.null(pie.groups.colors) && length(pie.groups.colors) != num.groups)
            warning("'Colors' does not have length equal to the number of groups (", num.groups, ").")

        num.values <- if (!pie.subslice.colors.repeat) nrow(df)
                      else                             length(unique(df[,1]))
        if (!is.null(pie.values.colors) && length(pie.values.colors) != num.values)
            warning("'Outer ring colors' should be a vector of colors of length ", num.values, ".")

        if (pie.subslice.colors.repeat)
        {
            v.list <- unique(df[,1])
            if (!is.null(pie.values.colors))
            {
                pie.values.color <- paste(rep("", length(v.list)), pie.values.colors)
                names(pie.values.colors) <- v.list
                pie.values.colors <- pie.values.colors[as.character(df[,1])]
            }
        }
    }

    if (type == "Pie" && is.null(groups))
        pie.inner.radius <- 0

    # Convert pie.inner.radius to character
    inner.radius <- paste(pie.inner.radius, "%", sep = "")
    values.display.as <- if (pie.show.percentages) "percentage" else "original"
    if (pie.show.percentages)
    {
        pie.values.prefix <- ""
        pie.values.suffix <- "%"
    }

    Donut(values = df[,2],
          labels = df[,1],
          values.color = pie.values.colors,
          values.order = pie.values.order,
          values.font.family = pie.values.font.family,
          values.font.size = pie.values.font.size,
          values.decimal.places = pie.values.decimals,
          values.display.as = values.display.as,
          values.display.thres = pie.data.threshold * 100,
          labels.font.family = pie.labels.font.family,
          labels.font.color = pie.labels.font.color,
          labels.font.size = pie.labels.font.size,
          labels.min.font.size = pie.labels.font.size,
          groups = groups,
          groups.color = pie.groups.colors,
          groups.order = pie.groups.order,
          groups.font.family = pie.groups.font.family,
          groups.font.color = pie.groups.font.color,
          groups.font.size = pie.groups.font.size,
          groups.min.font.size = pie.groups.font.size,
          title = title,
          title.font.family = title.font.family,
          title.font.size = title.font.size,
          title.font.color = title.font.color,
          prefix = pie.values.prefix,
          suffix = pie.values.suffix,
          border.color = pie.border.color,
          inner.radius = inner.radius)
}
