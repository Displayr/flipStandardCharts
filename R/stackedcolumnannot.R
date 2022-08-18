#' Stacked Column with annotations
#'
#' Stacked Column charts with annotations showing statistical
#'  significance of z-Statistics or Column Comparisons
#'
#' @inherit Column
#' @param x Input data may be a matrix or a vector, containing the height of the columns
#' to be plotted, with the name/rownames used as the column names of the chart. Numeric and date labels
#' will be parsed automatically.
#' @param type One of "Column", "Stacked Column" or "100\% Stacked Column"
#' @param font.unit One of "px" of "pt". By default all font sizes are
#'  specified in terms of pixels ("px"). But changing this to "pt" will
#'  mean that the font sizes will be in terms points ("pt"), which will
#'  be consistent with font sizes in text boxes.
#' @param num.categories.below.axis Number of categories shown below
#'  x-axis pointing downwards.
#' @param transpose Swaps the rows and columns of \code{x}. The rows
#'  and columns of the additional statistics used to create annotations
#'  are also respected. Note that PrepareData is not used before the function.
#' @param row.names.to.remove Names of rows that are not shown in the chart.
#' @param column.names.to.remove Names of columns that are not shown in the chart.
#' @param reverse.series.order Reverse the order in which the data series are shown
#'  (i.e. the order of the columns in \code{x}).
#' @param annot.footer.show Append a description explaining annotations in chart footer.
#' @param annot.arrow.size Size of the arrows in pixels.
#' @param annot.arrow.offset Horizontal offset (towards the right) of
#'  the arrows from the bars. If not specified, then it will be
#'  determined from \code{bar.gap}.
#' @param annot.legend.sep The string used between different entries describing
#'  the annotation symbols in the legend. By default it is a space as it works
#'  with line-wrap etc. But "<br>" can also be a good option for readibility.
#' @param annot.arrow.symbols A vector of three characters (or html symbols)
#'  used to show arrows. They are (in order):
#'  1. Up arrow for z-Statistics and upper-case Column Comparisons
#'  2. Down arrow for z-Statistics
#'  3. Up arrow for lower-case Column Comparisons
#' @param annot.arrow.colors A vector of colors for the arrows.
#'  For column comparisons, the number of colors should equal the number of columns.
#'  For z-Statistic, the colors are assigned to 'down' and 'up' arrows.
#' @param annot.sig.level Significance level above which arrows are shown
#'  next to the columns.
#' @param annot.differences.show Logical; whether to show the difference
#'  statistics if they are included in the input table.
#' @param annot.differences.decimals Number of decimals shown in the
#'  difference annotations.
#' @param append.annot.differences.to.datalabel Logical; Show difference statistics
#'  appended to the data labels (inside the bars).
#' @param annot.differences.prefix Option text to prepend to annot.differences.
#' @param annot.differences.suffix Option text to append to annot.differences.
#' @param annot.differences.sign.show Logical; whether to show the sign
#'  of the difference.
#' @param annot.differences.offset Horizontal offset (towards the right)
#'  of the difference annotation from the bars. If not specified, then
#'  it will be determined from \code{bar.gap}.
#' @param annot.differences.font.family Font family of the
#'  differences annotations
#' @param annot.differences.font.color Font color of the
#'  differences annotations
#' @param annot.differences.font.size Font size of the
#'  differences annotations
#' @param annot.hide.small.bar Hide annotations (arrow and differences)
#'  when the bar is smaller than the proportion of total range
#'  (the proportion as specified by \code{data.label.threshold}.
#' @param column.totals.above.show Show data labels containing the total of
#'  the categories above the x-axis.
#' @param column.totals.above.font.family Font family of \code{column.totals.above}.
#' @param column.totals.above.font.color Font color of \code{column.totals.above}.
#' @param column.totals.above.font.size Font size of \code{column.totals.above}.
#' @param column.totals.below.show Show data labels containing the total of
#'  the categories below the x-axis.
#' @param column.totals.below.font.family Font family of \code{column.totals.below}.
#' @param column.totals.below.font.color Font color of \code{column.totals.below}.
#' @param column.totals.below.font.size Font size of \code{column.totals.below}.
#' @param grid.show Logical; whether to show grid lines.
#' @param opacity Opacity of bars as an alpha value (0 to 1).
#' @param colors Character; a vector containing one or more colors specified
#'  as hex codes.
#' @param background.fill.color Background color in character format
#'  (e.g. "black") or a hex code.
#' @param background.fill.opacity Background opacity as an alpha value (0 to 1).
#' @param charting.area.fill.color Charting area background color as
#'  a named color in character format (e.g. "black") or a hex code.
#' @param charting.area.fill.opacity Charting area background opacity
#'  as an alpha value (0 to 1).
#' @importFrom grDevices rgb
#' @importFrom flipChartBasics ChartColors
#' @importFrom flipTables AsTidyTabularData RemoveRowsAndOrColumns ConvertQTableToArray
#' @importFrom plotly plot_ly config toRGB add_trace add_text layout hide_colorbar
#' @importFrom stats qnorm
#' @importFrom verbs SumEmptyHandling
#' @export
StackedColumnWithStatisticalSignificance <- function(x,
                    num.categories.below.axis = 0,
                    transpose = FALSE,
                    row.names.to.remove = "NET, SUM, Total",
                    column.names.to.remove = "NET, SUM, Total",
                    reverse.series.order = FALSE,
                    colors = ChartColors(max(1, NCOL(x), na.rm = TRUE)),
                    opacity = NULL,
                    type = "Stacked",
                    global.font.family = "Arial",
                    global.font.color = rgb(44, 44, 44, maxColorValue = 255),
                    font.unit = "px",
                    annot.footer.show = TRUE,
                    annot.arrow.size = 15,
                    annot.arrow.colors = ChartColors(9, "Strong colors"),
                    annot.arrow.offset = NULL,
                    annot.arrow.symbols = c("&#8593;", "&#8595;", "&#8673;"),
                    annot.sig.level = 0.05,
                    annot.legend.sep = " &#183; ",
                    append.annot.differences.to.datalabel = FALSE,
                    annot.differences.show = TRUE,
                    annot.differences.decimals = 0,
                    annot.differences.prefix = "",
                    annot.differences.suffix = "",
                    annot.differences.sign.show = TRUE,
                    annot.differences.font.family = global.font.family,
                    annot.differences.font.color = global.font.color,
                    annot.differences.font.size = 10,
                    annot.differences.offset = NULL,
                    annot.hide.small.bar = FALSE,
                    column.totals.above.show = FALSE,
                    column.totals.above.font.family = global.font.family,
                    column.totals.above.font.color = global.font.color,
                    column.totals.above.font.size = 10,
                    column.totals.below.show = FALSE,
                    column.totals.below.font.family = global.font.family,
                    column.totals.below.font.color = global.font.color,
                    column.totals.below.font.size = 10,
                    title = "",
                    title.font.family = global.font.family,
                    title.font.color = global.font.color,
                    title.font.size = 16,
                    title.align = "center",
                    subtitle = "",
                    subtitle.font.family = global.font.family,
                    subtitle.font.color = global.font.color,
                    subtitle.font.size = 12,
                    subtitle.align = "center",
                    footer = "",
                    footer.font.family = global.font.family,
                    footer.font.color = global.font.color,
                    footer.font.size = 8,
                    footer.wrap = TRUE,
                    footer.wrap.nchar = 150,
                    footer.align = "center",
                    background.fill.color = "transparent",
                    background.fill.opacity = 1,
                    charting.area.fill.color = background.fill.color,
                    charting.area.fill.opacity = 0,
                    legend.show = NA,
                    legend.orientation = 'Vertical',
                    legend.wrap = TRUE,
                    legend.wrap.nchar = 30,
                    legend.position.x = NULL,
                    legend.position.y = NULL,
                    legend.fill.color = background.fill.color,
                    legend.fill.opacity = 0,
                    legend.border.color = rgb(44, 44, 44, maxColorValue = 255),
                    legend.border.line.width = 0,
                    legend.font.color = global.font.color,
                    legend.font.family = global.font.family,
                    legend.font.size = 10,
                    legend.ascending = NA,
                    hovertext.font.family = global.font.family,
                    hovertext.font.size = 11,
                    margin.top = NULL,
                    margin.bottom = NULL,
                    margin.left = NULL,
                    margin.right = NULL,
                    margin.inner.pad = NULL,
                    margin.autoexpand = TRUE,
                    grid.show = TRUE,
                    y.title = "",
                    y.title.font.color = global.font.color,
                    y.title.font.family = global.font.family,
                    y.title.font.size = 12,
                    y.line.width = 0,
                    y.line.color = rgb(0, 0, 0, maxColorValue = 255),
                    y.tick.mark.length = 0,
                    y.tick.mark.color = "transparent",
                    y.bounds.minimum = NULL,
                    y.bounds.maximum = NULL,
                    y.tick.distance = NULL,
                    y.zero = TRUE,
                    y.zero.line.width = 3,
                    y.zero.line.color = rgb(0, 0, 0, maxColorValue = 255),
                    y.data.reversed = FALSE,
                    y.grid.width = 1 * grid.show,
                    y.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                    y.tick.show = TRUE,
                    y.tick.suffix = "",
                    y.tick.prefix = "",
                    y.tick.format = "",
                    y.hovertext.format = y.tick.format,
                    y.tick.angle = NULL,
                    y.tick.font.color = global.font.color,
                    y.tick.font.family = global.font.family,
                    y.tick.font.size = 10,
                    x.title = "",
                    x.title.font.color = global.font.color,
                    x.title.font.family = global.font.family,
                    x.title.font.size = 12,
                    x.line.width = 0,
                    x.line.color = rgb(0, 0, 0, maxColorValue = 255),
                    x.tick.marks = "",
                    x.tick.mark.length = 3,
                    x.tick.mark.color = "transparent",
                    x.bounds.minimum = NULL,
                    x.bounds.maximum = NULL,
                    x.tick.distance = NULL,
                    x.zero = FALSE,
                    x.zero.line.width = 0,
                    x.zero.line.color = rgb(225, 225, 225, maxColorValue = 255),
                    x.data.reversed = FALSE,
                    x.grid.width = 0 * grid.show,
                    x.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                    x.tick.show = TRUE,
                    x.tick.suffix = "",
                    x.tick.prefix = "",
                    x.tick.format = "",
                    x.hovertext.format = x.tick.format,
                    x.tick.angle = NULL,
                    x.tick.font.color = global.font.color,
                    x.tick.font.family = global.font.family,
                    x.tick.font.size = 10,
                    x.tick.label.wrap = TRUE,
                    x.tick.label.wrap.nchar = 21,
                    marker.border.width = 1,
                    marker.border.colors = colors,
                    marker.border.opacity = NULL,
                    tooltip.show = TRUE,
                    modebar.show = FALSE,
                    zoom.enable = TRUE,
                    axis.drag.enable = FALSE,
                    bar.gap = 0.5,
                    data.label.show = FALSE,
                    data.label.centered = TRUE,
                    data.label.font.autocolor = FALSE,
                    data.label.font.family = global.font.family,
                    data.label.font.size = 10,
                    data.label.font.color = global.font.color,
                    data.label.format = "",
                    data.label.prefix = "",
                    data.label.suffix = "",
                    data.label.threshold = NULL)
{
    ErrorIfNotEnoughData(x)
    x <- ConvertQTableToArray(x)
    if (length(dim(x)) < 3 ||
        !any(dimnames(x)[[3]] %in% c("z-Statistic", "Column Comparisons", "p")))
        warning("No annotations for statistical signficance are shown ",
        "as input tables do not contain additional cell statistics ",
        "such as 'Column Comparisons' or 'z-Statistic'.")
    colcmp.names <- colnames(x) # used for column comparisons

    if (isPercentData(x))
    {
        if (isAutoFormat(y.tick.format))
            y.tick.format <- paste0(y.tick.format, "%")
        if (isAutoFormat(y.hovertext.format))
            y.hovertext.format <- paste0(y.hovertext.format, "%")
        if (isAutoFormat(data.label.format))
            data.label.format <- paste0(data.label.format, "%")

        sfx <- checkSuffixForExtraPercent(c(y.tick.suffix, data.label.suffix),
            c(y.tick.format, data.label.format))
        y.tick.suffix <- sfx[1]
        data.label.suffix <- sfx[2]
    }

    # Save data for annotating column totals before
    # rows/columns are removed
    col.totals.annot.data <- NULL
    ind <- grep("NET|SUM|Total", dimnames(x)[[1L + !transpose]])
    if (length(ind) > 0 && column.totals.above.show) {
        ind <- ind[1L]
        col.totals.annot.data <- if (transpose) x[ind, , , drop = FALSE] else x[, ind, , drop = FALSE]
    }
    x <- RemoveRowsAndOrColumns(x,
            row.names.to.remove = row.names.to.remove,
            column.names.to.remove = column.names.to.remove)
    if (transpose)
    {
        x <- aperm(x, c(2,1,3))
        if (!is.null(col.totals.annot.data))
            col.totals.annot.data <- aperm(col.totals.annot.data, c(2,1,3))
    }
    # Some names might be missing or no names exist
    if (!is.null(col.totals.annot.data) && SumEmptyHandling(nchar(rownames(x))) > 0)
        col.totals.annot.data <- col.totals.annot.data[rownames(x)[!is.na(rownames(x))], , , drop = FALSE]

    if (bar.gap < 0.0 || bar.gap >= 1.0)
    {
        warning("Parameter 'bar gap' must be between 0 and 1. ",
                "Invalid 'bar gap' set to default value of 0.15.")
        bar.gap <- 0.15
    }
    if (reverse.series.order)
        x <- x[,NCOL(x):1,,drop = FALSE]


    # Store data for chart annotations
    annot.data <- x

    # Prepare data for plotting chart
    chart.matrix <- checkMatrixNames(x)
    if (!is.numeric(chart.matrix))
        stop("Input data should be numeric.")
    if (num.categories.below.axis > 0 &&
        any(!is.na(chart.matrix) & chart.matrix < 0))
        stop("All values in input data must be positive when some categories are shown below the axis")
    n <- nrow(chart.matrix)
    m <- ncol(chart.matrix)
    x.labels.full <- rownames(chart.matrix)

    if (num.categories.below.axis > 0)
    {
        if (num.categories.below.axis > ncol(chart.matrix))
        {
            warning("The number of categories shown below the axis has been set to the maximum number of categories(", ncol(chart.matrix), ")")
            num.categories.below.axis <- ncol(chart.matrix)
        }
        m.sign <- matrix(1, nrow = nrow(chart.matrix), ncol = ncol(chart.matrix))
        m.sign[,1:num.categories.below.axis] <- -1
        chart.matrix <- chart.matrix * m.sign
    }
    if (any(!is.finite(as.matrix(chart.matrix))))
        warning("Missing values have been set to zero.")

    # Some minimal data cleaning
    # Assume formatting and Qtable/attribute handling already done
    data.label.mult <- 1
    data.label.suffix.2 <- data.label.suffix
    if (percentFromD3(data.label.format))
    {
        data.label.mult <- 100
        data.label.suffix.2 <- paste0("%", data.label.suffix.2)
    }
    data.label.decimals <- decimalsFromD3(data.label.format)
    data.label.prefix <- vectorize(data.label.prefix, ncol(chart.matrix), nrow(chart.matrix), split = NULL)
    data.label.suffix <- vectorize(data.label.suffix, ncol(chart.matrix), nrow(chart.matrix), split = NULL)


    matrix.labels <- names(dimnames(chart.matrix))
    if (nchar(x.title) == 0 && length(matrix.labels) == 2)
        x.title <- matrix.labels[1]

    # Constants
    barmode <- "relative"
    if (is.null(opacity))
        opacity <- 1
    if (is.null(marker.border.opacity))
        marker.border.opacity <- opacity
    colors <- paste0(rep("", NROW(chart.matrix)), colors)

    if (data.label.font.autocolor)
        dlab.color <- autoFontColor(colors)
    else
        dlab.color <- vectorize(data.label.font.color, ncol(chart.matrix))
    data.label.show <- vectorize(data.label.show, NCOL(chart.matrix), NROW(chart.matrix))

    data.label.font = lapply(dlab.color,
        function(cc) list(family = data.label.font.family, size = data.label.font.size, color = cc))
    title.font = list(family = title.font.family, size = title.font.size, color = title.font.color)
    subtitle.font = list(family = subtitle.font.family, size = subtitle.font.size, color = subtitle.font.color)
    x.title.font = list(family = x.title.font.family, size = x.title.font.size, color = x.title.font.color)
    y.title.font = list(family = y.title.font.family, size = y.title.font.size, color = y.title.font.color)
    xtick.font = list(family = x.tick.font.family, size = x.tick.font.size, color = x.tick.font.color)
    ytick.font = list(family = y.tick.font.family, size = y.tick.font.size, color = y.tick.font.color)
    footer.font = list(family = footer.font.family, size = footer.font.size, color = footer.font.color)
    legend.font = list(family = legend.font.family, size = legend.font.size, color = legend.font.color)


    # Stacked charts cannot be toggled so annotations can be added
    # as a single trace, which will render faster than multiple traces
    # These annotations will be shown on the right of the bars
    annot.text <- NULL
    diff.annot.text <- NULL
    totals.annot.text <- NULL
    tmp.arrow.html <- NULL
    if (nchar(footer) > 0)
        footer <- paste0(footer, "<br>")
    if ("Column Comparisons" %in% dimnames(annot.data)[[3]])
    {
        ind.colcmp <- which(dimnames(annot.data)[[3]] == "Column Comparisons")
        n.colcmp <- length(colcmp.names)
        annot.arrow.colors <- rep(annot.arrow.colors, length = n.colcmp)
        tmp.arrow.html <- matrix(nrow = n.colcmp, ncol = 2,
            sprintf("<span style=\"color:%s; font-size:%.0fpx;\">%s</span>",
                rep(annot.arrow.colors, 2), annot.arrow.size,
                rep(annot.arrow.symbols[c(1,3)], each = n.colcmp)))
        annot.text <- getColCmpAnnot(annot.data[,,ind.colcmp], tmp.arrow.html)
        if (column.totals.above.show && !is.null(col.totals.annot.data))
             totals.annot.text <- getColCmpAnnot(
            col.totals.annot.data[,,ind.colcmp], tmp.arrow.html)

        if (annot.footer.show)
        {
            arrow.desc <- sprintf("Significantly %s %s %s",
                "greater than",
                rep(colcmp.names, 2),
                rep(c("at the 99.9% confidence level",
                "at the 95% confidence level"), each = n.colcmp))
            footer <- paste0(footer,
                paste(rmFontSize(tmp.arrow.html), arrow.desc, sep = "", collapse = annot.legend.sep))
        }

    }  else if (all(c("Differences", "p") %in% dimnames(annot.data)[[3]]))
    {
        tmp.arrow.html <- sprintf("<span style=\"color:%s; font-size:%.0fpx;\">%s</span>",
                annot.arrow.colors[1:2], annot.arrow.size,
                annot.arrow.symbols[1:2])
        annot.text <- getPDiffAnnot(annot.data, annot.sig.level, tmp.arrow.html)
        if (column.totals.above.show && !is.null(col.totals.annot.data))
            totals.annot.text <- getPDiffAnnot(
            col.totals.annot.data, annot.sig.level, tmp.arrow.html)
        if (annot.differences.show)
        {
            empty.arrow <- ""
            if (!append.annot.differences.to.datalabel)
                empty.arrow <- paste0("<span style='color:transparent; font-size:",
                    round(annot.arrow.size), "px;'>", annot.arrow.symbols[1], "</span>")
            diff.annot.labels <- paste0(annot.differences.prefix,
                formatC(annot.data[,,"Differences"],
                format = "f", digits = annot.differences.decimals,
                flag = if (annot.differences.sign.show) "+" else ""),
                annot.differences.suffix)
            diff.annot.text <- paste0(empty.arrow, diff.annot.labels)
            diff.annot.text <- matrix(diff.annot.text, NROW(chart.matrix), NCOL(chart.matrix))
            diff.annot.labels <- matrix(diff.annot.labels, NROW(chart.matrix), NCOL(chart.matrix))
        }
        if (annot.footer.show)
        {
            footer <- paste0(footer,
            paste(rmFontSize(tmp.arrow.html), sprintf("Significant %s at the %s%% confidence level",
            c("increase", "decrease"), round_half_up((1-annot.sig.level) * 100)),
            sep = "", collapse = annot.legend.sep))
        }
    } else if ("z-Statistic" %in% dimnames(annot.data)[[3]])
    {
        ind.zstat <- which(dimnames(annot.data)[[3]] == "z-Statistic")
        z.threshold <- qnorm(1 - (annot.sig.level/2))
        tmp.arrow.html <- sprintf("<span style=\"color:%s; font-size:%.0fpx;\">%s</span>",
                annot.arrow.colors[1:2], annot.arrow.size,
                annot.arrow.symbols[1:2])
        annot.text <- getZStatAnnot(annot.data[,,ind.zstat,drop = FALSE], z.threshold, tmp.arrow.html)
        if (column.totals.above.show && !is.null(col.totals.annot.data))
            totals.annot.text <- getZStatAnnot(
            col.totals.annot.data[,,ind.zstat], z.threshold, tmp.arrow.html)
        if (annot.footer.show)
        {
            footer <- paste0(footer,
            paste(rmFontSize(tmp.arrow.html), sprintf("Significant %s at the %s%% confidence level",
            c("increase", "decrease"), round_half_up((1-annot.sig.level) * 100)),
            sep = "", collapse = annot.legend.sep))
        }

    }

    legend.show <- setShowLegend(legend.show, NCOL(chart.matrix))
    legend <- setLegend(type, legend.font, legend.ascending, legend.fill.color, legend.fill.opacity,
                        legend.border.color, legend.border.line.width,
                        legend.position.x, legend.position.y, y.data.reversed,
                        legend.orientation)
    footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate = FALSE)


    # Format axis labels
    axisFormat <- formatLabels(chart.matrix, type, x.tick.label.wrap, x.tick.label.wrap.nchar,
                               x.tick.format, y.tick.format)

    x.range <- setValRange(x.bounds.minimum, x.bounds.maximum, axisFormat, x.zero, is.null(x.tick.distance), is.bar = TRUE)
    y.range <- setValRange(y.bounds.minimum, y.bounds.maximum, chart.matrix, y.zero, is.null(y.tick.distance))
    xtick <- setTicks(x.range$min, x.range$max, x.tick.distance, x.data.reversed, is.bar = TRUE)
    ytick <- setTicks(y.range$min, y.range$max, y.tick.distance, y.data.reversed)

    yaxis <- setAxis(y.title, "left", axisFormat, y.title.font,
                  y.line.color, y.line.width, y.grid.width * grid.show, y.grid.color,
                  ytick, ytick.font, y.tick.angle, y.tick.mark.length, y.tick.distance,
                  y.tick.format, y.tick.prefix, y.tick.suffix,
                  y.tick.show, y.zero, 0, y.zero.line.color,
                  y.hovertext.format, tickcolor = y.tick.mark.color,
                  zoom.enable = zoom.enable)
    xaxis <- setAxis(x.title, "bottom", axisFormat, x.title.font,
                  x.line.color, x.line.width, x.grid.width * grid.show, x.grid.color,
                  xtick, xtick.font, x.tick.angle, x.tick.mark.length, x.tick.distance,
                  x.tick.format, x.tick.prefix, x.tick.suffix, x.tick.show,
                  x.zero, x.zero.line.width, x.zero.line.color,
                  x.hovertext.format, axisFormat$labels,
                  num.series = NCOL(chart.matrix), tickcolor = x.tick.mark.color,
                  with.bars = TRUE, zoom.enable = zoom.enable)


    x.labels <- axisFormat$labels
    y.labels <- colnames(chart.matrix)
    x.all.labels <- x.labels
    x.range <- getRange(x.labels, xaxis, axisFormat)

    # Set up second x-axis for data labels
    # Even when data.label.show is false, data.annotations
    # is used to position arrow annotations etc
    xaxis2 <- list(overlaying = "x", visible = FALSE, range = x.range,
        fixedrange = !zoom.enable)
    data.annotations <- dataLabelPositions(chart.matrix = chart.matrix,
                        axis.type = xaxis$type,
                        annotations = NULL,
                        data.label.mult = data.label.mult,
                        bar.decimals = data.label.decimals,
                        bar.prefix = data.label.prefix,
                        bar.suffix = data.label.suffix.2,
                        barmode = barmode,
                        swap.axes.and.data = FALSE,
                        bar.gap = bar.gap,
                        display.threshold = data.label.threshold,
                        dates = axisFormat$ymd,
                        reversed = isReversed(yaxis),
                        font = data.label.font,
                        hide.sign = TRUE,
                        center.data.labels = data.label.centered)

    if (append.annot.differences.to.datalabel)
    {
        tmp.n <- NROW(data.annotations$text)
        tmp.m <- NCOL(data.annotations$text)
        ind.blank <- which(nchar(data.annotations$text) == 0)
        data.annotations$text <- matrix(paste0(data.annotations$text,
            "<span style='font-family:", annot.differences.font.family,
            "; font-size:", annot.differences.font.size, font.unit,
            "; color:", annot.differences.font.color, ";'>",
            diff.annot.text, "</span>"), nrow = tmp.n, ncol = tmp.m)
        data.annotations$text[ind.blank] <- ""
    }



    # Work out margin spacing
    margins <- list(t = 20, b = 20, r = if (!legend.show) 80 else 60, l = 80, pad = 0)
    margins <- setMarginsForAxis(margins, axisFormat, xaxis)
    margins <- setMarginsForText(margins, title, subtitle, footer, title.font.size,
                                 subtitle.font.size, footer.font.size)

    legend.text <- autoFormatLongLabels(colnames(chart.matrix), legend.wrap, legend.wrap.nchar)
    margins <- setMarginsForLegend(margins, legend.show, legend, legend.text, right.axis = FALSE)

    # Set default margin on the right to ensure there is space
    # for annotation arrows on the right of the bars
    # This default is also smaller than the space estimated by
    # to be needed for the legend
    margins$r <- 80
    margins <- setCustomMargins(margins, margin.top, margin.bottom, margin.left,
                    margin.right, margin.inner.pad)
    margins$autoexpand <- margin.autoexpand
    chart.labels <- list(SeriesLabels = list())


    # Add invisible line to force all categorical labels to be shown
    # Type "scatter" ensures y-axis tick bounds are treated properly
    # but it also adds extra space next to the y-axis
    p <- plot_ly(as.data.frame(chart.matrix))
    tmp.min <- if (any(is.finite(chart.matrix))) min(chart.matrix[is.finite(chart.matrix)])
               else y.bounds.minimum
    p <- add_trace(p, x = x.all.labels,
                   y = rep(tmp.min, length(x.all.labels)),
                   mode = if (notAutoRange(yaxis)) "markers" else "lines",
                   type = "scatter", cliponaxis = TRUE,
                   hoverinfo = "skip", showlegend = FALSE, opacity = 0)



    ## Add a trace for each col of data in the matrix
    for (i in 1:ncol(chart.matrix))
    {
        y <- as.numeric(chart.matrix[, i])
        y.filled <- ifelse(is.finite(y), y, 0)
        x <- x.labels
        marker <- list(color = toRGB(colors[i], alpha = opacity),
                      line = list(color = toRGB(marker.border.colors[i],
                      alpha = marker.border.opacity),
                      width = marker.border.width))

        tmp.y <- if (num.categories.below.axis) abs(y) else y
        tmp.y.str <- formatByD3(tmp.y, y.hovertext.format, y.tick.prefix, y.tick.suffix)
        tmp.x.str <- formatByD3(x, x.hovertext.format, x.tick.prefix, x.tick.suffix)
        if (xaxis$type  == "category")
            tmp.hover.text <- paste0(tmp.x.str, ": ", tmp.y.str)
        else
            tmp.hover.text <- paste0("(", tmp.x.str, ", ", tmp.y.str, ")")

        # This is the main trace for each data series
        p <- add_trace(p, x = x, y = y.filled, type = "bar",
                       orientation = "v", marker = marker, name = legend.text[i],
                       hoverlabel = list(font = list(color = autoFontColor(colors[i]),
                       size = hovertext.font.size, family = hovertext.font.family)),
                       hoverinfo = "text+name", text = tmp.hover.text, textposition = "none",
                       legendgroup = i)

        # Add attribute for PPT exporting
        # Note that even without data labels, overlay annotations can still be present
        chart.labels$SeriesLabels[[i]] <- list(Font = setFontForPPT(data.label.font[[i]]), ShowValue = FALSE)
        tmp.suffix <- if (percentFromD3(data.label.format)) sub("%", "", data.label.suffix[,i])
                      else                                               data.label.suffix[,i]

        pt.segs <- lapply(1:nrow(chart.matrix),
            function(ii)
            {
                pt <- list(Index = ii-1)
                if (data.label.show[ii,i])
                    pt$Segments <-  c(
                    if (nzchar(data.label.prefix[ii,i])) list(list(Text = data.label.prefix[ii,i])) else NULL,
                    list(list(Field="Value")),
                    if (nzchar(tmp.suffix[ii])) list(list(Text = tmp.suffix[ii])) else NULL)
                else
                    pt$ShowValue <- FALSE
                return(pt)
            }
        )

        # Plotly text marker positions are not spaced properly when placed to
        # the below the bar (i.e. negative values or reversed axis).
        # Adjusted by controlling the size of the marker
        # Hover must be included because this trace hides existing hover items
        # Always show data label trace - firstly to ensure stacking covers all series
        # and secondly because it is used for column totals
        if (TRUE)
        {
            # Apply annotations to data label
            ind.show <- which(data.label.show[,i])
            data.label.text <- data.annotations$text[,i]
            data.label.nchar <- nchar(data.label.text) # get length before adding html tags
            attr(data.label.text, "customPoints") <- pt.segs
            if (length(ind.show) > 0)
            {
                data.label.text[!data.label.show[,i]] <- ""
                data.label.text <- applyAllAnnotationsToDataLabels(data.label.text, NULL,
                    annot.data, i, ind.show, "Bar", clean.pt.segs = FALSE)
                pt.segs <- attr(data.label.text, "customPoints")
            } else
                data.label.text <- rep("", n)
            p <- addTraceForBarTypeDataLabelAnnotations(p, type = "Column", legend.text[i],
                    data.label.xpos = data.annotations$x[,i],
                    data.label.ypos = data.annotations$y[,i],
                    data.label.show = data.label.show[,i],
                    data.label.text = data.label.text,
                    data.label.sign = getSign(data.annotations$y[,i], yaxis), data.label.nchar,
                    NULL, annot.data, i,
                    xaxis = "x2", yaxis = "y",
                    data.label.font[[i]], TRUE, data.label.centered)
        }

        # Add difference annotations to chart label attributes
        if (!is.null(diff.annot.text))
        {
            if (annot.hide.small.bar)
                ind.diff.show <- which(nchar(data.annotations$text[,i]) > 0)
            else
                ind.diff.show <- 1:NROW(diff.annot.text)

            if (length(ind.diff.show) > 0)
                pt.segs <- getPointSegmentsForPPT(pt.segs, ind.diff.show,
                list(type = "Text - after data label", color = annot.differences.font.color,
                    size = annot.differences.font.size, font.family = annot.differences.font.family,
                    format = "Category"), diff.annot.labels[ind.diff.show, i])
        }

        # Add arrow annotations to chart label attributes
        for (tmp.arrow in tmp.arrow.html)
        {
            ind.tmp <- grep(tmp.arrow, annot.text[,i], fixed = TRUE)
            if (length(ind.tmp) > 0)
            {
                tmp.color <- regmatches(tmp.arrow, regexec("color:(\\S+);", tmp.arrow))[[1]][2]
                tmp.symbol <- regmatches(tmp.arrow, regexec(">(\\S+)</span>", tmp.arrow))[[1]][2]
                pt.segs <- getPointSegmentsForPPT(pt.segs, ind.tmp,
                    annot = list(type = "Custom text", format = "Category", size = annot.arrow.size,
                    font.family = "Arial", color = tmp.color, custom.symbol = tmp.symbol))
            }
        }


        # Clean up PPT chart labels
        pt.segs <- tidyPointSegments(pt.segs, nrow(chart.matrix))
        if (!is.null(pt.segs))
        {
            if (isTRUE(attr(pt.segs, "SeriesShowValue")))
            {
                chart.labels$SeriesLabels[[i]]$ShowValue <- TRUE
                attr(pt.segs, "SeriesShowValue") <- NULL
            }
            if (length(pt.segs) > 0)
                chart.labels$SeriesLabels[[i]]$CustomPoints <- pt.segs
        }
    }

    # Arrow Annotations to the right of the bar
    # These are shown even if no data labels are shown
    if (is.null(annot.arrow.offset) || !is.numeric(annot.arrow.offset))
        annot.arrow.offset <- (1 - bar.gap)/2
    xdiff <- annot.arrow.offset # in case the data only has one row
    if (nrow(data.annotations$x) >= 2)
        xdiff <- (data.annotations$x[2,1] - data.annotations$x[1,1]) * annot.arrow.offset
    if (annot.hide.small.bar)
    {
        ind <- which(nchar(data.annotations$text) == 0)
        if (length(ind) > 0)
            warning("Some significant values were not shown because the bars were too small.")
        annot.text[ind] <- ""
    }
    for (i in 1:ncol(chart.matrix))
        p <- addTraceForBarTypeDataLabelAnnotations(p, type = "Column", "Annotations",
                data.label.xpos = data.annotations$x[,i] + xdiff,
                data.label.ypos = data.annotations$y[,i],
                data.label.show = rep(TRUE, n),
                data.label.text = annot.text[,i],
                data.label.sign = getSign(data.annotations$y[,i], yaxis),
                0, NULL, annot.data, i,
                xaxis = "x2", yaxis = "y",
                data.label.font = data.label.font[[1]],
                TRUE, data.label.centered, "right", stackgroupname = "rightannot")

    if (!is.null(diff.annot.text) && !append.annot.differences.to.datalabel)
    {
        if (is.null(annot.differences.offset) ||
            !is.numeric(annot.differences.offset))
            annot.differences.offset <- (1 - bar.gap)/2
        xdiff <- annot.differences.offset
        if (nrow(data.annotations$x) >= 2)
            xdiff <- (data.annotations$x[2,1] - data.annotations$x[1,1]) *
            annot.differences.offset
        if (annot.hide.small.bar)
            diff.annot.text[which(nchar(data.annotations$text) == 0)] <- ""
        for (i in 1:ncol(chart.matrix))
            p <- addTraceForBarTypeDataLabelAnnotations(p, type = "Column", "Differences",
                data.label.xpos = data.annotations$x[,i] + xdiff,
                data.label.ypos = data.annotations$y[,i],
                data.label.show = rep(TRUE, n),
                data.label.text = diff.annot.text[,i],
                data.label.sign = getSign(data.annotations$y[,i], yaxis), 0,
                NULL, annot.data, i,
                xaxis = "x2", yaxis = "y",
                data.label.font = list(family = annot.differences.font.family,
                color = annot.differences.font.color, size = annot.differences.font.size),
                TRUE, data.label.centered, "right", stackgroupname = "differences")
    }

    # Column totals
    # These two sets of data labels are not added to ChartLabels because powerpoint has no way
    # of adding labels associated with the whole column
    if (column.totals.above.show)
    {
        # Add invisible string to center the column totals
        pre.annot <- gsub("color:.*?;", "color:transparent;", totals.annot.text)
        totals.above <- apply(chart.matrix, 1, function(xx) sum(xx[which(xx > 0)]))
        p <- addAnnotScatterTrace(p, orientation = "v", name = "Column totals - above",
                xpos = data.annotations$x[,1], ypos = rep(0, n),
                text = paste0(pre.annot,
                    formatByD3(totals.above,
                    data.label.format, data.label.prefix[,m], data.label.suffix[,m]),
                    totals.annot.text),
                textposition = "top center", marker = list(opacity = 0.0, size = 1),
                xaxis = "x2", yaxis = "y", hoverinfo = "skip",
                textfont = list(family = column.totals.above.font.family,
                color = column.totals.above.font.color,
                size = column.totals.above.font.size), stackgroup = "datalabel")
    }
    if (column.totals.below.show)
    {
        totals.below <- apply(chart.matrix, 1, function(xx) sum(xx[which(xx < 0)]))
        p <- addAnnotScatterTrace(p, name = "Column totals - below", orientation = "v",
                xpos = data.annotations$x[,1], ypos = rep(-.Machine$double.eps, n),
                text = formatByD3(abs(totals.below),
                    data.label.format, data.label.prefix[,m], data.label.suffix[,m]),
                textposition = "bottom center", marker = list(opacity = 0.0),
                xaxis = "x2", yaxis = "y", hoverinfo = "skip",
                textfont = list(family = column.totals.below.font.family,
                color = column.totals.below.font.color,
                size = column.totals.below.font.size), stackgroup = "datalabel")
    }


    # Add text elements surrounding chart
    annotations <- NULL
    n <- length(annotations)
    annotations[[n+1]] <- setTitle(title, title.font, margins, title.align)
    annotations[[n+2]] <- setFooter(footer, footer.font, margins, footer.align)
    annotations[[n+3]] <- setSubtitle(subtitle, subtitle.font, margins, subtitle.align)
    annotations <- Filter(Negate(is.null), annotations)

    serieslabels.num.changes <- vapply(chart.labels$SeriesLabels, function(s) isTRUE(s$ShowValue) + length(s$CustomPoints), numeric(1L))
    if (SumEmptyHandling(serieslabels.num.changes) == 0)
        chart.labels <- NULL

    shapes <- NULL
    if (isTRUE(y.zero)) # default plotly zero line is shown below bars
        shapes <- list(type = "line", layer = "above",
            yref = "y", y0 = 0, y1 = 0,
            xref = "paper", x0 = 0, x1 = 1,
            line = list(color = y.zero.line.color, width = y.zero.line.width))

    p <- config(p, displayModeBar = modebar.show, showAxisDragHandles = axis.drag.enable)
    p$sizingPolicy$browser$padding <- 0
    p <- layout(p,
        showlegend = legend.show,
        legend = legend,
        yaxis = yaxis,
        xaxis2 = xaxis2,
        xaxis = xaxis,
        margin = margins,
        annotations =  annotations,
        shapes = shapes,
        plot_bgcolor = toRGB(charting.area.fill.color, alpha = charting.area.fill.opacity),
        paper_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity),
        hoverlabel = list(namelength = -1, bordercolor = "transparent",
            font = list(size = hovertext.font.size, family = hovertext.font.family)),
        hovermode = if (tooltip.show) "x" else FALSE,
        bargap = bar.gap,
        barmode = barmode
    )
    attr(p, "can-run-in-root-dom") <- TRUE
    result <- list(htmlwidget = p)
    class(result) <- c("StandardChart", "visualization-selector")
    if (isPercentData(annot.data))
    {
        chart.matrix <- chart.matrix * 100
        attr(chart.matrix, "statistic") <- "%"
    }
    attr(result, "ChartData") <- chart.matrix
    attr(result, "ChartType") <- "Column Stacked"
    attr(result, "ChartLabels") <- chart.labels
    result
}

# z.data is matrix of z-statistics
# z.threshold is the threshold z-score use to determine significance
# arrow.html is a vector of two html strings: 1) up-arrow, 2) down-arrow
getZStatAnnot <- function(z.data, z.threshold, arrow.html)
{
    if (length(dim(z.data)) < 2)
        z.data <- as.matrix(z.data)
    n <- NROW(z.data)
    m <- NCOL(z.data)
    annot.txt <- matrix("", n, m)
    ind <- which(z.data > z.threshold)
    annot.txt[ind] <- arrow.html[1]
    ind <- which(z.data < -z.threshold)
    annot.txt[ind] <- arrow.html[2]
    return(annot.txt)
}

# annot.data is a 3d array containing p-values and Differences
# p.thres is the significance level of the test
# arrow.html is a vector of two html strings: 1) up-arrow, 2) down-arrow
getPDiffAnnot <- function(annot.data, p.thres, arrow.html)
{
    n <- nrow(annot.data)
    m <- ncol(annot.data)
    annot.txt <- matrix("", n, m)
    ind.p <- which(dimnames(annot.data)[[3]] == "p")
    ind.d <- which(dimnames(annot.data)[[3]] == "Differences")

    ind <- which(annot.data[,,ind.p,drop = FALSE] < p.thres, arr.ind = TRUE)
    if (length(ind) == 0 || nrow(ind) == 0)
        return(annot.txt)
    for (i in 1:nrow(ind))
    {
        if (annot.data[ind[i,1], ind[i,2], ind.d] < 0)
            annot.txt[ind[i,1], ind[i,2]] <- arrow.html[2]
        else
            annot.txt[ind[i,1], ind[i,2]] <- arrow.html[1]
    }
    return(annot.txt)
}

# colcmp is the 2d matrix that contains the letters describing column comparison
# arrow.html is a vector of
getColCmpAnnot <- function(colcmp.matrix, arrow.html)
{
    if (length(dim(colcmp.matrix)) < 2)
        colcmp.matrix <- as.matrix(colcmp.matrix)
    n <- NROW(colcmp.matrix)
    m <- NCOL(colcmp.matrix)
    annot.txt <- matrix("", n, m)

    for (i in 1:n)
    {
        for (j in 1:m)
        {
            if (is.na(colcmp.matrix[i,j]) || colcmp.matrix[i,j] == "" ||
                colcmp.matrix[i, j] == "-")
                next

            c.list <- unlist(strsplit(colcmp.matrix[i,j], split = " "))
            res <- ""
            for (cc in c.list)
            {
                if (cc == tolower(cc))
                {
                    ind <- which(letters == cc)
                    tmp <- arrow.html[ind,2]

                } else
                {
                    ind <- which(LETTERS == cc)
                    tmp <- arrow.html[ind,1]
                }
                res <- paste(res, tmp)
            }
            annot.txt[i,j] <- res
        }
    }
    return(annot.txt)
}

# This function removes the font-size attribute
# of the arrows in the footer text. This avoids the problem of arrows overlapping
# when there are multiple rows of text
rmFontSize <- function(x)
{
    return(gsub("font-size.*?;", "", x))
}
