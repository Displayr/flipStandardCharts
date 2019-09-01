#' Plotting a chart with a table
#' @description Display a chart with a table below it.
#' @param x Input data for chart as a matrix or dataframe.
#' @param table Input data for table shown below chart.
#' @param chart.type Can be one of "Area", "Column", "Bar", "Line".
#' @param chart.height The height of the chart (not including axis)
#'    as a proportion of the total display area.
#' @param table.height The height of the table as a proportion of the total display area.
#' @param table.column.widths A vector or comma-separated string specifying
#'    the relative widths of columns. If a row-header is shown, this vector should
#'    contain one value for the width of the rowheader and then one value for
#'    each column in the table.#'
#' @param table.values.format A string representing a d3 formatting code.
#'    See https://github.com/d3/d3/blob/master/API.md#number-formats-d3-format
#' @param table.values.fill Background color of the cells in the table.
#' @param table.values.border.width Width of border around table cells (in pixels).
#' @param table.values.border.color Color of border around table cells,
#' @param table.values.align.horizontal Horizontal alignment of text in table cells.
#' @param table.values.font.family Font family of text in table cells.
#' @param table.values.font.color Font color of text in table cells.
#' @param table.values.font.size Font size (in pixels) of text in table cells.
#' @param table.values.font.bold Logical; whether text in table cells should be bold.
#' @param table.values.font.italics Logical; whether text in table cells should be italicized
#' @param table.colhead.show Logical; whether to show column headers in the table.
#' This will be ignored if \code{table} does not contain column names.
#' @param table.colhead.fill Background color of the column headers in the table.
#' @param table.colhead.border.width Width of border around table column headers (in pixels).
#' @param table.colhead.border.color Color of border around table column headers,
#' @param table.colhead.align.horizontal Horizontal alignment of text in table column headers.
#' @param table.colhead.font.family Font family of text in table column headers.
#' @param table.colhead.font.color Font color of text in table column headers.
#' @param table.colhead.font.size Font size (in pixels) of text in table column headers.
#' @param table.colhead.font.bold Logical; whether text in table column headers should be bold.
#' @param table.colhead.font.italics Logical; whether text in table column headers should be italicized
#' @param table.rowhead.show Logical; whether to show row headers in the table.
#' This will be ignored if \code{table} does not contain row names.
#' @param table.rowhead.fill Background color of the row headers in the table.
#' @param table.rowhead.border.width Width of border around table row headers (in pixels).
#' @param table.rowhead.border.color Color of border around table row headers,
#' @param table.rowhead.align.horizontal Horizontal alignment of text in table row headers.
#' @param table.rowhead.font.family Font family of text in table row headers.
#' @param table.rowhead.font.color Font color of text in table row headers.
#' @param table.rowhead.font.size Font size (in pixels) of text in table row headers.
#' @param table.rowhead.font.bold Logical; whether text in table row headers should be bold.
#' @param table.rowhead.font.italics Logical; whether text in table row headers should be italicized
#' @param ... Extra arguments passed to the charting function
#' @inherit Column
#' @importFrom plotly subplot
#' @export
ChartWithTable <- function(x,
                           table,
                           chart.type = "Column",
                           global.font.family = "Arial",
                           global.font.color = rgb(44, 44, 44, maxColorValue = 255),
                           chart.height = 0.6,
                           table.height = 1 - chart.height - 0.1,
                           table.column.widths = NULL,
                           table.values.fill = "transparent",
                           table.values.format = ".0f",
                           table.values.border.width = 1,
                           table.values.border.color = "#FFFFFF",
                           table.values.align.horizontal = "center",
                           table.values.font.family = global.font.family,
                           table.values.font.color = global.font.color,
                           table.values.font.size = 10,
                           table.values.font.bold = FALSE,
                           table.values.font.italics = FALSE,
                           table.colhead.show = TRUE,
                           table.colhead.fill = "#DDDDDD",
                           table.colhead.border.width = 1,
                           table.colhead.border.color = "#FFFFFF",
                           table.colhead.align.horizontal = "center",
                           table.colhead.font.family = global.font.family,
                           table.colhead.font.color = global.font.color,
                           table.colhead.font.size = 10,
                           table.colhead.font.bold = FALSE,
                           table.colhead.font.italics = FALSE,
                           table.rowhead.show = TRUE,
                           table.rowhead.fill = table.colhead.fill,
                           table.rowhead.border.width = 1,
                           table.rowhead.border.color = "#FFFFFF",
                           table.rowhead.align.horizontal = "right",
                           table.rowhead.font.family = global.font.family,
                           table.rowhead.font.color = global.font.color,
                           table.rowhead.font.size = 10,
                           table.rowhead.font.bold = FALSE,
                           table.rowhead.font.italics = FALSE,
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
                           footer.wrap = TRUE,
                           footer.wrap.nchar = 100,
                           margin.left = NULL,
                           margin.right = NULL,
                           margin.top = NULL,
                           margin.bottom = NULL,
                           background.fill.color = "transparent",
                           background.fill.opacity = 1,
                           charting.area.fill.color = background.fill.color,
                           charting.area.fill.opacity = 0,
                           ...)
{
    chart.type <- gsub(" ", "", chart.type)
    chart <- get0(chart.type, mode = "function")

    tb.ncol <- ncol(table)
    cell.values <- t(as.matrix(table))
    cell.values <- formatByD3(cell.values, table.values.format)
    if (table.values.font.bold)
        cell.values <- paste0("<b>", cell.values, "</b>")
    if (table.values.font.italics)
        cell.values <- paste0("<i>", cell.values, "</i>")
    cell.values <- matrix(cell.values, nrow = tb.ncol)

    colhead.names <- colnames(table)
    if (!is.null(rownames(table)) && table.rowhead.show)
    {
        rowhead.names <- rownames(table)
        if (table.rowhead.font.bold)
            rowhead.names <- paste0("<b>", rowhead.names, "</b>")
        if (table.rowhead.font.italics)
            rowhead.names <- paste0("<i>", rowhead.names, "</i>")
        cell.values <- rbind(rowhead.names, cell.values)
        table.values.border.width <- c(table.rowhead.border.width, rep(table.values.border.width, tb.ncol))
        table.values.border.color <- c(table.rowhead.border.color, rep(table.values.border.color, tb.ncol))
        table.values.font.family <- c(table.rowhead.font.family, rep(table.values.font.family, tb.ncol))
        table.values.font.color <- c(table.rowhead.font.color, rep(table.values.font.color, tb.ncol))
        table.values.font.size <- c(table.rowhead.font.size, rep(table.values.font.size, tb.ncol))
        table.values.fill <- c(table.rowhead.fill, rep(table.values.fill, tb.ncol))
        table.values.align.horizontal <- c(table.rowhead.align.horizontal, rep(table.values.align.horizontal, tb.ncol))
        table.column.widths <- if (is.null(table.column.widths)) c(1, rep(2, tb.ncol))
                               else rep(as.numeric(TextAsVector(table.column.widths)), length = tb.ncol + 1)
        colhead.names <- c("", colhead.names)

    } else if (!is.null(table.column.widths))
        table.column.widths <- rep(as.numeric(TextAsVector(table.column.widths)), length = tb.ncol)

    colhead.format <- list(height = 0, fill = list(color = "transparent"),
                       line = list(width = 0))
    if (!is.null(colnames(table)) && table.colhead.show)
    {
        if (table.colhead.font.bold)
            colhead.names <- paste0("<b>", colhead.names, "</b>")
        if (table.colhead.font.italics)
            colhead.names <- paste0("<i>", colhead.names, "</i>")
        colhead.format <- list(values = colhead.names,
               line = list(width = table.colhead.border.width, color = table.colhead.border.color),
               font = list(family = table.colhead.font.family, color = table.colhead.font.color,
               size = table.colhead.font.size),
               fill = list(color = table.colhead.fill), align = table.colhead.align.horizontal)
    }

    cells.format <- list(values = cell.values,
           line = list(width = table.values.border.width, color = table.values.border.color),
           font = list(family = table.values.font.family, color = table.values.font.color,
           size = table.values.font.size),
           fill = list(color = table.values.fill), align = table.values.align.horizontal)

    sub.chart <- chart(x, ...)$htmlwidget
    sub.table <- plot_ly(type = "table",
                    header = colhead.format, cells = cells.format, columnwidth = table.column.widths,
                    domain = list(x = c(0,1), y = c(0, table.height)))

    sub.chart$x$config <- NULL
    sub.table$x$config <- NULL
    res <- subplot(list(sub.chart, sub.table), nrows = 2, which_layout = 1, heights = c(0.7, 0.3),
                   titleX = TRUE, titleY = TRUE)

    # Titles and margin text
    title.font <- list(family = title.font.family, size = title.font.size, color = title.font.color)
    subtitle.font <- list(family = subtitle.font.family, size = subtitle.font.size,
                          color = subtitle.font.color)
    footer.font <- list(family = footer.font.family, size = footer.font.size, color = footer.font.color)
    footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate = FALSE)
    margins <- list(t = 20, b = 20, r = 60, l = 80, pad = 0)
    margins <- setCustomMargins(margins, margin.top, margin.bottom, margin.left,
                    margin.right, 0)
    margins <- setMarginsForText(margins, title, subtitle, footer, title.font.size,
                                 subtitle.font.size, footer.font.size)
    if (sum(nchar(subtitle)) > 0)
        subtitle <- paste0("<br>&nbsp;<br>", subtitle)
    annotations <- list(setSubtitle(subtitle, subtitle.font, margins),
                        setTitle(title, title.font, margins),
                        setFooter(footer, footer.font, margins))
    annotations <- Filter(Negate(is.null), annotations)

    res <- config(res, displayModeBar = FALSE, editable = FALSE)
    res$sizingPolicy$browser$padding <- 0
    res <- layout(res, yaxis = list(domain = c(1 - chart.height, 1)),
                  plot_bgcolor = toRGB(charting.area.fill.color, alpha = charting.area.fill.opacity),
                  paper_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity),
                  annotations = annotations, margin = margins)

    # Silence random plotly warnings
    res$x$layout$NA2 <- NULL
    res$x$layout$NA3 <- NULL

    result <- list(htmlwidget = res)
    class(result) <- "StandardChart"
    result
}
