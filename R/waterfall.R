#' Waterfall
#'
#' Creates a waterfall chart showing the source(s) of change in sales for one period, relative to the previous period.
#' @param y A vector, or, a matrix.
#' @param cumulative If \code{y} is a \code{vector}, this argument is ignored. Otherwise,
#' it can be a \code{\link{vector}} of length \code{length(y)}, or, an \code{integer} indicating the column
#' in \code{y} containing the cumulative values. It defaults to \code{1}.
#' @param top The position from which the blocks "hang" (i.e., the top of each column). Only used if
#' \code{y} is a \code{\link{matrix}}. If not supplied, it is assumed to be the same as \code{cumulative}.
#' @param decimals The number of decimals to show in value labels. Defaults to 1.
#' @param prefix The text to appear before after a value in labels. Defaults to nothing.
#' @param suffix The text to appear after a value in labels. Defaults to nothing.
#' @param title The title for the chart. Defaults to nothing.
#' @param y.title The title for the chart. Defaults to nothing.
#' @param colors The colors of the blocks, defaulting to red for negative values and blue for positive values.
#' Where \code{y} is a \code{\link{vector}}, negative values get the first color and positives the second, unless the length
#' of \code{colors} is the same as \code{y}, in which case each color represents each category of \code{y} if \code{y} is a \code{vector},
#' and each column otherwise.
#' a \code{\link{matrix}}, colors are assigned to each column of \code{y}.
#' @param show.cumulative If \code{TRUE}, which is the default, the cumulative labels are shown at the top of each bar.
#' @param label.in.column.threshold Where the height of a block as a proportion of the range of values on the y-axis
#' is less than this value, the size of the block is shown below (rather than within) the columns.
#' The defaut is 0.025.
#' @param label.offset When a label is not plotted in a box (which includes all times when the cumulative value is plotted above),
#' this parameter controls how far the labels is from the box, and it is intepretated as a proportion of the range of \code{y}.
#' The defaut is 0.025.
#' is less than this value, the size of the block is shown below (rather than within) the columns.
#' @importFrom plotly plot_ly add_trace layout config add_annotations add_text
#' @importFrom flipFormat FormatAsReal
#' @export
Waterfall <- function(y,
                      cumulative = 1,
                      top = NULL,
                      decimals = 1,
                      prefix = "",
                      suffix = "",
                      title = "",
                      y.title = "",
                      colors = c("red", "blue"),
                      show.cumulative = TRUE,
                      label.in.column.threshold = 0.025,
                      label.offset = 0.025)
{
    if (is.vector(y)) # Converting to a matrix
    {
        original.y <- y
        n <- length(y)
        y <- matrix(0, n, 2, dimnames = list(names(y),c("Decrease", "Increase")))
        y[original.y < 0, 1] <- original.y[original.y < 0]
        y[original.y >= 0, 2] <- original.y[original.y >= 0]
        cumulative <- cumsum(original.y)
        bs <- ifelse(y < 0, cumulative, c(0, cumulative[-n]))
        top <- bs + abs(original.y)
    } else if (length(cumulative) == 1)
    {
        cm <- y[, cumulative]
        y <- y[, -cumulative, drop = FALSE]
        cumulative <- cm
    }
    if (is.null(top))
        top <- cumulative
    n <- nrow(y)
    k <- ncol(y)
    categories <- rownames(y)
    categories <- factor(categories, levels = categories)
    # Formatting labels
    y.text <- FormatAsReal(y, decimals = decimals)
    y.text[y > 0] <- paste0("+", y.text[y > 0])
    y.text <- matrix(paste0(prefix, y.text, suffix), ncol = ncol(y.text))
    # Creating series for the plot
    y <- abs(y)
    base <- top - apply(y, 1, sum)
    cumulative.text <- paste0(prefix, FormatAsReal(cumulative, decimals = decimals), suffix)
    rng <- max(top) - min(base)
    label.offset <- rng * label.offset
    # Creating the plot (empty)
    p <- plot_ly(x = categories,
                 y = base,
                 marker = list(color = "white"),
                 hoverinfo='none',
                 type = "bar")
    # blocks
    block.base <- base
    if (length(colors) < k)
        k <- rep(colors, k) #Creates an unnecssarily long but harmless vector
    for (i in 1:k)
    {
        y.values <- y[, i]
        p <- add_trace(p = p,
                       x = categories,
                       y = y.values,
                       name = categories,
                       marker = list(color = colors[i]),
                       hoverinfo = "text",
                       type = "bar")
        # Labels
        positives <- y.values > 0
        ys <- y.values[positives]
        small.box <- ys / rng < label.in.column.threshold
        label.y.position <- ifelse (small.box, block.base[positives] - label.offset,  (block.base[positives] * 2 + ys) / 2 )
        p <- add_text(p,
                              x = categories[positives],
                              y = label.y.position,
                              text = y.text[positives, i],
                              textfont = list(color = ifelse(label.y.position < base[positives], "black", "white")),
                              marker = NULL)
        block.base <- block.base + y.values
    }
    # Cumulative labels
    if (show.cumulative)
        p <- add_text(p,
                              x = categories,
                              y = top + label.offset,
                              text = cumulative.text,
                              textfont = list(color = "black"),
                              marker = NULL)
    # Finalizing the plotting options
    p <- config(p, displayModeBar = FALSE)
    layout(p = p,
           barmode = "stack",
           showlegend = FALSE,
           title = title,
           xaxis = list(title = "",
                        zeroline = FALSE,
                        showticklabels = TRUE,
                        showgrid = FALSE),
           yaxis = list(title = y.title,
                        zeroline = TRUE,
                        showticklabels = FALSE,
                        showgrid = FALSE))
}

