#' @importFrom flipFormat FormatAsReal
dataLabelPositions <- function(chart.matrix,
                                axis.type,
                                annotations = NULL,
                                data.label.mult = 1,
                                bar.decimals = 0,
                                bar.prefix = "",
                                bar.suffix = "",
                                barmode = "",
                                swap.axes.and.data = FALSE,
                                bar.gap = 0,
                                display.threshold = NULL,
                                dates,
                                reversed = FALSE,
                                font,
                                hide.sign = FALSE,
                                center.data.labels = FALSE)
{
    text.values <- chart.matrix * data.label.mult
    if (hide.sign)
        text.values <- abs(text.values)
    text <- if (!is.null(annotations)) annotations
            else paste(bar.prefix,
                  FormatAsReal(text.values, decimals = bar.decimals),
                  bar.suffix, sep = "")

    chart.matrix[which(is.na(chart.matrix))] <- 0
    if (barmode == "relative")
       barmode <- "stack"

    series.count <- ncol(chart.matrix)
    if (barmode == "stack")
    {
        series.pos <- rep(0, series.count)
        y.pos <- if (swap.axes.and.data || center.data.labels)
            cum.signed.data(chart.matrix) - 0.5 * chart.matrix
        else
            cum.signed.data(chart.matrix)

        largest.bar <- max(rowSums(abs(chart.matrix)))
        if (is.null(display.threshold))
            display.threshold <- 0.05
        text[abs(chart.matrix) < largest.bar * display.threshold] <- ""
        text[chart.matrix == 0] <- ""


    } else
    {
        series.pos <- ((0:(series.count - 1) + 0.5) / series.count - 0.5) * (1 - bar.gap)
        y.pos <- chart.matrix
    }

    x.pos <- NULL
    if (axis.type == "date")
    {
        date.vals <- as.numeric(dates) * 1000           # convert to milliseconds
        date.diff <- if (length(dates) == 1) 1 else date.vals[2] - date.vals[1]
        if (barmode != "stack")
            x.pos <- date.vals + (rep(series.pos, each = nrow(chart.matrix)) * date.diff)
        else
            x.pos <- rep(date.vals, ncol(chart.matrix))
    }
    else if (axis.type == "numeric")
        x.pos <- as.numeric(rownames(chart.matrix)) + rep(series.pos, each = nrow(chart.matrix))
    else
        x.pos <- 0:(nrow(chart.matrix) - 1) + rep(series.pos, each = nrow(chart.matrix))

    if(swap.axes.and.data)
    {
        tmp.pos <- x.pos
        x.pos <- y.pos
        y.pos <- tmp.pos
    }
    text <- matrix(text, ncol=ncol(chart.matrix))
    x.pos <- matrix(x.pos, ncol=ncol(chart.matrix))
    y.pos <- matrix(y.pos, ncol=ncol(chart.matrix))
    return(list(text = text, x = x.pos, y = y.pos))
}
