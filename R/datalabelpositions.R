#' @importFrom flipFormat FormatAsReal
dataLabelPositions <- function(chart.matrix,
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
                                font)
{
    text <- if (!is.null(annotations)) annotations
            else paste(bar.prefix,
                  FormatAsReal(chart.matrix * data.label.mult, decimals = bar.decimals),
                  bar.suffix, sep = "")

    chart.matrix[which(is.na(chart.matrix))] <- 0

    series.count <- ncol(chart.matrix)
    if (barmode == "stack")
    {
        series.pos <- rep(0, series.count)
        y.pos <- if (swap.axes.and.data)
            cum.data(chart.matrix, "cumulative.sum") - 0.5 * chart.matrix
        else
            cum.data(chart.matrix, "cumulative.sum")

        largest.bar <- max(rowSums(chart.matrix))
        if (is.null(display.threshold))
            display.threshold <- 0.05
        text[chart.matrix < largest.bar * display.threshold] <- ""
    }
    else
    {
        series.pos <- ((0:(series.count - 1) + 0.5) / series.count - 0.5) * (1 - bar.gap)
        y.pos <- chart.matrix
    }

    x.pos <- NULL
    if (!is.null(dates))
    {
        date.vals <- as.numeric(dates) * 1000           # convert to milliseconds
        date.vals <- date.vals - 43200000               # middle of day
        x.pos <- date.vals + rep(series.pos, each = nrow(chart.matrix)) * (date.vals[2] - date.vals[1])
    }
    else if (all(!is.na(suppressWarnings(as.numeric(rownames(chart.matrix))))))
        x.pos <- as.numeric(rownames(chart.matrix)) + rep(series.pos, each = nrow(chart.matrix))
    else
        x.pos <- 0:(nrow(chart.matrix) - 1) + rep(series.pos, each = nrow(chart.matrix))

    if(swap.axes.and.data)
    {
        tmp.pos <- x.pos
        x.pos <- y.pos
        y.pos <- tmp.pos
    }
    if (barmode != "stack")
    {
        text <- matrix(text, ncol=ncol(chart.matrix))
        x.pos <- matrix(x.pos, ncol=ncol(chart.matrix))
        y.pos <- matrix(y.pos, ncol=ncol(chart.matrix))
        return(list(text = text, x = x.pos, y = y.pos))
    }
    xanchor <- "center"
    yanchor <- "middle"
    if (!swap.axes.and.data)
    {
        yanchor <- "top"
        if (barmode == "stack" && reversed)
            yanchor <- "bottom"
    }
    n <- length(text)
    return(lapply(1:n, function(ii) list(text = text[ii], font = font, 
           x = x.pos[ii], y = y.pos[ii], showarrow = FALSE,
           xref = "x", yref = "y", xanchor = xanchor, yanchor = yanchor)))
}
