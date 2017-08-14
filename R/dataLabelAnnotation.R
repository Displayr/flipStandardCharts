#' @importFrom flipFormat FormatAsReal
dataLabelAnnotation <- function(chart.matrix,
                                data.label.mult = 1,
                                bar.decimals = 0,
                                bar.prefix = "",
                                bar.suffix = "",
                                barmode = "",
                                swap.axes.and.data = FALSE,
                                bar.gap = 0,
                                display.threshold = NULL,
                                dates)
{
    text <- paste(bar.prefix,
                  FormatWithReal(chart.matrix * data.label.mult, decimals = bar.decimals),
                  bar.suffix, sep = "")

    chart.matrix[which(is.na(chart.matrix))] <- 0

    series.count <- ncol(chart.matrix)
    if (barmode == "stack")
    {
        series.positions <- rep(0, series.count)
        y.positions <- if (swap.axes.and.data)
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
        series.positions <- ((0:(series.count - 1) + 0.5) / series.count - 0.5) * (1 - bar.gap)
        y.positions <- chart.matrix
    }

    x.positions <- if (!is.null(dates))
    {
        date.vals <- as.numeric(dates) * 1000           # convert to milliseconds
        date.vals <- date.vals - 43200000               # middle of day
        x.positions <- date.vals + rep(series.positions, each = nrow(chart.matrix)) * (date.vals[2] - date.vals[1])
    }
    else
        0:(nrow(chart.matrix) - 1) + rep(series.positions, each = nrow(chart.matrix))

    if (barmode != "stack")
    {
        text <- matrix(text, ncol=ncol(chart.matrix))
        x.positions <- matrix(x.positions, ncol=ncol(chart.matrix))
        y.positions <- matrix(y.positions, ncol=ncol(chart.matrix))
    } else
    {
        # Plotly v4.7.1 handles unnamed matrices differently for some reason
        if (all(rownames(chart.matrix) == as.character(1:nrow(chart.matrix))))
            x.positions <- x.positions + 1
    }

    data.annotations <- if (swap.axes.and.data)
    {
        xanchor <- if (barmode == "stack")
            "middle"
        else
            ifelse(chart.matrix >= 0, "left", "right")

        list(x = y.positions,
             y = x.positions,
             text = text,
             showarrow = FALSE,
             xanchor = xanchor)
    }
    else
    {
        yanchor <- if (barmode == "stack")
            "top"
        else
            ifelse(chart.matrix >= 0, "bottom", "top")

        list(x = x.positions,
             y = y.positions,
             text = text,
             showarrow = FALSE,
             yanchor = yanchor)
    }
    data.annotations
}
