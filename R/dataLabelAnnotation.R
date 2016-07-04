## Create list of annotations if data.label > 0
dataLabelAnnotation <- function(chart.matrix,
                                bar.offset = 0,
                                bar.family = "Arial",
                                bar.size = 10,
                                bar.color = "black",
                                bar.decimals = 0,
                                barmode = "",
                                bar.data.label.as.percent = FALSE,
                                swap.axes.and.data = FALSE
                        )
{
series.count <- nrow(chart.matrix)
x.items <- 0:(ncol(chart.matrix) - 1)
spacing <- 1 / series.count
series.positions <- NULL

if (length(bar.color) < series.count)
    bar.color <- rep(bar.color, series.count)

if (series.count%%2 == 0)
    series.count <- series.count / 2
else
    series.count <- trunc(series.count / 2, 0)

for (i in 1:series.count)
{
    series.positions <- c(series.positions, i * spacing, -1 * (i * spacing))
}
series.positions <- sort(c(series.positions, 0))

data.annotations <- list()

if (barmode == "stack")
{
    series.positions <- rep(0, length(series.positions))

    # Y positions become the stacked values, however, which is different from the data values.
    ypositions <- cum.data(chart.matrix, "cumulative.sum")

    # Modify to become the middle of the range
    ypositions.half <- chart.matrix / 2

    ypositions.sum <- rbind(rep(0,ncol(ypositions)), ypositions)
    ypositions.sum <- ypositions.sum[1:nrow(ypositions.sum) - 1, ]
    ypositions.sum <- ypositions.sum + ypositions.half
}

## Are column headers to be treated as numbers?
column.names <- colnames(chart.matrix)
check.coercion <- tryCatch(as.numeric(column.names), error = function(e) e, warning = function(w) w)

column.character <- FALSE
if (inherits(check.coercion, "warning"))
    column.character <- TRUE

if (barmode == "stack")
    loop.by <- 1:length(x.items)
else
    loop.by <- x.items

for (a in loop.by)
{
    if (column.character == FALSE)
        add.factor <- as.numeric(column.names[a])
    else
        add.factor <- a

    x.offsets <- series.positions + add.factor

    for (b in 1:length(x.offsets))
    {
        if (barmode == "stack")
            data.point <- chart.matrix[b, a]
        else
            data.point <- chart.matrix[b, a + 1]

        if (data.point < 0 && barmode != "stack")
            ypos <- data.point + bar.offset
        else if (data.point >= 0 && barmode != "stack")
            ypos <- data.point - bar.offset
        else
            ypos <- ypositions.sum[b, a]

        # if (is.na(x.offsets[b]))
        #     break

        # Data label as percent formatting
        if (bar.data.label.as.percent)
            text <- paste(round((data.point * 100), bar.decimals), "%", sep = "")
        else
            text <- round(data.point, bar.decimals)

        x <- x.offsets[b]
        y <- ypos
        if (swap.axes.and.data)
        {
            x <- ypos
            y <- x.offsets[b]
        }

        data.annotations[[length(data.annotations) + 1]] <- list(x = x,
                                                                 y = y,
                                                                 text = text,
                                                                 font = list(
                                                                     family = bar.family,
                                                                     size = bar.size,
                                                                     color = plotly::toRGB(bar.color[b], 1)
                                                                 ),
                                                                 showarrow = FALSE)
    }
}

data.annotations
}
