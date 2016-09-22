labeledScatterplot <- function(chart.matrix,
                        type = "Labeled Scatterplot",
                        group = NULL,
                        grid = TRUE,
                        origin = FALSE,
                        transpose = FALSE,
                        colors = NULL,
                        qinput = FALSE
                        )
{
    is.bubble <- type == "Labeled Bubbleplot"
    is.scatter <- type == "Labeled Scatterplot"
    has.spans <- length(dim(chart.matrix)) == 3
    q.array <- is.array(chart.matrix) && qinput



    ## Check for spans and restructure if needed
    if (qinput)
    {
        if (q.array)
        {
        ## If it's an array... hammer it into shape - i.e. it MAY have a third dimension with span labels included. Yay.
        ## dput 2 objects, one with a grid in the blue drop-down, one with a grid in the brow drop-down.



        ## If it's not an array, do this:
        } else {
            reshaped <- spanCheck(chart.matrix, group)[[1]]
            chart.matrix <- reshaped[[1]]
            group <- reshaped[[2]]
        }

        if (is.bubble && ncol(chart.matrix) != 3)
            stop("The number of columns in the input table must be 3 or a multiple of 3 for a bubbleplot.")

        if (!is.bubble && ncol(chart.matrix) != 2)
            stop("The number of columns in the input table must be 2 or a multiple of 2 for a scatterplot.")
    } else {
        ## Check class of input, must be data.frame, columns need to be numeric with one optional character column.

        ## If they are not, attempt to coerce to required classes. (e.g. convert factors to numerics)

        ## If coercion does not work, then stop.

        ## Input must have labelled rows and column headings; if not, stop.

        if (is.bubble && !(ncol(chart.matrix) %in% c(3,4)))
            stop("The number of columns in the input table must be 3 or 4 columns for a bubbleplot (with the optional 4th column containing group labels).")

        if (!is.bubble && ncol(chart.matrix) %in% c(2,3))
            stop("The number of columns in the input table must be 2 or 3 columns for a scatterplot (with the optional 3rd column containing group labels).")


        ## Restructure so that final output is in a list like for the Qinput.
    }



    # ## If the table has one column but two or three rows, then accept, but transpose
    # if (ncol(chart.matrix) == 1 && (nrow(chart.matrix) == 2 || nrow(chart.matrix) == 3))
    #     chart.matrix <- t(chart.matrix)

    ## Below needs to be strict about the chart type, viz. Bubble vs. Scatter; Bubble can have 3 columns (or 4 for
    ## rItems); Scatter should have 2 (or 3)

    ## If transpose = true, then swap x and y
    if (transpose && ncol(chart.matrix) == 2)
        chart.matrix <- chart.matrix[, c(2, 1)]
    else if (transpose && ncol(chart.matrix) == 3)
        chart.matrix <- chart.matrix[, c(2, 1, 3)]

    X <- chart.matrix[, 1]
    Y <- chart.matrix[, 2]
    Z <- NULL
    if (is.bubble)
        Z <- chart.matrix[, 3]

    label <- rownames(chart.matrix)

    ## Strip out alpha
    colors <- flipChartBasics::StripAlphaChannel(colors)

    output <- list(X = X,
                   Y = Y,
                   Z = Z,
                   label = label,
                   group = group,
                   grid = grid,
                   origin = origin,
                   colors = colors)

    return(output)
}



## Function to resolve structure from dput Q table, where
span.in.stub <- structure(c(10.625, 3.125, 8.125, 9.375, 1.25, 7.5, 8.125, 3.75,
5.625, 7.5, 1.875, 6.875, 9.375, 3.75, 8.125, 7.5, 5, 6.875,
9.375, 3.125, 10, 14.375, 5.625, 15, 5, 1.875, 6.875, 11.9760479041916,
1.79640718562874, 6.58682634730539, 11.9760479041916, 1.19760479041916,
7.78443113772455, 7.18562874251497, 2.9940119760479, 3.59281437125748,
11.377245508982, 2.9940119760479, 10.7784431137725, 6.58682634730539,
1.79640718562874, 6.58682634730539, 7.78443113772455, 1.19760479041916,
5.98802395209581, 8.98203592814371, 4.19161676646707, 7.78443113772455,
10.7784431137725, 3.59281437125748, 8.98203592814371, 4.79041916167665,
1.79640718562874, 4.79041916167665), .Dim = c(27L, 2L), statistic = "Column %", .Dimnames = list(
    c("Colas (e.g., Coca Cola, Pepsi Max)?", "Sparkling mineral water",
    "Coffee", "Colas (e.g., Coca Cola, Pepsi Max)?", "Sparkling mineral water",
    "Coffee", "Colas (e.g., Coca Cola, Pepsi Max)?", "Sparkling mineral water",
    "Coffee", "Colas (e.g., Coca Cola, Pepsi Max)?", "Sparkling mineral water",
    "Coffee", "Colas (e.g., Coca Cola, Pepsi Max)?", "Sparkling mineral water",
    "Coffee", "Colas (e.g., Coca Cola, Pepsi Max)?", "Sparkling mineral water",
    "Coffee", "Colas (e.g., Coca Cola, Pepsi Max)?", "Sparkling mineral water",
    "Coffee", "Colas (e.g., Coca Cola, Pepsi Max)?", "Sparkling mineral water",
    "Coffee", "Colas (e.g., Coca Cola, Pepsi Max)?", "Sparkling mineral water",
    "Coffee"), c("Male", "Female")), name = "BANNER by Q2. Gender", questions = c("BANNER",
"Q2. Gender"))

span.in.banner <- structure(c(13.0769230769231, 11.5384615384615, 10, 9.23076923076923,
11.5384615384615, 9.23076923076923, 11.5384615384615, 17.6923076923077,
6.15384615384615, 10.6382978723404, 4.25531914893617, 12.7659574468085,
6.38297872340426, 12.7659574468085, 17.0212765957447, 10.6382978723404,
19.1489361702128, 6.38297872340426, 10.8333333333333, 10, 7.5,
9.16666666666667, 10.8333333333333, 9.16666666666667, 13.3333333333333,
20, 9.16666666666667, 14.7058823529412, 14.7058823529412, 8.82352941176471,
13.9705882352941, 8.08823529411765, 9.55882352941176, 11.0294117647059,
13.2352941176471, 5.88235294117647, 8.33333333333333, 5.55555555555556,
13.8888888888889, 13.8888888888889, 8.33333333333333, 5.55555555555556,
19.4444444444444, 16.6666666666667, 8.33333333333333, 10.4761904761905,
12.3809523809524, 5.71428571428571, 17.1428571428571, 10.4761904761905,
9.52380952380952, 12.3809523809524, 14.2857142857143, 7.61904761904762
), .Dim = c(9L, 6L), statistic = "Column %", .Dimnames = list(
    c("18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44",
    "45 to 49", "50 to 54", "55 to 64", "65 or more"), c("Colas (e.g., Coca Cola, Pepsi Max)?",
    "Sparkling mineral water", "Coffee", "Colas (e.g., Coca Cola, Pepsi Max)?",
    "Sparkling mineral water", "Coffee")), name = "Span in Banner", questions = c("Q3. Age",
"BANNER1"))

crossed.grid <- structure(c(2.5, 65.625, 18.125, 8.75, 67.5, 10, 8.125, 100,
1.875, 55.625, 50.625, 3.75, 53.125, 33.125, 21.875, 100, 7.5,
22.5, 12.5, 11.25, 40.625, 8.75, 33.125, 100, 65, 23.75, 3.125,
35.625, 9.375, 8.75, 8.75, 100, 17.5, 7.5, 52.5, 18.125, 13.75,
53.125, 16.25, 100, 22.5, 3.75, 31.25, 18.75, 3.125, 42.5, 17.5,
100, 8.125, 25.625, 12.5, 14.375, 30, 8.75, 32.5, 100, 90, 13.125,
3.75, 54.375, 3.125, 4.375, 4.375, 100, 1.25, 75, 61.25, 0, 73.75,
41.875, 7.5, 100, 96.25, 91.875, 88.75, 81.25, 95, 87.5, 58.125,
100, 10.1796407185629, 49.7005988023952, 26.3473053892216, 8.38323353293413,
53.8922155688623, 10.1796407185629, 11.377245508982, 100, 1.79640718562874,
61.6766467065868, 59.2814371257485, 0.598802395209581, 62.2754491017964,
28.7425149700599, 13.1736526946108, 100, 10.7784431137725, 23.3532934131737,
13.1736526946108, 8.38323353293413, 46.1077844311377, 5.98802395209581,
26.9461077844311, 100, 65.2694610778443, 19.7604790419162, 5.98802395209581,
40.1197604790419, 8.98203592814371, 4.19161676646707, 8.38323353293413,
100, 27.5449101796407, 10.7784431137725, 51.497005988024, 13.1736526946108,
18.562874251497, 47.9041916167665, 7.78443113772455, 100, 29.940119760479,
5.38922155688623, 31.7365269461078, 16.7664670658683, 4.79041916167665,
46.7065868263473, 14.3712574850299, 100, 10.1796407185629, 21.5568862275449,
5.98802395209581, 14.3712574850299, 29.3413173652695, 4.19161676646707,
44.9101796407186, 100, 94.6107784431138, 16.1676646706587, 2.39520958083832,
53.2934131736527, 3.59281437125748, 3.59281437125748, 1.19760479041916,
100, 0, 77.8443113772455, 67.6646706586826, 0, 79.0419161676647,
39.5209580838323, 3.59281437125748, 100, 100, 92.814371257485,
92.814371257485, 76.6467065868264, 95.2095808383233, 86.2275449101796,
56.8862275449102, 100), .Dim = c(8L, 10L, 2L), statistic = "Column %", .Dimnames = list(
    c("Coke", "Diet Coke", "Coke Zero", "Pepsi", "Diet Pepsi",
    "Pepsi Max", "None of these", "NET"), c("Feminine", "Health-conscious",
    "Innocent", "Older", "Open to new experiences", "Rebellious",
    "Sleepy", "Traditional", "Weight-conscious", "NET"), c("Male",
    "Female")), name = "Q5.  Brand associations by Q2. Gender", questions = c("Q5.  Brand associations",
"Q2. Gender"))


## Resolving structure where there is no indication of spans
spanCheck <- function(chart.matrix, span.labels = NULL)
{

    # Checking unique rows and columns is only necessary when
    # the input is a matrix. If it is an array, the elements
    # of the array can be stacked on top of one another.









    rows <- dim(chart.matrix)[1]
    cols <- dim(chart.matrix)[2]

    ## If duplication of column headings in matrix, then spans belong in banner
    unique.cols <- length(unique(colnames(chart.matrix)))
    unique.rows <- length(unique(rownames(chart.matrix)))

    ### Span in stub or span in banner?
    span.in.banner <- FALSE
    span.in.stub <- FALSE
    no.span <- FALSE
    if (unique.cols < cols)
    {
        if (cols %% unique.cols == 0)
        {
            span.in.banner <- TRUE
        } else {
            warning("There are duplicate column labels in the input. Consider changing some of the column labels.")
        }
    }


    if (unique.rows < rows)
    {
        if (rows %% unique.rows== 0)
        {
            span.in.stub <- TRUE
        } else {
            warning("There are duplicate row labels in the input. Consider changing some of the row labels.")
        }
    }

    if (unique.cols == cols && unique.rows == rows)
        no.span <- TRUE

    ## If the span is in the banner, then restructure and create groups
    if (span.in.banner)
    {
        repetitions <- cols / unique.cols

        ## Stack data
        ### Store row labels
        row.labels <- rownames(chart.matrix)

        ### Store column headings
        column.headings <- colnames(chart.matrix)

        ### Turn it into a data frame
        chart.matrix <- data.frame(chart.matrix)

        ### Change headings so that reshape function can cope
        colnames(chart.matrix) <- paste(1:(ncol(chart.matrix)/repetitions), rep(0:(repetitions-1), each = unique.cols), sep = ".")

        ### Reshape the data
        chart.matrix <- stats::reshape(chart.matrix, direction = "long", varying = 1:cols)

        ### Turn back to matrix
        chart.matrix <- as.matrix(chart.matrix)

        ### Get rid of extra columns
        chart.matrix <- chart.matrix[, 2:(unique.cols + 1)]

        ### Add back row labels
        rownames(chart.matrix) <- rep(row.labels, repetitions)

        ### Add back column labels
        colnames(chart.matrix) <- column.headings[1:3]

        ## Create vector of group names
        if (is.null(span.labels) || length(span.labels) != repetitions)
            group.names <- paste("Category ", 1:repetitions)
        else
            group.names <- span.labels

        ### Get as many group names as required (i.e. same as number of rows)
        groups <- rep(group.names, each = rows)
    }

    ## If the span is in the stub, then the data is already structured, and we just need to create the groups
    if (span.in.stub)
    {
        repetitions <- rows / unique.rows

        if (is.null(span.labels) || length(span.labels) != repetitions)
            group.names <- paste("Category ", 1:repetitions)
        else
            group.names <- span.labels

        groups <- rep(group.names, each = unique.rows)
    }

    ## If no span, make single group
    if (no.span)
        groups <- rep("Category", rows)

    return(list(chart.matrix = chart.matrix,
                groups = groups))
}
