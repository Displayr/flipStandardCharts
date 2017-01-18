scatterplot <- function(chart.matrix,
                             x.bounds.minimum,
                             x.bounds.maximum,
                             x.tick.distance)
{
    if (any(is.na(as.matrix(chart.matrix))))
    {
        warning("Data points with missing values have been omitted.")
        chart.matrix <- chart.matrix[!is.na(rowSums(chart.matrix)), ]
    }

    if (!is.matrix(chart.matrix))
        chart.matrix <- as.matrix(chart.matrix)

    ## Showing markers and lines
    series.mode = "markers"  #default = markers

    ## If chart.matrix has no colnames, and only one row, and y is a single numerical vector, then:
    ## colnames become the index value
    ## but xlabels becomes every x-label inteval value (if none, then 10 evenly spaced values)
    ## and xvalues also get set...?

    divide.by <- 10
    if (ncol(chart.matrix) <= 10)
        divide.by <- ncol(chart.matrix)

    colname.upperRange <- ncol(chart.matrix) - 1

    if (is.null(colnames(chart.matrix)) ) #&& nrow(chart.matrix) == 1
    {
        colnames(chart.matrix) <- 0:colname.upperRange

        if (is.null(x.bounds.minimum))
            x.bounds.minimum <- 0

        if (is.null(x.bounds.maximum))
            x.bounds.maximum <- ncol(chart.matrix)

        if (is.null(x.tick.distance))
            x.tick.distance <- ncol(chart.matrix) / divide.by
    }

    ## If there are no column names at this stage, then assign them:
    if (is.null(colnames(chart.matrix)))
        colnames(chart.matrix) <- paste("trace ", 1:ncol(chart.matrix))

    return(list(chart.matrix = chart.matrix,
                series.mode = series.mode,
                x.bounds.minimum = x.bounds.minimum,
                x.bounds.maximum = x.bounds.maximum,
                x.tick.distance = x.tick.distance))
}

scatterplotData <- function(chart.matrix, is.bubble, group.labels.text, group.indices.text, transpose,
                            rows.to.ignore, cols.to.ignore, x.title, y.title)
{
    if (any(is.na(as.matrix(chart.matrix))))
    {
        warning("Data points with missing values have been omitted.")
        chart.matrix <- chart.matrix[!is.na(rowSums(chart.matrix)), ]
    }

    if (is.bubble && ncol(chart.matrix) != 3)
        stop("The number of columns in the input table (after excluding ignored columns) must be 3 for a bubbleplot.")
    if (!is.bubble && ncol(chart.matrix) != 2)
        stop("The number of columns in the input table (after excluding ignored columns) must be 2 for a scatterplot.")

    # Converts a comma separated string to a vector of strings
    .parseCommaSeparatedText <- function(t)
    {
        as.vector(sapply(strsplit(t, ","), function(x) gsub("^\\s+|\\s+$", "", x)))
    }

    # Remove rows and columns to ignore
    rows.to.ignore <- .parseCommaSeparatedText(rows.to.ignore)
    cols.to.ignore <- .parseCommaSeparatedText(cols.to.ignore)
    if (length(rows.to.ignore[[1]]) == 0)
        rows.to.ignore <- NULL
    if (length(cols.to.ignore[[1]]) == 0)
        cols.to.ignore <- NULL
    chart.matrix <- GetTidyTwoDimensionalArray(chart.matrix,
                                               row.names.to.remove = rows.to.ignore,
                                               column.names.to.remove = cols.to.ignore)

    if (group.labels.text != "")
    {
        if (group.indices.text != "")
        {
            group.labels <- .parseCommaSeparatedText(group.labels.text)
            group.indices <- as.numeric(.parseCommaSeparatedText(group.indices.text))

            if (length(group.labels) == 1)
                stop(paste0("Only one group has been specified: ", group.labels[1]))

            if (length(group.indices) != nrow(chart.matrix))
                stop(paste0("The number of group indices (", length(group.indices), ") needs to equal the number of rows in the table (", nrow(chart.matrix), ")."))

            permitted.indices <- 1:length(group.labels)
            if (any(is.na(group.indices)) || !all(group.indices %in% permitted.indices))
                stop(paste0("The group indices are not in the correct format."))

            group <- group.labels[group.indices]
        }
        else
            stop("Group labels were provided but group indices are missing.")
    }
    else
    {
        if (group.indices.text != "")
            stop("Group indices were provided but group labels are missing.")
        else
            group <- rep("Group", nrow(chart.matrix))
    }

    result <- list()

    result$x <- if (transpose) as.numeric(chart.matrix[, 2]) else as.numeric(chart.matrix[, 1])
    result$y <- if (transpose) as.numeric(chart.matrix[, 1]) else as.numeric(chart.matrix[, 2])
    result$z <- if (is.bubble) as.numeric(chart.matrix[, 3]) else NULL

    if (is.bubble && any(result$z < 0))
        stop("Negative values are present in the third column. No bubbles are shown for such cases.")

    result$row.names <- rownames(chart.matrix)
    result$group <- group

    # Resolve axes labels if none specified manually
    if (x.title == "" || length(x.title) == 0)
        x.title <- colnames(chart.matrix)[1]
    if (x.title == "FALSE" || x.title == FALSE)
        x.title <- ""

    if (y.title == "" || length(y.title) == 0)
        y.title <- colnames(chart.matrix)[2]
    if (y.title == "FALSE" || y.title == FALSE)
        y.title <- ""

    result$x.title <- x.title
    result$y.title <- y.title

    result
}
