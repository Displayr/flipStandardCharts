labeledScatterplot <- function(chart.matrix,
                        colors = qColors,
                        colors.reverse = FALSE,
                        type = "Labeled Scatterplot",
                        group = NULL,
                        grid = TRUE,
                        origin = FALSE,
                        transpose = FALSE,
                        qinput = FALSE,
                        rows.to.ignore = "",
                        cols.to.ignore = "",
                        legend.show = TRUE,
                        x.title = "",
                        y.title = ""
                        )
{
    is.bubble <- type == "Labeled Bubbleplot"
    is.scatter <- type == "Labeled Scatterplot"
    has.spans <- length(dim(chart.matrix)) == 3
    q.array <- is.array(chart.matrix) && qinput && length(dim(chart.matrix)) == 3
    stored.group <- group

    ## As rows and columns to ignore are dealt with locally for these plot types, we need to separate on comma here.
    rows.to.ignore <- as.vector(sapply(strsplit(rows.to.ignore, ","), function(x) gsub("^\\s+|\\s+$", "", x)))
    cols.to.ignore <- as.vector(sapply(strsplit(cols.to.ignore, ","), function(x) gsub("^\\s+|\\s+$", "", x)))

    ## Is there an empty group string?
    if (nchar(group) == 0 && !is.null(group))
        group <- NULL

    ## Separate group string into vector
    if (!is.null(group))
        group <- as.vector(sapply(strsplit(group, ","), function(x) gsub("^\\s+|\\s+$", "", x)))

    if (is.array(chart.matrix) && !is.matrix(chart.matrix))
    {
        ## Get column names
        column.names <- unlist(lapply(strsplit(rownames(as.matrix(stats::ftable(chart.matrix))), "_"), function(x) x[1]))

        ## Convert to a matrix
        chart.matrix <- as.matrix(stats::ftable(chart.matrix))

        ## Set the row-names
        rownames(chart.matrix) <- unlist(lapply(strsplit(rownames(as.matrix(stats::ftable(chart.matrix))), "_"), function(x) x[2]))

        ## If there are "columns" to remove, they're now stored in column names, so remove these from the chart.matrix.
        ## "rows" to remove are removed further down; this is only relevant to array inputs.
        if (cols.to.ignore[1] != "")
            chart.matrix <- chart.matrix[!(column.names %in% cols.to.ignore), ]

        ## If no groups manually provided, then get from data
        ## If span names and/or groups specified.  Functionality removed 30/09; should it be?
        if (!is.null(group))
            group <- rep(group, nrow(chart.matrix) / length(group))
        else
            group <- column.names[!(column.names %in% cols.to.ignore)]

    } else if (is.matrix(chart.matrix) || is.data.frame(chart.matrix))
    {
        reshaped <- spanCheck(chart.matrix, group)
        chart.matrix <- reshaped[[1]]
        group <- reshaped[[2]]
    }

    if (is.bubble && ncol(chart.matrix) != 3)
        stop("The number of columns in the input table must be 3 or a multiple of 3 for a bubbleplot.")

    if (!is.bubble && ncol(chart.matrix) != 2)
        stop("The number of columns in the input table must be 2 or a multiple of 2 for a scatterplot.")

    #
    #
    #
    # ## Check for spans and restructure if needed
    # if (qinput)
    # {
    #     ## If the input is an array
    #     if (q.array)
    #     {
    #         chart.matrix <- as.matrix(stats::ftable(chart.matrix))
    #
    #         if (!is.null(group))
    #             group <- rep(group, nrow(chart.matrix) / length(group))
    #         else
    #             group <- unlist(lapply(strsplit(rownames(as.matrix(stats::ftable(chart.matrix))), "_"), function(x) x[1]))
    #
    #         rownames(chart.matrix) <- unlist(lapply(strsplit(rownames(as.matrix(stats::ftable(chart.matrix))), "_"), function(x) x[2]))
    #    ## If it's not an array
    #     } else {
    #         reshaped <- spanCheck(chart.matrix, group)
    #         chart.matrix <- reshaped[[1]]
    #         group <- reshaped[[2]]
    #     }
    #
    #     if (is.bubble && ncol(chart.matrix) != 3)
    #         stop("The number of columns in the input table must be 3 or a multiple of 3 for a bubbleplot.")
    #
    #     if (!is.bubble && ncol(chart.matrix) != 2)
    #         stop("The number of columns in the input table must be 2 or a multiple of 2 for a scatterplot.")
    # } else {
    #     ## If the input is not from q, i.e. an rItem
    #     reshaped <- rItemTransform(chart.matrix, is.bubble, group)
    #     chart.matrix <- reshaped[[1]]
    #     group <- reshaped[[2]]
    # }

    ## If transpose = true, then swap x and y
    if (transpose && ncol(chart.matrix) == 2)
        chart.matrix <- chart.matrix[, c(2, 1)]
    else if (transpose && ncol(chart.matrix) == 3)
        chart.matrix <- chart.matrix[, c(2, 1, 3)]

    if (rows.to.ignore[1] != "")
    {
        chart.matrix <- flipData::GetTidyTwoDimensionalArray(chart.matrix, rows.to.ignore)

        # ## Remove rows to ignore - special operation to allow array input.
        # remove.rows <- as.vector(sapply(strsplit(rows.to.ignore, ","), function(x) gsub("^\\s+|\\s+$", "", x)))
        # ## First, remove any rows with a corresponding item to ignore in the groups
        # chart.matrix <- chart.matrix[!group %in% remove.rows, ]
        # ## Second, remove any items from group with corresponding items to ignore
        # group <- group[!group %in% remove.rows]
        # ## Third, remove any items from group with corresponding items to ignore in the chart.matrix (labels)
        # group <- group[!rownames(chart.matrix) %in% remove.rows]
        # ## Fourth, remove any rows from chart.matric with items to ignore in the row names
        # chart.matrix <- chart.matrix[!rownames(chart.matrix) %in% remove.rows, ]
    }

    ## Populate the final items for output
    X <- as.numeric(chart.matrix[, 1])
    Y <- as.numeric(chart.matrix[, 2])
    Z <- NULL
    if (is.bubble)
        Z <- as.numeric(chart.matrix[, 3])

    label <- rownames(chart.matrix)

    ## Resolve whether to show the legend for groups/colors or not.
    if ((nchar(stored.group) == 0 || is.null(stored.group)) && length(unique(group)) == 1 && type %in% c("Labeled Scatterplot") )
        legend.show = FALSE

    ## Resolving colors
    num.colors <- length(unique(group))
    colors <- flipChartBasics::StripAlphaChannel(flipChartBasics::ChartColors(number.colors.needed = num.colors, given.colors = colors, reverse = colors.reverse))

    ## Resolve axes labels if none specified manually
    if (x.title == "" || length(x.title) == 0)
        x.title <- colnames(chart.matrix)[1]

    if (x.title == "FALSE" || x.title == FALSE)
        x.title <- ""

    if (y.title == "" || length(y.title) == 0)
        y.title <- colnames(chart.matrix)[2]

    if (y.title == "FALSE" || y.title == FALSE)
        y.title <- ""

    output <- list(X = X,
                   Y = Y,
                   Z = Z,
                   colors = colors,
                   label = label,
                   group = group,
                   grid = grid,
                   origin = origin,
                   colors = colors,
                   legend.show = legend.show,
                   x.title = x.title,
                   y.title = y.title)

    return(output)
}

## Resolving structure where there is no indication of spans
spanCheck <- function(chart.matrix, span.labels = NULL)
{
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
        colnames(chart.matrix) <- column.headings[1:unique.cols]

        ## Create vector of group names
        if (is.null(span.labels) || length(span.labels) != repetitions)
            group.names <- paste("Group ", each = 1:repetitions)
        else
            group.names <- span.labels

        ### Get as many group names as required (i.e. same as number of rows)
            groups <- rep(group.names, each = unique.rows)
    }

    ## If the span is in the stub, then the data is already structured, and we just need to create the groups
    if (span.in.stub)
    {
        repetitions <- rows / unique.rows

        if (is.null(span.labels) || length(span.labels) != repetitions)
            group.names <- paste("Group ", each = 1:repetitions)
        else
            group.names <- span.labels

        groups <- rep(group.names, each = unique.rows)
    }

    ## If no span, make single group
    if (no.span && is.null(span.labels))
    {
        groups <- rep("Group", rows)
    }
    else if (!is.null(span.labels))
    {
        if (length(span.labels) < unique.rows)
            groups <- rep(span.labels, each = unique.rows)
        else
            groups <- span.labels
    }

    return(list(chart.matrix = chart.matrix,
                groups = groups))
}


# rItemTransform <- function(chart.matrix, is.bubble, span.labels)
# {
#
#     if ((is.matrix(chart.matrix) && is.numeric(chart.matrix))
#         || (is.data.frame(chart.matrix) && !any(unlist(lapply(chart.matrix, class)) %in% c("character", "factor")) ))
#     {
#         if (is.null(rownames(chart.matrix)) || is.null(colnames(chart.matrix)))
#             stop("Your input matrix/data frame must have row and column names")
#
#         if (is.bubble && ncol(chart.matrix) != 3)
#             stop("You need exactly three data columns for a bubbleplot")
#
#         if (!is.bubble && ncol(chart.matrix) != 2)
#             stop("You need exactly two data columns for a scatterplot")
#
#         ## Make group
#         use.text <- c("Group")
#         if (!is.null(span.labels))
#             use.text <- span.labels
#
#         groups <- rep(use.text, each = nrow(chart.matrix) / length(use.text))
#
#         ## If it's a data frame, make matrix
#         temp <- as.data.frame(chart.matrix)
#
#     } else {
#         ## Check that the input data frame has the appropriate number of columns
#         if (!is.bubble)
#             right.cols <- ncol(chart.matrix) %in% c(3,4)
#         else
#             right.cols <- ncol(chart.matrix) %in% c(4,5)
#
#         if (!right.cols)
#             stop(paste("The input data frame must have between ", ifelse(!is.bubble, "3 and 4 ", "4 and 5 "), "columns", sep = ""))
#
#         ## Check that the class of the input is a data frame
#         if (!is.data.frame(chart.matrix))
#             stop(paste("The input data must be a data frame with ", ifelse(!is.bubble, "4", "5"), " columns for a ",
#                        ifelse(!is.bubble, "scatterplot", "bubbleplot"), " where the first ",
#                        ifelse(!is.bubble, " and second ", ", second and third "),
#                        "are numeric, the ", ifelse(!is.bubble, "third ", "fourth "), "contains the labels, and an optional ",
#                        ifelse(!is.bubble, "fourth ", "fifth "), "contains groupings/series.", sep = ""))
#
#         ## Any factor columns need to be converted to character
#         for (n in 1:ncol(chart.matrix))
#         {
#             if(is.factor(chart.matrix[, n]))
#                 chart.matrix[, n] <- as.character(chart.matrix[, n])
#             else
#                 chart.matrix[, n] <- chart.matrix[, n]
#         }
#
#         ## Columns need to be numeric with one compulsory character column (labels) and one optional character column.
#         class.check <- c(is.numeric(chart.matrix[, 1]),
#                          is.numeric(chart.matrix[, 2]),
#                          ifelse(is.bubble, is.numeric(chart.matrix[, 3]), is.character(chart.matrix[, 3])),
#                          ifelse(ncol(chart.matrix) >= 4, is.character(chart.matrix[, 4]), FALSE),
#                          ifelse(ncol(chart.matrix) > 4 && is.bubble, is.character(chart.matrix[, 5]), FALSE))
#
#         if (ncol(chart.matrix) != sum(class.check))
#             stop("Please ensure that your data frame has ", ifelse(!is.bubble, "two ", "three "),
#                  "numeric columns, and at least one character column, in that order.", sep = "")
#
#         ## duplicate row names/labels are allowed if there are groups, but not if there are no groups
#         if (ncol(chart.matrix) == 3 && !is.bubble)
#             if (unique(chart.matrix[, 3]) != nrow(chart.matrix))
#                 stop("Your row labels must be unique.")
#
#         if (ncol(chart.matrix) == 4 && is.bubble)
#             if (unique(chart.matrix[, 4]) != nrow(chart.matrix))
#                 stop("Your row labels must be unique")
#
#         ## Generate suitable output
#         if (!is.null(span.labels))
#             use.text <- span.labels
#
#         if (!is.bubble && ncol(chart.matrix) == 4 && is.null(span.labels))
#             use.text <- chart.matrix[, 4]
#
#         if (is.bubble && ncol(chart.matrix) == 5 && is.null(span.labels))
#             use.text <- chart.matrix[, 5]
#
#         groups <- rep(use.text, each = nrow(chart.matrix) / length(use.text))
#
#         if (is.bubble)
#             get.col <- 3
#         else
#             get.col <- 2
#
#         temp <- as.matrix(cbind(chart.matrix[, 1:get.col]))
#         rownames(temp) <- chart.matrix[, (get.col + 1)]
#     }
#
#     return(list(chart.matrix = temp, groups = groups))
# }
