#' @importFrom flipTransformations TextAsVector AsNumeric
#' @importFrom grDevices colorRamp
scatterplotData <- function(chart.matrix,
                            type,
                            colors = NULL,
                            colors.reverse = FALSE,
                            colors.custom.color = NA,
                            colors.custom.gradient.start = NA,
                            colors.custom.gradient.end = NA,
                            colors.custom.palette = NA,
                            group.labels.text = "",
                            group.indices.text = "",
                            origin = FALSE,
                            transpose = FALSE,
                            rows.to.ignore = "",
                            cols.to.ignore = "",
                            legend.show = TRUE,
                            x.title = "",
                            y.title = "",
                            colorscale.variable = NULL)
{
    is.bubble <- type == "Labeled Bubbleplot"
    if (any(is.na(as.matrix(chart.matrix))))
    {
        warning("Data points with missing values have been omitted.")
        chart.matrix <- chart.matrix[!is.na(rowSums(chart.matrix)), ]
    }
    if (!is.null(group.indices.text) && group.indices.text[1] != "" && !is.null(colorscale.variable))
    {
        colorscale.variable <- NULL
        warning("Color-scale variable ignored when groups are provided\n")
    }
    
    # Remove rows and columns to ignore
    no.dimnames <- is.null(dimnames(chart.matrix))
    chart.matrix <- GetTidyTwoDimensionalArray(chart.matrix,
                                               row.names.to.remove = rows.to.ignore,
                                               column.names.to.remove = cols.to.ignore)
    if (no.dimnames)
        dimnames(chart.matrix) <- NULL

    pt.ord <- NULL
    if (!is.null(group.labels.text) && any(group.labels.text != ""))
    {
        if (!is.null(group.indices.text) && any(group.indices.text != ""))
        {
            group.labels.text[group.labels.text == ""] <- "Undefined"
            group.labels <- if (length(group.labels.text) > 1) group.labels.text
                            else TextAsVector(group.labels.text)
            group.indices <- if (is.numeric(group.indices.text)) group.indices.text
                             else as.numeric(TextAsVector(group.indices.text))

            if (length(group.labels) == 1 && length(unique(group.indices)) > 1)
                stop(paste0("Only one group has been specified: ", group.labels[1]))

            if (length(group.indices) != nrow(chart.matrix))
                stop(paste0("The number of group indices (", length(group.indices), ") needs to equal the number of rows in the table (", nrow(chart.matrix), ")."))

            permitted.indices <- 1:length(group.labels)
            if (any(is.na(group.indices)) || !all(group.indices %in% permitted.indices))
                stop(paste0("The group indices are not in the correct format."))

            group <- group.labels[group.indices]
            pt.ord <- order(group.indices)
        }
        else
            stop("Group labels were provided but group indices are missing.")
    }
    else
    {
        if (!is.null(group.indices.text) && any(group.indices.text != ""))
            stop("Group indices were provided but group labels are missing.")
        else
            group <- rep("Group", nrow(chart.matrix))
    }

    # order data points so that the color of groups are ordered
    if (!is.null(pt.ord))
    {
        chart.matrix <- chart.matrix[pt.ord,]
        group <- group[pt.ord]
        # colorscale.variable not compatible with groups
    }

    if (is.null(colors))
        colors <- "Default colors"
    if (!is.null(colorscale.variable))
        colorscale.variable <- AsNumeric(colorscale.variable, binary=F)
    num.colors <- if (!is.null(colorscale.variable)) 3
                  else                               length(unique(group))
    colors <- StripAlphaChannel(ChartColors(number.colors.needed = num.colors,
                                            given.colors = colors,
                                            custom.color = colors.custom.color,
                                            custom.gradient.start = colors.custom.gradient.start,
                                            custom.gradient.end = colors.custom.gradient.end,
                                            custom.palette = colors.custom.palette,
                                            reverse = colors.reverse,
                                            trim.light.colors = TRUE))
    if (!is.null(colorscale.variable))
    {
        col.fun <- colorRamp(colors)
        group <- 1:length(colorscale.variable)
        sc.vals <- (colorscale.variable - min(colorscale.variable, na.rm=T))/diff(range(colorscale.variable, na.rm=T))
        sc.tmp <- col.fun(sc.vals)
        sc.tmp[is.na(sc.tmp)] <- 204    # NAs turn grey
        colors <- rgb(sc.tmp, maxColorValue=255)
    }
    result <- list()
    result$x <- if (transpose) as.numeric(chart.matrix[, 2]) else as.numeric(chart.matrix[, 1])
    result$y <- if (transpose) as.numeric(chart.matrix[, 1]) else as.numeric(chart.matrix[, 2])
    result$z <- if (ncol(chart.matrix) >= 3) as.numeric(abs(chart.matrix[, 3])) else NULL
    result$colors <- colors

    #if (is.bubble && any(result$z < 0))
    #    stop("Negative values are present in the third column. No bubbles are shown for such cases.")

    result$label <- rownames(chart.matrix)
    result$group <- group
    result$origin <- origin
    result$legend.show <- is.null(colorscale.variable) && length(unique(result$group)) > 1 && legend.show
    result$legend.bubbles.show <- is.bubble

    # Resolve axes labels if none specified manually
    if (x.title == "" || length(x.title) == 0)
        x.title <- colnames(chart.matrix)[1]
    if (is.null(x.title) || x.title == "FALSE" || x.title == FALSE)
        x.title <- ""

    if (y.title == "" || length(y.title) == 0)
        y.title <- colnames(chart.matrix)[2]
    if (is.null(y.title) || y.title == "FALSE" || y.title == FALSE)
        y.title <- ""

    result$x.title <- if (transpose) y.title else x.title
    result$y.title <- if (transpose) x.title else y.title

    result
}

