removeRowsAndColumns <- function(chart.matrix, rows.to.ignore = "", cols.to.ignore = "")
{
    ## Get the individual headings from the input
    remove.rows <- as.vector(sapply(strsplit(rows.to.ignore, ","), function(x) gsub("^\\s+|\\s+$", "", x)))
    remove.cols <- as.vector(sapply(strsplit(cols.to.ignore, ","), function(x) gsub("^\\s+|\\s+$", "", x)))

    ## Coerce to a df to get colnames
    chart.matrix <- as.data.frame(chart.matrix)

    ## Get rid of the pertinent bits
    chart.matrix <- chart.matrix[!rownames(chart.matrix) %in% remove.rows, ]
    chart.matrix <- chart.matrix[, !(colnames(chart.matrix) %in% remove.cols)]

    chart.matrix <- as.matrix(chart.matrix)

    return(chart.matrix)
}
