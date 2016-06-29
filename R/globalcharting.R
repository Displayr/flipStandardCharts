## Takes a matrix, and returns a matrix of either a cumulative sum, or a cumulative sum of percentages.
cum.data <- function(x, output = "cumulative.percentage") {
    if (output == "cumulative.sum"){
        x <- apply(x, 2, function(z) {cumsum(z)})
    } else if (output == "cumulative.percentage") {
        x <- apply(x, 2, function(z) {cumsum(prop.table(z))})
    } else if (output == "column.percentage") {
        x <- apply(x, 2, function(z) {prop.table(z)})
    }

    return(x)
}

## Takes a single string and puts <br> in place of the closest space preceding the n= value character.
## E.g. if n= 20 then count 20 characters.  The space preceding character 20 is replaced by "<br>".
lineBreakEveryN <- function(x, n = 21)
{
    start <- 1
    final <- NULL
    last.space <- 0

    for (i in 1:nchar(x[1]))
    {
        my.character <- substr(x, i, i)
        if (my.character == " ")
            last.space <- i

        if (i %% n == 0 && last.space > 0)
        {
            temp <- paste(substr(x, start, last.space - 1),"<br>",substr(x, last.space + 1, i), sep = "")
            final <- paste(final, temp, sep = "")
            start <- start + n
        }

        if (i == nchar(x[1]))
            final <- paste(final, substr(x, start, nchar(x[1])), sep = "")
    }

    final
}

autoFormatLongLabels <- function(x, n = 21)
{
    # Anything 60 characters or over gets an ellipsis from char 57 to char 60 and then nothing
    output.text <- sapply(x, function(x) {ifelse(nchar(x) > 60, paste(substr(x, 1, 57),"...", sep = ""), x)})

    # Anything over n characters gets <br> inserted in the closest space below n characters, e.g. at 20, 40, and 60 characters
    output.text <- sapply(output.text, function(x) lineBreakEveryN(x, n))

    # Don't need any attributes (names)
    attributes(output.text) <- NULL

    output.text
}

stripAlphaChannel <- function(hex.colors)
{
    if (nchar(hex.colors[1]) == 9)
        return(gsub("(FF|ff)$", "", hex.colors))
    else
        return(hex.colors)
}
