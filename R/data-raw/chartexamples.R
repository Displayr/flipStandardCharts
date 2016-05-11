make.table <- function(x, y, label = "Series", row = TRUE)
{
    output <- table(x,y)
    if (row)
        rownames(output) <- paste("Series", 1:length(x))
    else
        colnames(output) <- paste("Series", 1:length(y))

    output
}

x.data = c("A","B","C","D","E")

y.data = c(1, 2, 3, 4, 5)

var1 <- c(1, 2, 3, 4, 5)

var2 <- c(5, 4, 3, 2, 1)

var3 <- c(3, 3, 3, 3, 3)

var4 <- c(3, 4, 5, 2, 1)

var5 <- c(5, 4, 2, 3, 1)

alpha.five <- LETTERS[1:5]

logic.vector <- c(TRUE, FALSE, TRUE, TRUE, FALSE)

logic.vector.named <- c(A = TRUE, B = FALSE, C = TRUE, D = TRUE, E = FALSE)

named.vector.a <- c("A" = 1, "B" = 2, "C" = 3)

named.vector.b <- c("D" = 3, "E" = 2, "F" = 1)

factor.a <- factor(x.data)

factor.b <- factor(y.data)

x.dates <- c(1440236400000,1450236400000,1460236400000,1470236400000,1480236400000)
x.dates <- as.POSIXct(x.dates/1000, origin = "1970-01-01")

z <- matrix(1:5, ncol = 1, dimnames = list(x = LETTERS[1:5], series = "Series 1"))

good.examples <- list("A named vector becomes a ChartMatrix" = list(Y = c("A" = 1, "B" = 2, "C" = 3, "D" = 4, "E" = 5), X = NULL, transpose = FALSE, n.row = 1, n.columns = 5),
                      "A single column table becomes a chart matrix" = list(X = NULL, Y = make.table(x.data, var3), transpose = FALSE, n.row = 1, n.columns = 5),
                      "A numeric matrix with one column becomes a chart matrix" = list(X = NULL, Y = z, transpose = FALSE, n.row = 1, n.columns = 5),
                      "A table with one row becomes a chart matrix" = list(X = NULL, Y = make.table(var3, x.data, row = FALSE), transpose = TRUE, n.row = 1, n.columns = 5),
                      "One numeric or integer vector and one character vector become a chart matrix" = list(X = x.data, Y = y.data, transpose = TRUE, n.row = 1, n.columns = 5),
                      "One numeric or integer vector and one factor vector become a chart matrix" = list(Y = y.data, X = factor(x.data), transpose = TRUE, n.row = 1, n.columns = 5),
                      "One numeric or integer vector and one ordered factor vector become a chart matrix" = list(Y = y.data, X = as.ordered(x.data), transpose = TRUE, n.row = 1, n.columns = 5),
                      "One or more numeric or integer vector(s) in a list and one character vector become a chart matrix" = list(Y = list(A = var1, B = var1, C = var1, D = var1, E = var1), X = alpha.five, transpose = TRUE, n.row = 5, n.columns = 5),
                      "One or more numeric or integer vector(s) in a data frame and one character vector become a chart matrix" = list(Y = cbind(var1, var1, var1, var1, var1), X = alpha.five, transpose = TRUE, n.row = 5, n.columns = 5),
                      "One or more factors with the same levels become a chart matrix" = list(X = factor.a, Y = factor.a, transpose = TRUE, n.row = 5, n.columns = 5),
                      "One numeric or integer variable and one date variable become a chart matrix" = list(Y = var1, X = x.dates, transpose = TRUE, n.row = 1, n.columns = 5))

bad.examples <- list("Y cannot take an unnamed numeric vector without an X input" = list(X = NULL, Y = y.data, transpose = FALSE, n.row = 1, n.columns = 5),
                     "Y cannot take an unnamed logic vector regardless of X-value" = list(X = NULL, Y = logic.vector, transpose = FALSE, n.row = 1, n.columns = 5),
                     "Y cannot take a list of logic vectors (unnamed)" = list(X = NULL, Y = list(logic.vector, logic.vector, logic.vector), transpose = FALSE, n.row = 1, n.columns = 3),
                     "Y cannot take a list of logic vectors (named)" = list(X = NULL, Y = list(logic.vector.named, logic.vector.named, logic.vector.named), transpose = FALSE, n.row = 1, n.columns = 3),
                     "Y cannot take a list of differently named vectors" = list(X = NULL, Y = list(named.vector.a, named.vector.b), transpose = FALSE, n.row = 1, n.columns = 2),
                     "Y cannot take a list of character and integer vectors" = list(X = NULL, Y = list(var1, x.data), transpose = FALSE, n.row = 1, n.columns = 2),
                     "Y cannot take a data frame of mixed character and integer vectors" = list(X = NULL, Y = cbind(var1, x.data), transpose = FALSE, n.row = 2, n.columns = 5),
                     "Y cannot take a list of multiple factors" = list(X = NULL, Y = list(factor.a, factor.b), transpose = FALSE, n.row = 1, n.columns = 2),
                     "Y cannot take a list of mixed integer vectors and factors" = list(X = NULL, Y = list(factor.a, y.data), transpose = FALSE, n.row = 1, n.columns = 2),
                     "Y cannot take a list of mixed character vectors and factors" = list(X = NULL, Y = list(factor.a, x.data), transpose = FALSE, n.row = 1, n.columns = 2),
                     "Y cannot take a character matrix" = list(X = NULL, Y = matrix(LETTERS[1:3]), transpose = FALSE, n.row = 1, n.columns = 3),
                     "Y cannot take a logic matrix" = list(X = NULL, Y = matrix(rep(c(TRUE,FALSE),3)), transpose = FALSE, n.row = 1, n.columns = 6),
                     "Y cannot take a character vector" = list(X = NULL, Y = LETTERS[1:5], transpose = FALSE, n.row = 1, n.columns = 5),
                     "Y cannot take a list of multiple character vectors" = list(X = NULL, Y = list(LETTERS[1:5], LETTERS[6:10], LETTERS[11:15]), transpose = FALSE, n.row = 1, n.columns = 3))

errorAsChartMatrix.examples <- list("Y cannot take a data frame of logic vectors (unnamed)" = list(X = NULL, Y = data.frame(cbind(logic.vector, logic.vector, logic.vector))),
                                    "Y cannot take a data frame of multiple character vectors" = list(X = NULL, Y = data.frame(cbind(LETTERS[1:5], LETTERS[6:10], LETTERS[11:15]))),
                                    "Y cannot take a data frame of mixed integer vectors and factors" = list(X = NULL, Y = data.frame(cbind(factor.a, x.data))),
                                    "Y cannot take a data frame of mixed integer vectors and factors" = list(X = NULL, Y = data.frame(cbind(factor.a, y.data))),
                                    "Y cannot take a data frame of multiple factors" = list(X = NULL, Y = data.frame(factor.a, factor.b)),
                                    "Y cannot take a data frame of differently named vectors" = list(X = NULL, Y = data.frame(cbind(named.vector.a, named.vector.b))),
                                    "Y cannot take a data frame of logic vectors (named)" = list(X = NULL, Y = data.frame(cbind(logic.vector.named, logic.vector.named, logic.vector.named))),
                                    "X cannot take a logic vector" = list(Y = var1, X = logic.vector, n.row = 1, n.columns = 5),
                                    "X cannot take a data frame" = list(Y = var1, X = data.frame(cbind(var1, var2, var3))),
                                    "X cannot take a list" = list(Y = var1, X = list(var1, var2, var3)))

errorIsChartMatrix.examples <- list("Y cannot take a named logic vector" = list(X = NULL, Y = logic.vector.named, transpose = FALSE, n.row = 1, n.columns = 5))

qColors <- c(grDevices::rgb(91, 155, 213, 255, maxColorValue = 255), # blue
             grDevices::rgb(237, 125, 49, 255, maxColorValue = 255), # orange
             grDevices::rgb(165, 165, 165, 255, maxColorValue = 255), # grey
             grDevices::rgb(30, 192, 0, 255, maxColorValue = 255), # yellow
             grDevices::rgb(68, 114, 196, 255, maxColorValue = 255), # darker blue
             grDevices::rgb(112, 173, 71, 255, maxColorValue = 255), # green
             grDevices::rgb(37, 94, 145, 255, maxColorValue = 255), # even darker blue
             grDevices::rgb(158, 72, 14, 255, maxColorValue = 255), # blood
             grDevices::rgb(99, 99, 99, 255, maxColorValue = 255), # dark grey
             grDevices::rgb(153, 115, 0, 255, maxColorValue = 255), # brown
             grDevices::rgb(38, 68, 120, 255, maxColorValue = 255), # very dark blue
             grDevices::rgb(67, 104, 43, 255, maxColorValue = 255), # darker green
             grDevices::rgb(255, 255, 255, 255, maxColorValue = 255), # black
             grDevices::rgb(255, 35, 35, 255, maxColorValue = 255)) # red

plotlySymbols <- plotlySymbols <- c(0,100,200,300,1,101,201,301,2,102,202,302,3,103,203,303,4,104,204,304,5,105,205,305,6,106,206,306,7,107,207,307,8,108,208,308,9,109,209,309,10,110,210,310,11,111,211,311,12,112,212,312,13,113,213,313,14,114,214,314,15,115,215,315,16,116,216,316,17,117,217,317,18,118,218,318,19,119,219,319,20,120,220,320,21,121,221,321,22,122,222,322,23,123,223,323,24,124,224,324,25,125,26,126,27,127,28,128,29,129,30,130,31,131,32,132,33,133,34,134,35,135,36,136,37,137,38,138,39,139,40,140,41,141,42,142,43,143,44,144)

available.fonts <- c("Arial Black", "Arial", "Comic Sans MS", "Courier New", "Georgia", "Impact", "Lucida Console", "Lucida Sans Unicode", "Marlett", "Symbol", "Tahoma", "Times New Roman", "Trebuchet MS", "Verdana", "Webdings")


devtools::use_data(qColors, plotlySymbols, available.fonts, x.data, y.data, var1, var2, var3, var4, var5, alpha.five, logic.vector, logic.vector.named, named.vector.a, named.vector.b, factor.a, factor.b, x.dates, z, good.examples, bad.examples, errorAsChartMatrix.examples, errorIsChartMatrix.examples, internal = FALSE, overwrite = TRUE)
devtools::use_data(qColors, plotlySymbols, available.fonts, internal = TRUE, overwrite = TRUE)
