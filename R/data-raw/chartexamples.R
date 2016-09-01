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

weight5 <- c(2, 1.5, 1, 0.5, 0.1)
weight3 <- c(2, 1, 0.1)

x.dates <- c(1440236400000,1450236400000,1460236400000,1470236400000,1480236400000)
x.dates <- as.POSIXct(x.dates/1000, origin = "1970-01-01")

z <- matrix(1:5, ncol = 1, dimnames = list(x = LETTERS[1:5], series = "Series 1"))

character.matrix <- matrix(LETTERS[1:20], ncol = 2)
colnames(character.matrix) <- LETTERS[1:2]
rownames(character.matrix) <- LETTERS[1:10]

good.examples <- list("1 A named vector becomes a ChartMatrix" = list(Y = c("A" = 1, "B" = 2, "C" = 3, "D" = 4, "E" = 5), X = NULL, transpose = FALSE, n.row = 1, n.columns = 5),
                      "2 A single column table becomes a chart matrix" = list(X = NULL, Y = make.table(x.data, var3), transpose = FALSE, n.row = 1, n.columns = 5),
                      "3 A numeric matrix with one column becomes a chart matrix" = list(X = NULL, Y = z, transpose = FALSE, n.row = 1, n.columns = 5),
                      "4 A table with one row becomes a chart matrix" = list(X = NULL, Y = make.table(var3, x.data, row = FALSE), transpose = TRUE, n.row = 1, n.columns = 5),
                      "5 One numeric or integer vector and one character vector become a chart matrix" = list(X = x.data, Y = y.data, transpose = TRUE, n.row = 1, n.columns = 5),
                      "6 One numeric or integer vector and one factor vector become a chart matrix" = list(Y = y.data, X = factor(x.data), transpose = TRUE, n.row = 1, n.columns = 5),
                      "7 One numeric or integer vector and one ordered factor vector become a chart matrix" = list(Y = y.data, X = as.ordered(x.data), transpose = TRUE, n.row = 1, n.columns = 5),
                      "8 One or more numeric or integer vector(s) in a list and one character vector become a chart matrix" = list(Y = list(A = var1, B = var1, C = var1, D = var1, E = var1), X = alpha.five, transpose = TRUE, n.row = 5, n.columns = 5),
                      "9 One or more numeric or integer vector(s) in a data frame and one character vector become a chart matrix" = list(Y = cbind(var1, var1, var1, var1, var1), X = alpha.five, transpose = TRUE, n.row = 5, n.columns = 5),
                      "10 One or more factors with the same levels become a chart matrix" = list(X = factor.a, Y = factor.a, transpose = TRUE, n.row = 5, n.columns = 5),
                      "11 One numeric or integer variable and one date variable become a chart matrix" = list(Y = var1, X = x.dates, transpose = TRUE, n.row = 1, n.columns = 5, aggregate.period = "month"),
                      "12 X can take a logic vector" = list(Y = var1, X = logic.vector, n.row = 1, n.columns = 2),
                      "13 Filtered: A named vector becomes a ChartMatrix" = list(Y = c("A" = 1, "B" = 2, "C" = 3, "D" = 4, "E" = 5), X = NULL, transpose = FALSE, n.row = 1, n.columns = 3, subset = logic.vector),
                      "14 Filtered: A single column table becomes a chart matrix" = list(X = NULL, Y = make.table(x.data, var3), transpose = FALSE, n.row = 1, n.columns = 3, subset = logic.vector),
                      "15 Filtered: A numeric matrix with one column becomes a chart matrix" = list(X = NULL, Y = z, transpose = FALSE, n.row = 1, n.columns = 3, subset = logic.vector),
                      "16 Filtered: One numeric or integer vector and one character vector become a chart matrix" = list(X = x.data, Y = y.data, transpose = TRUE, n.row = 1, n.columns = 3, subset = logic.vector),
                      "17 Filtered: One numeric or integer vector and one factor vector become a chart matrix" = list(Y = y.data, X = factor(x.data), transpose = TRUE, n.row = 1, n.columns = 3, subset = logic.vector),
                      "18 Filtered: One numeric or integer vector and one ordered factor vector become a chart matrix" = list(Y = y.data, X = as.ordered(x.data), transpose = TRUE, n.row = 1, n.columns = 3, subset = logic.vector),
                      "19 Filtered: One or more numeric or integer vector(s) in a list and one character vector become a chart matrix" = list(Y = list(A = var1, B = var1, C = var1, D = var1, E = var1), X = alpha.five, transpose = TRUE, n.row = 5, n.columns = 3, subset = logic.vector),
                      "20 Filtered: One or more numeric or integer vector(s) in a data frame and one character vector become a chart matrix" = list(Y = cbind(var1, var1, var1, var1, var1), X = alpha.five, transpose = TRUE, n.row = 5, n.columns = 3, subset = logic.vector),
                      "21 Filtered: One or more factors with the same levels become a chart matrix" = list(X = factor.a, Y = factor.a, transpose = TRUE, n.row = 5, n.columns = 5, subset = logic.vector),
                      "22 Filtered: One numeric or integer variable and one date variable become a chart matrix" = list(Y = var1, X = x.dates, transpose = TRUE, n.row = 1, n.columns = 3, aggregate.period = "month", subset = logic.vector))

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
                                    "X cannot take a data frame" = list(Y = var1, X = data.frame(cbind(var1, var2, var3))),
                                    "X cannot take a list" = list(Y = var1, X = list(var1, var2, var3)))

errorIsChartMatrix.examples <- list("Y cannot take a named logic vector" = list(X = NULL, Y = logic.vector.named, transpose = FALSE, n.row = 1, n.columns = 5))

qTab.examples <- list("ex1 - Area - Pick One by Pick One" = list(y = structure(c(0, 22, 18, 15, 16, 19, 13, 18, 27, 12, 0, 22, 21, 18, 20, 16, 14, 22, 24, 10),
                                                                  .Dim = c(10L, 2L), statistic = "n", .Dimnames = list(c("Less than 18", "18 to 24",
                                                                  "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
                                                                  "65 or more"), c("Male", "Female")), name = "Y.Pick.One.by.X.Pick.One.Area.Chart", questions =
                                                                   c("Q3. Age", "Q2. Gender")), transpose = TRUE, type = "Area",
                                                                   subtitle.text = NULL),
                      "ex2 - Area - Y Number Multi" = list(y = structure(c(2.98165137614679, 4.11009174311927, 3.07339449541284,
                                                              2.63302752293578, 3.34862385321101, 2.45565749235474, 3.40366972477064,
                                                              3.52905198776758, 4.02752293577982, 2.28440366972477), .Dim = 10L, statistic = "Average", .Dimnames = list(
                                                              c("My friends would describe me as cultured, and refined",
                                                              "I think it is important to be honest when giving complements",
                                                              "I can be a little naive at times", "I am the life of the party",
                                                              "I am relaxed most of the time and not easily worried", "Living in a big city is important to me",
                                                              "I think it is important to follow and maintain traditions",
                                                              "I enjoy being attractive to the opposite sex", "I am young at heart",
                                                              "I follow all the latest fashions")), name = "Y.Number.Multi.Area.Chart", questions = c("Q25. Respondent image (number multi)",
                                                              "SUMMARY")), transpose = FALSE, type = "Area",
                                                               subtitle.text = NULL),
                      "ex3 - Area - Y Number by Pick One" = list(y = structure(c(2.92537313432836, 3.01666666666667, 3.02380952380952,
                                                              3.08, 3.22222222222222, 2.91666666666667), .Dim = c(1L, 6L), statistic = "Average", .Dimnames = list(
                                                              "My friends would describe me as cultured, and refined",
                                                              c("Coca Cola", "Coke Zero", "Diet Coke", "Pepsi", "Pepsi Light",
                                                              "Pepsi Max")), name = "Y.Number.by.X.Pick.One.Area.Chart", questions = c("My friends would describe me as cultured, and refined",
                                                              "Global frequentCola - Categorical")), transpose = FALSE, type = "Area",
                                                               subtitle.text = NULL),
                      "ex4 - Area - Y Pick One by X Pick One for stacked example" = list(y = structure(c(0, 26, 17, 13, 20, 12, 6, 19, 14, 7, 0, 8, 10, 6,
                                                               6, 4, 8, 8, 8, 2, 0, 2, 3, 4, 8, 6, 3, 6, 8, 2, 0, 4, 0, 4, 1,
                                                               3, 3, 1, 7, 2, 0, 0, 1, 1, 0, 3, 2, 2, 4, 5, 0, 4, 8, 5, 1, 7,
                                                               5, 4, 10, 4), .Dim = c(10L, 6L), statistic = "n", .Dimnames = list(
                                                               c("Less than 18", "18 to 24", "25 to 29", "30 to 34", "35 to 39",
                                                               "40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more"
                                                               ), c("Coca Cola", "Coke Zero", "Diet Coke", "Pepsi", "Pepsi Light",
                                                               "Pepsi Max")), name = "Y.Pick.One.by.X.Pick.One.stacked.Area.Chart", questions = c("Q3. Age",
                                                               "Global frequentCola - Categorical")), transpose = TRUE, type = "Stacked Area",
                                                               subtitle.text = NULL),
                      "ex5 - Area - Y Pick One by X Pick One for 100pct stacked example" = list(y = structure(c(0, 26, 17, 13, 20, 12, 6, 19, 14, 7, 0, 8, 10, 6,
                                                               6, 4, 8, 8, 8, 2, 0, 2, 3, 4, 8, 6, 3, 6, 8, 2, 0, 4, 0, 4, 1,
                                                               3, 3, 1, 7, 2, 0, 0, 1, 1, 0, 3, 2, 2, 4, 5, 0, 4, 8, 5, 1, 7,
                                                               5, 4, 10, 4), .Dim = c(10L, 6L), statistic = "n", .Dimnames = list(
                                                               c("Less than 18", "18 to 24", "25 to 29", "30 to 34", "35 to 39",
                                                               "40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more"
                                                               ), c("Coca Cola", "Coke Zero", "Diet Coke", "Pepsi", "Pepsi Light",
                                                               "Pepsi Max")), name = "Y.Pick.One.by.X.Pick.One.100pct.stacked.Area.Chart", questions = c("Q3. Age",
                                                               "Global frequentCola - Categorical")), transpose = TRUE, type = "100% Stacked Area",
                                                               subtitle.text = NULL),
                      "ex6 - Area - R-generated named table" = list(y = structure(c(0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 1L,
                                                               0L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L), .Dim = c(5L,
                                                               5L), .Dimnames = structure(list(var1 = c("1", "2", "3", "4",
                                                               "5"), var2 = c("1", "2", "3", "4", "5")), .Names = c("var1",
                                                               "var2")), class = "table", name = "R-generated.named.table"), type = "Area", transpose = FALSE,
                                                               subtitle.text = NULL),
                      "ex7 - Area - R-generated named numeric matrix" = list(y = structure(c(1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L), .Dim = c(5L,
                                                               2L), .Dimnames = list(c("A", "B", "C", "D", "E"), c("Series 1",
                                                               "Series 2")), name = "R-generated.named.numeric.matrix"), type = "Area", transpose = TRUE,
                                                               subtitle.text = NULL),
                      "ex8 - Area - R-generated named numeric data frame" = list(y = structure(list(A = c(4L, 10L, 8L, 1L, 5L, 6L, 9L, 2L, 3L, 7L),
                                                               B = c(8L, 3L, 2L, 4L, 10L, 7L, 5L, 1L, 6L, 9L)), .Names = c("A",
                                                               "B"), row.names = c("A", "B", "C", "D", "E", "F", "G", "H", "I",
                                                               "J"), name = "R-generated.named.numeric.data.frame", class = "data.frame"), type = "Area", transpose = TRUE,
                                                               subtitle.text = NULL),
                      "ex9 - Line - Pick One by Pick One" = list(y = structure(c(0, 22, 18, 15, 16, 19, 13, 18, 27, 12, 0, 22, 21, 18, 20, 16, 14, 22, 24, 10),
                                                                  .Dim = c(10L, 2L), statistic = "n", .Dimnames = list(c("Less than 18", "18 to 24",
                                                                  "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
                                                                  "65 or more"), c("Male", "Female")), name = "Y.Pick.One.by.X.Pick.One.Line.Chart", questions =
                                                                   c("Q3. Age", "Q2. Gender")), transpose = TRUE, type = "Line",
                                                               subtitle.text = NULL),
                      "ex10 - Line - Y Number Multi" = list(y = structure(c(2.98165137614679, 4.11009174311927, 3.07339449541284,
                                                              2.63302752293578, 3.34862385321101, 2.45565749235474, 3.40366972477064,
                                                              3.52905198776758, 4.02752293577982, 2.28440366972477), .Dim = 10L, statistic = "Average", .Dimnames = list(
                                                              c("My friends would describe me as cultured, and refined",
                                                              "I think it is important to be honest when giving complements",
                                                              "I can be a little naive at times", "I am the life of the party",
                                                              "I am relaxed most of the time and not easily worried", "Living in a big city is important to me",
                                                              "I think it is important to follow and maintain traditions",
                                                              "I enjoy being attractive to the opposite sex", "I am young at heart",
                                                              "I follow all the latest fashions")), name = "Y.Number.Multi.Line.Chart", questions = c("Q25. Respondent image (number multi)",
                                                              "SUMMARY")), transpose = FALSE, type = "Line",
                                                               subtitle.text = NULL),
                      "ex11 - Line - Y Number by Pick One" = list(y = structure(c(2.92537313432836, 3.01666666666667, 3.02380952380952,
                                                              3.08, 3.22222222222222, 2.91666666666667), .Dim = c(1L, 6L), statistic = "Average", .Dimnames = list(
                                                              "My friends would describe me as cultured, and refined",
                                                              c("Coca Cola", "Coke Zero", "Diet Coke", "Pepsi", "Pepsi Light",
                                                              "Pepsi Max")), name = "Y.Number.by.X.Pick.One.Line.Chart", questions = c("My friends would describe me as cultured, and refined",
                                                              "Global frequentCola - Categorical")), transpose = FALSE, type = "Line",
                                                               subtitle.text = NULL),
                      "ex12 - Line - R-generated named table" = list(y = structure(c(0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 1L,
                                                               0L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L), .Dim = c(5L,
                                                               5L), .Dimnames = structure(list(var1 = c("1", "2", "3", "4",
                                                               "5"), var2 = c("1", "2", "3", "4", "5")), .Names = c("var1",
                                                               "var2")), class = "table", name = "R-generated.named.table"), type = "Line", transpose = FALSE,
                                                               subtitle.text = NULL),
                      "ex13 - Line - R-generated named numeric matrix" = list(y = structure(c(1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L), .Dim = c(5L,
                                                               2L), .Dimnames = list(c("A", "B", "C", "D", "E"), c("Series 1",
                                                               "Series 2")), name = "R-generated.named.numeric.matrix"), type = "Line", transpose = TRUE,
                                                               subtitle.text = NULL),
                      "ex14 - Line - R-generated named numeric data frame" = list(y = structure(list(A = c(4L, 10L, 8L, 1L, 5L, 6L, 9L, 2L, 3L, 7L),
                                                               B = c(8L, 3L, 2L, 4L, 10L, 7L, 5L, 1L, 6L, 9L)), .Names = c("A",
                                                               "B"), row.names = c("A", "B", "C", "D", "E", "F", "G", "H", "I",
                                                               "J"), name = "R-generated.named.numeric.data.frame", class = "data.frame"), type = "Line", transpose = TRUE,
                                                               subtitle.text = NULL),
                      "ex15 - Area - R-generated named numeric data frame with subtitle" = list(y = structure(list(A = c(4L, 10L, 8L, 1L, 5L, 6L, 9L, 2L, 3L, 7L),
                                                               B = c(8L, 3L, 2L, 4L, 10L, 7L, 5L, 1L, 6L, 9L)), .Names = c("A",
                                                               "B"), row.names = c("A", "B", "C", "D", "E", "F", "G", "H", "I",
                                                               "J"), name = "R-generated.named.numeric.data.frame", class = "data.frame"), type = "Area", transpose = TRUE,
                                                               subtitle.text = "in magnis siquidem corporibus aut certe maioribus facilis officina sequaci<br>materia fuit: in his tam parvis atque tam nullis..."))

qTab.bad.examples <- list("Cannot take unnamed matrix" = list(y = structure(c(0, 22, 18, 15, 16, 19, 13, 18, 27, 12, 0, 22, 21, 18, 20, 16, 14, 22, 24, 10),
                                                                  .Dim = c(10L, 2L), statistic = "n", name = "Y.Pick.One.by.X.Pick.One.Area.Chart", questions =
                                                                   c("Q3. Age", "Q2. Gender")), transpose = TRUE, type = "Area"),
                          "Cannot take a numeric vector" = list(y = sample(1:10, 10), transpose = FALSE, type = "Area"),
                          "Cannot take a character vector" = list(y = sample(LETTERS[1:10], 10), transpose = FALSE, type = "Area"),
                          "Cannot take a character matrix" = list(y = character.matrix, transpose = FALSE, type = "Area"),
                          "Cannot take unnamed matrix" = list(y = structure(c(0, 22, 18, 15, 16, 19, 13, 18, 27, 12, 0, 22, 21, 18, 20, 16, 14, 22, 24, 10),
                                                                  .Dim = c(10L, 2L), statistic = "n", name = "Y.Pick.One.by.X.Pick.One.Area.Chart", questions =
                                                                   c("Q3. Age", "Q2. Gender")), transpose = TRUE, type = "Line"),
                          "Cannot take a numeric vector" = list(y = sample(1:10, 10), transpose = FALSE, type = "Line"),
                          "Cannot take a character vector" = list(y = sample(LETTERS[1:10], 10), transpose = FALSE, type = "Line"),
                          "Cannot take a character matrix" = list(y = character.matrix, transpose = FALSE, type = "Line"))

### To create diffs between Plotly chart objects we need to store the approved examples.
### Run the below code whenever a change has been made that you're happy with and you've experienced a diff.
######  For each plotly plot, there are two lists created:
library(plotly)

replace_zero_length <- function(x) {
  lapply(x, function(x) {
    if (is.list(x)){
      replace_null(x)
      } else{
        if(length(x) == 0) "" else(x)
      }
    })
}

replace_null <- function(x) {
  lapply(x, function(x) {
    if (is.list(x)){
      replace_null(x)
      } else{
        if(is.null(x)) "" else(x)
      }
    })
}

for (i in 1:length(qTab.examples))
{
    example.number <- i
    assign(paste("ex", i, ".1", sep = ""), as.list(plotly_build(Chart(y = qTab.examples[[example.number]]$y, type = qTab.examples[[example.number]]$type, transpose = qTab.examples[[example.number]]$transpose, title = attr(qTab.examples[[example.number]]$y, "name"), subtitle.text = qTab.examples[[example.number]]$subtitle.text))[[1]]))
    assign(paste("ex", i, ".2", sep = ""), as.list(plotly_build(Chart(y = qTab.examples[[example.number]]$y, type = qTab.examples[[example.number]]$type, transpose = qTab.examples[[example.number]]$transpose, title = attr(qTab.examples[[example.number]]$y, "name"), subtitle.text = qTab.examples[[example.number]]$subtitle.text))[[2]]))
}

ex1.1 <- replace_zero_length(ex1.1)
ex2.1 <- replace_zero_length(ex2.1)
ex3.1 <- replace_zero_length(ex3.1)
ex4.1 <- replace_zero_length(ex4.1)
ex5.1 <- replace_zero_length(ex5.1)
ex6.1 <- replace_zero_length(ex6.1)
ex7.1 <- replace_zero_length(ex7.1)
ex8.1 <- replace_zero_length(ex8.1)
ex9.1 <- replace_zero_length(ex9.1)
ex10.1 <- replace_zero_length(ex10.1)
ex11.1 <- replace_zero_length(ex11.1)
ex12.1 <- replace_zero_length(ex12.1)
ex13.1 <- replace_zero_length(ex13.1)
ex14.1 <- replace_zero_length(ex14.1)
ex15.1 <- replace_zero_length(ex15.1)
ex1.2 <- replace_zero_length(ex1.2)
ex2.2 <- replace_zero_length(ex2.2)
ex3.2 <- replace_zero_length(ex3.2)
ex4.2 <- replace_zero_length(ex4.2)
ex5.2 <- replace_zero_length(ex5.2)
ex6.2 <- replace_zero_length(ex6.2)
ex7.2 <- replace_zero_length(ex7.2)
ex8.2 <- replace_zero_length(ex8.2)
ex9.2 <- replace_zero_length(ex9.2)
ex10.2 <- replace_zero_length(ex10.2)
ex11.2 <- replace_zero_length(ex11.2)
ex12.2 <- replace_zero_length(ex12.2)
ex13.2 <- replace_zero_length(ex13.2)
ex14.2 <- replace_zero_length(ex14.2)
ex15.2 <- replace_zero_length(ex15.2)

ex1.1 <- replace_null(ex1.1)
ex2.1 <- replace_null(ex2.1)
ex3.1 <- replace_null(ex3.1)
ex4.1 <- replace_null(ex4.1)
ex5.1 <- replace_null(ex5.1)
ex6.1 <- replace_null(ex6.1)
ex7.1 <- replace_null(ex7.1)
ex8.1 <- replace_null(ex8.1)
ex9.1 <- replace_null(ex9.1)
ex10.1 <- replace_null(ex10.1)
ex11.1 <- replace_null(ex11.1)
ex12.1 <- replace_null(ex12.1)
ex13.1 <- replace_null(ex13.1)
ex14.1 <- replace_null(ex14.1)
ex15.1 <- replace_null(ex15.1)
ex1.2 <- replace_null(ex1.2)
ex2.2 <- replace_null(ex2.2)
ex3.2 <- replace_null(ex3.2)
ex4.2 <- replace_null(ex4.2)
ex5.2 <- replace_null(ex5.2)
ex6.2 <- replace_null(ex6.2)
ex7.2 <- replace_null(ex7.2)
ex8.2 <- replace_null(ex8.2)
ex9.2 <- replace_null(ex9.2)
ex10.2 <- replace_null(ex10.2)
ex11.2 <- replace_null(ex11.2)
ex12.2 <- replace_null(ex12.2)
ex13.2 <- replace_null(ex13.2)
ex14.2 <- replace_null(ex14.2)
ex15.2 <- replace_null(ex15.2)



##### End of repeated code for diffs setting




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
             grDevices::rgb(0, 0, 0, 255, maxColorValue = 255), # black
             grDevices::rgb(255, 35, 35, 255, maxColorValue = 255)) # red

plotlySymbols <- plotlySymbols <- c(0,100,200,300,1,101,201,301,2,102,202,302,3,103,203,303,4,104,204,304,5,105,205,305,6,106,206,306,7,107,207,307,8,108,208,308,9,109,209,309,10,110,210,310,11,111,211,311,12,112,212,312,13,113,213,313,14,114,214,314,15,115,215,315,16,116,216,316,17,117,217,317,18,118,218,318,19,119,219,319,20,120,220,320,21,121,221,321,22,122,222,322,23,123,223,323,24,124,224,324,25,125,26,126,27,127,28,128,29,129,30,130,31,131,32,132,33,133,34,134,35,135,36,136,37,137,38,138,39,139,40,140,41,141,42,142,43,143,44,144)

available.fonts <- c("Arial Black", "Arial", "Comic Sans MS", "Courier New", "Georgia", "Impact", "Lucida Console", "Lucida Sans Unicode", "Marlett", "Symbol", "Tahoma", "Times New Roman", "Trebuchet MS", "Verdana", "Webdings")

devtools::use_data(qTab.examples, qTab.bad.examples, character.matrix, qColors, plotlySymbols, available.fonts, x.data, y.data, var1, var2, var3, var4, var5, alpha.five, logic.vector, logic.vector.named, named.vector.a, named.vector.b, factor.a, factor.b, x.dates, z, good.examples, bad.examples, errorAsChartMatrix.examples, errorIsChartMatrix.examples,
                   ex1.1, ex2.1, ex3.1, ex4.1, ex5.1, ex6.1, ex7.1, ex8.1, ex9.1, ex10.1, ex11.1, ex12.1, ex13.1, ex14.1, ex15.1, ex1.2, ex2.2, ex3.2, ex4.2, ex5.2, ex6.2, ex7.2, ex8.2, ex9.2, ex10.2, ex11.2, ex12.2, ex13.2, ex14.2, ex15.2, internal = FALSE, overwrite = TRUE)
devtools::use_data(qColors, plotlySymbols, available.fonts, internal = TRUE, overwrite = TRUE)
