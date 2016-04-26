context("AsChartMatrix")
#  rm(list=ls())

x.data = c("A","B","B","C","C")
y.data = c(0, 0, 0, 0, 0)
var1 <- c(1,2,3,4,5)
var2 <- c(5,4,3,2,1)
var3 <- c(3,3,3,3,3)
alpha.five <- LETTERS[1:5]
logic.vector <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
logic.vector.named <- c(A = TRUE, B = FALSE, C = TRUE, D = TRUE, E = FALSE)
named.vector.a <- c("A" = 1, "B" = 2, "C" = 3)
named.vector.b <- c("D" = 3, "E" = 2, "F" = 1)
factor.a <- factor(x.data)
factor.b <- factor(y.data)
x.dates <- c(1450236400000,1450236400000,1450332800000,1460332800000,1460419200000)
x.dates <- as.POSIXct(x.dates/1000, origin = "1970-01-01")


##### GOOD CASES ######
test_that("A named vector becomes a ChartMatrix", {
    X = c("A" = 1, "B" = 2, "C" = 3)
    my.chart.matrix <- AsChartMatrix(X)
    expect_true(IsChartMatrix(my.chart.matrix, 1, 3))
})

test_that("A single column table becomes a chart matrix", {
    Y <- table(x.data, y.data)
    colnames(Y) <- "Series 1"

    my.chart.matrix <- AsChartMatrix(Y)
    expect_true(IsChartMatrix(my.chart.matrix, 3, 1))
})

test_that("A numeric matrix with one column becomes a chart matrix", {
    Y <- matrix(1:3, ncol = 1, dimnames = list(x = LETTERS[1:3], series = "Series 1"))

    my.chart.matrix <- AsChartMatrix(Y)
    expect_true(IsChartMatrix(my.chart.matrix, 3, 1))
})

test_that("A table with one row becomes a chart matrix", {
    Y <- table(y.data, x.data)
    rownames(Y) <- "Series 1"

    my.chart.matrix <- AsChartMatrix(Y)
    expect_true(IsChartMatrix(my.chart.matrix, 1, 3))
})

test_that("One numeric or integer vector and one character vector become a chart matrix", {
    my.chart.matrix <- AsChartMatrix(y.data, x.data)
    expect_true(IsChartMatrix(my.chart.matrix, 1, 3))
})

test_that("One numeric or integer vector and one factor vector become a chart matrix", {
    my.chart.matrix <- AsChartMatrix(y.data, factor(x.data))
    expect_true(IsChartMatrix(my.chart.matrix, 1, 3))
})

test_that("One numeric or integer vector and one ordered factor vector become a chart matrix", {
    my.chart.matrix <- AsChartMatrix(y.data, as.ordered(x.data))
    expect_true(IsChartMatrix(my.chart.matrix, 1, 3))
})

test_that("One or more numeric or integer vector(s) in a list and one character vector become a chart matrix", {
    Y <- list(A = var1, B = var2, C = var3)
    X <- alpha.five

    my.chart.matrix <- AsChartMatrix(Y, alpha.five)
    expect_true(IsChartMatrix(my.chart.matrix, 3, 5))
})

test_that("One or more numeric or integer vector(s) in a data frame and one character vector become a chart matrix", {
    Y <- cbind(var1, var2, var3)
    X <- alpha.five

    my.chart.matrix <- AsChartMatrix(Y, alpha.five)
    expect_true(IsChartMatrix(my.chart.matrix, 3, 5))
})

test_that("One or more factors with the same levels in a data frame become a chart matrix", {
    my.chart.matrix <- AsChartMatrix(factor.a, factor.a)
    expect_true(IsChartMatrix(my.chart.matrix, 3, 3))
})

test_that("One numeric or integer variable and one date variable become a chart matrix", {
    my.chart.matrix <- AsChartMatrix(var2, x.dates)
    expect_true(IsChartMatrix(my.chart.matrix, nrow(my.chart.matrix), ncol(my.chart.matrix)))
})

test_that("A numeric weight vector applied over integer Y before aggregation", {
    my.chart.matrix <- AsChartMatrix(y = var3, x.data, weights = var1)
    expect_true(IsChartMatrix(my.chart.matrix, 1, 3))
})

## What about lists of weights to be passed over a list of Y-vectors?  If, e.g. each entity in the Y-list
## contained values that should be weighted differently?



##### BAD CASES ######

## Y bad cases are:
test_that("Y cannot take an unnamed numeric vector without an X input", {
    my.chart.matrix <- AsChartMatrix(y.data)
    expect_false(IsChartMatrix(my.chart.matrix, 1, 5))
})

test_that("Y cannot take an unnamed logic vector regardless of X-value", {
    my.chart.matrix <- AsChartMatrix(logic.vector)
    expect_false(IsChartMatrix(my.chart.matrix, 1, 5))
})

test_that("Y cannot take a named logic vector", {
    my.chart.matrix <- AsChartMatrix(logic.vector.named)
    expect_error(IsChartMatrix(my.chart.matrix, 1, 5))
})

test_that("Y cannot take a list of logic vectors (unnamed)", {
    Y <- list(logic.vector, logic.vector, logic.vector)

    my.chart.matrix <- AsChartMatrix(Y)
    expect_false(IsChartMatrix(my.chart.matrix, 1, 3))
})

test_that("Y cannot take a list of logic vectors (named)", {
    Y <- list(logic.vector.named, logic.vector.named, logic.vector.named)

    my.chart.matrix <- AsChartMatrix(Y)
    expect_false(IsChartMatrix(my.chart.matrix, 1, 3))
})

test_that("Y cannot take a data frame of logic vectors (unnamed)", {
    Y <- data.frame(cbind(logic.vector, logic.vector, logic.vector))

    my.chart.matrix <- AsChartMatrix(Y)
    expect_error(IsChartMatrix(my.chart.matrix, 5, 3))
})

test_that("Y cannot take a data frame of logic vectors (named)", {
    Y <- data.frame(cbind(logic.vector.named, logic.vector.named, logic.vector.named))

    my.chart.matrix <- AsChartMatrix(Y)
    expect_error(IsChartMatrix(my.chart.matrix, 5, 3))
})

test_that("Y cannot take a list of differently named vectors", {
    Y <- list(named.vector.a, named.vector.b)

    my.chart.matrix <- AsChartMatrix(Y)
    expect_false(IsChartMatrix(my.chart.matrix, 1, 2))
})

test_that("Y cannot take a data frame of differently named vectors", {
    Y <- data.frame(cbind(named.vector.a, named.vector.b))

    my.chart.matrix <- AsChartMatrix(Y)
    expect_error(IsChartMatrix(my.chart.matrix, 3, 2))
})

test_that("Y cannot take a list of character and integer vectors", {
    Y <- list(var1, x.data)

    my.chart.matrix <- AsChartMatrix(Y)
    expect_false(IsChartMatrix(my.chart.matrix, 1, 2))
})

test_that("Y cannot take a data frame of mixed character and integer vectors", {
    Y <- cbind(var1, x.data)

    my.chart.matrix <- AsChartMatrix(Y)
    expect_false(IsChartMatrix(my.chart.matrix, 5, 2))
})

test_that("Y cannot take a list of multiple factors", {
    Y <- list(factor.a, factor.b)

    my.chart.matrix <- AsChartMatrix(Y)
    expect_false(IsChartMatrix(my.chart.matrix, 1, 2))
})

test_that("Y cannot take a data frame of multiple factors", {
    Y <- data.frame(factor.a, factor.b)

    my.chart.matrix <- AsChartMatrix(Y)
    expect_error(IsChartMatrix(my.chart.matrix, 5, 2))
})

test_that("Y cannot take a list of mixed integer vectors and factors", {
    Y <- list(factor.a, y.data)

    my.chart.matrix <- AsChartMatrix(Y)
    expect_false(IsChartMatrix(my.chart.matrix, 1, 2))
})

test_that("Y cannot take a list of mixed character vectors and factors", {
    Y <- list(factor.a, x.data)

    my.chart.matrix <- AsChartMatrix(Y)
    expect_false(IsChartMatrix(my.chart.matrix, 1, 2))
})

test_that("Y cannot take a data frame of mixed integer vectors and factors", {
    Y <- data.frame(cbind(factor.a, y.data))

    my.chart.matrix <- AsChartMatrix(Y)
    expect_error(IsChartMatrix(my.chart.matrix, 5, 2))
})

test_that("Y cannot take a data frame of mixed integer vectors and factors", {
    Y <- data.frame(cbind(factor.a, x.data))

    my.chart.matrix <- AsChartMatrix(Y)
    expect_error(IsChartMatrix(my.chart.matrix, 5, 2))
})

test_that("Y cannot take a character matrix", {
    Y <- matrix(LETTERS[1:3])

    my.chart.matrix <- AsChartMatrix(Y)
    expect_false(IsChartMatrix(my.chart.matrix, 3, 1))
})

test_that("Y cannot take a logic matrix", {
    Y <- matrix(rep(c(TRUE,FALSE),3))

    my.chart.matrix <- AsChartMatrix(Y)
    expect_false(IsChartMatrix(my.chart.matrix, 6, 1))
})

test_that("Y cannot take a character vector", {
    Y <- LETTERS[1:5]

    my.chart.matrix <- AsChartMatrix(Y)
    expect_false(IsChartMatrix(my.chart.matrix, 1, 5))
})

test_that("Y cannot take a list of multiple character vectors", {
    Y <- list(LETTERS[1:5], LETTERS[6:10], LETTERS[11:15])

    my.chart.matrix <- AsChartMatrix(Y)
    expect_false(IsChartMatrix(my.chart.matrix, 1, 3))
})

test_that("Y cannot take a data frame of multiple character vectors", {
    Y <- data.frame(cbind(LETTERS[1:5], LETTERS[6:10], LETTERS[11:15]))

    my.chart.matrix <- AsChartMatrix(Y)
    expect_error(IsChartMatrix(my.chart.matrix, 5, 3))
})

## X bad cases are:
test_that("X cannot take a logic vector", {
    expect_error(AsChartMatrix(var1, logic.vector))
})

test_that("X cannot take a data frame", {
    X <- data.frame(cbind(var1, var2, var3))

    expect_error(AsChartMatrix(var1, X))
})

test_that("X cannot take a list", {
    X <- list(var1, var2, var3)

    expect_error(AsChartMatrix(var1, X))
})

## Bad weights
test_that("A weight cannot be logical", {
    expect_error(AsChartMatrix(y = var3, x.data, weights = logic.vector))
})

test_that("A weight cannot be characters", {
    expect_error(AsChartMatrix(y = var3, x.data, weights = alpha.five))
})

test_that("A weight cannot be a factor", {
    expect_error(AsChartMatrix(y = var3, x.data, weights = factor.a))
})

test_that("A weight cannot be a date", {
    expect_error(AsChartMatrix(y = var3, x.data, weights = x.dates))
})
