context("AsChartMatrix")
#  rm(list=ls())

x.data = c("A","B","B","C","C")
y.data = c(0, 0, 0, 0, 0)
var1 <- c(1,2,3,4,5)
var2 <- c(5,4,3,2,1)
var3 <- c(3,3,3,3,3)
alpha.five <- LETTERS[1:5]

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

# Factor variables
test_that("One or more numeric or integer vector(s) in a data frame and one character vector become a chart matrix", {
    X <- as.factor(c("A","B","A","B","C","C"))
    Y <- as.factor(c("A","B","A","B","C","C"))

    my.chart.matrix <- AsChartMatrix(Y, X)
    expect_true(IsChartMatrix(my.chart.matrix, 3, 3))
})

##### BAD CASES ######

## Regardless of X input, Y bad cases are:
## Unnamed numeric vector
Y <- c(1, 2, 3, 4, 5)

## Unnamed logic vector
logic.vector <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
Y <- c(TRUE, FALSE, TRUE, TRUE, FALSE)

## Named logic vector
Y <- c(A = TRUE, B = FALSE, C = TRUE, D = TRUE, E = FALSE)

## List of logic vectors
Y <- list(logic.vector, logic.vector, logic.vector)

## Data frame of logic vectors
Y <- data.frame(cbind(logic.vector, logic.vector, logic.vector))

## List of named vectors
named.vector.a <- c("A" = 1, "B" = 2, "C" = 3)
named.vector.b <- c("D" = 3, "E" = 2, "F" = 1)
Y <- list(named.vector.a, named.vector.b)

## Data frame of named vectors
Y <- data.frame(cbind(named.vector.a, named.vector.b))

## Mixed mode list - character and integer
var4 <- c("A", "B", "C", "D", "C")
Y <- list(var1, var2, var3, var4)

## Mixed mode data frame - character and integer
Y <- cbind(var1, var2, var3, var4)

## List of factors
factor.a <- factor(x.data)
factor.b <- factor(y.data)
Y <- list(factor.a, factor.b)

## Data frame of factors
Y <- data.frame(factor.a, factor.b)

## Mixed mode list - integer and factor
Y <- list(factor.a, y.data)

## Mixed mode list - character and factor
Y <- list(factor.a, var4)

## Mixed mode data frame - integer and factor
Y <- data.frame(cbind(factor.a, y.data))

## Mixed mode data frame - character and factor
Y <- data.frame(cbind(factor.a, var4))

## Character matrix
Y <- matrix(LETTERS[1:3])

## Logic matrix
Y <- matrix(rep(c(TRUE,FALSE),3))

## Raw data consisting of one character vector
Y <- LETTERS[1:5]

## Raw data consisting of multiple character vectors passed as a list
Y <- list(LETTERS[1:5], LETTERS[6:10], LETTERS[11:15])

## Raw data consisting of multiple character vectors passed as a data frame
Y <- data.frame(cbind(LETTERS[1:5], LETTERS[6:10], LETTERS[11:15]))

## Regardless of Y input, X bad cases are:
## X as a logic vector *********************** Would not make much sense as an area or line chart (but could be used in column or bar charts?)
X <- logic.vector

## X as a data frame, regardless of composition
X <- data.frame(cbind(var1, var2, var3))

## X as a list, regardless of composition
X <- list(var1, var2, var3)


