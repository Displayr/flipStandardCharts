context("Parallel Coordinates")

test_that("Parallel Coordinates",
{
    expect_error(ParallelCoordinates(iris), NA)
    expect_error(ParallelCoordinates(iris[,c(5,1:4)], group = iris[,5],
        colors=c("red", "orange", "green", "blue"), opacity = 0.2,
        reverse.axes = FALSE, label.rotate = TRUE), NA)
    expect_error(ParallelCoordinates(iris[,1:4], group = iris[,5],
        colors=c("red", "orange", "green", "blue")), NA)


    large.mat <- matrix(rnorm(100*500), nrow=100, ncol = 500)
    expect_warning(ParallelCoordinates(as.data.frame(large.mat)),
        "Only the first 100 variables will be shown.")
    expect_warning(ParallelCoordinates(as.data.frame(large.mat), max.nvar=20),
        "Only the first 20 variables will be shown.")
})
