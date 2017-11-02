context("Histogram")
#
# # The tests on this page function only as a smoke test.
# # The testing of all the individual parameters is
# # performed in test-Distribution and integration tests run outside of the package.
#
test_that("Histogram", {
    set.seed(1223)
    z = list(Normal = rnorm(1000), "Poisson with unit lamda" = rpois(1000, 1),
             Exponential = rexp(1000))
    Histogram(z[[1]])
    Histogram(z, title = "Comparing distributions",
            values.title = "Values",
            global.font.family = "Courier",
            global.font.color = "Red")
    Histogram(z, title = "Comparing distributions",
            values.title = "Values",
            global.font.family = "Courier",
            global.font.color = "Red",
            vertical = TRUE)
    Histogram(z, title = "Comparing distributions",
              values.title = "Values",
              global.font.family = "Courier",
              global.font.color = "Red")
    Histogram(z, title = "Comparing distributions",
              values.title = "Values",
              show.values = TRUE,
              histogram.cumulative = TRUE,
              histogram.counts = TRUE,
              maximum.bins = 5)
})
