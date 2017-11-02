context("Box")
#
# # The tests on this page function only as a smoke test.
# # The testing of all the individual parameters is
# # performed in test-Distribution and integration tests run outside of the package.
#
test_that("Box", {
   set.seed(1223)
   z = list(Normal = rnorm(1000), "Poisson with unit lamda" = rpois(1000, 1),
            Exponential = rexp(1000))
    Box(z[[1]])
    Box(z, title = "Comparing distributions",
            values.title = "Values",
            global.font.family = "Courier",
            global.font.color = "Red")
    Box(z, title = "Comparing distributions",
        values.title = "Values",
        global.font.family = "Courier",
        global.font.color = "Red",
        vertical = FALSE,
        box.points = "Outliers")
    Box(z, title = "Comparing distributions",
        values.title = "Values",
        global.font.family = "Courier",
        global.font.color = "Red",
        show.values = TRUE,
        values.color = "Blue")
    Box(z, title = "Comparing distributions",
        values.title = "Values",
        global.font.family = "Courier",
        global.font.color = "Red",
        values.color = "Blue")
})
