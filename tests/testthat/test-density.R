context("Density")
#
# # The tests on this page function only as a smoke test.
# # The testing of all the individual parameters is
# # performed in test-Distribution and integration tests run outside of the package.
#
test_that("Density", {
    set.seed(1223)
    z = list(Normal = rnorm(1000), "Poisson with unit lamda" = rpois(1000, 1),
             Exponential = rexp(1000))
    expect_error(print(Density(z[[1]])), NA)
    Density(z, title = "Comparing distributions",
         values.title = "Values",
         global.font.family = "Courier",
         global.font.color = "Red")
    Density(z, title = "Comparing distributions",
         values.title = "Values",
         global.font.family = "Courier",
         global.font.color = "Red",
         adjust = .2)

    Density(z, title = "Comparing distributions",
         values.title = "Values",
         global.font.family = "Courier",
         global.font.color = "Red",
         vertical = TRUE, show.values = TRUE)
})
