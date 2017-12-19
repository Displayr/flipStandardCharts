context("Violin")
#
# # The tests on this page function only as a smoke test.
# # The testing of all the individual parameters is
# # performed in test-Distribution and integration tests run outside of the package.
#
test_that("Violin", {
    set.seed(1223)
    z = list(Normal = rnorm(1000), "Poisson with unit lamda" = rpois(1000, 1),
             Exponential = rexp(1000))
    expect_error(print(Violin(z[[1]])), NA)
    Violin(z, title = "Comparing distributions",
           values.title = "Values",
           global.font.family = "Courier",
           global.font.color = "Red")
    Violin(z, title = "Comparing distributions",
           values.title = "Values",
           global.font.family = "Courier",
           global.font.color = "Red",
           vertical = FALSE)
    Violin(z, title = "Comparing distributions",
           values.title = "Values",
           global.font.family = "Courier",
           global.font.color = "Red",
           categories.tick.label.wrap.nchar = 5,
           vertical = FALSE)
    Violin(z, title = "Comparing distributions",
           values.title = "Values",
           values.hovertext.format = "%",
           global.font.family = "Courier",
           global.font.color = "Red",
           categories.tick.label.wrap.nchar = 5,
           vertical = FALSE)
    Violin(z, title = "Comparing distributions",
           values.title = "Values",
           values.hovertext.format = "%",
           global.font.family = "Courier",
           global.font.color = "Red",
           categories.tick.label.wrap.nchar = 5,
           vertical = TRUE)

    # matrix
    z = matrix(1:10, 5)
    Violin(z, title = "Comparing distributions",
           values.title = "Values",
           values.hovertext.format = "%",
           global.font.family = "Courier",
           global.font.color = "Red",
           categories.tick.label.wrap.nchar = 5,
           vertical = TRUE)
    # data.frame
    z = as.data.frame(matrix(1:10, 5))
    Violin(z, title = "Comparing distributions",
           values.title = "Values",
           values.hovertext.format = "%",
           global.font.family = "Courier",
           global.font.color = "Red",
           categories.tick.label.wrap.nchar = 5,
           vertical = TRUE)

})
