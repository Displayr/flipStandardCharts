context("Bean")

# The tests on this page function only as a smoke test.
# The testing of all the individual parameters is
# performed in test-Distribution and integration tests run outside of the package.

    set.seed(1223)
    z = list(Normal = rnorm(1000), "Poisson with unit lamda" = rpois(1000, 1), Exponential = rexp(1000))
test_that("Bean with numeric vector data",
{
    expect_error(Bean(z[[1]]), NA)
# "Bean with data supplied as list",
    Bean(z, title = "Comparing distributions",
           values.title = "Values",
           global.font.family = "Courier",
           global.font.color = "Red")
# "Bean with vertical arg TRUE", {
    Bean(z, title = "Comparing distributions",
           values.title = "Values",
           global.font.family = "Courier",
           global.font.color = "Red",
         vertical = TRUE)
# "Bean called from another function",
    zzz <- function(x)
                      {Bean(x)}
    zzz(z[[1]])
# "var in global env",
    title <- "Comp dists"
    expect_silent(Bean(z, title = title))
})

