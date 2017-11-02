#context("Bean")

# The tests on this page function only as a smoke test.
# The testing of all the individual parameters is
# performed in test-Distribution and integration tests run outside of the package.

#test_that("Bean", {
    set.seed(1223)
    z = list(Normal = rnorm(1000), "Poisson with unit lamda" = rpois(1000, 1), Exponential = rexp(1000))
test_that("Bean with numeric vector data",
{
    Bean(z[[1]])
})

test_that("Bean with data supplied as list", {
    Bean(z, title = "Comparing distributions",
           values.title = "Values",
           global.font.family = "Courier",
           global.font.color = "Red")
})

test_that("Bean with vertical arg TRUE", {
    Bean(z, title = "Comparing distributions",
           values.title = "Values",
           global.font.family = "Courier",
           global.font.color = "Red",
         vertical = TRUE)
})

test_that("Bean called from another function",
{
    zzz <- function(x)
                      {Bean(x)}
    zzz(z[[1]])
})

test_that("var in global env",
{
    title <- "Comp dists"
    expect_silent(Bean(z, title = title))
})

