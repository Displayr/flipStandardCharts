#context("Bean")

# The tests on this page function only as a smoke test.
# The testing of all the individual parameters is
# performed in test-Distribution and integration tests run outside of the package.

#test_that("Bean", {
library(flipStandardCharts)
cat("dog\n")
    set.seed(1223)
    z = list(Normal = rnorm(1000), "Poisson with unit lamda" = rpois(1000, 1), Exponential = rexp(1000))
    Bean(z[[1]])
    Bean(z, title = "Comparing distributions",
           values.title = "Values",
           global.font.family = "Courier",
           global.font.color = "Red")
    Bean(z, title = "Comparing distributions",
           values.title = "Values",
           global.font.family = "Courier",
           global.font.color = "Red",
           vertical = TRUE)
        zzz <- function(x)
    {Bean(x)}
    zzz(z[[1]])

cat("cat\n")

#})
