context("vectorize")

test_that("vectorize recycles when fewer values than series", {
    expect_equal(flipStandardCharts:::vectorize(c(1, 2), 5), c(1, 2, 1, 2, 1))
    expect_equal(flipStandardCharts:::vectorize(c("a", "b"), 3), c("a", "b", "a"))
})

test_that("vectorize silently ignores excess values (truncates to n)", {
    expect_equal(flipStandardCharts:::vectorize(c(1, 2, 3, 4, 5), 3), c(1, 2, 3))
    expect_equal(flipStandardCharts:::vectorize("solid,dot,dash,longdash", 2),
                 c("solid", "dot"))
})

test_that("vectorize leaves exact-length input unchanged", {
    expect_equal(flipStandardCharts:::vectorize(c(1, 2, 3), 3), c(1, 2, 3))
    expect_equal(flipStandardCharts:::vectorize("Solid", 1), "Solid")
})

test_that("vectorize builds a per-point matrix from an exact per-series vector", {
    m <- flipStandardCharts:::vectorize(c(6, 10, 14), 3, nrow = 2)
    expect_equal(dim(m), c(2, 3))
    expect_equal(m[1, ], c(6, 10, 14))
    expect_equal(m[2, ], c(6, 10, 14))
})
