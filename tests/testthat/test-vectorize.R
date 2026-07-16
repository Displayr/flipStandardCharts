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

test_that("vectorize on the matrix path recycles a short per-series vector into constant columns", {
    # 2 values, 3 series, 2 rows -> series 3 recycles series 1; every column constant
    m <- flipStandardCharts:::vectorize(c(6, 10), 3, nrow = 2)
    expect_equal(dim(m), c(2, 3))
    expect_equal(m[1, ], c(6, 10, 6))
    expect_equal(m[2, ], c(6, 10, 6))
})

test_that("vectorize on the matrix path silently drops excess per-series selections", {
    # 4 symbols, 3 series, 4 rows -> 4th symbol ignored, each column constant per series
    m <- flipStandardCharts:::vectorize("circle,square,diamond,star", 3, nrow = 4)
    expect_equal(dim(m), c(4, 3))
    expect_true(all(m[, 1] == "circle"))
    expect_true(all(m[, 2] == "square"))
    expect_true(all(m[, 3] == "diamond"))
    expect_false(any(m == "star"))
})

test_that("vectorize preserves every value of a full per-point matrix input", {
    # A matrix already carrying one value per position (rows = data points, cols = series)
    # passes through unchanged: vectorize(m, n = ncol(m), nrow = nrow(m)) is an identity.
    n.rows <- 4
    n.cols <- 3
    input <- matrix(seq_len(n.rows * n.cols) * 1.5, nrow = n.rows, ncol = n.cols) # unique values
    out <- flipStandardCharts:::vectorize(input, n.cols, nrow = n.rows)
    expect_identical(out, input)
    expect_setequal(as.vector(out), as.vector(input)) # no value dropped or duplicated

    # ... also holds for a character matrix
    cinput <- matrix(letters[seq_len(n.rows * n.cols)], nrow = n.rows, ncol = n.cols)
    expect_identical(flipStandardCharts:::vectorize(cinput, n.cols, nrow = n.rows), cinput)
})
