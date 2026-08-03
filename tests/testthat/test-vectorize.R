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

# On the nrow path a value is only read per series when there is exactly one per series.
# Anything else is recycled across the whole matrix row-major, so it spreads over the points
# rather than staying constant down each column. The two tests below pin that down. Settings
# that must be read per series are reduced to one value per series before they get here --
# readNumericSeries() does it for marker.size and line.thickness -- and the per-series
# settings that are not, line.type and marker.symbols, are vectorized without an nrow.

test_that("vectorize recycles a short vector across the matrix, not down each column", {
    # 2 values, 3 series, 2 rows -> recycled row-major, so no column is constant
    m <- flipStandardCharts:::vectorize(c(6, 10), 3, nrow = 2)
    expect_equal(dim(m), c(2, 3))
    expect_equal(m[1, ], c(6, 10, 6))
    expect_equal(m[2, ], c(10, 6, 10))
})

test_that("vectorize recycles an over-length vector across the matrix rather than truncating", {
    # 4 symbols, 3 series, 4 rows -> every symbol is used, spread over the points
    m <- flipStandardCharts:::vectorize("circle,square,diamond,star", 3, nrow = 4)
    expect_equal(dim(m), c(4, 3))
    expect_equal(m[1, ], c("circle", "square", "diamond"))
    expect_equal(m[2, ], c("star", "circle", "square"))
    expect_equal(m[3, ], c("diamond", "star", "circle"))
    expect_equal(m[4, ], c("square", "diamond", "star"))
})

test_that("vectorize truncates an over-length vector when there is no nrow", {
    # The per-series settings that rely on truncation -- line.type, marker.symbols -- ask
    # for a plain vector, and there the excess is still dropped.
    expect_equal(flipStandardCharts:::vectorize("circle,square,diamond,star", 3),
                 c("circle", "square", "diamond"))
})

test_that("vectorize preserves a logical per-point vector given one value per position", {
    # 1 series, 5 points: the vector describes the points, not the series. Sizing it to the
    # series count before the nrow expansion would collapse it to its first value.
    expect_equal(flipStandardCharts:::vectorize(c(TRUE, FALSE, TRUE, FALSE, FALSE), 1, nrow = 5),
                 matrix(c(TRUE, FALSE, TRUE, FALSE, FALSE), nrow = 5))

    # 2 series, 3 points; a flat per-point vector fills the matrix row-major
    input <- c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE)
    expect_equal(flipStandardCharts:::vectorize(input, 2, nrow = 3),
                 matrix(input, nrow = 3, byrow = TRUE))
})

test_that("vectorize preserves a character per-point vector", {
    # data.label.prefix reaches vectorize as split = NULL with an nrow, so a per-point
    # character vector has to survive the same way the logical one does.
    expect_equal(flipStandardCharts:::vectorize(LETTERS[1:5], 1, nrow = 5, split = NULL),
                 matrix(LETTERS[1:5], nrow = 5))
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

test_that("A setting that was never set produces no warning", {
    # NULL reaches rep(x, length = n), which warns "'x' is NULL so the result will be NULL".
    # An argument nobody supplied is not worth a warning, and skipping rep for it gives the
    # same answer anyway.
    expect_warning(flipStandardCharts:::vectorize(NULL, 3), NA)
    expect_warning(flipStandardCharts:::vectorize(NULL, 3, nrow = 2), NA)
    expect_warning(flipStandardCharts:::readNumericSeries(NULL, 3, "smoothing"), NA)

    # and the answer is unchanged
    expect_equal(suppressWarnings(flipStandardCharts:::vectorize(NULL, 3)), c("", "", ""))
    expect_null(suppressWarnings(flipStandardCharts:::readNumericSeries(NULL, 3, "smoothing")))
})
