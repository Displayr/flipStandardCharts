library(testthat)

types <- c("Area", "Stacked Area", "100% Stacked Area",
           "Bar", "Stacked Bar", "100% Stacked Bar",
           "Column", "Stacked Column", "100% Stacked Column",
           "Line", "Pie", "Donut")

stacked.types <- c("Stacked Area", "100% Stacked Area",
                   "Stacked Bar", "100% Stacked Bar",
                   "Stacked Column", "100% Stacked Column")

hundred.percent.stacked.types <- c("100% Stacked Area", "100% Stacked Bar", "100% Stacked Column")

unnamed.vector <- c(5, 6, 2, 1.5, 9, 2.2)
named.vector <- structure(c(5, 6, 2, 1.5, 9, 2.2), .Names = c("A", "B", "C", "D", "E", "F"))
unnamed.matrix <- structure(c(1.59, 0.44, 2.52, 0.19, 0.71, 0.18, 0.18, 0.61, 0.08,
                              1.07, 1.31, 0.45, 0.17, 2.87, 2.08, 0.53, 2.62, 1.88, 1.73, 0.12),
                            .Dim = c(5L, 4L))
named.matrix <- structure(c(1.59, 0.44, 2.52, 0.19, 0.71, 0.18, 0.18, 0.61, 0.08,
                            1.07, 1.31, 0.45, 0.17, 2.87, 2.08, 0.53, 2.62, 1.88, 1.73, 0.12),
                          .Dim = c(5L, 4L), .Dimnames = list(c("Row 1", "Row 2", "Row 3", "Row 4", "Row 5"),
                                                             c("Column 1", "Column 2", "Column 3", "Column 4")))
missing <- structure(c(NA, NA, NA, NA, 0.71, NA, 0.18, 0.61, 0.08,
                            1.07, NA, 0.45, 0.17, 2.87, NaN, 0.53, 2.62, 1.88, 1.73, 0.12),
                          .Dim = c(5L, 4L), .Dimnames = list(c("Row 1", "Row 2", "Row 3", "Row 4", "Row 5"),
                                                             c("Column 1", "Column 2", "Column 3", "Column 4")))
row.sum.zero <- structure(c(0, 0.44, 2.52, 0.19, 0.71, 0, 0.18, 0.61, 0.08,
                              1.07, 0, 0.45, 0.17, 2.87, 2.08, 0, 2.62, 1.88, 1.73, 0.12),
                            .Dim = c(5L, 4L))
duplicate.rows <- structure(c(1.59, 0.44, 2.52, 0.19, 0.71, 0.18, 0.18, 0.61, 0.08,
                              1.07, 1.31, 0.45, 0.17, 2.87, 2.08, 0.53, 2.62, 1.88, 1.73, 0.12),
                          .Dim = c(5L, 4L), .Dimnames = list(c("Row", "Row 2", "Row", "Row 4", "Row 5"),
                                                             c("Column 1", "Column 2", "Column 3", "Column 4")))


dat <- data.frame(named.matrix)

# Input types

for (t in types)
{
    outcome <- if (t %in% stacked.types) "requires more than one series" else NA
    test_that(paste(t, "- unnamed vector input"), {
        expect_error(print(Chart(unnamed.vector, type = t)), outcome)
    })
    test_that(paste(t, "- named vector input"), {
        expect_error(print(Chart(named.vector, type = t)), outcome)
    })

    outcome <- if (t == "Donut")
        "The table supplied is two-dimensional and cannot be displayed as a donut chart"
    else
        NA
    test_that(paste(t, "- unnamed matrix input"), {
        expect_error(print(Chart(unnamed.matrix, type = t)), outcome)
    })
    test_that(paste(t, "- named matrix input"), {
        expect_error(print(Chart(named.matrix, type = t)), outcome)
    })
    test_that(paste(t, "- data frame input"), {
        expect_error(print(Chart(dat, type = t)), outcome)
    })
    test_that(paste(t, "- transpose"), {
        expect_error(print(Chart(dat, type = t, transpose = TRUE)), outcome)
    })
}

# Missing values

test_that("Area - missing", {
    expect_warning(print(Chart(missing, type = "Area")), "Missing values have been interpolated or omitted.")
})

test_that("Bar - missing", {
    expect_warning(print(Chart(missing, type = "Bar")), "Missing values have been set to zero.")
})

test_that("Column - missing", {
    expect_warning(print(Chart(missing, type = "Column")), "Missing values have been set to zero.")
})

test_that("Line - missing", {
    expect_warning(print(Chart(missing, type = "Line")), "Missing values have been omitted.")
})

test_that("Pie - missing", {
    expect_warning(print(Chart(missing, type = "Pie")), "Missing and negative values have been set to zero.")
})

for (t in stacked.types)
{
    test_that(paste(t, "- single series"), {
        expect_error(print(Chart(named.vector, type = t)), "requires more than one series.")
    })
}

for (t in stacked.types)
{
    test_that(paste(t, "- missing"), {
        expect_error(print(Chart(missing, type = t)),
                     "Stacked charts cannot be produced with missing or negative values.")
    })
}

for (t in hundred.percent.stacked.types)
{
    test_that(paste(t, "- row sum zero"), {
        expect_error(print(Chart(row.sum.zero, type = t)),
                     "100% stacked charts cannot be produced with rows that do not contain positive values.")
    })
}

for (t in types)
{
    test_that(paste(t, "- duplicate rows"), {
        expect_error(print(Chart(duplicate.rows, type = t)),
                     "Row names of the input table must be unique.")
    })
}

