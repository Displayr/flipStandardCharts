library(testthat)

types <- c("Area", "Stacked Area", "100% Stacked Area",
           "Bar", "Stacked Bar", "100% Stacked Bar",
           "Column", "Stacked Column", "100% Stacked Column",
           "Line", "Pie", "Donut")

multiple.series.input.types <- c("Stacked Area", "100% Stacked Area",
                                 "Stacked Bar", "100% Stacked Bar",
                                 "Stacked Column", "100% Stacked Column")

unnamed.vector <- c(5, 6, 2, 1.5, 9, 2.2)
named.vector <- structure(c(5, 6, 2, 1.5, 9, 2.2), .Names = c("A", "B", "C", "D", "E", "F"))
unnamed.matrix <- structure(c(1.59, 0.44, 2.52, 0.19, 0.71, 0.18, 0.18, 0.61, 0.08,
                              1.07, 1.31, 0.45, 0.17, 2.87, 2.08, 0.53, 2.62, 1.88, 1.73, 0.12),
                            .Dim = c(5L, 4L))
named.matrix <- structure(c(1.59, 0.44, 2.52, 0.19, 0.71, 0.18, 0.18, 0.61, 0.08,
                            1.07, 1.31, 0.45, 0.17, 2.87, 2.08, 0.53, 2.62, 1.88, 1.73, 0.12),
                          .Dim = c(5L, 4L), .Dimnames = list(c("Row 1", "Row 2", "Row 3", "Row 4", "Row 5"),
                                                             c("Column 1", "Column 2", "Column 3", "Column 4")))
dat <- data.frame(named.matrix)

for (t in types)
{
    outcome <- if (t %in% multiple.series.input.types) "requires more than one series" else NA
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

