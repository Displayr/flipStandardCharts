library(testthat)

types <- c("Area", "Stacked Area", "100% Stacked Area",
           "Bar", "Stacked Bar", "100% Stacked Bar",
           "Column", "Stacked Column", "100% Stacked Column",
           "Line", "Pie", "Donut")

stacked.types <- c("Stacked Area", "100% Stacked Area",
                   "Stacked Bar", "100% Stacked Bar",
                   "Stacked Column", "100% Stacked Column")

hundred.percent.stacked.types <- c("100% Stacked Area", "100% Stacked Bar", "100% Stacked Column")

area.or.line.charts <- c("Area", "Stacked Area", "100% Stacked Area", "Line")

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

three.dimensional <- structure(1:60, .Dim = 3:5)

table.with.statistic <- structure(c(1.59, 0.44, 2.52, 0.19, 0.71, 0.18, 0.18, 0.61, 0.08,
            1.07, 1.31, 0.45, 0.17, 2.87, 2.08, 0.53, 2.62, 1.88, 1.73, 0.12), .Dim = c(5L, 4L),
            .Dimnames = list(c("Row 1", "Row 2", "Row 3", "Row 4", "Row 5"),
                             c("Column 1", "Column 2", "Column 3", "Column 4")),
            statistic = "%", questions = "Q1")

monthly <- structure(c(7.34177215189873, 9.36708860759494, 9.36708860759494,
                      7.84810126582278, 8.60759493670886, 8.86075949367089, 10.6329113924051,
                      5.31645569620253, 8.35443037974684, 7.84810126582278, 9.11392405063291,
                      7.34177215189873, 7.40740740740741, 6.91358024691358, 9.62962962962963,
                      10.8641975308642, 6.41975308641975, 7.40740740740741, 8.64197530864197,
                      8.39506172839506, 8.64197530864197, 8.88888888888889, 8.64197530864197,
                      8.14814814814815, 7.375, 8.125, 9.5, 9.375, 7.5, 8.125, 9.625,
                      6.875, 8.5, 8.375, 8.875, 7.75), .Dim = c(12L, 3L),
                    statistic = "Column %", .Dimnames = list(
                          c("January 2012", "February 2012", "March 2012", "April 2012",
                            "May 2012", "June 2012", "July 2012", "August 2012", "September 2012",
                            "October 2012", "November 2012", "December 2012"), c("Male", "Female", "NET")),
                    name = "Interview Date by Gender",
                    questions = c("Interview Date", "Gender [Cola Tracking - January to December.sav]"))

arr <- structure(c(13.4556574923547, 11.9266055045872, 10.0917431192661,
                     11.0091743119266, 10.7033639143731, 8.25688073394496, 12.2324159021407,
                     15.5963302752294, 6.72782874617737, 100), .Dim = 10L, statistic = "%", .Dimnames = list(
                     c("18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44",
                     "45 to 49", "50 to 54", "55 to 64", "65 or more", "NET")),
                       name = "Q3. Age", questions = c("Q3. Age", "SUMMARY"))

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

test_that("Pie - missing", {
    expect_warning(print(Chart(missing, type = "Pie")), "Missing and negative values have been omitted.")
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

for (t in area.or.line.charts)
{
    test_that(paste(t, "- numeric labels"), {
        expect_error(print(Chart(unnamed.matrix, type = t)), NA)
    })
}

test_that("Area - opacity", {
    expect_warning(print(Chart(unnamed.matrix, type = "Area", opacity = 1)),
        "Displaying this chart with opacity set to 1 will make it difficult to read as some data series may be obscured.")
})

for (t in c("Line", "Pie", "Donut"))
{
    test_that(paste(t, "- opacity"), {
        expect_warning(print(Chart(named.vector, type = t, opacity = 1)),
                       "The opacity parameter is only valid for area, bar and column charts.")
    })
}

test_that("3D data", {
    expect_warning(print(Chart(three.dimensional, type = "Column")),
                   "The input has more than 2 dimensions, only the first 2 have been displayed.")
})

test_that("Percentage statistics", {
    expect_warning(print(Chart(table.with.statistic, type = "Pie")),
        paste("The percentage values in the table do not sum to 100%.",
              "Consider choosing a different statistic for the table."))
})

test_that("Chart type check", {
    expect_error(print(Chart(unnamed.matrix, type = "invalid type")), "The input chart type is not supported.")
})

for (t in c("Line", "Bar", "Column"))
{
    test_that(paste(t, "- dates"), {
        expect_error(print(Chart(monthly, t)), NA)
    })
}

test_that("Array", {
    expect_error(Chart(y = arr, type = "Area"), NA)
})
