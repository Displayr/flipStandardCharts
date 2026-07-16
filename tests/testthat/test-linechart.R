context("Line chart")

data("WorldPhones")

test_that("Line thickness",
{
    expect_error(Line(WorldPhones, line.thickness = ""), NA)
    expect_error(Line(WorldPhones, line.thickness = "1,4"), NA)
    expect_warning(Line(WorldPhones, line.thickness = "blah"),
                   "Non-numeric line thickness value 'blah' was ignored")
    expect_warning(Line(WorldPhones, line.thickness = "6,5,4..3,2,l"),
                   "Non-numeric line thickness values '4..3', 'l' were ignored")
})

test_that("FS2-4532: readNumericSeries parses, recycles and truncates", {
    expect_equal(flipStandardCharts:::readNumericSeries(6, 3, "marker size"), c(6, 6, 6))
    expect_equal(flipStandardCharts:::readNumericSeries("6,10,14", 3, "marker size"), c(6, 10, 14))
    expect_equal(flipStandardCharts:::readNumericSeries("6, 10", 3, "marker size"), c(6, 10, 6))
    expect_equal(flipStandardCharts:::readNumericSeries("6,10,14,20", 3, "marker size"), c(6, 10, 14))
    expect_warning(flipStandardCharts:::readNumericSeries("6,foo,14", 3, "marker size"),
                   "Non-numeric marker size value 'foo' was ignored")
    # Position-preserving: the bad token stays NA in its own slot
    expect_equal(suppressWarnings(flipStandardCharts:::readNumericSeries("6,foo,14", 3, "marker size")),
                 c(6, NA, 14))
    # `what` names the setting in the warning
    expect_warning(flipStandardCharts:::readNumericSeries("1,x,y", 3, "line thickness"),
                   "Non-numeric line thickness values 'x', 'y' were ignored")
})

test_that("FS2-4532: Line renders with per-series marker size string", {
    dat <- matrix(c(1, 4, 2, 5, 3, 6), nrow = 2,
                  dimnames = list(c("a", "b"), c("x", "y", "z")))
    expect_error(Line(dat, marker.show = TRUE, marker.size = "6,10,14"), NA)
})
