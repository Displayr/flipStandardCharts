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

test_that("The opacity family accepts every input form without erroring",
{
    z <- structure(c(1L, 2L, 3L, 4L, 5L, 2L, 3L, 4L, 5L, 6L), .Dim = c(5L, 2L),
        .Dimnames = list(c("T", "U", "V", "W", "X"), c("A", "B")))

    # A comma-separated string is what the Plugins pass for a per-series setting, and it
    # reached arithmetic and toRGB as text
    expect_error(suppressWarnings(Line(z, marker.show = TRUE, opacity = "0.5, 0.9")), NA)
    expect_error(suppressWarnings(Line(z, marker.show = TRUE, marker.opacity = "0.5, 0.5")), NA)
    expect_error(suppressWarnings(Line(z, marker.show = TRUE, marker.opacity = c(0.5, 0.9))), NA)
    expect_error(suppressWarnings(
        Line(z, marker.show = TRUE, marker.border.opacity = "0.5, 0.9")), NA)

    # opacity stays per series: the traces get one each
    p <- plotly::plotly_build(Line(z, opacity = c(0.3, 1),
        colors = c("#FF0000", "#00FF00"))$htmlwidget)
    line.colors <- Filter(Negate(is.null), lapply(p$x$data, function(tr) tr$line$color))
    expect_true(any(grepl("rgba\\(255,0,0,0.3\\)", line.colors)))
    expect_true(any(grepl("rgba\\(0,255,0,1\\)", line.colors)))

    # marker.border.opacity is a single value by contract, so more than one warns and the
    # first wins
    expect_warning(Line(z, marker.show = TRUE, marker.border.opacity = c(0.3, 1)),
                   "marker.border.opacity")
    # One value, or several that agree, is silent
    expect_warning(Line(z, marker.show = TRUE, marker.opacity = 0.5), NA)
    expect_warning(Line(z, marker.show = TRUE, marker.opacity = "0.5, 0.5"), NA)

    # A per-series opacity is legitimate, and markers inherit it - so the chart must not
    # complain about a marker.opacity the caller never set. On the plain plotly route (no
    # data.label.auto.placement), nothing else warns about a per-series opacity either, with
    # or without markers shown.
    expect_warning(Line(z, opacity = c(0.3, 1)), NA)
    expect_warning(Line(z, marker.show = TRUE, opacity = c(0.3, 1)), NA)

    # The contract warning is for a value the caller set, and only when a marker is drawn:
    # an opacity you cannot see is not worth a warning
    expect_warning(Line(z, marker.opacity = c(0.3, 1)), NA)
    expect_warning(Line(z, marker.show = TRUE, marker.opacity = c(0.3, 1)),
                   "Only one marker.opacity can be used")
    expect_warning(Line(z, marker.border.opacity = c(0.3, 1)), NA)
})
