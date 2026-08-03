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

test_that("FS2-4532: firstOpacity drops a non-numeric entry rather than taking it verbatim", {
    # A non-numeric first entry stays NA through readNumericSeries; firstOpacity must not
    # let that NA reach toRGB(alpha = NA), which silently draws a fully opaque colour.
    expect_warning(flipStandardCharts:::firstOpacity("x, 0.5", "marker.opacity"),
                   "Non-numeric marker.opacity value 'x' was ignored")
    expect_equal(suppressWarnings(flipStandardCharts:::firstOpacity("x, 0.5", "marker.opacity")),
                 0.5)
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
    # The hover background on the data-label path indexes opacity by series (toRGB takes a
    # length-1 alpha), so both the string and vector forms of a per-series opacity need
    # covering here specifically
    expect_error(suppressWarnings(Line(z, opacity = "0.5, 0.9", data.label.show = TRUE)), NA)
    expect_error(suppressWarnings(Line(z, opacity = c(0.3, 1), data.label.show = TRUE)), NA)
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

    # Markers inherit a per-series opacity, but only one of them can be used, so the chart
    # says which one it took rather than quietly drawing later series at the first series'
    # transparency. The inherited case warns like an explicit one: the caller cannot tell the
    # difference by looking at the chart.
    expect_warning(Line(z, marker.show = TRUE, opacity = c(0.3, 1)),
                   "Only one marker.opacity can be used")
    # No marker is drawn, so there is no difference to see and nothing to say
    expect_warning(Line(z, opacity = c(0.3, 1)), NA)

    # The contract warning is for a value the caller set, and only when a marker is drawn:
    # an opacity you cannot see is not worth a warning
    expect_warning(Line(z, marker.opacity = c(0.3, 1)), NA)
    expect_warning(Line(z, marker.show = TRUE, marker.opacity = c(0.3, 1)),
                   "Only one marker.opacity can be used")
    expect_warning(Line(z, marker.border.opacity = c(0.3, 1)), NA)
})

test_that("An isolated point is drawn at its own series' marker size", {
    # A point with a gap on both sides has no line to sit on, so it is drawn by a trace of
    # its own. Each series' isolated points must take that series' size, not the first one's.
    dat <- cbind(A = c(1, NA, 3, 4, NA), B = c(NA, 2, NA, 4, 5))
    rownames(dat) <- letters[1:5]
    pb <- plotly::plotly_build(suppressWarnings(
        Line(dat, marker.show = TRUE, marker.size = c(6, 14))$htmlwidget))

    # The isolated-point traces are the marker-only ones; the series themselves draw lines
    isolated <- Filter(function(tr) identical(as.character(tr$mode)[1], "markers") &&
                                    !is.null(tr$name), pb$x$data)
    sizes <- setNames(lapply(isolated, function(tr) as.numeric(tr$marker$size)),
                      vapply(isolated, function(tr) as.character(tr$name)[1], character(1)))
    expect_equal(sizes[["A"]], 6)
    expect_equal(sizes[["B"]], 14)
})

test_that("A per-point marker.size matrix reaches every point it names", {
    # One size per position, which the Plugins never send but a caller can, and which the
    # per-series parsing used to flatten to its first few cells
    z2 <- cbind(A = 1:5, B = 2:6)
    rownames(z2) <- letters[1:5]
    szmat <- cbind(c(2, 4, 6, 8, 10), c(3, 5, 7, 9, 11))
    pb <- plotly::plotly_build(Line(z2, marker.show = TRUE, marker.size = szmat)$htmlwidget)
    sizes <- setNames(lapply(Filter(function(tr) !is.null(tr$marker$size) && !is.null(tr$name),
                                    pb$x$data),
                             function(tr) as.numeric(tr$marker$size)),
                      vapply(Filter(function(tr) !is.null(tr$marker$size) && !is.null(tr$name),
                                    pb$x$data),
                             function(tr) as.character(tr$name)[1], character(1)))
    expect_equal(sizes[["A"]], c(2, 4, 6, 8, 10))
    expect_equal(sizes[["B"]], c(3, 5, 7, 9, 11))
})

# The first trace carrying each series' name is the one drawing its line; the chart also adds
# an unnamed trace to force categorical labels, and further named ones for data labels.
seriesLine <- function(pp, nm)
{
    pb <- plotly::plotly_build(pp$htmlwidget)
    for (tr in pb$x$data)
        if (identical(as.character(tr$name)[1], nm) && !is.null(tr$line$shape))
            return(tr$line)
    NULL
}

test_that("Line joins each series with the shape asked for", {
    z2 <- cbind(A = 1:5, B = 2:6)
    rownames(z2) <- letters[1:5]
    pp <- Line(z2, shape = "Straight, Curved")
    expect_equal(seriesLine(pp, "A")$shape, "linear")
    expect_equal(seriesLine(pp, "B")$shape, "spline")
})

test_that("Line accepts a single shape for every series", {
    z2 <- cbind(A = 1:5, B = 2:6)
    rownames(z2) <- letters[1:5]
    pp <- Line(z2, shape = "Curved")
    expect_equal(seriesLine(pp, "A")$shape, "spline")
    expect_equal(seriesLine(pp, "B")$shape, "spline")

    # The plotly spellings work too, whatever their case
    expect_equal(seriesLine(Line(z2, shape = "Spline"), "A")$shape, "spline")
    expect_equal(seriesLine(Line(z2), "A")$shape, "linear")
})

test_that("Line smooths each series by its own amount", {
    z2 <- cbind(A = 1:5, B = 2:6)
    rownames(z2) <- letters[1:5]
    pp <- Line(z2, shape = "Curved", smoothing = "0.5, 1.3")
    expect_equal(seriesLine(pp, "A")$smoothing, 0.5)
    expect_equal(seriesLine(pp, "B")$smoothing, 1.3)
})
