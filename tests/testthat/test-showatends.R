context("show at ends")

# Every test here pairs the existing 'at ends' behaviour with the 'at last end' variant, so
# the shared helper cannot be changed for one without the other being checked.

z <- structure(c(1L, 2L, 3L, 4L, 5L, 2L, 3L, 4L, 5L, 6L), .Dim = c(5L, 2L),
    .Dimnames = list(c("T", "U", "V", "W", "X"), c("A", "B")))

# Marker sizes plotly draws for each named series. Hidden markers are size 0, and plotly is
# only given the points between the first and last finite value, so the vector can be
# shorter than the series when values are missing at either end.
markerSizes <- function(...)
{
    pb <- plotly::plotly_build(suppressWarnings(Line(...))$htmlwidget)
    traces <- Filter(function(tr) !is.null(tr$marker$size), pb$x$data)
    setNames(lapply(traces, function(tr) as.numeric(tr$marker$size)),
             vapply(traces, function(tr) as.character(tr$name)[1], character(1)))
}

# Zero-based indices of the points whose data label differs from the series default
labelOverrides <- function(...)
{
    lapply(attr(suppressWarnings(Line(...)), "ChartLabels")$SeriesLabels,
           function(lab) vapply(lab$CustomPoints, function(cp) cp$Index, numeric(1)))
}

# The invisible marker plotly draws under each data label, one series per element, in the
# order the series are charted. Its size is what holds the text clear of the line, so it is
# the only place the offset can be read back from. Deliberately not wrapped in
# suppressWarnings: a length mismatch here shows up as a warning before it shows up as a
# wrong number.
labelOffsets <- function(...)
{
    pb <- plotly::plotly_build(Line(...)$htmlwidget)
    traces <- Filter(function(tr) isTRUE(tr$mode == "markers+text"), pb$x$data)
    lapply(traces, function(tr) as.numeric(tr$marker$size))
}

test_that("Markers at ends are drawn at the first and last point of each series", {
    sizes <- markerSizes(z, marker.show.at.ends = TRUE, marker.size = 8)
    expect_equal(sizes[["A"]], c(8, 0, 0, 0, 8))
    expect_equal(sizes[["B"]], c(8, 0, 0, 0, 8))
})

test_that("Markers at the last end are drawn only at the final point of each series", {
    sizes <- markerSizes(z, marker.show.at.last.end = TRUE, marker.size = 8)
    expect_equal(sizes[["A"]], c(0, 0, 0, 0, 8))
    expect_equal(sizes[["B"]], c(0, 0, 0, 0, 8))
})

test_that("Data labels at ends are shown at the first and last point of each series", {
    # 5 points, so hiding 1, 2 and 3 leaves the labels at points 0 and 4
    expect_equal(labelOverrides(z, data.label.show.at.ends = TRUE),
                 list(c(1, 2, 3), c(1, 2, 3)))
})

test_that("Data labels at the last end are shown only at the final point of each series", {
    expect_equal(labelOverrides(z, data.label.show.at.last.end = TRUE),
                 list(c(0, 1, 2, 3), c(0, 1, 2, 3)))
})

test_that("Missing values at the end of a series move the mark to the last finite point", {
    zna <- z
    zna[5, 1] <- NA        # series A ends at point 4
    zna[4:5, 2] <- NA      # series B ends at point 3

    ends <- markerSizes(zna, marker.show.at.ends = TRUE, marker.size = 8)
    expect_equal(ends[["A"]], c(8, 0, 0, 8))
    expect_equal(ends[["B"]], c(8, 0, 8))

    last <- markerSizes(zna, marker.show.at.last.end = TRUE, marker.size = 8)
    expect_equal(last[["A"]], c(0, 0, 0, 8))
    expect_equal(last[["B"]], c(0, 0, 8))
})

test_that("Missing values at the start of a series do not move the last end", {
    zna <- z
    zna[1:2, 1] <- NA
    last <- markerSizes(zna, marker.show.at.last.end = TRUE, marker.size = 8)
    expect_equal(last[["A"]], c(0, 0, 8))   # points 3..5, marked at the last
})

test_that("A series with one finite point marks that point under either setting", {
    zone <- z
    zone[2:5, 1] <- NA     # series A has a single finite value, so its ends coincide
    expect_equal(markerSizes(zone, marker.show.at.ends = TRUE, marker.size = 8)[["A"]], 8)
    expect_equal(markerSizes(zone, marker.show.at.last.end = TRUE, marker.size = 8)[["A"]], 8)
})

test_that("Showing at the last end overrides showing at both ends", {
    sizes <- markerSizes(z, marker.show.at.ends = TRUE, marker.show.at.last.end = TRUE,
                         marker.size = 8)
    expect_equal(sizes[["A"]], c(0, 0, 0, 0, 8))

    expect_equal(labelOverrides(z, data.label.show.at.ends = TRUE,
                                data.label.show.at.last.end = TRUE),
                 list(c(0, 1, 2, 3), c(0, 1, 2, 3)))
})

test_that("Showing at the last end overrides an explicit per-point data.label.show", {
    # The documented contract for at.ends, which at.last.end inherits: the flag wins
    shown <- matrix(TRUE, 5, 2)
    expect_equal(labelOverrides(z, data.label.show = shown, data.label.show.at.ends = TRUE),
                 list(c(1, 2, 3), c(1, 2, 3)))
    expect_equal(labelOverrides(z, data.label.show = shown,
                                data.label.show.at.last.end = TRUE),
                 list(c(0, 1, 2, 3), c(0, 1, 2, 3)))
})

test_that("The two settings each mark different points independently", {
    # Labels at both ends while markers are only at the last one: the single shared ends
    # matrix the old code built cannot express this
    sizes <- markerSizes(z, data.label.show.at.ends = TRUE, marker.show.at.last.end = TRUE,
                         marker.size = 8)
    expect_equal(sizes[["A"]], c(0, 0, 0, 0, 8))
    expect_equal(labelOverrides(z, data.label.show.at.ends = TRUE,
                                marker.show.at.last.end = TRUE),
                 list(c(1, 2, 3), c(1, 2, 3)))
})

test_that("A data label clears whatever is drawn at its own point", {
    # Half the line thickness where the line is all there is, and the marker's own size
    # where a marker is drawn. Sized per point so that a label reading its neighbour's
    # marker rather than its own is visible in the numbers.
    sizes <- cbind(c(10, 20, 30, 40, 50), c(11, 21, 31, 41, 51))

    expect_warning(offsets <- labelOffsets(z, marker.show.at.ends = TRUE,
        data.label.show = TRUE, marker.size = sizes, line.thickness = 4), NA)
    expect_equal(offsets, list(c(10, 2, 2, 2, 50), c(11, 2, 2, 2, 51)))

    expect_warning(offsets <- labelOffsets(z, marker.show.at.last.end = TRUE,
        data.label.show = TRUE, marker.size = sizes, line.thickness = 4), NA)
    expect_equal(offsets, list(c(2, 2, 2, 2, 50), c(2, 2, 2, 2, 51)))

    # Labels only where a marker is not, so the marker sizes never apply
    expect_warning(offsets <- labelOffsets(z, marker.show.at.last.end = TRUE,
        data.label.show = cbind(c(TRUE, TRUE, FALSE, FALSE, FALSE),
                                c(TRUE, TRUE, FALSE, FALSE, FALSE)),
        marker.size = sizes, line.thickness = 4), NA)
    expect_equal(offsets, list(c(2, 2), c(2, 2)))
})

test_that("markersAreDrawn reports markers requested at either kind of end", {
    f <- flipStandardCharts:::markersAreDrawn
    expect_true(f(NULL, TRUE, FALSE, 2, 5))
    expect_true(f(NULL, FALSE, TRUE, 2, 5))
    expect_false(f(NULL, FALSE, FALSE, 2, 5))
    expect_true(f(TRUE, FALSE, FALSE, 2, 5))
})

test_that("endPointsMatrix marks the finite extremes of each series", {
    f <- flipStandardCharts:::endPointsMatrix
    m <- cbind(c(1, 2, 3, NA), c(NA, 2, 3, 4))

    expect_equal(f(m), cbind(c(TRUE, FALSE, TRUE, FALSE), c(FALSE, TRUE, FALSE, TRUE)))
    expect_equal(f(m, last.only = TRUE),
                 cbind(c(FALSE, FALSE, TRUE, FALSE), c(FALSE, FALSE, FALSE, TRUE)))

    # A series with no finite value marks nothing rather than erroring
    expect_equal(f(cbind(c(NA_real_, NA_real_))), cbind(c(FALSE, FALSE)))
})

test_that("Automatic placement is used when only the last end requests data labels", {
    expect_s3_class(suppressWarnings(
        Line(z, data.label.auto.placement = TRUE,
             data.label.show.at.last.end = TRUE))$htmlwidget, "rhtmlCombinedScatter")
})

test_that("labeledLine sizes markers at the ends the same way the plotly chart does", {
    radiusOf <- function(...)
        suppressWarnings(Line(..., data.label.auto.placement = TRUE,
                              data.label.show = TRUE))$htmlwidget$x$pointRadius

    expect_equal(radiusOf(z, marker.show.at.ends = TRUE, marker.size = c(6, 14)),
                 c(3, 0, 0, 0, 3, 7, 0, 0, 0, 7))
    expect_equal(radiusOf(z, marker.show.at.last.end = TRUE, marker.size = c(6, 14)),
                 c(0, 0, 0, 0, 3, 0, 0, 0, 0, 7))
})
