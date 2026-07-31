context("marker settings for PPT export")

# Every chart here has two series and five points, so an index numbered across the whole
# chart rather than within its series would show up: series 2 would run 5..9 instead of 0..4.

z <- structure(c(1L, 2L, 3L, 4L, 5L, 2L, 3L, 4L, 5L, 6L), .Dim = c(5L, 2L),
    .Dimnames = list(c("T", "U", "V", "W", "X"), c("A", "B")))

pointsOf <- function(...) attr(suppressWarnings(Line(...)), "CustomPoints")
indicesOf <- function(series) vapply(series, function(p) p$Index, numeric(1))
sizesOf <- function(series) vapply(series, function(p) p$Size, numeric(1))

test_that("Marker points are emitted for both line chart paths", {
    expect_false(is.null(pointsOf(z, marker.show.at.ends = TRUE)))
    expect_false(is.null(pointsOf(z, marker.show.at.ends = TRUE, data.label.show = TRUE,
                                  data.label.auto.placement = TRUE)))
})

test_that("The index convention is recorded on the list", {
    expect_equal(attr(pointsOf(z, marker.show.at.ends = TRUE), "IndexBase"), "series")
})

test_that("No marker points are emitted when every point shows a marker", {
    pp <- Line(z, marker.show = TRUE)
    expect_equal(attr(pp, "ChartType"), "Line Markers")
    pts <- attr(pp, "CustomPoints")
    expect_length(pts, 2)
    expect_equal(lengths(pts), c(0L, 0L))
})

test_that("No marker points are emitted when no point shows a marker", {
    # Empty for a different reason than above: nothing to turn on rather than nothing to
    # override. ChartType is what tells the two apart downstream.
    pp <- Line(z, marker.show = FALSE)
    expect_equal(attr(pp, "ChartType"), "Line")
    pts <- attr(pp, "CustomPoints")
    expect_length(pts, 2)
    expect_equal(lengths(pts), c(0L, 0L))
})

test_that("Markers at ends are numbered within their own series", {
    pts <- pointsOf(z, marker.show.at.ends = TRUE)
    expect_length(pts, 2)
    expect_equal(indicesOf(pts[[1]]), c(0, 4))
    expect_equal(indicesOf(pts[[2]]), c(0, 4))   # not c(5, 9)
})

test_that("Markers at the last end emit a single point per series", {
    pts <- pointsOf(z, marker.show.at.last.end = TRUE)
    expect_equal(indicesOf(pts[[1]]), 4)
    expect_equal(indicesOf(pts[[2]]), 4)
})

test_that("Each marker point carries the size of its own series", {
    pts <- pointsOf(z, marker.show.at.ends = TRUE, marker.size = c(6, 14))
    expect_equal(sizesOf(pts[[1]]), c(6, 6))
    expect_equal(sizesOf(pts[[2]]), c(14, 14))
})

test_that("A per-point marker.show matrix selects exactly the points it names", {
    shown <- matrix(FALSE, 5, 2)
    shown[c(2, 4), 1] <- TRUE
    shown[3, 2] <- TRUE
    pts <- pointsOf(z, marker.show = shown)
    expect_equal(indicesOf(pts[[1]]), c(1, 3))
    expect_equal(indicesOf(pts[[2]]), 2)
})

test_that("Missing values move a marker point to the last finite point of its series", {
    zna <- z
    zna[5, 1] <- NA
    zna[4:5, 2] <- NA
    pts <- pointsOf(zna, marker.show.at.last.end = TRUE)
    expect_equal(indicesOf(pts[[1]]), 3)
    expect_equal(indicesOf(pts[[2]]), 2)
})

test_that("The labeledLine path emits the same marker points as the plotly path", {
    plotly.pts <- pointsOf(z, marker.show.at.ends = TRUE, marker.size = c(6, 14))
    labeled.pts <- pointsOf(z, marker.show.at.ends = TRUE, marker.size = c(6, 14),
                            data.label.show = TRUE, data.label.auto.placement = TRUE)
    expect_equal(labeled.pts, plotly.pts)
})

test_that("An average series does not gain marker points of its own", {
    pts <- pointsOf(z, marker.show.at.ends = TRUE, average.series = rep(3, 5))
    expect_length(pts, 2)   # the average series is not one of the charted series
})
