context("Radar chart")

# Set up various data types to test
set.seed(1234)
unnamed <- abs(rnorm(10))
named <- structure(rpois(6, 20), .Names = c("Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta"))
negatives <- structure(rnorm(25), .Names = letters[1:25])
missing1 <- named
missing1[1] <- NA
single <- c(5)
double <- c(5, 1)
matrix2d <- cbind('Random values' = named, 'More random values' = rpois(6, 21))

# Create lists of all charting functions, data types and options to use
# Include relevant options from test-background.R and test-vectordata.R

dat.list <- c("unnamed", "named", "missing1","single", "double", "matrix2d")

opts <- c('default' = '',
          'datalabel' = 'data.label.show=TRUE, data.label.font.color="red", data.label.font.family="Courier"',
          'backgroundcolors' = 'background.fill.color="grey", charting.area.fill.color="yellow", charting.area.fill.opacity=0.2',
          'grid' = 'x.grid.width=4, y.grid.width=1, y.tick.distance = 2, y.tick.font.size=8, y.tick.font.color="green"',
          'nogrid' = 'grid.show=FALSE, legend.show=FALSE, x.grid.width=1, y.grid.width=1',
          'ygrid' = 'y.grid.width = 0, data.label.show=TRUE, y.bounds.maximum = 5, x.tick.font.family="Arial Black", x.tick.font.size=16',

          'legendpos' = 'legend.position.y=0.5, legend.position.x=0, legend.font.color="red"',
          'legendbg' = 'legend.fill.color="blue", legend.fill.opacity=0.5, legend.border.color="red", legend.border.line.width=2',
          'margins' = 'margin.left=0, margin.right=0, margin.top=0, margin.inner.pad=10, charting.area.fill.color="red", legend.show=FALSE, grid.show=FALSE',
          'font' = 'global.font.family="Courier", global.font.color="red"',
          'nooutline' = 'line.thickness=0, x.tick.font.color="green"',
          'opacity' = 'opacity = 0.9',
          'modebar' = 'modebar.show = TRUE')


for (dat in dat.list)
{
    for (i in 1:length(opts))
    {
        # filestem is both the name of the image in accepted-snapshots
        # and the error msg expected on the output of devtools::test()
        filestem <- paste("radar", dat, names(opts)[i], sep="-")
        test_that(filestem, {

            cmd <- paste0("pp <- Radar(", dat, ",", opts[i], ")")
            if (grepl("missing|negative", filestem))
                expect_error(eval(parse(text=cmd)))
            else if (grepl("single|double", filestem))
                expect_warning(eval(parse(text=cmd)))
            else
            {
                expect_error(eval(parse(text=cmd)), NA)
                #expect_true(TestWidget(pp, filestem))
                #print(pp)
                #readline(prompt=paste0(filestem, ": press [enter] to continue: "))
            }
        })
    }
}

test_that("Adjustable y.bounds.minimum",
{
    expect_error(Radar(negatives), NA)
    expect_error(Radar(negatives, y.bounds.maximum = "", y.bounds.minimum = ""), NA)
    expect_error(Radar(negatives, data.label.show = TRUE, x.tick.show = FALSE), NA)
    expect_error(Radar(negatives[1:5], y.bounds.maximum = -2, y.bounds.minimum = 2,
        data.label.show = TRUE, x.tick.show = FALSE, data.label.format = ".2f"), NA)
    expect_error(Radar(named, y.bounds.minimum = 10), NA)

    expect_error(SmallMultiples(matrix2d, "Radar", data.label.show = TRUE,
        y.bounds.minimum = "", y.bounds.maximum = ""), NA)
    expect_warning(SmallMultiples(matrix2d, "Radar", data.label.show = TRUE,
        y.bounds.minimum = "23", y.bounds.maximum = "12"),
        "Please specify a value outside [12, 26].", fixed = TRUE)
})

test_that("FS2-4532: Radar renders with per-series marker size string", {
    dat <- matrix(c(1, 4, 2, 5, 3, 6), nrow = 2,
                  dimnames = list(c("a", "b"), c("x", "y", "z")))
    expect_error(Radar(dat, marker.show = TRUE, marker.size = "6,10,14"), NA)

    pp <- Radar(dat, marker.show = TRUE, marker.size = "6,10,14")
    pb <- plotly::plotly_build(pp$htmlwidget)
    trace.sizes <- setNames(lapply(pb$x$data, function(tr) tr$marker$size),
                             vapply(pb$x$data, function(tr) as.character(tr$name), character(1)))

    # One trace per series (named after the column), each with the marker size
    # parsed from "6,10,14" recycled across the series' data points. This would
    # fail without readNumericSeries(): pre-fix, marker.size stayed a character
    # matrix and each trace's marker$size would be a character vector like
    # c("6,10,14", "6,10,14") rather than the numeric per-series constant.
    expect_type(trace.sizes[["x"]], "double")
    expect_type(trace.sizes[["y"]], "double")
    expect_type(trace.sizes[["z"]], "double")
    expect_equal(unique(trace.sizes[["x"]]), 6)
    expect_equal(unique(trace.sizes[["y"]]), 10)
    expect_equal(unique(trace.sizes[["z"]]), 14)
})

test_that("FS2-4532: Radar's own opacity accepts the string form without erroring", {
    # opacity is per series for Radar too; the string form is what the Plugins pass, and
    # vectorize() alone leaves it as character, which fails downstream in toRGB.
    expect_error(Radar(matrix2d, opacity = "0.5, 0.9"), NA)
})

test_that("FS2-4532: marker.opacity accepts every input form without erroring", {
    # A comma-separated string is what the Plugins pass for a per-series setting, and it
    # reached toRGB as text; a vector of more than one value reached toRGB unindexed. Radar
    # has no marker.border.opacity.
    expect_error(suppressWarnings(
        Radar(matrix2d, marker.show = TRUE, marker.opacity = "0.5, 0.9")), NA)
    expect_error(suppressWarnings(
        Radar(matrix2d, marker.show = TRUE, marker.opacity = c(0.5, 0.9))), NA)

    # marker.opacity is a single value by contract, so more than one distinct value warns
    # and the first wins - but only when a marker is actually drawn, so an opacity you
    # cannot see is not worth a warning
    expect_warning(Radar(matrix2d, marker.opacity = c(0.3, 1)), NA)
    expect_warning(Radar(matrix2d, marker.show = TRUE, marker.opacity = c(0.3, 1)),
                   "Only one marker.opacity can be used")

    # One value, or several that agree, is silent
    expect_warning(Radar(matrix2d, marker.show = TRUE, marker.opacity = 0.5), NA)
    expect_warning(Radar(matrix2d, marker.show = TRUE, marker.opacity = "0.5, 0.5"), NA)
})


# The radar polygons are ordinary scatter traces with polar coordinates worked out in
# advance, so they take a line dash the same way the line chart's traces do. Each series
# has more than one trace; the polygon is the first one carrying the series' name.
polygonLine <- function(pp, nm)
{
    pb <- plotly::plotly_build(pp$htmlwidget)
    for (tr in pb$x$data)
        if (identical(as.character(tr$name)[1], nm))
            return(tr$line)
    NULL
}

test_that("Radar draws a line type per series", {
    pp <- Radar(matrix2d, line.type = "Solid, Dot")
    expect_equal(polygonLine(pp, "Random values")$dash, "solid")
    expect_equal(polygonLine(pp, "More random values")$dash, "dot")
})

test_that("Radar accepts a single line type for every series", {
    pp <- Radar(matrix2d, line.type = "Dot")
    expect_equal(polygonLine(pp, "Random values")$dash, "dot")
    expect_equal(polygonLine(pp, "More random values")$dash, "dot")
})

test_that("Radar lines are solid by default", {
    pp <- Radar(matrix2d)
    expect_equal(polygonLine(pp, "Random values")$dash, "solid")
})

test_that("Radar line type is recycled and truncated like its line thickness", {
    # Two series here, so the third line type is dropped
    pp <- Radar(matrix2d, line.type = "Dot, Solid, Dash", line.thickness = c(2, 5))
    expect_equal(polygonLine(pp, "Random values")$dash, "dot")
    expect_equal(polygonLine(pp, "More random values")$dash, "solid")
    expect_equal(polygonLine(pp, "Random values")$width, 2)
    expect_equal(polygonLine(pp, "More random values")$width, 5)

    # A single type covers every series
    pp <- Radar(matrix2d, line.type = "Dash")
    expect_equal(polygonLine(pp, "Random values")$dash, "dash")
    expect_equal(polygonLine(pp, "More random values")$dash, "dash")
})

test_that("Radar reads line thickness the way the line chart does", {
    # readNumericSeries, as marker.size and opacity here already use: a comma-separated
    # string becomes numbers, and a value that is not one is dropped with a warning rather
    # than reaching plotly as text
    expect_warning(Radar(matrix2d, line.thickness = "2, foo"),
                   "Non-numeric line thickness value 'foo' was ignored")

    pp <- Radar(matrix2d, line.thickness = "2, 5")
    expect_true(is.numeric(polygonLine(pp, "Random values")$width))
    expect_equal(polygonLine(pp, "Random values")$width, 2)
    expect_equal(polygonLine(pp, "More random values")$width, 5)
})

test_that("A per-point marker.size matrix reaches every point it names", {
    szmat <- cbind(c(2, 4, 6, 8, 10, 12), c(3, 5, 7, 9, 11, 13))
    pp <- Radar(matrix2d, marker.show = TRUE, marker.size = szmat)
    pb <- plotly::plotly_build(pp$htmlwidget)
    sized <- Filter(function(tr) !is.null(tr$marker$size) && !is.null(tr$name), pb$x$data)
    sizes <- setNames(lapply(sized, function(tr) as.numeric(tr$marker$size)),
                      vapply(sized, function(tr) as.character(tr$name)[1], character(1)))
    # The radar closes its polygon, so each series repeats its first point at the end
    expect_equal(sizes[["Random values"]][1:6], c(2, 4, 6, 8, 10, 12))
    expect_equal(sizes[["More random values"]][1:6], c(3, 5, 7, 9, 11, 13))
})
