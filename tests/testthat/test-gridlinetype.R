context("Grid line type (RS-22447)")

# The "Grid line type" control (Solid/Dot/Dash) must be applied to both the
# x and y grid lines for the plotly-based charts. Previously the setting was
# silently dropped because the chart functions had no grid dash parameter and
# setAxis() never emitted a `griddash` attribute (only the zero line honoured
# its dash, via zerolines()).

dat <- cbind(A = 1:5, B = 5:1)
rownames(dat) <- letters[1:5]

test_that("Grid line type applied to x and y axes", {
    for (func in c("Line", "Area", "Column", "Bar"))
    {
        cmd <- paste0(func, "(dat, x.grid.width = 1, y.grid.width = 1, ",
                      "x.grid.dash = 'Dot', y.grid.dash = 'Dash')")
        pp <- suppressWarnings(eval(parse(text = cmd)))
        built <- plotly::plotly_build(pp$htmlwidget)$x$layout
        expect_equal(built$xaxis$griddash, "dot", info = func)
        expect_equal(built$yaxis$griddash, "dash", info = func)
    }
})

test_that("Grid line type defaults to solid", {
    pp <- suppressWarnings(Line(dat, x.grid.width = 1, y.grid.width = 1))
    built <- plotly::plotly_build(pp$htmlwidget)$x$layout
    expect_equal(built$xaxis$griddash, "solid")
    expect_equal(built$yaxis$griddash, "solid")
})
