context("Grid line type (RS-22447)")

# The "Grid line type" control (Solid/Dot/Dash) must be applied to the grid
# lines for the plotly-based charts. Previously the setting was silently
# dropped because the chart functions had no grid dash parameter and
# setAxis() never emitted a `griddash` attribute (only the zero line honoured
# its dash, via zerolines()).

dat <- cbind(A = 1:5, B = 5:1)
rownames(dat) <- letters[1:5]

# Charts with independent x and y axes both built via setAxis().
xy.funcs <- c("Line", "Area", "Column", "Bar", "Scatter",
              "BarMultiColor", "ColumnMultiColor", "Pyramid")

test_that("Grid line type applied to x and y axes", {
    for (func in xy.funcs)
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

# Distribution family shares one values axis (built via setAxis); the
# categories axis is drawn separately and carries no griddash.
test_that("Grid line type applied to the values axis of distribution charts", {
    for (func in c("Box", "Bean", "Density", "Violin", "Histogram"))
    {
        cmd <- paste0(func, "(dat, values.grid.width = 1, values.grid.dash = 'Dash')")
        pp <- suppressWarnings(eval(parse(text = cmd)))
        built <- plotly::plotly_build(pp$htmlwidget)$x$layout
        griddashes <- unlist(lapply(built[grepl("axis", names(built))],
                                    function(ax) ax$griddash))
        expect_true("dash" %in% griddashes, info = func)
    }
})
