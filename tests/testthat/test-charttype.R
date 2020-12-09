context("ChartType attribute")

# Using date format rownames because this is needed for TimeSeries and Streamgraph
set.seed(123456)
dat.1d <- structure(abs(rnorm(10)), .Names=sprintf("2020-01-%s", 1:10))
dat.2d <- matrix(rpois(60, 4) + 1, 20, 3, dimnames = list(sprintf("%02d/01/2020", 1:20), LETTERS[1:3]))
charting.funcs <- c("Area", "Bar", "BarMultiColor", "BarPictograph", "Bean", "Box",
    "Column", "ColumnMultiColor", "Density", "Donut", "Histogram", "Heat",
    "LabeledScatter", "Line", "Palm", "Pie", "Radar", "Scatter",
    "Stream", "TimeSeries", "Violin")

test_that("ChartType attribute",
{
    # Just checking that the attribute exist - it may change later
    # Note that ChartData is appended later in CChart (after PrepareData)
    for (func in charting.funcs)
    {
         cmd <- paste0("pp <- ", func, "(dat.1d)")
         expect_error(eval(parse(text=cmd)), NA)
         expect_true(!is.null(attr(pp, "ChartType")))
    }

    # Some other chart types with different input type requirements
    pp <- GeographicMap(c(France = 1, Germany = 2, Spain = 3))
    expect_true(!is.null(attr(pp, "ChartType")))

    r.output <- list(
        list("sets"= list(0), "label"= "Like", "size"= 100),
        list("sets"= list(1), "label"= "Love", "size"= 50),
        list("sets"= list(2), "label"= "Dislike", "size"= 100),
        list("sets"= list(3), "label"= "Hate", "size"= 50),
        list("sets"= list(0, 1), "size"= 50),
        list("sets"= list(0, 2), "size"= 0),
        list("sets"= list(2, 3), "size"= 50))
    pp <- Venn(r.output)
    expect_true(!is.null(attr(pp, "ChartType")))
})

test_that("Use charting options",
{
    set.seed(1234)
    dat <- data.frame('Score' = rnorm(20),
                      'Cost ($)' = abs(rnorm(20)), # check plotly is handling '$' properly
                      'Age' = rpois(20, 40),
                      'Class' = factor(sample(LETTERS[4:1], 20, replace = TRUE), levels = LETTERS[4:1]), # reverse order to check DS-1645
                      'Sporadic' = c(1:5, NA, 6:10, NA, NA, 11:12, NA, NA, 13:15), # missing values
                      'Date' = as.Date(sprintf("2017-01-%02d", 20:1)),
                       check.names = FALSE, stringsAsFactors = FALSE)
    rownames(dat) <- letters[1:20]

    pp <- Scatter(dat)
    expect_equal(attr(pp, "ChartType"), "Bubble")
    pp <- Scatter(dat, scatter.sizes.column = 0)
    expect_equal(attr(pp, "ChartType"), "X Y Scatter")
    pp <- LabeledScatter(dat)
    expect_equal(attr(pp, "ChartType"), "Bubble")
    pp <- LabeledScatter(dat, scatter.sizes.column = 0)
    expect_equal(attr(pp, "ChartType"), "X Y Scatter")

    dat2 <- structure(list(Length = c(1, 2, 4, 5, 7),
            Width = c(6, 2, 4, 2, 4), Random = c(1, 2, 3, 4, 5),
            Class = c("X", "X", "Y", "Y", "Y")), .Names = c("Length", "Width",
            "Random", "Class"), row.names = c("a", "b", "c", "d", "e"),
            scatter.variable.indices = structure(c(1, 2, 3, 4),
            .Names = c("x", "y", "sizes", "colors")), class = "data.frame")
    pp <- SmallMultiples(dat2, "Scatter")
    expect_equal(attr(pp, "ChartType", colors), "Bubble")
    pp <- SmallMultiples(dat2, "Scatter", scatter.sizes.column = 4)
    expect_equal(attr(pp, "ChartType", colors), "X Y Scatter")

    pp <- Line(dat.2d, marker.show = TRUE)
    expect_equal(attr(pp, "ChartType"), "Line Markers")
    pp <- Line(dat.2d, marker.show = TRUE, marker.show.at.ends = TRUE)
    expect_equal(attr(pp, "ChartType"), "Line")
})





