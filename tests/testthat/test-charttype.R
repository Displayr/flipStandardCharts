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





