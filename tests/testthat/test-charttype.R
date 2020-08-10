context("ChartType attribute")

# Using date format rownames because this is needed for TimeSeries and Streamgraph
set.seed(123456)
dat.1d <- structure(abs(rnorm(10)), .Names=sprintf("2020-01-%s", 1:10))
dat.2d <- matrix(rpois(60, 4) + 1, 20, 3, dimnames = list(sprintf("%02d/01/2020", 1:20), LETTERS[1:3]))
charting.funcs <- c("Area", "Bar", "BarMultiColor", "Column", "ColumnMultiColor",
    "Donut", "LabeledScatter", "Line", "Pie", "Scatter", "Stream", "TimeSeries")

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

    # 2D Pie has no similar Excel charttype
    pp <- Pie(dat.2d)
    expect_true(is.null(attr(pp, "ChartType")))
})




