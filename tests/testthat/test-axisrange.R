context("Axis range")

test_that("Range as a string",
{
    xx <- 1:10
    names(xx) <- letters[xx]
    expect_error(Bar(xx, x.bounds.maximum = ""), NA)
    expect_error(BarMultiColor(xx, x.bounds.maximum = "20"), NA)
    expect_error(Pyramid(-xx, x.bounds.maximum = "-1e1"), NA)
    expect_error(Column(xx, y.bounds.maximum = "2e1"), NA)
    expect_error(Column(xx, y.bounds.maximum = "2,000"), NA)
})

test_that("Format of axis labels",
{
    xx <- c("1-25%", "26-50%", "51-75%", "76-100%", "NET")
    expect_equal(getAxisType(xx, ""), "category")
})
