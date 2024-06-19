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

test_that("Conversion of factor with numeric levels to numeric",
{
    xx <- factor(3:1, labels = c("4", "5", "7"))
    expect_equal(convertAxis(xx, "numeric"), c(7, 5, 4))
})

