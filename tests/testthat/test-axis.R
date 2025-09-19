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

test_that("setTicks",
{
    ticks <- setTicks(NULL, NULL, NULL)
    expect_equal(ticks$range, NULL)

    ticks <- setTicks(NULL, 20, NULL, data = 1:5)
    expect_equal(ticks$range, c(0, 20))

    ticks <- setTicks(NULL, 20, NULL, data = -5:5)
    expect_equal(ticks$range, c(-5, 20))

    ticks <- setTicks(NULL, NULL, NULL, data = -46, labels = "-46")
    expect_equal(ticks$range, c(-46, 0))

    ticks <- setTicks(NULL, NULL, NULL, data = -46, labels = "-46", type = "Bar")
    expect_equal(ticks$range, c(-57.5, 0))

    ticks <- setTicks(NULL, 0, NULL, data = -46, labels = "-46", type = "Bar")
    expect_equal(ticks$range, c(-57.5, 0))

    ticks <- setTicks(-50, NULL, NULL, data = -46, labels = "-46")
    expect_equal(ticks$range, c(-50, 0))

    ticks <- setTicks(NULL, NULL, NULL, data = 46, labels = "46")
    expect_equal(ticks$range, c(0, 46))

    ticks <- setTicks(NULL, NULL, NULL, data = 46, labels = "46", type = "Bar")
    expect_equal(ticks$range, c(0, 55.2))

    ticks <- setTicks(40, NULL, NULL, data = 46, labels = "46", type = "Bar")
    expect_equal(ticks$range, c(40, 55.2))

    neg_data <- -50 + 1:5
    neg_labels <- sprintf("%.2f", neg_data)
    ticks <- setTicks(NULL, NULL, NULL, data = neg_data, labels = neg_labels)
    expect_equal(ticks$range, c(-49, -45))

    ticks <- setTicks(NULL, NULL, NULL, data = neg_data, labels = neg_labels, type = "Bar")
    expect_equal(ticks$range, c(-50.6, -45))

    pos_and_neg_data <- -5:5
    pos_and_neg_labels <- sprintf("%.2f", pos_and_neg_data)
    ticks <- setTicks(NULL, NULL, NULL, data = pos_and_neg_data, labels = pos_and_neg_labels, type = "Bar")
    expect_equal(ticks$range, c(-8.5, 8.5))

})

