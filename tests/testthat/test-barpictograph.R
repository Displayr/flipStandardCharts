context("BarPictograph")

test_that("Colors of incorrect length",
{
    expect_error(BarPictograph(1:6, colors=rainbow(5)), NA)
})


test_that("Large vectors error",
{
    expect_error(BarPictograph(rep(1, 800)), "Input data containing 800 rows is too large to show")
})
