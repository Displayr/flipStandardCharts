context("BarPictograph")

test_that("Colors of incorrect length",
{
    expect_error(BarPictograph(1:6, colors=rainbow(5)), NA)
})
