context("BarPictograph")

test_that("Colors of incorrect length",
{
    expect_error(BarPictograph(1:6, colors=rainbow(5)), NA)
})

test_that("Custom image",
{
    expect_error(BarPictograph(1:6, custom.image = "https://wiki.q-researchsoftware.com/images/7/78/Democrats_donkey_black.png"), NA)
    expect_error(BarPictograph(1:6, custom.image = "tps://wiki.q-researchsoftware.com/images/7/78/Democrats_donkey_black.png"),
                 "Could not retrieve image")
    expect_error(BarPictograph(1:6, custom.image = "https://wiki.q-researchsoftware.com/images/7/78/Demorats_donkey_black.png"),
                 "Error (status code 404) retrieving image", fixed = TRUE)

})


test_that("Large vectors error",
{
    expect_error(BarPictograph(rep(1, 800)), "Input data containing 800 rows is too large to show")
})

test_that("JSON config contains no newlines",
{
    xx <- structure(1:6, .Names = c("a", "b", "c", "d", "e", "f"))
    expect_equal(length(capture.output(BarPictograph(xx, hide.base.image = FALSE,
        data.label.show = TRUE, data.label.position = "Next to bar",
        print.config = TRUE))), 1)
})

test_that("JSON config escapes quotes",
{
    x2 <- structure(1:10, .Names = c("a", "b", "\"C\"", "d's", "e", "f",
        "g", "h", "i", "j"))
    expect_error(BarPictograph(x2), NA)
    expect_error(BarPictograph(x2, data.label.position = "Above row label"), NA)
})
