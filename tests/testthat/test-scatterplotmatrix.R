context("ScatterplotMatrix")

# Long text used for footer, for checking line wrap
filler.text <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."

test_that("ScatterplotMatrix",
{
    expect_error(ScatterplotMatrix(iris, global.font.family = "Courier New",
        title = "Title", subtitle = "Some more description", footer = filler.text), NA)
})
