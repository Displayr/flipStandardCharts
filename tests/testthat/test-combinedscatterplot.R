test_that("x and y",
{
    expect_error(CombinedScatter(1:10, 1:10), NA)
})

test_that("scatter labels",
{
    expect_error(CombinedScatter(1:10, 1:10, scatter.labels = letters[1:10],
                                 scatter.labels.name = "letters",
                                 scatter.labels.as.hovertext = FALSE), NA)
})

test_that("scatter sizes",
{
    expect_error(CombinedScatter(1:10, 1:10, scatter.sizes = 1:10,
                                 scatter.sizes.name = "sizes"), NA)
})

test_that("scatter sizes as diameter",
{
    expect_error(CombinedScatter(1:10, 1:10, scatter.sizes = 1:10,
                                 scatter.sizes.as.diameter = TRUE), NA)
})

test_that("scatter colors",
{
    expect_error(CombinedScatter(1:4, 1:4, colors = c("red", "green"),
                                 scatter.colors = c(1,1,2,2),
                                 scatter.colors.name = "red and green"), NA)
})

test_that("scatter colors as numeric",
{
    expect_error(CombinedScatter(1:4, 1:4, colors = c("red", "green"),
                                 scatter.colors = c(1,1.2,1.6,2),
                                 scatter.colors.as.categorical = FALSE), NA)
})

test_that("scatter max labels",
{
    expect_warning(CombinedScatter(1:10, 1:10, scatter.labels = letters[1:10],
                                  scatter.labels.name = "letters",
                                  scatter.labels.as.hovertext = FALSE,
                                  scatter.max.labels = 5),
                                  "Some labels have been hidden")
})

test_that("trend lines",
{
    expect_error(CombinedScatter(1:4, 1:4, colors = c("red", "green"),
                                 scatter.colors = c(1,1,2,2),
                                 scatter.colors.name = "red and green", trend.lines = TRUE), NA)
})

test_that("logos",
{
    # url <- "https://www.qresearchsoftware.com/wp-content/themes/q/assets/images/Q-Instagram.svg"
    url <- "https://displayrcors.displayr.com/images/apple_grey.svg"
    expect_error(CombinedScatter(1:2, 1:2, logos = c(url, url),
                                 logo.size = c(0.5, 2),
                                 scatter.labels.as.hovertext = FALSE), NA)
})

test_that("no legend",
{
    expect_error(CombinedScatter(1:4, 1:4, colors = c("red", "green"),
                                 scatter.colors = c(1,1,2,2),
                                 scatter.colors.name = "red and green",
                                 legend.show = FALSE), NA)
})

test_that("legend orientation",
{
    expect_error(CombinedScatter(1:4, 1:4, colors = c("red", "green"),
                                 scatter.colors = c(1,1,2,2),
                                 scatter.colors.name = "red and green",
                                 legend.orientation = "Horizontal"), NA)
})

test_that("legend wrap",
{
    expect_error(CombinedScatter(1:4, 1:4, colors = c("red", "green"),
                                 scatter.colors = c("this is red",
                                                    "this is red",
                                                    "this is green",
                                                    "this is green"),
                                 scatter.colors.name = "red and green",
                                 legend.wrap.nchar = 3), NA)
})

test_that("global font family and color",
{
    expect_error(CombinedScatter(1:4, 1:4, colors = c("red", "green"),
                                 scatter.colors = c(1,1,2,2),
                                 scatter.colors.name = "red and green",
                                 global.font.color = "blue",
                                 global.font.family = "Courier New",
                                 title = "title",
                                 subtitle = "subtitle",
                                 x.title = "x title",
                                 y.title = "y title"), NA)
})

test_that("title, subtitle and footer font",
{
    expect_error(CombinedScatter(1:4, 1:4, colors = c("red", "green"),
                                 scatter.colors = c(1,1,2,2),
                                 scatter.colors.name = "red and green",
                                 title = "title",
                                 title.font.family = "Courier New",
                                 title.font.color = "red",
                                 title.font.size = 20,
                                 subtitle = "subtitle",
                                 subtitle.font.family = "Impact",
                                 subtitle.font.color = "green",
                                 subtitle.font.size = 16,
                                 footer = "footer",
                                 subtitle.font.family = "Times New Roman",
                                 subtitle.font.color = "blue",
                                 subtitle.font.size = 12,), NA)
})

