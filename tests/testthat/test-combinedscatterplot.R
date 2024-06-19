test_that("x and y",
{
    expect_error(CombinedScatter(1:10, 1:10), NA)
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

test_that("scatter labels",
{
    expect_error(CombinedScatter(1:10, 1:10, scatter.labels = letters[1:10],
                                 scatter.labels.name = "letters",
                                 scatter.labels.as.hovertext = FALSE), NA)
})

test_that("scatter max labels",
{
    expect_warning(CombinedScatter(1:10, 1:10, scatter.labels = letters[1:10],
                                  scatter.labels.name = "letters",
                                  scatter.labels.as.hovertext = FALSE,
                                  scatter.max.labels = 5),
                                  "Some labels have been hidden")
})

test_that("scatter label autoplacement off",
{
    expect_error(CombinedScatter(1:26, rep(0, 26),
                                 scatter.labels = paste0("letter ", letters),
                                 scatter.labels.name = "letters",
                                 scatter.labels.as.hovertext = FALSE,
                                 label.auto.placement = FALSE), NA)
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

test_that("legend position",
{
    expect_error(CombinedScatter(1:4, 1:4, colors = c("red", "green"),
                                 scatter.colors = c(1,1,2,2),
                                 scatter.colors.name = "red and green",
                                 legend.orientation = "Horizontal",
                                 legend.position.x = 0.5,
                                 legend.position.y = -0.05), NA)
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

test_that("legend font",
{
    expect_error(CombinedScatter(1:4, 1:4, scatter.sizes = 1:4,
                                 colors = c("red", "green"),
                                 scatter.colors = c("this is red",
                                                    "this is red",
                                                    "this is green",
                                                    "this is green"),
                                 scatter.colors.name = "red and green",
                                 legend.font.color = "blue",
                                 legend.font.family = "Courier New",
                                 legend.font.size = 20), NA)
})

test_that("legend bubbles hide",
          {
              expect_error(CombinedScatter(1:4, 1:4, scatter.sizes = 1:4,
                                           colors = c("red", "green"),
                                           scatter.colors = c("this is red",
                                                              "this is red",
                                                              "this is green",
                                                              "this is green"),
                                           scatter.colors.name = "red and green",
                                           legend.font.color = "blue",
                                           legend.font.family = "Courier New",
                                           legend.font.size = 20), NA)
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
                                 footer.font.family = "Times New Roman",
                                 footer.font.color = "blue",
                                 footer.font.size = 28), NA)
})

test_that("footer wrap",
{
    expect_error(CombinedScatter(1:4, 1:4, footer = "long footer",
                                 footer.wrap.nchar = 5), NA)
})

test_that("data label font",
{
    expect_error(CombinedScatter(1:10, 1:10, scatter.labels = letters[1:10],
                                 scatter.labels.name = "letters",
                                 scatter.labels.as.hovertext = FALSE,
                                 data.label.font.family = "Courier New",
                                 data.label.font.color = "red",
                                 data.label.font.size = 20), NA)
})

test_that("data label autocolor",
{
    expect_error(CombinedScatter(1:10, 1:10, scatter.labels = letters[1:10],
                                 colors = c("red", "green"),
                                 scatter.colors = c(rep(1,5), rep(2,5)),
                                 scatter.labels.name = "letters",
                                 scatter.labels.as.hovertext = FALSE,
                                 data.label.font.autocolor = TRUE), NA)
})

test_that("data label format",
{
    expect_error(CombinedScatter(1:10, 1:10, scatter.labels = 1:10,
                                 scatter.labels.name = "numbers",
                                 scatter.labels.as.hovertext = FALSE,
                                 data.label.format = "%"), NA)
})

test_that("marker opacity",
{
    expect_error(CombinedScatter(1:10, 1:10, scatter.labels = 1:10,
                                 scatter.labels.name = "numbers",
                                 scatter.labels.as.hovertext = FALSE,
                                 opacity = 0.5), NA)
})

test_that("background and charting area fill color",
{
    expect_error(CombinedScatter(1:10, 1:10,
                                 background.fill.color = "red",
                                 charting.area.fill.color = "green"), NA)
})

test_that("margin autoexpand",
{
    expect_error(CombinedScatter(1:10, 1:10,
                                 margin.autoexpand = FALSE), NA)
})

test_that("margin",
{
    expect_error(CombinedScatter(1:10, 1:10,
                                 margin.top = 100, margin.bottom = 150,
                                 margin.left = 200, margin.right = 50), NA)
})

test_that("hide grid",
{
    expect_error(CombinedScatter(1:10, 1:10, grid.show = FALSE), NA)
})

test_that("x and y title font",
{
    expect_error(CombinedScatter(1:10, 1:10,
                                 x.title = "x title",
                                 x.title.font.color = "red",
                                 x.title.font.family = "Courier New",
                                 x.title.font.size = 20,
                                 y.title = "y title",
                                 y.title.font.color = "green",
                                 y.title.font.family = "Impact",
                                 y.title.font.size = 30), NA)
})

test_that("x and y tick marks",
{
    expect_error(CombinedScatter(1:10, 1:10,
                                 x.tick.mark.length = 10,
                                 x.tick.mark.color = "red",
                                 y.tick.mark.length = 20,
                                 y.tick.mark.color = "green"), NA)
})

test_that("x and y tick distance and maxnum",
{
    expect_error(CombinedScatter(1:10, 1:10,
                                 x.tick.distance = 5,
                                 y.tick.distance = 2.5), NA)

    expect_error(CombinedScatter(1:10, 1:10,
                                 x.tick.maxnum = 5,
                                 y.tick.maxnum = 3), NA)
})

test_that("x and y tick hide",
{
    expect_error(CombinedScatter(1:10, 1:10,
                                 x.tick.show = FALSE,
                                 y.tick.show = FALSE), NA)
})

test_that("x and y tick prefix and suffix",
{
    expect_error(CombinedScatter(1:10, 1:10,
                                 x.tick.prefix = "<",
                                 x.tick.suffix = ">",
                                 y.tick.prefix = "[",
                                 y.tick.suffix = "]"), NA)
})

test_that("x and y tick font",
{
    expect_error(CombinedScatter(1:10, 1:10,
                                 x.tick.font.color = "red",
                                 x.tick.font.family = "Courier New",
                                 x.tick.font.size = 20,
                                 y.tick.font.color = "green",
                                 y.tick.font.family = "Impact",
                                 y.tick.font.size = 30), NA)
})

test_that("x and y zero line width and color",
{
    expect_error(CombinedScatter(-5:5, -5:5,
                                 x.zero.line.color = "red",
                                 x.zero.line.width = 3,
                                 y.zero.line.color = "green",
                                 y.zero.line.width = 5), NA)
})

test_that("x and y grid width and color",
{
  expect_error(CombinedScatter(-5:5, -5:5,
                               x.grid.color = "red",
                               x.grid.width = 3,
                               y.grid.color = "green",
                               y.grid.width = 5), NA)
})

test_that("x and y bounds",
{
    expect_error(CombinedScatter(1:10, 1:10,
                                 x.bounds.minimum = 2,
                                 x.bounds.maximum = 7,
                                 y.bounds.minimum = 3,
                                 y.bounds.maximum = 9), NA)

    expect_error(CombinedScatter(1:10, 1:10,
                                 x.bounds.minimum = -1,
                                 x.bounds.maximum = 13,
                                 y.bounds.minimum = -3,
                                 y.bounds.maximum = 12), NA)

    expect_error(CombinedScatter(1:10, 1:10,
                                 x.bounds.minimum = 10,
                                 x.bounds.maximum = 0,
                                 y.bounds.minimum = 10,
                                 y.bounds.maximum = 0), NA)
})

test_that("x tick label wrap",
{
    expect_error(CombinedScatter(paste0("letter ", letters[1:10]), 1:10,
                                 x.tick.label.wrap.nchar = 6), NA)
})

test_that("x tick label angle",
{
    expect_error(CombinedScatter(paste0("letter ", letters[1:10]), 1:10, x.tick.angle = 90), NA)
})


test_that("hovertext font",
{
  expect_error(CombinedScatter(1:10, 1:10,
                               hovertext.font.family = "Courier New",
                               hovertext.font.size = 20), NA)
})

test_that("marker size",
{
    expect_error(CombinedScatter(1:10, 1:10,
                                 marker.size = 20), NA)

    expect_error(CombinedScatter(1:10, 1:10, scatter.sizes = 1:10,
                                 marker.size = 20), NA)
})

test_that("swap x and y",
{
    expect_error(CombinedScatter(0:10, -5:5,
                                 x.title = "x title", y.title = "y title",
                                 swap.x.and.y = TRUE), NA)
})

test_that("small multiples",
{
    expect_warning(CombinedScatter(iris, scatter.groups.column = 6,
                                 scatter.colors.column = 1,
                                 colors = c("#FF0000", "#FFFFFF", "#0000FF"),
                                 scatter.colors.as.categorical = FALSE), "overlapping points")
    expect_warning(CombinedScatter(iris, scatter.groups.column = 5,
                                 scatter.colors.column = 5,
                                 scatter.colors.as.categorical = TRUE,
                                 panel.title.font.color="#FF0000"), "overlapping points")
    expect_warning(CombinedScatter(x=iris$Petal.Length, y = iris$Petal.Width,
                    scatter.sizes = iris$Sepal.Length, scatter.colors = iris$Sepal.Width,
                    scatter.groups = iris$Species, colors = c("#FF0000", "#0000FF"),
                    scatter.colors.as.categorical = FALSE), "overlapping points")
    expect_warning(CombinedScatter(x = iris[,3], y=iris[,4], scatter.groups = iris$Species,
                    fit.type = "Loess", fit.CI.show = T,
                    scatter.colors=rep(1:5, each=30),
                    scatter.colors.as.categorical = T), "overlapping points")
    expect_error(CombinedScatter(x = 1:10, y = 1:10,
                    scatter.groups = rep(LETTERS[1:2], each=5),
                    scatter.colors=rep(1:2, 5), scatter.labels=letters[1:10],
                    scatter.labels.as.hovertext = F), NA)
})
