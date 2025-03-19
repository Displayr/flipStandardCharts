test_that("x and y",
{
    expect_error(CombinedScatter(1:10, 1:10), NA)
})

test_that("scatter sizes",
{
    expect_error(CombinedScatter(1:10, 1:10, scatter.sizes = 1:10,
                                 scatter.sizes.name = "sizes"), NA)
    expect_warning(CombinedScatter(x=1:10, y=1:10,
                                 scatter.sizes=c(NA, 2:10)),
                   "Some points omitted")
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
    expect_error(CombinedScatter(1:4, 1:4, scatter.colors = factor(c('B', 'B', 'A', 'C'),
                                levels=LETTERS[1:3])), NA)
})

test_that("scatter colors as numeric",
{
    expect_error(CombinedScatter(1:4, 1:4, colors = c("red", "green"),
                                 scatter.colors = c(1,1.2,1.6,2),
                                 scatter.colors.as.categorical = FALSE), NA)
    expect_warning(CombinedScatter(1:4, 1:4, colors = c("red"),
                                 scatter.colors = c(1,1.2,1.6,2),
                                 scatter.colors.as.categorical = FALSE),
                   "Supply a color palette of 2 or more colors")
    expect_warning(CombinedScatter(1:4, 1:4, colors = ChartColors(4),
                                 scatter.colors = c(1,1.2,1.6,2),
                                 scatter.colors.as.categorical = FALSE),
                   "qualitative palette should not be used")
    expect_warning(CombinedScatter(1:10, 1:10,
                    scatter.colors.as.categorical = FALSE),
                   "'Colors' variable not provided")
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

test_that("lines of best fit",
{
    expect_error(CombinedScatter(1:10, rnorm(10), fit.type = "Friedman",
                                 fit.CI.show = FALSE), NA)
    expect_error(CombinedScatter(1:10, rnorm(10), fit.type = "Linear",
                                 fit.CI.show = TRUE), NA)
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
                                 logo.size = 2,
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
                    scatter.labels.as.hovertext = FALSE), NA)
    expect_error(CombinedScatter(1:4, 1:4, scatter.labels = letters[1:4],
                    scatter.labels.as.hovertext = FALSE,
                    scatter.groups = factor(c('B', 'B', 'A', 'C'),
                    levels=LETTERS[1:3])), NA)
})

test_that("small multiples rows",
{
    expect_warning(CombinedScatter(x=iris$Petal.Length,
                                   y = iris$Petal.Width,
                                   scatter.sizes = iris$Sepal.Length,
                                   scatter.colors = iris$Sepal.Width,
                                   scatter.groups = iris$Species,
                                   colors = c("#FF0000", "#0000FF"),
                                   scatter.colors.as.categorical = FALSE,
                                   nrows = 3), "overlapping points")
})

test_that("small multiples share axes",
{
    expect_warning(CombinedScatter(x=iris$Petal.Length,
                                   y = iris$Petal.Width,
                                   scatter.sizes = iris$Sepal.Length,
                                   scatter.colors = iris$Sepal.Width,
                                   scatter.groups = iris$Species,
                                   colors = c("#FF0000", "#0000FF"),
                                   scatter.colors.as.categorical = FALSE,
                                   share.axes = TRUE), "overlapping points")
})

test_that("small multiples reorder",
{
    expect_warning(CombinedScatter(x=iris$Petal.Length,
                                   y = iris$Petal.Width,
                                   scatter.sizes = iris$Sepal.Length,
                                   scatter.colors = iris$Sepal.Width,
                                   scatter.groups = iris$Species,
                                   colors = c("#FF0000", "#0000FF"),
                                   scatter.colors.as.categorical = FALSE,
                                   x.order = "2,3,1"), "overlapping points")
})

test_that("small multiples panel gap",
{
    expect_warning(CombinedScatter(x=iris$Petal.Length,
                                   y = iris$Petal.Width,
                                   scatter.sizes = iris$Sepal.Length,
                                   scatter.colors = iris$Sepal.Width,
                                   scatter.groups = iris$Species,
                                   colors = c("#FF0000", "#0000FF"),
                                   scatter.colors.as.categorical = FALSE,
                                   panel.x.gap = 0.5,
                                   panel.y.gap = 0.5), "overlapping points")
})

test_that("annotations",
{
    dat <- data.frame(x = 1:10, y=1:10)

    annotation.list <- list(list(type = "Arrow - up", data = "x", threstype = "above threshold",
                    threshold = "-Inf", color = "red", size = 12, width = 1,
                    offset = 0, font.family = "Arial", font.weight = "normal",
                    font.style = "normal"))
    expect_error(CombinedScatter(dat, annotation.list = annotation.list), NA)
    expect_error(CombinedScatter(dat, scatter.labels = letters[1:10], scatter.labels.as.hovertext = FALSE, annotation.list = annotation.list), NA)

    annotation.list <- list(list(type = "Shadow", data = "x", threstype = "above threshold",
                                 threshold = "-Inf", color = "red", size = 12, width = 1,
                                 offset = 0, font.family = "Arial", font.weight = "normal",
                                 font.style = "normal"))
    expect_error(CombinedScatter(dat, annotation.list = annotation.list), NA)
    expect_error(CombinedScatter(dat, scatter.labels = letters[1:10], scatter.labels.as.hovertext = FALSE, annotation.list = annotation.list), NA)

    annotation.list <- list(list(type = "Marker border", data = "x", threstype = "above threshold",
                                 threshold = "-Inf", width = 1, color = "red"))
    expect_error(CombinedScatter(dat, annotation.list = annotation.list), NA)
    expect_error(CombinedScatter(dat, scatter.labels = letters[1:10], scatter.labels.as.hovertext = FALSE, annotation.list = annotation.list), NA)
})

test_that("legend hidden",
{
    expect_error(CombinedScatter(1:10, 1:10, scatter.sizes = 1:10,
                                 scatter.sizes.name = "sizes",
                                 legend.show = FALSE), NA)

    expect_error(CombinedScatter(1:4, 1:4, colors = c("red", "green"),
                                 scatter.colors = c(1,1.2,1.6,2),
                                 scatter.colors.as.categorical = FALSE,
                                 legend.show = FALSE), NA)
})

test_that("quadrants",
{
    expect_error(CombinedScatter(1:10,11:20, quadrants.show = TRUE,
                                 x.midpoint.line.color = "#FF0000",
                                 x.midpoint.line.dash = "Dash",
                                 x.midpoint.line.width = 2,
                                 y.midpoint.line.color = "#00FF00",
                                 y.midpoint.line.dash = "Dot",
                                 y.midpoint.line.width = 5), NA)

    expect_error(CombinedScatter(c(1,1,1:10),c(11:20, 20, 20), quadrants.show = TRUE,
                                x.midpoint.type = "Median",
                                y.midpoint.type = "Median"), NA)

    expect_error(CombinedScatter(1:10,11:20, quadrants.show = TRUE,
                                x.midpoint.type = "Value",
                                y.midpoint.type = "Value",
                                x.midpoint.value = 6.5, y.midpoint.value = 13.3), NA)

    expect_error(CombinedScatter(1:10,11:20, quadrants.show = TRUE,
                                 x.midpoint.type = "Value",
                                 y.midpoint.type = "Value",
                                 x.midpoint.value = "6.5", y.midpoint.value = "13.3"), NA)

    expect_error(CombinedScatter(1:10,11:20, quadrants.show = TRUE,
                                 x.midpoint.type = "Calculation",
                                 y.midpoint.type = "Calculation",
                                 x.midpoint.input = 2, y.midpoint.input = 16), NA)

    expect_warning(CombinedScatter(letters[1:10],11:20, quadrants.show = TRUE,
                                   x.midpoint.type = "Value",
                                   y.midpoint.type = "Value",
                                   x.midpoint.value = 6.5, y.midpoint.value = 13.3),
                   "Quadrants cannot be shown as the x-axis has non-numeric data")

    expect_warning(CombinedScatter(1:10,11:20, quadrants.show = TRUE,
                                 x.midpoint.type = "Value",
                                 y.midpoint.type = "Value",
                                 x.midpoint.value = 6.5, y.midpoint.value = 3.3),
                   "The y midpoint line is not shown as it is outside the plot range")

    expect_warning(CombinedScatter(1:10,11:20, quadrants.show = TRUE,
                                   x.midpoint.type = "Average",
                                   y.midpoint.type = "Average",
                                   x.bounds.minimum = 7),
                   "The x midpoint line is not shown as it is outside the plot range")

    expect_warning(CombinedScatter(1:10,11:20, quadrants.show = TRUE,
                                   x.midpoint.type = "Median",
                                   y.midpoint.type = "Median",
                                   x.bounds.minimum = 7),
                   "The x midpoint line is not shown as it is outside the plot range")

    expect_warning(CombinedScatter(1:10,11:20, quadrants.show = TRUE,
                                   x.midpoint.type = "Value",
                                   y.midpoint.type = "Value",
                                   x.midpoint.value = NaN, y.midpoint.value = 3.3),
                   "Quadrants cannot be shown as the x midpoint value is invalid")

    expect_warning(CombinedScatter(1:10,11:20, quadrants.show = TRUE,
                                   x.midpoint.type = "Value",
                                   y.midpoint.type = "Value",
                                   x.midpoint.value = "abc", y.midpoint.value = 3.3),
                   "Quadrants cannot be shown as the x midpoint value is invalid")

    expect_warning(CombinedScatter(1:10,11:20, quadrants.show = TRUE,
                                   x.midpoint.type = "Calculation",
                                   y.midpoint.type = "Calculation",
                                   x.midpoint.input = NaN, y.midpoint.input = 3.3),
                   "Quadrants cannot be shown as the x midpoint value is invalid")

    expect_warning(CombinedScatter(1:10,11:20, quadrants.show = TRUE,
                                   x.midpoint.type = "Calculation",
                                   y.midpoint.type = "Calculation",
                                   x.midpoint.input = c(2,3), y.midpoint.input = 12),
                   "The input for the x midpoint has multiple elements")
})

test_that("Categorical colors and ordering",
{
    # Scatterplot doesn't actually use names but we have added them for readability
    # Functions in flipChartBasics ensures that colors have already been ordered correctly
    # but they don't know where missing values are
    named.colors <- c(`Coca-Cola` = "#FF0000",
                      `Diet Coke` = "#FFC000", `Coke Zero` = "#FFFF00", Pepsi = "#92D050",
                      `Diet Pepsi` = "#00B050", `Pepsi Max` = "#0070C0", `Unnamed values` = "#CCCCCC")
    data.missing <- structure(list(
            X = c("95", "96", NA, "100", "80.5", "91.75"),
            Y = c("95", "96", NA, "100", "82.75", "91"),
            Size = c("1", "1", "1", "1", "1", "1"),
            Colors = c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi", "Diet Pepsi", "Pepsi Max")),
            row.names = c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi", "Diet Pepsi", "Pepsi Max"),
            assigned.rownames = TRUE,
            scatter.variable.indices = c(x = 1, y = 2, sizes = 3, colors = 4, groups = 4),
            class = "data.frame")

    # Note these looks the same as if Scatter() was called instead
    expect_warning(CombinedScatter(data.missing, colors = named.colors), "missing values")
    res <- getColors(NULL, data.missing$Colors, named.colors, nrow(data.missing),
                              which(!is.na(data.missing$X) & !is.na(data.missing$Y)),
                              TRUE, 1, TRUE)
    expect_equal(res$colors, named.colors[-c(3,7)])

    f.manylevels <- factor(letters[c(1,1,3,5,3)], levels = letters[6:1])
    n <- length(f.manylevels)
    reverse.colors <- c(f="red", e="orange", d="yellow", c="green", b="blue", a="purple")
    CombinedScatter(x = 1:5, y = 1:5, scatter.colors.name = "Non-missing levels",
                    colors = reverse.colors, scatter.colors = f.manylevels)
    res <- getColors(NULL, f.manylevels, reverse.colors, n, 1:n, TRUE, 1, TRUE)
    expect_equal(res$colors, reverse.colors[7-c(5,3,1)])


    num.nonseq <- c(7, 1, 21, 5, 5)
    n <- length(num.nonseq)
    CombinedScatter(x = 1:5, y = 1:5, scatter.colors.name = "Non-sequential numbers",
                    colors = reverse.colors, scatter.colors = num.nonseq)
    res <- getColors(NULL, num.nonseq, reverse.colors, n, 1:n, TRUE, 1, TRUE)
    expect_equal(unname(res$colors), unname(reverse.colors[1:4]))
    expect_equal(names(res$colors), as.character(sort(unique(num.nonseq))))

    f.char <- letters[c(1,1,3,5,3)]
    n <- length(f.char)
    CombinedScatter(x = 1:5, y = 1:5, scatter.colors.name = "Alphabetical",
                    colors = rev(reverse.colors), scatter.colors = f.char)
    res <- getColors(NULL, f.char, rev(reverse.colors), n, 1:n, TRUE, 1, TRUE)
    expect_equal(unname(res$colors), rev(unname(reverse.colors))[1:3])
    expect_equal(names(res$colors), letters[c(1,3,5)])
})
