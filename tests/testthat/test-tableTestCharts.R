context("Q-file coverage gaps")

named.vector <- structure(c(5, 6, 2, 1.5, 9, 2.2), .Names = c("A", "B", "C", "D", "E", "F"))

named.matrix <- structure(
    c(1.59, 0.44, 2.52, 0.19, 0.71, 0.18, 0.18, 0.61, 0.08,
      1.07, 1.31, 0.45, 0.17, 2.87, 2.08, 0.53, 2.62, 1.88, 1.73, 0.12),
    .Dim = c(5L, 4L),
    .Dimnames = list(
        c("Row 1", "Row 2", "Row 3", "Row 4", "Row 5"),
        c("Col 1", "Col 2", "Col 3", "Col 4")))

two.col.mat <- structure(
    c(10.6, 3.1, 8.1, 9.4, 1.2, 7.5, 8.1, 3.8, 5.6, 7.5,
      12.0, 1.8, 6.6, 9.4, 3.8, 8.1, 7.5, 5.0, 6.9, 3.1),
    .Dim = c(10L, 2L),
    .Dimnames = list(letters[1:10], c("Male", "Female")))

neg.pos.vector <- -50:50

# ---- Line: axis lines ----

test_that("Line - y.line.width and x.line.width", {
    expect_error(print(Chart(named.matrix, type = "Line",
                             y.line.width = 2, y.line.color = "blue",
                             x.line.width = 2, x.line.color = "red")), NA)
})

# ---- Line: grid ----

test_that("Line - y.grid and x.grid styling", {
    expect_error(print(Chart(named.matrix, type = "Line",
                             y.grid.width = 2, y.grid.color = "#cccccc",
                             x.grid.width = 2, x.grid.color = "#dddddd")), NA)
})

# ---- Line: tick visibility ----

test_that("Line - y.tick.show and x.tick.show FALSE", {
    expect_error(print(Chart(named.matrix, type = "Line",
                             y.tick.show = FALSE, x.tick.show = FALSE)), NA)
})

# ---- Line: tick marks ----

test_that("Line - x.tick.marks none", {
    expect_error(print(Chart(named.matrix, type = "Line",
                             x.tick.marks = "none")), NA)
})

test_that("Line - x.tick.mark.length", {
    expect_error(print(Chart(named.matrix, type = "Line",
                             x.tick.mark.length = 20)), NA)
})

# ---- Line: axis position ----

test_that("Line - x.position top", {
    expect_error(print(Chart(named.matrix, type = "Line",
                             x.position = "top")), NA)
})

# ---- Line: y bounds ----

test_that("Line - y.bounds.minimum, y.bounds.maximum, y.tick.distance", {
    expect_error(print(Chart(named.matrix, type = "Line",
                             y.bounds.minimum = -20, y.bounds.maximum = 160,
                             y.tick.distance = 10)), NA)
})

# ---- Line: zero lines ----

test_that("Line - y.zero.line.width and color", {
    y <- -100:100
    expect_error(print(Chart(y, type = "Line",
                             y.zero.line.width = 2, y.zero.line.color = "red")), NA)
})

# ---- Bar: zero line ----

test_that("Bar - x.zero.line.width and color", {
    expect_error(print(Chart(named.matrix, type = "Bar",
                             x.zero.line.width = 5, x.zero.line.color = "red")), NA)
})

# ---- Line: y tick formatting ----

test_that("Line - y.tick.prefix, y.tick.suffix, y.tick.decimals, y.tick.angle", {
    expect_error(print(Chart(named.matrix, type = "Line",
                             y.tick.prefix = "$", y.tick.suffix = "k",
                             y.tick.decimals = 1, y.tick.angle = 45)), NA)
})

# ---- Bar: x tick formatting ----

test_that("Bar - x.tick.prefix, x.tick.suffix, x.tick.decimals, x.tick.angle", {
    expect_error(print(Chart(named.matrix, type = "Bar",
                             x.tick.prefix = "$", x.tick.suffix = "k",
                             x.tick.decimals = 1, x.tick.angle = 45)), NA)
})

# ---- Line: series lines ----

test_that("Line - series.line.width, series.line.colors, series.line.opacity", {
    expect_error(print(Chart(named.matrix, type = "Line",
                             series.line.width = 5,
                             series.line.colors = "Primary colors",
                             series.line.opacity = 0.7)), NA)
})

# ---- Line: series markers ----

test_that("Line - series.marker.show, size, opacity, border", {
    expect_error(print(Chart(named.matrix, type = "Line",
                             series.marker.show = "All",
                             series.marker.size = 10,
                             series.marker.opacity = 0.8,
                             series.marker.border.width = 2,
                             series.marker.border.colors = "Primary colors")), NA)
})

# ---- Line: colors ----

test_that("Line - colors Rainbow", {
    expect_error(print(Chart(named.matrix, type = "Line",
                             transpose = TRUE, colors = "Rainbow")), NA)
})

test_that("Line - colors.reverse", {
    expect_error(print(Chart(named.matrix, type = "Line",
                             colors = "Rainbow", colors.reverse = TRUE)), NA)
})

test_that("Line - colors Custom palette", {
    expect_error(suppressWarnings(print(Chart(named.matrix, type = "Line",
                             colors = "Custom palette",
                             colors.custom.palette = "red,blue,green,orange"))), NA)
})

# ---- Area: transpose and opacity ----

test_that("Area - transpose TRUE with opacity", {
    expect_error(print(Chart(named.matrix, type = "Area",
                             transpose = TRUE, opacity = 0.4)), NA)
})

# ---- Area: series.line.width zero ----

test_that("Area - series.line.width 0", {
    expect_error(print(Chart(named.matrix, type = "Area",
                             transpose = TRUE, opacity = 0.3,
                             series.line.width = 0)), NA)
})

# ---- Area: axis titles ----

test_that("Area - y.title and x.title", {
    expect_error(print(Chart(named.matrix, type = "Area",
                             y.title = "Y Axis", y.title.font.size = 14,
                             x.title = "X Axis", x.title.font.size = 14)), NA)
})

# ---- Area: margin params ----

test_that("Area - margin params", {
    expect_error(print(Chart(named.matrix, type = "Area",
                             margin.top = 80, margin.bottom = 80,
                             margin.left = 80, margin.right = 80,
                             margin.inner.pad = 20)), NA)
})

# ---- Area: background fill via Chart ----

test_that("Area - background.fill and charting.area.fill", {
    expect_error(print(Chart(named.matrix, type = "Area",
                             background.fill.color = "#f0f0f0",
                             background.fill.opacity = 0.5,
                             charting.area.fill.color = "#ffffff",
                             charting.area.fill.opacity = 0.8)), NA)
})

# ---- Bar: axis position and reversed ----

test_that("Bar - y.position right", {
    expect_error(print(Chart(named.matrix, type = "Bar",
                             y.position = "right")), NA)
})

test_that("Bar - y.data.reversed", {
    expect_error(print(Chart(named.matrix, type = "Bar",
                             y.data.reversed = TRUE)), NA)
})

# ---- Stacked Area: legend ----

test_that("Stacked Area - legend.show FALSE", {
    expect_error(print(Chart(named.matrix, type = "Stacked Area",
                             legend.show = FALSE)), NA)
})

test_that("Stacked Area - legend styling", {
    expect_error(print(Chart(named.matrix, type = "Stacked Area",
                             legend.fill = "#ffffff",
                             legend.border.color = "#000000",
                             legend.border.line.width = 1,
                             legend.position = "bottom",
                             legend.ascending = TRUE,
                             legend.font.family = "Arial",
                             legend.font.color = "#333333",
                             legend.font.size = 12)), NA)
})

test_that("Stacked Area - transpose", {
    expect_error(print(Chart(named.matrix, type = "Stacked Area",
                             transpose = TRUE)), NA)
})

# ---- Labeled Scatterplot: axis titles and marker size ----

test_that("Labeled Scatterplot - y.title and x.title with font params", {
    expect_warning(print(Chart(two.col.mat, type = "Labeled Scatterplot",
                               y.title = "Y Axis", y.title.font.size = 14,
                               x.title = "X Axis", x.title.font.size = 14)))
})

test_that("Labeled Scatterplot - series.marker.size", {
    expect_warning(print(Chart(two.col.mat, type = "Labeled Scatterplot",
                               series.marker.size = 20)))
})

test_that("Labeled Scatterplot - legend font params", {
    expect_warning(print(Chart(two.col.mat, type = "Labeled Scatterplot",
                               scatter.group.indices = paste(rep(1:2, 5), collapse = ", "),
                               scatter.group.labels = "Group A, Group B",
                               legend.show = TRUE,
                               legend.font.family = "Arial",
                               legend.font.color = "#333333",
                               legend.font.size = 12)))
})

# ---- LabeledScatter: title param ----

test_that("LabeledScatter - title param", {
    mds.dat <- structure(
        c(1.2, -0.5, 0.3, 0.8, -1.1, -0.9, 0.4, 1.0, -0.2, 0.6),
        .Dim = c(5L, 2L),
        .Dimnames = list(c("A", "B", "C", "D", "E"), c("X", "Y")))
    expect_error(print(LabeledScatter(mds.dat, title = "DEBUG_MODE_ON")), NA)
})

# ---- Area: title font params (from .Q file item 20) ----

test_that("Area - title with font params", {
    expect_error(print(Chart(named.matrix, type = "Area",
                             title = "My Chart",
                             title.font.family = "Arial",
                             title.font.color = "#333333",
                             title.font.size = 30)), NA)
})

# ---- Radar: cols.to.ignore ----

test_that("Radar - cols.to.ignore", {
    expect_error(print(Chart(named.matrix, type = "Radar",
                             cols.to.ignore = "")), NA)
})
