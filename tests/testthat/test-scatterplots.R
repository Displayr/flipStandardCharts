library(testthat)

two.cols <- structure(c(10.625, 3.125, 8.125, 9.375, 1.25, 7.5, 8.125, 3.75,
            5.625, 7.5, 1.875, 6.875, 9.375, 3.75, 8.125, 7.5, 5, 6.875,
            9.375, 3.125, 10, 14.375, 5.625, 15, 5, 1.875, 6.875, 11.9760479041916,
            1.79640718562874, 6.58682634730539, 11.9760479041916, 1.19760479041916,
            7.78443113772455, 7.18562874251497, 2.9940119760479, 3.59281437125748,
            11.377245508982, 2.9940119760479, 10.7784431137725, 6.58682634730539,
            1.79640718562874, 6.58682634730539, 7.78443113772455, 1.19760479041916,
            5.98802395209581, 8.98203592814371, 4.19161676646707, 7.78443113772455,
            10.7784431137725, 3.59281437125748, 8.98203592814371, 4.79041916167665,
            1.79640718562874, 4.79041916167665), .Dim = c(27L, 2L), statistic = "Column %", .Dimnames = list(
                paste0(rep(c("a", "b", "c"), 9), rep(1:9, each = 3)),
                c("Male", "Female")), name = "Span in Stub", questions = c("BANNER", "Q2. Gender"))

three.cols <- structure(c(13.9097744360902, 3.00751879699248, 7.89473684210526,
            13.1578947368421, 1.50375939849624, 8.27067669172932, 9.3984962406015,
            3.7593984962406, 4.88721804511278, 11.6541353383459, 2.25563909774436,
            9.77443609022556, 9.77443609022556, 2.63157894736842, 6.39097744360902,
            9.3984962406015, 3.7593984962406, 7.14285714285714, 11.2781954887218,
            3.38345864661654, 7.89473684210526, 15.4135338345865, 4.13533834586466,
            12.0300751879699, 6.01503759398496, 1.8796992481203, 4.88721804511278,
            9.63855421686747, 9.63855421686747, 8.43373493975904, 4.81927710843374,
            4.81927710843374, 3.6144578313253, 12.0481927710843, 13.2530120481928,
            7.2289156626506, 7.2289156626506, 9.63855421686747, 9.63855421686747,
            8.43373493975904, 10.8433734939759, 9.63855421686747, 12.0481927710843,
            12.0481927710843, 8.43373493975904, 10.8433734939759, 14.4578313253012,
            10.8433734939759, 13.2530120481928, 18.0722891566265, 15.6626506024096,
            6.02409638554217, 7.2289156626506, 7.2289156626506, 9.33333333333333,
            3.11111111111111, 10.6666666666667, 9.77777777777778, 1.33333333333333,
            11.1111111111111, 5.77777777777778, 2.66666666666667, 6.66666666666667,
            11.5555555555556, 3.55555555555556, 12.8888888888889, 7.55555555555556,
            3.55555555555556, 10.6666666666667, 8.44444444444444, 3.11111111111111,
            9.33333333333333, 9.33333333333333, 4, 12.8888888888889, 14.2222222222222,
            5.77777777777778, 17.3333333333333, 5.77777777777778, 2.66666666666667,
            8.44444444444444), .Dim = c(27L, 3L), statistic = "Column %", .Dimnames = list(
                paste0(rep(c("a", "b", "c"), 9), rep(1:9, each = 3)), c("Colas (e.g., Coca Cola, Pepsi Max)?", "Sparkling mineral water",
                               "Coffee")), name = "BANNER by Q4. Drink categorical", questions = c("BANNER", "Q4. Drink categorical"))

missing <- structure(c(10.625, NA, 8.125, 9.375, 1.25, 7.5, 8.125, 3.75,
                       5.625, 7.5, 1.875, 6.875, 9.375, 3.75, 8.125, 7.5, 5, 6.875,
                       9.375, 3.125, 10, 14.375, 5.625, 15, 5, 1.875, 6.875, 11.9760479041916,
                       1.79640718562874, 6.58682634730539, 11.9760479041916, 1.19760479041916,
                       7.78443113772455, 7.18562874251497, 2.9940119760479, 3.59281437125748,
                       11.377245508982, NaN, 10.7784431137725, 6.58682634730539,
                       1.79640718562874, 6.58682634730539, 7.78443113772455, 1.19760479041916,
                       5.98802395209581, 8.98203592814371, 4.19161676646707, 7.78443113772455,
                       10.7784431137725, 3.59281437125748, 8.98203592814371, 4.79041916167665,
                       1.79640718562874, NA), .Dim = c(27L, 2L), statistic = "Column %", .Dimnames = list(
                           paste0(rep(c("a", "b", "c"), 9), rep(1:9, each = 3)),
                           c("Male", "Female")), name = "Span in Stub", questions = c("BANNER", "Q2. Gender"))

test_that("Labeled Scatterplot ungrouped",
          expect_warning(print(Chart(y = two.cols,
                             type = "Labeled Scatterplot",
                             title = "Labeled Scatterplot"))))

test_that("Labeled Bubbleplot ungrouped",
          expect_error(print(Chart(y = three.cols,
                             type = "Labeled Bubbleplot",
                             title = "Labeled Bubbleplot")), NA))

test_that("Labeled Scatterplot grouped",
          expect_warning(print(Chart(y = two.cols,
                                   type = "Labeled Scatterplot",
                                   title = "Labeled Scatterplot",
                                   scatter.group.indices = "1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3",
                                   scatter.group.labels = "colas, water, coffee",
                                   legend.show = TRUE))))

test_that("Labeled Bubbleplot grouped",
          expect_error(print(Chart(y = three.cols,
                                   type = "Labeled Bubbleplot",
                                   title = "Labeled Bubbleplot",
                                   scatter.group.indices = "1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3",
                                   scatter.group.labels = "colas, water, coffee")), NA))

test_that("Labeled Scatterplot transposed",
          expect_warning(print(Chart(y = two.cols,
                                   type = "Labeled Scatterplot",
                                   transpose = TRUE,
                                   title = "Labeled Scatterplot"))))

test_that("Labeled Scatterplot missing",
          expect_warning(print(Chart(y = missing,
                                     type = "Labeled Scatterplot",
                                     title = "Labeled Scatterplot")),
                         "Data points with missing values have been omitted."))

test_that("Scatterplot",
    {
          expect_warning(print(Chart(y = two.cols,
                             type = "Scatterplot",
                             title = "Unlabeled Scatterplot")), "Chart contains overlapping points in the same position.")

          expect_warning(print(Chart(y = two.cols,
                             type = "Scatterplot",
                             title = "Unlabeled Scatterplot",
                             scatter.group.indices = "1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3",
                             scatter.group.labels = "colas, water, coffee")),
                         "Chart contains overlapping points in the same position.")

          expect_warning(print(Chart(y = two.cols,
                             type = "Scatterplot",
                             transpose = TRUE,
                             title = "Unlabeled Scatterplot")),
                        "Chart contains overlapping points in the same position.")

         expect_warning(print(Chart(y = two.cols,
                             type = "Labeled Scatterplot",
                             transpose = TRUE,
                             data.label.max.plot = 4,
                             title = "Unlabeled Scatterplot")))
         })

test_that("Group ordering",
          {
                xx <- data.frame(Age=1:9, Weight=c(12,14,15,12,13,14,16,18,12))
                rownames(xx) <- 1:9
                expect_error(print(Chart(xx, type="Labeled Scatterplot", colors="Reds, dark to light",
                                         scatter.group.indices = "2,2,1,4,4,3,2,4,2", scatter.group.labels = "D,B,C,A")), NA)
                expect_error(print(Chart(xx, type="Scatterplot", colors="Reds, dark to light",
                                         scatter.group.indices = "2,2,1,4,4,3,2,4,2", scatter.group.labels = "D,B,C,A")), NA)
          })

test_that("Alternative arguments", {
    expect_error(print(Chart(scatter.var.from.matrix=F, type="Scatterplot", colors="Heat colors (red, yellow, white)", scatter.x.var=1:10, scatter.y.var=10:1, scatter.colors.var = rep(c(1, 2),5))), NA)
    expect_error(print(Chart(scatter.var.from.matrix=F, type="Scatterplot", colors="Heat colors (red, yellow, white)", scatter.x.var=1:10, scatter.y.var=10:1, scatter.colors.var = c(1:5, rep(3,5)))), NA)


    z <- 1:10
    print(Chart(scatter.var.from.matrix=F, type="Scatterplot", colors="Heat colors (red, yellow, white)", scatter.x.var=1:10, scatter.y.var=10:1, scatter.colors.var = 1:10))
    print(Chart(scatter.var.from.matrix=F, type="Scatterplot", colors="Terrain colors (green, beige, grey)", scatter.x.var=z, scatter.y.var=z, scatter.colors.var = z, x.title="X", y.title="Y"))
    print(Chart(scatter.var.from.matrix=F, type="Scatterplot", colors="Terrain colors (green, beige, grey)", scatter.x.var=z, scatter.colors.var = z, x.title="X", y.title="Y"))
    print(Chart(scatter.var.from.matrix=F, type="Scatterplot", colors="Reds, dark to light", scatter.x.var=1:10, scatter.y.var=10:1, scatter.colors.var = c(1:5, rep(3,5))))
    print(Chart(scatter.var.from.matrix=F, type="Scatterplot", colors="Strong colors", scatter.x.var=z, scatter.y.var=z, scatter.colors.var = rep(c('a','b'), each=5), scatter.colors.as.group = T, x.title="X", y.title="Y"))
    suppressWarnings(print(Chart(scatter.var.from.matrix=F, type="Scatterplot", colors="Strong colors", scatter.x.var=z, scatter.y.var=z, scatter.colors.var = rep(c('a','b'), each=5), scatter.colors.as.group = F, x.title="X", y.title="Y")))
    print(Chart(scatter.var.from.matrix=F, type="Labeled Scatterplot", colors="Reds, dark to light", scatter.x.var=1:10, scatter.y.var=10:1, scatter.colors.var = 1:10, scatter.labels.var=LETTERS[1:10], scatter.sizes.var=1:10))
    print(Chart(scatter.var.from.matrix=F, type="Labeled Bubbleplot", colors="Reds, dark to light", scatter.x.var=1:10, scatter.y.var=10:1, scatter.colors.var = 1:10, scatter.labels.var=LETTERS[1:10], scatter.sizes.var=1:10))
    print(Chart(type="Scatterplot", scatter.var.from.matrix = F, scatter.x.var = 1:10, scatter.y.var=letters[1:10]))

})


t1 <- structure(c(42.7848101265823, 8.60759493670886, 14.9367088607595,
                  11.3924050632911, 1.77215189873418, 17.9746835443038, 0.759493670886076,
                  1.77215189873418, 100, 42.4691358024691, 13.5802469135802, 20.7407407407407,
                  6.66666666666667, 3.20987654320988, 11.8518518518519, 0.740740740740741,
                  0.740740740740741, 100, 42.625, 11.125, 17.875, 9, 2.5, 14.875,
                  0.75, 1.25, 100), .Dim = c(9L, 3L), statistic = "Column %", .Dimnames = list(
                      c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi ", "Diet Pepsi",
                        "Pepsi Max", "Dislike all cola", "Don't care", "NET"), c("Male",
                                                                                 "Female", "NET")), name = "Preferred cola by Gender", questions = c("Preferred cola",
                                                                                                                                                     "Gender"))

t2 <- structure(c(52.7848101265823, 18.6075949367089, 24.9367088607595,
                  21.3924050632911, 11.7721518987342, 27.9746835443038, 10.7594936708861,
                  11.7721518987342, 110, 42.4691358024691, 13.5802469135802, 20.7407407407407,
                  6.66666666666667, 3.20987654320988, 11.8518518518519, 0.740740740740741,
                  0.740740740740741, 100, 42.625, 11.125, 17.875, 9, 2.5, 14.875,
                  0.75, 1.25, 100), .Dim = c(9L, 3L), statistic = "Column %", .Dimnames = list(
                      c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi ", "Diet Pepsi",
                        "Pepsi Max", "Dislike all cola", "Don't care", "NET"), c("Male",
                                                                                 "Female", "NET")), name = "Preferred cola by Gender2", questions = c("Preferred cola",
                                                                                                                                                     "Gender"))

t3 <- structure(c(52.7848101265823, 18.6075949367089, 24.9367088607595,
                  21.3924050632911, 11.7721518987342, 27.9746835443038, 10.7594936708861,
                  11.7721518987342, 110, 62.4691358024691, 33.5802469135802, 40.7407407407407,
                  26.6666666666667, 23.2098765432099, 31.8518518518519, 20.7407407407407,
                  20.7407407407407, 120, 42.625, 11.125, 17.875, 9, 2.5, 14.875,
                  0.75, 1.25, 100), .Dim = c(9L, 3L), statistic = "Column %", .Dimnames = list(
                      c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi ", "Diet Pepsi",
                        "Pepsi Max", "Dislike all cola", "Don't care", "NET"), c("Male",
                                                                                 "Female", "NET")), name = "Preferred cola by Gender3", questions = c("Preferred cola",
                                                                                                                                                     "Gender"))

test_that("Multiple tables", {
    for (trend.lines in c(TRUE, FALSE)) {
        expect_error(print(Chart(t1, type = "Labeled Scatterplot", trend.lines = trend.lines)), NA)
        expect_error(print(Chart(list(t1), type = "Labeled Scatterplot", trend.lines = trend.lines)), NA)
        expect_error(print(Chart(list(t1, t2), type = "Labeled Scatterplot", trend.lines = trend.lines)), NA)
        expect_error(print(Chart(list(t1, t2, t3), type = "Labeled Scatterplot", trend.lines = trend.lines)), NA)
        expect_error(print(Chart(list(t1, t2, t3), type = "Bar", trend.lines = trend.lines)), "Multiple tables can only be used with Labeled Scatterplot.")
    }
    rownames(t1)[2] <- "Coca-Cola"
    expect_error(print(Chart(list(t1, t2, t3), type = "Labeled Scatterplot", trend.lines = TRUE)), "Row names of tables must be unique or NULL for trend lines.")
    expect_error(print(Chart(list(t1, t2, t3), type = "Labeled Scatterplot", trend.lines = FALSE)), "Tables should have identical row names.")
    rownames(t2)[2] <- "Coca-Cola"
    rownames(t3)[2] <- "Coca-Cola"
    expect_error(print(Chart(list(t1, t2, t3), type = "Labeled Scatterplot", trend.lines = TRUE)), "Row names of tables must be unique or NULL for trend lines.")
    expect_error(print(Chart(list(t1, t2, t3), type = "Labeled Scatterplot", trend.lines = FALSE)), NA)
})


logo.8string <- "https://dl.dropboxusercontent.com/u/539177224/bread_grey.svg, https://dl.dropboxusercontent.com/u/539177224/car_grey.svg, https://dl.dropboxusercontent.com/u/539177224/elephant_grey.svg, https://dl.dropboxusercontent.com/u/539177224/baby_grey.svg, https://dl.dropboxusercontent.com/u/539177224/apple_grey.svg, https://dl.dropboxusercontent.com/u/539177224/chicken_grey.svg, https://dl.dropboxusercontent.com/u/539177224/cow_grey.svg, https://dl.dropboxusercontent.com/u/539177224/thumbsup_grey.svg"
test_that("Logos", {
    expect_error(print(Chart(t1, type = "Labeled Scatterplot", trend.lines = TRUE, logos = logo.8string)), NA)
    expect_error(print(Chart(t1, type = "Labeled Scatterplot", trend.lines = FALSE, logos = logo.8string)), NA)
    expect_error(print(Chart(list(t1, t2, t3), type = "Labeled Scatterplot", trend.lines = TRUE, logos = logo.8string)), NA)
    expect_error(print(Chart(list(t1, t2, t3), type = "Labeled Scatterplot", trend.lines = FALSE, logos = logo.8string)), NA)
    expect_error(print(Chart(t1[1:5, ], type = "Labeled Scatterplot", trend.lines = FALSE, logos = logo.8string)), "Number of URLs supplied in logos is 8 but must be equal to the number of rows in the table.")
})


