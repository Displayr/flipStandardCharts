library(testthat)

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

test_that("HeatMap",
          {
            expect_error(print(HeatMap(table = three.cols)), NA)
            expect_error(print(HeatMap(table = three.cols, transpose = TRUE)), NA)
            for (sort in c("Sort by averages (ascending)", "Sort by averages (descending)", "Dendrogram")) {
                expect_error(print(HeatMap(table = three.cols, sort.rows = sort)), NA)
                expect_error(print(HeatMap(table = three.cols, sort.columns = sort)), NA)
            }
            expect_error(print(HeatMap(table = three.cols, standardization = "Standardize rows")), NA)
            expect_error(print(HeatMap(table = three.cols, standardization = "Standardize columns")), NA)
            expect_error(print(HeatMap(table = three.cols, show.cell.values = "No",
                                       show.row.labels = "No", show.column.labels = "No")), NA)
        })
