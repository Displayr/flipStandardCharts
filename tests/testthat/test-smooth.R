context("Smooth series")

search.share <- structure(c(1.47058823529412, 14.2857142857143, 22.2222222222222,
                                6.25, 60, 41.6666666666667, 26.3157894736842, 53.8461538461538,
                                42.8571428571429, 45, 57.1428571428571, 55.5555555555556, 78.5714285714286,
                                77.7777777777778, 66.6666666666667, 59.2592592592593, 60, 90,
                                100, 78.9473684210526, 80, 83.3333333333333, 100, NaN, 85.7142857142857,
                                NaN, 50, 100, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN,
                                NaN, NaN), .Names = c("Week 1 n:136", "2 n:21", "3 n:18", "4 n:16",
                                "5 n:5", "6 n:12", "7 n:19", "8 n:13", "9 n:21", "10 n:20", "11 n:7",
                                "12 n:9", "13 n:14", "14 n:18", "15 n:9", "16 n:27", "17 n:10",
                                "18 n:10", "19 n:6", "20 n:19", "21 n:10", "22 n:6", "23 n:4",
                                "24 n:0", "25 n:7", "26 n:0", "27 n:2", "28 n:1", "29 n:0", "30 n:0",
                                "31 n:0", "32 n:0", "33 n:0", "34 n:0", "35 n:0", "36 n:0", "37 n:0",
                                "38 n:0", "39 n:0", "40 n:0"))

test_that("line-of-best-fit",
{
    expect_warning(Column(search.share, fit.type = "Friedman"), "Missing values have been set to zero")
    expect_warning(Column(search.share, fit.type = "LOESS"), "Missing values have been set to zero")
    expect_warning(Column(search.share, fit.type = "Linear"), "Missing values have been set to zero")
})
