context("Histogram")
#
# # The tests on this page function only as a smoke test.
# # The testing of all the individual parameters is
# # performed in test-Distribution and integration tests run outside of the package.
#
test_that("Histogram", {
    set.seed(1223)
    z = list(Normal = rnorm(1000), "Poisson with unit lamda" = rpois(1000, 1),
             Exponential = rexp(1000))
    Histogram(z[[1]])
    Histogram(z, title = "Comparing distributions",
            values.title = "Values",
            global.font.family = "Courier",
            global.font.color = "Red")
    Histogram(z, title = "Comparing distributions",
            values.title = "Values",
            global.font.family = "Courier",
            global.font.color = "Red",
            vertical = TRUE)
    Histogram(z, title = "Comparing distributions",
              values.title = "Values",
              global.font.family = "Courier",
              global.font.color = "Red")
    Histogram(z, title = "Comparing distributions",
              values.title = "Values",
              show.values = TRUE,
              histogram.cumulative = TRUE,
              histogram.counts = TRUE,
              maximum.bins = 5)
    names(z)[1] = "A really, really, long label designed to not wrap to see what is happening with wrapping."
    Histogram(z, title = "Comparing distributions",
              values.title = "Values",
              vertical = FALSE,
              histogram.cumulative = TRUE,
              histogram.counts = TRUE,
              maximum.bins = 2, categories.tick.label.wrap = TRUE)


    t1 = structure(c(0, 8.28402366863905, 34.3195266272189, 18.0473372781065,
        2.9585798816568, 4.14201183431953, 25.4437869822485, 4.73372781065089,
        2.07100591715976, 0, 100, 0.265957446808511, 11.968085106383,
        30.0531914893617, 11.7021276595745, 3.98936170212766, 5.58510638297872,
        29.5212765957447, 5.31914893617021, 1.59574468085106, 0, 100,
        0.140056022408964, 10.2240896358543, 32.0728291316527, 14.7058823529412,
        3.50140056022409, 4.90196078431373, 27.5910364145658, 5.04201680672269,
        1.82072829131653, 0, 100), .Dim = c(11L, 3L), statistic = "Column %", .Dimnames = list(
            c("15 and under", "16-19 yrs", "20-24 yrs", "25-29 yrs",
            "30-34 yrs", "35-44 yrs", "45-54 yrs", "55-64 yrs", "65 and over",
            "Don't know", "NET"), c("male", "female", "NET")), name = "Age by Gender", questions = c("Age",
        "Gender"))
    expect_error(print(Histogram(t1)), NA)
    expect_error(print(Histogram(as.matrix(t1))), NA)
    data(phone, package = "flipExampleData")
    suppressWarnings(Histogram(list(phone$q4)))
    suppressWarnings(Histogram(list(Q4 = phone$q4, phone$q4)))
    suppressWarnings(Histogram(list(phone$id, phone$q4)))
    suppressWarnings(Histogram(list(Q1 = phone$q2, Q4 = phone$q4)))


        suppressWarnings(flipChart::CChart("Histogram", list(phone$q4), title = "Comparing distributions",
              values.title = "Values",
              histogram.cumulative = TRUE,
              histogram.counts = TRUE,
              maximum.bins = 2, categories.tick.label.wrap = TRUE))



})
