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

    t2 = structure(c(0.478468899521531, 26.3157894736842, 66.0287081339713,
        4.30622009569378, 0.478468899521531, 0.956937799043062, 0.956937799043062,
        0.478468899521531, 0, 0, 100, 0, 1.61290322580645, 3.2258064516129,
        9.67741935483871, 9.67741935483871, 17.741935483871, 50, 8.06451612903226,
        0, 0, 100, 0, 0, 0, 0, 0, 0, 51.7241379310345, 17.2413793103448,
        31.0344827586207, 0, 100, 0, 10, 25, 25, 0, 5, 35, 0, 0, 0, 100,
        0, 5.95238095238095, 17.8571428571429, 15.4761904761905, 7.14285714285714,
        2.38095238095238, 39.2857142857143, 10.7142857142857, 1.19047619047619,
        0, 100, 0, 3.06122448979592, 20.0680272108844, 23.469387755102,
        4.08163265306122, 6.46258503401361, 36.3945578231293, 5.4421768707483,
        1.02040816326531, 0, 100, NaN, NaN, NaN, NaN, NaN, NaN, NaN,
        NaN, NaN, NaN, NaN, 0.143266475644699, 10.3151862464183, 31.3753581661891,
        14.6131805157593, 3.58166189111748, 5.01432664756447, 27.9369627507163,
        5.15759312320917, 1.86246418338109, 0, 100), .Dim = c(11L, 8L
        ), statistic = "Column %", .Dimnames = list(c("15 and under",
        "16-19 yrs", "20-24 yrs", "25-29 yrs", "30-34 yrs", "35-44 yrs",
        "45-54 yrs", "55-64 yrs", "65 and over", "Don't know", "NET"),
            c("Student", "Home maker", "Retired", "Not working", "Part-time worker",
            "Fulltime worker", "Don't know/refused", "NET")), name = "Age by Work status", questions = c("Age",
        "Work status"))
    expect_warning(print(Histogram(t2)), "The following categories contain only missing values: Don't know/refused")
    expect_warning(print(Histogram(list(t2))), "The following categories contain only missing values: Don't know/refused")
    expect_error(Histogram(rep(NA, 100)))

    t3 <- 1:5
    attr(t3, "name") = "dog"
    Histogram(t3)

    data(phone, package = "flipExampleData")
    suppressWarnings(Histogram(list(phone$q4)))
    suppressWarnings(Histogram(list(Q4 = phone$q4, phone$q4)))
    suppressWarnings(Histogram(list(phone$id, phone$q4)))
    suppressWarnings(Histogram(list(Q1 = phone$q2, Q4 = phone$q4)))

        # Weights
    dt = list(suppressWarnings(flipTransformations::AsNumeric(phone$q4, binary = FALSE)))
    expect_warning(Histogram(dt, weights = NULL), NA)
    expect_error(suppressWarnings(Histogram(dt, weights = 1:3)), "The data and the weights do not have the same number of observations.")
    expect_warning(Histogram(dt, weights = runif(length(dt[[1]]))), "Weights are ignored in histograms.")



})
