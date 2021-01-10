# Testing annotations attached to the data labels

context("Annotations")
library(flipStandardCharts)

data.with.stats <- structure(c(2.75482093663912, 6.06060606060606, 12.6721763085399,
18.4573002754821, 24.7933884297521, 15.9779614325069, 6.06060606060606,
8.26446280991736, 4.95867768595041, 100, 3.77906976744186, 15.9883720930233,
7.84883720930233, 18.0232558139535, 19.7674418604651, 13.0813953488372,
10.7558139534884, 4.06976744186047, 6.68604651162791, 100, 3.25318246110325,
10.8910891089109, 10.3253182461103, 18.2461103253182, 22.3479490806223,
14.5685997171146, 8.34512022630834, 6.22347949080622, 5.7991513437058,
100, 0.442913092343004, 0.0000228306627828578, 0.0351514682974756,
0.881274082835059, 0.108843509396061, 0.275202305069102, 0.0240561692086175,
0.0210216801333983, 0.326003170694519, NA, 0.442913092343004,
0.0000228306627828578, 0.0351514682974756, 0.881274082835059,
0.108843509396061, 0.275202305069102, 0.0240561692086175, 0.0210216801333983,
0.326003170694519, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
), .Dim = c(10L, 3L, 2L), .Dimnames = list(c("Less than $15,000",
"$15,001 to $30,000", "$30,001 to $45,000", "$45,001 to $60,000",
"$60,001 to $90,000", "$90,001 to $120,000", "$120,001 to $150,000",
"$150,001 to $200,000", "$200,001 or more", "NET"), c("Male",
"Female", "NET"), c("Column %", "p")), name = "Income by Gender", questions = c("Income",
"Gender"))

vec.with.stats <- structure(c(3.25318246110325, 10.8910891089109, 10.3253182461103,
18.2461103253182, 22.3479490806223, 14.5685997171146, 8.34512022630834,
6.22347949080622, 5.7991513437058, 100, 2.96352779053704e-11,
0.852323672450741, 0.506154634139539, 1.57276658363514e-09, 0,
0.00344142405418046, 0.0192720273455812, 0.0000354510822852101,
0.00000697993687950735, 0), .Dim = c(10L, 2L), .Dimnames = list(
    c("Less than $15,000", "$15,001 to $30,000", "$30,001 to $45,000",
    "$45,001 to $60,000", "$60,001 to $90,000", "$90,001 to $120,000",
    "$120,001 to $150,000", "$150,001 to $200,000", "$200,001 or more",
    "NET"), c("%", "p")), name = "Income", questions = c("Income",
"SUMMARY"))

# Annotations
a1 <- list(list(type = "Circle - thin outline", data = "p", threstype = "above threshold",
    threshold = "-Inf", color = "red", size = 20, width = 1,
    offset = 0, font.family = "Arial", font.weight = "normal",
    font.style = "normal"), list(type = "Circle - thick outline", data = "p",
    threstype = "above threshold", threshold = "0.05", color = "blue",
    size = 20, width = 1, offset = 0, font.family = "Arial",
    font.weight = "normal", font.style = "normal"))

a2 <- list(list(type = "Circle - filled", data = "p", threstype = "below threshold",
    threshold = "0.10", color = "#C0C0C0", size = 25, width = NULL,
    offset = NULL, font.family = NULL, font.weight = NULL, font.style = NULL),
    list(type = "Circle - filled", data = "p", threstype = "below threshold",
        threshold = "0.05", color = "#80FFFF", size = 20, width = NULL,
        offset = NULL, font.family = NULL, font.weight = NULL,
        font.style = NULL), list(type = "Arrow - up", data = "p",
        threstype = "below threshold", threshold = "0.05", color = "#CD343C",
        size = 15, width = NULL, offset = NULL, font.family = NULL,
        font.weight = NULL, font.style = NULL))
a3 <- list(list(type = "Circle - filled", data = "p", threstype = "above threshold",
    threshold = "-Inf", color = "red", size = 20, width = 1,
    offset = 0, font.family = "Arial", font.weight = "normal",
    font.style = "normal"), list(type = "Circle - thin outline", data = "p",
    threstype = "above threshold", threshold = "0.05", color = "blue",
    size = 20, width = 1, offset = 0, font.family = "Arial",
    font.weight = "normal", font.style = "normal"))

a4 <- list(list(type = "Text - after data label", data = "p", threstype = "above threshold", threshold = "-Inf", color = "red", size = 8, width = 1, font.family = "Arial", font.weight = "normal", font.style = "normal", format = ".3f", prefix = " +/-"))

a5 <- list(list(type = "Arrow - up", data = "p", threstype = "below threshold",
    threshold = "0.05", color = "red", size = 20, width = 1,
    offset = 0, font.family = "Arial", font.weight = "normal",
    font.style = "normal"), list(type = "Arrow - down", data = "p",
    threstype = "above threshold", threshold = "0.05", color = "blue",
    size = 20, width = 1, offset = 0, font.family = "Arial",
    font.weight = "normal", font.style = "normal"))

test_that("Annotations",
{
    expect_error(Column(data.with.stats, data.label.show = TRUE, annotation.list=list(list(data="p", type = "Arrow - up", threstype = "above threshold", threshold = 0.05, color = "#FF0000", size = 15))), NA)
    expect_error(Column(data.with.stats, data.label.show = TRUE, annotation.list=list(list(data="p values", type = "Arrow - up", threstype = "above threshold", threshold = 0.05, color = "#FF0000", size = 15))),
                 "Annotation data does not contain a statistic named 'p values'")

    expect_error(Column(vec.with.stats, data.label.show = TRUE, annotation.list=list(list(data="p", type = "Arrow - up", threstype = "above threshold", threshold = 0.05, color = "#0000FF"))), NA)

    expect_error(Column(vec.with.stats, data.label.show = TRUE, annotation.list = list(list(data = "p",
    type = "Circle - filled", size = 30, color = "red", threstype = "above threshold", threshold = 0.05), list(data = "p", type = "Circle - thin outline", size = 30, color = "blue"))), NA)

    expect_error(Column(vec.with.stats, data.label.show = TRUE, annotation.list = list(list(data = "p",
    type = "Circle - filled", size = 35, color = "blue"), list(data = "p", type = "Circle - filled", size = 30, color = "red"))), NA)

    expect_warning(Column(vec.with.stats, data.label.show = TRUE, annotation.list = list(list(data = "p",
    type = "Filled circle", size = 35, color = "blue"), list(data = "p", type = "Filled circle", size = 30, color = "red"))), "Unknown annotation type")

    expect_error(Column(vec.with.stats, data.label.show = TRUE, annotation.list = list(list(data = "p",
    type = "Circle - filled", size = 40, color = "red"), list(data = "p", type = "Circle - filled", size = 30, color = "orange"), list(data = "p", type = "Circle - filled", size = 20, color = "yellow"))), NA)

    expect_error(Bar(data.with.stats, data.label.show = TRUE, type = "Stacked", annotation.list = a1), NA)

    expect_error(SmallMultiples(data.with.stats, "Bar", data.label.show = TRUE, annotation.list = a2), NA)

    expect_error(SmallMultiples(data.with.stats, "BarMultiColor", data.label.show = TRUE, annotation.list = a3), NA)

    expect_error(SmallMultiples(data.with.stats, "ColumnMultiColor", data.label.show = TRUE, annotation.list = a4), NA)

    expect_error(Line(data.with.stats[-10,,], annotation.list = a5, data.label.show = TRUE), NA)
    expect_error(Line(data.with.stats[-10,,], annotation.list = a5, data.label.show = TRUE,
        data.label.show.at.ends = TRUE), NA)
    expect_error(SmallMultiples(data.with.stats[-10,,], "Line", annotation.list = a5,
        data.label.show = TRUE), NA)
    expect_error(SmallMultiples(data.with.stats[-10,,], "Line", annotation.list = a5,
        data.label.show = TRUE, data.label.show.at.ends = TRUE,
        marker.show.at.ends = TRUE, marker.size = 10), NA)
})

dat2 <- structure(c(38.8888888888889, 0, 0, 11.1111111111111, 18.1818181818182,
9.09090909090909, 0, 7.27272727272727, 17.5438596491228, 5.26315789473684,
8.7719298245614, 10.5263157894737, 13.5416666666667, 7.29166666666667,
13.5416666666667, 10.4166666666667, 18, 18, 18, 18, 55, 55, 55,
55, 57, 57, 57, 57, 96, 96, 96, 96), .Dim = c(4L, 4L, 2L), .Dimnames = list(
    c("18 to 24", "25 to 29", "30 to 34", "35 to 39"), c("Less than $15,000",
    "$15,001 to $30,000", "$30,001 to $45,000", "$45,001 to $60,000"
    ), c("Column %", "Column Sample Size")), name = "table.D1.Age.by.D2.Income", questions = c("D1 - Age",
"D2 - Income"), assigned.rownames = TRUE)

dat.with.text <- structure(c("0", "8.55263157894737", "7.23684210526316", "7.89473684210526",
"9.86842105263158", "10.5263157894737", "8.55263157894737", "10.5263157894737",
"21.0526315789474", "15.7894736842105", "100", "0", "4.375",
"7.5", "11.875", "11.875", "11.25", "9.375", "10.625", "17.5",
"15.625", "100", "-", "B", NA, NA, NA, NA, NA, NA, "b", NA, "-",
"-", NA, NA, "A", "a", NA, NA, NA, NA, NA, "-"), .Dim = c(11L,
2L, 2L), .Dimnames = list(c("Less than 18 years", "18 to 24 years",
"25 to 29 years", "30 to 34 years", "35 to 39 years", "40 to 44 years",
"45 to 49 years", "50 to 54 years", "55 to 64 years", "65 years or more",
"NET"), c("Male", "Female"), c("Column %", "Column Comparisons"
)), name = "table.Q2.Age.by.Q1.Gender", questions = c("Q2. Age",
"Q1. Gender"))

test_that("Input matrix converted to character",
{
    expect_error(Column(dat.with.text, data.label.show = TRUE,
        annotation.list = list(
        list(type = "Text - after data label", data = "Column Comparisons",
             font.style = "normal", font.weight = "normal",
             format = ".3f", prefix = "", suffix = "",
             threshold = "-", threstype = "above threshold",
             color = "red", font.family = "Courier New"))), NA)

})

test_that("No errors for all chart types",
{
    charting.funcs <- c("Column", "Bar", "Area", "Line", "Pie", "Radar", #"Donut",
                    "Scatter", "LabeledScatter")
                    #"Box", "Bean", "Distribution", "Density", "Violin")
    for (func in charting.funcs)
    {
        cmd <- paste0("pp <- ", func, "(dat.with.text)")
        expect_error(suppressWarnings(eval(parse(text=cmd))), NA)
    }
    expect_error(SmallMultiples(dat.with.text, "Bar",
        data.label.show = T, average.show = T, fit.type = "supsmu"), NA)
})

# Set up dataframe containing different types of data types
set.seed(1234)
dat <- data.frame('Score' = rnorm(20),
                  'Cost' = abs(rnorm(20)), # check plotly is handling '$' properly
                  'Age' = rpois(20, 40),
                  'Class' = factor(sample(LETTERS[4:1], 20, replace = TRUE), levels = LETTERS[4:1], ordered = FALSE),
                  'Sporadic' = c(1:5, NA, 6:10, NA, NA, 11:12, NA, NA, 13:15), # missing values
                  'Date' = as.Date(sprintf("2017-01-%02d", 20:1)),
                  check.names = FALSE, stringsAsFactors = FALSE)
rownames(dat) <- letters[1:20]

test_that("Scatter plot annotations",
{
    expect_error(Scatter(dat, annotation.list = list(list(type = "Text - after data label",
        data = "Cost", threstype = "above threshold", threshold = "1.0",
        color = "red", size = 8, width = 1, font.family = "Arial",
        font.weight = "normal", font.style = "normal", format = ".2f", prefix = "$"),
        list(type = "Border", data = "Cost", threstype = "above threshold", threshold = "2.0",
        color = "grey", width = 2))), NA)

    expect_error(Scatter(dat, annotation.list = list(list(type = "Text - after data label",
        data = "Class", threstype = "above threshold", threshold = "",
        color = "red", size = 8, width = 1, font.family = "Arial",
        font.weight = "normal", font.style = "normal", format = ".2f", prefix = "Class "),
        list(type = "Border", data = "Cost", threstype = "above threshold", threshold = "2.0",
        color = "grey", width = 2))), NA)

    expect_warning(Scatter(dat, annotation.list = list(list(type = "Arrow - up",
        data = "Sporadic", threstype = "above threshold", threshold = "1.0",
        color = "red", size = 15, width = 1, font.family = "Arial",
        font.weight = "normal", font.style = "normal", format = ".2f", prefix = "$"),
        list(type = "Marker border", data = "Class", threstype = "above threshold",
        threshold = "C", width = 3, color = "blue"),
        list(type = "Marker border", data = "Date", threstype = "above threshold",
        threshold = "2017-01-9", width = 1, color = "red"))),
        "Inequalities are not applicable to 'Class'")

    expect_error(Scatter(dat, annotation.list = list(list(type = "Arrow - up",
        data = "Sporadic typo", threstype = "above threshold", threshold = "1.0",
        color = "red", size = 15, width = 1, font.family = "Arial",
        font.weight = "normal", font.style = "normal", format = ".2f", prefix = "$"),
        list(type = "Marker border", data = "Class", threstype = "above threshold",
        threshold = "C", width = 3, color = "blue"),
        list(type = "Marker border", data = "Date", threstype = "above threshold",
        threshold = "2017-01-9", width = 1, color = "red"))),
        "Annotation data does not contain")
})

tb <- structure(c(NA, NA, NA, NA, 9.07042253521127, 8.55072463768116,
            8.89795918367347, 8.92413793103448, 8.17142857142857, 0, 0, 0,
            0, 71, 69, 49, 145, 35, NA, NA, NA, NA, 1.81278883943602, -2.27748923545864,
            0.113642487963684, 1.17602961985239, -1.50006621469747), .Dim = c(1L,
                                                                              9L, 3L), .Dimnames = list("Satisfaction with firm - Numeric",
                                                                                                        c("A", "B", "C", "D", "Absolute Performance", "Ongoing Advice",
                                                                                                          "Performance Only", "Relative Performance", "Wealth Partner"
                                                                                                        ), c("Average", "Column Sample Size", "z-Statistic")), name = "table.Q2.1.Satisfaction.with.firm.Numeric.by.Fee.Structure", questions = c("Q2.1 Satisfaction with firm - Numeric",
                                                                                                                                                                                                                                                  "Fee Structure"), assigned.rownames = TRUE)
a.list <- list(list(type = "Arrow - up", data = "z-Statistic", threstype = "above threshold",
          threshold = "1.96", color = "#0066A5", size = 15, width = NULL,
          offset = NULL, shiftleft = NULL, shiftright = NULL, format = NULL,
          prefix = NULL, suffix = NULL, font.family = NULL, font.weight = NULL,
          font.style = NULL), list(type = "Arrow - down", data = "z-Statistic",
                                   threstype = "below threshold", threshold = "-0.001", color = "#0066A5",
                                   size = 15, width = NULL, offset = NULL, shiftleft = NULL,
                                   shiftright = NULL, format = NULL, prefix = NULL, suffix = NULL,
                                   font.family = NULL, font.weight = NULL, font.style = NULL))

test_that("One-row matrix is transposed",
{
    expect_warning(Column(aperm(tb, c(2,1,3)), annotation.list = a.list, data.label.show = T),
                   "Missing values have been set to zero")
    expect_warning(Column(tb, annotation.list = a.list, data.label.show = T, type = "Stacked"),
                   "Missing values have been set to zero")
    expect_warning(Bar(aperm(tb, c(2,1,3)), annotation.list = a.list, data.label.show = T),
                   "Missing values have been set to zero")
    expect_warning(Bar(tb, annotation.list = a.list, data.label.show = T),
                   "Missing values have been set to zero")
    expect_warning(ColumnMultiColor(tb[,5:7,,drop = FALSE], annotation.list = a.list, data.label.show = T),
                   paste0("Column chart with multi color series can only show a single series. ",
                          "To show multiple series use Small Multiples"))
    expect_warning(BarMultiColor(tb[,5:7,,drop = FALSE], annotation.list = a.list, data.label.show = T),
                   paste0("Column chart with multi color series can only show a single series. ",
                          "To show multiple series use Small Multiples"))
    expect_warning(SmallMultiples(tb, "ColumnMultiColor", data.label.show = TRUE, annotation.list = a.list),
                   "Ignoring 1 observations")
})

dat2 <- structure(c(0.388888888888889, 0, 0, 0.111111111111111, 0, 0,
                    0, 0.444444444444444, 0.0555555555555556, 0.181818181818182,
                    0.0909090909090909, 0, 0.0727272727272727, 0.181818181818182,
                    0.0545454545454545, 0.0545454545454545, 0.2, 0.163636363636364,
                    0.175438596491228, 0.0526315789473684, 0.087719298245614, 0.105263157894737,
                    0.0350877192982456, 0.0701754385964912, 0.12280701754386, 0.263157894736842,
                    0.087719298245614, 0.135416666666667, 0.0729166666666667, 0.135416666666667,
                    0.104166666666667, 0.0833333333333333, 0.135416666666667, 0.104166666666667,
                    0.0625, 0.166666666666667, 0.0833333333333333, 0.116666666666667,
                    0.0833333333333333, 0.0833333333333333, 0.191666666666667, 0.0833333333333333,
                    0.175, 0.166666666666667, 0.0166666666666667, 0.0657894736842105,
                    0.276315789473684, 0.118421052631579, 0.197368421052632, 0.0657894736842105,
                    0.0263157894736842, 0.157894736842105, 0.0921052631578947, 0,
                    0.0666666666666667, 0.133333333333333, 0.155555555555556, 0.0666666666666667,
                    0.111111111111111, 0.133333333333333, 0.0888888888888889, 0.244444444444444,
                    0, 0.0294117647058823, 0.117647058823529, 0.117647058823529,
                    0.323529411764706, 0.0588235294117647, 0.0882352941176471, 0.0588235294117647,
                    0.205882352941176, 0, 0.2, 0.2, 0.133333333333333, 0.0666666666666667,
                    0.133333333333333, 0, 0.133333333333333, 0.133333333333333, 0),
                    name = "table.D1.Age.by.D2.Income", questions = c("D1 - Age",
                    "D2 - Income [Cola Tracking - January to September 2017.sav]"),
                    assigned.rownames = TRUE, statistic = "Column %", scatter.variable.indices = c(x = 1,
                    y = 2, sizes = 3, colors = 4, groups = 10), .Dim = c(9L, 9L), .Dimnames = list(
                    c("18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44",
                    "45 to 49", "50 to 54", "55 to 64", "65+"), c("Less than $15,000",
                    "$15,001 to $30,000", "$30,001 to $45,000", "$45,001 to $60,000",
                    "$60,001 to $90,000", "$90,001 to $120,000", "$120,001 to $150,000",
                    "$150,001 to $200,000", "$200,001 or more")))

a2 <- list(list(type = "Text - after data label", data = "Less than $15,000",
                threstype = "above threshold", threshold = " 0.05", color = "#CD343C",
                size = 8, width = NULL, offset = NULL, shiftleft = NULL,
                shiftright = NULL, format = "", prefix = "", suffix = "",
                font.family = "Impact", font.weight = "normal", font.style = "normal"),
           list(type = "Marker border", data = "$15,001 to $30,000",
                threstype = "above threshold", threshold = "0.05", color = "red",
                size = 15, width = 1, offset = NULL, shiftleft = NULL,
                shiftright = NULL, format = NULL, prefix = NULL, suffix = NULL,
                font.family = NULL, font.weight = NULL, font.style = NULL),
           list(type = "Text - before data label", data = "Less than $15,000",
                threstype = "above threshold", threshold = " -Inf", color = "#3E7DCC",
                size = 15, width = NULL, offset = NULL, shiftleft = NULL,
                shiftright = NULL, format = "", prefix = "", suffix = "",
                font.family = "Arial", font.weight = "normal", font.style = "normal"))

test_that("More scatter annotations",
{
    expect_warning(Scatter(dat2, annotation.list = a2),
                   "Chart contains overlapping points in the same position.")
})

