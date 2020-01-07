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

test_that("Annotations",
{
    expect_error(Column(data.with.stats, data.label.show = TRUE, annotation.list=list(list(data="p", type = "Arrow - up", threstype = "above threshold", threshold = 0.05, color = "#FF0000", size = 15))), NA)

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
        list(type = "Text = after data labels", data = "Column Comparisons",
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
})

# Set up dataframe containing different types of data types
set.seed(1234)
dat <- data.frame('Score' = rnorm(20),
                  'Cost' = abs(rnorm(20)), # check plotly is handling '$' properly
                  'Age' = rpois(20, 40),
                  'Class' = factor(sample(LETTERS[4:1], 20, replace = TRUE), levels = LETTERS[4:1], ordered = TRUE), # reverse order to check DS-1645
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

    expect_error(Scatter(dat, annotation.list = list(list(type = "Arrow - up",
        data = "Sporadic", threstype = "above threshold", threshold = "1.0",
        color = "red", size = 15, width = 1, font.family = "Arial",
        font.weight = "normal", font.style = "normal", format = ".2f", prefix = "$"),
        list(type = "Marker border", data = "Class", threstype = "above threshold",
        threshold = "C", width = 3, color = "blue"),
        list(type = "Marker border", data = "Date", threstype = "above threshold",
        threshold = "2017-01-9", width = 1, color = "red"))), NA)
})
