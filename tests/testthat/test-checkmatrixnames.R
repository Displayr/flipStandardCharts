context("checkMatrixNames")

# Tests an internal function which is used to standardize input data

tab2d.with.stats <- structure(c(2.75482093663912, 6.06060606060606, 12.6721763085399,
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

tab1d.with.stats <- structure(c(3.25318246110325, 10.8910891089109, 10.3253182461103,
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

tab.2d.nonQ <- structure(c(21.7684616846345, 16.8273954590784, 23.6218418617686,
22.3939814605362, 15.3883195339823, 20.2682378794749, 22.3469131808503,
19.5399118569831, 24.9071455030989, 12.9377915795928, 24.2191612233134,
14.4972683594848, 26.9645648467198, 22.8280612061214, 11.4909443643606,
21.4238052135251, 16.9520193150924, 23.230939342932, 22.1717903016062,
16.2214458268442, 21.7684616846345, 16.8273954590784, 23.6218418617686,
22.3939814605362, 15.3883195339823), .Dim = c(5L, 5L), .Dimnames = list(
    c("Available", "Clean", "Dependable", "Informed", "Safe"),
    c("< 15%", "15 - 50%", "> 50% and Regional median",
    "Regional median", "NET")))

tab.as.char <- structure(c("11.1111111111111", "33.3333333333333", "37.037037037037",
    "3.7037037037037", "0", "14.8148148148148", "0", "100", "40.8284023668639",
    "8.87573964497041", "19.5266272189349", "7.69230769230769", "3.55029585798817",
    "17.7514792899408", "0.591715976331361", "100", "54.1984732824427",
    "9.9236641221374", "13.7404580152672", "10.6870229007634", "2.29007633587786",
    "7.63358778625954", "0.763358778625954", "100", "43.7308868501529",
    "11.3149847094801", "18.6544342507645", "8.56269113149847", "2.75229357798165",
    "13.4556574923547", "0.611620795107034", "100", "-3.56735988542559",
    "3.77075023250509", "2.56002467253868", "-0.942050206800309",
    "-0.912646989970321", "0.216069296011427", "-0.425567494420911",
    NA, "-1.09428239999305", "-1.4400978823522", "0.418738876007761",
    "-0.581744715753815", "0.912232950688339", "2.35430724535457",
    "-0.0477461092441626", NA, "3.11959967561212", "-0.649316728101167",
    "-1.86490432940116", "1.12237239171791", "-0.417677007228672",
    "-2.52224393244656", "0.287717892938221", NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, "B c", "b c", NA, NA, NA, NA, "-", "a", NA,
    NA, NA, NA, "c", NA, "-", "A b", NA, NA, NA, NA, NA, NA, "-",
    "-", "-", "-", "-", "-", "-", "-", "-"), .Dim = c(8L, 4L, 3L), .Dimnames = list(
        c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi ", "Diet Pepsi",
        "Pepsi Max", "Dislike all cola", "NET"), c("I am on a diet, so I tend to watch what I eat and drink",
        "I tend watch what I eat and drink, but don’t consider myself",
        "I typically eat and drink whatever I feel like", "NET"),
        c("Column %", "z-Statistic", "Column Comparisons")), basedescriptiontext = "sample size = 327", basedescription = list(
        Minimum = 327L, Maximum = 327L, Range = FALSE, Total = 327L,
        Missing = 0L, EffectiveSampleSize = 327L, EffectiveSampleSizeProportion = 100,
        FilteredProportion = 0), questiontypes = c("PickOne", "PickOne"
    ), span = list(rows = structure(list(c("Coca-Cola", "Diet Coke",
    "Coke Zero", "Pepsi ", "Diet Pepsi", "Pepsi Max", "Dislike all cola",
    "NET")), class = "data.frame", .Names = "", row.names = c(NA,
    8L)), columns = structure(list(c("I am on a diet, so I tend to watch what I eat and drink",
    "I tend watch what I eat and drink, but don’t consider myself",
    "I typically eat and drink whatever I feel like", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
    4L))), name = "table.Preferred.cola.by.Weight.consciousness", questions = c("Preferred cola",
    "Weight-consciousness"))

test_that("checkMatrixNames",
{
    res <- checkMatrixNames(tab2d.with.stats)
    expect_equal(res, structure(c(0.0275482093663912, 0.0606060606060606,
        0.126721763085399, 0.184573002754821, 0.247933884297521,
        0.159779614325069, 0.0606060606060606,
        0.0826446280991736, 0.0495867768595041, 1, 0.0377906976744186,
        0.159883720930233, 0.0784883720930233, 0.180232558139535, 0.197674418604651,
        0.130813953488372, 0.107558139534884, 0.0406976744186047,
        0.0668604651162791,
        1, 0.0325318246110325, 0.108910891089109, 0.103253182461103,
        0.182461103253182, 0.223479490806223, 0.145685997171146, 0.0834512022630834,
        0.0622347949080622, 0.057991513437058, 1), .Dim = c(10L, 3L),
        .Dimnames = list(c("Less than $15,000", "$15,001 to $30,000",
        "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000",
        "$90,001 to $120,000", "$120,001 to $150,000", "$150,001 to $200,000",
        "$200,001 or more", "NET"), c("Male", "Female", "NET"))))

    res <- checkMatrixNames(tab1d.with.stats)
    expect_equal(res, structure(c(0.0325318246110325, 0.108910891089109,
        0.103253182461103, 0.182461103253182, 0.223479490806223,
        0.145685997171146, 0.0834512022630834, 0.0622347949080622,
        0.057991513437058, 1), .Dim = c(10L, 1L), .Dimnames = list(
        c("Less than $15,000", "$15,001 to $30,000", "$30,001 to $45,000",
        "$45,001 to $60,000", "$60,001 to $90,000", "$90,001 to $120,000",
        "$120,001 to $150,000", "$150,001 to $200,000", "$200,001 or more",
        "NET"), "Series 1")))

    # Does not unexpectedly divide by 100
    res <- checkMatrixNames(tab.2d.nonQ)
    expect_equal(res, structure(c(21.7684616846345, 16.8273954590784,
        23.6218418617686, 22.3939814605362, 15.3883195339823, 20.2682378794749,
        22.3469131808503, 19.5399118569831, 24.9071455030989, 12.9377915795928,
        24.2191612233134, 14.4972683594848, 26.9645648467198, 22.8280612061214,
        11.4909443643606, 21.4238052135251, 16.9520193150924, 23.230939342932,
        22.1717903016062, 16.2214458268442, 21.7684616846345, 16.8273954590784,
        23.6218418617686, 22.3939814605362, 15.3883195339823),
        .Dim = c(5L, 5L), .Dimnames = list(
        c("Available", "Clean", "Dependable", "Informed", "Safe"),
        c("< 15%", "15 - 50%", "> 50% and Regional median", "Regional median",
        "NET"))))

    res <- checkMatrixNames(tab.as.char)
    expect_equal(res, structure(c(0.111111111111111,
        0.333333333333333, 0.37037037037037, 0.037037037037037, 0,
        0.148148148148148, 0, 1, 0.408284023668639,
        0.0887573964497041, 0.195266272189349, 0.0769230769230769,
        0.0355029585798817,
        0.177514792899408, 0.00591715976331361, 1, 0.541984732824427,
        0.099236641221374, 0.137404580152672, 0.106870229007634,
        0.0229007633587786,
        0.0763358778625954, 0.00763358778625954, 1, 0.437308868501529,
        0.113149847094801, 0.186544342507645, 0.0856269113149847,
        0.0275229357798165, 0.134556574923547, 0.00611620795107034, 1),
        .Dim = c(8L, 4L), .Dimnames = list(
        c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi ", "Diet Pepsi",
        "Pepsi Max", "Dislike all cola", "NET"),
        c("I am on a diet, so I tend to watch what I eat and drink",
        "I tend watch what I eat and drink, but don’t consider myself",
        "I typically eat and drink whatever I feel like", "NET"))))

    expect_equal(checkMatrixNames(1:10),
        structure(1:10, .Dim = c(10L, 1L), .Dimnames = list(c("1", "2",
        "3", "4", "5", "6", "7", "8", "9", "10"), "Series 1")))

    xx <- structure(1:10, .Names = c("A", "B", "C", "D", "E", "F", "G",
        "H", "I", "J"), statistic = "%")
    expect_equal(checkMatrixNames(xx),
        structure(c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09,
        0.1), .Dim = c(10L, 1L), .Dimnames = list(c("A", "B", "C", "D",
    "E", "F", "G", "H", "I", "J"), "Series 1")))
})

# Check function used for automatically detecting value axis formatting
test_that("isPercentData",
{
    tb1 <- structure(list(`100ab%` = c(0.5, 1, 0)), row.names = c("a", "b",
        "c"), assigned.rownames = TRUE, class = "data.frame")
    expect_equal(isPercentData(tb1), FALSE)

    expect_equal(isPercentData(tab.2d.nonQ), FALSE)
    expect_equal(isPercentData(tab1d.with.stats), TRUE)
    expect_equal(isPercentData(tab2d.with.stats), TRUE)
})

dat <- structure(c(8.25688073394496, 9.54063604240283, 16.3265306122449,
25.2293577981651, 30.7420494699647, 37.7551020408163, 33.256880733945,
35.6890459363958, 28.2312925170068, 24.7706422018349, 18.3745583038869,
11.2244897959184, 8.48623853211009, 5.65371024734982, 6.46258503401361
), .Dim = c(3L, 5L), .Dimnames = list(c("Q4 20", "Q1 21", "Q2 21"
), c("Increased more than 25%", "Increased 5% to 25%", "Stayed about the same",
"Decreased 5% to 25%", "Decreased more than 25%")))
test_that("evalHoverTemplate",
{
    expect_error(Bar(dat), NA)
    expect_error(Bar(dat, hovertext.template = "%{x}"), NA)
})

# Data source is a slice off a crosstab with multiple statistics
pd <- structure(c(43.7988826815642, 40.5586592178771, 34.1899441340782,
33.5195530726257, 28.1564245810056, 38.3240223463687, 392, 363,
306, 300, 252, 343, 0.00000455107586216918, 0.0101672137704076,
0.1647604620363, 0.0709169193677982, 0.000000274475803801506,
0.237661483248857), dim = c(6L, 3L), dimnames = list(c("Understand your bill",
"Understand the pricing plans", "Get help from customer or technical support",
"Upgrade/downgrade plans", "Cancel your subscription/plan", "Check your internet usage"
), c("Row %", "Count", "p")), label = "table.Customer.effort[, 1, ]", assigned.rownames = TRUE)
test_that("check colnames for cell statistics",
{
    expect_equal(checkMatrixNames(pd), structure(c(0.437988826815642,
        0.405586592178771, 0.341899441340782, 0.335195530726257, 0.281564245810056,
        0.383240223463687), dim = c(6L, 1L), dimnames = list(c("Understand your bill",
        "Understand the pricing plans", "Get help from customer or technical support",
        "Upgrade/downgrade plans", "Cancel your subscription/plan", "Check your internet usage"),
        "Series 1")))
    expect_error(Bar(pd, data.label.show = T, annotation.list = list(list(type="Text - after data label",
        data = "p", format = ".1e"))), NA)

})
