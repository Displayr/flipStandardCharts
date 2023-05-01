context("Annotations")
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

tb.as.char <- structure(c("11.1111111111111", "33.3333333333333", "37.037037037037",
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
    "-", "-", "-", "-", "-", "-", "-", "-", "FALSE", "FALSE", "FALSE",
    "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE",
    "FALSE", "FALSE", "TRUE", "FALSE", "FALSE", "FALSE", "FALSE",
    "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE",
    "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE"), .Dim = c(8L, 4L, 4L), .Dimnames = list(
        c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi ", "Diet Pepsi",
        "Pepsi Max", "Dislike all cola", "NET"), c("I am on a diet, so I tend to watch what I eat and drink",
        "I tend watch what I eat and drink, but don’t consider myself",
        "I typically eat and drink whatever I feel like", "NET"),
        c("Column %", "z-Statistic", "Column Comparisons", "Signif")), basedescriptiontext = "sample size = 327", basedescription = list(
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


test_that("getAnnotData",
{
    expect_error(getAnnotData(tab2d.with.stats, "z-Statistic", 3),
        "Annotation data does not contain a statistic named 'z-Statistic'. Allowable names are: 'Column %', 'p'.",
        fixed = TRUE)
    expect_equal(getAnnotData(tab2d.with.stats, "p", 2),
        c(0.442913092343004, 2.28306627828578e-05, 0.0351514682974756,
        0.881274082835059, 0.108843509396061, 0.275202305069102, 0.0240561692086175,
        0.0210216801333983, 0.326003170694519, NA))
    expect_equal(getAnnotData(tab2d.with.stats, "p", 3), rep(NA_real_, 10))

    expect_equal(getAnnotData(tab1d.with.stats, "p", 1),
        c(2.96352779053704e-11, 0.852323672450741, 0.506154634139539,
        1.57276658363514e-09, 0, 0.00344142405418046, 0.0192720273455812,
        3.54510822852101e-05, 6.97993687950735e-06, 0))

    expect_equal(getAnnotData(tb.as.char, "z-Statistic", 2),
        c(-1.09428239999305, -1.4400978823522, 0.418738876007761, -0.581744715753815,
        0.912232950688339, 2.35430724535457, -0.0477461092441626, NA))
    expect_equal(getAnnotData(tb.as.char, "Column Comparisons", 2), rep(NA_real_, 8))
    expect_equal(getAnnotData(tb.as.char, "Column Comparisons", 2, as.numeric = FALSE),
        c(`Coca-Cola` = "a", `Diet Coke` = NA, `Coke Zero` = NA, `Pepsi ` = NA,
        `Diet Pepsi` = NA, `Pepsi Max` = "c", `Dislike all cola` = NA,
        NET = "-"))
    expect_equal(getAnnotData(tb.as.char, "Signif", 2, as.numeric = T),
        c(`Coca-Cola` = 0, `Diet Coke` = 0, `Coke Zero` = 0, `Pepsi ` = 0,
          `Diet Pepsi` = 0, `Pepsi Max` = 1, `Dislike all cola` = 0, NET = 0))
})

test_that("extractSelectedAnnot",
{
    expect_equal(extractSelectedAnnot(1:10, "", NULL), 1:10)
    expect_equal(extractSelectedAnnot(1:10, "", "above threshold"), 1:10)
    expect_equal(extractSelectedAnnot(1:10, 5, "below threshold"), 1:4)

    # string comparison
    expect_equal(extractSelectedAnnot(tb.as.char[,2,"Column Comparisons"], "a",
        "above threshold"), c(`Pepsi Max` = 6L))
    expect_equal(extractSelectedAnnot(tb.as.char[,2,"Column Comparisons"], "",
        "above threshold"), c(`Coca-Cola` = 1L, `Pepsi Max` = 6L, NET = 8L))
})

test_that("parseThreshold",
{
    expect_equal(parseThreshold("0.5"), 0.5)
    expect_equal(parseThreshold("Some text"), "Some text")
})

test_that("addAnnotToDataLabel",
{
    for (tt in c("Shadow", "Border", "Arrow - up", "Arrow - down",
                 "Caret - up", "Caret - down",
                "Hide", "Text - before data label", "Text - after data label"))
    {
        a.tmp <- list(type = tt, data = "p",
            threstype = "above threshold", threshold = "-Inf",
            color = "red", size = 20, width = 1,
            offset = 0, font.family = "Arial", font.weight = "normal",
            font.style = "normal", format = ".2f")
        if (tt == "Hide")
            expect_equal(addAnnotToDataLabel("d1", a.tmp, "p1"), "")
        else if (tt == "Text - before data label")
        {
            dlab <- paste0("d", 1:3)
            aout <- addAnnotToDataLabel(dlab, a.tmp, 1:3)
            alen <- nchar(aout)
            expect_equal(substr(aout, alen-1, alen), dlab)

        } else if (tt == "Text - after data label")
        {
            dlab <- paste0("d", 1:3)
            aout <- addAnnotToDataLabel(dlab, a.tmp, 1:3)
            alen <- nchar(aout)
            expect_equal(substr(aout, 1, 2), dlab)

        } else
            expect_error(addAnnotToDataLabel("d1", a.tmp, "p1"), NA)
            # The rest of the annotation types are checked in visual tests
    }
})

