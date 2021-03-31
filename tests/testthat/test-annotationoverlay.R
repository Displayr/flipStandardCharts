# Testing annotation overlays which are similar but more flexible than
# the annotations attached to data labels

context("Annotation overlays")

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

test_that("Overlay annotations",
{
    expect_error(Column(data.with.stats[-10,-3,], type = "Column",
        overlay.annotation.list = list(list(type = "Arrow - up", data = "p",
        threstype = "below threshold", threshold = "0.05",
        relative.pos = 0.0,  valign = "top", halign = "center",
        offset = 5, color = "red", size = 20,
        font.family = "Arial"), list(type = "Arrow - down", data = "p",
        threstype = "above threshold", threshold = "0.05", color = "blue",
        relative.pos = 1.0,  valign = "top", halign = "center",
        size = 20, offset = 5, font.family = "Arial",
        font.weight = "normal", font.style = "normal"))), NA)

    expect_error(SmallMultiples(data.with.stats[-10,-3,], "Column",
        overlay.annotation.list = list(list(type = "Arrow - up", data = "p",
        threstype = "below threshold", threshold = "0.05",
        relative.pos = 0.0,  valign = "top", halign = "center",
        offset = 5, color = "red", size = 20,
        font.family = "Arial"), list(type = "Arrow - down", data = "p",
        threstype = "above threshold", threshold = "0.05", color = "blue",
        relative.pos = 1.0,  valign = "top", halign = "center",
        size = 20, offset = 5, font.family = "Arial",
        font.weight = "normal", font.style = "normal"))), NA)


    expect_error(SmallMultiples(data.with.stats[-10,-3,], "Radar",
        overlay.annotation.list = list(list(type = "Custom text",
        custom.symbol = "*", data = "p",
        threstype = "below threshold", threshold = "0.05",
        color = "red", size = 20, font.family = "Arial")),
        x.tick.font.size = 9, average.show = TRUE), NA)

    expect_error(Radar(data.with.stats[-10,-3,],
        overlay.annotation.list = list(list(type = "Caret - up", data = "p",
        threstype = "below threshold", threshold = "0.05",
        color = NULL, size = 20,
        font.family = "Arial"), list(type = "Caret - down", data = "p",
        threstype = "above threshold", threshold = "0.05", color = NULL,
        size = 20, font.family = "Arial",
        font.weight = "normal", font.style = "normal"))), NA)

    expect_error(Column(data.with.stats[-10,-3,], type = "Stacked", bar.gap = 0.3,
        data.label.show = TRUE,
        overlay.annotation.list = list(list(type = "Custom symbol",
        custom.symbol = "★", data = "p",
        threstype = "below threshold", threshold = "0.05",
        relative.pos = 0.5,  valign = "middle", halign = "right",
        offset = 25, color = "red", size = 12,
        font.family = "Arial"),
        list(type = "Text", data = "p", format = ".2e", prefix = "p = ",
        threstype = "below threshold", threshold = "0.05", color = "#555555",
        relative.pos = 0.0,  valign = "top", halign = "center",
        size = 6, offset = 2, font.family = "Arial",
        font.weight = "normal", font.style = "normal"))), NA)
})

dat.with.colcmp <- structure(c("58.3333333333333", "41.6666666666667", "64.2857142857143",
"35.7142857142857", "50.188679245283", "49.811320754717", "25",
"75", "48.8054607508532", "51.1945392491468", "35.7142857142857",
"64.2857142857143", "0", "100", "1.44309826516645", "-1.44309826516645",
"2.88928578847612", "-2.88928578847612", "0.32397254347378",
"-0.32397254347378", "-3.16356487521093", "3.16356487521093",
"-0.244941520775787", "0.244941520775787", "-2.12026368475829",
"2.12026368475829", "-1.39839409648114", "1.39839409648114",
"0.148992791985379", "0.148992791985379", "0.00386117978910683",
"0.00386117978910683", "0.745958821255696", "0.745958821255696",
"0.00155849612347869", "0.00155849612347869", "0.806501711785767",
"0.806501711785767", "0.0339838143903261", "0.0339838143903261",
"0.161994754984221", "0.161994754984221", "d f", NA, "c D e F",
NA, "d f", "b", NA, "a B c e", "d", "b", NA, "a B c", NA, NA), .Dim = c(2L,
7L, 4L), .Dimnames = list(c("Male", "Female"), c("Living with your parents/guardian",
"Living alone", "Living with partner only", "Living with children only",
"Living with partner and children", "Sharing accommodation",
"Other (Please type into the box.)"), c("Column %", "z-Statistic",
"p", "Column Comparisons")), basedescriptiontext = "sample size = 800", basedescription = list(
    Minimum = 800L, Maximum = 800L, Range = FALSE, Total = 800L,
    Missing = 0L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = c("PickOne", "PickOne"
), span = list(rows = structure(list(c("Male", "Female", "NET"
)), class = "data.frame", .Names = "", row.names = c(NA, 3L)),
    columns = structure(list(c("Living with your parents/guardian",
    "Living alone", "Living with partner only", "Living with children only",
    "Living with partner and children", "Sharing accommodation",
    "Other (Please type into the box.)", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
    8L))), name = "table.Gender.by.Living.arrangements.2", questions = c("Gender",
"Living arrangements"), assigned.rownames = TRUE)

test_that("Showing column comparisons with arrows",
{
    expect_error(Column(dat.with.colcmp, type = "Stacked",
        overlay.annotation.list = list(list(type = "Arrow - up",
        data = "Column Comparisons", threstype = "above threshold", threshold = " ",
        relative.pos = 0.5, halign = "Center", valign = "Middle", offset = 0,
        color = "#FF0000", size = 11, font.family = "Arial"))), NA)

    expect_error(pp <- Radar(aperm(dat.with.colcmp, c(2,1,3)),
        overlay.annotation.list = list(list(type = "Arrow - up",
        data = "Column Comparisons", threstype = "above threshold", threshold = " ",
        color = NULL, size = 11, font.family = "Impact"))), NA)
    expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]]$ShowValue, FALSE)
    expect_equal(length(attr(pp, "ChartLabels")$SeriesLabels[[2]]$CustomPoints), 4)
    expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[2]]$CustomPoints[[4]],
        list(Index = 5, Segments = list(list(Field = "CategoryName"),
        list(Font = list(color = "#ED7D31", size = 8.25206301575394,
        family = "Impact"), Text = "a↑ B↑ c↑"))))

    expect_error(pp <- Column(dat.with.colcmp,
        overlay.annotation.list = list(list(type = "Arrow - up",
        data = "Column Comparisons", threstype = "above threshold", threshold = " ",
        relative.pos = 0, halign = "Center", valign = "Top", offset = 5,
        color = "#444444", size = 11, font.family = "Courier New"),
        list(type = "Arrow - down", data = "z-Statistic",
        threstype = "below threshold", threshold = "-1.96",
        relative.pos = 1.0, halign = "Center", valign = "Top", offset = 5,
        color = "#FF0000", size = 15),
        list(type = "Arrow - up", data = "z-Statistic",
        threstype = "above threshold", threshold = "1.96",
        relative.pos = 1.0, halign = "Center", valign = "Top", offset = 5,
        color = "#0000FF", size = 15))), NA)
    expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[2]]$CustomPoint[[1]]$Segments[[1]]$Text,
                 "c↑ D↑ e↑ F↑")
    expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[2]]$CustomPoint[[1]]$Segments[[2]]$Text,
                 "↑")
})

tb1d.with.single.stat <- structure(c(`Less than $15,000` = 3.25318246110325, `$15,001 to $30,000` = 10.8910891089109,
`$30,001 to $45,000` = 10.3253182461103, `$45,001 to $60,000` = 18.2461103253182,
`$60,001 to $90,000` = 22.3479490806223, `$90,001 to $120,000` = 14.5685997171146,
`$120,001 to $150,000` = 8.34512022630834, `$150,001 to $200,000` = 6.22347949080622,
`$200,001 or more` = 5.7991513437058), .Dim = 9L, .Dimnames = list(
    c("Less than $15,000", "$15,001 to $30,000", "$30,001 to $45,000",
    "$45,001 to $60,000", "$60,001 to $90,000", "$90,001 to $120,000",
    "$120,001 to $150,000", "$150,001 to $200,000", "$200,001 or more")), statistic = "%",
    basedescriptiontext = "sample size = 707; total sample size = 800; 93 missing",
    basedescription = list(Minimum = 707L, Maximum = 707L, Range = FALSE, Total = 800L,
    Missing = 93L, EffectiveSampleSize = 707L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = "PickOne", span = list(
    rows = structure(list(c("Less than $15,000", "$15,001 to $30,000",
    "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000",
    "$90,001 to $120,000", "$120,001 to $150,000", "$150,001 to $200,000",
    "$200,001 or more", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
    10L))), name = "table.Income", questions = c("Income", "SUMMARY"
), assigned.rownames = TRUE)

aa.txt <- list(list(type = "Text", data = "%", custom.symbol = NULL, threstype = "above threshold",
    threshold = " 8", relative.pos = 0, halign = "Center",
    valign = "Top", offset = 5, color = "#CD343C", size = 15,
    format = "", prefix = "", suffix = "", font.family = "Arial"))


test_that("Annotations for tables with only one statistic",
{
    expect_error(Column(tb1d.with.single.stat, overlay.annotation.list = aa.txt), NA)
})
