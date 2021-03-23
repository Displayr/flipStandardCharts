context("ChartLabels")

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

for (charting.func in c("Column", "Bar", "Line", "Radar"))
{
    cfunc <- get(charting.func)
    test_that(paste(charting.func, "ChartLabels with no annotations"),
    {
        # Some basic cases with no annotations
        pp <- cfunc(data.with.stats[-10,,])
        expect_equal(attr(pp, "ChartLabels"), NULL)

        pp <- cfunc(data.with.stats[-10,,], data.label.show = TRUE,
                   data.label.font.autocolor = TRUE)
        expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]]$ShowValue, TRUE)
        expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]]$Font$color,
                     if (charting.func %in% c("Radar", "Line")) "#5C9AD3" else "#2C2C2C")
        expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[2]]$Font$color,
                     if (charting.func %in% c("Radar", "Line")) "#ED7D31" else "#2C2C2C")
        expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[2]]$CustomPoints, NULL)

        pp <- cfunc(data.with.stats[-10,,], data.label.show = TRUE, data.label.prefix = "$")
        expect_equal(length(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoints), 9)
        expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoints[[9]],
            list(Index = 8, Segments = list(list(Text = "$"), list(Field = "Value"))))

        if (charting.func == "Line")
        {
            pp <- cfunc(data.with.stats[-10,,], data.label.show = TRUE,
                       data.label.show.at.ends = TRUE)
            expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]]$ShowValue, TRUE)
            expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoints[[1]]$Index, 1)
            expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoints[[1]]$ShowValue, FALSE)
        }
    })

    test_that(paste(charting.func, "ChartLabels with data label annotations"),
    {
        # Some simple cases with annotations
        pp <- cfunc(data.with.stats[-10,,], data.label.show = TRUE,
            annotation.list = list(list(type = "Border", data = "p",
            threstype = "below threshold", threshold = "0.05",
            color = "#ff0000", width = 2, offset = 1)))
        expect_equal(length(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoints), 4)
        expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoints[[4]],
            list(Index = 7, OutlineStyle = "Solid",
            OutlineColor = "#ff0000", OutlineWidth = 1.50003750093752,
            ShowValue = TRUE))
        expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[3]]$CustomPoints, NULL)

        pp <- cfunc(data.with.stats[-10,,], data.label.show = TRUE,
            annotation.list = list(list(type = "Hide", data = "p",
            threstype = "above threshold", threshold = "0.05",
            color = "#ff0000", width = 2, offset = 1)))
        #expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]]$ShowValue, FALSE)
        expect_equal(length(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoints), 5)
        expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoints[[1]],
            list(Index = 0, ShowValue = FALSE))

        pp <- cfunc(data.with.stats[-10,,], data.label.show = TRUE,
            annotation.list = list(list(type = "Custom text", data = "p",
            threstype = "below threshold", threshold = "0.05", custom.symbol = "*",
            color = "#ff0000", size = 21, offset = 1)))
        expect_equal(length(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoints), 4)
        expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[2]]$ShowValue, TRUE)
        expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoints[[3]],
            list(Index = 6, Segments = list(list(Field = "Value"), list(
            Font = list(color = "#ff0000", size = 15.7539384846212), Text = "*"))))
    })
}
