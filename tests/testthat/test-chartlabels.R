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
        if (charting.func == "Radar")
            expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoints[[9]],
            list(Index = 8, Segments = list(list(Field = "CategoryName"),
            list(Text = "$"), list(Field = "Value"))))
        else
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
        expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoints[[4]]$Index, 7)
        expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoints[[4]]$OutlineStyle, "Solid")
        expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoints[[4]]$OutlineColor, "#ff0000")
        expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoints[[4]]$OutlineWidth, 1.5, tol=1e-2)
        expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoints[[4]]$OutlineWidth, 1.5, tol=1e-2)
        expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoints[[4]]$ShowValue, TRUE)
        expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[3]]$CustomPoints, NULL)

        pp <- cfunc(data.with.stats[-10,-3,], data.label.show = TRUE,
            annotation.list = list(list(type = "Hide", data = "p",
            threstype = "above threshold", threshold = "0.05",
            color = "#ff0000", width = 2, offset = 1)))
        #expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]]$ShowValue, FALSE)
        expect_equal(length(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoints), 5)
        expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoints[[1]]$Index, 0)
        expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoints[[1]]$ShowValue, FALSE)

        pp <- cfunc(data.with.stats[-10,,], data.label.show = TRUE,
            annotation.list = list(list(type = "Custom text", data = "p",
            threstype = "below threshold", threshold = "0.05", custom.symbol = "*",
            color = "#ff0000", size = 21, offset = 1)))
        expect_equal(length(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoints), 4)
        expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[2]]$ShowValue, TRUE)
        tmp.seg <- list(list(Field = "Value"), list(
            Font = list(color = "#ff0000", size = 15.7539384846212), Text = "*"))
        if (charting.func == "Radar")
            tmp.seg <- c(list(list(Field = "CategoryName")), tmp.seg)
        expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoints[[3]],
            list(Index = 6, Segments = tmp.seg))

        pp <- cfunc(data.with.stats[-10,-3,], data.label.show = TRUE,
            annotation.list = list(list(type = "Recolor text", data = "p",
            threstype = "below threshold", threshold = "0.05", color = "#FF0000")))
        expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[2]]$CustomPoints[[4]]$Index, 7)
        expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[2]]$CustomPoints[[4]]$Segments[[1]]$Font$color, "#FF0000")


        pp <- Line(data.with.stats[-10, 1, , drop = FALSE], data.label.show = TRUE,
            annotation.list = list(list(type = "Custom text", data = "p",
            threstype = "below threshold", threshold = "0.05", custom.symbol = "*",
            color = "#ff0000", size = 21, offset = 1), list(type = "Recolor text", data = "p",
            threstype = "below threshold", threshold = "0.05", color = "#FF0000")))
        expect_equal(length(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoints), 4)
        expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoints[[3]]$Index, 6)
        expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoints[[3]]$Segments[[1]]$Font,
            list(color = "#FF0000"))
    })
}

test_that("Multi color labels", {
    xx <- c(A = 1, B = 2, C = 3)
    pp <- BarMultiColor(xx, data.label.show = TRUE,
        data.label.font.color = "#FF0000,#00FF00,#0000FF")
    expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoints, list(
        list(Index = 0, Segments = list(list(Field = "Value", Font = list(color = "#FF0000")))),
        list(Index = 1, Segments = list(list(Field = "Value", Font = list(color = "#00FF00")))),
        list(Index = 2, Segments = list(list(Field = "Value", Font = list(color = "#0000FF"))))))

    pp <- ColumnMultiColor(xx, data.label.show = TRUE, data.label.prefix = "<",
        data.label.suffix = ">", data.label.font.color = "#FF0000,#00FF00,#0000FF")
    expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoints, list(
        list(Index = 0, Segments = list(list(Text = "<", Font = list(color = "#FF0000")),
            list(Field = "Value", Font = list(color = "#FF0000")),
            list(Text = ">", Font = list(color = "#FF0000")))),
        list(Index = 1, Segments = list(list(Text = "<", Font = list(color = "#00FF00")),
            list(Field = "Value", Font = list(color = "#00FF00")),
            list(Text = ">", Font = list(color = "#00FF00")))),
        list(Index = 2, Segments = list(list(Text = "<", Font = list(color = "#0000FF")),
            list(Field = "Value", Font = list(color = "#0000FF")),
            list(Text = ">", Font = list(color = "#0000FF"))))))

    annot.list <- list(list(type = "Recolor text", data = "",
        threstype = "below threshold",threshold = 2, color = "#CCCCCC"),
        list(type = "Arrow - down", data = "", threstype = "above threshold",
        threshold = 1, color = "#FF0000", size = 12),
        list(type = "Hide", data = "", threstype = "above threshold",
        threshold = 2))
    pp <- BarMultiColor(xx, annotation.list = annot.list,
        data.label.show = TRUE, data.label.prefix = "<", data.label.suffix = ">",
        data.label.font.color = "#FF0000,#00FF00,#0000FF")
    expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]]$CustomPoint, list(
        list(Index = 0, Segments = list(list(Text = "<", Font = list(color = "#CCCCCC")),
        list(Field = "Value", Font = list(color = "#CCCCCC")),
        list(Text = ">", Font = list(color = "#CCCCCC")))),
        list(Index = 1, Segments = list(list(Text = "<", Font = list(color = "#00FF00")),
        list(Field = "Value", Font = list(color = "#00FF00")),
        list(Text = ">", Font = list(color = "#00FF00")),
        list(Font = list(color = "#FF0000", size = 9.00225056264066), Text = "↓")))))
})

set.seed(12345)
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
    pp <- Scatter(dat)
    expect_equal(attr(pp, "ChartLabels")$SeriesLabels, NULL)

    pp <- Scatter(dat, annotation.list = list(list(type = "Text - after data label",
        data = "Cost", threstype = "above threshold", threshold = "1.65",
        color = "red", size = 8, width = 1, font.family = "Arial",
        font.weight = "normal", font.style = "normal", format = ".2f", prefix = "$"),
        list(type = "Border", data = "Cost", threstype = "above threshold", threshold = "2.0",
        color = "grey", width = 2)))
    expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[4]]$ShowValue, FALSE)
    expect_equal(length(attr(pp, "ChartLabels")$SeriesLabels[[2]]$CustomPoints), 3)
    expect_equal(attr(pp, "ChartLabels")$SeriesLabels[[1]],
    list(Font = list(color = "#2C2C2C", size = 7.50187546886722),
    ShowValue = FALSE, CustomPoints = list(list(Index = 11, Segments = list(
        list(Font = list(color = "red", size = 6.00150037509377,
            family = "Arial", bold = FALSE, italic = FALSE),
            Text = "$2.20")), OutlineStyle = "Solid", OutlineColor = "grey",
        OutlineWidth = 1.50003750093752))))
})
