context("Pie")
library(flipStandardCharts)
library(flipChartBasics)

set.seed(12345)
unnamed <- 1:10
vec1d <- structure(rpois(8, 20), .Names = c("Epsilon", "Beta", "Something longer that may need wrapping", "Delta ($)", "Alpha", "Many but still very short words", "Zeta", "Eta"))
missingvals <- c(A = 1, B = 2, C = NA, D = NA, E = 0, F = 3)
dat2 <- matrix(1:12, 3, 4)
dat3 <- structure(list(City = c("Sydney", "Melbourne", "Brisbane", "Perth",
"Adelaide", "Gold Coast-Tweed Heads", "Newcastle-Maitland",
"Canberra-Queanbeyan", "Sunshine Coast", "Wollongong", "Hobart",
"Geelong", "Townsville", "Cairns", "Darwin", "Toowoomba", "Ballarat",
"Bendigo", "Albury-Wodonga", "Launceston"), Population = c(5029768,
4725316, 2360241, 2022044, 1324279, 646983, 436171, 435019, 317404,
295669, 224462, 192393, 178864, 150041, 145916, 114024, 101588,
95587, 90576, 86335), State = c("New South Wales", "Victoria",
"Queensland", "Western Australia", "South Australia", "Queensland/New South Wales",
"New South Wales", "Australian Capital Territory/New South Wales",
"Queensland", "New South Wales", "Tasmania", "Victoria", "Queensland",
"Queensland", "Northern Territory", "Queensland", "Victoria",
"Victoria", "New South Wales/Victoria", "Tasmania")), .Names = c("City",
"Population", "State"), row.names = c(NA, 20L), class = "data.frame")

opts <- c('title' = 'title = "Pie chart", global.font.family = "Courier", title.font.family = "Arial Black", title.font.color = "red"',
          'border' = 'pie.border.color = rgb(1,0,0,0.5), colors = ChartColors(10, "Reds")',
          'radius' = 'pie.inner.radius = 95, pie.values.order = "alphabetical"',
          #'decreasing' = 'pie.inner.radius = 95, pie.values.order = "decreasing"',
          'labels' = 'data.label.prefix = "<", data.label.suffix = ">", data.label.format = ".0f", data.label.font.family = "Century Gothic", pie.groups.font.family = "Arial Black", pie.groups.font.color = "red", pie.values.order = "descending", pie.groups.order = "descending"',
          'threshold' = 'pie.data.threshold = 0.05, data.label.font.color = "red"',
          'percentages' = 'pie.data.threshold = 0.05, data.label.format = ".2%"',
          'repeatcolor' = 'pie.subslice.colors.repeat = TRUE, colors = ChartColors(10, "Greys"), pie.subslice.colors = ChartColors(3, "Greens")',
          'norepeatcolor' = 'pie.subslice.colors.repeat = FALSE, colors = ChartColors(10, "Greys"), pie.subslice.colors = ChartColors(20, "Greens")')

dat.list <- c("vec1d", "unnamed", "missingvals", "dat2", "dat3")
for (dat in dat.list)
{
    for (ii in 1:length(opts))
    {
        # Create name which will appear in the error message if test fails
        # Filestem should be prefixed by test file name to avoid name conflicts
        filestem <- paste0("pie-", dat, "-", names(opts)[ii])

        test_that(filestem, {

            # Create command that will create widget
            cmd <- paste0("pp <- Pie(", dat, ",", opts[ii], ")")

            # Run command and check outputs
            # Lots of warning about colors because of the wrong lengths
            # so ignore all
            expect_error(suppressWarnings(eval(parse(text=cmd))), NA)

            #print(pp)
            #readline(prompt=paste0(filestem, ": press [enter] to continue: "))
        })
    }
}



test_that("DS-1701 Donut chart from one variable raw data",{
    z = 1:5
    names(z) = letters[1:5]
    flipStandardCharts::Pie(z)

    attr(z, "statistic") = "%"
    expect_error(Pie(z), NA)

})

test_that("DS-1768: One-column matrix", {
    dat <- structure(list(Score = c(20, 30, 50)), .Names = "Score", row.names = c("Cat",
"Dog", "Pigeon"), class = "data.frame")
    expect_error(Pie(dat), NA)
})

dat <- c(Red = 1, Orange = 0, Blue = 2, Purple = 0, Green = 1)
col <- structure(c(Red = "#FF0000", Orange = "#FF905A", Blue = "#0080FF",
   Purple = "#8000FF", Green = "#008080")) #, palette.type = c(Red = "#FF0000"))


datNA <- structure(list(V1 = c(11711, 93, NaN, 2762, NaN, NaN, NaN, NaN,
NaN, NaN, NaN, NaN, NaN, NaN), `brand:datacracker` = c(NaN, NaN,
NaN, NaN, NaN, NaN, NaN, 5, NaN, NaN, NaN, NaN, NaN, NaN), `brand:datacracker, dispatch:chrisfacer` = c(NaN,
NaN, NaN, NaN, NaN, NaN, NaN, NaN, 93, NaN, 259, NaN, NaN, 90
), `brand:datacracker, dispatch:chrisfacer, dispatch:timali` = c(NaN,
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 1, NaN, NaN, NaN, NaN
), `brand:datacracker, dispatch:chrisfacer, dispatch:timbock` = c(NaN,
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 627, NaN, 1002
), `brand:datacracker, dispatch:mattiasengdahl` = c(NaN, NaN,
NaN, NaN, NaN, NaN, NaN, NaN, 594, 8, 16, 347, 22, NA), `brand:datacracker, dispatch:mattiasengdahl, dispatch:timali` = c(NaN,
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 60, NaN, NaN, 68, NaN
), `brand:datacracker, dispatch:mattiasengdahl, dispatch:timali, dispatch:timbock` = c(NaN,
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 489, NaN
), `brand:datacracker, dispatch:mattsteele, dispatch:timbock` = c(NaN,
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NA
), `brand:datacracker, dispatch:timali` = c(NaN, NaN, NaN, NaN,
NaN, NaN, NaN, NaN, NaN, 180, 26, 134, 130, 220), `brand:datacracker, dispatch:timali, dispatch:timbock` = c(NaN,
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 460
), `brand:datacracker, dispatch:timbock` = c(NaN, NaN, NaN, NaN,
NaN, NaN, NaN, 169, 171, 325, 193, 1158, 1583, 664), `brand:displayr, dispatch:chrisfacer` = c(NaN,
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 111, NaN, NaN
), `brand:displayr, dispatch:chrisfacer, dispatch:timbock` = c(NaN,
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 150, NaN, NaN
), `brand:displayr, dispatch:mattiasengdahl` = c(NaN, NaN, NaN,
NaN, NaN, NaN, NaN, 4, NaN, NaN, NaN, NaN, NaN, 2925), `brand:displayr, dispatch:timali` = c(NaN,
NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 137, NaN, 2, NaN, NaN
), `brand:displayr, dispatch:timbock` = c(NaN, NaN, NaN, NaN,
NaN, NaN, NaN, NaN, NaN, 157, 600, NaN, NaN, 423), `dispatch:chrisfacer` = c(NaN,
43, 1034, 9, 156, 47, 1004, NaN, NaN, NaN, NaN, NaN, NaN, NaN
), `dispatch:chrisfacer, dispatch:mattiasengdahl` = c(NaN, NaN,
657, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN),
    `dispatch:chrisfacer, dispatch:timbock` = c(NaN, NaN, NaN,
    2532, NaN, 3, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN), `dispatch:mattiasengdahl` = c(NaN,
    163, 2730, 411, 2, 22, 50, 9, NaN, NaN, NaN, NaN, NaN, NaN
    ), `dispatch:mattiasengdahl, dispatch:timbock` = c(NaN, 2344,
    NaN, NaN, NaN, NaN, 249, NaN, NaN, NaN, NaN, NaN, NaN, NaN
    ), `dispatch:timali` = c(NaN, 1737, 602, 3007, 2218, 2106,
    57, NaN, NaN, NaN, NaN, NaN, NaN, NaN), `dispatch:timbock` = c(NaN,
    113, 1938, 1028, 1666, 306, 1067, NaN, NaN, NaN, NaN, NaN,
    NaN, NaN)), .Names = c("V1", "brand:datacracker", "brand:datacracker, dispatch:chrisfacer",
"brand:datacracker, dispatch:chrisfacer, dispatch:timali", "brand:datacracker, dispatch:chrisfacer, dispatch:timbock",
"brand:datacracker, dispatch:mattiasengdahl", "brand:datacracker, dispatch:mattiasengdahl, dispatch:timali",
"brand:datacracker, dispatch:mattiasengdahl, dispatch:timali, dispatch:timbock",
"brand:datacracker, dispatch:mattsteele, dispatch:timbock", "brand:datacracker, dispatch:timali",
"brand:datacracker, dispatch:timali, dispatch:timbock", "brand:datacracker, dispatch:timbock",
"brand:displayr, dispatch:chrisfacer", "brand:displayr, dispatch:chrisfacer, dispatch:timbock",
"brand:displayr, dispatch:mattiasengdahl", "brand:displayr, dispatch:timali",
"brand:displayr, dispatch:timbock", "dispatch:chrisfacer", "dispatch:chrisfacer, dispatch:mattiasengdahl",
"dispatch:chrisfacer, dispatch:timbock", "dispatch:mattiasengdahl",
"dispatch:mattiasengdahl, dispatch:timbock", "dispatch:timali",
"dispatch:timbock"), class = "data.frame", row.names = c("2017-02-12",
"2017-03-05", "2017-03-12", "2017-03-19", "2017-03-26", "2017-04-02",
"2017-04-09", "2017-04-16", "2017-04-23", "2017-04-30", "2017-05-07",
"2017-05-14", "2017-05-21", "2017-05-28"))
