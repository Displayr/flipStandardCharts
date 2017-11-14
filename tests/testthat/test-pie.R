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




