context("Palm")
library(flipStandardCharts)
library(flipChartBasics)

set.seed(12345)

dat1 <- matrix(1:12, 3, 4)
dat2 <- matrix(1:60, 12, 5)
dat2[3, 3] <- NA
colnames(dat2) <- c("Bayswater", "Queensway", "Hammersmith", "Holland Park", "Notting Hill")
rownames(dat2) <- LETTERS[1:12]
dat3 <- matrix(runif(100), 5, 20)
rownames(dat3) <- c("Mon", "Tue", "Wed", "Thu", "Fri")
tourist <- structure(c(0.52, 0.09, 0.06, 0.36, 0.04, 0.37, 0.51, 0.07, 0.06,
                      0.39, 0.05, 0.45, 0.79, 0.15, 0.1, 0.78, 0.11, 0.46, 0.09, 0.6,
                      0.51, 0.44, 0.57, 0.52, 0.23, 0.07, 0.11, 0.28, 0.06, 0.3, 0.17,
                      0.43, 0.05, 0.48, 0.03, 0.6, 0.11, 0.4, 0.08, 0.34, 0.06, 0.25,
                      0.03, 0.05, 0.05, 0.04, 0.01, 0.05), statistic = "%", .Dim = c(6L,
                                                                                     8L), .Dimnames = list(c("Mexico", "France", "Great Britain",
                                                                                                             "Egypt", "Australia", "China"), c("Cleanliness", "Health", "Safety",
                                                                                                                                               "Cost", "Food", "Not being understood", "Friendliness of the people",
                                                                                                                                               "Boredom")))



q5cc <- structure(c(0.252327048028994, 0.312881504763389, 0.309835063713764,
                    0.175546469946982, 0.332850525773139, 0.213918060868462, 0.148891326905278,
                    0.99999999992966, 0.321856718881157, 0.390384865558457, 0.387008828633533,
                    0.23043980297302, 0.412332045095876, 0.276781998955081, 0.197445739902877,
                    0.999999999949984, 0.193901751154243, 0.245030129463578, 0.2424112651755,
                    0.131766253337998, 0.26232192530017, 0.162453846830681, 0.110864828737829,
                    0.999999999901313, 0.223980226347181, 0.280282076203562, 0.277424891486241,
                    0.154048099182854, 0.299076106591684, 0.188796593933267, 0.130142006255211,
                    0.999999999917754, 0.256383779654847, 0.317498370032718, 0.314427546535753,
                    0.17866375634708, 0.337617270166789, 0.21753694692716, 0.151622330335653,
                    0.999999999931149, 0.253986416353672, 0.314771442945425, 0.311714948165763,
                    0.17682032200925, 0.334802311923131, 0.215397609802415, 0.150006948800344,
                    0.999999999930275, 0.186950249396378, 0.236784065423265, 0.23422599150159,
                    0.126692243563911, 0.253689477903583, 0.156411012525024, 0.106496959686249,
                    0.999999999896761, 0.251773988570791, 0.312251150086532, 0.309208084668817,
                    0.175122283403448, 0.332199390423663, 0.21342515643933, 0.148519946407419,
                    0.999999999929453, 0.383662387414414, 0.456491679510049, 0.452968741467223,
                    0.281990522244833, 0.479233721374194, 0.334197641302466, 0.243955306686821,
                    0.999999999961865), name = "q5 - column comparisons", questions = c("q5",
                                                                                        "SUMMARY"), statistic = "Expected %", .Dim = 8:9, .Dimnames = list(
                                                                                            c("Coke", "Diet Coke", "Coke Zero", "Pepsi", "Diet Pepsi",
                                                                                              "Pepsi Max", "None of these", "NET"), c("Feminine", "Health-conscious",
                                                                                                                                      "Innocent", "Older", "Open to new experiences", "Rebellious",
                                                                                                                                      "Sleepy", "Traditional", "Weight-conscious")))


opts <- c('fonts' = 'x.title = "MY X-AXIS IS MINE", x.title.font.family = "Century Gothic", x.title.font.size = 18, legend.font.family = "Arial Black", legend.font.size = 18, y.tick.font.family = "Georgia", y.tick.font.size = 18, y.title = "MY Y-AXIS IS MINE", y.title.font.family = "Open Sans", y.title.font.size = 18',
          'percent' = 'y.tick.format = ".3%", y.tick.prefix = "PRE", y.tick.suffix = "SUF"',
          'colorsnums' = 'colors = ChartColors(10, "Blues"), y.tick.format = ".1f"',
          'titlenoaxis' = 'y.axis.show = FALSE, x.title = "something here", y.title = "something else"')

dat.list <- c("dat1", "dat2", "dat3", "tourist", "q5cc")
for (dat in dat.list)
{
    for (ii in 1:length(opts))
    {
        # Create name which will appear in the error message if test fails
        # Filestem should be prefixed by test file name to avoid name conflicts
        filestem <- paste0("palm-", dat, "-", names(opts)[ii])

        test_that(filestem, {

            # Create command that will create widget
            cmd <- paste0("pp <- Palm(", dat, ",", opts[ii], ")")

            # Run command and check outputs
            expect_error(suppressWarnings(eval(parse(text=cmd))), NA)

            #print(pp)
            #readline(prompt=paste0(filestem, ": press [enter] to continue: "))
        })
    }
}

test_that("No data",
{
    expect_error(Palm(matrix(NA, 0, 0)), "There is not enough data to create a plot")
    expect_error(Palm(c()), "The data is not in an appropriate format")
})

test_that("Strip alpha values",
{
    expect_warning(Palm(tourist, global.font.color = "#0000FF80"),
                   "Alpha values for colors in Palm trees are ignored")

})


