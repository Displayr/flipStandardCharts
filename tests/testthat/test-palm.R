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
tourist <- structure(c(52, 9, 6, 36, 4, 37, 51, 7, 6, 39, 5, 45, 79, 15,
            10, 78, 11, 46, 9, 60, 51, 44, 57, 52, 23, 7, 11, 28, 6, 30,
            17, 43, 5, 48, 3, 60, 11, 40, 8, 34, 6, 25, 3, 5, 5, 4, 1, 5), statistic = "%", .Dim = c(6L,
                                                                                                     8L), .Dimnames = list(c("Mexico", "France", "Great Britain",
                                                                                                                             "Egypt", "Australia", "China"), c("Cleanliness", "Health", "Safety",
                                                                                                                                                               "Cost", "Food", "Not being understood", "Friendliness of the people",
                                                                                                                                                               "Boredom")))
q5cc <- structure(c(25.2327048028994, 31.2881504763389, 30.9835063713764,
                    17.5546469946982, 33.2850525773139, 21.3918060868462, 14.8891326905278,
                    99.999999992966, 32.1856718881157, 39.0384865558457, 38.7008828633533,
                    23.043980297302, 41.2332045095876, 27.6781998955081, 19.7445739902877,
                    99.9999999949984, 19.3901751154243, 24.5030129463578, 24.24112651755,
                    13.1766253337998, 26.232192530017, 16.2453846830681, 11.0864828737829,
                    99.9999999901313, 22.3980226347181, 28.0282076203562, 27.7424891486241,
                    15.4048099182854, 29.9076106591684, 18.8796593933267, 13.0142006255211,
                    99.9999999917754, 25.6383779654847, 31.7498370032718, 31.4427546535753,
                    17.866375634708, 33.7617270166789, 21.753694692716, 15.1622330335653,
                    99.9999999931149, 25.3986416353672, 31.4771442945425, 31.1714948165763,
                    17.682032200925, 33.4802311923131, 21.5397609802415, 15.0006948800344,
                    99.9999999930275, 18.6950249396378, 23.6784065423265, 23.422599150159,
                    12.6692243563911, 25.3689477903583, 15.6411012525024, 10.6496959686249,
                    99.9999999896761, 25.1773988570791, 31.2251150086532, 30.9208084668817,
                    17.5122283403448, 33.2199390423663, 21.342515643933, 14.8519946407419,
                    99.9999999929453, 38.3662387414414, 45.6491679510049, 45.2968741467223,
                    28.1990522244833, 47.9233721374194, 33.4197641302466, 24.3955306686821,
                    99.9999999961865), .Dim = 8:9, .Dimnames = list(c("Coke", "Diet Coke",
                                                                      "Coke Zero", "Pepsi", "Diet Pepsi", "Pepsi Max", "None of these",
                                                                      "NET"), c("Feminine", "Health-conscious", "Innocent", "Older",
                                                                                "Open to new experiences", "Rebellious", "Sleepy", "Traditional",
                                                                                "Weight-conscious")), name = "q5 - column comparisons", questions = c("q5",
                                                                                                                                                      "SUMMARY"), statistic = "Expected %")


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

            print(pp)
            readline(prompt=paste0(filestem, ": press [enter] to continue: "))
        })
    }
}




