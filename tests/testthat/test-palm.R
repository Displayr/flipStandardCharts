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
                       0.03, 0.05, 0.05, 0.04, 0.01, 0.05), .Dim = c(6L, 8L), .Dimnames = list(
                           c("Mexico", "France", "Great Britain", "Egypt", "Australia",
                             "China"), c("Cleanliness", "Health", "Safety", "Cost", "Food",
                                         "Not being understood", "Friendliness of the people", "Boredom"
                             )), statistic = "%")


opts <- c('fonts' = 'x.title = "MY X-AXIS IS MINE", x.title.font.family = "Century Gothic", x.title.font.size = 18, legend.font.family = "Arial Black", legend.font.size = 18, y.tick.font.family = "Georgia", y.tick.font.size = 18, y.title = "MY Y-AXIS IS MINE", y.title.font.family = "Open Sans", y.title.font.size = 18')
opts <- c('fonts' = 'x.title = "MY X-AXIS IS MINE"')

dat.list <- c("dat1", "dat2", "dat3", "tourist")
for (dat in dat.list)
{
    for (ii in 1:length(opts))
    {
        # Create name which will appear in the error message if test fails
        # Filestem should be prefixed by test file name to avoid name conflicts
        filestem <- paste0("palm-", dat, "-", names(opts)[ii])

        test_that(filestem, {

            # Create command that will create widget
            print(paste0("pp <- Palm(", dat, ",", opts[ii], ")"))
            cmd <- paste0("pp <- Palm(", dat, ",", opts[ii], ")")

            # Run command and check outputs
            expect_error(eval(parse(text=cmd)), NA)

            #print(pp)
            #readline(prompt=paste0(filestem, ": press [enter] to continue: "))
        })
    }
}




