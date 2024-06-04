context("Scatter plot")
library("flipChartBasics")

# Set up dataframe containing different types of data types
set.seed(1234)
dat <- data.frame('Score' = rnorm(20),
                  'Cost ($)' = abs(rnorm(20)), # check plotly is handling '$' properly
                  'Age' = rpois(20, 40),
                  'Class' = factor(sample(LETTERS[4:1], 20, replace = TRUE), levels = LETTERS[4:1]), # reverse order to check DS-1645
                  'Sporadic' = c(1:5, NA, 6:10, NA, NA, 11:12, NA, NA, 13:15), # missing values
                  'Date' = as.Date(sprintf("2017-01-%02d", 20:1)),
                   check.names = FALSE, stringsAsFactors = FALSE)
rownames(dat) <- letters[1:20]

# Set up matrix to use the different variable types
grid2 <- expand.grid(0:6, 0:6)
tmp <- cbind(rbind(c(NA, 6), c(6, NA), grid2[2:49,]), grid2[c(1, 49:1),])

columns.str <- sprintf("scatter.x.column = %.0f, scatter.y.column = %d,
                        scatter.colors.column = %d, scatter.sizes.column = %d",
                       tmp[,1], tmp[,2], tmp[,3], tmp[,4])
names(columns.str) <- apply(tmp, 1, paste, collapse="")

# These are only the options that can be used by both Labeled and (plotly) Scatterplots
# Line of best fit is already tested in test-backgrounds.R
opts <- c('default' = 'colors = ChartColors(5, "Blues")',
         'categoricalcolor' = 'scatter.colors.as.categorical = TRUE, legend.font.color = "red"',
         'numericalcolor' = 'scatter.colors.as.categorical = FALSE, colors = grey(1:4/5)',
         'nolegend' = 'legend.show = FALSE, colors = "red"',
         'markerbig' = 'marker.size = 20, grid.show = FALSE',
         'thickxgrid' = 'x.grid.width = 10, global.font.color = "red", global.font.family = "Courier"')

n <- length(opts)
index <- 1
for (func in c("Scatter", "LabeledScatter"))
{
    for (ii in 1:length(columns.str))
    {
        jj <- n - (index %% n)
        filestem <- paste0(tolower(func), "-", names(columns.str)[ii], "-", names(opts)[jj])
        test_that(filestem, {

            cmd <- paste0("pp <- ", func, "(dat, ", columns.str[ii], ", ", opts[jj], ")")
            expect_error(suppressWarnings(eval(parse(text = cmd))), NA)

            #print(pp)
            #readline(prompt=paste0(filestem, ": press [enter] to continue: "))
        })
        index <- index + 1
    }
}

test_that("Labeled Scatter arguments",
{
    expect_error(LabeledScatter(dat, scatter.y.column = 4, scatter.x.column = 6, y.tick.prefix = "$",
        label.auto.placement = TRUE, legend.bubbles.show = FALSE), NA)

    # Sometime, this is what gets passed in from Standard R form
    expect_error(LabeledScatter(dat, legend.bubbles.show = NULL), NA)
})

test_that("Labeled Scatter axis type",
{
    # X axis is being reordered categorically
    expect_error(LabeledScatter(c("dog", "bird", "apple"), 1:3), NA)
    expect_error(LabeledScatter(dat, scatter.x.column = 4, scatter.y.column = 1,
        x.tick.format = "Category", y.tick.format = "Category"), NA)
    expect_error(LabeledScatter(dat, scatter.x.column = 6, scatter.y.column = 1,
        x.tick.format = "Category", y.tick.format = "Category"), NA)

    expect_warning(LabeledScatter(c("dog", "bird", "apple"), 1:3, x.tick.format = ".2f"),
        "incompatible with axis type")
    expect_warning(LabeledScatter(1:4, Sys.Date() + 1:4, y.tick.format = ".2f"),
        "incompatible with axis type")

    expect_error(LabeledScatter(dat, scatter.x.column = 6, scatter.y.column = 1,
        x.tick.format = "%B %d"), NA)
})

test_that("Scatter with trend line that cannot be predicted",
{
    dat.no.xvariation <- structure(list(Started = structure(
        c(3L, 3L, 3L, 3L, 3L, 3L,
        3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
        3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
        3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
        3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
        3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
        3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L),
        .Label = c("9/7/2020", "9/8/2020", "9/9/2020"),
        class = c("ordered", "factor")), DurationMilliseconds = c(NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 455.12, 663.41,
        1.1, 1419.085, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 448.115,
        660.445, 1.13, 1412.935, 1718.585, 46865.67, 0.38, 2743.15, NA,
        NA, NA, NA, NA, 376.55, 624.395, 0.6, 1094.925, NA, 377.97, 631.765,
        0.41, 1104.43, 745.95, 3633.58, 0.245, 2695.135, NA, NA, NA,
        NA, 812.935, 640.84, 0.52, 1161.53, NA, 822.665, 687.69, 1.26,
        1360.375, 1196.515, 3577.61, 0.245, 2740.4, NA, NA, NA, NA, 364.275,
        640.185, 15.15, 1132.37, NA, 361.205, 633.53, 0.4, 1085.64, 696.92,
        4001.16, 0.245, 2670.62, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA)), assigned.rownames = FALSE,
        scatter.variable.indices = c(x = 1,
        y = 2, sizes = NA, colors = NA, groups = NA), row.names = c(1L,
        2L, 5L, 8L, 11L, 14L, 17L, 20L, 23L, 41L, 65L, 103L, 141L, 144L,
        149L, 150L, 151L, 152L, 170L, 225L, 279L, 305L, 318L, 338L, 360L,
        395L, 415L, 472L, 518L, 519L, 520L, 521L, 537L, 538L, 539L, 540L,
        607L, 608L, 617L, 626L, 636L, 641L, 642L, 643L, 644L, 660L, 700L,
        701L, 702L, 703L, 719L, 720L, 721L, 722L, 785L, 878L, 939L, 1007L,
        1012L, 1013L, 1014L, 1015L, 1031L, 1105L, 1106L, 1107L, 1108L,
        1124L, 1125L, 1126L, 1127L, 1190L, 1199L, 1208L, 1217L, 1222L,
        1223L, 1224L, 1225L, 1241L, 1277L, 1278L, 1279L, 1280L, 1296L,
        1297L, 1298L, 1299L, 1362L, 1363L, 1366L, 1369L, 1372L, 1375L,
        1378L, 1381L, 1384L, 1387L, 1426L, 1465L), class = "data.frame")

    expect_warning(Scatter(dat.no.xvariation, fit.type = "Loess", fit.CI.show = FALSE),
                   "Could not fit trend line to data. Check that you expect to map a single x-value to a single y-value.")
})
