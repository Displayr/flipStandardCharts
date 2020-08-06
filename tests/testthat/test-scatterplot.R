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

