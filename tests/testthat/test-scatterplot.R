context("Scatter plot")
library("flipChartBasics")

# Set up dataframe containing different types of data types
# None with missing values???
set.seed(1234)
dat <- data.frame('Score' = rnorm(20),
                  'Cost ($)' = abs(rnorm(20)), # check plotly is handling '$' properly
                  'Age' = rpois(20, 40),
                  'Class' = factor(sample(LETTERS[4:1], 20, replace = TRUE)),
                  'Desc' =  stringi::stri_rand_strings(20, 5),
                  #'Sporadic' = c(1, NA, rpois(17, 2), NA),
                  'Date' = as.Date(sprintf("2017-01-%02d", 1:20)),
                   check.names = FALSE, stringsAsFactors = FALSE)
rownames(dat) <- letters[1:20]

# Set up matrix to use the different variable types
#tmp <- expand.grid(0:6, 0:6, 0:6, 0:6)
tmp.attr <- expand.grid(0:6, 0:6)
tmp.var <- t(combn(0:6, 2))
tmp <- cbind(tmp.var[c(1:21, 1:21, 1:7),], tmp.attr)

columns.str <- sprintf("scatter.x.column = %d, scatter.y.column = %d,
                        scatter.colors.column = %d, scatter.sizes.column = %d",
                       tmp[,1], tmp[,2], tmp[,3], tmp[,4])
names(columns.str) <- apply(tmp, 1, paste, collapse="")

# These are only the options that can be used by both Labeled and (plotly) Scatterplots
# Line of best fit is already tested in test-backgrounds.R
opts <- c('default' = 'colors = ChartColors(5, "Blues")',
         'categoricalcolor' = 'scatter.colors.as.categorical = TRUE, legend.font.color = "red"',
         'numericalcolor' = 'scatter.colors.as.categorical = FALSE, colors = grey(1:4/5)',
         'nolegend' = 'legend.show = FALSE, colors = "red"',
         'markerbig' = 'series.marker.size = 20, grid.show = FALSE',
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

