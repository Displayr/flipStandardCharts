context("Pie")
library(flipStandardCharts)
library(flipChartBasics)

set.seed(12345)
unnamed <- 1:10
vec1d <- structure(rpois(8, 20), .Names = c("Epsilon", "Beta", "Something longer that may need wrapping", "Delta ($)", "Alpha", "Many but still very short words", "Zeta", "Eta"))
missingvals <- c(A = 1, B = 2, C = NA, D = NA, E = 0, F = 3)
dat2 <- matrix(1:12, 3, 4)

# We skip test on as.percentages, as that will be moved to PrepareData
# values.order is hooked up, but unclear what its supposed to do
opts <- c('title' = 'title = "Pie chart", global.font.family = "Courier", title.font.family = "Arial Black", title.font.color = "red"',
          'border' = 'pie.border.color = rgb(1,0,0,0.5), colors = ChartColors(10, "Reds")',
          'radius' = 'pie.inner.radius = 95, pie.values.order = "alphabetical"',
          'labels' = 'data.label.prefix = "<", data.label.suffix = ">", data.label.decimals = 0, data.label.font.family = "Courier", pie.groups.font.family = "Arial Black", pie.groups.font.color = "red", pie.values.order = "descending", pie.groups.order = "descending"',
          'threshold' = 'pie.data.threshold = 0.1, data.label.font.color = "red"',
          'repeatcolor' = 'pie.subslice.colors.repeat = TRUE, colors = ChartColors(10, "Greys"), pie.subslice.colors = ChartColors(3, "Greens")',
          'norepeatcolor' = 'pie.subslice.colors.repeat = FALSE, colors = ChartColors(10, "Greys"), pie.subslice.colors = ChartColors(20, "Greens")')

dat.list <- c("dat2", "vec1d", "unnamed", "missingvals")
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




