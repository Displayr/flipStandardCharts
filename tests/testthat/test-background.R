context("Chart backgrounds")
set.seed(3363629)

# Here we are checking the behaviour of the plotly chart background and axis
# Only checking charting functions supporting most of plotly chart axes options
# Corresponding tests for Radar and Scatter plots are in their own file
funcs <- c("Column", "Bar", "Area", "Line", "Scatter")

test.args <- c('default' = '',
    'datalabelonly' = 'data.label.show=TRUE, y.tick.show=FALSE',
    'zeros' = 'x.zero=TRUE, y.zero=TRUE, x.zero.line.color="red", y.zero.line.color="blue"',
    'zerolabeled' = 'x.zero=TRUE, y.zero=TRUE, x.zero.line.color="red", y.zero.line.color="blue", data.label.show=TRUE',
    'zerorev' = 'x.zero=TRUE, y.zero=TRUE, x.zero.line.color="red", y.zero.line.color="blue", x.data.reversed=TRUE, y.data.reversed=TRUE',
    'zerorevlabeled' = 'x.zero=TRUE, y.zero=TRUE, x.zero.line.color="red", y.zero.line.color="blue", data.label.show=T, x.data.reversed=TRUE, y.data.reversed=TRUE',
    'zerorevfit' = 'x.zero=TRUE, y.zero=TRUE, x.zero.line.color="red", y.zero.line.color="blue", fit.type = "Smooth", x.data.reversed=TRUE, y.data.reversed=TRUE',
    'fitlabeled' = 'fit.type = "Smooth", data.label.show = TRUE',
    'backgroundcolors' = 'background.fill.color="grey", charting.area.fill.color="yellow", charting.area.fill.opacity=0.2',
    'grid' = 'x.line.width=2, y.line.width=4, y.line.color="red", x.line.color="blue", y.tick.mark.length=10, x.tick.mark.length=1, x.grid.width=4, y.grid.width=1',
    'nogrid' = 'grid.show=FALSE, x.grid.width=1, y.grid.width=1',
    'tickdist' = 'y.bounds.minimum=3, y.bounds.maximum=20, y.tick.distance=1',
    'ticklabels' = 'y.tick.prefix="<", y.tick.suffix=">"',
    'reversed' = 'x.data.reversed=TRUE, y.data.reversed=TRUE',
    'legendpos' = 'legend.position.y=0.5, legend.position.x=0, legend.font.color="red", legend.fill.opacity=0',
    'legendbg' = 'legend.fill.color="blue", legend.fill.opacity=0.5, legend.border.color="red", legend.border.line.width=2',
    'margins' = 'margin.left=0, margin.right=0, margin.top=0, margin.inner.pad=10, margin.bottom=0, background.fill.color="blue", charting.area.fill.color="red", legend.show=FALSE, grid.show=FALSE',
    'font' = 'global.font.family="Courier", global.font.color="red"',
    'modebar' = 'modebar.show = TRUE')

# data set for each axis type
positives <- matrix(c(1:20), 10, 2, dimnames=list(1:10 + 5, c("X", "Y")))
negatives <- matrix(c(1:20), 10, 2, dimnames=list(1:10 - 20, c("X", "Y")))
categoricals <- matrix(c(1:20), 10, 2, dimnames=list(letters[1:10], c("X", "Y")))
dates <- matrix(c(1:20), 10, 2,
                dimnames=list(sprintf("%02d/01/2017", c(1:5, 16:20)), c("X", "Y")))

# corresponding data sets for scatterplot
sc.positives <- 1:10 + 5
sc.negatives <- 1:10 - 20
sc.categoricals <- data.frame(Class=factor(sample(LETTERS[1:3], 10, replace = TRUE)), Y=1:10)
sc.dates <- data.frame(Date=as.Date("2017-01-01") + c(0:4, 15:19), Y=1:10)

dat.list <- c("positives", "negatives", "categoricals", "dates")
for (ff in funcs)
{
    for (dat in dat.list)
    {
        #for (i in 1:length(test.args))
        for (i in 6)
        {
            # filestem is both the name of the image in accepted-snapshots
            # and the error msg expected on the output of devtools::test()
            filestem <- paste("background", tolower(ff), dat,
                              names(test.args)[i], sep="-")

            if (grepl("bar-.*-ticklabel", filestem))
                next
            test_that(filestem, {

                if (ff == "Scatter")
                    dat <- paste0("sc.", dat)
                cmd <- paste0("pp <- ", ff, "(", dat, ",", test.args[i], ")")
                expect_error(suppressWarnings(eval(parse(text=cmd))), NA)

                #expect_true(TestWidget(pp, filestem))
                print(pp)
                readline(prompt=paste0(filestem, ": press [enter] to continue: "))
            })
        }
    }
}
