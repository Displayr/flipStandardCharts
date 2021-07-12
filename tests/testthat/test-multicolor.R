context("Multicolor charts")
set.seed(3363629)

#funcs <- c("Pyramid", "BarMultiColor", "ColumnMultiColor")
funcs <- c("ColumnMultiColor")

test.args <- c('default' = '',
    'datalabelonly' = 'data.label.show=TRUE, y.tick.show=FALSE, colors=c("red","green","blue")',
    'zeros' = 'x.zero=TRUE, y.zero=TRUE, x.zero.line.color="red", y.zero.line.color="blue"',
    'zerolabeled' = 'x.zero=TRUE, y.zero=TRUE, x.zero.line.color="red", y.zero.line.color="blue", data.label.show=TRUE',
    'zerorev' = 'x.zero=TRUE, y.zero=TRUE, x.zero.line.color="red", y.zero.line.color="blue", x.data.reversed=TRUE, y.data.reversed=TRUE',
    'zerorevlabeled' = 'x.zero=TRUE, y.zero=TRUE, x.zero.line.color="red", y.zero.line.color="blue", data.label.show=T, x.data.reversed=TRUE, y.data.reversed=TRUE',
    #'zerorevfit' = 'x.zero=TRUE, y.zero=TRUE, x.zero.line.color="red", y.zero.line.color="blue", fit.type = "Smooth", x.data.reversed=TRUE, y.data.reversed=TRUE',
    #'fitlabeled' = 'fit.type = "Smooth", data.label.show = TRUE',
    'backgroundcolors' = 'background.fill.color="grey", charting.area.fill.color="yellow", charting.area.fill.opacity=0.2',
    'grid' = 'x.line.width=2, y.line.width=4, y.line.color="red", x.line.color="blue", y.tick.mark.length=10, x.tick.mark.length=1, x.grid.width=4, y.grid.width=1',
    'nogrid' = 'grid.show=FALSE, x.grid.width=1, y.grid.width=1',
    'tickdist' = 'y.bounds.minimum=3, y.bounds.maximum=20, y.tick.distance=1',
    'ticklabels' = 'y.tick.prefix="<", y.tick.suffix=">"',
    'reversed' = 'x.data.reversed=TRUE, y.data.reversed=TRUE',
    #'legendpos' = 'legend.position.y=0.5, legend.position.x=0, legend.font.color="red", legend.fill.opacity=0',
    #'legendbg' = 'legend.fill.color="blue", legend.fill.opacity=0.5, legend.border.color="red", legend.border.line.width=2',
    'margins' = 'margin.left=0, margin.right=0, margin.top=0, margin.inner.pad=10, margin.bottom=0, background.fill.color="blue", charting.area.fill.color="red", grid.show=FALSE',
    'font' = 'global.font.family="Courier", global.font.color="red"',
    'modebar' = 'modebar.show = TRUE')

# data set for each axis typeset.seed(1234)
unnamed <- abs(rnorm(10))
named <- structure(rnorm(10), .Names=letters[1:10])
manyvals <- structure(rnorm(25), .Names=letters[1:25])
missing1 <- named
missing1[1] <- NA
missing13 <- missing1
missing13[3] <- NA
gapped <- structure(abs(rnorm(10)), .Names=c(11:15, 26:30))
dated <- structure(rnorm(10), .Names=sprintf("2017-01-%s", 1:10))
gapdates <- structure(rnorm(10), .Names=sprintf("2017-01-%s", c(11:15, 26:30)))
single <- c(5)
double <- c(5, 1)
tabdata <- structure(c(12.375, 11.75, 10.375, 11.375, 11.625, 7.875, 11.875,
15.75, 7), class = "table", .Dim = 9L, .Dimnames = structure(list(
    Age = c("18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44",
    "45 to 49", "50 to 54", "55 to 64", "65 or more")), .Names = "Age"))

# corresponding data sets for scatterplot
sc.positives <- 1:10 + 5
sc.negatives <- 1:10 - 20
sc.categoricals <- data.frame(Class=factor(sample(LETTERS[1:3], 10, replace = TRUE)), Y=1:10)
sc.dates <- data.frame(Date=as.Date("2017-01-01") + c(0:4, 15:19), Y=1:10)

#dat.list <- c("positives", "negatives", "categoricals", "dates")
dat.list <- c("unnamed", "named", "dated")
for (ff in funcs)
{
    for (dat in dat.list)
    {
        for (i in 1:length(test.args))
        {
            # filestem is both the name of the image in accepted-snapshots
            # and the error msg expected on the output of devtools::test()
            filestem <- paste("multicolor", tolower(ff), dat,
                              names(test.args)[i], sep="-")

            if (grepl("bar-.*-ticklabel", filestem))
                next
            test_that(filestem, {

                cmd <- paste0("pp <- ", ff, "(", dat, ",", test.args[i], ")")
                expect_error(suppressWarnings(eval(parse(text=cmd))), NA)

                #expect_true(TestWidget(pp, filestem))
                #print(pp)
                #readline(prompt=paste0(filestem, ": press [enter] to continue: "))
            })
        }
    }
}

test_that("No warnings",
{
    tb <- structure(c(`18 to 24` = 12.1877350324422, `25 to 29` = 11.630627444206,
        `30 to 34` = 9.25844425314238, `35 to 39` = 12.3248525259547,
        `40 to 44` = 11.6035118837654, `45 to 49` = 7.86797917530882,
        `50 to 54` = 12.2980666219383, `55 to 64` = 15.8381229204451,
        `65+` = 6.99066014279715), .Dim = 9L, .Dimnames = list(
        c("18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44",
        "45 to 49", "50 to 54", "55 to 64", "65+")),
        statistic = "%", name = "D1 - Age", questions = c("D1 - Age",
        "SUMMARY"), weight.name = "weight_", weight.label = "weighting")
    expect_error(ColumnMultiColor(tb, data.label.show = TRUE, data.label.format = ".0f"), NA)
    expect_error(BarMultiColor(tb, data.label.show = TRUE, data.label.format = ".0f"), NA)
})

test_that("ColumnMultiColor + missing endpoints",
{
    xx <- c(A = 1, B = 2, C = NA)
    expect_warning(ColumnMultiColor(xx, data.label.show = TRUE))
})

