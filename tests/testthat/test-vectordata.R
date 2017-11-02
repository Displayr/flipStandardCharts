context("Vector data")

set.seed(1234)
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

funcs <- c("Column", "Bar", "Area", "Line", "Scatter", "LabeledScatter") # "Pie", "Radar")
dat.list <- c("unnamed", "named", "missing1", "missing13",
               "gapped", "dated", "gapdates", "single", "double")
opts <- c('default' = '',
          'datalabel' = 'data.label.show=TRUE',
          'linearfit' = 'fit.type="Linear", fit.ignore.last=T, fit.line.color="red"',
          'smoothfit' = 'fit.type="Smooth", fit.line.type="dashdot", fit.line.width=3')

for (ff in funcs)
{
    for (dd in dat.list)
    {
    for (ii in 1:length(opts))
    {
        filestem <- paste0(tolower(ff), "-", dd, "-", names(opts)[ii])
        if (grepl("area-single", filestem))
            next

        test_that(filestem, {
            cmd <- paste0("pp <- ", ff, "(", dd, ",", opts[ii], ")")

            if (grepl("labeledscatter-.*-(datalabel|linearfit|smoothfit)", filestem))
                expect_error(eval(parse(text=cmd)))
            else if (grepl("area-single", filestem))
                expect_error(eval(parse(text=cmd)))
            else if (grepl("missing", filestem))
                expect_warning(eval(parse(text=cmd)))
            else if (grepl("single-.*fit", filestem))
                expect_warning(eval(parse(text=cmd)))
            else if (grepl("double-linearfit", filestem))
                expect_warning(eval(parse(text=cmd)))
            else
                expect_error(eval(parse(text=cmd)), NA)

            #Useful for interactive viewing
            #print(pp)
            #readline(prompt=paste0(filestem, ": press [enter] to continue: "))

        })
    }
    }
}

