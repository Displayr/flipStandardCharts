context("Vector data")

# Set up various data types to test
# Names are used directory in filenames so keep punctuation use minimal
# Here we only use 1d vectors (see test-matrixdata.R for 2d data)
# They are kept separate because 2d data is applicable to more chart types
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
tabdata <- structure(c(12.375, 11.75, 10.375, 11.375, 11.625, 7.875, 11.875,
15.75, 7), class = "table", .Dim = 9L, .Dimnames = structure(list(
    Age = c("18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44",
    "45 to 49", "50 to 54", "55 to 64", "65 or more")), .Names = "Age"))

# Create lists of all charting functions, data types and options to use
# Relevant tests for radar charts in test-radar.R
charting.funcs <- c("Column", "Bar", "Area", "Line", "Scatter", "LabeledScatter")
dat.list <- c("unnamed", "named", "missing1", "missing13",
               "gapped", "dated", "gapdates", "single", "double", "tabdata")
opts <- c('default' = '',
          'datalabel' = 'data.label.show=TRUE',
          'linearfit' = 'fit.type="Linear", fit.ignore.last=T, fit.line.color="red"',
          'smoothfit' = 'fit.type="Smooth", fit.line.type="dashdot", fit.line.width=3')

# Iterate through all combinations of charting functions, data type and options
for (func in charting.funcs)
{
    for (dat in dat.list)
    {
        for (ii in 1:length(opts))
        {
            # Create name which will appear in the error message if test fails
            filestem <- paste0(tolower(func), "-", dat, "-", names(opts)[ii])

            test_that(filestem, {

                # Create command that will create widget
                cmd <- paste0("pp <- ", func, "(", dat, ",", opts[ii], ")")

                # Run command to create the widget and compare expectation
                if (grepl("labeledscatter-.*(datalabel|fit)", filestem))
                    expect_error(suppressWarnings(eval(parse(text=cmd))), NA)
                else if (grepl("missing", filestem))
                    expect_warning(eval(parse(text=cmd)))
                else if (grepl("single-.*fit", filestem))
                    expect_warning(eval(parse(text=cmd)))
                else if (grepl("double-linearfit", filestem))
                    expect_warning(eval(parse(text=cmd)))
                else
                {
                    expect_error(eval(parse(text=cmd)), NA)
                    #print(pp)
                    #readline(prompt=paste0(filestem, ": press [enter] to continue: "))
                }
            })
        }
    }
}

