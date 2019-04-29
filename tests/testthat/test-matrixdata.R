context("Matrix data")

# Data sets to check
# These tests are based on test-stacked.R
# But include more charting options not available for stacked charts
# e.g. negative values and lines of best fit
set.seed(654321)
unnamed <- matrix(rpois(60, 4), 20, 3) # all positives
named <- matrix(unnamed, 20, 3, dimnames = list(letters[1:20], LETTERS[1:3]))
signed <- sweep(named, 1, rnorm(2), "*")
signed2 <- matrix(rnorm(30), 10, 3)
gapped <- matrix(unnamed, 20, 3, dimnames = list(c(1:10, 21:30), LETTERS[1:3]))
missing1 <- gapped
missing1[1,1] <- NA
rownames(missing1) <- 25:44
missing124 <- missing1
missing124[c(1,2,4),1] <- NA
dated <- gapped
rownames(dated) <- sprintf("%02d/01/2017", 1:20)
gapdated <- dated
rownames(gapdated) <- sprintf("%02d/01/2017", c(1:10, 21:30))

# Set up combinations to iterate tests through
charting.funcs <- c("Bar", "Column", "Area", "Line")
dat.list <- c("unnamed", "named", "signed2", "gapped", "missing1", "missing124",
              "dated", "gapdated")
opts <- c('default' = '',
          'datalabels' = 'data.label.show = TRUE, data.label.format = ".0f"',
          'linearfit' = 'fit.type = "Linear", fit.ignore.last = TRUE',
          'reversefit' = 'fit.type = "Smooth", fit.ignore.last = TRUE, x.data.reversed = TRUE, y.data.reversed = TRUE, legend.show = "Show"',
          'reversed' = 'x.data.reversed = TRUE, y.data.reversed = TRUE, data.label.show = TRUE, data.label.format = ".0f"')

# data axis of stacked area chart gets chopped off
for (func in charting.funcs)
{
    for (dat in dat.list)
    {
        for (ii in 1:length(opts))
        {
            filestem <- paste("matrixdata", tolower(func), dat, names(opts)[ii], sep="-")
            test_that(filestem, {

                cmd <- paste0("pp <- ", func, "(", dat, "," , opts[ii], ")")

                if (grepl("missing", filestem))
                    expect_warning(eval(parse(text=cmd)))
                else
                    expect_error(eval(parse(text=cmd)), NA)

                #print(pp)
                #readline(prompt=paste0(filestem, ": press [enter] to continue: "))
            })
        }
    }
}
