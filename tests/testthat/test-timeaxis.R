context("Time axis")

# Since plotly v 4.8.0, if the axis range
# is around a few seconds, range need to be specified by a date STRING
# but most of the time, it is more efficient to store it as a
# the number of milliseconds since 1970.

# These tests try to cover a range of time-scales
# and all the if-clauses in getDateAxisRange

time0 <- structure(1534379835.05508, class = c("POSIXct", "POSIXt"))

for (n in c(5, 10, 11, 15))
{
    xx <- 1:n
    for (step in c(1, 8, 25, 1e3, 1e4, 1e5, 1e6, 1e7))
    {
        filestem <- sprintf("timeaxis-line-n%d-s%d", n, step)
        names(xx) <- time0 + (1:length(xx) * step)
        expect_error(pp <- Line(xx), NA)
        #print(pp)
        #readline(prompt=paste0(filestem, ": press [enter] to continue: "))

        filestem <- sprintf("timeaxis-column-n%d-s%d", n, step)
        names(xx) <- time0 + (1:length(xx) * step)
        expect_error(pp <- Column(xx), NA)
        #print(pp)
        #readline(prompt=paste0(filestem, ": press [enter] to continue: "))
    }
}
