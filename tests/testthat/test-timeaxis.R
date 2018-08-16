context("Time axis")

time0 <- structure(1534379835.05508, class = c("POSIXct", "POSIXt"))

for (n in c(5, 10, 11, 15))
{
    xx <- 1:n
    for (step in c(1, 8, 25, 60, 300))
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
