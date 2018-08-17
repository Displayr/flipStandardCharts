context("Time axis")

time0 <- structure(1534379835.05508, class = c("POSIXct", "POSIXt"))

for (n in c(5, 10, 11, 15))
{
    xx <- cbind(A=1:n, B=(1:n)+1, C=(1:n)+2)
    for (step in c(1, 8, 25, 1e3, 1e4, 1e5, 1e6, 1e7))
    {
        rownames(xx) <- as.character(time0 + (1:n * step))

        filestem <- sprintf("timeaxis-line-n%d-s%d", n, step)
        expect_error(pp <- Line(xx, data.label.show = TRUE), NA)
        #print(pp)
        #readline(prompt=paste0(filestem, ": press [enter] to continue: "))

        filestem <- sprintf("timeaxis-column-n%d-s%d", n, step)
        expect_error(pp <- Column(xx, data.label.show = TRUE), NA)
        #print(pp)
        #readline(prompt=paste0(filestem, ": press [enter] to continue: "))
    }
}
