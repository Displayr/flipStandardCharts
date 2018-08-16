context("Date axis")

date0 <- structure(17759, class = "Date")

for (n in c(5, 10, 11, 15))
{
    xx <- 1:n
    for (step in c(1, 8, 25, 60, 300))
    {
        filestem <- sprintf("dateaxis-line-n%d-s%d", n, step)
        names(xx) <- date0 + (1:length(xx) * step)
        expect_error(pp <- Line(xx), NA)
        #print(pp)
        #readline(prompt=paste0(filestem, ": press [enter] to continue: "))

        filestem <- sprintf("dateaxis-column-n%d-s%d", n, step)
        names(xx) <- date0 + (1:length(xx) * step)
        expect_error(pp <- Column(xx), NA)
        #print(pp)
        #readline(prompt=paste0(filestem, ": press [enter] to continue: "))
    }
}
