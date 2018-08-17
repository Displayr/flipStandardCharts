context("Date axis")

date0 <- structure(17759, class = "Date")

for (n in c(5, 10, 11, 15))
{
    xx <- cbind(A=1:n, B=(1:n)+1, C=(1:n)+2)
    for (step in c(1:5, 8, 25, 60, 300))
    {
        rownames(xx) <- as.character(date0 + (1:n * step))

        filestem <- sprintf("dateaxis-line-n%d-s%d", n, step)
        expect_error(pp <- Line(xx, data.label.show = TRUE), NA)
        #print(pp)
        #readline(prompt=paste0(filestem, ": press [enter] to continue: "))

        filestem <- sprintf("dateaxis-column-n%d-s%d", n, step)
        expect_error(pp <- Column(xx, data.label.show = TRUE), NA)
        #print(pp)
        #readline(prompt=paste0(filestem, ": press [enter] to continue: "))
    }
}
