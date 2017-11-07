context("Stacked Charts")

# data sets to check
set.seed(122333)
unnamed <- matrix(rpois(60, 4), 20, 3) # all positives
named <- matrix(unnamed, 20, 3, dimnames = list(letters[1:20], LETTERS[1:3]))
gapped <- matrix(unnamed, 20, 3, dimnames = list(c(1:10, 21:30), LETTERS[1:3]))
missing1 <- gapped
missing1[1,1] <- NA
rownames(missing1) <- 25:44
missing13 <- missing1
missing13[c(1,3),1] <- NA
dated <- gapped
rownames(dated) <- sprintf("%02d/01/2017", 1:20)
gapdated <- dated
rownames(gapdated) <- sprintf("%02d/01/2017", c(1:10, 21:30))
dat.list <- c("unnamed", "named", "gapped", "missing1", "missing13", "dated", "gapdated")

opts <- c('default' = '',
          'datalabels' = 'data.label.show = TRUE',
          'reversed' = 'x.data.reversed = TRUE, y.data.reversed = TRUE, data.label.show = TRUE')

# data axis of stacked area chart gets chopped off
for (func in c("Area", "Bar", "Column"))
{
    for (dat in dat.list)
    {
        for (ii in 1:length(opts))
        {
            filestem <- paste("stacked", tolower(func), dat, names(opts)[ii], sep="-")
            cmd <- paste0("pp <- ", func, "(", dat, ", type = \"Stacked\", ", opts[ii], ")")

            if (grepl("missing", filestem))
            {
                expect_error(eval(parse(text=cmd)))
                next
            }
            else
                expect_error(eval(parse(text=cmd)), NA)

            print(pp)
            readline(prompt=paste0(filestem, ": press [enter] to continue: "))
        }
    }
}
