context("Sparkline")

# Set up various data types to test
set.seed(1234)
unnamed <- rnorm(20) # includes negative numbers
numeric <- structure(c(1,3,2,5,4,6), .Names = c('1','3','2','5','4','6'))
categorical <- structure(1:5, .Names = c("Zebra", "Giraffe", "Ant", "Lizard", "Beetle"))
dates <- structure(1:20, .Names = sprintf("2018-11-%d", 1:20))
multcols <- matrix(rnorm(30), 10, 3, dimnames = list(letters[1:10], LETTERS[1:3]))

dat.list <- c("unnamed", "numeric", "categorical", "dates", "multcols")
type.list <- c("Area", "Line", "Curve", "Column", "Box")
opts = c('default' = '',
         'showaxes' = 'x.axis.show = TRUE, y.axis.show = TRUE')

for (dat in dat.list)
{
    for (type in type.list)
    {
        for (ii in 1:length(opts))
        {
            filestem <- paste("sparkline", type, dat, names(opts)[ii], sep = "-")
            test_that(filestem,
            {
                cmd <- paste0("pp <- Sparkline(", dat, ", type = '", type, "', ", opts[ii], ")")
                expect_error(suppressWarnings(eval(parse(text = cmd))), NA)

                #print(pp)
                #readline(prompt=paste0(filestem, ": press [enter] to continue: "))
            })

        }

    }
}

test_that("sparkline-box",
{
    set.seed(1223)
    Normals <- c(runif(38, 10, 20), rnorm(100,-5,2), rnorm(60, 5, 2), 35, 30)
    Normals = list(A = Normals, "Big fat dog jumped over the lazy bigger water buffalo" = Normals + 3, "Cat" = Normals + 10)
    expect_warning(Sparkline(Normals, "Box"), "Sparkline charts only show a single series")
})
