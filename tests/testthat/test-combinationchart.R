context("Combination chart")

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

x2.numeric <- structure(rnorm(10), .Names = (1:10) + 4)
x2.named <- structure((1:10)/100, .Names = letters[c(1:9, 26)])

test_that("Axis types",
{
    expect_error(Column(unnamed, x2 = x2.named), "do not have the same type")
    expect_error(Column(unnamed, x2 = x2.numeric, type = "Stacked", opacity = 0.7), NA)
    expect_error(Column(named, x2 = x2.named, y2.tick.format = "%", legend.orientation = "horizontal"), NA)
    expect_error(Column(dated, x2 = gapdated, opacity = 0.3, colors2 = c("red", "green", "blue")), NA)
})


