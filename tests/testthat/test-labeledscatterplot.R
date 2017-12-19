context("Labeled scatter plot")

set.seed(12358)
dat <- matrix(rnorm(24), 12, 2,
              dimnames=list(stringi::stri_rand_strings(12, 6), c("X", "Y")))
zgroup <- rep(LETTERS[1:3], 4)
logos <- sprintf("https://displayrcors.azureedge.net/images/%s_grey.svg",
                 c("apple", "soup", "bread", "car", "chicken", "rocket",
                   "sickperson", "thumbsup", "elephant", "tree", "weight", "tools"))

test_that("Max labels", {
    expect_warning(pp <- LabeledScatter(dat, scatter.max.labels = 5), "Some labels have been hidden")
})

test_that("Logos",  {
    expect_error(pp <- LabeledScatter(dat, logos = logos), NA)
    expect_error(pp <- LabeledScatter(dat, logos = paste(logos, collapse=","), logo.size = 0.2), NA)
    expect_error(pp <- LabeledScatter(dat, logos = c("Typo", logos[-1]), logo.size = 0.2), NA)
    expect_error(pp <- LabeledScatter(dat, logos = logos[-1], logo.size = 0.2))
    expect_error(pp <- LabeledScatter(dat, logos = c("", logos[-1]), logo.size = 0.2))
})

test_that("Trend lines", {
    expect_error(pp <- LabeledScatter(dat, trend.line = TRUE), NA)
    expect_error(pp <- LabeledScatter(dat, scatter.colors = zgroup,
                 scatter.colors.as.categorical = T, trend.line = TRUE), NA)
    expect_error(pp <- LabeledScatter(list(dat, dat+0.5, dat+1), trend.line = TRUE), NA)
    expect_warning(pp <- LabeledScatter(list(dat, dat+0.5, dat+1), trend.line = FALSE), "Tables have been automatically assigned names")
    expect_error(pp <- LabeledScatter(list(dat, dat+rnorm(24)), trend.line = TRUE, logos = logos, logo.size = 0.2), NA)

    # DS-1658
    tab3 <- structure(c(1, 2, 3, 4), .Dim = c(4L, 1L), .Dimnames = list(c("Apple", "Microsoft", "Google", "Yahoo"), "Price"))
    tab4 <- structure(c(1, 2, 3, 4), .Dim = c(4L, 1L), .Dimnames = list(c("Apple","Microsoft", "Google", "Yahoo"), "Price"))
    expect_warning(LabeledScatter(list(tab3, tab4)))

    })

#z <- cbind(1:5, 1:5)
#rownames(z) <- letters[1:5]
#test_that("LabeledScatter called from Scatter", {
#    expect_error(Scatter(z, scatter.labels.as.hovertext = TRUE), NA)
#    expect_warning(Scatter(list(z, z+1, z+2)))
#    expect_error(Scatter(z, scatter.labels.as.hovertext = TRUE, logos=sprintf("https://displayrcors.azureedge.net/images/%s_grey.svg", c("apple", #"elephant", "cow", "chicken", "stickman"))), NA)
#})

