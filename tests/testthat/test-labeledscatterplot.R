context("Labeled scatter plot")

dat <- structure(c(-0.378331991098098, -0.81165552109611, 0.217951487400431,
1.5286107300616, -1.8772553732612, -0.427440736452652, 1.81260348695547,
0.0210023953600193, 0.00559155278839169, 0.784822531228735, -1.46479949238719,
-1.68259776871402, -1.51650839628927, 0.955458580527818, -1.34834878068353,
0.876829966842232, -0.114809313591938, -0.293554423897848, -0.789860399916284,
-0.609446296323656, -0.141567243052079, -0.453809127391586, 0.39578896667589,
-0.37481125870847), .Dim = c(12L, 2L), .Dimnames = list(c("nLAGph",
"7hVoOM", "iBnrwa", "o3Dw2c", "uMZoXB", "C1IrLI", "G3ueGs", "TveROQ",
"QCBe1q", "z0bGc5", "zsakBj", "z60tz8"), c("X", "Y")))
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

test_that("Labeled Scatter accepts unused arguments",
{
    x <- structure(1:10, .Names = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"))
    expect_error(Scatter(x, scatter.labels.as.hovertext = F, fit.type = "None"), NA)
})

test_that("Warning is given for numeric color with qualitative palette",
{
    expect_warning(Scatter(1:10, 1:10, scatter.colors = 1:10, scatter.colors.as.categorical = FALSE),
                   "For a numeric 'colors' variable, a qualitative palette should not be used")
})
