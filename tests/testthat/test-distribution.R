context("Distribution")

TrialOpens = structure(c("UK/Europe", "Asia", "US/Canada", "UK/Europe", "UK/Europe",
"US/Canada", "Australia/NZ", "US/Canada", "Australia/NZ", "Australia/NZ",
"US/Canada", "US/Canada", "US/Canada", "Asia", "UK/Europe", "Australia/NZ",
"Australia/NZ", "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe",
"Asia", "UK/Europe", "US/Canada", "Australia/NZ", "Australia/NZ",
"US/Canada", "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe",
"US/Canada", "Asia", "US/Canada", "US/Canada", "UK/Europe", "US/Canada",
"UK/Europe", "UK/Europe", "Africa/ME", "US/Canada", "US/Canada",
"US/Canada", "US/Canada", "US/Canada", "US/Canada", "US/Canada",
"US/Canada", "US/Canada", "US/Canada", "US/Canada", "US/Canada",
"US/Canada", "US/Canada", "US/Canada", "US/Canada", "US/Canada",
"UK/Europe", "Australia/NZ", "UK/Europe", "US/Canada", "US/Canada",
"US/Canada", "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe",
"UK/Europe", "UK/Europe", "US/Canada", "Australia/NZ", "US/Canada",
"US/Canada", "UK/Europe", "UK/Europe", "UK/Europe", "US/Canada",
"US/Canada", "Asia", "US/Canada", "UK/Europe", "UK/Europe", "US/Canada",
"Australia/NZ", "UK/Europe", "US/Canada", "US/Canada", "Australia/NZ",
"Africa/ME", "Australia/NZ", "Australia/NZ", "Australia/NZ",
"US/Canada", "Australia/NZ", "UK/Europe", "US/Canada", "US/Canada",
"Australia/NZ", "US/Canada", "US/Canada", "US/Canada", "US/Canada",
"Australia/NZ", "UK/Europe", "Australia/NZ", "UK/Europe", "UK/Europe",
"UK/Europe", "UK/Europe", "US/Canada", "US/Canada", "US/Canada",
"Asia", "Africa/ME", "Africa/ME", "UK/Europe", "US/Canada", "US/Canada",
"Australia/NZ", "US/Canada", "Australia/NZ", "US/Canada", "UK/Europe",
"UK/Europe", "UK/Europe", "US/Canada", "US/Canada", "Missing data",
"US/Canada", "US/Canada", "Australia/NZ", "Australia/NZ", "UK/Europe",
"Asia", "Asia", "Asia", "Asia", "Australia/NZ", "UK/Europe",
"US/Canada", "US/Canada", "Australia/NZ", "Australia/NZ", "Asia",
"Africa/ME", "US/Canada", "US/Canada", "Asia", "UK/Europe", "UK/Europe",
"UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe",
"Africa/ME", "US/Canada", "US/Canada", "US/Canada", "US/Canada",
"US/Canada", "US/Canada", "US/Canada", "UK/Europe", "Asia", "UK/Europe",
"UK/Europe", "UK/Europe", "Australia/NZ", "US/Canada", "US/Canada",
"US/Canada", "US/Canada", "US/Canada", "UK/Europe", "US/Canada",
"US/Canada", "US/Canada", "US/Canada", "US/Canada", "UK/Europe",
"UK/Europe", "UK/Europe", "US/Canada", "Asia", "Asia", "Asia",
"Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia",
"Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia",
"Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia",
"Africa/ME", "US/Canada", "US/Canada", "US/Canada", "Australia/NZ",
"UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe",
"UK/Europe", "UK/Europe", "US/Canada", "UK/Europe", "UK/Europe",
"UK/Europe", "UK/Europe", "Australia/NZ", "UK/Europe", "Asia",
"US/Canada", "UK/Europe", "US/Canada", "US/Canada", "US/Canada",
"UK/Europe", "US/Canada", "US/Canada", "UK/Europe", "Australia/NZ",
"Asia", "UK/Europe", "US/Canada", "UK/Europe", "UK/Europe", "UK/Europe",
"UK/Europe", "US/Canada", "US/Canada", "US/Canada", "UK/Europe",
"US/Canada", "US/Canada", "UK/Europe", "US/Canada", "US/Canada",
"US/Canada", "UK/Europe", "US/Canada", "US/Canada", "Australia/NZ",
"Asia", "US/Canada", "US/Canada", "US/Canada", "UK/Europe", "Asia",
"Asia", "US/Canada", "Australia/NZ", "UK/Europe", "Australia/NZ",
"Asia", "Australia/NZ", "US/Canada", "Asia", "Asia", "US/Canada",
"US/Canada", "Asia", "US/Canada", "UK/Europe", "UK/Europe", "US/Canada",
"Australia/NZ", "Australia/NZ", "Asia", "UK/Europe", "US/Canada",
"US/Canada", "Australia/NZ", "Australia/NZ", "UK/Europe", "UK/Europe",
"UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe",
"UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe",
"UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe", "Asia", "Australia/NZ",
"UK/Europe", "US/Canada", "US/Canada", "US/Canada", "Australia/NZ",
"UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe", "Australia/NZ",
"Australia/NZ", "Australia/NZ", "US/Canada", "US/Canada", "US/Canada",
"US/Canada", "US/Canada", "Australia/NZ", "Australia/NZ", "US/Canada",
"Asia", "Australia/NZ", "UK/Europe", "US/Canada", "UK/Europe",
"Australia/NZ", "Australia/NZ", "Asia", "Asia", "Asia", "Asia",
"UK/Europe", "Australia/NZ", "Australia/NZ", "UK/Europe", "US/Canada",
"US/Canada", "US/Canada", "US/Canada", "UK/Europe", "US/Canada",
"US/Canada", "UK/Europe", "US/Canada", "Africa/ME", "Africa/ME",
"UK/Europe", "UK/Europe", "UK/Europe", "US/Canada", "US/Canada",
"US/Canada", "US/Canada", "US/Canada", "UK/Europe", "US/Canada",
"US/Canada", "US/Canada", "US/Canada", "Australia/NZ", "UK/Europe",
"US/Canada", "US/Canada", "US/Canada", "Australia/NZ", "US/Canada",
"US/Canada", "US/Canada", "UK/Europe", "UK/Europe", "US/Canada",
"US/Canada", "UK/Europe", "Australia/NZ", "US/Canada", "US/Canada",
"Australia/NZ", "Australia/NZ", "Australia/NZ", "Australia/NZ",
"UK/Europe", "UK/Europe", "US/Canada", "UK/Europe", "US/Canada",
"US/Canada", "US/Canada", "US/Canada", "US/Canada", "US/Canada",
"US/Canada", "UK/Europe", "Asia", "Australia/NZ", "UK/Europe",
"Missing data", "US/Canada", "Asia", "Australia/NZ", "Australia/NZ",
"UK/Europe", "Asia", "Missing data", "Australia/NZ", "Asia",
"Australia/NZ", "US/Canada", "US/Canada", "Australia/NZ", "Asia",
"Asia", "UK/Europe", "Asia", "US/Canada", "UK/Europe", "Australia/NZ",
"Asia", "UK/Europe", "UK/Europe", "Australia/NZ", "Australia/NZ",
"Australia/NZ", "Asia", "Africa/ME", "Missing data", "US/Canada",
"UK/Europe", "Missing data", "Africa/ME", "UK/Europe", "Africa/ME",
"UK/Europe", "Africa/ME", "US/Canada", "Australia/NZ", "UK/Europe",
"UK/Europe", "Asia", "US/Canada", "UK/Europe", "UK/Europe", "Australia/NZ",
"UK/Europe", "UK/Europe", "Asia", "Asia", "Australia/NZ", "US/Canada",
"US/Canada", "Australia/NZ", "US/Canada", "Australia/NZ", "US/Canada",
"UK/Europe", "Australia/NZ", "US/Canada", "Australia/NZ", "US/Canada",
"US/Canada", "Asia", "US/Canada", "US/Canada", "Missing data",
"UK/Europe", "US/Canada", "Australia/NZ", "Australia/NZ", "Asia",
"UK/Europe", "Australia/NZ", "UK/Europe", "UK/Europe", "US/Canada",
"US/Canada", "Australia/NZ", "UK/Europe", "Asia", "US/Canada",
"US/Canada", "UK/Europe", "Australia/NZ", "Asia", "US/Canada",
"Australia/NZ", "Africa/ME", "US/Canada", "Asia", "Missing data",
"US/Canada", "Australia/NZ", "Asia", "UK/Europe", "Australia/NZ",
"US/Canada", "UK/Europe", "UK/Europe", "UK/Europe", "Asia", "Asia",
"Africa/ME", "US/Canada", "Australia/NZ", "US/Canada", "Asia",
"Asia", "Africa/ME", "Africa/ME", "Australia/NZ", "Asia", "Asia",
"US/Canada", "UK/Europe", "Australia/NZ", "UK/Europe", "Asia",
"Australia/NZ", "Asia", "Missing data", "US/Canada", "US/Canada",
"US/Canada", "Australia/NZ", "Australia/NZ", "US/Canada", "Asia",
"Australia/NZ", "Missing data", "US/Canada", "Australia/NZ",
"Australia/NZ", "US/Canada", "UK/Europe", "Asia", "US/Canada",
"Africa/ME", "Africa/ME", "Australia/NZ", "UK/Europe", "US/Canada",
"US/Canada", "Australia/NZ", "Australia/NZ", "UK/Europe", "UK/Europe",
"US/Canada", "Asia", "Asia", "UK/Europe", "UK/Europe", "UK/Europe",
"UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe",
"US/Canada", "US/Canada", "US/Canada", "US/Canada", "US/Canada",
"Australia/NZ", "US/Canada", "UK/Europe", "US/Canada", "US/Canada",
"UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe",
"Australia/NZ", "UK/Europe", "UK/Europe", "UK/Europe", "Asia",
"Asia", "Asia", "Asia", "Australia/NZ", "US/Canada", "US/Canada",
"US/Canada", "US/Canada", "Australia/NZ", "US/Canada", "US/Canada",
"Asia", "UK/Europe", "US/Canada", "US/Canada", "UK/Europe", "US/Canada",
"UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe", "UK/Europe",
"US/Canada", "US/Canada", "US/Canada", "Australia/NZ", "Australia/NZ",
"US/Canada", "UK/Europe", "Australia/NZ", "Australia/NZ", "UK/Europe",
"UK/Europe", "UK/Europe", "US/Canada", "Africa/ME", "Africa/ME",
"UK/Europe", "6", "24", "5", "0", "8", "40", "12", "0", "10",
"1", "2", "6", "80", "10", "0", "0", "8", "2", "7", "1", "7",
"0", "14", "0", "1", "10", "31", "1", "9", "1", "13", "3", "12",
"1", "0", "77", "25", "25", "13", "0", "0", "1", "13", "26",
"13", "13", "3", "0", "11", "13", "18", "5", "19", "2", "16",
"5", "0", "3", "0", "1", "21", "2", "8", "9", "0", "20", "57",
"0", "0", "2", "28", "1", "2", "0", "34", "0", "0", "0", "3",
"124", "14", "4", "3", "27", "0", "3", "4", "36", "0", "1", "7",
"16", "0", "0", "2", "0", "2", "0", "3", "0", "0", "30", "8",
"13", "8", "6", "25", "28", "0", "0", "1", "7", "1", "0", "32",
"0", "25", "0", "25", "1", "11", "7", "2", "0", "10", "45", "2",
"3", "9", "0", "32", "11", "3", "2", "1", "4", "4", "4", "2",
"11", "0", "20", "16", "15", "1", "3", "0", "3", "50", "0", "0",
"0", "0", "6", "0", "19", "30", "6", "0", "0", "0", "25", "0",
"25", "0", "10", "0", "4", "10", "1", "0", "0", "0", "25", "1",
"0", "3", "4", "16", "11", "5", "4", "4", "0", "258", "0", "0",
"0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0",
"0", "0", "0", "0", "0", "0", "0", "0", "1", "1", "11", "99",
"0", "5", "1", "0", "14", "5", "10", "1", "6", "0", "0", "9",
"2", "5", "4", "32", "0", "3", "3", "59", "0", "0", "0", "7",
"0", "31", "139", "2", "14", "17", "17", "16", "2", "7", "1",
"28", "1", "0", "0", "1", "1", "0", "7", "2", "0", "11", "0",
"0", "3", "0", "0", "10", "0", "14", "31", "11", "0", "12", "0",
"1", "0", "31", "2", "2", "0", "21", "1", "0", "0", "3", "0",
"17", "21", "7", "0", "4", "0", "11", "9", "0", "4", "11", "0",
"25", "0", "48", "0", "0", "12", "12", "9", "10", "9", "0", "7",
"26", "14", "7", "0", "0", "0", "0", "0", "7", "0", "0", "15",
"1", "17", "18", "28", "2", "0", "6", "4", "5", "0", "0", "4",
"1", "1", "7", "1", "24", "0", "64", "7", "0", "18", "0", "6",
"0", "0", "0", "1", "8", "7", "6", "1", "3", "2", "2", "7", "1",
"5", "1", "5", "4", "46", "1", "8", "0", "0", "11", "11", "5",
"73", "37", "0", "2", "0", "2", "10", "12", "0", "249", "0",
"10", "0", "1", "64", "4", "23", "0", "22", "12", "40", "1",
"12", "1", "4", "8", "0", "3", "2", "2", "0", "2", "6", "4",
"2", "2", "2", "5", "0", "4", "1", "0", "4", "6", "3", "0", "0",
"9", "6", "0", "76", "0", "1", "0", "1", "1", "12", "2", "10",
"5", "2", "0", "3", "0", "3", "6", "3", "1", "0", "1", "0", "0",
"1", "0", "0", "0", "0", "0", "15", "1", "2", "2", "3", "2",
"1", "17", "0", "0", "0", "0", "0", "31", "2", "5", "3", "4",
"8", "0", "9", "8", "0", "0", "1", "1", "17", "2", "22", "0",
"0", "7", "1", "10", "46", "0", "0", "1", "28", "18", "5", "20",
"0", "2", "2", "0", "0", "0", "2", "2", "10", "32", "0", "23",
"1", "0", "0", "0", "2", "1", "2", "2", "5", "0", "4", "0", "1",
"5", "1", "6", "5", "2", "1", "13", "9", "0", "0", "1", "1",
"0", "2", "1", "0", "4", "15", "1", "10", "3", "1", "0", "17",
"0", "14", "25", "13", "15", "9", "15", "2", "57", "21", "1",
"2", "0", "0", "0", "0", "0", "24", "0", "30", "1", "0", "6",
"0", "4", "37", "19", "25", "7", "1", "0", "13", "12", "7", "1",
"3", "0", "8", "13", "2", "0", "39", "8", "21", "7", "1", "4",
"21", "0", "0", "24", "13", "1", "2", "2", "0", "18", "0", "5",
"0", "3", "1", "0", "2", "0", "1", "2", "2", "4", "0", "0", "0",
"12", "9", "30", "10", "10", "2", "5", "0", "0", "1", "9", "1",
"0", "1", "3"), .Dim = c(624L, 2L), .Dimnames = list(NULL, c("Region",
"numOpens")))
TrialOpens = as.data.frame(TrialOpens)
z = TrialOpens$Region
TrialOpens = tapply(TrialOpens[[2]], TrialOpens[[1]], c)
TrialOpens <- TrialOpens[-4] # Removing missing data
group.sizes <- sapply(TrialOpens, length)
weights <- runif(sum(group.sizes))
TrialOpenWeights = tapply(weights, z[z != 'Missing data'], c)

set.seed(1223)
Normals <- c(runif(38, 10, 20), rnorm(100,-5,2), rnorm(60, 5, 2), 35, 30)
Normals = list(A = Normals, "Big fat dog jumped over the lazy bigger water buffalo" = Normals + 3, "Cat" = Normals + 10)

data(phone, package = "flipExampleData")
PhoneDF <- phone[, match("q23a", names(phone)):match("q23y", names(phone))]
Phone <- as.list(PhoneDF)


test_that("Violin plots - Different data inputs and orientations", {
    # 1 horizontal
    Distribution(Normals[1], values.title = "Number of opens", vertical = TRUE, density.type = "Density", show.mirror.density = TRUE, show.mean = TRUE, show.median = TRUE, show.range = TRUE, show.quartiles = TRUE, show.values = FALSE)
    # Multiple horizontal
    Distribution(Normals, values.title = "Number of opens", vertical = TRUE, density.type = "Density", show.mirror.density = TRUE, show.mean = TRUE, show.median = TRUE, show.range = TRUE, show.quartiles = TRUE, show.values = FALSE)
    # 1 vertical
    Distribution(Normals[1], values.title = "Number of opens", vertical = FALSE, density.type = "Density", show.mirror.density = TRUE, show.mean = TRUE, show.median = TRUE, show.range = TRUE, show.quartiles = TRUE, show.values = FALSE)
    # Multiple verticals
    Distribution(Normals, values.title = "Number of opens", vertical = FALSE, density.type = "Density", show.mirror.density = TRUE, show.mean = TRUE, show.median = TRUE, show.range = TRUE, show.quartiles = TRUE, show.values = FALSE)
    # Factor
    expect_error(suppressWarnings(Distribution(Phone[1:5])), NA)

    # Factors as Data Frame with weights - vector
    set.seed(1223)
    zz = suppressWarnings(flipTransformations::AsNumeric(PhoneDF, binary = FALSE))
    zz = zz[complete.cases(zz),]
    suppressWarnings(Distribution(zz, weights = runif(NROW(zz))))
    # Factors as Data Frame with weights - list
    set.seed(1223)
    zz = suppressWarnings(flipTransformations::AsNumeric(PhoneDF, binary = FALSE))
    zz = zz[complete.cases(zz),]
    suppressWarnings(Distribution(zz, weights = rep(list(runif(NROW(zz))), NCOL(zz))))

    Distribution(list(rnorm(100)), values.tick.format=".2f")


})


test_that("Plots used in blog post", {
    # histogram - poor with small sample sizes
    expect_error(print(Distribution(TrialOpens[1], values.title = "Number of opens", vertical = FALSE, density.type = "Histogram", show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = FALSE)), NA)
    # Figure 2
    Distribution(TrialOpens[1], maximum.bins = 20, values.title = "Number of opens", vertical = FALSE, density.type = "Histogram", show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = FALSE)
    # Figure 3
    Distribution(TrialOpens[1], maximum.bins = 100, values.title = "Number of opens", vertical = FALSE, density.type = "Histogram",show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = FALSE)
    # Figure 4
    Distribution(TrialOpens, maximum.bins = 100, values.title = "Number of opens", vertical = FALSE, density.type = "Histogram",show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = FALSE)
    # Figure 5 - box plot
    Distribution(TrialOpens, maximum.bins = 100, values.title = "Number of opens", vertical = TRUE, density.type = "Box",show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = FALSE)
    # Figure 6 - box plot with all data points
    Distribution(TrialOpens, maximum.bins = 100, values.title = "Number of opens", box.points = "All", vertical = TRUE, density.type = "Box",show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = FALSE)
    # Figure 7 - Density plots
    Distribution(TrialOpens, maximum.bins = 100, values.title = "Number of opens", vertical = FALSE, density.type = "Density", show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = FALSE)
    # Figure 8 - Density plots with rug
    Distribution(TrialOpens, maximum.bins = 100, values.title = "Number of opens", vertical = FALSE, density.type = "Density", show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = TRUE)
    # Figure 9 - Density plots box plots
    Distribution(TrialOpens, maximum.bins = 100, values.title = "Number of opens", vertical = FALSE, density.type = "Density", show.mirror.density = FALSE, show.mean = TRUE, show.median = TRUE, show.range = TRUE, show.quartiles = TRUE, show.values = FALSE)
    # Figure 10 - Violin plot
    Distribution(TrialOpens, maximum.bins = 100, values.title = "Number of opens", vertical = TRUE, density.type = "Density", show.mirror.density = TRUE, show.mean = TRUE, show.median = TRUE, show.range = TRUE, show.quartiles = TRUE, show.values = FALSE)
    # Figure 11 - Bean Plot
    Distribution(TrialOpens, maximum.bins = 100, values.title = "Number of opens", vertical = TRUE, density.type = "Density", show.mirror.density = TRUE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = TRUE)
})


z = c(Normals,list(y1 = c(0.75, 5.25, 5.5, 6, 6.2, 6.6, 6.80, 7.0, 7.2, 7.5, 7.5, 7.75, 8.15,
       8.15, 8.65, 8.93, 9.2, 9.5, 10, 10.25, 11.5, 12, 16, 20.90, 22.3, 23.25),
y2 = c(0.75, 5.25, 5.5, 6, 6.2, 6.6, 6.80, 7.0, 7.2, 7.5, 7.5, 7.75, 8.15,
        8.15, 8.65, 8.93, 9.2, 9.5, 10, 10.25, 11.5, 12, 16, 20.90, 22.3, 23.25),
y3 = c(0.75, 5.25, 5.5, 6, 6.2, 6.6, 6.80, 7.0, 7.2, 7.5, 7.5, 7.75, 8.15,
        8.15, 8.65, 8.93, 9.2, 9.5, 10, 10.25, 11.5, 12, 16, 20.90, 22.3, 23.25),
y4 = c(0.75, 5.25, 5.5, 6, 6.2, 6.6, 6.80, 7.0, 7.2, 7.5, 7.5, 7.75, 8.15,
        8.15, 8.65, 8.93, 9.2, 9.5, 10, 10.25, 11.5, 12, 16, 20.90, 22.3, 23.25)) )

test_that("Box plots", {
    expect_error(print(Distribution(z, box.points = "All", values.title = "Number of opens", vertical = TRUE, density.type = "Box",show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = FALSE)), NA)
    Distribution(z, box.points = "All", values.title = "Number of opens", vertical = FALSE, density.type = "Box",show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = FALSE)
    Distribution(z, box.points = "Outliers", values.title = "Number of opens", vertical = TRUE, density.type = "Box",show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = FALSE)
    Distribution(z, box.points = "Suspected outliers", values.title = "Number of opens", vertical = TRUE, density.type = "Box",show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = FALSE)
})

test_that("Histograms", {
    expect_error(print(Distribution(z, values.title = "Number of opens", vertical = FALSE, density.type = "Histogram", show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = FALSE)), NA)
    Distribution(z, values.title = "Number of opens", maximum.bins = 100, vertical = FALSE, density.type = "Histogram", show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = FALSE)
    Distribution(z, values.title = "Number of opens", maximum.bins = 100, histogram.cumulative = TRUE, vertical = FALSE, density.type = "Histogram", show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = FALSE)
})

test_that("Density", {
    Distribution(TrialOpens, automatic.lower.density = FALSE, values.title = "Number of opens", vertical = FALSE, density.type = "Density", show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = FALSE)
    Distribution(TrialOpens, automatic.lower.density = TRUE, values.title = "Number of opens", vertical = FALSE, density.type = "Density", show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = FALSE)
    Distribution(TrialOpens, automatic.lower.density = TRUE, values.title = "Number of opens", vertical = FALSE, density.type = "Density", show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = TRUE)
})


test_that("Distribution: checking all the common parameters",{
    expect_error(print(Distribution(Normals[1])), NA)
    Distribution(Normals)
    Distribution(Normals, mean.color = "pink")
    Distribution(Normals, median.color = "orange")
    Distribution(Normals, range.color = "red")
    Distribution(Normals, quartile.color = "blue")
    Distribution(Normals, density.color = "brown")
    Distribution(Normals, density.color = "brown", density.type = "Box", show.median = FALSE, show.mirror.density = FALSE, show.quartiles = FALSE, show.mean = FALSE, show.range = FALSE)
    Distribution(Normals, density.color = "brown", density.type = "Histogram", show.mirror.density = FALSE)
    Distribution(Normals, global.font.family = "Courier", global.font.color = "Green", title = "My title", values.title = "Values title")
    Distribution(Normals, global.font.family = "Courier", global.font.color = "Green", title = "My title", values.title = "Values title",
                 title.font.family = "Times New Roman", title.font.color = "Red", title.font.size = 30, margin.top = 50)
    Distribution(Normals, background.fill.color = "Yellow")
    Distribution(Normals, background.fill.color = "Yellow", background.fill.opacity = 0.5)
    Distribution(Normals, background.fill.color = "Yellow", background.fill.opacity = 0.5, charting.area.fill.color = "blue")
    Distribution(Normals, background.fill.color = "Yellow", background.fill.opacity = 0.5, charting.area.fill.color = "blue", charting.area.fill.opacity = 0.5)
    Distribution(Normals, margin.top = 0, margin.left = 0, margin.bottom = 0, margin.right = 0)
    Distribution(Normals, margin.top = 200, margin.left = 200, margin.bottom = 200, margin.right = 200)
    Distribution(Normals, grid.show = TRUE)
    Distribution(Normals, grid.show = TRUE, vertical = FALSE, density.type = "Box", show.median = FALSE, show.mirror.density = FALSE, show.quartiles = FALSE, show.mean = FALSE, show.range = FALSE)
    Distribution(Normals, values.title = "Values title", values.title.font.color =  "Red", values.title.font.family = "Courier", values.title.font.size = 40)
    Distribution(Normals, values.line.width = 0)
    Distribution(Normals, values.line.width = 0.5)
    Distribution(Normals, values.line.width = 10, values.line.color = "Pink")
    Distribution(Normals, values.line.width = 1, values.line.color = "Pink", values.tick.mark.length = 10)
    Distribution(Normals, values.bounds.minimum = -100, values.bounds.maximum = 100)
    Distribution(Normals, values.bounds.minimum = -100, values.bounds.maximum = 100, values.tick.distance = 20)
    expect_error(Distribution(Normals, values.tick.distance = 20))
    Distribution(Normals, values.zero.line.width = 0)
    Distribution(Normals, values.zero.line.width = 1)
    Distribution(Normals, values.zero.line.width = 10)
    Distribution(Normals, values.zero.line.width = 10, values.zero.line.color = "Red")
    Distribution(Normals, grid.show = TRUE)

    Distribution(Normals, grid.show = TRUE, values.grid.width = 5)
    Distribution(Normals, grid.show = FALSE, values.grid.width = 5)

    Distribution(Normals, values.tick.show = FALSE)
    Distribution(Normals, values.tick.prefix = "My ")
    Distribution(Normals, values.tick.prefix = "My ", values.tick.suffix = " dogs")

    Distribution(Normals, values.tick.angle = 45)
    Distribution(Normals, values.tick.font.color = "Red")
    Distribution(Normals, values.tick.font.family = "Courier")
    Distribution(Normals, values.tick.font.size = 13)

    Distribution(Normals, categories.tick.font.color = "Red")
    Distribution(Normals, categories.tick.font.family = "Courier")
    Distribution(Normals, categories.tick.font.size = 13)

    Distribution(Normals, categories.tick.label.wrap = FALSE)
    Distribution(Normals, categories.tick.label.wrap.nchar = 12)

    Distribution(Normals, tooltip.show = FALSE)
    Distribution(Normals, modebar.show = TRUE)

    Distribution(Normals, values.line.width = 10)


          })

test_that("Tabular input",{
    t1 = structure(c(0, 0.0828402366863906, 0.343195266272189, 0.180473372781065,
        0.029585798816568, 0.0414201183431953, 0.254437869822485, 0.0473372781065089,
        0.0207100591715976, 0.00265957446808511, 0.11968085106383, 0.300531914893617,
        0.117021276595745, 0.0398936170212766, 0.0558510638297872, 0.295212765957447,
        0.0531914893617021, 0.0159574468085106), statistic = "Column %", name = "Age by Gender", questions = c("Age",
        "Gender"), .Dim = c(9L, 2L), .Dimnames = structure(list(Age = c("15 and under",
        "16-19 yrs", "20-24 yrs", "25-29 yrs", "30-34 yrs", "35-44 yrs",
        "45-54 yrs", "55-64 yrs", "65 and over"), `NA` = c("male", "female"
        )), .Names = c("Age", "Gender")))
    Histogram(t1)
    tab3 <- structure(c(853, 854, 855, 851, 852, 883, 884, 885, 881, 882,
713, 714, 715, 711, 712, 1351, 1352, 1353, 1354, 1355, 871, 872,
873, 874, 875, 761, 762, 763, 764, 765, 411, 412, 413, 414, 415,
791, 792, 793, 794, 795, 941, 942, 943, 944, 945, 1023, 1024,
1025, 1021, 1022, 911, 912, 913, 914, 915, 841, 842, 843, 844,
845, 893, 894, 895, 891, 892, 1034, 1033, 1035, 1031, 1032, 1171,
1172, 1173, 1174, 1175, 691, 692, 693, 694, 695, 631, 632, 633,
634, 635, 1311, 1312, 1313, 1314, 1315, 231, 232, 233, 234, 235,
551, 552, 553, 554, 555, 131, 132, 133, 134, 135, 1141, 1142,
1143, 1144, 1145, 483, 484, 485, 481, 482, 543, 544, 545, 541,
542, 1671, 1672, 1673, 1674, 1675, 1691, 1692, 1693, 1694, 1695,
1603, 1604, 1605, 1601, 1602, NaN, NaN, NaN, NaN, NaN, 1541,
1542, 1543, 1544, 1545, 1621, 1622, 1623, 1624, 1625, 571, 572,
573, 574, 575, 1631, 1632, 1633, 1634, 1635, 1684, 1685, 1682,
1683, 1681, 1511, 1512, 1513, 1514, 1515, 1163, 1164, 1165, 1161,
1162, 823, 824, 825, 821, 822, 1013, 1014, 1015, 1012, 1011,
863, 864, 865, 861, 862, 383, 384, 385, 381, 382, 813, 814, 815,
811, 812, 200029169, 200029169, 200029169, 200029169, 200029169,
43, 44, 45, 41, 42, 1193, 1194, 1195, 1191, 1192, 991, 992, 993,
994, 995, 371, 372, 373, 374, 375, 5417, 5417, 5417, 5417, 5417,
122, 122, 122, 122, 122, 1101, 1102, 1103, 1104, 1105, 1291,
1292, 1293, 1294, 1295, 573, 574, 575, 572, 571, 533, 534, 535,
531, 532, 593, 594, 595, 591, 592, 1211, 1212, 1213, 1214, 1215,
472, 473, 475, 471, 474, 1203, 1204, 1205, 1201, 1202, 963, 964,
965, 961, 962, 571, 572, 573, 574, 575, 1023, 1024, 1025, 1021,
1022, 621, 622, 623, 624, 625, 1423, 1424, 1425, 1421, 1422,
1371, 1372, 1373, 1374, 1375, 641, 642, 643, 644, 645, 573, 574,
575, 571, 572, 1261, 1262, 1263, 1264, 1265, 783, 784, 785, 781,
782, 31, 32, 33, 34, 35, 701, 702, 703, 704, 705, 901, 902, 903,
904, 905, 201, 202, 203, 204, 205, 511, 512, 513, 514, 515, 141,
142, 143, 144, 145, 161, 162, 163, 164, 165, 773, 774, 775, 771,
772, 664, 663, 661, 662, 665, 311, 312, 313, 314, 315, 261, 262,
263, 264, 265, 741, 742, 743, 744, 745, 281, 282, 283, 284, 285,
571, 572, 573, 574, 575, 833, 834, 835, 831, 832, 671, 672, 673,
674, 675, 21, 22, 23, 24, 25, 1321, 1322, 1703, 1704, 1705, 1331,
1332, 1333, 1334, 1335, 1493, 1494, 1495, 1491, 1492, 1481, 1482,
1483, 1484, 1485, 291, 292, 293, 294, 295, 251, 252, 253, 254,
255, 681, 682, 683, 684, 685, 1271, 1272, 1273, 1274, 1275, 71,
72, 73, 74, 75, 391, 392, 393, 394, 395, 451, 452, 453, 454,
455, 1441, 1442, 1443, 1444, 1445, 343, 344, 345, 341, 342, 61,
61, 61, 61, 61, 331, 332, 333, 334, 335, 124, 124, 124, 124,
124, 931, 932, 933, 934, 935, 1361, 1362, 1363, 1364, 1365, 491,
492, 493, 494, 495, 301, 302, 303, 304, 305, 1611, 1612, 1613,
1614, 1615, 463, 464, 465, 461, 462, 423, 424, 425, 422, 421,
401, 402, 403, 404, 405, 601, 602, 603, 604, 605, 271, 272, 273,
274, 275, 1471, 1472, 1473, 1474, 1475, 723, 724, 725, 721, 722,
1231, 1232, 1233, 1234, 1235, 181, 182, 183, 184, 185, 321, 322,
323, 324, 325, 653, 654, 655, 652, 651, 1581, 1582, 1583, 1584,
1585, NaN, NaN, NaN, NaN, NaN, 563, 564, 565, 561, 562, 171,
172, 173, 174, 175, 61, 62, 63, 64, 65, 981, 982, 983, 984, 985,
1671, 1672, 1673, 1674, 1675, 106, 106, 106, 106, 106, 1091,
1092, 1093, 1094, 1095, 1301, 1302, 1303, 1304, 1305, 1651, 1652,
1653, 1654, 1655, 921, 922, 923, 924, 925, 573, 574, 575, 571,
572, 1571, 1572, 1573, 1574, 1575, 501, 502, 503, 504, 505, 361,
362, 363, 364, 365, 1133, 1134, 1135, 1131, 1132, 1051, 1052,
1053, 1054, 1055, NaN, NaN, NaN, NaN, NaN, 1701, 1702, 1703,
1704, 1705, 1521, 1522, 1523, 1524, 1525, 213, 214, 215, 211,
212, 111, 112, 113, 114, 115, 351, 352, 353, 354, 355, 951, 952,
953, 954, 955, 801, 802, 803, 804, 805, 1531, 1532, 1533, 1534,
1535, 1561, 1562, 1563, 1564, 1565, 1381, 1382, 1383, 1384, 1385,
1041, 1042, 1043, 1044, 1045, 53, 54, 55, 51, 52), .Dim = 725L, .Dimnames = structure(list(
    `IID - Interviewer Identification` = c("1", "2", "3", "4",
    "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15",
    "16", "17", "18", "19", "20", "21", "22", "23", "24", "25",
    "26", "27", "28", "29", "30", "31", "32", "33", "34", "35",
    "36", "37", "38", "39", "40", "41", "42", "43", "44", "45",
    "46", "47", "48", "49", "50", "51", "52", "53", "54", "55",
    "56", "57", "58", "59", "60", "61", "62", "63", "64", "65",
    "66", "67", "68", "69", "70", "71", "72", "73", "74", "75",
    "76", "77", "78", "79", "80", "81", "82", "83", "84", "85",
    "86", "87", "88", "89", "90", "91", "92", "93", "94", "95",
    "96", "97", "98", "99", "100", "101", "102", "103", "104",
    "105", "106", "107", "108", "109", "110", "111", "112", "113",
    "114", "115", "116", "117", "118", "119", "120", "121", "122",
    "123", "124", "125", "126", "127", "128", "129", "130", "131",
    "132", "133", "134", "135", "136", "137", "138", "139", "140",
    "141", "142", "143", "144", "145", "146", "147", "148", "149",
    "150", "151", "152", "153", "154", "155", "156", "157", "158",
    "159", "160", "161", "162", "163", "164", "165", "166", "167",
    "168", "169", "170", "171", "172", "173", "174", "175", "176",
    "177", "178", "179", "180", "181", "182", "183", "184", "185",
    "186", "187", "188", "189", "190", "191", "192", "193", "194",
    "195", "196", "197", "198", "199", "200", "201", "202", "203",
    "204", "205", "206", "207", "208", "209", "210", "211", "212",
    "213", "214", "215", "216", "217", "218", "219", "220", "221",
    "222", "223", "224", "225", "226", "227", "228", "229", "230",
    "231", "232", "233", "234", "235", "236", "237", "238", "239",
    "240", "241", "242", "243", "244", "245", "246", "247", "248",
    "249", "250", "251", "252", "253", "254", "255", "256", "257",
    "258", "259", "260", "261", "262", "263", "264", "265", "266",
    "267", "268", "269", "270", "271", "272", "273", "274", "275",
    "276", "277", "278", "279", "280", "281", "282", "283", "284",
    "285", "286", "287", "288", "289", "290", "291", "292", "293",
    "294", "295", "296", "297", "298", "299", "300", "301", "302",
    "303", "304", "305", "306", "307", "308", "309", "310", "311",
    "312", "313", "314", "315", "316", "317", "318", "319", "320",
    "321", "322", "323", "324", "325", "326", "327", "328", "329",
    "330", "331", "332", "333", "334", "335", "336", "337", "338",
    "339", "340", "341", "342", "343", "344", "345", "346", "347",
    "348", "349", "350", "351", "352", "353", "354", "355", "356",
    "357", "358", "359", "360", "361", "362", "363", "364", "365",
    "366", "367", "368", "369", "370", "371", "372", "373", "374",
    "375", "376", "377", "378", "379", "380", "381", "382", "383",
    "384", "385", "386", "387", "388", "389", "390", "391", "392",
    "393", "394", "395", "396", "397", "398", "399", "400", "401",
    "402", "403", "404", "405", "406", "407", "408", "409", "410",
    "411", "412", "413", "414", "415", "416", "417", "418", "419",
    "420", "421", "422", "423", "424", "425", "426", "427", "428",
    "429", "430", "431", "432", "433", "434", "435", "436", "437",
    "438", "439", "440", "441", "442", "443", "444", "445", "446",
    "447", "448", "449", "450", "451", "452", "453", "454", "455",
    "456", "457", "458", "459", "460", "461", "462", "463", "464",
    "465", "466", "467", "468", "469", "470", "471", "472", "473",
    "474", "475", "476", "477", "478", "479", "480", "481", "482",
    "483", "484", "485", "486", "487", "488", "489", "490", "491",
    "492", "493", "494", "495", "496", "497", "498", "499", "500",
    "501", "502", "503", "504", "505", "506", "507", "508", "509",
    "510", "511", "512", "513", "514", "515", "516", "517", "518",
    "519", "520", "521", "522", "523", "524", "525", "526", "527",
    "528", "529", "530", "531", "532", "533", "534", "535", "536",
    "537", "538", "539", "540", "541", "542", "543", "544", "545",
    "546", "547", "548", "549", "550", "551", "552", "553", "554",
    "555", "556", "557", "558", "559", "560", "561", "562", "563",
    "564", "565", "566", "567", "568", "569", "570", "571", "572",
    "573", "574", "575", "576", "577", "578", "579", "580", "581",
    "582", "583", "584", "585", "586", "587", "588", "589", "590",
    "591", "592", "593", "594", "595", "596", "597", "598", "599",
    "600", "601", "602", "603", "604", "605", "606", "607", "608",
    "609", "610", "611", "612", "613", "614", "615", "616", "617",
    "618", "619", "620", "621", "622", "623", "624", "625", "626",
    "627", "628", "629", "630", "631", "632", "633", "634", "635",
    "636", "637", "638", "639", "640", "641", "642", "643", "644",
    "645", "646", "647", "648", "649", "650", "651", "652", "653",
    "654", "655", "656", "657", "658", "659", "660", "661", "662",
    "663", "664", "665", "666", "667", "668", "669", "670", "671",
    "672", "673", "674", "675", "676", "677", "678", "679", "680",
    "681", "682", "683", "684", "685", "686", "687", "688", "689",
    "690", "691", "692", "693", "694", "695", "696", "697", "698",
    "699", "700", "701", "702", "703", "704", "705", "706", "707",
    "708", "709", "710", "711", "712", "713", "714", "715", "716",
    "717", "718", "719", "720", "721", "722", "723", "724", "725"
    )), .Names = "IID - Interviewer Identification"), statistic = "Values", name = "IID - Interviewer Identification", questions = c("IID - Interviewer Identification",
"RAW DATA"))
    expect_error(Histogram(tab3), NA)
    t4 = array(1:5, 5)
    names(t4) = LETTERS[t4]
    expect_error(Histogram(t4), NA)


})
