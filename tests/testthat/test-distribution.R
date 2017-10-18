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
Phone <- as.list(phone[, match("q23a", names(phone)):match("q23y", names(phone))])


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
    supressWarnings(Distribution(Phone[1:5]))


})


test_that("Plots used in blog post", {
    # histogram - poor with small sample sizes
    Distribution(TrialOpens[1], values.title = "Number of opens", vertical = FALSE, density.type = "Histogram", show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = FALSE)
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
    Distribution(z, box.points = "All", values.title = "Number of opens", vertical = TRUE, density.type = "Box",show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = FALSE)
    Distribution(z, box.points = "All", values.title = "Number of opens", vertical = FALSE, density.type = "Box",show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = FALSE)
    Distribution(z, box.points = "Outliers", values.title = "Number of opens", vertical = TRUE, density.type = "Box",show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = FALSE)
    Distribution(z, box.points = "Suspected outliers", values.title = "Number of opens", vertical = TRUE, density.type = "Box",show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = FALSE)
})

test_that("Histograms", {
    Distribution(z, values.title = "Number of opens", vertical = FALSE, density.type = "Histogram", show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = FALSE)
    Distribution(z, values.title = "Number of opens", maximum.bins = 100, vertical = FALSE, density.type = "Histogram", show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = FALSE)
    Distribution(z, values.title = "Number of opens", maximum.bins = 100, histogram.cumulative = TRUE, vertical = FALSE, density.type = "Histogram", show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = FALSE)
})

test_that("Density", {
    Distribution(TrialOpens, automatic.lower.density = FALSE, values.title = "Number of opens", vertical = FALSE, density.type = "Density", show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = FALSE)
    Distribution(TrialOpens, automatic.lower.density = TRUE, values.title = "Number of opens", vertical = FALSE, density.type = "Density", show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = FALSE)
    Distribution(TrialOpens, automatic.lower.density = TRUE, values.title = "Number of opens", vertical = FALSE, density.type = "Density", show.mirror.density = FALSE, show.mean = FALSE, show.median = FALSE, show.range = FALSE, show.quartiles = FALSE, show.values = TRUE)
})


test_that("Distribution: checking all the common parameters",{
    Distribution(Normals[1])
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
    Distribution(Normals, values.line.width = 10) #That this is doing nothing is a problem in Carmen's code. She has been told.
    Distribution(Normals, values.line.width = 10, values.line.color = "Pink")
    Distribution(Normals, values.line.width = 1, values.line.color = "Pink", values.tick.mark.length = 10)
    # The following also do nothing at the time of creating this package. Also reported to Carmen.
    Distribution(Normals, values.tick.marks = "outside", values.line.width = 1, values.line.color = "Pink", values.tick.mark.length = 10)
    Distribution(Normals, values.tick.marks = "inside", values.line.width = 1, values.line.color = "Pink", values.tick.mark.length = 10)
    Distribution(Normals, values.tick.marks = "none", values.line.width = 1, values.line.color = "Pink", values.tick.mark.length = 10)

    Distribution(Normals, values.bounds.minimum = -100, values.bounds.maximum = 100)
    Distribution(Normals, values.bounds.minimum = -100, values.bounds.maximum = 100, values.tick.distance = 20)
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

    Distribution(Normals, values.tick.decimals = 2)
    Distribution(Normals, values.tick.decimals = 2)

    # ColumnChart(-1:5, y.line.width = 1, grid.show = FALSE, y.grid.width = 10)
    # ColumnChart(-1:5, y.line.width = 1, grid.show = TRUE, y.grid.width = 10)
    #
    #
    # ColumnChart(-1:5, y.zero = TRUE, grid.show = FALSE, y.tick.decimals = 2)
    #
    #
    # ColumnChart(-1:5, y.zero = TRUE, y.zero.line.width = 0,  grid.show = FALSE)
    # ColumnChart(-1:5, y.zero = TRUE, y.zero.line.width = 1,  grid.show = FALSE)
    # ColumnChart(-1:5, y.zero = FALSE, y.zero.line.width = 1,  grid.show = FALSE)

          })
