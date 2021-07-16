context("Missing values")
dat <- structure(c(11, 10, 6, 6, 5, 4, 4, 4, 3, 2, 2, 2, 1, 1, 1, 1,
    1, 1, 0, 0, 0, 0, 0, 0, 0, 65, 9, 6, 5, 5, 3, 3, 2, 2, 2, 5,
    4, 1, 2, 2, 1, 0, 0, 0, 5, 3, 2, 1, 0, 0, 0, 63, 8, 4, 6, 2,
    2, 0, 4, 4, 2, 3, 1, 3, 3, 0, 0, 3, 1, 0, 2, 8, 1, 0, 3, 2, 1,
    65), .Dim = c(26L, 3L), .Dimnames = list(c(" Norway (NOR)",
    " Germany (GER)", " Canada (CAN)", " Netherlands (NED)",
    " United States (USA)", " Sweden (SWE)", " Austria (AUT)",
    " France (FRA)", " South Korea (KOR)", " Japan (JPN)",
    " Switzerland (SUI)", " Italy (ITA)", " Czech Republic (CZE)",
    " Slovakia (SVK)", " Belarus (BLR)", " Great Britain (GBR)",
    " Poland (POL)", " Ukraine (UKR)", " China (CHN)", " Olympic Athletes from Russia (OAR)",
    " Australia (AUS)", " Slovenia (SLO)", " Finland (FIN)",
    " Spain (ESP)", " Kazakhstan (KAZ)", NA), c("Gold", "Silver",
    "Bronze")), statistic = structure("NOC", .Names = "Rank"))

test_that("Missing rownames",
{
    expect_silent(Column(dat))
})

set.seed(1234)
n <- 1e4
tb <- matrix(0, n, 10)
colnames(tb) <- LETTERS[1:ncol(tb)]
tb[,1] <- NA
tb[seq(1, n, by = 2), 2] <- NA
tb[seq(1, n, by = 3), 3] <- NA
tb[seq(1, n, by = 4), 4] <- NA
tb[seq(1, n, by = 5), 5] <- NA
tb[seq(1, n, by = 6), 6] <- NA
tb[seq(1, n, by = 10), 7] <- NA
tb[seq(1, n, by = 100), 8] <- NA
tb[seq(1, n, by = 1000), 9] <- NA
tb[seq(1, n, by = 10000), 10] <- NA

small <- cbind(
    Dog = 1:10,
    Elephant = c(1:5, NA, NA, 8:10),
    'Bugs & Spiders' = rep(1,10),
    Fish = c(1:4, rep(NA, 6))
)

test_that("MissingCasesPlot",
{
    # This should create a blank plot
    expect_error(MissingCasesPlot(dat), NA)
    expect_error(MissingCasesPlot(tb), NA)
    expect_error(MissingCasesPlot(small), NA)
})

test_that("Pyramid handles missing values",
{
    x <- c(1e-04, 2, 3, 4, NA, 0, 5)
    expect_warning(Pyramid(x), "Missing")
})

