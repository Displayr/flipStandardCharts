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

test_that("MissingCasesPlot",
{
    # This should create a blank plot
    expect_error(MissingCasesPlot(dat), NA)
})
