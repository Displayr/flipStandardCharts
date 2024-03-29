context("Time axis")

time0 <- structure(1534379835.05508, class = c("POSIXct", "POSIXt"))

for (n in c(5, 10, 11, 15))
{
    xx <- cbind(A=1:n, B=(1:n)+1, C=(1:n)+2)
    for (step in c(1, 8, 25, 1e3, 1e4, 1e5, 1e6, 1e7))
    {
        rownames(xx) <- as.character(time0 + (1:n * step))

        filestem <- sprintf("timeaxis-line-n%d-s%d", n, step)
        expect_error(pp <- Line(xx, data.label.show = TRUE), NA)
        #print(pp)
        #readline(prompt=paste0(filestem, ": press [enter] to continue: "))

        filestem <- sprintf("timeaxis-column-n%d-s%d", n, step)
        expect_error(pp <- Column(xx, data.label.show = TRUE), NA)
        #print(pp)
        #readline(prompt=paste0(filestem, ": press [enter] to continue: "))
    }
}

test_that("Warn about sorted data",
{
    date.sorted <- structure(c(10L, 8L, 6L, 4L, 2L), dim = c(5L, 1L), dimnames = list(
        c("Apr 22 2023", "Apr 20 2023", "Apr 18 2023", "Apr 16 2023",
        "Apr 14 2023"), "Every second day"), sorted.rows = TRUE)
    expect_warning(Column(date.sorted), "Sorting has been applied")
    expect_warning(Bar(date.sorted), "Sorting has been applied")
    expect_error(Scatter(date.sorted), NA)
    expect_error(Column(date.sorted, x.tick.format = "Category"), NA)
    expect_error(Bar(date.sorted, y.tick.format = "Category"), NA)

    numeric.sorted <- structure(c(17.5, 73.8333333333333, 42.6666666666667,
        45.1666666666667,40.6666666666667, 34.5, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1),
        dim = c(6L,1L, 3L), dimnames = list(c("3", "2", "1", "5", "6", "4"), "Like + Love",
        c("Row %", "signifUp#0000FF", "signifDown#FF0000")), dimnets = list(
        integer(0), 2L), dimduplicates = list(integer(0), 2L), span = list(
        rows = structure(list(c("Brand 2", "Brand 1", "Brand 5",
        "Brand 6", "Brand 4", "Brand 3")), class = "data.frame", names = "",
        row.names = c(NA,6L)), columns = structure(list(c("Like + Love", "NET")),
        class = "data.frame", names = "", row.names = 1:2)),
        basedescriptiontext = "sample size = 600", basedescription = list(
        Minimum = 600L, Maximum = 600L, Range = FALSE, Total = 600L,
        Missing = 0L, EffectiveSampleSize = 600L, EffectiveSampleSizeProportion = 100,
        FilteredProportion = 0), QStatisticsTestingInfo = structure(list(
        significancearrowratio = structure(c(0, 1, 0, 0, 0, 0), dim = 6L),
        significancedirection = structure(c("None", "Up", "None",
        "None", "None", "None"), dim = 6L), significancefontsizemultiplier = structure(c(1,
        4.89, 1, 1, 1, 1), dim = 6L), significanceissignificant = structure(c(FALSE,
        TRUE, FALSE, FALSE, FALSE, FALSE), dim = 6L), significanceargbcolor = structure(c(0L,
        -16776961L, 0L, 0L, 0L, 0L), dim = 6L), zstatistic = structure(c(NaN,
        15.5862059022843, NaN, 0.137687331292267, NaN, 1.37687331292265
        ), dim = 6L), pcorrected = structure(c(NaN, 0, NaN, 0.890487532196112,
        NaN, 0.16855141941882), dim = 6L)), row.names = c(6L, 1L, 2L, 3L, 4L, 5L),
        class = "data.frame"), questiontypes = "PickOneMulti",
        footerhtml = "Brand attitude SUMMARY&lt;br /&gt;sample size = 600; 95% confidence level",
        name = "table.Brand.attitude.4", questions = c("Brand attitude","SUMMARY"), assigned.rownames = TRUE, "`signif-annotations`" = list(
        list(type = "Recolor text", data = "signifUp#0000FF", threstype = "above threshold",
        threshold = 0, color = "#0000FF"), list(type = "Arrow - up",
        data = "signifUp#0000FF", threstype = "above threshold",
        threshold = 0, color = "#0000FF", size = 12), list(type = "Recolor text",
        data = "signifDown#FF0000", threstype = "above threshold",
        threshold = 0, color = "#FF0000"), list(type = "Arrow - down",
        data = "signifDown#FF0000", threstype = "above threshold",
        threshold = 0, color = "#FF0000", size = 12)), sorted.rows = TRUE)
    expect_warning(Line(numeric.sorted), "Sorting has been applied")
})

test_that("Date intervals",
{
    quarterly_dates <- as.Date(c("2021-03-31", "2021-06-30", "2021-09-30",
        "2021-12-31", "2022-03-31", "2022-06-30", "2022-09-30", "2022-12-31",
        "2023-03-31", "2023-06-30"))
    expect_equal(setDateTickDistance(quarterly_dates, 10), "M3")

    quarterly_posixct <- structure(c(1617148800, 1625011200, 1632960000,
        1640908800, 1648684800, 1656547200, 1664496000, 1672444800, 1680220800,
        1688083200), class = c("POSIXct", "POSIXt"), tzone = "UTC")
    expect_equal(setDateTickDistance(quarterly_posixct, 10), "M3")
    expect_equal(setDateTickDistance(quarterly_posixct, 4), "M9")

    weekly_dates <- as.Date(c("2023-10-16", "2023-10-23", "2023-10-30",
        "2023-11-06", "2023-11-13"))
    expect_equal(setDateTickDistance(weekly_dates, 5), 604800000)

    # Dates were supplied without day of month, i.e. Nov 2016, Dec 2016, Jan 2017, ...
    monthly_dates <- structure(c(1477958400, 1480550400, 1483228800, 1485907200,
        1488326400), class = c("POSIXct", "POSIXt"), tzone = "UTC")
    expect_equal(setDateTickDistance(monthly_dates, 5), NULL)

    # Dates were supplied without month or day, i.e. 2016, 2017, ...
    yearly_dates <- structure(c(1451606400, 1483228800, 1514764800, 1546300800,
        1577836800), class = c("POSIXct", "POSIXt"), tzone = "UTC")
    expect_equal(setDateTickDistance(yearly_dates, 5), NULL)
})

