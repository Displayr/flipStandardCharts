context("Basic object structure")

set.seed(1234)
dat <- cbind(1:20, abs(rnorm(20)))

# Create lists of all charting functions, and data types and options to use
# Functions excluded: Waterfall, Venn, Stream, HeatMap
charting.funcs <- c("Column", "Bar", "Area", "Line", "Pie", "Radar",
                    "Scatter", "LabeledScatter",
                    "Heat", "Palm", "Pie", "BarPictograph",
                    "Box", "Bean", "Distribution", "Density", "Histogram", "Violin")

for (func in charting.funcs)
    test_that(paste("Object structure of", func),
    {
        cmd <- paste0("pp <- ", func, "(dat)")
        expect_error(suppressWarnings(eval(parse(text = cmd))), NA)
        expect_true(!is.null(pp$htmlwidget))
    })


# Special data inputs
tdat <- dat
rownames(tdat) <- sprintf("%02d/01/2001", 1:20)
test_that("Object structure of TimeSeries",
{
    expect_silent(pp <- TimeSeries(tdat))
    expect_true(!is.null(pp$htmlwidget))
})

gdat <- dat[1:10,1]
data("LifeCycleSavings")
names(gdat) <- rownames(LifeCycleSavings)[1:length(gdat)]
test_that("Object structure of GeographicMap",
{
    expect_silent(pp <- GeographicMap(gdat))
    expect_true(!is.null(pp$htmlwidget))
})

vdat <- list(structure(list(sets = list(0), label = "Like", size = 100), .Names = c("sets",
"label", "size")), structure(list(sets = list(1), label = "Love",
    size = 50), .Names = c("sets", "label", "size")), structure(list(
    sets = list(2), label = "Dislike", size = 100), .Names = c("sets",
"label", "size")), structure(list(sets = list(3), label = "Hate",
    size = 50), .Names = c("sets", "label", "size")), structure(list(
    sets = list(0, 1), size = 50), .Names = c("sets", "size")),
    structure(list(sets = list(0, 2), size = 0), .Names = c("sets",
    "size")), structure(list(sets = list(2, 3), size = 50), .Names = c("sets",
    "size")))
test_that("Object structure of Venn",
{
    expect_silent(pp <- Venn(vdat))
    expect_true(!is.null(pp$htmlwidget))
})

sdat <- structure(c(1893, 1894, 1895, 1896, 1897, 1898, 1899, 1900, 1901,
1902, 1903, 1904, 1905, 1906, 1907, 1908, 1909, 1910, 1911, 1912,
1913, 1914, 1915, 1916, 1917, 1918, 1919, 1920, 1921, 1922, 1923,
1924, 1925, 1926, 1927, 1928, 1929, 1930, 1931, 1932, 1933, 1934,
1935, 1936, 1937, 1938, 1939, 1940, 1941, 1942, 1943, 1944, 1945,
1946, 1947, 1948, 1949, 1950, 1951, 1952, 1953, 1954, 1955, 1956,
1957, 1958, 1959, 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967,
1968, 1969, 1970, 1971, 1972), .Names = c("2000-01-01", "2000-01-08",
"2000-01-15", "2000-01-22", "2000-01-29", "2000-02-05", "2000-02-12",
"2000-02-19", "2000-02-26", "2000-03-04", "2000-03-11", "2000-03-18",
"2000-03-25", "2000-04-01", "2000-04-08", "2000-04-15", "2000-04-22",
"2000-04-29", "2000-05-06", "2000-05-13", "2000-05-20", "2000-05-27",
"2000-06-03", "2000-06-10", "2000-06-17", "2000-06-24", "2000-07-01",
"2000-07-08", "2000-07-15", "2000-07-22", "2000-07-29", "2000-08-05",
"2000-08-12", "2000-08-19", "2000-08-26", "2000-09-02", "2000-09-09",
"2000-09-16", "2000-09-23", "2000-09-30", "2000-10-07", "2000-10-14",
"2000-10-21", "2000-10-28", "2000-11-04", "2000-11-11", "2000-11-18",
"2000-11-25", "2000-12-02", "2000-12-09", "2000-12-16", "2000-12-23",
"2000-12-30", "2001-01-06", "2001-01-13", "2001-01-20", "2001-01-27",
"2001-02-03", "2001-02-10", "2001-02-17", "2001-02-24", "2001-03-03",
"2001-03-10", "2001-03-17", "2001-03-24", "2001-03-31", "2001-04-07",
"2001-04-14", "2001-04-21", "2001-04-28", "2001-05-05", "2001-05-12",
"2001-05-19", "2001-05-26", "2001-06-02", "2001-06-09", "2001-06-16",
"2001-06-23", "2001-06-30", "2001-07-07"))
test_that("Object structure of Stream",
{
    expect_silent(pp <- Stream(sdat))
    expect_true(!is.null(pp$htmlwidget))
})

# Check error message for invalid object
xx <- list(A=1, plotly.plot = 1:10)
class(xx) <- "StandardChart"
test_that("Invalid object",
{
    expect_error(print(xx), "StandardChart object does not contain htmlwidget")
})
