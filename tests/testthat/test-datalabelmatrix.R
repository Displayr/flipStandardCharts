context("Data label matrix")

set.seed(12345)
dat2d <- matrix(rnorm(35, 5, 4), 7, 5, dimnames = list(letters[1:7], LETTERS[1:5]))
dat2dpos <- matrix(abs(rnorm(35, 5, 4)), 7, 5, dimnames = list(letters[1:7], LETTERS[1:5]))
dshow <- matrix(FALSE, 7, 5)
dshow[1,2] <- TRUE
dshow[2,3] <- TRUE
dshow[5,4] <- TRUE
dpfix <- matrix("", 7, 5)
dpfix[which(dshow)] <- paste0(letters[1:3], "<")
dsfix <- matrix("", 7, 5)
dsfix[dshow] <- paste0(">", 1:3)

expect_error(Column(dat2d, data.label.show = dshow, data.label.prefix = dpfix, data.label.suffix = dsfix, type = "Column"), NA)
expect_error(Column(dat2d, data.label.show = dshow, data.label.prefix = dpfix, data.label.suffix = dsfix, type = "Stacked"), NA)
expect_error(Bar(dat2d, data.label.show = dshow, data.label.prefix = dpfix, data.label.suffix = dsfix, type = "Column"), NA)
expect_error(Bar(dat2d, data.label.show = dshow, data.label.prefix = dpfix, data.label.suffix = dsfix, type = "Stacked"), NA)
expect_error(Column(dat2d, data.label.show = dshow, data.label.prefix = dpfix, data.label.suffix = dsfix, type = "Column"), NA)
expect_error(Line(dat2d, data.label.show = dshow, data.label.prefix = dpfix, data.label.suffix = dsfix,
     marker.show = dshow), NA)
expect_error(Radar(dat2dpos, data.label.show = dshow, data.label.prefix = dpfix, data.label.suffix = dsfix), NA)
expect_error(Area(dat2dpos, data.label.show = dshow, data.label.prefix = dpfix, data.label.suffix = dsfix, type = "Stacked"), NA)

# 1D input
expect_error(Column(1:5, data.label.show = c(T,F,T,F,F), data.label.prefix = LETTERS[1:5]), NA)
expect_error(Pyramid(1:5, data.label.show = c(T,F,T,F,F), data.label.prefix = LETTERS[1:5]), NA)
expect_error(BarMultiColor(1:5, data.label.show = c(T,F,T,F,F), data.label.prefix = LETTERS[1:5]), NA)

# Small Multples + Averages
expect_error(SmallMultiples(dat2d, "Column", data.label.show = dshow, data.label.prefix = dpfix,
    average.show = T, data.label.format = ".2f"), NA)
expect_error(SmallMultiples(dat2d, "BarMultiColor", data.label.show = dshow, data.label.prefix = dpfix,
    average.show = T, data.label.format = ".2f"), NA)
expect_error(SmallMultiples(dat2dpos, "Pyramid", data.label.show = dshow, data.label.prefix = dpfix,
    average.show = T, data.label.format = ".2f"), NA)

# Problems with line, area and radar
expect_error(SmallMultiples(dat2d, "Line", data.label.show = dshow, data.label.prefix = dpfix, average.show = T), NA)
expect_error(SmallMultiples(dat2d, "Area", data.label.show = dshow, data.label.prefix = dpfix, average.show = T), NA)
expect_error(SmallMultiples(dat2dpos, "Radar", data.label.show = dshow, data.label.prefix = dpfix, average.show = T,
               opacity = 0.8, line.thickness = 2), NA)


# Data labels at ends
expect_error(Line(dat2d, data.label.show.at.ends = T, marker.show.at.ends = T), NA)
expect_error(Line(dat2d, data.label.show.at.ends = T, marker.show = T), NA)


# Missing values
missing1 <- structure(c(NA, 4L, 7L, 3L, 5L, 8L, 5L, 3L, 3L, 3L, 8L, 6L, 3L,
    5L, 4L, 5L, 4L, 5L, 4L, 5L, 6L, 3L, 9L, 5L, 4L, 6L, 5L, 3L, 2L,
    5L, 5L, 5L, 5L, 5L, 4L, 7L, 5L, 7L, 3L, 5L, 4L, 4L, 5L, 4L, 5L,
    5L, 2L, 6L, 3L, 4L, 2L, 7L, 4L, 1L, 6L, 1L, 6L, 5L, 1L, 6L), .Dim = c(20L, 3L),
    .Dimnames = list(c("25", "26", "27", "28", "29", "30", "31",
    "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42",
    "43", "44"), c("A", "B", "C")))

missing124 <- structure(c(NA, NA, 7L, NA, 5L, 8L, 5L, 3L, 3L, 3L, 8L, 6L, 3L,
   5L, 4L, 5L, 4L, 5L, 4L, 5L, 6L, 3L, 9L, 5L, 4L, 6L, 5L, 3L, 2L,
   5L, 5L, 5L, 5L, 5L, 4L, 7L, 5L, 7L, 3L, 5L, 4L, 4L, 5L, 4L, 5L,
   5L, 2L, 6L, 3L, 4L, 2L, 7L, 4L, 1L, 6L, 1L, 6L, 5L, 1L, 6L), .Dim = c(20L,
   3L), .Dimnames = list(c("25", "26", "27", "28", "29", "30", "31",
   "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42",
   "43", "44"), c("A", "B", "C")))


expect_warning(Line(missing1, data.label.show.at.ends = T, marker.show.at.ends = T,
    data.label.font.autocolor = TRUE, marker.size = 20), "Missing values have been omitted")
expect_warning(Line(missing124, data.label.show.at.ends = T, marker.show.at.ends = T,
    data.label.font.autocolor = TRUE, marker.size = 20), "Missing values have been omitted")
expect_warning(Line(missing1, data.label.show.at.ends = T, marker.show.at.ends = T,
    data.label.font.autocolor = TRUE, marker.size = 20, data.label.format = ".2f",
    data.label.prefix = "$", data.label.suffix = letters[1:6]))

showmat <- matrix(FALSE, 20, 3)
showmat[c(3,5, 19),] <- TRUE
expect_warning(Line(missing1, data.label.show = showmat, marker.show = showmat, marker.size = 20,
    opacity = 0.5), "Missing values have been omitted")
expect_warning(Line(missing124, data.label.show = showmat, marker.show = showmat, marker.size = 20), "Missing values have been omitted")


