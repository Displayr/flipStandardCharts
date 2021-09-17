context("Stream")

test_that("Stream",
          {
              set.seed(1223)
              x <- apply(matrix(runif(200), nrow = 4), 1, cumsum)
              colnames(x) <- c('Aardvark', 'Three toed sloth', 'Camel', 'Dog')
              rownames(x) <- as.character(seq(as.Date("1910/1/1"), by = "month", length.out = nrow(x)))

              # Testing combinations of inputs.
              Stream(x)
              Stream(x, x.tick.interval = 2)
              Stream(x, x.tick.interval = 6)
              Stream(x, x.tick.interval = 6, y.axis.show = FALSE)
              Stream(x, x.tick.interval = 6, y.number.ticks = 3)
              Stream(x, x.tick.interval = 6, y.number.ticks = 10)
              Stream(x/100, x.tick.interval = 6, y.number.ticks = 10,  y.tick.format = ".1%")

              # Testing better example
              library(dplyr)
              ggplot2movies::movies %>%
                  select(year, Action, Animation, Comedy, Drama, Documentary, Romance, Short)  %>%
                  group_by(year) %>% as.data.frame  -> dat
              dat <- aggregate.data.frame(dat, list(dat$year), sum)
              rownames(dat) <- dat[, 1]
              dat <- dat[, -1:-2]

              # Invalid data
              invalid <- seq(20)
              names(invalid) <- LETTERS[1:20]
              invalid.2 <- LETTERS[1:20]

              # Errors

              expect_error(Stream(invalid), "Stream requires the rownames of the input data.")
              expect_error(Stream(invalid.2), "Stream graphs should have a tabular input")
              expect_error(Stream(dat, x.tick.interval = 6, x.tick.units = "Month", x.tick.format = ".2f"), "x-axis tick format and units are incompatible.")

              # Automatic formatting

              Stream(dat, x.tick.interval = 0, x.tick.units = "Automatic", x.tick.format = "")
              Stream(dat, x.tick.interval = 6, x.tick.units = "Automatic", x.tick.format = "%Y")

              # Yearly data

              Stream(dat, x.tick.interval = 6, x.tick.units = "Year", x.tick.format = "%y")
              Stream(dat, x.tick.interval = 10, x.tick.units = "Year", x.tick.format = "%Y")
              Stream(dat, x.tick.interval = 20, x.tick.units = "Year", x.tick.format = "%d %b %y")
              Stream(dat, x.tick.interval = 20, x.tick.units = "Year", x.tick.format = "%d %B %y")
              Stream(dat, x.tick.interval = 20, x.tick.units = "Year", x.tick.format = "%d %m %y")
              Stream(dat, x.tick.interval = 20, x.tick.units = "Year", x.tick.format = "%d %b %Y")
              Stream(dat, x.tick.interval = 20, x.tick.units = "Year", x.tick.format = "%d %B %Y")
              Stream(dat, x.tick.interval = 20, x.tick.units = "Year", x.tick.format = "%d %m %Y")
              Stream(dat, x.tick.interval = 20, x.tick.units = "Year", x.tick.format = "%d %m %Y", y.tick.format = "%")

              Stream(dat[1:10, ], x.tick.interval = 12, x.tick.units = "Month", x.tick.format = "%d %m %Y")

              # Monthly data

              rownames(dat) <- as.character(seq.Date(as.Date("2000/1/1"), by = "month", length.out = nrow(dat)))
              dat = dat[1:80, ]

              Stream(dat, x.tick.interval = 3, x.tick.units = "Year", x.tick.format = "%y")
              Stream(dat, x.tick.interval = 3, x.tick.units = "Year", x.tick.format = "%d %B %Y")
              Stream(dat, x.tick.interval = 3, x.tick.units = "Year", x.tick.format = "%B %d %Y")

              Stream(dat, x.tick.interval = 6, x.tick.units = "Month", x.tick.format = "%Y")
              Stream(dat, x.tick.interval = 12, x.tick.units = "Month", x.tick.format = "%d %b %y")

              # Weekly data

              rownames(dat) <- as.character(seq.Date(as.Date("2000/1/1"), by = "week", length.out = nrow(dat)))
              dat = dat[1:80, ]

              Stream(dat, x.tick.interval = 1, x.tick.units = "Year", x.tick.format = "%y")
              Stream(dat, x.tick.interval = 1, x.tick.units = "Year", x.tick.format = "%d %B %Y")

              Stream(dat, x.tick.interval = 2, x.tick.units = "Month", x.tick.format = "%Y")
              Stream(dat, x.tick.interval = 2, x.tick.units = "Month", x.tick.format = "%d %b %y")


              Stream(dat, x.tick.interval = 52, x.tick.units = "Day", x.tick.format = "%Y")
              Stream(dat, x.tick.interval = 200, x.tick.units = "Day", x.tick.format = "%d %b %y")

              # Daily data

              rownames(dat) <- as.character(seq.Date(as.Date("2000/1/1"), by = "day", length.out = nrow(dat)))
              dat = dat[1:80, ]

              Stream(dat, x.tick.interval = 1, x.tick.units = "Year", x.tick.format = "%y")
              Stream(dat, x.tick.interval = 1, x.tick.units = "Year", x.tick.format = "%d %B %Y")

              Stream(dat, x.tick.interval = 1, x.tick.units = "Month", x.tick.format = "%Y")
              Stream(dat, x.tick.interval = 1, x.tick.units = "Month", x.tick.format = "%d %b %y")

              Stream(dat, x.tick.interval = 8, x.tick.units = "Day", x.tick.format = "%Y")
              Stream(dat, x.tick.interval = 8, x.tick.units = "Day", x.tick.format = "%d %b %y")

              # Integers

              rownames(dat) <- 0:(nrow(dat) - 1)
              expect_error(Stream(dat, x.tick.interval = 3, x.tick.format = "", x.tick.units = "Month"),"x-axis tick format and units are incompatible.")
              Stream(dat, x.tick.interval = 3, x.tick.format = "", x.tick.units = "Number")
              Stream(dat, x.tick.interval = 0, x.tick.format = "")
              Stream(dat, x.tick.interval = 5, x.tick.format = ".0f")
              Stream(dat, x.tick.interval = 10, x.tick.format = ".2f")
              Stream(dat, x.tick.interval = 12, x.tick.format = ",.0f")
          }
)

test_that("Date warnings",
{
    tb <- structure(c(41000, 41000, 41000, 41000, 41000, 41000, 41000,
        41000, 41000, 41000, 0, 0, 0, 0, 0, 25000, 25000, 25000, 25000,
        25000, 25000, 25000, 25000, 0, 0, 0, 0, 0, 0, 0, 20700, 20700,
        20700, 20700, 24850, 28605, 31605, 36355, 36355, 0, 0, 0, 0,
        68855, 68855, 13200, 13200, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 10200, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10195,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6836, 11343, 14087,
        17125, 17125, 0, 0, 0, 0, 44222, 55985, 58315, 58315, 0, 0, 6690,
        10980, 26285, 35089, 52950, 75520, 92690, 110124, 125444, 139750,
        167306, 179782, 198043, 231524, 246170, 5481, 10424, 13481, 0,
        20543, 20543, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5153, 13518, 13518,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 39170, 55790, 55790, 55790,
        56257, 76257, 76257, 76257, 76257, 77167, 77167, 109567, 109567,
        109567, 0, 13511, 13511, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 23000, 23000, 23000, 23000, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 84363, 84363, 84363, 84363, 84363, 84363, 84363, 84363,
        84363, 84363, 84363, 84363, 0, 0, 0, 22800, 42345, 57643.5, 65099.5,
        72033.5, 75737.5, 81675.5, 85831.5, 90652.5, 90652.5, 90652.5,
        90945.5, 0, 0, 0, 14558, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 26015, 26015, 26015, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 46626, 46626, 46626, 46626, 46626, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 36388, 63738, 63738, 63738, 63738, 63738, 65938,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 60630, 60630, 89910, 91328, 102648,
        104968, 108708, 0, 0, 0, 0, 0, 0, 0, 0, 35100, 45600, 56200,
        62200, 72400, 82700, 91000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 98730,
        98730, 129918, 177918, 277918, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 86493, 86493, 86493, 86493, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 101373, 101373), .Dim = c(15L, 24L), .Dimnames = list(
            c("01.01.2010", "01.04.2010", "01.07.2010", "01.10.2010",
            "01.01.2011", "01.04.2011", "01.07.2011", "01.10.2011", "01.01.2012",
            "01.04.2012", "01.07.2012", "01.10.2012", "01.01.2013", "01.04.2013",
            "01.07.2013"), c("кухня", "дача", "компьютер",
            "костюмы", "pocketbook 360", "Пенза", "атак",
            "комм платежи", "рынок на водном",
            "рамстор", "фотоаппарат", "Нижний",
            "дом отдыха", "ремонт", "перекресток",
            "остров", "Ставрополь", "кочубеевское",
            "настольные игры", "Германия", "бассейн",
            "Болгария", "отпуск", "Испания")), statistic = "дата")
    expect_warning(Stream(tb), "Date formats are ambiguous")

    new.dates <- as.Date(format="%d.%m.%Y", rownames(tb))
    tb2 <- tb
    rownames(tb2) <- as.character(new.dates)
    expect_error(Stream(tb2), NA)

})


