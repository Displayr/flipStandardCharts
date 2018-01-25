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
              Stream(x, x.tick.interval = 6, y.number.ticks = 10,  values.hovertext.format = ".0f")

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
              expect_error(Stream(invalid.2), "Stream requires numeric data to be plotted.")
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
          })

