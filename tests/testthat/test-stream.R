context("Stream")


test_that("Stream",
          {
              set.seed(1223)
              x <- t(apply(matrix(runif(200), nrow = 4), 1, cumsum))
              rownames(x) <- c('Aardvark', 'Three toed sloth', 'Camel', 'Dog')
              colnames(x) <- as.character(seq(as.Date("1910/1/1"), by = "month", length.out = ncol(x)))

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
              rownames(dat) <- dat[,1]
              dat <- t(dat[, -1:-2])

              # Automatic formatting

              Stream(dat, x.tick.interval = 6, x.tick.units = "", x.tick.format = "")
              Stream(dat, x.tick.interval = 6, x.tick.units = "", x.tick.format = "%Y")
              expect_error(Stream(dat, x.tick.interval = 6, x.tick.unit = "", x.tick.format = ".2f"), "x-axis tick format and units are incompatible.")

              # Yearly data

              Stream(dat, x.tick.interval = 6, x.tick.units = "year", x.tick.format = "%y")
              Stream(dat, x.tick.interval = 10, x.tick.units = "year", x.tick.format = "%Y")
              Stream(dat, x.tick.interval = 20, x.tick.units = "year", x.tick.format = "%d %b %y")
              Stream(dat, x.tick.interval = 20, x.tick.units = "year", x.tick.format = "%d %B %y")
              Stream(dat, x.tick.interval = 20, x.tick.units = "year", x.tick.format = "%d %m %y")
              Stream(dat, x.tick.interval = 20, x.tick.units = "year", x.tick.format = "%d %b %Y")
              Stream(dat, x.tick.interval = 20, x.tick.units = "year", x.tick.format = "%d %B %Y")
              Stream(dat, x.tick.interval = 20, x.tick.units = "year", x.tick.format = "%d %m %Y")
              Stream(dat, x.tick.interval = 20, x.tick.units = "year", x.tick.format = "%d %m %Y", y.tick.format = "%")

              Stream(dat[,1:10], x.tick.interval = 12, x.tick.units = "month", x.tick.format = "%d %m %Y")

              # Monthly data

              colnames(dat) <- as.character(seq.Date(as.Date("2000/1/1"), by = "month", length.out = ncol(dat)))
              dat = dat[, 1:80]

              Stream(dat, x.tick.interval = 3, x.tick.units = "year", x.tick.format = "%y")
              Stream(dat, x.tick.interval = 3, x.tick.units = "year", x.tick.format = "%d %B %Y")
              Stream(dat, x.tick.interval = 3, x.tick.units = "year", x.tick.format = "%B %d %Y")

              Stream(dat, x.tick.interval = 6, x.tick.units = "month", x.tick.format = "%Y")
              Stream(dat, x.tick.interval = 12, x.tick.units = "month", x.tick.format = "%d %b %y")

              # Weekly data

              colnames(dat) <- as.character(seq.Date(as.Date("2000/1/1"), by = "week", length.out = ncol(dat)))
              dat = dat[, 1:80]

              Stream(dat, x.tick.interval = 1, x.tick.units = "year", x.tick.format = "%y")
              Stream(dat, x.tick.interval = 1, x.tick.units = "year", x.tick.format = "%d %B %Y")

              Stream(dat, x.tick.interval = 2, x.tick.units = "month", x.tick.format = "%Y")
              Stream(dat, x.tick.interval = 2, x.tick.units = "month", x.tick.format = "%d %b %y")


              Stream(dat, x.tick.interval = 52, x.tick.units = "day", x.tick.format = "%Y")
              Stream(dat, x.tick.interval = 200, x.tick.units = "day", x.tick.format = "%d %b %y")

              # Daily data

              colnames(dat) <- as.character(seq.Date(as.Date("2000/1/1"), by = "day", length.out = ncol(dat)))
              dat = dat[, 1:80]

              Stream(dat, x.tick.interval = 1, x.tick.units = "year", x.tick.format = "%y")
              Stream(dat, x.tick.interval = 1, x.tick.units = "year", x.tick.format = "%d %B %Y")

              Stream(dat, x.tick.interval = 1, x.tick.units = "month", x.tick.format = "%Y")
              Stream(dat, x.tick.interval = 1, x.tick.units = "month", x.tick.format = "%d %b %y")

              Stream(dat, x.tick.interval = 8, x.tick.units = "day", x.tick.format = "%Y")
              Stream(dat, x.tick.interval = 8, x.tick.units = "day", x.tick.format = "%d %b %y")

              # Integers

              colnames(dat) <- 0:(ncol(dat) - 1)
              expect_error(Stream(dat, x.tick.interval = 3, x.tick.format = "", x.tick.units = "Month"),"x-axis tick format and units are incompatible.")
              Stream(dat, x.tick.interval = 3, x.tick.format = "", x.tick.units = "Number")
              Stream(dat, x.tick.interval = 3, x.tick.format = "")
              Stream(dat, x.tick.interval = 5, x.tick.format = ".0f")
              Stream(dat, x.tick.interval = 10, x.tick.format = ".2f")
              Stream(dat, x.tick.interval = 12, x.tick.format = ",.0f")
          })

