context("lineChart")
#  rm(list=ls())

for (i in 1:length(good.examples))
{
    print(StandardChart(y = good.examples[[i]]$Y, x = good.examples[[i]]$X, type = "Line", transpose = good.examples[[i]]$transpose, title = names(good.examples)[i]), hover.include.source.value = TRUE)

    # cat(paste("Press [enter] to continue (test ", i, "/", length(good.examples), ").", sep = ""))
    # line <- readline()
}



######## Test line charts / custom examples

## Basic style
a.matrix <- rbind(c(0.2,0.3,0.3,0.5,0.3,0.6,0.4,0.8,0.2,0.5,0.1,0.3),c(0.3,0.5,0.1,0.3,0.1,0.2,0.1,0.4,0.1,0.2,0.1,0.4))
rownames(a.matrix) <- c("Series1","Series2")
colnames(a.matrix) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

myCols <- c(rgb(0,176,240, max=255), rgb(197,90,17, max=255))
myLineCols <- c(rgb(31,78,121, max=255),rgb(192,0,0, max=255))

StandardChart(y = a.matrix,
              type = "Line",
              transpose = TRUE,
              series.line.color = myLineCols,
              series.line.width = 3,
              title = "Line chart - BASIC STYLE",
              y.tick.format.manual = "%",
              y.bounds.minimum = 0,
              y.bounds.maximum = 1,
              y.bounds.units.major = 0.1,
              x.tick.marks = "outside",
              y.tick.font.size = 16,
              x.tick.font.size = 16,
              legend.font.size = 14
              )

# Junk chart
data(junk)
junk.data <- junk
junk.data.text <- as.matrix((junk.data[, -1, drop = FALSE]))
rownames(junk.data) <- junk.data[, 1]
junk.data <- as.matrix(junk.data[, -1])
junk.colors <- c(grDevices::rgb(219, 99, 24, 255, max = 255), # Apples, orange-ish.
                 grDevices::rgb(13, 168, 170, 255, max = 255), # Bananas, teal.
                 grDevices::rgb(110, 18, 90, 255, max = 255), # Lemons, dark purple
                 grDevices::rgb(21, 113, 189, 255, max = 255), # Limes, blue
                 grDevices::rgb(97, 111, 2, 255, max = 255), # Oranges, dark green
                 grDevices::rgb(8, 68, 111, 255, max = 255), # Pears, dark blue
                 grDevices::rgb(175, 21, 1, 255, max = 255), # Strawberries, dark red
                 grDevices::rgb(158, 72, 14, 255, max = 255), # Gooseberries
                 grDevices::rgb(99, 99, 99, 255, max = 255), # Cloudberries
                 grDevices::rgb(153, 115, 0, 255, max = 255), # Kiwi fruit
                 grDevices::rgb(38, 68, 120, 255, max = 255), # Raspberries
                 grDevices::rgb(67, 104, 43, 255, max = 255), # Blackberries
                 grDevices::rgb(124, 175, 221, 255, max = 255), # Currants
                 grDevices::rgb(241, 151, 90, 255, max = 255), # Rhubarb
                 grDevices::rgb(183, 183, 183, 255, max = 255), # Peach
                 grDevices::rgb(255, 205, 51, 255, max = 255), # Mango
                 grDevices::rgb(105, 142, 208, 255, max = 255), # Papaya
                 grDevices::rgb(140, 193, 104, 255, max = 255), # Goat
                 grDevices::rgb(50, 125, 194, 255, max = 255), # Blueberries
                 grDevices::rgb(210, 96, 18, 255, max = 255)) # Lingonberries

StandardChart(y = junk.data,
              transpose = TRUE,
              title = "Monthly Sales",
              type = "Line",
              plot.fill.color = rgb(222, 233, 254, max = 255),
              chart.fill.color = rgb(222, 233, 254, max = 255),
              y.grid.width = 0,
              y.bounds.minimum = 0,
              y.bounds.maximum = 45,
              y.bounds.units.major = 5,
              y.tick.decimals = 0,
              y.tick.font.size = 11,
              y.tick.font.family = "Tahoma",
              series.marker.text = TRUE,
              series.marker.text.color = rgb(192, 192, 192, max = 255),
              series.marker.text.size = 12,
              series.line.width = 3,
              series.line.color = junk.colors,
              series.marker.show = "1",
              series.marker.color = junk.colors,
              series.marker.border.width = 1,
              series.marker.size = 10,
              series.marker.border.color = "black",
              margin.t = 30,
              legend.fill = rgb(222, 233, 254, max = 255),
              legend.font.size = 14,
              legend.font.family = "Tahoma",
              y.line.color = rgb(222, 233, 254, max = 255),
              x.line.color = rgb(222, 233, 254, max = 255))

## NPS chart
nps.data <- as.matrix(rbind(c(-4.35000, -4.30000, -4.25000, -4.20000, -4.15000, -4.10000, -4.05000, -4.00000, -3.95000, -3.90000, -3.85000, -3.80000, -3.75000, -3.70000),
                            c(1.70000, 1.80000, 1.90000, 2.00000, 2.10000, 2.20000, 2.30000, 2.40000, 2.50000, 2.60000, 2.70000, 2.80000, 2.90000, 3.00000),
                            c(3.17928, 2.71607, 2.18535, 1.60391, 0.98690, 0.34856, 0.29722, 0.93666, 1.55589, 2.14070, 2.67604, 3.14530, 3.52948, 3.80684)))
colnames(nps.data) <- c("Apr-15","May-15","Jun-15","Jul-15","Aug-15","Sep-15","Oct-15","Nov-15","Dec-15","Jan-16","Feb-16","Mar-16","Apr-16","May-16")
rownames(nps.data) <- c("Brand A", "Brand B", "Brand C")

nps.colors <- c(grDevices::rgb(46, 117, 182, 255, max = 255), # Brand A; blue.
                grDevices::rgb(192, 0, 0, 255, max = 255), # Brand B; red.
                grDevices::rgb(0, 176, 80, 255, max = 255)) # Brand C; green.

StandardChart(y = nps.data,
              title = "NPS",
              transpose = TRUE,
              type = "Line",
              series.line.color = nps.colors,
              series.line.width = 3,
              y.bounds.minimum = -5,
              y.bounds.maximum = 5,
              y.bounds.units.major = 1,
              y.grid.width = 0,
              y.zero.line.color = rgb(225, 225, 225, max = 255),
              x.tick.angle = 315,
              y.tick.font.size = 10,
              legend.font.size = 14)

## Aging population
age.data <- as.matrix(rbind(c(0.154, 0.143, 0.138, 0.122, 0.121, 0.116, 0.109, 0.093, 0.08, 0.107, 0.113, 0.084, 0.072, 0.076, 0.068, 0.068),
                            c(0.358, 0.354, 0.343, 0.339, 0.323, 0.304, 0.298, 0.295, 0.264, 0.232, 0.271, 0.295, 0.248, 0.213, 0.218, 0.207),
                            c(0.357, 0.354, 0.359, 0.369, 0.377, 0.39, 0.384, 0.383, 0.389, 0.376, 0.322, 0.317, 0.371, 0.401, 0.37, 0.354),
                            c(0.104, 0.119, 0.126, 0.131, 0.137, 0.146, 0.161, 0.174, 0.198, 0.203, 0.201, 0.206, 0.196, 0.186, 0.22, 0.246),
                            c(0.027, 0.03, 0.034, 0.039, 0.041, 0.043, 0.047, 0.054, 0.068, 0.081, 0.092, 0.098, 0.113, 0.125, 0.124, 0.124)))
x.labels <- c("1860", "1870", "1880", "1890", "1900", "1910", "1920", "1930", "1940", "1950", "1960", "1970", "1980", "1990", "2000", "2005")
rownames(age.data) <- c("Under 5","5 to 19","20 to 44","45 to 64","65+")
colnames(age.data) <- x.labels

# Assigns a vector of colours of as many members as the passed in vector x, in increasing lightness from a given source colour
setColors <- function (x, red, green, blue) {
    col.vector <- ""
    number.rows <- nrow(x) + 1

    for (i in nrow(x):1){
        red.factor <- ((255 - red) / number.rows) * i
        green.factor <- ((255 - green) / number.rows) * i
        blue.factor <- ((255 - blue) / number.rows) * i

        col.vector <- c(col.vector, grDevices::rgb(red + red.factor, green + green.factor, blue + blue.factor, 255, max = 255))
    }

    return(col.vector[-1, drop = FALSE])
}

age.colors <- setColors(age.data, 38, 85, 154)

StandardChart(y = age.data,
              title.font.size = 12,
              title.font.color = rgb(66, 66, 66, max = 255),
              type = "Line",
              title = "<b>AN AGING POPULATION</b>",
              transpose = TRUE,
              series.line.color = age.colors,
              series.line.width = 3,
              y.tick.format.manual = "%",
              y.line.color = "white",
              x.line.color = rgb(222, 222, 222, max = 255),
              y.zero.line.width = 0,
              x.zero.line.width = 0,
              subtitle.text = "In 1860, an estimated 13.1 percent of the U.S. population was 45 years or older.<br>In 2005, the estimate is up to 23.9 percent.",
              subtitle.font.size = 12,
              global.font.family.override = "Times New Roman"
              )


### Tufte
tufte.data <- as.matrix(c(8, 7, 5.7, 7, 7, 7.5, 7.5, 8, 9, 8.5, 12, 13, 14, 14.5, 15, 15.5, 20, 22, 30, 37, 39, 41.9))
rownames(tufte.data) <- c(1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008)
colnames(tufte.data) <- "Series 1"

StandardChart(y = tufte.data,
              type = "Line",
              series.line.width = 7,
              x.tick.font.size = 12,
              x.tick.angle = 315,
              series.line.color = "black",
              y.grid.width = 0,
              x.values = c(1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008),
              x.labels = c("1987", "", "", "1990", "", "", "1993", "", "", "1996", "", "", "1999", "", "", "2002", "", "", "2005", "", "", "2008"),
              margin.inner.pad = 20,
              y.bounds.minimum = 4,
              y.bounds.maximum = 44,
              y.bounds.units.major = 5
              )


