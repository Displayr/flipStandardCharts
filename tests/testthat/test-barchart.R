context("barChart")
#  rm(list=ls())

for (i in 1:length(good.examples))
{
    print(StandardChart(y = good.examples[[i]]$Y, x = good.examples[[i]]$X, type = "Bar", transpose = good.examples[[i]]$transpose, title = names(good.examples)[i]), hover.include.source.value = TRUE)

    # cat(paste("Press [enter] to continue (test ", i, "/", length(good.examples), ").", sep = ""))
    # line <- readline()
}


## Basic style

y.data <- rbind(c(0.2,0.3,0.3,0.5,0.3,0.6,0.4,0.8,0.2,0.5,0.1,0.3),c(0.3,0.5,0.1,0.3,0.1,0.2,0.1,0.4,0.1,0.2,0.1,0.4))
rownames(y.data) <- c("Series1","Series2")
colnames(y.data) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

myCols <- c(rgb(0,176,240, max=255), rgb(197,90,17, max=255))
myLineCols <- c(rgb(31,78,121, max=255), rgb(244, 177, 131, max=255))

StandardChart(type = "Bar",
              y = y.data,
              transpose = TRUE,
              x.bounds.minimum = 0,
              x.bounds.maximum = 1.01,
              x.bounds.units.major = .1,
              x.tick.format.manual = "%",
              y.values.reversed = TRUE,
              series.marker.color = myLineCols,
              series.marker.border.width = 0,
              title = "Bar chart - BASIC STYLE",
              x.line.color = rgb(225, 225, 225, max = 255),
              y.line.color = rgb(225, 225, 225, max = 255),
              y.tick.font.color = rgb(100, 100, 100, max = 255),
              x.tick.font.color = rgb(100, 100, 100, max = 255))

## Junk Chart
junk.data <- as.matrix(rbind(c(-32, 58, 35, -12, 35, -16, 79),
                             c(64, -23, 10, -8, 43, 78, 30),
                             c(22, 64, 48, 10, -7, 22, 4)))
colnames(junk.data) <- c("29-Dec","30-Dec","31-Dec","01-Jan","02-Jan","03-Jan","04-Jan")
rownames(junk.data) <- c("Blue", "Yellow", "Red")
myLineCols <- c(rgb(68, 112, 236, max=255), rgb(255, 192, 0, max=255), rgb(210, 0, 0, max=255))

StandardChart(type = "Bar",
              title = "Sample Chart",
              title.font.color = rgb(192, 0, 0, max = 255),
              y = junk.data,
              transpose = TRUE,
              x.bounds.minimum = -40.01,
              x.bounds.maximum = 85.0,
              x.bounds.units.major = 20,
              series.marker.color = "white",
              series.marker.transparency = 0,
              series.marker.border.width = 3,
              series.marker.border.color = myLineCols,
              y.values.reversed = TRUE,
              chart.fill.color = rgb(64, 64, 64, max = 255),
              plot.fill.color = rgb(64, 64, 64, max = 255),
              legend.fill = rgb(64, 64, 64, max = 255),
              legend.font.color = rgb(192, 192, 192, max = 255),
              x.tick.font.color = rgb(192, 192, 192, max = 255),
              y.tick.font.color = rgb(192, 192, 192, max = 255),
              x.zero.line.width = 3,
              y.line.width = 1,
              y.grid.width = 0,
              x.grid.width = 1,
              x.zero.line.color = rgb(100, 100, 100, max = 255),
              x.line.color = rgb(64, 64, 64, max = 255),
              y.line.color = rgb(64, 64, 64, max = 255),
              x.grid.color = rgb(100, 100, 100, max = 255),
              bar.gap = 0,
              x.tick.format.manual = "$",
              bar.data.label.offset = -3,
              bar.data.label.decimals = 0,
              bar.data.label.color = myLineCols)

## NPS chart
nps.data <- as.matrix(rbind(c(-4.35000, -4.30000, -4.25000, -4.20000, -4.15000, -4.10000, -4.05000, -4.00000, -3.95000, -3.90000, -3.85000, -3.80000, -3.75000, -3.70000),
                            c(1.70000, 1.80000, 1.90000, 2.00000, 2.10000, 2.20000, 2.30000, 2.40000, 2.50000, 2.60000, 2.70000, 2.80000, 2.90000, 3.00000),
                            c(3.17928, 2.71607, 2.18535, 1.60391, 0.98690, 0.34856, 0.29722, 0.93666, 1.55589, 2.14070, 2.67604, 3.14530, 3.52948, 3.80684)))
nps.colors <- c(grDevices::rgb(0, 176, 80, 255, max = 255), # Brand A; green.
                grDevices::rgb(192, 0, 0, 255, max = 255), # Brand B; red.
                grDevices::rgb(46, 117, 182, 255, max = 255)) # Brand C; blue.
rownames(nps.data) <- c("Brand A", "Brand B", "Brand C")
colnames(nps.data) <- c("Apr-15","May-15","Jun-15","Jul-15","Aug-15","Sep-15","Oct-15","Nov-15","Dec-15","Jan-16","Feb-16","Mar-16","Apr-16","May-16")

StandardChart(type = "Bar",
              title = "NPS",
              title.font.color = rgb(66, 66, 66, max = 255),
              y = nps.data,
              transpose = TRUE,
              x.bounds.minimum = -5,
              x.bounds.maximum = 5,
              x.bounds.units.major = 1,
              series.marker.color = nps.colors,
              bar.gap = 5,
              legend.font.color = rgb(66, 66, 66, max = 255),
              x.tick.font.color = rgb(66, 66, 66, max = 255),
              y.tick.font.color = rgb(66, 66, 66, max = 255),
              x.zero.line.color = rgb(192, 192, 192, max = 255),
              x.grid.width = 0,
              y.line.color = "white",
              x.line.color = "white",
              bar.data.label.offset = -0.3,
              bar.data.label.decimals = 1,
              bar.data.label.color = nps.colors)



## US Gun manufacture
guns <- as.matrix(rbind(Pistols = c(0.26, 0.24, 0.25, 0.21, 0.22, 0.25, 0.24, 0.25, 0.28, 0.31, 0.32, 0.34, 0.41, 0.4),
                        Revolvers = c(0.09, 0.08, 0.08, 0.11, 0.1, 0.09, 0.09, 0.08, 0.1, 0.1, 0.1, 0.1, 0.1, 0.09),
                        Rifles = c(0.36, 0.38, 0.41, 0.43, 0.45, 0.43, 0.43, 0.44, 0.41, 0.41, 0.41, 0.4, 0.34, 0.35),
                        Shotguns = c(0.28, 0.27, 0.23, 0.23, 0.22, 0.22, 0.24, 0.22, 0.2, 0.16, 0.15, 0.14, 0.14, 0.13),
                        "All others" = c(0.01, 0.03, 0.03, 0.02, 0.01, 0.01, 0, 0.01, 0.01, 0.02, 0.02, 0.02, 0.01, 0.03)))
colnames(guns) <- c(1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009)

gun.colors <- c(grDevices::rgb(0, 112, 192, 255, max = 255), # blue.
                grDevices::rgb(237, 125, 49, 255, max = 255), # orange
                grDevices::rgb(102, 255, 255, 255, max = 255), # bright turquoise
                grDevices::rgb(128, 128, 0, 255, max = 255), # green
                grDevices::rgb(244, 177, 131, 255, max = 255)) # beige

text.colors <- c(grDevices::rgb(255, 255, 255, 255, max = 255),
                 grDevices::rgb(0, 0, 0, 255, max = 255),
                 grDevices::rgb(0, 0, 0, 255, max = 255),
                 grDevices::rgb(255, 255, 255, 255, max = 255),
                 grDevices::rgb(0, 0, 0, 255, max = 255))

######################################## Continue here!!


StandardChart(type = "Stacked Bar",
              title = "U.S. Gun manufacture by type",
              title.font.color = rgb(66, 66, 66, max = 255),
              y = guns,
              transpose = TRUE,
              x.bounds.minimum = 0,
              x.bounds.maximum = 1.01,
              x.bounds.units.major = .1,
              x.tick.format.manual = "%",
              series.marker.color = gun.colors,
              bar.gap = 0,
              legend.font.color = rgb(66, 66, 66, max = 255),
              x.tick.font.color = rgb(66, 66, 66, max = 255),
              y.tick.font.color = rgb(66, 66, 66, max = 255),
              x.zero.line.color = rgb(192, 192, 192, max = 255),
              y.line.color = "white",
              x.line.color = "white",
              bar.data.label.offset = -0.3,
              bar.data.label.decimals = 2,
              bar.data.label.color = text.colors,
              series.marker.border.width = 0,
              bar.data.label.as.percent = TRUE,
              y.bounds.minimum = 1995,
              y.bounds.maximum = 2010,
              y.bounds.units.major = 1)

## Hot Dogs
hotdogs <- as.matrix(rbind("1st" = c(25, 50, 50.5, 44.5, 53.5, 49, 54, 66, 59, 68, 54),
                           "2nd" = c(24, 31, 26, 30.5, 38, 37, 52, 63, 59, 64.5, 43),
                           "3rd" = c(22, 23.5, 25.5, 29.5, 32, 32, 37, 49, 42, 55, 37)))
colnames(hotdogs) <- c("`00", "`01", "`02", "`03", "`04", "`05", "`06", "`07", "`08", "`09", "`10")

hotcols <- c(grDevices::rgb(7, 135, 65, 255, max = 255),
             grDevices::rgb(56, 184, 72, 255, max = 255),
             grDevices::rgb(162, 213, 101, 255, max = 255))

StandardChart(type = "Stacked Bar",
              title = "Hot Dog Competition",
              title.font.color = rgb(66, 66, 66, max = 255),
              y = hotdogs,
              transpose = TRUE,
              x.bounds.minimum = 0,
              x.bounds.maximum = 200,
              x.bounds.units.major = 50,
              series.marker.color = hotcols,
              series.marker.border.width = 0,
              y.line.color = "white",
              x.line.color = "white",
              x.grid.width = 0,
              x.title = "Hot dogs and buns")
