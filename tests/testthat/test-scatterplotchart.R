context("scatterplotChart")
#  rm(list=ls())

for (i in 1:length(good.examples))
{
    print(Chart(y = good.examples[[i]]$Y, x = good.examples[[i]]$X, type = "Scatter Plot", transpose = good.examples[[i]]$transpose, title = names(good.examples)[i], aggregate.period = good.examples[[i]]$aggregate.period, hover.include.source.value = TRUE))

    # cat(paste("Press [enter] to continue (test ", i, "/", length(good.examples), ").", sep = ""))
    # line <- readline()
}



######## Test scatter plot / custom examples

## Basic style
num.vector <- c(0.052031756, 0.288736132, 0.770838175, 0.295320494, 0.381308827, 0.916185648, 0.021294701, 0.871968413, 0.475025918, 0.105003445, 0.921155659, 0.22425887, 0.986267092, 0.136498702, 0.43939713, 0.429545097, 0.004527879, 0.782070465, 0.350548074, 0.341654347)
grey.line <- "#C6C6C6"


Chart(y = num.vector, type = "Scatter Plot", x.grid.width = 1, x.tick.frequency = 5, series.marker.size = 8,
      x.line.width = 1, x.line.color = grey.line,
      y.line.width = 1, y.line.color = grey.line,
      y.bounds.minimum = 0, y.bounds.maximum = 1.2, y.bounds.units.major = 0.2,
      x.bounds.minimum = 0, x.bounds.maximum = 20, x.bounds.units.major = 5,
      y.tick.decimals = 1)


## Junk chart
