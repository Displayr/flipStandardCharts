context("pieChart")
#  rm(list=ls())

for (i in 1:length(good.examples))
{
    print(Chart(y = good.examples[[i]]$Y, x = good.examples[[i]]$X, type = "Column", transpose = good.examples[[i]]$transpose, title = names(good.examples)[i]), hover.include.source.value = TRUE)

    # cat(paste("Press [enter] to continue (test ", i, "/", length(good.examples), ").", sep = ""))
    # line <- readline()
}

# Basic style
basic.style.colors <- c(grDevices::rgb(227, 211, 124, max = 255),
                        grDevices::rgb(244, 159, 154, max = 255),
                        grDevices::rgb(226, 125, 195, max = 255),
                        grDevices::rgb(189, 127, 238, max = 255),
                        grDevices::rgb(153, 164, 253, max = 255),
                        grDevices::rgb(138, 216, 228, max = 255),
                        grDevices::rgb(154, 250, 179, max = 255),
                        grDevices::rgb( 99,  99,  99, max = 255),
                        grDevices::rgb( 20, 225, 190, max = 255))

data1 <- c(100, 200, 180, 50, 320, 90, 30)
names(data1) <- c("JavaScript", "CSS", "HTML", "PHP", "MySQL", "Apache", "Linux")
data1 <- as.matrix(data1)
colnames(data1) <- "Language"

Chart(y = data1,
      type = "Pie",
      transpose = TRUE,
      pie.segment.colors = basic.style.colors,
      pie.inner.radius = "0%",
      pie.values.order = "initial",
      pie.values.display.format = "original")

# Junk style
junk.style.colors <- c(grDevices::rgb(0, 129, 204, max = 255),
                        grDevices::rgb(85, 153, 27, max = 255),
                        grDevices::rgb(192, 0, 11, max = 255),
                        grDevices::rgb(144, 69, 144, max = 255),
                        grDevices::rgb(1, 125, 125, max = 255),
                        grDevices::rgb(249, 65, 15, max = 255),
                        grDevices::rgb(199, 249, 249, max = 255))

data2 <- c(29.05,
           23.22,
           21.76,
           13.49,
           5.20,
           3.26,
           4.02)

names(data2) <- c("I.E.",
                  "Chrome",
                  "Firefox",
                  "Safari",
                  "Opera",
                  "Android",
                  "Other")

data2 <- as.matrix(data2)
colnames(data2) <- "Browser"

Chart(y = data2,
      type = "Pie",
      transpose = TRUE,
      pie.segment.colors = junk.style.colors,
      pie.inner.radius = "0%",
      pie.values.order = "initial",
      pie.values.display.format = "original",
      pie.labels.inner = TRUE,
      pie.values.suffix = "%")

# Donut
donut.style.colors <- c(grDevices::rgb(250, 107, 103, max = 255),
                        grDevices::rgb(136, 197, 155, max = 255),
                        grDevices::rgb(249, 162, 194, max = 255),
                        grDevices::rgb(253, 220, 116, max = 255),
                        grDevices::rgb(154, 228, 245, max = 255))

data4 <- c(8.3,
           16.7,
           16.7,
           25.0,
           33.3)

names(data4) <- c('"Other"',
                  'Forrst',
                  'Dribble',
                  'Instant Messenger',
                  'Twitter')
data4 <- as.matrix(data4)
colnames(data4) <- "Procrastination"

Chart(y = data4,
      type = "Pie",
      transpose = TRUE,
      pie.segment.colors = donut.style.colors,
      pie.inner.radius = "70%",
      pie.values.order = "initial",
      pie.values.display.format = "%",
      pie.labels.inner = TRUE)

# Minimalist
mini.style.colors <- c(grDevices::rgb(228, 108, 11, max = 255),
                       grDevices::rgb(192, 0, 0, max = 255),
                       grDevices::rgb(255, 189, 4, max = 255))

data5 <- c(43,
           28,
           29)

names(data5) <- c("Sample Text 1",
                  "Sample Text 2",
                  "Sample Text 3")

data5 <- as.matrix(data5)
colnames(data5) <- "Procrastination"

Chart(y = data5,
      type = "Pie",
      transpose = TRUE,
      pie.segment.colors = mini.style.colors,
      pie.inner.radius = "50%",
      pie.values.order = "initial",
      pie.values.display.format = "%",
      pie.labels.inner = TRUE)
