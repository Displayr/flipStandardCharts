# context("Waterfall")
#
# test_that("Waterfal inputs",
#           {
#               # vector
#               wt = c(Churned = -0.9845328369, Contraction =  -0.0084918804, Expansion = 0.0008160907, Resurrected = 0.6928227872, New = 0.3066487691)
#               Waterfall(wt)
#               Waterfall(wt * 100, suffix = "%", title = "Waterfall chart", decimals = 0)
#               Waterfall(wt * 100, suffix = "%", title = "Waterfall chart", decimals = 1, colors = c("red", "orange", "teal", "turquoise", "blue"))
#               wt = c(Churned = -0.085037667, Contraction =  -0.066377404, Expansion = 0.222780538, Resurrected = 0.004862808, New = 0.365256087)
#               Waterfall(wt * 100, suffix = "%", title = "Waterfall chart", decimals = 0, y.title = "Revenue change")
#               Waterfall(wt, suffix = "%", title = "Waterfall chart", decimals = 2, y.title = "Revenue change")
#               # reproducing the previous example, but with a matrix construction.
#               wt = c(Churned = -0.085037667, Contraction =  -0.066377404, Expansion = 0.222780538, Resurrected = 0.004862808, New = 0.365256087)
#               m = matrix(0, 5, 3, dimnames = list(names(wt),c("Decrease", "Increase", "Net")))
#               m[wt < 0, 1] <- wt[wt < 0]
#               m[wt >= 0, 2] <- wt[wt >= 0]
#               m[, 3] = cm = cumsum(wt)
#               bs <- ifelse(wt < 0, cm, c(0, cm[-5]))
#               tp = bs + abs(m[, 1]) + abs(m[, 2])
#               Waterfall(m * 100, top = tp * 100, cumulative = 3, suffix = "%", title = "Waterfall chart", decimals = 2, y.title = "Revenue change")
#
#               # Cash
#               m = matrix(0, 5, 4, dimnames = list(c("2016", "Revenue", "COGs", "Overhead", "2017"), c("2016", "Revenue", "Costs", "2017")))
#               m[1, 1] =  750000
#               m[2, 2] =  150000
#               m[3, 3] =  -20000
#               m[4, 3] =  -80000
#               m[5, 4] =  800000
#               cm = c(750000, 900000, 880000, 800000, 800000)
#               tp = c(750000, 900000, 900000, 880000, 800000)
#               Waterfall(m, top = tp, cumulative = cm, show.cumulative = FALSE, colors = c("grey", "green", "red", "black"), decimals = 0, prefix = "$")
#
#               # Reach
#               m = matrix(0, 5, 2, dimnames = list(paste("Product", LETTERS[1:5]), c("Duplicated", "Unique")))
#               cm = c(65, 80, 88, 92, 93)
#               m[, 1] = c(0, 30, 28, 27, 23)
#               m[, 2] = c(cm[1], cm[-1] - cm[-length(cm)])
#               Waterfall(m, top = cm, cumulative = cm, colors = c("grey", "blue"), decimals = 0, suffix = "%", y.title = "Reach")
#           })
#
