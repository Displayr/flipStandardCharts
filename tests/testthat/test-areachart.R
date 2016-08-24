context("areaChart")

library(png)
library(plotly)

for (i in 1:length(qTab.examples))
{
    image.test <- FALSE

    ## Run image and chart output
    print(Chart(y = qTab.examples[[i]]$y, type = qTab.examples[[i]]$type, transpose = qTab.examples[[i]]$transpose, title = attr(qTab.examples[[i]]$y, "name"), y.title = qTab.examples[[i]]$y.title))

    test.1 <- as.list(plotly_build(Chart(y = qTab.examples[[i]]$y, type = qTab.examples[[i]]$type, transpose = qTab.examples[[i]]$transpose, title = attr(qTab.examples[[i]]$y, "name")))[[1]])
    test.2 <- as.list(plotly_build(Chart(y = qTab.examples[[i]]$y, type = qTab.examples[[i]]$type, transpose = qTab.examples[[i]]$transpose, title = attr(qTab.examples[[i]]$y, "name")))[[2]])

    approved.1 <- get(paste("ex", i, ".1", sep=""))
    approved.2 <- get(paste("ex", i, ".2", sep=""))

    for (a in 1:length(test.1))
    {
        if(length(test.1[[a]]) > 1)
        {
            for (b in 1:length(test.1[[a]]))
            {
                if (length(test.1[[a]][[b]]) > 1)
                {
                    for (c in 1:length(test.1[[a]][[b]]))
                        if (length(test.1[[a]][[b]][[c]]) > 1)
                        {
                            for (d in 1:length(test.1[[a]][[b]][[c]]))
                            {
                                if (is.null(test.1[[a]][[b]][[c]][[d]]) || is.na(test.1[[a]][[b]][[c]][[d]]) || length(test.1[[a]][[b]][[c]][[d]]) == 0)
                                    test.1[[a]][[b]][[c]][[d]] <- ""

                                if (is.null(approved.1[[a]][[b]][[c]][[d]]) || is.na(approved.1[[a]][[b]][[c]][[d]]) || length(approved.1[[a]][[b]][[c]][[d]]) == 0)
                                    approved.1[[a]][[b]][[c]][[d]] <- ""

                                # if (!is.null(test.1[[a]][[b]][[c]][[d]]) && !is.na(test.1[[a]][[b]][[c]][[d]]))
                                # {
                                    #print(paste("in test (", a, " ", b, " ", c, " ", d, "): ", test.1[[a]][[b]][[c]][[d]], " / in approved: ", approved.1[[a]][[b]][[c]][[d]], sep = ""))
                                    test_that(paste("in test (", a, " ", b, " ", c, " ", d,"): ", test.1[[a]][[b]][[c]][[d]], " / in approved: ", approved.1[[a]][[b]][[c]][[d]], sep = ""), {expect_that(test.1[[a]][[b]][[c]][[d]] == approved.1[[a]][[b]][[c]][[d]], is_true())})
                                # }
                            }
                        } else {
                            if (is.null(test.1[[a]][[b]][[c]]) || is.na(test.1[[a]][[b]][[c]]) || length(test.1[[a]][[b]][[c]]) == 0)
                                test.1[[a]][[b]][[c]] <- ""

                            if (is.null(approved.1[[a]][[b]][[c]]) || is.na(approved.1[[a]][[b]][[c]]) || length(approved.1[[a]][[b]][[c]]) == 0)
                                approved.1[[a]][[b]][[c]] <- ""

                            # if (!is.null(test.1[[a]][[b]][[c]]) && !is.na(test.1[[a]][[b]][[c]]))
                            # {
                                #print(paste("in test (", a, " ", b, " ", c, "): ", test.1[[a]][[b]][[c]], " / in approved: ", approved.1[[a]][[b]][[c]], sep = ""))
                                test_that(paste("in test (", a, " ", b, " ", c, "): ", test.1[[a]][[b]][[c]], " / in approved: ", approved.1[[a]][[b]][[c]], sep = ""), {expect_that(test.1[[a]][[b]][[c]] == approved.1[[a]][[b]][[c]], is_true())})
                            # }
                        }
                } else {
                    if (is.null(test.1[[a]][[b]]) || is.na(test.1[[a]][[b]]) || length(test.1[[a]][[b]]) == 0)
                        test.1[[a]][[b]] <- ""

                    if (is.null(approved.1[[a]][[b]]) || is.na(approved.1[[a]][[b]]) || length(approved.1[[a]][[b]]) == 0)
                        approved.1[[a]][[b]] <- ""

                    # if (!is.null(test.1[[a]][[b]]) && !is.na(test.1[[a]][[b]]))
                    # {
                        #print(paste("in test (", a, " ", b, "): ", test.1[[a]][[b]], " / in approved: ", approved.1[[a]][[b]], sep = ""))
                        test_that(paste("in test (", a, " ", b, "): ", test.1[[a]][[b]], " / in approved: ", approved.1[[a]][[b]], sep = ""), {expect_that(test.1[[a]][[b]] == approved.1[[a]][[b]], is_true())})
                    # }
                }
            }
        } else {
            if (is.null(test.1[[a]]) || is.na(test.1[[a]]) || length(test.1[[a]]) == 0)
                test.2[[a]] <- ""

            if (is.null(approved.1[[a]]) || is.na(approved.1[[a]]) || length(approved.1[[a]]) == 0)
                approved.1[[a]] <- ""

            test_that(paste("in test (", a, "): ", test.1[[a]], " / in approved: ", approved.1[[a]], sep = ""), {expect_that(test.1[[a]] == approved.1[[a]], is_true())})
        }
    }

    for (a in 1:length(test.2))
    {
        if(length(test.2[[a]]) > 1)
        {
            for (b in 1:length(test.2[[a]]))
            {
                if (length(test.2[[a]][[b]]) > 1)
                {
                    for (c in 1:length(test.2[[a]][[b]]))
                        if (length(test.2[[a]][[b]][[c]]) > 1)
                        {
                            for (d in 1:length(test.2[[a]][[b]][[c]]))
                            {
                                if (is.null(test.2[[a]][[b]][[c]][[d]]) || is.na(test.2[[a]][[b]][[c]][[d]]) || length(test.2[[a]][[b]][[c]][[d]]) == 0)
                                    test.2[[a]][[b]][[c]][[d]] <- ""

                                if (is.null(approved.2[[a]][[b]][[c]][[d]]) || is.na(approved.2[[a]][[b]][[c]][[d]]) || length(approved.2[[a]][[b]][[c]][[d]]) == 0)
                                    approved.2[[a]][[b]][[c]][[d]] <- ""

                                # if (!is.null(test.2[[a]][[b]][[c]][[d]]) && !is.na(test.2[[a]][[b]][[c]][[d]]))
                                # {
                                    #print(paste("in test (", a, " ", b, " ", c, " ", d, "): ", test.2[[a]][[b]][[c]][[d]], " / in approved: ", approved.2[[a]][[b]][[c]][[d]], sep = ""))
                                    test_that(paste("in test (", a, " ", b, " ", c, " ", d,"): ", test.2[[a]][[b]][[c]][[d]], " / in approved: ", approved.2[[a]][[b]][[c]][[d]], sep = ""), {expect_that(test.2[[a]][[b]][[c]][[d]] == approved.2[[a]][[b]][[c]][[d]], is_true())})
                                # }
                            }
                        } else {
                            if (is.null(test.2[[a]][[b]][[c]]) || is.na(test.2[[a]][[b]][[c]]) || length(test.2[[a]][[b]][[c]]) == 0)
                                test.2[[a]][[b]][[c]] <- ""

                            if (is.null(approved.2[[a]][[b]][[c]]) || is.na(approved.2[[a]][[b]][[c]]) || length(approved.2[[a]][[b]][[c]]) == 0)
                                approved.2[[a]][[b]][[c]] <- ""

                            # if (!is.null(test.2[[a]][[b]][[c]]) && !is.na(test.2[[a]][[b]][[c]]))
                            # {
                                #print(paste("in test (", a, " ", b, " ", c, "): ", test.2[[a]][[b]][[c]], " / in approved: ", approved.2[[a]][[b]][[c]], sep = ""))
                                test_that(paste("in test (", a, " ", b, " ", c, "): ", test.2[[a]][[b]][[c]], " / in approved: ", approved.2[[a]][[b]][[c]], sep = ""), {expect_that(test.2[[a]][[b]][[c]] == approved.2[[a]][[b]][[c]], is_true())})
                            # }
                        }
                } else {
                    if (is.null(test.2[[a]][[b]]) || is.na(test.2[[a]][[b]]) || length(test.2[[a]][[b]]) == 0)
                        test.2[[a]][[b]] <- ""

                    if (is.null(approved.2[[a]][[b]]) || is.na(approved.2[[a]][[b]]) || length(approved.2[[a]][[b]]) == 0)
                        approved.2[[a]][[b]] <- ""

                    # if (!is.null(test.2[[a]][[b]]) && !is.na(test.2[[a]][[b]]))
                    # {
                        #print(paste("in test (", a, " ", b, "): ", test.2[[a]][[b]], " / in approved: ", approved.2[[a]][[b]], sep = ""))
                        test_that(paste("in test (", a, " ", b, "): ", test.2[[a]][[b]], " / in approved: ", approved.2[[a]][[b]], sep = ""), {expect_that(test.2[[a]][[b]] == approved.2[[a]][[b]], is_true())})
                    # }
                }
            }
        } else {
            if (is.null(test.2[[a]]) || is.na(test.2[[a]]) || length(test.2[[a]]) == 0)
                test.2[[a]] <- ""

            if (is.null(approved.2[[a]]) || is.na(approved.2[[a]]) || length(approved.2[[a]]) == 0)
                approved.2[[a]] <- ""

            test_that(paste("in test (", a, "): ", test.2[[a]], " / in approved: ", approved.2[[a]], sep = ""), {expect_that(test.2[[a]] == approved.2[[a]], is_true())})
        }
    }
}

for (i in 1:length(qTab.bad.examples))
{
    test_that(paste(names(qTab.bad.examples[i])), {expect_error(
        Chart(y = qTab.bad.examples[[i]]$y,
              type = qTab.bad.examples[[i]]$type,
              transpose = qTab.bad.examples[[i]]$transpose,
              title = attr(qTab.bad.examples[[i]]$y, "name"),
              y.title = qTab.bad.examples[[i]]$y.title))})

}

#############################################################################
#############################################################################
##                            AREA CHART EXAMPLES                          ##
#############################################################################
#############################################################################

##### BASIC STYLE
a.matrix <- rbind(c(0.2,0.3,0.3,0.5,0.3,0.6,0.4,0.8,0.2,0.5,0.1,0.3),c(0.3,0.5,0.1,0.3,0.1,0.2,0.1,0.4,0.1,0.2,0.1,0.4))
rownames(a.matrix) <- c("Series1","Series2")
colnames(a.matrix) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

myCols <- c(rgb(0,176,240, max=255), rgb(197,90,17, max=255))
myLineCols <- c(rgb(31,78,121, max=255),rgb(192,0,0, max=255))

myPlot <- Chart(a.matrix, type = "Area",
          transparency = 0.4,
          colors = myCols,
          series.line.width = 1,
          series.line.color = myLineCols,
          series.marker.show = "none",
          title = "Area chart - BASIC style",
          y.bounds.minimum = 0,
          y.bounds.maximum = 1.01,
          y.bounds.units.major = 0.1)

myPlot

##### JUNK STYLE
a.matrix <- rbind(c(50,70,70,100,80,90,90), c(50,50,30,40,70,70,90), c(30,30,50,70,40,50,50), c(0,10,10,30,20,30,10))
colnames(a.matrix) <- c("D0","D1","D2","D3","D4","D5","D6")
rownames(a.matrix) <- c("pink","green","orange","blue")
markerCols <- c(rgb(245,56,177, max=255),rgb(56,193,55, max=255),rgb(245,163,65, max=255),rgb(61,108,255, max=255))

yourPlot <- Chart(a.matrix,
          legend.show = FALSE,
          type = "Area",
          transparency = 0.4,
          transpose = TRUE,
          colors = rgb(255, 255, 255, max=255),
          plot.fill.color = rgb(176, 191, 214, maxColorValue = 255),
          chart.fill.color = rgb(176, 191, 214, maxColorValue = 255),
          series.line.width = 2,
          series.line.color = rgb(255, 255, 255, maxColorValue = 255),
          series.marker.color = markerCols,
          series.marker.transparency = 0.7,
          series.marker.show = c(100),
          series.marker.size = 10,
          series.marker.border.width = 3,
          y.bounds.minimum = 0,
          y.bounds.maximum = 100,
          y.bounds.units.major = 10,
          y.line.width = 2,
          y.line.color = rgb(141, 159, 183, maxColorValue = 255),
          y.grid.width = 0,
          y.tick.font.color = rgb(141, 159, 183, maxColorValue = 255),
          y.tick.font.size = 14,
          y.tick.font.family = "PT Sans Narrow, sans-serif",
          x.tick.font.color = rgb(255, 255, 255, maxColorValue = 255),
          x.tick.font.size = 20,
          x.tick.font.family = "PT Sans Narrow, sans-serif")

##### Clean style
a.matrix <- rbind(c(0.841470985,0.89120736,0.932039086,0.963558185,0.98544973,0.997494987,0.999573603,0.99166481,0.973847631,0.946300088,0.909297427,0.863209367,0.808496404,0.745705212,0.675463181,0.598472144,0.515501372,0.42737988,0.33498815,0.239249329,0.141120008,0.041580662,0.058374143,0.157745694,0.255541102,0.350783228,0.442520443,0.529836141,0.611857891,0.687766159,0.756802495,0.818277111,0.871575772,0.916165937,0.951602074,0.977530118,0.993691004,0.999923258,0.996164609,0.982452613,0.958924275,0.925814682,0.883454656,0.832267442,0.772764488,0.705540326,0.631266638,0.550685543,0.464602179,0.373876665,0.279415498,0.182162504,0.083089403,0.0168139,0.116549205,0.215119988,0.311541364,0.404849921,0.494113351,0.578439764,0.656986599,0.72896904,0.793667864,0.850436621,0.898708096,0.937999977,0.967919672,0.988168234,0.998543345,0.998941342,0.989358247,0.969889811,0.940730557,0.902171834,0.854598908,0.798487113,0.734397098),
                  c(0.540302306,0.453596121,0.362357754,0.267498829,0.169967143,0.070737202,0.029199522,0.128844494,0.227202095,0.323289567,0.416146837,0.504846105,0.588501117,0.666276021,0.737393716,0.801143616,0.856888753,0.904072142,0.942222341,0.970958165,0.989992497,0.99913515,0.998294776,0.98747977,0.966798193,0.936456687,0.896758416,0.848100032,0.790967712,0.725932304,0.653643621,0.574823947,0.490260821,0.400799172,0.30733287,0.210795799,0.112152527,0.012388663,0.087498983,0.186512369,0.283662185,0.377977743,0.468516671,0.554374336,0.634692876,0.708669774,0.775565879,0.834712785,0.885519517,0.927478431,0.960170287,0.983268438,0.996542097,0.999858636,0.993184919,0.976587626,0.950232592,0.914383148,0.86939749,0.8157251,0.753902254,0.684546666,0.608351315,0.526077517,0.438547328,0.346635318,0.251259843,0.153373862,0.053955421,0.046002126,0.145500034,0.243544154,0.339154861,0.431376845,0.519288654,0.602011903,0.678720047),
                  c(0.324921345,0.279916494,0.227856881,0.170456593,0.109251961,0.045678943,0.018870567,0.083017584,0.145378206,0.204540087,0.259017159,0.307179239,0.347172513,0.37686789,0.393889282,0.395774144,0.380296607,0.345936803,0.29241662,0.221162939,0.135533951,0.040677919,0.057009965,0.150737164,0.234264983,0.302718644,0.353065915,0.38419087,0.396616833,0.392019506,0.372694029,0.341106472,0.299598458,0.250251887,0.194878007,0.135077103,0.072318523,0.008007389,0.056474473,0.11974729,0.180414543,0.237028067,0.288027375,0.331660601,0.365915574,0.388508069,0.396981824,0.38896283,0.362573713,0.316955771,0.252782003,0.172601307,0.08086104,0.016472917,0.112658058,0.201113541,0.276317022,0.334437658,0.373578553,0.393632114,0.395860367,0.382362426,0.355578701,0.317927724,0.27160674,0.218535104,0.160391304,0.098689763,0.03485583,0.029722376,0.093666196,0.155589031,0.214070037,0.267604404,0.314529716,0.352948003,0.380683664))
colnames(a.matrix) <- c("Jan-10","Feb-10","Mar-10","Apr-10","May-10","Jun-10","Jul-10","Aug-10","Sep-10","Oct-10","Nov-10","Dec-10","Jan-11","Feb-11","Mar-11","Apr-11","May-11","Jun-11","Jul-11","Aug-11","Sep-11","Oct-11","Nov-11","Dec-11","Jan-12","Feb-12","Mar-12","Apr-12","May-12","Jun-12","Jul-12","Aug-12","Sep-12","Oct-12","Nov-12","Dec-12","Jan-13","Feb-13","Mar-13","Apr-13","May-13","Jun-13","Jul-13","Aug-13","Sep-13","Oct-13","Nov-13","Dec-13","Jan-14","Feb-14","Mar-14","Apr-14","May-14","Jun-14","Jul-14","Aug-14","Sep-14","Oct-14","Nov-14","Dec-14","Jan-15","Feb-15","Mar-15","Apr-15","May-15","Jun-15","Jul-15","Aug-15","Sep-15","Oct-15","Nov-15","Dec-15","Jan-16","Feb-16","Mar-16","Apr-16","May-16")
rownames(a.matrix) <- c("Factory A", "Factory B", "Factory C")
itemCols <- c(rgb(46,117,182, max=255),rgb(248,203,173, max=255),rgb(79,190,70, max=255))

Chart(a.matrix,
          legend.show = TRUE,
          type = "Area",
          transparency = 0.4,
          colors = itemCols,
          series.line.width = 0,
          x.tick.angle = -45,
          # x.bounds.minimum = 0,
          # x.bounds.maximum = 1,
          # x.bounds.units.major = .1,
          y.tick.format.manual = "%",
          y.hovertext.format.manual = "%",
          x.tick.frequency = 5,
          hover.mode = "x")

#### 100% Stacked area chart example
# Read in data
a.matrix <- as.matrix(read.csv("C:/R/consumerspending.csv", header = TRUE, row.names = 1))
## Figure out colours
col.scheme <- MakeColorGradient(chart.matrix = a.matrix, base.red = 112, base.green = 48, base.blue = 160, by = "mean")
## Sort alphabetically descending
a.matrix <- AlphabeticRowNames(a.matrix)

Chart(a.matrix,
          legend.show = TRUE,
          type = "100% Stacked Area",
          colors = col.scheme,
          transpose = TRUE,
          transparency = 1,
          legend.ascending = FALSE,
          y.position = "right",
          y.hovertext.format.manual = "%",
          y.tick.format.manual = "%")

# Excel equivalent "Stacked AC"
a.matrix <- as.matrix(read.csv("C:/R/resources.csv", header = TRUE, row.names = 1))

Chart(a.matrix,
          legend.show = TRUE,
          type = "Stacked Area",
          transpose = TRUE,
          transparency = 1,
          legend.ascending = FALSE,
          hover.mode = "x",
          y.tick.decimals = 2,
          hover.include.source.value = TRUE,
          hover.include.source.value.prefix = "Staff count:",
          x.hovertext.decimals = 0,
          y.hovertext.decimals = 0)



### Code for running unit tests off images / only works if you have API access to plotly. :(


    # if (image.test)
    # {
    #     image.location <- getwd()
    #
    #     ## Load existing image
    #     existing.image <- as.raster(readPNG(paste(image.location, "/images/", attr(qTab.examples[[i]]$y, "name"), ".png", sep = "")))
    #
    #     ## Load new image
    #     new.image <- as.raster(readPNG(paste(image.location, "/", attr(qTab.examples[[i]]$y, "name"), ".png", sep = "")))
    #
    #     ## Test_that the images match, in the case of good examples
    #     test_that(paste(names(qTab.examples[i])), {expect_that(all(existing.image == new.image), is_true())})
    #
    #     # cat(paste("Press [enter] to continue (test ", i, "/", length(good.examples), ").", sep = ""))
    #     # line <- readline()
    # }
