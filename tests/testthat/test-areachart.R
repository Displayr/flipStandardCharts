context("areaChart")
#  rm(list=ls())

for (i in 1:length(good.examples))
{
    print(Chart(y = good.examples[[i]]$Y, x = good.examples[[i]]$X, type = "Area", transpose = good.examples[[i]]$transpose, title = names(good.examples)[i], aggregate.period = good.examples[[i]]$aggregate.period, hover.include.source.value = TRUE))

    # cat(paste("Press [enter] to continue (test ", i, "/", length(good.examples), ").", sep = ""))
    # line <- readline()
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

Chart(a.matrix, type = "Area",
          transparency = 0.4,
          transpose = TRUE,
          colors = myCols,
          series.line.width = 1,
          series.line.color = myLineCols,
          series.marker.show = "none",
          title = "Area chart - BASIC style",
          y.bounds.minimum = 0,
          y.bounds.maximum = 1.01,
          y.bounds.units.major = 0.1)



##### JUNK STYLE
a.matrix <- rbind(c(50,70,70,100,80,90,90), c(50,50,30,40,70,70,90), c(30,30,50,70,40,50,50), c(0,10,10,30,20,30,10))
colnames(a.matrix) <- c("D0","D1","D2","D3","D4","D5","D6")
rownames(a.matrix) <- c("pink","green","orange","blue")
markerCols <- c(rgb(245,56,177, max=255),rgb(56,193,55, max=255),rgb(245,163,65, max=255),rgb(61,108,255, max=255))

Chart(a.matrix,
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
          transpose = TRUE,
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
col.scheme <- MakeColorGradient(y = a.matrix, transpose = TRUE, base.red = 112, base.green = 48, base.blue = 160, by = "mean")
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




































#
# ######################## AS LINE CHARTS
#
# ##### BASIC STYLE
# a.matrix <- rbind(c(0.2,0.3,0.3,0.5,0.3,0.6,0.4,0.8,0.2,0.5,0.1,0.3),c(0.3,0.5,0.1,0.3,0.1,0.2,0.1,0.4,0.1,0.2,0.1,0.4))
# rownames(a.matrix) <- c("Series1","Series2")
# colnames(a.matrix) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
#
# myCols <- c(rgb(0,176,240, max=255), rgb(197,90,17, max=255))
# myLineCols <- c(rgb(31,78,121, max=255),rgb(192,0,0, max=255))
#
# Chart(a.matrix, type = "Line",
#           transparency = 0.4,
#           colors = myCols,
#           y.tick.format = c("%","2"),
#           series.line.width = 1,
#           series.line.color = myLineCols,
#           series.marker.show = "none",
#           y.grid.show = TRUE,
#           title = "Area chart - BASIC style",
#           y.bounds.manual = TRUE,
#           y.bounds.minimum = 0,
#           y.bounds.maximum = 1.01,
#           y.bounds.units.major = 0.1)
#
# ##### JUNK STYLE
# a.matrix <- rbind(c(50,70,70,100,80,90,90), c(50,50,30,40,70,70,90), c(30,30,50,70,40,50,50), c(0,10,10,30,20,30,10))
# colnames(a.matrix) <- c("D0","D1","D2","D3","D4","D5","D6")
# rownames(a.matrix) <- c("pink","green","orange","blue")
# markerCols <- c(rgb(245,56,177, max=255),rgb(56,193,55, max=255),rgb(245,163,65, max=255),rgb(61,108,255, max=255))
#
# Chart(a.matrix,
#           legend.show = FALSE,
#           type = "Line",
#           transparency = 0.4,
#           colors = rgb(255, 255, 255, max=255),
#           plot.background.color = rgb(176, 191, 214, maxColorValue = 255),
#           background.color = rgb(176, 191, 214, maxColorValue = 255),
#           series.line.width = 2,
#           series.line.color = markerCols,
#           series.marker.color = markerCols,
#           series.marker.transparency = 0.7,
#           series.marker.show = c(100),
#           series.marker.size = 10,
#           series.marker.border.width = 3,
#           y.bounds.manual = TRUE,
#           y.bounds.minimum = 0,
#           y.bounds.maximum = 100,
#           y.bounds.units.major = 10,
#           y.line.width = 2,
#           y.line.color = rgb(141, 159, 183, maxColorValue = 255),
#           y.tick.font.color = rgb(141, 159, 183, maxColorValue = 255),
#           y.tick.font.size = 14,
#           y.tick.font.family = "PT Sans Narrow, sans-serif",
#           x.tick.font.color = rgb(255, 255, 255, maxColorValue = 255),
#           x.tick.font.size = 20,
#           x.tick.font.family = "PT Sans Narrow, sans-serif")
#
# ##### Clean style
# a.matrix <- rbind(c(0.841470985,0.89120736,0.932039086,0.963558185,0.98544973,0.997494987,0.999573603,0.99166481,0.973847631,0.946300088,0.909297427,0.863209367,0.808496404,0.745705212,0.675463181,0.598472144,0.515501372,0.42737988,0.33498815,0.239249329,0.141120008,0.041580662,0.058374143,0.157745694,0.255541102,0.350783228,0.442520443,0.529836141,0.611857891,0.687766159,0.756802495,0.818277111,0.871575772,0.916165937,0.951602074,0.977530118,0.993691004,0.999923258,0.996164609,0.982452613,0.958924275,0.925814682,0.883454656,0.832267442,0.772764488,0.705540326,0.631266638,0.550685543,0.464602179,0.373876665,0.279415498,0.182162504,0.083089403,0.0168139,0.116549205,0.215119988,0.311541364,0.404849921,0.494113351,0.578439764,0.656986599,0.72896904,0.793667864,0.850436621,0.898708096,0.937999977,0.967919672,0.988168234,0.998543345,0.998941342,0.989358247,0.969889811,0.940730557,0.902171834,0.854598908,0.798487113,0.734397098),
#                   c(0.540302306,0.453596121,0.362357754,0.267498829,0.169967143,0.070737202,0.029199522,0.128844494,0.227202095,0.323289567,0.416146837,0.504846105,0.588501117,0.666276021,0.737393716,0.801143616,0.856888753,0.904072142,0.942222341,0.970958165,0.989992497,0.99913515,0.998294776,0.98747977,0.966798193,0.936456687,0.896758416,0.848100032,0.790967712,0.725932304,0.653643621,0.574823947,0.490260821,0.400799172,0.30733287,0.210795799,0.112152527,0.012388663,0.087498983,0.186512369,0.283662185,0.377977743,0.468516671,0.554374336,0.634692876,0.708669774,0.775565879,0.834712785,0.885519517,0.927478431,0.960170287,0.983268438,0.996542097,0.999858636,0.993184919,0.976587626,0.950232592,0.914383148,0.86939749,0.8157251,0.753902254,0.684546666,0.608351315,0.526077517,0.438547328,0.346635318,0.251259843,0.153373862,0.053955421,0.046002126,0.145500034,0.243544154,0.339154861,0.431376845,0.519288654,0.602011903,0.678720047),
#                   c(0.324921345,0.279916494,0.227856881,0.170456593,0.109251961,0.045678943,0.018870567,0.083017584,0.145378206,0.204540087,0.259017159,0.307179239,0.347172513,0.37686789,0.393889282,0.395774144,0.380296607,0.345936803,0.29241662,0.221162939,0.135533951,0.040677919,0.057009965,0.150737164,0.234264983,0.302718644,0.353065915,0.38419087,0.396616833,0.392019506,0.372694029,0.341106472,0.299598458,0.250251887,0.194878007,0.135077103,0.072318523,0.008007389,0.056474473,0.11974729,0.180414543,0.237028067,0.288027375,0.331660601,0.365915574,0.388508069,0.396981824,0.38896283,0.362573713,0.316955771,0.252782003,0.172601307,0.08086104,0.016472917,0.112658058,0.201113541,0.276317022,0.334437658,0.373578553,0.393632114,0.395860367,0.382362426,0.355578701,0.317927724,0.27160674,0.218535104,0.160391304,0.098689763,0.03485583,0.029722376,0.093666196,0.155589031,0.214070037,0.267604404,0.314529716,0.352948003,0.380683664))
# colnames(a.matrix) <- c("Jan-10","Feb-10","Mar-10","Apr-10","May-10","Jun-10","Jul-10","Aug-10","Sep-10","Oct-10","Nov-10","Dec-10","Jan-11","Feb-11","Mar-11","Apr-11","May-11","Jun-11","Jul-11","Aug-11","Sep-11","Oct-11","Nov-11","Dec-11","Jan-12","Feb-12","Mar-12","Apr-12","May-12","Jun-12","Jul-12","Aug-12","Sep-12","Oct-12","Nov-12","Dec-12","Jan-13","Feb-13","Mar-13","Apr-13","May-13","Jun-13","Jul-13","Aug-13","Sep-13","Oct-13","Nov-13","Dec-13","Jan-14","Feb-14","Mar-14","Apr-14","May-14","Jun-14","Jul-14","Aug-14","Sep-14","Oct-14","Nov-14","Dec-14","Jan-15","Feb-15","Mar-15","Apr-15","May-15","Jun-15","Jul-15","Aug-15","Sep-15","Oct-15","Nov-15","Dec-15","Jan-16","Feb-16","Mar-16","Apr-16","May-16")
# rownames(a.matrix) <- c("Factory 1", "Factory 2", "Factory 3")
# itemCols <- c(rgb(46,117,182, max=255),rgb(248,203,173, max=255),rgb(79,190,70, max=255))
#
# Chart(a.matrix,
#           legend.show = TRUE,
#           type = "Line",
#           transparency = 0.4,
#           series.line.color = itemCols,
#           series.line.width = 2,
#           x.tick.angle = -45,
#           y.tick.format = c("%",1))
#
# ##### 100% Stacked area chart example
# ## Read in data
# a.matrix <- as.matrix(read.csv("consumerspending.csv", header = TRUE, row.names = 1))
# ## Figure out colours
# col.scheme <- make.color.gradient(a.matrix,112,48,160)
# ## Sort alphabetically descending
# a.matrix <- alphabetic.row.names(a.matrix)
#
# Chart(a.matrix,
#           legend.show = TRUE,
#           type = "100% Stacked Line",
#           series.line.color = col.scheme,
#           series.line.width = 2,
#           y.tick.format = c("%",1),
#           legend.sort.order = "reverse")
#
# ## Excel equivalent "Stacked AC"
# a.matrix <- as.matrix(read.csv("resources.csv", header = TRUE, row.names = 1))
# a.matrix <- alphabetic.row.names(a.matrix)
#
# Chart(a.matrix,
#           legend.show = TRUE,
#           type = "Line",
#           series.line.width = 2,
#           y.tick.format = c("n",1),
#           legend.sort.order = "reverse")
#
#
#
#
#
# ######################## AS COLUMN CHARTS
#
# ##### BASIC STYLE
# a.matrix <- rbind(c(0.2,0.3,0.3,0.5,0.3,0.6,0.4,0.8,0.2,0.5,0.1,0.3),c(0.3,0.5,0.1,0.3,0.1,0.2,0.1,0.4,0.1,0.2,0.1,0.4))
# rownames(a.matrix) <- c("Series1","Series2")
# colnames(a.matrix) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
#
# myCols <- c(rgb(0,176,240, max=255), rgb(197,90,17, max=255))
# myLineCols <- c(rgb(31,78,121, max=255),rgb(192,0,0, max=255))
#
# Chart(a.matrix, type = "Clustered Column",
#           transparency = 0.4,
#           colors = myCols,
#           y.tick.format = c("%","2"),
#           y.grid.show = TRUE,
#           title = "Area chart - BASIC style",
#           y.bounds.manual = TRUE,
#           y.bounds.minimum = 0,
#           y.bounds.maximum = 1.01,
#           y.bounds.units.major = 0.1)
#
# ##### JUNK STYLE
# a.matrix <- rbind(c(50,70,70,100,80,90,90), c(50,50,30,40,70,70,90), c(30,30,50,70,40,50,50), c(0,10,10,30,20,30,10))
# colnames(a.matrix) <- c("D0","D1","D2","D3","D4","D5","D6")
# rownames(a.matrix) <- c("pink","green","orange","blue")
# markerCols <- c(rgb(245,56,177, max=255),rgb(56,193,55, max=255),rgb(245,163,65, max=255),rgb(61,108,255, max=255))
#
# Chart(a.matrix,
#           legend.show = FALSE,
#           type = "Clustered Bar",
#           transparency = 0.4,
#           colors = rgb(255, 255, 255, max=255),
#           plot.background.color = rgb(176, 191, 214, maxColorValue = 255),
#           background.color = rgb(176, 191, 214, maxColorValue = 255),
#           series.marker.color = markerCols,
#           series.marker.border.width = 1,
#           series.marker.border.color = rgb(255, 255, 255, maxColorValue = 255),
#           y.line.width = 2,
#           y.line.color = rgb(141, 159, 183, maxColorValue = 255),
#           y.tick.font.color = rgb(141, 159, 183, maxColorValue = 255),
#           y.tick.font.size = 14,
#           y.tick.font.family = "PT Sans Narrow, sans-serif",
#           x.tick.font.color = rgb(255, 255, 255, maxColorValue = 255),
#           x.tick.font.size = 20,
#           x.tick.font.family = "PT Sans Narrow, sans-serif")
#
# ##### Clean style
# a.matrix <- rbind(c(0.841470985,0.89120736,0.932039086,0.963558185,0.98544973,0.997494987,0.999573603,0.99166481,0.973847631,0.946300088,0.909297427,0.863209367,0.808496404,0.745705212,0.675463181,0.598472144,0.515501372,0.42737988,0.33498815,0.239249329,0.141120008,0.041580662,0.058374143,0.157745694,0.255541102,0.350783228,0.442520443,0.529836141,0.611857891,0.687766159,0.756802495,0.818277111,0.871575772,0.916165937,0.951602074,0.977530118,0.993691004,0.999923258,0.996164609,0.982452613,0.958924275,0.925814682,0.883454656,0.832267442,0.772764488,0.705540326,0.631266638,0.550685543,0.464602179,0.373876665,0.279415498,0.182162504,0.083089403,0.0168139,0.116549205,0.215119988,0.311541364,0.404849921,0.494113351,0.578439764,0.656986599,0.72896904,0.793667864,0.850436621,0.898708096,0.937999977,0.967919672,0.988168234,0.998543345,0.998941342,0.989358247,0.969889811,0.940730557,0.902171834,0.854598908,0.798487113,0.734397098),
#                   c(0.540302306,0.453596121,0.362357754,0.267498829,0.169967143,0.070737202,0.029199522,0.128844494,0.227202095,0.323289567,0.416146837,0.504846105,0.588501117,0.666276021,0.737393716,0.801143616,0.856888753,0.904072142,0.942222341,0.970958165,0.989992497,0.99913515,0.998294776,0.98747977,0.966798193,0.936456687,0.896758416,0.848100032,0.790967712,0.725932304,0.653643621,0.574823947,0.490260821,0.400799172,0.30733287,0.210795799,0.112152527,0.012388663,0.087498983,0.186512369,0.283662185,0.377977743,0.468516671,0.554374336,0.634692876,0.708669774,0.775565879,0.834712785,0.885519517,0.927478431,0.960170287,0.983268438,0.996542097,0.999858636,0.993184919,0.976587626,0.950232592,0.914383148,0.86939749,0.8157251,0.753902254,0.684546666,0.608351315,0.526077517,0.438547328,0.346635318,0.251259843,0.153373862,0.053955421,0.046002126,0.145500034,0.243544154,0.339154861,0.431376845,0.519288654,0.602011903,0.678720047),
#                   c(0.324921345,0.279916494,0.227856881,0.170456593,0.109251961,0.045678943,0.018870567,0.083017584,0.145378206,0.204540087,0.259017159,0.307179239,0.347172513,0.37686789,0.393889282,0.395774144,0.380296607,0.345936803,0.29241662,0.221162939,0.135533951,0.040677919,0.057009965,0.150737164,0.234264983,0.302718644,0.353065915,0.38419087,0.396616833,0.392019506,0.372694029,0.341106472,0.299598458,0.250251887,0.194878007,0.135077103,0.072318523,0.008007389,0.056474473,0.11974729,0.180414543,0.237028067,0.288027375,0.331660601,0.365915574,0.388508069,0.396981824,0.38896283,0.362573713,0.316955771,0.252782003,0.172601307,0.08086104,0.016472917,0.112658058,0.201113541,0.276317022,0.334437658,0.373578553,0.393632114,0.395860367,0.382362426,0.355578701,0.317927724,0.27160674,0.218535104,0.160391304,0.098689763,0.03485583,0.029722376,0.093666196,0.155589031,0.214070037,0.267604404,0.314529716,0.352948003,0.380683664))
# colnames(a.matrix) <- c("Jan-10","Feb-10","Mar-10","Apr-10","May-10","Jun-10","Jul-10","Aug-10","Sep-10","Oct-10","Nov-10","Dec-10","Jan-11","Feb-11","Mar-11","Apr-11","May-11","Jun-11","Jul-11","Aug-11","Sep-11","Oct-11","Nov-11","Dec-11","Jan-12","Feb-12","Mar-12","Apr-12","May-12","Jun-12","Jul-12","Aug-12","Sep-12","Oct-12","Nov-12","Dec-12","Jan-13","Feb-13","Mar-13","Apr-13","May-13","Jun-13","Jul-13","Aug-13","Sep-13","Oct-13","Nov-13","Dec-13","Jan-14","Feb-14","Mar-14","Apr-14","May-14","Jun-14","Jul-14","Aug-14","Sep-14","Oct-14","Nov-14","Dec-14","Jan-15","Feb-15","Mar-15","Apr-15","May-15","Jun-15","Jul-15","Aug-15","Sep-15","Oct-15","Nov-15","Dec-15","Jan-16","Feb-16","Mar-16","Apr-16","May-16")
# rownames(a.matrix) <- c("Factory 1", "Factory 2", "Factory 3")
# itemCols <- c(rgb(46,117,182, max=255),rgb(248,203,173, max=255),rgb(79,190,70, max=255))
#
# Chart(a.matrix,
#           legend.show = TRUE,
#           type = "Stacked Column",
#           transparency = 0.4,
#           series.marker.color = itemCols,
#           x.tick.angle = -45,
#           y.tick.format = c("%",1))
#
# ##### 100% Stacked area chart example
# ## Read in data
# a.matrix <- as.matrix(read.csv("consumerspending.csv", header = TRUE, row.names = 1))
# ## Figure out colours
# col.scheme <- make.color.gradient(a.matrix,112,48,160)
# ## Sort alphabetically descending
# a.matrix <- alphabetic.row.names(a.matrix)
#
# Chart(a.matrix,
#           legend.show = TRUE,
#           type = "100% Stacked Column",
#           series.line.color = col.scheme,
#           series.line.width = 2,
#           y.tick.format = c("%",1),
#           legend.sort.order = "reverse")
#
# ## Excel equivalent "Stacked AC"
# a.matrix <- as.matrix(read.csv("resources.csv", header = TRUE, row.names = 1))
# a.matrix <- alphabetic.row.names(a.matrix)
#
# Chart(a.matrix,
#           legend.show = TRUE,
#           type = "100% Stacked Bar",
#           series.line.width = 2,
#           y.tick.format = c("n",1),
#           legend.sort.order = "reverse")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# # ######################## AS LINE CHARTS
# #
# # ##### BASIC STYLE
# # a.matrix <- rbind(c(0.2,0.3,0.3,0.5,0.3,0.6,0.4,0.8,0.2,0.5,0.1,0.3),c(0.3,0.5,0.1,0.3,0.1,0.2,0.1,0.4,0.1,0.2,0.1,0.4))
# # rownames(a.matrix) <- c("Series1","Series2")
# # colnames(a.matrix) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
# #
# # myCols <- c(rgb(0,176,240, max=255), rgb(197,90,17, max=255))
# # myLineCols <- c(rgb(31,78,121, max=255),rgb(192,0,0, max=255))
# #
# # Chart(a.matrix, type = "Line",
# #           transparency = 0.4,
# #           colors = myCols,
# #           y.tick.format = c("%","2"),
# #           series.line.width = 1,
# #           series.line.color = myLineCols,
# #           series.marker.show = "none",
# #           y.grid.show = TRUE,
# #           title = "Area chart - BASIC style",
# #           y.bounds.manual = TRUE,
# #           y.bounds.minimum = 0,
# #           y.bounds.maximum = 1.01,
# #           y.bounds.units.major = 0.1)
# #
# # ##### JUNK STYLE
# # a.matrix <- rbind(c(50,70,70,100,80,90,90), c(50,50,30,40,70,70,90), c(30,30,50,70,40,50,50), c(0,10,10,30,20,30,10))
# # colnames(a.matrix) <- c("D0","D1","D2","D3","D4","D5","D6")
# # rownames(a.matrix) <- c("pink","green","orange","blue")
# # markerCols <- c(rgb(245,56,177, max=255),rgb(56,193,55, max=255),rgb(245,163,65, max=255),rgb(61,108,255, max=255))
# #
# # Chart(a.matrix,
# #           legend.show = FALSE,
# #           type = "Line",
# #           transparency = 0.4,
# #           colors = rgb(255, 255, 255, max=255),
# #           plot.background.color = rgb(176, 191, 214, maxColorValue = 255),
# #           background.color = rgb(176, 191, 214, maxColorValue = 255),
# #           series.line.width = 2,
# #           series.line.color = markerCols,
# #           series.marker.color = markerCols,
# #           series.marker.transparency = 0.7,
# #           series.marker.show = c(100),
# #           series.marker.size = 10,
# #           series.marker.border.width = 3,
# #           y.bounds.manual = TRUE,
# #           y.bounds.minimum = 0,
# #           y.bounds.maximum = 100,
# #           y.bounds.units.major = 10,
# #           y.line.width = 2,
# #           y.line.color = rgb(141, 159, 183, maxColorValue = 255),
# #           y.tick.font.color = rgb(141, 159, 183, maxColorValue = 255),
# #           y.tick.font.size = 14,
# #           y.tick.font.family = "PT Sans Narrow, sans-serif",
# #           x.tick.font.color = rgb(255, 255, 255, maxColorValue = 255),
# #           x.tick.font.size = 20,
# #           x.tick.font.family = "PT Sans Narrow, sans-serif")
# #
# # ##### Clean style
# # a.matrix <- rbind(c(0.841470985,0.89120736,0.932039086,0.963558185,0.98544973,0.997494987,0.999573603,0.99166481,0.973847631,0.946300088,0.909297427,0.863209367,0.808496404,0.745705212,0.675463181,0.598472144,0.515501372,0.42737988,0.33498815,0.239249329,0.141120008,0.041580662,0.058374143,0.157745694,0.255541102,0.350783228,0.442520443,0.529836141,0.611857891,0.687766159,0.756802495,0.818277111,0.871575772,0.916165937,0.951602074,0.977530118,0.993691004,0.999923258,0.996164609,0.982452613,0.958924275,0.925814682,0.883454656,0.832267442,0.772764488,0.705540326,0.631266638,0.550685543,0.464602179,0.373876665,0.279415498,0.182162504,0.083089403,0.0168139,0.116549205,0.215119988,0.311541364,0.404849921,0.494113351,0.578439764,0.656986599,0.72896904,0.793667864,0.850436621,0.898708096,0.937999977,0.967919672,0.988168234,0.998543345,0.998941342,0.989358247,0.969889811,0.940730557,0.902171834,0.854598908,0.798487113,0.734397098),
# #                   c(0.540302306,0.453596121,0.362357754,0.267498829,0.169967143,0.070737202,0.029199522,0.128844494,0.227202095,0.323289567,0.416146837,0.504846105,0.588501117,0.666276021,0.737393716,0.801143616,0.856888753,0.904072142,0.942222341,0.970958165,0.989992497,0.99913515,0.998294776,0.98747977,0.966798193,0.936456687,0.896758416,0.848100032,0.790967712,0.725932304,0.653643621,0.574823947,0.490260821,0.400799172,0.30733287,0.210795799,0.112152527,0.012388663,0.087498983,0.186512369,0.283662185,0.377977743,0.468516671,0.554374336,0.634692876,0.708669774,0.775565879,0.834712785,0.885519517,0.927478431,0.960170287,0.983268438,0.996542097,0.999858636,0.993184919,0.976587626,0.950232592,0.914383148,0.86939749,0.8157251,0.753902254,0.684546666,0.608351315,0.526077517,0.438547328,0.346635318,0.251259843,0.153373862,0.053955421,0.046002126,0.145500034,0.243544154,0.339154861,0.431376845,0.519288654,0.602011903,0.678720047),
# #                   c(0.324921345,0.279916494,0.227856881,0.170456593,0.109251961,0.045678943,0.018870567,0.083017584,0.145378206,0.204540087,0.259017159,0.307179239,0.347172513,0.37686789,0.393889282,0.395774144,0.380296607,0.345936803,0.29241662,0.221162939,0.135533951,0.040677919,0.057009965,0.150737164,0.234264983,0.302718644,0.353065915,0.38419087,0.396616833,0.392019506,0.372694029,0.341106472,0.299598458,0.250251887,0.194878007,0.135077103,0.072318523,0.008007389,0.056474473,0.11974729,0.180414543,0.237028067,0.288027375,0.331660601,0.365915574,0.388508069,0.396981824,0.38896283,0.362573713,0.316955771,0.252782003,0.172601307,0.08086104,0.016472917,0.112658058,0.201113541,0.276317022,0.334437658,0.373578553,0.393632114,0.395860367,0.382362426,0.355578701,0.317927724,0.27160674,0.218535104,0.160391304,0.098689763,0.03485583,0.029722376,0.093666196,0.155589031,0.214070037,0.267604404,0.314529716,0.352948003,0.380683664))
# # colnames(a.matrix) <- c("Jan-10","Feb-10","Mar-10","Apr-10","May-10","Jun-10","Jul-10","Aug-10","Sep-10","Oct-10","Nov-10","Dec-10","Jan-11","Feb-11","Mar-11","Apr-11","May-11","Jun-11","Jul-11","Aug-11","Sep-11","Oct-11","Nov-11","Dec-11","Jan-12","Feb-12","Mar-12","Apr-12","May-12","Jun-12","Jul-12","Aug-12","Sep-12","Oct-12","Nov-12","Dec-12","Jan-13","Feb-13","Mar-13","Apr-13","May-13","Jun-13","Jul-13","Aug-13","Sep-13","Oct-13","Nov-13","Dec-13","Jan-14","Feb-14","Mar-14","Apr-14","May-14","Jun-14","Jul-14","Aug-14","Sep-14","Oct-14","Nov-14","Dec-14","Jan-15","Feb-15","Mar-15","Apr-15","May-15","Jun-15","Jul-15","Aug-15","Sep-15","Oct-15","Nov-15","Dec-15","Jan-16","Feb-16","Mar-16","Apr-16","May-16")
# # rownames(a.matrix) <- c("Factory 1", "Factory 2", "Factory 3")
# # itemCols <- c(rgb(46,117,182, max=255),rgb(248,203,173, max=255),rgb(79,190,70, max=255))
# #
# # Chart(a.matrix,
# #           legend.show = TRUE,
# #           type = "Line",
# #           transparency = 0.4,
# #           series.line.color = itemCols,
# #           series.line.width = 2,
# #           x.tick.angle = -45,
# #           y.tick.format = c("%",1))
# #
# # ##### 100% Stacked area chart example
# # ## Read in data
# # a.matrix <- as.matrix(read.csv("consumerspending.csv", header = TRUE, row.names = 1))
# # ## Figure out colours
# # col.scheme <- make.color.gradient(a.matrix,112,48,160)
# # ## Sort alphabetically descending
# # a.matrix <- alphabetic.row.names(a.matrix)
# #
# # Chart(a.matrix,
# #           legend.show = TRUE,
# #           type = "100% Stacked Line",
# #           series.line.color = col.scheme,
# #           series.line.width = 2,
# #           y.tick.format = c("%",1),
# #           legend.sort.order = "reverse")
# #
# # ## Excel equivalent "Stacked AC"
# # a.matrix <- as.matrix(read.csv("resources.csv", header = TRUE, row.names = 1))
# # a.matrix <- alphabetic.row.names(a.matrix)
# #
# # Chart(a.matrix,
# #           legend.show = TRUE,
# #           type = "Line",
# #           series.line.width = 2,
# #           y.tick.format = c("n",1),
# #           legend.sort.order = "reverse")
# #
# #
# #
# #
# #
# # ######################## AS COLUMN CHARTS
# #
# # ##### BASIC STYLE
# # a.matrix <- rbind(c(0.2,0.3,0.3,0.5,0.3,0.6,0.4,0.8,0.2,0.5,0.1,0.3),c(0.3,0.5,0.1,0.3,0.1,0.2,0.1,0.4,0.1,0.2,0.1,0.4))
# # rownames(a.matrix) <- c("Series1","Series2")
# # colnames(a.matrix) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
# #
# # myCols <- c(rgb(0,176,240, max=255), rgb(197,90,17, max=255))
# # myLineCols <- c(rgb(31,78,121, max=255),rgb(192,0,0, max=255))
# #
# # Chart(a.matrix, type = "Clustered Column",
# #           transparency = 0.4,
# #           colors = myCols,
# #           y.tick.format = c("%","2"),
# #           y.grid.show = TRUE,
# #           title = "Area chart - BASIC style",
# #           y.bounds.manual = TRUE,
# #           y.bounds.minimum = 0,
# #           y.bounds.maximum = 1.01,
# #           y.bounds.units.major = 0.1)
# #
# # ##### JUNK STYLE
# # a.matrix <- rbind(c(50,70,70,100,80,90,90), c(50,50,30,40,70,70,90), c(30,30,50,70,40,50,50), c(0,10,10,30,20,30,10))
# # colnames(a.matrix) <- c("D0","D1","D2","D3","D4","D5","D6")
# # rownames(a.matrix) <- c("pink","green","orange","blue")
# # markerCols <- c(rgb(245,56,177, max=255),rgb(56,193,55, max=255),rgb(245,163,65, max=255),rgb(61,108,255, max=255))
# #
# # Chart(a.matrix,
# #           legend.show = FALSE,
# #           type = "Clustered Bar",
# #           transparency = 0.4,
# #           colors = rgb(255, 255, 255, max=255),
# #           plot.background.color = rgb(176, 191, 214, maxColorValue = 255),
# #           background.color = rgb(176, 191, 214, maxColorValue = 255),
# #           series.marker.color = markerCols,
# #           series.marker.border.width = 1,
# #           series.marker.border.color = rgb(255, 255, 255, maxColorValue = 255),
# #           y.line.width = 2,
# #           y.line.color = rgb(141, 159, 183, maxColorValue = 255),
# #           y.tick.font.color = rgb(141, 159, 183, maxColorValue = 255),
# #           y.tick.font.size = 14,
# #           y.tick.font.family = "PT Sans Narrow, sans-serif",
# #           x.tick.font.color = rgb(255, 255, 255, maxColorValue = 255),
# #           x.tick.font.size = 20,
# #           x.tick.font.family = "PT Sans Narrow, sans-serif")
# #
# # ##### Clean style
# # a.matrix <- rbind(c(0.841470985,0.89120736,0.932039086,0.963558185,0.98544973,0.997494987,0.999573603,0.99166481,0.973847631,0.946300088,0.909297427,0.863209367,0.808496404,0.745705212,0.675463181,0.598472144,0.515501372,0.42737988,0.33498815,0.239249329,0.141120008,0.041580662,0.058374143,0.157745694,0.255541102,0.350783228,0.442520443,0.529836141,0.611857891,0.687766159,0.756802495,0.818277111,0.871575772,0.916165937,0.951602074,0.977530118,0.993691004,0.999923258,0.996164609,0.982452613,0.958924275,0.925814682,0.883454656,0.832267442,0.772764488,0.705540326,0.631266638,0.550685543,0.464602179,0.373876665,0.279415498,0.182162504,0.083089403,0.0168139,0.116549205,0.215119988,0.311541364,0.404849921,0.494113351,0.578439764,0.656986599,0.72896904,0.793667864,0.850436621,0.898708096,0.937999977,0.967919672,0.988168234,0.998543345,0.998941342,0.989358247,0.969889811,0.940730557,0.902171834,0.854598908,0.798487113,0.734397098),
# #                   c(0.540302306,0.453596121,0.362357754,0.267498829,0.169967143,0.070737202,0.029199522,0.128844494,0.227202095,0.323289567,0.416146837,0.504846105,0.588501117,0.666276021,0.737393716,0.801143616,0.856888753,0.904072142,0.942222341,0.970958165,0.989992497,0.99913515,0.998294776,0.98747977,0.966798193,0.936456687,0.896758416,0.848100032,0.790967712,0.725932304,0.653643621,0.574823947,0.490260821,0.400799172,0.30733287,0.210795799,0.112152527,0.012388663,0.087498983,0.186512369,0.283662185,0.377977743,0.468516671,0.554374336,0.634692876,0.708669774,0.775565879,0.834712785,0.885519517,0.927478431,0.960170287,0.983268438,0.996542097,0.999858636,0.993184919,0.976587626,0.950232592,0.914383148,0.86939749,0.8157251,0.753902254,0.684546666,0.608351315,0.526077517,0.438547328,0.346635318,0.251259843,0.153373862,0.053955421,0.046002126,0.145500034,0.243544154,0.339154861,0.431376845,0.519288654,0.602011903,0.678720047),
# #                   c(0.324921345,0.279916494,0.227856881,0.170456593,0.109251961,0.045678943,0.018870567,0.083017584,0.145378206,0.204540087,0.259017159,0.307179239,0.347172513,0.37686789,0.393889282,0.395774144,0.380296607,0.345936803,0.29241662,0.221162939,0.135533951,0.040677919,0.057009965,0.150737164,0.234264983,0.302718644,0.353065915,0.38419087,0.396616833,0.392019506,0.372694029,0.341106472,0.299598458,0.250251887,0.194878007,0.135077103,0.072318523,0.008007389,0.056474473,0.11974729,0.180414543,0.237028067,0.288027375,0.331660601,0.365915574,0.388508069,0.396981824,0.38896283,0.362573713,0.316955771,0.252782003,0.172601307,0.08086104,0.016472917,0.112658058,0.201113541,0.276317022,0.334437658,0.373578553,0.393632114,0.395860367,0.382362426,0.355578701,0.317927724,0.27160674,0.218535104,0.160391304,0.098689763,0.03485583,0.029722376,0.093666196,0.155589031,0.214070037,0.267604404,0.314529716,0.352948003,0.380683664))
# # colnames(a.matrix) <- c("Jan-10","Feb-10","Mar-10","Apr-10","May-10","Jun-10","Jul-10","Aug-10","Sep-10","Oct-10","Nov-10","Dec-10","Jan-11","Feb-11","Mar-11","Apr-11","May-11","Jun-11","Jul-11","Aug-11","Sep-11","Oct-11","Nov-11","Dec-11","Jan-12","Feb-12","Mar-12","Apr-12","May-12","Jun-12","Jul-12","Aug-12","Sep-12","Oct-12","Nov-12","Dec-12","Jan-13","Feb-13","Mar-13","Apr-13","May-13","Jun-13","Jul-13","Aug-13","Sep-13","Oct-13","Nov-13","Dec-13","Jan-14","Feb-14","Mar-14","Apr-14","May-14","Jun-14","Jul-14","Aug-14","Sep-14","Oct-14","Nov-14","Dec-14","Jan-15","Feb-15","Mar-15","Apr-15","May-15","Jun-15","Jul-15","Aug-15","Sep-15","Oct-15","Nov-15","Dec-15","Jan-16","Feb-16","Mar-16","Apr-16","May-16")
# # rownames(a.matrix) <- c("Factory 1", "Factory 2", "Factory 3")
# # itemCols <- c(rgb(46,117,182, max=255),rgb(248,203,173, max=255),rgb(79,190,70, max=255))
# #
# # Chart(a.matrix,
# #           legend.show = TRUE,
# #           type = "Stacked Column",
# #           transparency = 0.4,
# #           series.marker.color = itemCols,
# #           x.tick.angle = -45,
# #           y.tick.format = c("%",1))
# #
# # ##### 100% Stacked area chart example
# # ## Read in data
# # a.matrix <- as.matrix(read.csv("consumerspending.csv", header = TRUE, row.names = 1))
# # ## Figure out colours
# # col.scheme <- make.color.gradient(a.matrix,112,48,160)
# # ## Sort alphabetically descending
# # a.matrix <- alphabetic.row.names(a.matrix)
# #
# # Chart(a.matrix,
# #           legend.show = TRUE,
# #           type = "100% Stacked Column",
# #           series.line.color = col.scheme,
# #           series.line.width = 2,
# #           y.tick.format = c("%",1),
# #           legend.sort.order = "reverse")
# #
# # ## Excel equivalent "Stacked AC"
# # a.matrix <- as.matrix(read.csv("resources.csv", header = TRUE, row.names = 1))
# # a.matrix <- alphabetic.row.names(a.matrix)
# #
# # Chart(a.matrix,
# #           legend.show = TRUE,
# #           type = "100% Stacked Bar",
# #           series.line.width = 2,
# #           y.tick.format = c("n",1),
# #           legend.sort.order = "reverse")
#
#
#
#
#
#
#
#
#
# #
# #
# #
# # ################## Additional bar and column examples based on other data types.
# #
# # ## Unnamed vector to matrix  - Doesn't make sense as an Area Chart...
# # Chart(c(1,2,2,6,4),
# #           row.labels = c("a","b","c","d","e"),
# #           legend.show = TRUE,
# #           type = "Column")
# #
# # ## Named vector to matrix, unique names   - Also doesn't make sense as an Area Chart...
# # Chart(c(a = 1, b = 2, c = 2, d = 6, e = 4),
# #           legend.show = TRUE,
# #           type = "Column")
# #
# # ## Single factor variable
# # a <- as.factor(rep(c(1,4,12,5,3),10))
# # Chart(a,
# #           legend.show = TRUE,
# #           type = "Bar")
# #
# # ## List of two factor variables   - Also not a sensible Area Chart...?
# # b <- as.factor(rep(c(1,4,12,5,3),10))
# # levels(b) <- c("Celery","Apples","Walnuts","Grapes","Mayonnaise")
# # factor.list <- list(a, b)
# # Chart(factor.list,
# #           legend.show = TRUE,
# #           transparency = 0.4,
# #           type = "Stacked Column")
# #
# #
# #
# # ################### ALL DATA CONVERSION EXAMPLES
# #
# # ## Unnamed vector to matrix
# # y <- c(1,2,2,6,4)
# # DataToMatrix(data.input = y,
# #              row.labels = c("a","b","c","d","e"))
# #
# # ############ ERROR - Counts number of named items, not the sums of the named items.
# # ## Named vector to matrix, non-unique names
# # x <- c(a = 1, b = 2, c = 2, d = 6, b = 4)
# # DataToMatrix(data.input = x)
# #
# # ## Named vector to matrix, unique names
# # z <- c(a = 1, b = 2, c = 2, d = 6, e = 4)
# # DataToMatrix(data.input = z)
# #
# # ## Single factor variable
# # a <- as.factor(rep(c(1,4,12,5,3),10))
# # levels(a) <- c("Celery","Apples","Walnuts","Grapes","Mayonnaise")
# # DataToMatrix(data.input = a)
# #
# # ## List of two factor variables
# # b <- as.factor(rep(c(1,4,12,5,3),10))
# # levels(b) <- c("Celery","Apples","Walnuts","Grapes","Mayonnaise")
# # factor.list <- list(a, b)
# # DataToMatrix(data.input = factor.list)
# #
# # ## MR set from data frame
# # df <- as.data.frame(read.csv("pastaSaucesFinal.csv", header = TRUE, row.names = 72))
# # var.names <- c("buy_a","buy_b","buy_c","buy_d","buy_e","buy_f","buy_g","buy_h","buy_i")
# # DataToMatrix(data.input = df,
# #              variable.names.vector = var.names)
# #
# # ## MR set from data frame with named rows and columns
# # row.labels <- c("a","b","c","d","e","f","g","h","i")
# # column.labels <- c("Not selected","Selected")
# # DataToMatrix(data.input = df,
# #              variable.names.vector = var.names,
# #              row.labels = row.labels,
# #              column.labels = column.labels)
# #
# # ## MR set with categorical data structure (e.g. var1 = [0, 1]; var2 = [0,2] ...)
# # var.names <- c("retail_a",	"retail_b", "retail_c", "retail_d", "retail_e", "retail_f", "retail_g")
# # row.labels <- c("A shop", "Another shop", "My Store", "My outlet", "A Discount Department Store", "Myer", "Selfriges")
# # column.labels <- c("Not visited", "Visited")
# # DataToMatrix(data.input = df,
# #              variable.names.vector = var.names,
# #              row.labels = row.labels,
# #              column.labels = column.labels)
# #
# # ## Pick One grid (no specified labelling)
# # var.names <- c("attr1_a","attr1_b","attr1_c")
# # DataToMatrix(data.input = df,
# #              variable.names.vector = var.names)
# #
# # ## Pick One grid (labelled)
# # row.labels <- c("Attribute 1", "Attribute 2", "Attribute 3")
# # column.labels <- c("Rated 1", "Rated 2", "Rated 3", "Rated 4", "Rated 5", "Rated 6", "Rated 7")
# # DataToMatrix(data.input = df,
# #              variable.names.vector = var.names,
# #              row.labels = row.labels,
# #              column.labels = column.labels)
