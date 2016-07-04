#
# series.count <- nrow(junk.data)
# x.items <- 0:(ncol(junk.data) - 1)
# spacing <- 1 / series.count
# series.positions <- NULL
#
# for (i in 1:positions)
# {
#     series.positions <- c(series.positions, i * spacing, -1 * (i * spacing))
# }
# series.positions <- sort(c(series.positions, 0))
#
# data.annotations <- list()
#
# for (a in x.items)
# {
#     x.offsets <- series.positions + a
#
#     for (b in 1:length(x.offsets))
#     {
#         data.point <- junk.data[b, a + 1]
#
#         if (data.point < 0)
#             ypos <- data.point + 5
#         else
#             ypos <- data.point - 5
#
#         data.annotations[[length(data.annotations) + 1]] <- list(x = x.offsets[b],
#                                                      y = ypos,
#                                                      text = data.point,
#                                                      showarrow = FALSE)
#     }
# }
#
# ## IF range is not specified
#
# range <- abs(min(junk.data)) + abs(max(junk.data))
#
#
#
# ## IF range is specified
#
#
#
#
#
#
# ## Junk chart
# junk.data <- as.matrix(rbind(Blue = c(-32, 58, 35, -12, 35, -16, 79),
#                              Yellow = c(64, -23, 10, -8, 43, 78, 30),
#                              Red = c(22, 64, 48, 10, -7, 22, 4)))
# colnames(junk.data) <- c("29-Dec","30-Dec","31-Dec","01-Jan","02-Jan","03-Jan","04-Jan")
# myLineCols <- c(rgb(68, 112, 236, max=255), rgb(255, 192, 0, max=255), rgb(210, 0, 0, max=255))
#
#
# p2 <- plot_ly()
#
# p2 <- config(displayModeBar = FALSE)
#
# for (i in 1:nrow(junk.data))
# {
#     p2 <- add_trace(p2,
#                     y = junk.data[i, ],
#                     x = paste(" ",colnames(junk.data), sep = ""),
#                     evaluate = TRUE,
#                     name = rownames(junk.data)[i],
#                     type = "bar",
#                     marker = list(
#                         color = plotly::toRGB(myLineCols[i], 0),
#                         line = list(
#                             width = 3,
#                             color = plotly::toRGB(myLineCols[i], 1)
#                         )
#                     ),
#                     orientation = "v"
#     )
#
#
# }
#
# p2 <- layout(p2,
#              font = list(
#                  color = plotly::toRGB(rgb(192, 0, 0, maxColorValue = 255))
#              ),
#              title = "<b>Sample Chart</b>",
#              legend = list(
#                  font = list(
#                      color = plotly::toRGB(rgb(192, 192, 192, maxColorValue = 255)),
#                      size = 14
#                  )
#              ),
#              plot_bgcolor = plotly::toRGB(rgb(64, 64, 64, maxColorValue = 255)),
#              paper_bgcolor = plotly::toRGB(rgb(64, 64, 64, maxColorValue = 255)),
#              bargap = 0,
#              bargroupgap = 0.15,
#              yaxis = list(
#                  gridcolor = plotly::toRGB(rgb(104, 104, 104, maxColorValue = 255)),
#                  tickfont = list(
#                      color = plotly::toRGB(rgb(192, 192, 192, maxColorValue = 255))
#                  ),
#                  range = list(-41, 81),
#                  tickformat = "$"
#              ),
#              xaxis = list(
#                  tickfont = list(
#                      color = plotly::toRGB(rgb(192, 192, 192, maxColorValue = 255))
#                  )
#              ),
#              annotations = data.annotations,
#              hovermode = "closest")
#
# p2
#
#
#
# p2 <- add_trace(p2,
#                 name = "Blue",
#                 type = "bar",
#                 x = c("29-Dec","30-Dec","31-Dec","01-Jan","02-Jan","03-Jan","04-Jan"),
#                 y = c(-32, 58, 35, -12, 35, -16, 79),
#                 marker = list(
#                     color = plotly::toRGB(myLineCols[1], 0),
#                     line = list(
#                         width = 3,
#                         color = plotly::toRGB(myLineCols[1], 1)
#                     )
#                 ),
#                 mode = "lines+markers+text",
#                 orientation = "v")
#
# p2 <- add_trace(p2,
#                 name = "Yellow",
#                 type = "bar",
#                 x = c("29-Dec","30-Dec","31-Dec","01-Jan","02-Jan","03-Jan","04-Jan"),
#                 y = c(64, -23, 10, -8, 43, 78, 30),
#                 marker = list(
#                     color = plotly::toRGB(myLineCols[2], 0),
#                     line = list(
#                         width = 3,
#                         color = plotly::toRGB(myLineCols[2], 1)
#                     )
#                 ),
#                 orientation = "v")
#
# p2 <- add_trace(p2,
#                 name = "Red",
#                 type = "bar",
#                 x = c("29-Dec","30-Dec","31-Dec","01-Jan","02-Jan","03-Jan","04-Jan"),
#                 y = c(22, 64, 48, 10, -7, 22, 4),
#                 text = c("$22", "$64", "$48", "$10", "-$7", "$22", "$4"),
#                 marker = list(
#                     color = plotly::toRGB(myLineCols[3], 0),
#                     line = list(
#                         width = 3,
#                         color = plotly::toRGB(myLineCols[3], 1)
#                     )
#                 ),
#                 orientation = "v")
#
#
# a <- list(x = -.33,
#           y = -32,
#           text = "-32",
#           showarrow = FALSE
# )
#
# b <- list(
#     x = 6.33,
#     y = -32,
#     text = "-32",
#     showarrow = FALSE
# )
#
# my.annotations <- list()
# my.annotations <- list(a, b)
#
#
# # (1 / ((7 * 3) - 2)) / 2
