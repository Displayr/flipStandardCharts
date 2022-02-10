#' #' Waterfall
#' #'
#' #' Creates a waterfall chart showing the source(s) of change in sales for one period, relative to the previous period.
#' #' @inherit Column
#' #' @param x A named vector or a matrix.
#' #' @param cumulative If \code{y} is a \code{vector}, this argument is ignored. Otherwise,
#' #' it can be a \code{\link{vector}} of length \code{length(y)}, or, an \code{integer} indicating the column
#' #' in \code{y} containing the cumulative values. It defaults to \code{1}.
#' #' @param top The position from which the blocks "hang" (i.e., the top of each column). Only used if
#' #' \code{y} is a \code{\link{matrix}}. If not supplied, it is assumed to be the same as \code{cumulative}.
#' #' @param colors The colors of the blocks, defaulting to red for negative values and blue for positive values.
#' #' Where \code{y} is a \code{\link{vector}}, negative values get the first color and positives the second, unless the length
#' #' of \code{colors} is the same as \code{y}, in which case each color represents each category of \code{y} if \code{y} is a \code{vector},
#' #' and each column otherwise.
#' #' a \code{\link{matrix}}, colors are assigned to each column of \code{y}.
#' #' @param show.cumulative If \code{TRUE}, which is the default, the cumulative labels are shown at the top of each bar.
#' #' @param label.in.column.threshold Where the height of a block as a proportion of the range of values on the y-axis
#' #' is less than this value, the size of the block is shown below (rather than within) the columns.
#' #' The defaut is 0.025.
#' #' @param label.offset When a label is not plotted in a box (which includes all times when the cumulative value is plotted above),
#' #' this parameter controls how far the labels is from the box, and it is intepretated as a proportion of the range of \code{y}.
#' #' The defaut is 0.025.
#' #' is less than this value, the size of the block is shown below (rather than within) the columns.
#' #' @importFrom plotly plot_ly add_trace layout config add_annotations add_text
#' #' @importFrom flipFormat FormatAsReal
#' #' @export
#' Waterfall <- function(y,
#'                       cumulative = 1,
#'                       top = NULL,
#'                       decimals = 1,
#'                       show.cumulative = TRUE,
#'                       label.in.column.threshold = 0.025,
#'                       label.offset = 0.025,
#'
#'                                          global.font.family = "Arial",
#'                                          global.font.color = rgb(44, 44, 44, maxColorValue = 255),
#'                                          title = "",
#'                                          title.font.family = global.font.family,
#'                                          title.font.color = global.font.color,
#'                                          title.font.size = 16,
#'                                          subtitle = "",
#'                                          subtitle.font.family = global.font.family,
#'                                          subtitle.font.color = global.font.color,
#'                                          subtitle.font.size = 12,
#'                                          footer = "",
#'                                          footer.font.family = global.font.family,
#'                                          footer.font.color = global.font.color,
#'                                          footer.font.size = 8,
#'                                          footer.wrap = TRUE,
#'                                          footer.wrap.nchar = 100,
#'                                          colors =  c("red", "blue")#ChartColors(max(1, ncol(x))),
#'                                          fit.line.colors = colors,
#'                                          opacity = NULL,
#'                                          background.fill.color = rgb(255, 255, 255, maxColorValue = 255),
#'                                          background.fill.opacity = 1,
#'                                          charting.area.fill.color = background.fill.color,
#'                                          charting.area.fill.opacity = 1,
#'                                          legend.show = TRUE,
#'                                          legend.position.x = 1.02,
#'                                          legend.position.y = 1,
#'                                          legend.fill.color = background.fill.color,
#'                                          legend.fill.opacity = 1,
#'                                          legend.border.color = rgb(44, 44, 44, maxColorValue = 255),
#'                                          legend.border.line.width = 0,
#'                                          legend.font.color = global.font.color,
#'                                          legend.font.family = global.font.family,
#'                                          legend.font.size = 10,
#'                                          legend.ascending = NA,
#'                                          margin.top = NULL,
#'                                          margin.bottom = NULL,
#'                                          margin.left = NULL,
#'                                          margin.right = NULL,
#'                                          margin.inner.pad = NULL,
#'                                          grid.show = TRUE,
#'                                          y.title = "",
#'                                          y.title.font.color = global.font.color,
#'                                          y.title.font.family = global.font.family,
#'                                          y.title.font.size = 12,
#'                                          y.line.width = 0,
#'                                          y.line.color = rgb(0, 0, 0, maxColorValue = 255),
#'                                          y.tick.mark.length = 5,
#'                                          y.bounds.minimum = NULL,
#'                                          y.bounds.maximum = NULL,
#'                                          y.tick.distance = NULL,
#'                                          y.zero = TRUE,
#'                                          y.zero.line.width = 0,
#'                                          y.zero.line.color = rgb(44, 44, 44, maxColorValue = 255),
#'                                          y.data.reversed = FALSE,
#'                                          y.grid.width = 1 * grid.show,
#'                                          y.grid.color = rgb(225, 225, 225, maxColorValue = 255),
#'                                          y.tick.show = TRUE,
#'                                          y.tick.suffix = "",
#'                                          y.tick.y.tick.prefix = "",
#'                                          y.tick.format = "",
#'                                          y.hovertext.format = y.tick.format,
#'                                          y.tick.angle = NULL,
#'                                          y.tick.font.color = global.font.color,
#'                                          y.tick.font.family = global.font.family,
#'                                          y.tick.font.size = 10,
#'                                          x.title = "",
#'                                          x.title.font.color = global.font.color,
#'                                          x.title.font.family = global.font.family,
#'                                          x.title.font.size = 12,
#'                                          x.line.width = 0,
#'                                          x.line.color = rgb(0, 0, 0, maxColorValue = 255),
#'                                          x.tick.marks = "",
#'                                          x.tick.mark.length = 5,
#'                                          x.bounds.minimum = NULL,
#'                                          x.bounds.maximum = NULL,
#'                                          x.tick.distance = NULL,
#'                                          x.zero.line.width = 0,
#'                                          x.zero.line.color = rgb(44, 44, 44, maxColorValue = 255),
#'                                          x.data.reversed = FALSE,
#'                                          x.grid.width = 0 * grid.show,
#'                                          x.grid.color = rgb(225, 225, 225, maxColorValue = 255),
#'                                          x.tick.show = TRUE,
#'                                          x.tick.format = "",
#'                                          x.hovertext.format = x.tick.format,
#'                                          x.tick.angle = NULL,
#'                                          x.tick.font.color = global.font.color,
#'                                          x.tick.font.family = global.font.family,
#'                                          x.tick.font.size = 10,
#'                                          x.tick.label.wrap = TRUE,
#'                                          x.tick.label.wrap.nchar = 21,
#'                                          marker.border.width = 1,
#'                                          marker.border.colors = colors,
#'                                          marker.border.opacity = 1,
#'                                          tooltip.show = TRUE,
#'                                          modebar.show = FALSE,
#'                                          bar.gap = 0.15,
#'                                          data.label.show = FALSE,
#'                                          data.label.font.family = global.font.family,
#'                                          data.label.font.size = 10,
#'                                          data.label.font.color = global.font.color,
#'                                          data.label.decimals = 2,
#'                                          data.label.y.tick.prefix = "",
#'                                          data.label.suffix = "",
#'                                          data.label.threshold = NULL))
#'
#'                       )
#' {
#'     ErrorIfNotEnoughData(x)
#'     if (is.vector(x)) # Converting to a matrix
#'     {
#'         original.x <- x
#'         n <- length(x)
#'         x <- matrix(0, n, 2, dimnames = list(names(x),c("Decrease", "Increase")))
#'         x[original.x < 0, 1] <- original.x[original.x < 0]
#'         x[original.x >= 0, 2] <- original.x[original.x >= 0]
#'         cumulative <- cumsum(original.x)
#'         bs <- ifelse(x < 0, cumulative, c(0, cumulative[-n]))
#'         top <- bs + abs(original.x)
#'     } else if (length(cumulative) == 1)
#'     {
#'         cm <- x[, cumulative]
#'         x <- x[, -cumulative, drop = FALSE]
#'         cumulative <- cm
#'     }
#'     if (is.null(top))
#'         top <- cumulative
#'     n <- nrow(x)
#'     k <- ncol(x)
#'     categories <- rownames(x)
#'     categories <- factor(categories, levels = categories)
#'     # Formatting labels
#'     x.text <- FormatAsReal(x, decimals = data.label.decimals)
#'     x.text[x > 0] <- paste0("+", x.text[x > 0])
#'     x.text <- matrix(paste0(y.tick.prefix, x.text, y.tick.suffix), ncol = ncol(x.text))
#'     # Creating series for the plot
#'     x <- abs(x)
#'     base <- top - apply(x, 1, sum)
#'     cumulative.text <- paste0(y.tick.prefix, FormatAsReal(cumulative, decimals = data.label.decimals), y.tick.suffix)
#'     rng <- max(top) - min(base)
#'     label.offset <- rng * label.offset
#'     # Creating the plot (empty)
#'     p <- plot_ly(x = categories,
#'                  x = base,
#'                  marker = list(color = "white"),
#'                  hoverinfo='none',
#'                  type = "bar")
#'     # blocks
#'     block.base <- base
#'     if (length(colors) < k)
#'         k <- rep(colors, k) #Creates an unnecssarily long but harmless vector
#'     for (i in 1:k)
#'     {
#'         x.values <- x[, i]
#'         p <- add_trace(p = p,
#'                        x = categories,
#'                        x = x.values,
#'                        name = categories,
#'                        marker = list(color = colors[i]),
#'                        hoverinfo = "text",
#'                        type = "bar")
#'         # Labels
#'         positives <- x.values > 0
#'         ys <- x.values[positives]
#'         small.box <- ys / rng < label.in.column.threshold
#'         label.x.position <- ifelse (small.box, block.base[positives] - label.offset,  (block.base[positives] * 2 + ys) / 2 )
#'         p <- add_text(p,
#'                               x = categories[positives],
#'                               x = label.x.position,
#'                               text = x.text[positives, i],
#'                               textfont = list(color = ifelse(label.x.position < base[positives], "black", "white")),
#'                               marker = NULL)
#'         block.base <- block.base + x.values
#'     }
#'     # Cumulative labels
#'     if (show.cumulative)
#'         p <- add_text(p,
#'                               x = categories,
#'                               x = top + label.offset,
#'                               text = cumulative.text,
#'                               textfont = list(color = "black"),
#'                               marker = NULL)
#'     # Finalizing the plotting options
#'     p <- config(p, displayModeBar = FALSE)
#'     layout(p = p,
#'            barmode = "stack",
#'            showlegend = FALSE,
#'            title = title,
#'            xaxis = list(title = "",
#'                         zeroline = FALSE,
#'                         showticklabels = TRUE,
#'                         showgrid = FALSE),
#'            yaxis = list(title = x.title,
#'                         zeroline = TRUE,
#'                         showticklabels = FALSE,
#'                         showgrid = FALSE))
#'
#'
#'
#'     # Data checking
#'     chart.matrix <- checkMatrixNames(x)
#'     if (!is.numeric(chart.matrix))
#'         stop("Input data should be numeric.")
#'     x.labels.full <- rownames(chart.matrix)
#'
#'     is.stacked <- grepl("Stacked", type, fixed=T)
#'     is.hundred.percent.stacked <- grepl("100% Stacked", type, fixed=T)
#'     if (is.stacked && ncol(chart.matrix) < 2)
#'         stop(paste(type, "requires more than one series. Use Column Chart instead for this data."))
#'     if (is.stacked && (any(is.na(chart.matrix)) || any(chart.matrix < 0)))
#'         stop("Stacked charts cannot be produced with missing or negative values.")
#'     if (is.hundred.percent.stacked && any(SumEachRow(chart.matrix) == 0))
#'         stop("100% stacked charts cannot be produced with rows that do not contain positive values.")
#'     if (any(!is.finite(chart.matrix)))
#'         warning("Missing values have been set to zero.")
#'
#'     # Some minimal data cleaning
#'     # Assume formatting and Qtable/attribute handling already done
#'     data.label.mult <- 1
#'     if (is.hundred.percent.stacked)
#'     {
#'         chart.matrix <- cum.data(chart.matrix, "column.percentage")
#'         y.tick.format <- "%"
#'         y.hovertext.format <- "%"
#'         data.label.suffix <- "%"
#'         data.label.mult <- 100
#'     }
#'     matrix.labels <- names(dimnames(chart.matrix))
#'     if (nchar(x.title) == 0 && length(matrix.labels) == 2)
#'         x.title <- matrix.labels[1]
#'
#'     # Constants
#'     hover.mode <- if (tooltip.show) "closest" else FALSE
#'     barmode <- if (is.stacked) "stack" else ""
#'     legend.group <- if (is.stacked) "grouped" else ""
#'     if (is.null(opacity))
#'         opacity <- 1
#'     eval(colors) # not sure why, but this is necessary for bars to appear properly
#'
#'     title.font=list(family=title.font.family, size=title.font.size, color=title.font.color)
#'     subtitle.font=list(family=subtitle.font.family, size=subtitle.font.size, color=subtitle.font.color)
#'     x.title.font=list(family=x.title.font.family, size=x.title.font.size, color=x.title.font.color)
#'     y.title.font=list(family=y.title.font.family, size=y.title.font.size, color=y.title.font.color)
#'     ytick.font=list(family=y.tick.font.family, size=y.tick.font.size, color=y.tick.font.color)
#'     xtick.font=list(family=x.tick.font.family, size=x.tick.font.size, color=x.tick.font.color)
#'     footer.font=list(family=footer.font.family, size=footer.font.size, color=footer.font.color)
#'     legend.font=list(family=legend.font.family, size=legend.font.size, color=legend.font.color)
#'     data.label.font=list(family=data.label.font.family, size=data.label.font.size, color=data.label.font.color)
#'
#'     if (ncol(chart.matrix) == 1)
#'         legend.show <- FALSE
#'     legend <- setLegend(type, legend.font, legend.ascending, legend.fill.color, legend.fill.opacity,
#'                         legend.border.color, legend.border.line.width, legend.position.x, legend.position.y)
#'     footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate=FALSE)
#'
#'     # Format axis labels
#'     if (!grid.show)
#'     {
#'         x.grid.width <- 0
#'         y.grid.width <- 0
#'     }
#'
#'     # Turn off autorange if data labels are shown
#'     if (data.label.show && is.null(x.bounds.minimum))
#'     {
#'         tmp.range <- getRange(rownames(chart.matrix), NULL, NULL)
#'         x.bounds.minimum <- tmp.range[1]
#'         x.bounds.maximum <- tmp.range[2]
#'     }
#'     xtick <- setTicks(x.bounds.minimum, x.bounds.maximum, x.tick.distance, x.data.reversed)
#'     ytick <- setTicks(y.bounds.minimum, y.bounds.maximum, y.tick.distance, y.data.reversed)
#'
#'     axisFormat <- formatLabels(chart.matrix, type, x.tick.label.wrap, x.tick.label.wrap.nchar,
#'                                x.tick.format, y.tick.format)
#'
#'     yaxis <- setAxis(y.title, "left", axisFormat, y.title.font,
#'                      y.line.color, y.line.width, y.grid.width, y.grid.color,
#'                      ytick, ytick.font, y.tick.angle, y.tick.mark.length, y.tick.distance, y.tick.format,
#'                      y.tick.y.tick.prefix, y.tick.suffix,
#'                      y.tick.show, y.zero, y.zero.line.width, y.zero.line.color,
#'                      y.hovertext.format)
#'     xaxis <- setAxis(x.title, "bottom", axisFormat, x.title.font,
#'                      x.line.color, x.line.width, x.grid.width, x.grid.color,
#'                      xtick, xtick.font, x.tick.angle, x.tick.mark.length, x.tick.distance, x.tick.format,
#'                      "", "", x.tick.show, FALSE, x.zero.line.width, x.zero.line.color,
#'                      x.hovertext.format, axisFormat$labels)
#'
#'     # Work out margin spacing
#'     margins <- list(t = 20, b = 50, r = 60, l = 80, pad = 0)
#'     margins <- setMarginsForAxis(margins, axisFormat, xaxis)
#'     margins <- setMarginsForText(margins, title, subtitle, footer, title.font.size,
#'                                  subtitle.font.size, footer.font.size)
#'     margins <- setMarginsForLegend(margins, legend.show, legend)
#'     if (!is.null(margin.top))
#'         margins$t <- margin.top
#'     if (!is.null(margin.bottom))
#'         margins$b <- margin.bottom
#'     if (!is.null(margin.left))
#'         margins$l <- margin.left
#'     if (!is.null(margin.right))
#'         margins$r <- margin.right
#'     if (!is.null(margin.inner.pad))
#'         margins$pad <- margin.inner.pad
#'
#'     # Finalise text in margins
#'     footer.axis <- setFooterAxis(footer, footer.font, margins)
#'     subtitle.axis <- setSubtitleAxis(subtitle, subtitle.font, title, title.font)
#'
#'     # Data label annotations
#'     data.annotations <- NULL
#'     if (data.label.show)
#'         data.annotations <- dataLabelPositions(chart.matrix = chart.matrix,
#'                                                annotations = NULL,
#'                                                data.label.mult = data.label.mult,
#'                                                bar.decimals = data.label.decimals,
#'                                                bar.y.tick.prefix = data.label.y.tick.prefix,
#'                                                bar.suffix = data.label.suffix,
#'                                                barmode = barmode,
#'                                                swap.axes.and.data = FALSE,
#'                                                bar.gap = bar.gap,
#'                                                display.threshold = data.label.threshold,
#'                                                dates = axisFormat$ymd)
#'
#'     ## Initiate plotly object
#'     p <- plot_ly(as.data.frame(chart.matrix))
#'     x.labels <- axisFormat$labels
#'     y.labels <- colnames(chart.matrix)
#'     xaxis2 <- NULL
#'
#'     ## Add a trace for each col of data in the matrix
#'     for (i in 1:ncol(chart.matrix))
#'     {
#'         y <- as.numeric(chart.matrix[, i])
#'         x <- x.labels
#'         marker <- list(color = toRGB(colors[i], alpha = opacity),
#'                        line = list(color = toRGB(marker.border.colors[i],
#'                                                  alpha = marker.border.opacity),
#'                                    width = marker.border.width))
#'
#'         # add invisible line to force all categorical labels to be shown
#'         if (!is.stacked && i == 1)
#'             p <- add_trace(p, x = x, y = rep(min(y,na.rm = T), length(x)),
#'                            type = "scatter", mode = "lines",
#'                            hoverinfo = "none", showlegend = F, opacity = 0)
#'
#'         # this is the main trace for each data series
#'         tmp.group <- if (legend.group == "") paste("group", i) else legend.group
#'         p <- add_trace(p, x = x, y = y, type = "bar", orientation = "v", marker = marker,
#'                        name  =  y.labels[i], legendgroup  =  tmp.group,
#'                        text = autoFormatLongLabels(x.labels.full, wordwrap=T, truncate=F),
#'                        hoverinfo  = setHoverText(xaxis, chart.matrix))
#'
#'         if (fit.type != "None" && !is.stacked)
#'         {
#'             tmp.fit <- fitSeries(x, y, fit.type, fit.ignore.last, xaxis$type)
#'             tmp.fname <- if (ncol(chart.matrix) == 1)  fit.line.name
#'             else sprintf("%s: %s", fit.line.name, y.labels[i])
#'             p <- add_trace(p, x = tmp.fit$x, y = tmp.fit$y, type = 'scatter', mode = "lines",
#'                            name = tmp.fname, legendgroup = tmp.group, showlegend = F,
#'                            line = list(dash = fit.line.type, width = fit.line.width,
#'                                        color = fit.line.colors[i], shape = 'spline'))
#'         }
#'
#'         if (data.label.show && !is.stacked)
#'         {
#'             x.range <- getRange(x, xaxis, axisFormat)
#'             y.sign <- sign(data.annotations$y[,i])
#'             y.diff <- -1 * (y.sign < 0) * diff(range(data.annotations$y))/200
#'             xaxis2 <- list(overlaying = "x", visible = FALSE, range = x.range)
#'             p <- add_text(p, xaxis = "x2", x = data.annotations$x[,i],
#'                           y = data.annotations$y[,i] + y.diff,
#'                           text = data.annotations$text[,i],
#'                           textposition = ifelse(y.sign >= 0, "top center", "bottom center"),
#'                           textfont = data.label.font, hoverinfo = "none",
#'                           showlegend = FALSE, legendgroup = tmp.group)
#'         }
#'     }
#'
#'     p <- config(p, displayModeBar = modebar.show)
#'     p$sizingPolicy$browser$padding <- 0
#'     p <- layout(p,
#'                 title = title,
#'                 showlegend = legend.show,
#'                 legend = legend,
#'                 yaxis = yaxis,
#'                 xaxis4 = footer.axis,
#'                 xaxis3 = subtitle.axis,
#'                 xaxis2 = xaxis2,
#'                 xaxis = xaxis,
#'                 margin = margins,
#'                 plot_bgcolor = toRGB(charting.area.fill.color, alpha = charting.area.fill.opacity),
#'                 paper_bgcolor = toRGB(background.fill.color, alpha = background.fill.opacity),
#'                 hovermode = hover.mode,
#'                 titlefont = title.font,
#'                 font = data.label.font,
#'                 annotations = if (is.stacked) data.annotations else NULL,
#'                 bargap = bar.gap,
#'                 barmode = barmode
#'     )
#'     result <- list(htmlwidget = p)
#'     class(result) <- "StandardChart"
#'     result
#' }
#'
#' }
#'
