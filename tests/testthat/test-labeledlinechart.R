context("labeled line chart")

z <- structure(c(1L, 2L, 3L, 4L, 5L, 2L, 3L, 4L, 5L, 6L), .Dim = c(5L, 2L),
    .Dimnames = list(c("T", "U", "V", "W", "X"), c("A", "B")))

# Convenience: build the chart and return the htmlwidget payload
widgetOf <- function(...) suppressWarnings(Line(...))$htmlwidget$x

# The widget JSON encodes the labels and the tooltip text, so each comes back as one string
decodeJson <- function(v) as.character(jsonlite::fromJSON(as.character(v)))
labelsOf <- function(x) decodeJson(x$label)
tooltipsOf <- function(x) decodeJson(x$tooltipText)

test_that("Line only dispatches to labeledLine when there are labels to place",
{
    expect_s3_class(Line(z, data.label.auto.placement = TRUE)$htmlwidget, "plotly")
    expect_s3_class(Line(z, data.label.auto.placement = FALSE,
                         data.label.show = TRUE)$htmlwidget, "plotly")
    expect_s3_class(Line(z, data.label.auto.placement = TRUE,
                         data.label.show = TRUE)$htmlwidget, "rhtmlCombinedScatter")
    expect_s3_class(Line(z, data.label.auto.placement = TRUE,
                         data.label.show.at.ends = TRUE)$htmlwidget,
                    "rhtmlCombinedScatter")
    expect_s3_class(Line(z, data.label.auto.placement = TRUE,
                         data.label.show = c(TRUE, FALSE))$htmlwidget,
                    "rhtmlCombinedScatter")
})

test_that("Series-specific line settings are passed through per series",
{
    x <- widgetOf(z, data.label.auto.placement = TRUE, data.label.show = TRUE,
                  line.type = c("Solid", "Dot"), line.thickness = "2, 8",
                  colors = c("#FF0000", "#00AA00"), opacity = 0.5)
    expect_equal(as.character(x$lineType), "[\"solid\",\"dot\"]")
    expect_equal(as.character(x$lineThickness), "[2,8]")
    # opacity is folded into the line color, as the plotly line chart does
    expect_equal(as.character(x$lineColors),
                 "[\"rgba(255,0,0,0.5)\",\"rgba(0,170,0,0.5)\"]")
    expect_true(x$lineShow)
})

test_that("Markers are sized per point, with hidden markers at zero",
{
    x <- widgetOf(z, data.label.auto.placement = TRUE, data.label.show = TRUE,
                  marker.show.at.ends = TRUE, marker.size = c(6, 14))
    expect_equal(x$pointRadius, c(3, 0, 0, 0, 3, 7, 0, 0, 0, 7))

    x <- widgetOf(z, data.label.auto.placement = TRUE, data.label.show = TRUE)
    expect_equal(x$pointRadius, rep(0, 10)) # marker.show defaults to FALSE
})

test_that("Data labels are only supplied for the points that show them",
{
    x <- widgetOf(z, data.label.auto.placement = TRUE, data.label.show = c(TRUE, FALSE))
    expect_equal(labelsOf(x), c("1", "2", "3", "4", "5", "", "", "", "", ""))

    x <- widgetOf(z, data.label.auto.placement = TRUE, data.label.show = TRUE,
                  data.label.format = ".1f", data.label.prefix = "<",
                  data.label.suffix = ">")
    expect_equal(labelsOf(x)[1:3], c("<1.0>", "<2.0>", "<3.0>"))
})

test_that("Data label font color falls back to the series color when autocoloring",
{
    x <- widgetOf(z, data.label.auto.placement = TRUE, data.label.show = TRUE,
                  data.label.font.autocolor = TRUE)
    expect_null(x$labelsFontColor)

    x <- widgetOf(z, data.label.auto.placement = TRUE, data.label.show = TRUE,
                  data.label.font.color = "#123456")
    expect_equal(x$labelsFontColor, "#123456")
})

test_that("Annotation markup is sent beside the data label, not inside it",
{
    # The widget escapes the label, because for a scatter plot it is untrusted text from
    # the data, and takes the markup separately. Markup left in the label is escaped and
    # then shows as literal "<tspan ...>" text on the chart.
    recolor <- list(list(type = "Recolor text", data = "",
        threstype = "above threshold", threshold = "3", color = "#00FF00"))
    x <- widgetOf(z, data.label.auto.placement = TRUE, data.label.show = TRUE,
                  annotation.list = recolor)
    expect_equal(labelsOf(x), c("1", "2", "3", "4", "5", "2", "3", "4", "5", "6"))
    expect_false(any(grepl("span", labelsOf(x), fixed = TRUE)))

    # Recoloring wraps the label, so it arrives as an opening tag before and a closing
    # tag after it
    expect_equal(x$preLabelAnnotations[4], "<tspan style='fill:#00FF00'>")
    expect_equal(x$postLabelAnnotations[4], "</tspan>")
    expect_equal(x$preLabelAnnotations[1], "")          # below the threshold
    expect_equal(x$postLabelAnnotations[1], "")
    # and never as <span>, which the SVG sanitiser discards along with its contents
    expect_false(any(grepl("<span", x$preLabelAnnotations, fixed = TRUE)))
    expect_false(any(grepl("<span", x$postLabelAnnotations, fixed = TRUE)))

    # An annotation that only follows the label leaves the front alone
    arrow <- list(list(type = "Arrow - up", data = "", threstype = "above threshold",
        threshold = "3", color = "red", size = 14, width = 1, offset = 0,
        font.family = "Arial", font.weight = "normal", font.style = "normal"))
    x <- widgetOf(z, data.label.auto.placement = TRUE, data.label.show = TRUE,
                  annotation.list = arrow)
    expect_equal(x$preLabelAnnotations[4], "")
    expect_match(x$postLabelAnnotations[4], "^<tspan style='fill:red")
    expect_match(x$postLabelAnnotations[4], "&#8593;</tspan>$")

    # Hiding a label empties it, and leaves no markup that would show on its own
    hide <- list(list(type = "Hide", data = "",
        threstype = "above threshold", threshold = "4"))
    x <- widgetOf(z, data.label.auto.placement = TRUE, data.label.show = TRUE,
                  annotation.list = hide)
    expect_equal(labelsOf(x), c("1", "2", "3", "4", "", "2", "3", "4", "", ""))
    expect_equal(x$preLabelAnnotations[c(5, 9, 10)], c("", "", ""))
    expect_equal(x$postLabelAnnotations[c(5, 9, 10)], c("", "", ""))

    # With no annotations there is nothing beside the label at all
    x <- widgetOf(z, data.label.auto.placement = TRUE, data.label.show = TRUE)
    expect_true(all(!nzchar(x$preLabelAnnotations)))
    expect_true(all(!nzchar(x$postLabelAnnotations)))
})

test_that("The annotated label matches the plotly line chart's, for every type",
{
    # pre + label + post is what the widget renders, and it has to come to the same thing
    # as the single string the plotly chart is given
    mk <- function(type, extra = list()) list(c(list(type = type, data = "",
        threstype = "above threshold", threshold = "3", color = "red", size = 14,
        width = 1, offset = 0, font.family = "Arial", font.weight = "normal",
        font.style = "normal", format = ".2f", prefix = "", suffix = ""), extra))

    fromPlotly <- function(p) {
        b <- plotly::plotly_build(p)
        unlist(Filter(Negate(is.null), lapply(b$x$data, function(tr)
            if (!is.null(tr$mode) && grepl("text", tr$mode) && !is.null(tr$text))
                tr$text else NULL)))
    }
    # the widget needs tspan and fill where plotly takes span and color
    norm <- function(v) {
        v <- gsub("tspan", "span", v, fixed = TRUE)
        sort(gsub("fill:", "color:", v[nzchar(v)], fixed = TRUE))
    }

    for (ty in c("Arrow - up", "Arrow - down", "Border", "Caret - up", "Caret - down",
                 "Recolor text", "Hide", "Shadow", "Text - after data label",
                 "Text - before data label")) {
        annot <- mk(ty)
        a <- suppressWarnings(Line(z, data.label.show = TRUE, annotation.list = annot))
        x <- widgetOf(z, data.label.show = TRUE, annotation.list = annot,
                      data.label.auto.placement = TRUE)
        combined <- paste0(x$preLabelAnnotations, labelsOf(x), x$postLabelAnnotations)
        expect_equal(norm(combined), norm(fromPlotly(a$htmlwidget)), info = ty)
    }
})

test_that("Hovertext is resolved in R and passed as tooltip text",
{
    x <- widgetOf(z, data.label.auto.placement = TRUE, data.label.show = TRUE)
    expect_equal(tooltipsOf(x)[1:3], c("T: 1.00", "U: 2.00", "V: 3.00"))

    x <- widgetOf(z, data.label.auto.placement = TRUE, data.label.show = TRUE,
                  hovertext.template = "%{x} had %{y}", y.hovertext.format = ".2f",
                  y.tick.prefix = "$")
    expect_equal(tooltipsOf(x)[1:2], c("T had $1.00", "U had $2.00"))

    # Turning the tooltip off has to turn hover off, not just withhold the text: the
    # widget would otherwise build a tooltip of its own from the label and coordinates
    x <- widgetOf(z, data.label.auto.placement = TRUE, data.label.show = TRUE,
                  tooltip.show = FALSE)
    expect_null(x$tooltipText)
    expect_false(x$tooltipShow)
    expect_true(widgetOf(z, data.label.auto.placement = TRUE,
                         data.label.show = TRUE)$tooltipShow)
})

test_that("The axis type is determined in R and the labels left for the widget",
{
    zd <- z
    rownames(zd) <- c("Jan 2020", "Feb 2020", "Mar 2020", "Apr 2020", "May 2020")
    x <- widgetOf(zd, data.label.auto.placement = TRUE, data.label.show = TRUE)
    expect_true(x$xIsDateTime)
    expect_null(x$xLevels)

    zn <- z
    rownames(zn) <- c("10", "20", "30", "40", "50")
    x <- widgetOf(zn, data.label.auto.placement = TRUE, data.label.show = TRUE)
    expect_false(x$xIsDateTime)
    expect_equal(as.character(x$X), "[10,20,30,40,50,10,20,30,40,50]")

    x <- widgetOf(z, data.label.auto.placement = TRUE, data.label.show = TRUE)
    expect_false(x$xIsDateTime)
    expect_equal(as.character(x$xLevels), "[\"T\",\"U\",\"V\",\"W\",\"X\"]")
})

test_that("The axis type decides the x values of the trend line as well as the data",
{
    # A trend line has to sit on the same axis as the points it is fitted to, and the axis
    # type is what puts it there: fitSeries maps a categorical axis onto 0..n-1 to do the
    # fitting and then hands back the original labels, which is what pairs with x.levels.
    # Pinned because the axis type reaches the fit separately from the values, so the two
    # can disagree without anything else in the payload looking wrong.
    fitXOf <- function(x) jsonlite::fromJSON(as.character(x$fitX), simplifyMatrix = FALSE)

    x <- widgetOf(z, data.label.auto.placement = TRUE, data.label.show = TRUE,
                  fit.type = "Smooth")
    expect_equal(decodeJson(x$xLevels), rownames(z))
    expect_equal(fitXOf(x), list(rownames(z), rownames(z)))

    # A numeric axis is a different shape entirely: the fit is drawn over 100 interpolated
    # points spanning the data rather than at the values, so a categorical axis mistaken
    # for a numeric one does not merely mislabel the trend line, it changes what is drawn
    zn <- z
    rownames(zn) <- c("10", "20", "30", "40", "50")
    x <- widgetOf(zn, data.label.auto.placement = TRUE, data.label.show = TRUE,
                  fit.type = "Smooth")
    expect_null(x$xLevels)
    fit.x <- fitXOf(x)
    expect_equal(lengths(fit.x), c(100L, 100L))
    expect_equal(range(fit.x[[1]]), c(10, 50))
})

test_that("Missing values are sent as null so the widget can break the line at them",
{
    # A gap must reach the widget as the JSON null literal. jsonlite would otherwise
    # encode it as the string "NA", which the widget cannot recognise as missing: the
    # axis is then classified as categorical, and plotly draws straight through the gap
    # instead of breaking the line.
    with.na <- structure(c(1, 2, NA, 4, 5, 2, 3, 4, 5, 6), .Dim = c(5L, 2L),
        .Dimnames = list(letters[1:5], c("A", "B")))
    x <- widgetOf(with.na, data.label.auto.placement = TRUE, data.label.show = TRUE)
    expect_match(as.character(x$Y), "null", fixed = TRUE)
    expect_false(grepl('"NA"', as.character(x$Y), fixed = TRUE))

    # The gap must stay in place rather than being dropped, so that it separates the
    # points on either side of it
    y <- jsonlite::fromJSON(as.character(x$Y))
    expect_equal(y, c(1, 2, NA, 4, 5, 2, 3, 4, 5, 6))

    # and it carries no label of its own
    expect_equal(labelsOf(x)[3], "")

    no.na <- with.na; no.na[3, 1] <- 3
    expect_false(grepl("null", as.character(
        widgetOf(no.na, data.label.auto.placement = TRUE,
                 data.label.show = TRUE)$Y), fixed = TRUE))
})

test_that("The margins match the plotly line chart's",
{
    # Turning automatic placement on should not reflow the chart, so labeledLine reserves
    # the margins Line works out rather than letting the widget use its own smaller ones.
    # Compared after plotly_build, which is where the layout Line asked for is resolved.
    long <- z
    rownames(long) <- paste(c("Alpha","Beta","Gamma","Delta","Epsilon"), "category")

    expectSameMargins <- function(dat, ...) {
        a <- plotly::plotly_build(
            Line(dat, data.label.show = TRUE, ...)$htmlwidget)$x$layout$margin
        b <- widgetOf(dat, data.label.auto.placement = TRUE, data.label.show = TRUE, ...)
        expect_equal(c(l = a$l, r = a$r, t = a$t, b = a$b),
                     c(l = b$marginLeft, r = b$marginRight,
                       t = b$marginTop, b = b$marginBottom))
        expect_equal(a$autoexpand, b$marginAutoexpand)
    }

    expectSameMargins(z)
    expectSameMargins(z, title = "T", subtitle = "S", footer = "F",
                      x.title = "X", y.title = "Y")
    expectSameMargins(long)                       # grows the bottom margin
    expectSameMargins(z, legend.show = FALSE)     # shrinks the right margin
    expectSameMargins(z, margin.left = 123, margin.top = 45)   # caller overrides win
    expectSameMargins(z, margin.autoexpand = FALSE)
})

test_that("The axis range mode and tick count match the plotly line chart's",
{
    # y.zero and x.zero reach plotly as the axis range mode, and the tick maximums as the
    # tick count. Neither had an equivalent in the widget, so with y.zero defaulting to
    # TRUE the y axis stopped short of zero and the chart reflowed. Both are taken from
    # the axes setAxis built, so whatever it decides is what the widget is told.
    num <- z
    rownames(num) <- c("10", "20", "30", "40", "50")

    # plotly_build represents an unset axis attribute as an empty list, where setAxis
    # itself leaves it NULL, so both stand for "not set"
    unset <- function(v) if (length(v) == 0) NULL else v

    expectSameAxes <- function(dat, ...) {
        a <- plotly::plotly_build(
            Line(dat, data.label.show = TRUE, ...)$htmlwidget)$x$layout
        b <- widgetOf(dat, data.label.auto.placement = TRUE, data.label.show = TRUE, ...)
        expect_equal(unset(a$yaxis$rangemode), unset(b$yAxisRangeMode))
        expect_equal(unset(a$xaxis$rangemode), unset(b$xAxisRangeMode))
        expect_equal(unset(a$yaxis$nticks), unset(b$yAxisTickMaxnum))
        expect_equal(unset(a$xaxis$nticks), unset(b$xAxisTickMaxnum))
        # setAxis rotates long categorical labels, and the widget has to rotate them too
        expect_equal(unset(a$xaxis$tickangle), unset(b$xAxisTickAngle))
        expect_equal(unset(a$yaxis$tickangle), unset(b$yAxisTickAngle))
    }

    expectSameAxes(z)                                  # y.zero defaults to TRUE
    expectSameAxes(z, y.zero = FALSE)
    expectSameAxes(num)                                # a numeric x axis sets nticks
    expectSameAxes(num, x.zero = TRUE)
    expectSameAxes(z, y.tick.maxnum = 4)
    expectSameAxes(num, x.tick.maxnum = 3)
    expectSameAxes(num, y.zero = FALSE, x.zero = FALSE,
                   y.tick.maxnum = 5, x.tick.maxnum = 4)

    long <- z
    rownames(long) <- paste(c("Alpha","Beta","Gamma","Delta","Epsilon"), "category")
    expectSameAxes(long)                               # long labels get rotated
    expect_equal(widgetOf(long, data.label.auto.placement = TRUE,
                          data.label.show = TRUE)$xAxisTickAngle, 90)

    # The default really is tozero, which is the case that was wrong
    expect_equal(widgetOf(z, data.label.auto.placement = TRUE,
                          data.label.show = TRUE)$yAxisRangeMode, "tozero")
})

test_that("The footer is padded down the bottom margin as the plotly chart does",
{
    # The widget places the title, subtitle and footer inside the reserved margins rather
    # than laying them out itself, which only lands in the same place if the footer text
    # carries the padding setFooter adds.
    x <- widgetOf(z, data.label.auto.placement = TRUE, data.label.show = TRUE,
                  footer = "A footer")
    expect_match(x$footer, "A footer", fixed = TRUE)
    expect_match(x$footer, "&nbsp;", fixed = TRUE)   # setFooter's leading pad

    plain <- Line(z, data.label.show = TRUE, footer = "A footer")$htmlwidget
    footer.annot <- Filter(function(a) identical(a$text, x$footer),
                           plotly::plotly_build(plain)$x$layout$annotations)
    expect_length(footer.annot, 1)                   # byte-identical to Line's

    # No footer means no padding to send
    expect_equal(widgetOf(z, data.label.auto.placement = TRUE,
                          data.label.show = TRUE)$footer, "")
})

test_that("The footer alignment is passed on",
{
    for (al in c("left", "center", "right"))
        expect_equal(widgetOf(z, data.label.auto.placement = TRUE, data.label.show = TRUE,
                              footer = "A footer", footer.align = al)$footerAlignment, al)

    # and no longer reported as unsupported
    expect_warning(Line(z, data.label.auto.placement = TRUE, data.label.show = TRUE,
                        footer = "A footer", footer.align = "right"), NA)
})

test_that("x.zero and y.zero are honoured independently",
{
    # The widget's origin flag gates both zero lines at once, so each is turned off
    # through its own width. Setting one flag used to draw both lines.
    num <- z
    rownames(num) <- c("10", "20", "30", "40", "50")

    countShapes <- function(p) {
        sh <- plotly::plotly_build(p)$x$layout$shapes
        if (is.null(sh)) 0 else if (!is.null(sh$type)) 1 else length(sh)
    }

    for (xz in c(FALSE, TRUE)) for (yz in c(FALSE, TRUE)) {
        args <- list(num, data.label.show = TRUE, x.zero = xz, y.zero = yz,
                     x.zero.line.width = 2, y.zero.line.width = 3)
        x <- do.call(widgetOf, c(args, list(data.label.auto.placement = TRUE)))
        # each width is zero exactly when its own flag is off
        expect_equal(x$xAxisZeroLineWidth, if (xz) 2 else 0)
        expect_equal(x$yAxisZeroLineWidth, if (yz) 3 else 0)
        # and that comes to the same number of lines as the plotly chart draws
        expect_equal(x$xAxisZeroLineWidth > 0, xz)
        expect_equal(countShapes(do.call(Line, args)$htmlwidget), sum(xz, yz))
    }

    # The range still reaches zero for whichever axis asks for it, independently
    x <- widgetOf(num, data.label.show = TRUE, x.zero = TRUE, y.zero = FALSE,
                  data.label.auto.placement = TRUE)
    expect_equal(x$xAxisRangeMode, "tozero")
    expect_equal(x$yAxisRangeMode, "normal")
})

test_that("PPT export metadata matches the plotly line chart",
{
    auto <- suppressWarnings(Line(z, data.label.auto.placement = TRUE,
                                  data.label.show = TRUE))
    plain <- Line(z, data.label.show = TRUE)
    expect_equal(attr(auto, "ChartLabels"), attr(plain, "ChartLabels"))
    expect_equal(attr(auto, "ChartType"), attr(plain, "ChartType"))

    auto <- suppressWarnings(Line(z, data.label.auto.placement = TRUE,
        data.label.show = TRUE, data.label.prefix = "<"))
    plain <- Line(z, data.label.show = TRUE, data.label.prefix = "<")
    expect_equal(attr(auto, "ChartLabels"), attr(plain, "ChartLabels"))
})

test_that("Charting options with no widget equivalent warn instead of failing",
{
    expect_warning(Line(z, data.label.auto.placement = TRUE, data.label.show = TRUE,
                        marker.symbols = "square"),
                   "does not support the setting 'marker.symbols'")
    expect_warning(Line(z, data.label.auto.placement = TRUE, data.label.show = TRUE,
                        modebar.show = TRUE, x.data.reversed = TRUE),
                   "does not support the settings 'modebar.show', 'x.data.reversed'")
    # Defaults must not warn, including ones that get vectorized later on
    expect_warning(Line(z, data.label.auto.placement = TRUE, data.label.show = TRUE),
                   NA)
})

test_that("Marker border opacity is folded into the border color",
{
    x <- widgetOf(z, data.label.auto.placement = TRUE, data.label.show = TRUE,
                  marker.show = TRUE, marker.border.width = 2,
                  marker.border.colors = c("#FF0000", "#00FF00"),
                  marker.border.opacity = 0.4)
    expect_equal(unique(x$pointBorderColor),
                 c("rgba(255,0,0,0.4)", "rgba(0,255,0,0.4)"))

    # Hidden markers carry no border at all, so the widget does not draw one
    x <- widgetOf(z, data.label.auto.placement = TRUE, data.label.show = TRUE,
                  marker.border.colors = "#FF0000", marker.border.opacity = 0.4)
    expect_equal(unique(x$pointBorderColor), "")
})

test_that("Arguments defaulting to another argument only warn when they diverge",
{
    # marker.colors defaults to colors, and passing the series colors is exactly right,
    # so it must only warn when the caller asks for something different
    expect_warning(Line(z, data.label.auto.placement = TRUE, data.label.show = TRUE,
                        colors = c("#FF0000", "#00FF00")), NA)
    expect_warning(Line(z, data.label.auto.placement = TRUE, data.label.show = TRUE,
                        marker.colors = c("#FF0000", "#00FF00")),
                   "does not support the setting 'marker.colors'")
    expect_warning(Line(z, data.label.auto.placement = TRUE, data.label.show = TRUE,
                        colors = c("#FF0000", "#00FF00"),
                        marker.colors = c("#FF0000", "#00FF00")), NA)

    # legend.fill.color defaults to background.fill.color, likewise
    expect_warning(Line(z, data.label.auto.placement = TRUE, data.label.show = TRUE,
                        background.fill.color = "#EEEEEE"), NA)
    expect_warning(Line(z, data.label.auto.placement = TRUE, data.label.show = TRUE,
                        legend.fill.color = "#EEEEEE"),
                   "does not support the setting 'legend.fill.color'")

    expect_warning(Line(z, data.label.auto.placement = TRUE, data.label.show = TRUE,
                        legend.border.color = "#FF00FF"),
                   "does not support the setting 'legend.border.color'")
})

test_that("Marker opacity is judged by the value the markers end up with",
{
    auto <- function(...) Line(z, data.label.auto.placement = TRUE,
                              data.label.show = TRUE, ...)

    # marker.opacity defaults to opacity, so a per-series opacity leaves the markers with
    # per-series values the widget cannot draw. The warning names the argument that was
    # actually passed, and says it is only the markers that miss out, because the lines
    # do get an opacity each.
    expect_warning(auto(opacity = c(0.3, 1), marker.show = TRUE),
                   "does not support the setting 'opacity \\(for the markers\\)'")
    expect_warning(auto(marker.opacity = c(0.3, 1), marker.show = TRUE),
                   "does not support the setting 'marker.opacity'")

    # Nothing is lost while the markers are hidden, which is the default
    expect_warning(auto(opacity = c(0.3, 1)), NA)
    expect_warning(auto(marker.opacity = c(0.3, 1)), NA)
    expect_warning(auto(opacity = c(0.3, 1), marker.show = FALSE), NA)

    # One opacity for the whole chart is supported either way
    expect_warning(auto(opacity = 0.5, marker.show = TRUE), NA)
    expect_warning(auto(marker.opacity = 0.5, marker.show = TRUE), NA)

    # Markers drawn only at the ends of the series are still markers
    expect_warning(auto(opacity = c(0.3, 1), marker.show.at.ends = TRUE),
                   "'opacity \\(for the markers\\)'")

    # The line opacity is honoured per series, so it is not part of the complaint
    x <- widgetOf(z, data.label.auto.placement = TRUE, data.label.show = TRUE,
                  colors = c("#FF0000", "#00FF00"), opacity = c(0.3, 1))
    expect_equal(as.character(jsonlite::fromJSON(as.character(x$lineColors))),
                 c("rgba(255,0,0,0.3)", "rgba(0,255,0,1)"))
})

test_that("Every fit line is given a color, so the widget can draw them all",
{
    x <- widgetOf(z, data.label.auto.placement = TRUE, data.label.show = TRUE,
                  colors = "red", fit.type = "Smooth", fit.CI.show = TRUE)
    fitColors <- function(v) as.character(jsonlite::fromJSON(as.character(v)))
    # One color for two series used to send a single entry, leaving the widget with
    # nothing to color the second fit line with
    expect_equal(fitColors(x$fitLineColors), rep("red", 2))
    expect_equal(fitColors(x$fitCILabelColors), rep("red", 2))
    expect_equal(length(fitColors(x$fitCIColors)), 2)

    x <- widgetOf(z, data.label.auto.placement = TRUE, data.label.show = TRUE,
                  colors = "red", fit.type = "Smooth",
                  average.series = rep(9, 5), average.color = "#123456")
    expect_equal(fitColors(x$fitLineColors), c("red", "red", "#123456"))
})

test_that("Automatic placement warns once it exceeds the widget's label limit",
{
    big <- matrix(1:120, 60, 2, dimnames = list(paste0("r", 1:60), c("A", "B")))
    expect_warning(Line(big, data.label.auto.placement = TRUE, data.label.show = TRUE),
                   "100 or more labels")
    small <- matrix(1:98, 49, 2, dimnames = list(paste0("r", 1:49), c("A", "B")))
    expect_warning(Line(small, data.label.auto.placement = TRUE,
                        data.label.show = TRUE), NA)
})

test_that("Other line chart features still render",
{
    expect_error(suppressWarnings(Line(z, data.label.auto.placement = TRUE,
        data.label.show = TRUE, average.series = c(1.5, 2.5, 3.5, 4.5, 5.5))), NA)
    expect_error(suppressWarnings(Line(z, data.label.auto.placement = TRUE,
        data.label.show = TRUE, fit.type = "Smooth")), NA)
    expect_error(suppressWarnings(Line(z, data.label.auto.placement = TRUE,
        data.label.show = TRUE, fit.type = "Smooth", fit.CI.show = TRUE)), NA)
    expect_error(suppressWarnings(Line(z[, 1, drop = FALSE],
        data.label.auto.placement = TRUE, data.label.show = TRUE)), NA)
    expect_error(suppressWarnings(Line(structure(z / 100, statistic = "%"),
        data.label.auto.placement = TRUE, data.label.show = TRUE)), NA)

    with.na <- structure(c(1, NA, 3, 4, NA, 2, 3, NA, 5, 6), .Dim = c(5L, 2L),
        .Dimnames = list(c("T", "U", "V", "W", "X"), c("A", "B")))
    expect_warning(Line(with.na, data.label.auto.placement = TRUE,
                        data.label.show = TRUE), "Missing values")
})
