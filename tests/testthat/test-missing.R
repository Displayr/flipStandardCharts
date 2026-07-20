context("Missing values")
dat <- structure(c(11, 10, 6, 6, 5, 4, 4, 4, 3, 2, 2, 2, 1, 1, 1, 1,
    1, 1, 0, 0, 0, 0, 0, 0, 0, 65, 9, 6, 5, 5, 3, 3, 2, 2, 2, 5,
    4, 1, 2, 2, 1, 0, 0, 0, 5, 3, 2, 1, 0, 0, 0, 63, 8, 4, 6, 2,
    2, 0, 4, 4, 2, 3, 1, 3, 3, 0, 0, 3, 1, 0, 2, 8, 1, 0, 3, 2, 1,
    65), .Dim = c(26L, 3L), .Dimnames = list(c(" Norway (NOR)",
    " Germany (GER)", " Canada (CAN)", " Netherlands (NED)",
    " United States (USA)", " Sweden (SWE)", " Austria (AUT)",
    " France (FRA)", " South Korea (KOR)", " Japan (JPN)",
    " Switzerland (SUI)", " Italy (ITA)", " Czech Republic (CZE)",
    " Slovakia (SVK)", " Belarus (BLR)", " Great Britain (GBR)",
    " Poland (POL)", " Ukraine (UKR)", " China (CHN)", " Olympic Athletes from Russia (OAR)",
    " Australia (AUS)", " Slovenia (SLO)", " Finland (FIN)",
    " Spain (ESP)", " Kazakhstan (KAZ)", NA), c("Gold", "Silver",
    "Bronze")), statistic = structure("NOC", .Names = "Rank"))

test_that("Missing rownames",
{
    expect_silent(Column(dat))
})

set.seed(1234)
n <- 1e4
tb <- matrix(0, n, 10)
colnames(tb) <- LETTERS[1:ncol(tb)]
tb[,1] <- NA
tb[seq(1, n, by = 2), 2] <- NA
tb[seq(1, n, by = 3), 3] <- NA
tb[seq(1, n, by = 4), 4] <- NA
tb[seq(1, n, by = 5), 5] <- NA
tb[seq(1, n, by = 6), 6] <- NA
tb[seq(1, n, by = 10), 7] <- NA
tb[seq(1, n, by = 100), 8] <- NA
tb[seq(1, n, by = 1000), 9] <- NA
tb[seq(1, n, by = 10000), 10] <- NA

small <- cbind(
    Dog = 1:10,
    Elephant = c(1:5, NA, NA, 8:10),
    'Bugs & Spiders' = rep(1,10),
    Fish = c(1:4, rep(NA, 6))
)

test_that("MissingCasesPlot",
{
    # This should create a blank plot
    expect_error(MissingCasesPlot(dat), NA)
    expect_error(MissingCasesPlot(tb), NA)
    expect_error(MissingCasesPlot(small), NA)
})

test_that("Pyramid handles missing values",
{
    x <- c(1e-04, 2, 3, 4, NA, 0, 5)
    expect_warning(Pyramid(x), "Missing")
})

## MissingCasesPlot - additional data-pinned scenarios (RS-22827)
## small has 10 rows, columns Dog (0 missing), Elephant (2 missing),
## Bugs & Spiders (0 missing), Fish (6 missing).

# Fixture for row-count > 200 scatter-only branch
set.seed(1)
bigMatrix <- matrix(sample(c(NA, 1), 210 * 3, replace = TRUE), nrow = 210)

# Subset vectors
subset.mismatched <- c(TRUE, FALSE)
subset.matching <- c(rep(TRUE, 5), rep(FALSE, 5))

# Fixtures for fill-opacity auto-scaling
bigMissing <- matrix(1, nrow = 1100, ncol = 2)
bigMissing[1:1050, 1] <- NA
colnames(bigMissing) <- c("A", "B")

medMissing <- matrix(1, nrow = 500, ncol = 2)
medMissing[1:300, 1] <- NA
colnames(medMissing) <- c("A", "B")

# Helper: pull out the fill line color from the built plotly object
# (i.e. the "lines" trace drawn over the missing cells, not the base trace)
getFillLineColor <- function(p)
{
    built <- plotly::plotly_build(p)$x
    for (tr in built$data)
        if (identical(tr$mode, "lines"))
            return(tr$line$color)
    NULL
}

test_that("Errors when subset length doesn't match data rows",
{
    expect_error(MissingCasesPlot(small, subset = subset.mismatched),
                 "Filters must be from the same data set as the input variables.")
})

test_that("Subset filters out excluded rows",
{
    built <- plotly::plotly_build(MissingCasesPlot(small, subset = subset.matching))$x
    expect_equal(built$layout$yaxis$range, c(5, 1))
    # subset.matching keeps rows 1-5 only. Elephant's 2 missing values are in
    # rows 6-7 (excluded), so its missing count should drop from 2 to 0.
    # Fish's missing values are in rows 5-10; only row 5 remains, so its
    # missing count should drop from 6 to 1 (of the 5 remaining rows).
    labels <- built$layout$xaxis$ticktext
    names(labels) <- c("Dog", "Elephant", "Bugs & Spiders", "Fish")
    expect_match(labels[["Elephant"]], "0 cases")
    expect_match(labels[["Fish"]], "1 cases")
})

test_that("Auto x-axis angle: many columns (ncol >= 10) rotates ticks",
{
    built <- plotly::plotly_build(MissingCasesPlot(tb))$x
    expect_equal(built$layout$xaxis$tickangle, 90)
})

test_that("Auto x-axis angle: few columns (ncol < 10) keeps ticks horizontal",
{
    built <- plotly::plotly_build(MissingCasesPlot(small))$x
    expect_equal(built$layout$xaxis$tickangle, 0)
})

test_that("Label wrap follows explicit x.tick.label.wrap when angle is 0",
{
    # x.tick.angle = 90 means the default wrap (angle == 0) would be FALSE;
    # explicitly passing x.tick.label.wrap = TRUE should override that default.
    built <- plotly::plotly_build(MissingCasesPlot(small, x.tick.angle = 90,
                                                     x.tick.label.wrap = TRUE))$x
    expect_true(all(grepl("<br>", built$layout$xaxis$ticktext)))
})

test_that("show.counts.missing adds a count to x-axis labels",
{
    built <- plotly::plotly_build(MissingCasesPlot(small, show.counts.missing = TRUE,
                                                     show.percentages.missing = FALSE))$x
    labels <- built$layout$xaxis$ticktext
    names(labels) <- c("Dog", "Elephant", "Bugs & Spiders", "Fish")
    expect_true(all(grepl("cases", labels)))
    expect_match(labels[["Dog"]], "0 cases")
    expect_match(labels[["Elephant"]], "2 cases")
    expect_match(labels[["Bugs & Spiders"]], "0 cases")
    expect_match(labels[["Fish"]], "6 cases")
})

test_that("show.percentages.missing adds a percentage to x-axis labels",
{
    built <- plotly::plotly_build(MissingCasesPlot(small, show.counts.missing = FALSE,
                                                     show.percentages.missing = TRUE))$x
    labels <- built$layout$xaxis$ticktext
    names(labels) <- c("Dog", "Elephant", "Bugs & Spiders", "Fish")
    expect_true(all(grepl("%", labels)))
    expect_match(labels[["Dog"]], "0% missing")
    expect_match(labels[["Elephant"]], "20% missing")
    expect_match(labels[["Bugs & Spiders"]], "0% missing")
    expect_match(labels[["Fish"]], "60% missing")
})

test_that("Both counts and percentages combine with \"or\"",
{
    built <- plotly::plotly_build(MissingCasesPlot(small, show.counts.missing = TRUE,
                                                     show.percentages.missing = TRUE))$x
    label <- built$layout$xaxis$ticktext[2] # Elephant
    pos.cases <- regexpr("cases", label)
    pos.or <- regexpr("or", label)
    pos.pct <- regexpr("%", label)
    expect_true(pos.cases > 0 && pos.or > pos.cases && pos.pct > pos.or)
    expect_match(label, "2 cases")
    expect_match(label, "20%")
})

test_that("Neither counts nor percentages: label has no missing-count suffix",
{
    built <- plotly::plotly_build(MissingCasesPlot(small, show.counts.missing = FALSE,
                                                     show.percentages.missing = FALSE))$x
    expect_equal(built$layout$xaxis$ticktext,
                 paste0("<b>", colnames(small), "</b>"))
    expect_false(any(grepl("missing", built$layout$xaxis$ticktext)))
})

test_that("data.label.show = TRUE adds one annotation per missing cell",
{
    built <- plotly::plotly_build(MissingCasesPlot(small, data.label.show = TRUE))$x
    # base annotations = 1 (title only; subtitle/footer default to "")
    expect_equal(length(built$layout$annotations), 1 + 8)
})

test_that("data.label.position maps to correct yanchor",
{
    positions <- list(above = "bottom", below = "top", center = "center")
    for (pos in names(positions))
    {
        built <- plotly::plotly_build(MissingCasesPlot(small, data.label.show = TRUE,
                                                         data.label.position = pos))$x
        expect_equal(built$layout$annotations[[2]]$yanchor, positions[[pos]], info = pos)
    }
})

test_that("data.label.show = FALSE adds no extra annotations",
{
    built <- plotly::plotly_build(MissingCasesPlot(small, data.label.show = FALSE))$x
    expect_equal(length(built$layout$annotations), 1)
})

test_that("tooltip.show = TRUE sets hovermode and hover text",
{
    built <- plotly::plotly_build(MissingCasesPlot(small, tooltip.show = TRUE))$x
    expect_equal(built$layout$hovermode, "closest")
    lines.trace <- Filter(function(tr) identical(tr$mode, "lines"), built$data)[[1]]
    expect_equal(unique(lines.trace$hoverinfo), c("text", NA))
    expect_true(any(grepl("missing from", lines.trace$text)))
})

test_that("tooltip.show = FALSE disables hover",
{
    built <- plotly::plotly_build(MissingCasesPlot(small, tooltip.show = FALSE))$x
    expect_equal(built$layout$hovermode, FALSE)
    lines.trace <- Filter(function(tr) identical(tr$mode, "lines"), built$data)[[1]]
    expect_equal(unique(lines.trace$hoverinfo), c("skip", NA))
})

test_that("enable.zoom = FALSE fixes axis range",
{
    built <- plotly::plotly_build(MissingCasesPlot(small, enable.zoom = FALSE))$x
    expect_true(built$layout$xaxis$fixedrange)
    expect_true(built$layout$yaxis$fixedrange)
    expect_equal(built$layout$dragmode, FALSE)
})

test_that("font.unit = \"pt\" scales all font sizes by 1.3333 (rounded)",
{
    built <- plotly::plotly_build(MissingCasesPlot(small, font.unit = "pt"))$x
    expect_equal(built$layout$annotations[[1]]$font$size, round(16 * 1.3333, 0)) # title 16 -> 21
    expect_equal(built$layout$xaxis$tickfont$size, round(11 * 1.3333, 0)) # x tick 11 -> 15
    expect_equal(built$layout$yaxis$tickfont$size, round(10 * 1.3333, 0)) # y tick 10 -> 13
})

test_that("font.unit = \"px\" (default) leaves font sizes unchanged",
{
    built <- plotly::plotly_build(MissingCasesPlot(small))$x
    expect_equal(built$layout$annotations[[1]]$font$size, 16)
})

test_that("Auto fill-opacity: high missing density (> 1000) sets opacity 0.2",
{
    expect_equal(getFillLineColor(MissingCasesPlot(bigMissing, fill.opacity = NULL)),
                 rgb(t(col2rgb("#5C9AD3")), maxColorValue = 255, alpha = 255 * 0.2))
})

test_that("Auto fill-opacity: medium density (> 200, <= 1000) sets opacity 0.5",
{
    expect_equal(getFillLineColor(MissingCasesPlot(medMissing, fill.opacity = NULL)),
                 rgb(t(col2rgb("#5C9AD3")), maxColorValue = 255, alpha = 255 * 0.5))
})

test_that("Auto fill-opacity: low density (<= 200) sets opacity 1.0",
{
    expect_equal(getFillLineColor(MissingCasesPlot(small)),
                 rgb(t(col2rgb("#5C9AD3")), maxColorValue = 255, alpha = 255 * 1.0))
})

test_that("Explicit fill.opacity bypasses auto-scaling",
{
    expect_equal(getFillLineColor(MissingCasesPlot(small, fill.opacity = 0.7)),
                 rgb(t(col2rgb("#5C9AD3")), maxColorValue = 255, alpha = 255 * 0.7))
})

test_that("All-missing data uses fill.color as the base color",
{
    allNA <- matrix(NA, nrow = 5, ncol = 2)
    colnames(allNA) <- c("X", "Y")
    built <- plotly::plotly_build(MissingCasesPlot(allNA))$x
    heatmap.trace <- Filter(function(tr) identical(tr$type, "heatmap"), built$data)[[1]]
    # both colorscale endpoints collapse to the fill color (#5C9AD3)
    expect_equal(heatmap.trace$colorscale[1, 2], heatmap.trace$colorscale[2, 2])
    expect_equal(heatmap.trace$colorscale[1, 2], "rgba(92,154,211,1)")
})

test_that("All-present data (no missing) uses base.color as fill",
{
    noNA <- matrix(1, nrow = 5, ncol = 2)
    colnames(noNA) <- c("X", "Y")
    built <- plotly::plotly_build(MissingCasesPlot(noNA))$x
    heatmap.trace <- Filter(function(tr) identical(tr$type, "heatmap"), built$data)[[1]]
    # both colorscale endpoints collapse to the base color (#E6E6E6)
    expect_equal(heatmap.trace$colorscale[1, 2], heatmap.trace$colorscale[2, 2])
    expect_equal(heatmap.trace$colorscale[1, 2], "rgba(230,230,230,1)")
    # na.ind is empty, so no extra "lines" trace is added - only the heatmap trace
    expect_equal(length(built$data), 1)
})

test_that("Row count > 200 switches to scatter-only base trace (no heatmap)",
{
    built <- plotly::plotly_build(MissingCasesPlot(bigMatrix))$x
    expect_equal(built$data[[1]]$type, "scatter")
    expect_equal(built$data[[1]]$mode, "none")
    expect_false(any(sapply(built$data, function(tr) identical(tr$type, "heatmap"))))
})

test_that("Row count <= 200 uses heatmap trace",
{
    built <- plotly::plotly_build(MissingCasesPlot(small))$x
    expect_true(any(sapply(built$data, function(tr) identical(tr$type, "heatmap"))))
})

test_that("Unnamed columns get default \"Variable N\" labels",
{
    m <- small
    colnames(m) <- NULL
    built <- plotly::plotly_build(MissingCasesPlot(m))$x
    expect_match(built$layout$xaxis$ticktext[1], "<b>Variable 1</b>")
    expect_match(built$layout$xaxis$ticktext[2], "<b>Variable 2</b>")
    expect_match(built$layout$xaxis$ticktext[3], "<b>Variable 3</b>")
    expect_match(built$layout$xaxis$ticktext[4], "<b>Variable 4</b>")
})

