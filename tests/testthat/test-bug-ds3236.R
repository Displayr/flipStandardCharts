# For backwards compatibility, check that we do not add an extra percentage sign
# to value axis tick labels and data labels
# (these may have been manually added to the user to work around previous
# handling of percentage data)

context("Bug-DS3236")

data.with.stats <- structure(c(2.75482093663912, 6.06060606060606, 12.6721763085399,
18.4573002754821, 24.7933884297521, 15.9779614325069, 6.06060606060606,
8.26446280991736, 4.95867768595041, 100, 3.77906976744186, 15.9883720930233,
7.84883720930233, 18.0232558139535, 19.7674418604651, 13.0813953488372,
10.7558139534884, 4.06976744186047, 6.68604651162791, 100, 3.25318246110325,
10.8910891089109, 10.3253182461103, 18.2461103253182, 22.3479490806223,
14.5685997171146, 8.34512022630834, 6.22347949080622, 5.7991513437058,
100, 0.442913092343004, 0.0000228306627828578, 0.0351514682974756,
0.881274082835059, 0.108843509396061, 0.275202305069102, 0.0240561692086175,
0.0210216801333983, 0.326003170694519, NA, 0.442913092343004,
0.0000228306627828578, 0.0351514682974756, 0.881274082835059,
0.108843509396061, 0.275202305069102, 0.0240561692086175, 0.0210216801333983,
0.326003170694519, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
), .Dim = c(10L, 3L, 2L), .Dimnames = list(c("Less than $15,000",
"$15,001 to $30,000", "$30,001 to $45,000", "$45,001 to $60,000",
"$60,001 to $90,000", "$90,001 to $120,000", "$120,001 to $150,000",
"$150,001 to $200,000", "$200,001 or more", "NET"), c("Male",
"Female", "NET"), c("Column %", "p")), name = "Income by Gender", questions = c("Income",
"Gender"))

test_that("Check for extra percent sign",
{
    expect_warning(Palm(data.with.stats[-10,,], y.tick.suffix = "%"),
                   "A percentage sign is automatically added to ",
                    "percent data. The first '%' in the suffix will be ignored.")

    for (func in c("Column", "ColumnMultiColor", "Line", "Area", "Radar",
                    "StackedColumnWithStatisticalSignificance"))
    {
        cmd <- paste0("res <- ", func, "(data.with.stats[-10,,], data.label.show = TRUE,",
                      "data.label.suffix = '%', y.tick.suffix = '%')")
        expect_warning(eval(parse(text=cmd)), "A percentage sign is automatically added to ",
                       "percent data. The first '%' in the suffix will be ignored.")
    }

    for (func in c("Bar", "BarMultiColor", "Pyramid"))
    {
        cmd <- paste0("res <- ", func, "(data.with.stats[-10,,], data.label.show = TRUE,",
                      "data.label.suffix = '%', x.tick.suffix = '%')")
        expect_warning(eval(parse(text=cmd)))
    }
})
