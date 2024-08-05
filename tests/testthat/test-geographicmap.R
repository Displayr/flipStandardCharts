context("GeographicMap")
library(flipStandardCharts)
library(flipChartBasics)

austria.state.table <- structure(c(0.194518122589216, 0.0159799950197339, 0.639135647797957,
                                   0.102287880377844,
                                   0.0943967222701758, 0.963936795480549), .Names = c("Burgenland",
                                                                                      "Salzburg",
                                                                                      "Steiermark", "Tirol", "Vorarlberg", "Wien"))

tb.with.spaces <- structure(c(-10, 0, NA, 50, 60, 90, 150, 140, 180, 160, 1000, 1050, 2000,
                                        84, 150), .Dim = c(5L, 3L), .Dimnames = list(c(" france", " germany",
                                                                                       " usa", " brazil", " malaysia"), c("age", "weight", "IQ")))

world.multi.series.table <- structure(c(-10, 0, NA, 50, 60, 90, 150, 140, 180, 160, 1000, 1050, 2000,
                                        84, 150), .Dim = c(5L, 3L), .Dimnames = list(c("france", "germany",
                                                                                       "usa", "brazil", "malaysia"), c("age", "weight", "IQ")))

country.codes <- seq(4)
names(country.codes) <- c("AUS", "HUN", "SOM", "PAK")

region.pct <- structure(c(-0.17, 0.155, NA, 0.39) * 100, .Names = c("Midwest", "Northeast", "South", "West"), statistic = "%")

continents <- rnorm(5) * 100
names(continents) <- c("Asia", "Europe", "Oceania", "North America", "Africa")

aus.post <- structure(c(0.178899773163721, 0.351680957945064, 0.622020776616409,
                        0.367020988371223, 0.916988451732323, 0.570868947776034, 0.406481134006754,
                        0.931390148354694, 0.64271322870627, 0.0315834735520184), .Names = c("2220",
                                                                                             "7175", "4882", "872", "4737", "3345", "2799", "5350", "2774",
                                                                                             "6798"))
us.post <- structure(c(0.464362588245422, 0.313019739231095, 0.297980585601181,
                       0.185166553128511, 0.685781141044572, 0.267244004877284, 0.11511912709102,
                       0.016194264171645, 0.991070948075503, 0.130910981912166), .Names = c("15672",
                                                                                            "45434", "26253", "59225", "1129", "48710", "12106", "07439",
                                                                                            "26711", "90079"))
uk.post <- structure(c(0.0904847530182451, 0.978746091481298, 0.25075147789903,
                       0.0231262978632003, 0.0964917673263699, 0.151060470147058, 0.269070032751188,
                       0.0642851775046438, 0.141419374151155, 0.847822815412655), .Names = c("DA13",
                                                                                             "GU51", "G22", "BA2", "L25", "TQ6", "WD5", "E77", "EX22", "G14"
                       ))

opts <- c('leaflet_nazero' = 'mapping.package = "leaflet", treat.NA.as.0 = TRUE, legend.show = FALSE, values.hovertext.format = ".3%", show.missing.regions = FALSE',
          'plotly_nazero' = 'mapping.package = "plotly", treat.NA.as.0 = TRUE, legend.show = FALSE, values.hovertext.format = ".3%", background = TRUE, show.missing.regions = FALSE',
          'leaflet_colors' = 'mapping.package = "leaflet", color.NA = "#f4aa42", colors = c("#301f68", "#c11d3b"), ocean.color = "#abb280", legend.title = "SUPER", values.hovertext.format = ",.1f"',
          'plotly_colors' = 'mapping.package ="plotly", color.NA = "#f4aa42", colors = c("#301f68", "#c11d3b"), ocean.color = "#abb280", legend.title = "SUPER", values.hovertext.format = ",.1f"')

leaflet.only <- c("austria.state.table", "continents", "aus.post", "uk.post")
# "us.post") # Temporarily skip seg fault so we can get other test results for now
# Note this error does not occur locally or on the Standard R Tests

both.packages <- c("world.multi.series.table", "country.codes", "region.pct", "tb.with.spaces")
dat.list <- c(leaflet.only, both.packages)

for (dat in dat.list)
{

    for (ii in 1:length(opts))
    {
        # Do not test charts with plotly that cannot be plotted with plotly
        if (dat %in% leaflet.only && grepl("plotly", names(opts)[ii], fixed = TRUE))
            next

        # Create name which will appear in the error message if test fails
        # Filestem should be prefixed by test file name to avoid name conflicts
        filestem <- paste0("geographicmap-", dat, "-", names(opts)[ii])

        test_that(filestem, {

            # Create command that will create widget
            cmd <- paste0("pp <- GeographicMap(", dat, ",", opts[ii], ")")

            # Run command and check outputs
            if (grepl("world.multi.series.table-plotly", filestem, fixed = TRUE))
                expect_warning(eval(parse(text=cmd)), "Only the first series")
            else if (grepl("tb.with.spaces-plotly", filestem, fixed = TRUE))
                expect_warning(eval(parse(text=cmd)), "Only the first series")
            else
                expect_error(eval(parse(text=cmd)), NA)

            #print(pp)
            #readline(prompt=paste0(filestem, ": press [enter] to continue: "))
        })
    }
}

test_that ("Country states", {
    expect_error(sapply(CountriesOrContinents("country"), StatesInCountry), NA)
})

test_that ("Postcodes by country", {
    expect_error(sapply(c("US", "UK", "Australia"), ZipcodesInCountry), NA)
})

test_that ("No zip codes", {
    expect_error(ZipcodesInCountry("Portugal"), "Zip code or postcode mapping data is not avaialble for this country.")
})

test_that ("Areas in Australia", {
    expect_error(AreasInCountry("Australia"), NA)
})

names(country.codes)[1] <- "XXX"
test_that ("Unmatched name warning", {
    expect_warning(GeographicMap(country.codes), "Unmatched region names: XXX")
})

test_that ("Ambiguous names DS-2016", {
    ambiguous <- seq(3)
    names(ambiguous) <- c("Georgia", "Indiana", "Virginia")
    expect_error(GeographicMap(ambiguous), NA)
})

names(austria.state.table)[1:5] <- c("aaaaa", "bbbbb", "ccccc", "ddddd", "eeeee")
test_that ("Most unmatched", {
    expect_warning(GeographicMap(austria.state.table), "5 rows of the input data were not matched.")
})

names(austria.state.table) <- c("aaaaa", "bbbbb", "ccccc", "ddddd", "eeeee", "fffff")
test_that ("All unmatched", {
    expect_error(GeographicMap(austria.state.table), "No rows of the input data were matched with geographic entity names.")
})

mat <- structure(c(2031, 2032, 2121, 2037, 2, 3, 4, 5),
                 .Dim = c(4L, 2L), .Dimnames = list(NULL, c("postcode", "val")))
test_that ("Warning using first column as rownames", {
    expect_warning(GeographicMap(mat), "The first column of the input data has been used as the geographic entitites.")
})

test_that("DS-3647: Can recognize new counties of Norway",
{
    tbl <- as.matrix(c(Oslo = 9, Rogaland = 10, "M\u00F8re og Romsdal" = 7.5,
                       Nordland = 9, Viken = 9.5, Innlandet = 9,
                       `Vestfold og Telemark` = 10,
                       Agder = 7, Vestland = 11.5, "Tr\u00F8ndelag" = 8.5,
                       `Troms og Finnmark` = 9))
    expect_silent(GeographicMap(tbl, mapping.package = "leaflet"))
})
