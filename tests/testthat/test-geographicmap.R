context("GeographicMap")
library(flipStandardCharts)
library(flipChartBasics)

austria.state.table <- structure(c(0.194518122589216, 0.0159799950197339, 0.639135647797957,
                                   0.102287880377844,
                                   0.0943967222701758, 0.963936795480549), .Names = c("Burgenland",
                                                                                      "Salzburg",
                                                                                      "Steiermark", "Tirol", "Vorarlberg", "Wien"))

world.multi.series.table <- structure(c(-10, 0, NA, 50, 60, 90, 150, 140, 180, 160, 1000, 1050, 2000,
                                        84, 150), .Dim = c(5L, 3L), .Dimnames = list(c("france", "germany",
                                                                                       "usa", "brazil", "malaysia"), c("age", "weight", "IQ")))

country.codes <- seq(4)
names(country.codes) <- c("AUS", "HUN", "SOM", "PAK")

region.pct <- structure(c(-0.17, 0.155, NA, 0.39), .Names = c("Midwest", "Northeast", "South", "West"), statistic = "%")

continents <- rnorm(5) * 100
names(continents) <- c("Asia", "Europe", "Oceania", "North America", "Africa")

opts <- c('leaflet_nazero' = 'mapping.package = "leaflet", treat.NA.as.0 = TRUE, legend.show = FALSE, values.hovertext.format = ".3%"',
          'plotly_nazero' = 'mapping.package = "plotly", treat.NA.as.0 = TRUE, legend.show = FALSE, values.hovertext.format = ".3%"',
          'leaflet_colors' = 'mapping.package = "leaflet", color.NA = "#f4aa42", colors = c("#301f68", "#c11d3b"), ocean.color = "#abb280", legend.title = "SUPER", values.hovertext.format = ",.1f"',
          'plotly_colors' = 'mapping.package ="plotly", color.NA = "#f4aa42", colors = c("#301f68", "#c11d3b"), ocean.color = "#abb280", legend.title = "SUPER", values.hovertext.format = ",.1f"')

leaflet.only <- c("austria.state.table", "continents")
both.packages <- c("world.multi.series.table", "country.codes", "region.pct")
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
            expect_error(suppressWarnings(eval(parse(text=cmd))), NA)

            print(pp)
            readline(prompt=paste0(filestem, ": press [enter] to continue: "))
        })
    }
}

