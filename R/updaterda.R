#' Read and process the various definitions of geographic regions
#' to update the sysdata.rda file (for use in mapping functions)
#'
#' @return NULL. Updates an rda file with the following components
#' \itemize{
#' \item \code{missing110} - a \code{\link{list}} of strings of the names and
#'       alternative names of countries that are in \code{map.coordinates.110} but not \code{map.coordinates.50}
#' \item \code{admin1.name.map} - a named \code{\link{list}} of countries, where each item is a named \code{\link{list}}
#'       of states, within which each item is a \code{\link{vector}} of alternative names of that state.
#' \item \code{admin0.name.map.by.name} - NOT USED.
#' \item \code{admin0.name.map.by.admin} - a named \code{\link{list}} of countries, where each item is a \code{\link{vector}}
#'       of alternative names and abbreviations of that country.
#' \item \code{admin1.coordinates} - a \code{\link{SpatialPolygonsDataFrame}} of regional data and polygons.
#' \item \code{map.coordinates.50} - a \code{\link{SpatialPolygonsDataFrame}} of country data and polygons at the
#'       higher resolution of 1:50,000,000
#' \item \code{map.coordinates.110} - a \code{\link{SpatialPolygonsDataFrame}} of country data and polygons at the
#'       higher resolution of 1:110,000,000
#' \item \code{country.coordinates} - NOT USED.
#' \item \code{ISO_3166_1} - a \code{\link{data.frame}} of country names.
#' \item \code{ISO_3166_2} - a \code{\link{data.frame}} of region names.
#' \item \code{country.center.coords} - a \code{\link{data.frame}} of the latitude and longitude of the center of each country.
#' }
#' @importFrom XML readHTMLTable
#' @importFrom httr GET content
#' @importFrom devtools use_data
#' @importFrom rgdal readOGR
#' @importFrom utils download.file unzip data
#' @noRd
updateGeographicData <- function() {

    # ISO codes of countries and regions
    data("ISO_3166_1",  package = "ISOcodes", envir = environment())
    data("ISO_3166_2", package = "ISOcodes", envir = environment())

    # TODO delete below
    # base.url <- "https://raw.githubusercontent.com/datasets/geo-boundaries-world-110m/master/countries.geojson"
    # download.file(base.url, f <- tempfile())
    # country.coordinates <- readOGR(f, "OGRGeoJSON")
    #
    # country.coordinates.df <- data.frame(country.coordinates)
    # column.class <- sapply(country.coordinates.df, class)
    # column.class <- column.class[column.class == "factor"]
    # rm(country.coordinates.df)
    #
    # for (column in names(column.class))
    #     Encoding(levels(country.coordinates[[column]])) <- "UTF-8"

    # Code source: http://stackoverflow.com/questions/29118059/display-spatialpolygonsdataframe-on-leaflet-map-with-r

    # Higher resolution country data
    download.file(file.path('http://www.naturalearthdata.com/http/',
                            'www.naturalearthdata.com/download/50m/cultural',
                            'ne_50m_admin_0_countries.zip'), f <- tempfile())
    unzip(f, exdir=tempdir())
    map.coordinates.50 <- readOGR(tempdir(), 'ne_50m_admin_0_countries', encoding='UTF-8')
    map.coordinates.df <- data.frame(map.coordinates.50)
    column.class <- sapply(map.coordinates.df, class)
    column.class <- column.class[column.class == "factor"]
    rm(map.coordinates.df)
    for (column in names(column.class))
        levels(map.coordinates.50[[column]]) <- enc2utf8(levels(map.coordinates.50[[column]]))

    # Lower resolution country data
    download.file(file.path('http://www.naturalearthdata.com/http/',
                            'www.naturalearthdata.com/download/110m/cultural',
                            'ne_110m_admin_0_countries.zip'), f <- tempfile())
    unzip(f, exdir=tempdir())
    map.coordinates.110 <- readOGR(tempdir(), 'ne_110m_admin_0_countries', encoding='UTF-8')
    map.coordinates.df <- data.frame(map.coordinates.110)
    column.class <- sapply(map.coordinates.df, class)
    column.class <- column.class[column.class == "factor"]
    rm(map.coordinates.df)
    for (column in names(column.class))
        levels(map.coordinates.110[[column]]) <- enc2utf8(levels(map.coordinates.110[[column]]))

    # TODO temp comment out
    # State data
    # f <- tempfile()
    # download.file(file.path('http://www.naturalearthdata.com/http/',
    #                         'www.naturalearthdata.com/download/10m/cultural',
    #                         'ne_10m_admin_1_states_provinces.zip'), f)
    # d <- tempdir()
    # unzip(f, exdir = d)
    # admin1.coordinates <- readOGR(d, 'ne_10m_admin_1_states_provinces')
    # admin1.coordinates.df <- data.frame(admin1.coordinates)
    # column.class <- sapply(admin1.coordinates.df, class)
    # column.class <- column.class[column.class == "factor"]
    # rm(admin1.coordinates.df)
    # for (column in names(column.class))
    #     Encoding(levels(admin1.coordinates[[column]])) <- "UTF-8"


    # Mapping of countries to alternative names
    #admin0.name.map.by.name <- makeNameMap("name")
    #admin0.name.map.by.admin <- makeNameMap("admin")
    map.coordinates <- data.frame(map.coordinates.50)
    admin0.name.map.by.admin <- list()
    for (i in seq_len(nrow(map.coordinates)))
    {
        country.name <- as.character(map.coordinates[["admin"]][i])

        columns <- c("admin", "adm0_a3", "geounit", "gu_a3", "subunit", "su_a3",
                     "name", "name_long", "brk_a3", "brk_name", "abbrev", "postal",
                     "formal_en", "formal_fr", "name_sort", "name_alt",
                     "iso_a2", "iso_a3", "wb_a2", "wb_a3", "adm0_a3_is", "adm0_a3_us")

        all.names <- as.character(unlist(map.coordinates[i, columns]))
        all.names[all.names == "-99"] <- NA
        all.names <- all.names[!is.na(all.names)]
        all.names <- unique(all.names)
        all.names <- all.names[all.names != country.name]

        iso.match <- match(country.name, ISO_3166_1$Name)
        if (is.na(iso.match))
        {
            iso.match <- match(all.names, ISO_3166_1$Name)
            if (any(!is.na(iso.match)))
                iso.match <- iso.match[!is.na(iso.match)]

            iso.match <- iso.match[1]
        }

        if (!is.na(iso.match))
        {
            columns <- c("Alpha_2", "Alpha_3", "Name", "Official_name", "Common_name")
            iso.names <- unlist(ISO_3166_1[iso.match, columns])
            iso.names <- iso.names[!is.na(iso.names)]
            all.names <- unique(c(all.names, iso.names))
        }

        all.names <- all.names[all.names != country.name]

        if (length(all.names) == 0)
            next

        admin0.name.map.by.admin[[country.name]] <- all.names
    }


    # Mapping of states to alternative names
    admin1.name.map <- local(
        {
            admin1.coordinates <- data.frame(admin1.coordinates)

            ISO_3166_1$Country <- rep(NA, nrow(ISO_3166_1))
            for (i in seq_len(nrow(ISO_3166_1)))
            {
                country <- ISO_3166_1$Name[i]
                if (country %in% names(admin0.name.map.by.admin))
                {
                    ISO_3166_1$Country[i] <- country
                }
                else
                {
                    for (admin in names(admin0.name.map.by.admin))
                    {
                        alt <- admin0.name.map.by.admin[[admin]]
                        if (country %in% alt)
                        {
                            ISO_3166_1$Country[i] <- admin
                            break
                        }
                    }
                }
            }

            iso.3166 <- merge(ISO_3166_1[, c("Alpha_2", "Country")], ISO_3166_2,
                              by.x = "Alpha_2", by.y = "Country")

            iso.3166$Code <- substring(iso.3166$Code, 4)
            iso.3166$Code[!grepl("^[[:alpha:]]+$", iso.3166$Code)] <- NA

            name.map <- list()
            for (i in seq_len(nrow(admin1.coordinates)))
            {
                state <- as.character(admin1.coordinates$name[i])
                if (is.na(state))
                    next

                country <- as.character(admin1.coordinates$admin[i])
                country.name.map <- name.map[[country]]
                if (is.null(country.name.map))
                    country.name.map <- list()

                columns <- c("abbrev", "postal", "woe_name", "gn_name")
                all.names <- as.character(unlist(admin1.coordinates[i, columns]))
                all.names[all.names == "-99"] <- NA

                name.alt <- as.character(admin1.coordinates$name_alt[i])
                all.names <- c(all.names, strsplit(name.alt, "|", fixed = TRUE)[[1]])
                name.local <- as.character(admin1.coordinates$name_local[i])
                all.names <- c(all.names, strsplit(name.local, "|", fixed = TRUE)[[1]])

                iso.code <- iso.3166$Code[iso.3166$Country == country & iso.3166$Name == state]
                all.names <- c(all.names, iso.code)

                all.names <- all.names[!is.na(all.names)]
                all.names <- unique(all.names)
                all.names <- all.names[all.names != state]

                if (length(all.names) == 0)
                    next

                country.name.map[[state]] <- all.names
                name.map[[country]] <- country.name.map
            }

            name.map
        })

    # The file at 1:110m is missing a few small countries compared to the 1:50m data,
    # notably Singapore and Hong Kong
    missing110 <- local({
        missing.countries <- setdiff(levels(map.coordinates.50[["name"]]),
                                     levels(map.coordinates.110[["name"]]))
        alt.names <- admin0.name.map.by.admin[missing.countries]
        alt.names <- c(names(alt.names), unname(unlist(alt.names)))
        alt.names
    })

    # The latitude and longitude of the center of each country
    url <- "https://developers.google.com/public-data/docs/canonical/countries_csv"
    country.center.coords <- readHTMLTable(doc = content(GET(url), "text"))[[1]]

    # Save everything into sysdata.rda
    use_data(missing110,
             admin1.name.map,
             admin0.name.map.by.name,
             admin0.name.map.by.admin,
             admin1.coordinates,
             map.coordinates.50,
             map.coordinates.110,
             country.coordinates,
             ISO_3166_1,
             ISO_3166_2,
             internal = TRUE, overwrite = TRUE)
}





# # This can be used to load in the existing "sysdata.rda" file, modify it and save it again.
#
# # load existing .rda file
# setwd("C:/Users/jake.NUMDOM2/Git packages/flipStandardCharts/R")
# load("sysdata.rda")
#
# # do something to manipualte/correct/extend data
#
# # e.g. extract table with HTTP GET
# library(httr)
# library(XML)
# url <- "https://developers.google.com/public-data/docs/canonical/countries_csv"
# r <- GET(url)
# country.center.coords <- readHTMLTable(doc = content(r, "text"))[[1]]
#
# # save back rda file with all (possibly updated) objects
# devtools::use_data(missing110,
#                    admin1.name.map,
#                    admin0.name.map.by.name,
#                    admin0.name.map.by.admin,
#                    admin1.coordinates,
#                    map.coordinates.50,
#                    map.coordinates.110,
#                    country.coordinates,
#                    ISO_3166_1,
#                    ISO_3166_2,
#                    country.center.coords,
#                    us.regions,
#                    internal = TRUE, overwrite = TRUE)

