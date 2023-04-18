#' This script reads and processes the various definitions of geographic regions
#' and updates the sysdata.rda file (for use in mapping functions).
#'
#' sysdata.rda is defined as follows:
#' missing110 - a list of strings of the names and alternative names of countries that are in
#'     map.coordinates.110 but not map.coordinates.50.
#' admin1.name.map - a named list of countries, where each item is a named list
#'     of states, within which each item is a vector of alternative names of that state.
#' admin0.name.map.by.admin - a named list of countries, where each item is a vector
#'     of alternative names and abbreviations of that country.
#' admin1.coordinates - a SpatialPolygonsDataFrame of regional data and polygons.
#' map.coordinates.50 - a SpatialPolygonsDataFrame of country data and polygons at the
#'     higher resolution of 1:50,000,000
#' map.coordinates.110 - a SpatialPolygonsDataFrame of country data and polygons at the
#'     lower resolution of 1:110,000,000
#' ISO_3166_1 - a data.frame of country names
#' ISO_3166_2 - a data.frame of region names
#' country.center.coords - a data.frame of the latitude and longitude of the center of each country
#' us.regions - a data.frame of the us.states and regions
#'
#' To load in the existing "sysdata.rda" file, use setwd("C:/Users/jake.NUMDOM2/Git packages/flipStandardCharts/R") and load("sysdata.rda")
#'
#' TO RUN THE SCRIPT AND UPDATE THE .RDA FILE, UNCOMMENT ALL LINES BELOW HERE


# https://gis.stackexchange.com/questions/374508/spproj4string-equivalent-in-the-proj6-framework
#
# options("rgdal_show_exportToProj4_warnings"="none")
# library(httr)
# library(XML)
# library(devtools)
# library(rgdal)
# library(utils)
# library(rmapshaper)

# # ISO codes of countries and regions
# data("ISO_3166_1",  package = "ISOcodes", envir = environment())
# data("ISO_3166_2", package = "ISOcodes", envir = environment())
# ISO_3166_2$Country <- substr(ISO_3166_2$Code, 1, 2)


# # Code source: http://stackoverflow.com/questions/29118059/display-spatialpolygonsdataframe-on-leaflet-map-with-r

# # Higher resolution country data
# download.file(file.path('https://www.naturalearthdata.com/http/',
#                         'www.naturalearthdata.com/download/50m/cultural',
#                         'ne_50m_admin_0_countries.zip'), f <- tempfile())
# unzip(f, exdir=tempdir())
# map.coordinates.50 <- readOGR(tempdir(), 'ne_50m_admin_0_countries', encoding='UTF-8', stringsAsFactors = TRUE)
# map.coordinates.df <- data.frame(map.coordinates.50)
# column.class <- sapply(map.coordinates.df, class)
# column.class <- column.class[column.class == "factor"]
# rm(map.coordinates.df)
# for (column in names(column.class))
#     levels(map.coordinates.50[[column]]) <- enc2utf8(levels(map.coordinates.50[[column]]))
# names(map.coordinates.50) <- tolower(names(map.coordinates.50))

# # Lower resolution country data
# download.file(file.path('https://www.naturalearthdata.com/http/',
#                         'www.naturalearthdata.com/download/110m/cultural',
#                         'ne_110m_admin_0_countries.zip'), f <- tempfile())
# unzip(f, exdir=tempdir())
# map.coordinates.110 <- readOGR(tempdir(), 'ne_110m_admin_0_countries', encoding='UTF-8', stringsAsFactors = TRUE)
# map.coordinates.df <- data.frame(map.coordinates.110)
# column.class <- sapply(map.coordinates.df, class)
# column.class <- column.class[column.class == "factor"]
# rm(map.coordinates.df)
# for (column in names(column.class))
#     levels(map.coordinates.110[[column]]) <- enc2utf8(levels(map.coordinates.110[[column]]))
# names(map.coordinates.110) <- tolower(names(map.coordinates.110))

# # State data
# f <- tempfile()
# download.file(file.path('https://www.naturalearthdata.com/http/',
#                         'www.naturalearthdata.com/download/10m/cultural',
#                         'ne_10m_admin_1_states_provinces.zip'), f)
# d <- tempdir()
# unzip(f, exdir = d)
# admin1.coordinates <- readOGR(d, 'ne_10m_admin_1_states_provinces', stringsAsFactors = TRUE)
# admin1.coordinates.df <- data.frame(admin1.coordinates)
# column.class <- sapply(admin1.coordinates.df, class)
# column.class <- column.class[column.class == "factor"]
# rm(admin1.coordinates.df)
# for (column in names(column.class))
#     Encoding(levels(admin1.coordinates[[column]])) <- "UTF-8"

# # Mapping of countries to alternative names
# #admin0.name.map.by.name <- makeNameMap("name")
# #admin0.name.map.by.admin <- makeNameMap("admin")
# map.coordinates <- data.frame(map.coordinates.50)
# admin0.name.map.by.admin <- list()
# for (i in seq_len(nrow(map.coordinates)))
# {
#     country.name <- as.character(map.coordinates[["admin"]][i])
#
#     columns <- c("admin", "adm0_a3", "geounit", "gu_a3", "subunit", "su_a3",
#                  "name", "name_long", "brk_a3", "brk_name", "abbrev", "postal",
#                  "formal_en", "formal_fr", "name_sort", "name_alt",
#                  "iso_a2", "iso_a3", "wb_a2", "wb_a3", "adm0_a3_us")
#
#     all.names <- as.character(unlist(map.coordinates[i, columns]))
#     all.names[all.names == "-99"] <- NA
#     all.names <- all.names[!is.na(all.names)]
#     all.names <- unique(all.names)
#     all.names <- all.names[all.names != country.name]
#
#     iso.match <- match(country.name, ISO_3166_1$Name)
#     if (is.na(iso.match))
#     {
#         iso.match <- match(all.names, ISO_3166_1$Name)
#         if (any(!is.na(iso.match)))
#             iso.match <- iso.match[!is.na(iso.match)]
#
#         iso.match <- iso.match[1]
#     }
#
#     if (!is.na(iso.match))
#     {
#         columns <- c("Alpha_2", "Alpha_3", "Name", "Official_name", "Common_name")
#         iso.names <- unlist(ISO_3166_1[iso.match, columns])
#         iso.names <- iso.names[!is.na(iso.names)]
#         all.names <- unique(c(all.names, iso.names))
#     }
#
#     all.names <- all.names[all.names != country.name]
#
#     if (length(all.names) == 0)
#         next
#
#     admin0.name.map.by.admin[[country.name]] <- all.names
# }

# # Some mappings are duplicated, so manually remove the less common ones

# all.names <- unlist(admin0.name.map.by.admin)
# duplicates <- length(all.names) - length(unique(all.names))
# print(paste0(duplicates, " duplicates in country alternative names before processing."))

# remove.from <- c("Anguilla", "Aland", "Ashmore and Cartier Islands", "Indian Ocean Territories", "Ashmore and Cartier Islands",
#                  "Northern Cyprus", "Northern Cyprus", "British Indian Ocean Territory", "Israel", "Guernsey",
#                  "Guernsey", "Jamaica", "Jordan", "Western Sahara", "Siachen Glacier",
#                  "Saint Lucia", "Somaliland", "Somaliland", "Sint Maarten", "Samoa")
# to.remove <- c("Ang.", "AI", "AUS", "AUS", "AU",
#                "CN", "CYP", "IOT", "IS", "JG",
#                "CHI", "J", "J", "MAR", "SG",
#                "S.L.", "SL", "SOM", "St. M.", "WS")
# for (i in seq(length(remove.from)))
# {
#     country <- remove.from[i]
#     abbreviation <- to.remove[i]
#     admin0.name.map.by.admin[[country]] <- admin0.name.map.by.admin[[country]][admin0.name.map.by.admin[[country]] != abbreviation]
# }

# # Add missing mappings
# admin0.name.map.by.admin[["United Kingdom"]] <- c("UK", admin0.name.map.by.admin[["United Kingdom"]])
# admin0.name.map.by.admin$Palestine <- c("State of Palestine", admin0.name.map.by.admin$Palestine)
# admin0.name.map.by.admin$Fiji <- c("Fiji Islands", admin0.name.map.by.admin$Fiji)

# # Check that there are no more duplicates
# all.names <- unlist(admin0.name.map.by.admin)
# duplicates <- length(all.names) - length(unique(all.names))
# print(paste0(duplicates, " duplicates in country alternative names after processing."))


# # Mapping of states to alternative names
# admin1.df <- data.frame(admin1.coordinates)

# ISO_3166_1$Country <- rep(NA, nrow(ISO_3166_1))
# for (i in seq_len(nrow(ISO_3166_1)))
# {
#     country <- ISO_3166_1$Name[i]
#     if (country %in% names(admin0.name.map.by.admin))
#     {
#         ISO_3166_1$Country[i] <- country
#     }
#     else
#     {
#         for (admin in names(admin0.name.map.by.admin))
#         {
#             alt <- admin0.name.map.by.admin[[admin]]
#             if (country %in% alt)
#             {
#                 ISO_3166_1$Country[i] <- admin
#                 break
#             }
#         }
#     }
# }

# iso.3166 <- merge(ISO_3166_1[, c("Alpha_2", "Country")], ISO_3166_2,
#                   by.x = "Alpha_2", by.y = "Country")

# iso.3166$Code <- substring(iso.3166$Code, 4)
# iso.3166$Code[!grepl("^[[:alpha:]]+$", iso.3166$Code)] <- NA
#
# admin1.name.map <- list()
# for (i in seq_len(nrow(admin1.df)))
# {
#     state <- as.character(admin1.df$name[i])
#     if (is.na(state))
#         next
#
#     country <- as.character(admin1.df$admin[i])
#     country.name.map <- admin1.name.map[[country]]
#     if (is.null(country.name.map))
#         country.name.map <- list()
#
#     columns <- c("abbrev", "postal", "woe_name", "gn_name")
#     all.names <- as.character(unlist(admin1.df[i, columns]))
#     all.names[all.names == "-99"] <- NA
#
#     name.alt <- as.character(admin1.df$name_alt[i])
#     all.names <- c(all.names, strsplit(name.alt, "|", fixed = TRUE)[[1]])
#     name.local <- as.character(admin1.df$name_local[i])
#     all.names <- c(all.names, strsplit(name.local, "|", fixed = TRUE)[[1]])
#
#     iso.code <- iso.3166$Code[iso.3166$Country == country & iso.3166$Name == state]
#     all.names <- c(all.names, iso.code)
#
#     all.names <- all.names[!is.na(all.names)]
#     all.names <- unique(all.names)
#     all.names <- all.names[all.names != state]
#
#     if (length(all.names) == 0)
#         next
#
#     country.name.map[[state]] <- all.names
#     admin1.name.map[[country]] <- country.name.map
# }

# # Manual amendments to state alternative names
# admin1.name.map[["Australia"]]$`Jervis Bay Territory` <- admin1.name.map[["Australia"]]$`Lord Howe Island` <- NULL
# Zendesk #93689: Fix ISO code for Brandenburg so it doesn't match Berlin's
# admin1.name.map[["Germany"]][["Brandenburg"]][1] <- "BP"

# # The file at 1:110m is missing a few small countries compared to the 1:50m data,
# # notably Singapore and Hong Kong
# missing110 <- local({
#     missing.countries <- setdiff(levels(map.coordinates.50[["admin"]]),
#                                  levels(map.coordinates.110[["admin"]]))
#     alt.names <- admin0.name.map.by.admin[missing.countries]
#     alt.names <- c(names(alt.names), unname(unlist(alt.names)))
#     alt.names
# })

# # The latitude and longitude of the center of each country
# url <- "https://developers.google.com/public-data/docs/canonical/countries_csv"
# country.center.coords <- readHTMLTable(doc = content(GET(url), "text"))[[1]]

# us.regions <- structure(list(RegionNumber = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
#                                             3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L),
#                                             .Label = c("1", "2", "3", "4"), class = "factor"),
#                             DivisionNumber = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 5L, 5L, 5L,
#                                             5L, 5L, 5L, 5L, 5L, 5L, 6L, 6L, 6L, 6L, 7L, 7L, 7L, 7L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 9L, 9L, 9L, 9L, 9L),
#                                             .Label = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), class = "factor"),
#                             Region = structure(c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
#                                             3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L),
#                                             .Label = c("Midwest", "Northeast", "South", "West"), class = "factor"),
#                             Division = structure(c(5L, 5L, 5L, 5L, 5L, 5L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 1L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 7L, 7L, 7L, 7L,
#                                             7L, 7L, 7L, 7L, 7L, 2L, 2L, 2L, 2L, 9L, 9L, 9L, 9L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 6L, 6L, 6L, 6L, 6L),
#                                             .Label = c("East North Central", "East South Central", "Mid-Atlantic", "Mountain", "New England", "Pacific", "South Atlantic", "West North Central", "West South Central"), class = "factor"),
#                             State = structure(c(7L, 20L, 22L, 30L, 40L, 46L, 31L, 33L, 39L, 14L, 15L, 23L, 36L, 50L, 16L, 17L, 24L, 26L, 28L, 35L,
#                                             42L, 8L, 10L, 11L, 21L, 34L, 41L, 47L, 9L, 49L, 1L, 18L, 25L, 43L, 4L, 19L, 37L, 44L, 3L, 6L, 13L, 27L, 29L, 32L, 45L, 51L, 2L, 5L, 12L, 38L, 48L),
#                                             .Label = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia",
#                                             "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
#                                             "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico",
#                                             "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
#                                             "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"), class = "factor"),
#                             Code = structure(c(7L, 22L, 20L, 31L, 40L, 47L, 32L, 35L, 39L, 15L, 16L, 23L, 36L, 49L, 13L, 17L, 24L, 25L, 30L, 29L, 42L, 9L, 10L,
#                                             11L, 21L, 28L, 41L, 46L, 8L, 50L, 2L, 18L, 26L, 43L, 3L, 19L, 37L, 44L, 4L, 6L, 14L, 27L, 34L, 33L, 45L, 51L, 1L, 5L, 12L, 38L, 48L),
#                                             .Label = c("AK","AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
#                                             "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY"), class = "factor")),
#                         .Names = c("RegionNumber", "DivisionNumber", "Region", "Division", "State", "Code"), row.names = c(NA, 51L), class = "data.frame")

# # USA zip codes
# # Originally used 'http://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_zcta510_500k.zip'
# download.file(file.path('http://www2.census.gov/geo/tiger/GENZ2019/shp/cb_2019_us_zcta510_500k.zip'),
#              f <- tempfile())
# unzip(f, exdir = tempdir())
# us.postcodes <- readOGR(tempdir(), "cb_2019_us_zcta510_500k")
# colnames(us.postcodes@data)[3] <- "name"
# # Simplify polygons to reduce size. Named polygons causes leaflet map to be blank.
# set_thin_PROJ6_warnings(TRUE)
# us.postcodes <- ms_simplify(us.postcodes, keep = 0.01, keep_shapes = TRUE)
# names(us.postcodes@polygons) <- NULL

# # UK postcodes
# # Note terms and conditions - http://www.opendoorlogistics.com/downloads/
# # Note distinction between sector eg "LS25 5", district (eg "LS25") and area (eg "LS")
# download.file(file.path('http://www.opendoorlogistics.com/wp-content/uploads/Data/UK-postcode-boundaries-Jan-2015.zip'),
#               f <- tempfile())
# unzip(f, exdir = tempdir())
# uk.postcodes <- readOGR(file.path(tempdir(), "Distribution"), "Districts")
# # Simplify polygons to reduce size - error if this is done in one go so 3 chunks
# uk.compress <- ms_simplify(uk.postcodes[1:700, ], keep_shapes = TRUE)
# uk.compress2 <- ms_simplify(uk.postcodes[701:750, ], keep_shapes = TRUE)
# uk.compress3 <- ms_simplify(uk.postcodes[751:2880, ], keep_shapes = TRUE)
# uk.postcodes <- rbind(uk.compress, uk.compress2, uk.compress3)
# names(uk.postcodes@polygons) <- NULL

# # Australia post codes
# # http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.003July%202016?OpenDocument

# # http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055003_poa_2016_aust_shape.zip&1270.0.55.003&Data%20Cubes&4FB811FA48EECA7ACA25802C001432D0&0&July%202016&13.09.2016&Latest

# download.file(file.path('https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files/POA_2021_AUST_GDA2020_SHP.zip'),
#               f <- tempfile(),
#               mode = "wb")
# unzip(f, exdir = tempdir())
# australia.postcodes <- readOGR(tempdir(), "POA_2021_AUST_GDA2020")
# colnames(australia.postcodes@data)[2] <- "name"
# australia.postcodes <- ms_simplify(australia.postcodes, keep = 0.02, keep_shapes = TRUE)
# names(australia.postcodes@polygons) <- NULL

# # Australia SA4 areas
# url <- "http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_sa4_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&C65BC89E549D1CA3CA257FED0013E074&0&July%202016&12.07.2016&Latest"
# download.file(file.path(url),
#               f <- tempfile(),
#               mode = "wb")
# unzip(f, exdir = tempdir())
# australia.areas <- readOGR(tempdir(), "SA4_2016_AUST")
# colnames(australia.areas@data)[2] <- "name"
# australia.areas <- ms_simplify(australia.areas, keep = 0.005, keep_shapes = TRUE)
# names(australia.areas@polygons) <- NULL


# ### Manually combine 2 counties of Norway, zendesk 16755, 82120
# ## DS-3647: https://www.lifeinnorway.net/norway-new-counties/
# ## Aust-Agder and Vest Agder have combined to become Agder
# ## Buskerud, Akershus and Ostfold have combined to become Viken
# ## Finnmark and Troms have combined to form Troms og Finnmark
# ## Hordaland and Sogn og Fjordane have combined to form Vestland
# ## Nord-Trond Sor-Trondelag merged into Trondelag
# ## Oppland and Hedmark joined to become Innlandet
# ## Telemark og Vestfold combined to become Vestfold og Telemark
# ## names(admin1.name.map[["Norway"]])
# new.counties <- c("Tr\u00F8ndelag", "Agder", "Viken", "Troms og Finnmark", "Vestland",
#                   "Innlandet", "Vestfold og Telemark")
# merge.list <- list(c("Nord-Tr\u00F8ndelag", "S\u00F8r-Tr\u00F8ndelag"),
#                    c("Aust-Agder", "Vest-Agder"),
#                    c("Buskerud", "Akershus", "\u00D8stfold"),
#                    c("Finnmark", "Troms"),
#                    c("Hordaland", "Sogn og Fjordane"),
#                    c("Oppland", "Hedmark"),
#                    c("Telemark", "Vestfold"))
# names(merge.list) <- new.counties
# ad <- admin1.coordinates
# for (i in seq_along(new.counties))
# {
#     to.merge <- ad[ad$name %in% merge.list[[i]], ]
#     merged <- suppressWarnings(aggregate(to.merge, FUN = mean,
#                                          dissolve = TRUE))
#     merged$admin <- "Norway"
#     merged$name <- new.counties[i]
#     merged$ID <- NULL

#     levels(ad$name) <- c(levels(ad$name), new.counties[i])
#     ad <- rbind(ad, merged)
# }
# admin1.coordinates <- ad

# anm <- admin1.name.map
# anm[["Norway"]]$"Tr\u00F8ndelag" <- "Trondelag"
# print(anm[["Norway"]])
# admin1.name.map <- anm


# ### Manually remove Macquarie Island from Australia state map
# ac <- admin1.coordinates
# keep <- admin1.coordinates$name_en != "Macquarie Island"
# keep[is.na(keep)] <- TRUE
# ac <- ac[keep, ]
# admin1.coordinates <- ac


# # Save everything into sysdata.rda
# # need to check that objects simplified with ms_simplify aren't
# # unnecessarily large (I'm guessing R stores a very large
# # environment with one of the SpatialPolygonsDataFrame objects)
# # which was making sysdata.rda >100MB
# #save(missing110,
#      admin1.name.map,
#      admin0.name.map.by.admin,
#      admin1.coordinates,
#      map.coordinates.50,
#      map.coordinates.110,
#      ISO_3166_1,
#      ISO_3166_2,
#      us.regions,
#      us.postcodes,
#      uk.postcodes,
#      australia.postcodes,
#      australia.areas,
#      file = "R/sysdata.rda")
