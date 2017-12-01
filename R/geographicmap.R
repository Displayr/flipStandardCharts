#' Geographic Map
#'
#' Creates a map with a table as input, using shading to represent the values of
#' countries or states on the map.
#'
#' @param x A matrix, two-dimensional array, table or vector, containing the
#'   data to be plotted. The \code{\link{rownames}} (or \code{\link{names}} in
#'   the case of a vector) should contain the names of the geographic entities
#'   to be plotted.
#' @param map.type One of \code{"Continents"}, \code{"Countries"}, \code{"Regions"} or \code{"States"}
#' which respectively plot a world map by continent, a world map by country, a map of USA by region,
#' or a single country map by state.
#' @param country Character string optionally stating the country that the states are from, if
#' \code{map.type} is \code{"states"}.
#' @param high.resolution Specifically request a high resolution map. Otherwise
#' the resolution of the map is chosen automatically based on the resolution required
#' for the requested countries or regions.
#' @param ... Other parameters to pass to \code{BaseMap}.
#'
#' @export
GeographicMap <- function(x, map.type, country, high.resolution = FALSE, ...) {

    requireNamespace("sp")

    table <- cleanMapInput(x)
    map.type <- tolower(map.type)

    if (map.type == "countries" || map.type == "continents") {

        # Getting geographic boundaries. If the user asks for high resolution maps
        # or any of the requested regions are missing in the low resolution map, use
        # the 1:50m map, otherwise use the 1:110m map.
        req.names <- rownames(table)

        if (high.resolution || any(req.names %in% missing110))
        {
            coords <- map.coordinates.50
        }
        else
        {
            coords <- map.coordinates.110
        }

        coords <- coords[!(coords$continent %in% "Antarctica"), ]
        remove.regions <- "Antarctica"

        BaseMap(table = table, ..., coords = coords, remove.regions = remove.regions,
                name.map = admin0.name.map.by.name, high.resolution = high.resolution,
                map.type = map.type)
    }
    else if (map.type == "states")
    {
        # Attempt to guess the country from the rownames if not specified
        if (missing(country) || country == "")
            country <- FindCountryFromRegions(rownames(table))

        # If the country is not an exact match, search wider for it
        else
            country <- tidyCountryName(country)

        coords <- subset(admin1.coordinates, admin1.coordinates$admin == country)

        name.map <- admin1.name.map[[country]]

        BaseMap(table = table, coords = coords, ..., name.map = name.map,
                high.resolution = high.resolution, map.type = country)
    }
    else if (map.type == "regions")
    {
        coords <- subset(admin1.coordinates, admin1.coordinates$admin == "United States of America")
        name.map <- admin1.name.map[["United States of America"]]
        BaseMap(table = table, coords = coords, ..., name.map = name.map,
                high.resolution = high.resolution, map.type = map.type)
    }
    else
        stop("Unrecognized map.type. Please use one of continents, countries, regions or states.")
}
