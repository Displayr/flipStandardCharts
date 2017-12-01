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
#' @param treat.NA.as.0 Plots any \code{NA} values in the data and any
#'   geographical entities without data as having a zero value.
#' @param colors A vector of two colors, which are used as endpoints in
#'   interpolating colors.
#' @param ocean.color The color used for oceans, used only by \code{plotly}.
#' @param color.NA The color used to represent missing values. Not used when
#'   \code{treat.NA.as.0}, is set to missing.
#' @param legend.title The text to appear above the legend.
#' @param mapping.package Either \code{"leaflet"} (better graphics, more country
#' maps) or \code{"plotly"} (faster).
#' @export
GeographicMap <- function(x,
                          map.type,
                          country,
                          high.resolution = FALSE,
                          treat.NA.as.0 = FALSE,
                          colors = c("#CCF3FF", "#23B0DB"),
                          ocean.color = "#DDDDDD",
                          color.NA = "#808080",
                          legend.title = "",
                          mapping.package = "leaflet") {

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

        BaseMap(table = table, coords = coords, remove.regions = remove.regions,
                name.map = admin0.name.map.by.name, high.resolution = high.resolution,
                map.type = map.type, treat.NA.as.0 = treat.NA.as.0, colors = colors,
                ocean.color = ocean.color, color.NA = color.NA, legend.title = legend.title,
                mapping.package = mapping.package)
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

        BaseMap(table = table, coords = coords, name.map = name.map,
                high.resolution = high.resolution, map.type = country, treat.NA.as.0 = treat.NA.as.0, colors = colors,
                ocean.color = ocean.color, color.NA = color.NA, legend.title = legend.title,
                mapping.package = mapping.package)
    }
    else if (map.type == "regions")
    {
        coords <- subset(admin1.coordinates, admin1.coordinates$admin == "United States of America")
        name.map <- admin1.name.map[["United States of America"]]
        BaseMap(table = table, coords = coords, name.map = name.map,
                high.resolution = high.resolution, map.type = map.type, treat.NA.as.0 = treat.NA.as.0, colors = colors,
                ocean.color = ocean.color, color.NA = color.NA, legend.title = legend.title,
                mapping.package = mapping.package)
    }
    else
        stop("Unrecognized map.type. Please use one of continents, countries, regions or states.")
}
