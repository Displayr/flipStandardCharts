#' \code{CountriesOrContinents} Names of geographic regions.
#'
#' Returns the list of unique geographic names that can be used when creating a
#' world map.
#'
#' @param type The name of the geographic region type or "country". See
#'   \code{\link{GeographicRegionTypes}}
#'
#' @examples
#' CountriesOrContinents("country")
#' CountriesOrContinents("continent")
#'
#' @export
CountriesOrContinents <- function(type)
{
    requireNamespace("sp")
    if (type == "country" || type == "name")
        return(names(admin0.name.map.by.admin))

    return(levels(map.coordinates.50[["continent"]]))
}

#' \code{GeographicRegionRowNames} Names of geographic regions.
#'
#' Deprecated - use \code{\link{CountriesOrContinents}}
#'
#' @param type The name of the geographic region type. See
#'   \code{\link{GeographicRegionTypes}}
#' @export
GeographicRegionRowNames <- function(type)
{
    return(CountriesOrContinents(type))
}


#' \code{GeographicRegionTypes} Types of Geographic Regions
#'
#' The geographic region types that are available for referring in a map. E.g.,
#' \code{name}, \code{continent}
#'
#' @examples
#' GeographicRegionTypes()
#'
#' @export
GeographicRegionTypes <- function()
{
    requireNamespace("sp")
    names(map.coordinates.50)
}


#' Get the states in a country
#'
#' When mapping the states of a country you need to match the state names exactly.
#' You can use this function to look up the correct names of the states for the
#' country that you are interested in.
#'
#' @param country Character; name of the country
#' @export
#' @seealso \code{\link{CountriesOrContinents}}
#' @seealso \code{\link{ZipcodesInCountry}}
StatesInCountry <- function(country)
{
    country <- tidyCountryName(country)
    levels(droplevels(subset(admin1.coordinates, admin1.coordinates$admin == country)$name))
}


#' Get the zip/post codes in a country
#'
#' When mapping the zip or postcodes of a country you need to match the codes exactly.
#' You can use this function to look up the full list of codes from USA, UK, or Australia.
#'
#' @param country Character; name of the country
#' @export
#' @seealso \code{\link{CountriesOrContinents}}
#' @seealso \code{\link{StatesInCountry}}
ZipcodesInCountry <- function(country)
{
    country <- tidyCountryName(country)
    data <- switch(country, Australia = australia.postcodes$name,
                   `United Kingdom` = uk.postcodes$name,
                   `United States of America` = us.postcodes$name)
    if (is.null(data))
        stop("Zip code or postcode mapping data is not avaialble for this country.")
    sort(unique(as.character(data)))
}

#' Get the areas codes in a country
#'
#' When mapping the areas of a country you need to match the areas exactly.
#' You can use this function to look up the full list of areas in Australia.
#'
#' @param country Character; name of the country
#' @export
#' @seealso \code{\link{CountriesOrContinents}}
#' @seealso \code{\link{StatesInCountry}}
#' @seealso \code{\link{ZipcodesInCountry}}
AreasInCountry <- function(country)
{
    country <- tidyCountryName(country)
    data <- switch(country, Australia = australia.areas$name)
    if (is.null(data))
        stop("Area mapping data is not available for this country.")
    sort(unique(as.character(data)))
}


#' Standardize country name
#'
#' @param country Character string; the country to search for
#' @return Character string; the corrected country name
#' @noRd
tidyCountryName <- function(country)
{
    requireNamespace("sp")

    # If the country is not an exact match, search wider for it
    if (!(country %in% names(admin0.name.map.by.admin)))
    {
        for (admin in names(admin0.name.map.by.admin))
        {
            alt <- admin0.name.map.by.admin[[admin]]
            if (country %in% alt)
            {
                country <- admin
                break
            }
        }
        rm(admin)
    }

    if (!(country %in% names(admin0.name.map.by.admin)))
        stop("Country '", country, "' not found.")

    return(country)
}


#' Find the name of a country based upon a vector of state names
#'
#' @param states Character vector of states.
#' @return Character string; the corrected country name
#' @examples
#' FindCountryFromRegions(c("Bavaria", "Hesse"))
#' @export
FindCountryFromRegions <- function(states) {

    if (is.null(states) || all(!is.na(suppressWarnings(as.numeric(states)))))
        stop("Cannot guess country without useful state names.")

    country.matches <- list()
    for (current in names(admin1.name.map))
    {
        all.states <- admin1.name.map[[current]]
        all.states <- c(names(all.states), unique(unlist(all.states)))
        matches <- sum(tolower(states) %in% tolower(all.states))
        if (matches > 0)
            country.matches[[current]] <- matches
    }

    if (length(country.matches) == 0) {
        country <- "unknown"
        attr(country, "matches") <- 0
        return(country)
    }

    # In the case of ties this will choose the first one.
    max.match <- which.max(country.matches)
    country <- names(max.match)
    attr(country, "matches") <- country.matches[[max.match]]
    return(country)
}

# Check and standarize input format
cleanMapInput <- function(table)
{
    # Correcting rowname errors for country names.
    # Neatening the data.
    statistic <- attr(table, "statistic", exact = TRUE)

    table.name <- deparse(substitute(table))
    if (is.null(dim(table)) || length(dim(table)) == 1) # better than is.vector()
    {
        if(is.null(names(table)))
            stop(paste(table.name, "has no names. The names are required to match known geographic entitites."))

        table <- as.matrix(table)
    }

    if (length(dim(table)) != 2)
        stop(paste("Tables must contain one or more columns of data, and may not have three or more dimensions."))

    if (ncol(table) == 1 && is.null(dimnames(table)[[2]]))
        dimnames(table)[[2]] = table.name

    if (is.null(colnames(table)))
        stop(paste(table.name, "has no column names"))

    if (is.null(rownames(table)))
        stop(paste(table.name, "has no row names. The row names are required to match known geographic entitites."))

    if (all(!is.na(suppressWarnings(as.numeric(rownames(table))))) && !is.null(statistic) && statistic == "Text")
        stop(paste(table.name, "contains text and has numeric row names. Did you mean to convert this table to percentages?"))

    if (!is.null(statistic))
        attr(table, "statistic") <- statistic

    table[!is.finite(table)] <- NA       # convert NaN, inf etc to NA

    return(table)
}


definedFormatMapTypes <- function(names, zip.country) {

    if (all(nchar(names) == 3))
        return("countries")

    if (is.null(zip.country) || zip.country == "")
        zip.country <- "Automatic"

    if (zip.country != "Automatic")
        return(switch(zip.country, Australia = "aus_postcodes", USA = "us_postcodes", UK = "uk_postcodes"))

    if (suppressWarnings(all(!is.na(as.numeric(names)))) && max(sapply(as.character(names), nchar)) == 4)
        return("aus_postcodes")

    if (suppressWarnings(all(!is.na(as.numeric(names)))) && max(sapply(as.character(names), nchar)) == 5)
        return("us_postcodes")

    # Check if first part (before any space) <= 4 chars, starts with a letter and contains a digit.
    split.names <- strsplit(names, " ", fixed = FALSE)
    first.words <- sapply(split.names, function (x) x[1])
    if (all(grepl("^[A-z]+[0-9]+", first.words)) && all(sapply(first.words, length) <= 4))
        return("uk_postcodes")

    return("unknown")
}

# Adds leading zeros to standardize length of postcodes
tidyPostcodes <- function(names, map.type) {
    if (map.type == "aus_postcodes")
        return(sapply(names, padWithZeros, 4))

    if (map.type == "us_postcodes")
        return(sapply(names, padWithZeros, 5))

    return(names)
}

padWithZeros <- function(s, len) {
    return(paste0(paste0(rep("0", max(len - nchar(s), 0)), collapse = ""), s))
}

australiaAreasNameMap <- function() {
    nms <- australia.areas$name
    mat <- cbind(gsub(" ", "", nms, fixed = TRUE),
                 gsub("-", ":", nms, fixed = TRUE),
                 gsub(" - ", ":", nms, fixed = TRUE))
    map <- split(mat, 1:nrow(mat))
    names(map) <- nms
    map$`Coffs Harbour - Grafton` <- c(map$`Coffs Harbour - Grafton`, "Coffs Harbor", "Grafton")
    map$`Hunter Valley exc Newcastle` <- c(map$`Hunter Valley exc Newcastle`, "Hunter Valley")
    map$`New England and North West` <- c(map$`New England and North West`, "New England")
    map$`Newcastle and Lake Macquarie` <- c(map$`Newcastle and Lake Macquarie`, "Newcastle")
    map$`Richmond - Tweed` <- c(map$`Richmond - Tweed`, "Richmond", "Tweed")
    map$`Southern Highlands and Shoalhaven` <- c(map$`Southern Highlands and Shoalhaven`, "Southern Highlands", "Shoalhaven")
    map$`Latrobe - Gippsland` <- c(map$`Latrobe - Gippsland`, "Latrobe", "Gippsland")
    map$`Warrnambool and South West` <- c(map$`Warrnambool and South West`, "Warrnambool")
    map$`Darling Downs - Maranoa` <- c(map$`Darling Downs - Maranoa`, "Darling Downs", "Maranoa")
    map$`Logan - Beaudesert` <- c(map$`Logan - Beaudesert`, "Logan", "Beaudesert")
    map$`Mackay - Isaac - Whitsunday` <- c(map$`Mackay - Isaac - Whitsunday`, "Mackay", "Isaac", "Whitsunday")
    map$`Barossa - Yorke - Mid North` <- c(map$`Barossa - Yorke - Mid North`, "Barossa", "Yorke")
    map
}
