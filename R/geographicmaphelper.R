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
ZipcodesInCountry <- function(country)
{
    country <- tidyCountryName(country)
    data <- switch(country, Australia = australia.postcodes$name,
                   `United Kingdom` = uk.postcodes$name,
                   `United States of America` = us.postcodes$name)
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

    if (length(country.matches) == 0)
        stop("Could not guess country from rownames.")

    # In the case of ties this will choose the first one.
    max.match <- which.max(country.matches)
    country <- names(max.match)
    message("Country '", country, "' was automatically chosen from the rownames.")
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

    return(table)
}


postcodesOrStates <- function(names, zip.country) {

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

    return("states")
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

