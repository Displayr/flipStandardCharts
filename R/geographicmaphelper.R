#' \code{CountriesOrContinents} Names of geographic regions.
#'
#' Returns the list of unique geographic names that can be used when creating a
#' WorldMap.
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
#' When mapping sthe states of a country you need to match the state names exactly.
#' You can use this function to look up the correct names of the states for the
#' country that you are interested in.
#'
#' @param country The country to look at
#' @export
#' @seealso \code{\link{CountriesOrContinents}}
StatesInCountry <- function(country)
{
    country <- tidyCountryName(country)
    levels(droplevels(subset(admin1.coordinates, admin1.coordinates$admin == country)$name))
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


postcodesOrStates <- function(names) {

    if (all(!is.na(as.numeric(names))) && all(sapply(as.character(names), nchar) == 4))
        return("aus_postcodes")
    return("states")
}

