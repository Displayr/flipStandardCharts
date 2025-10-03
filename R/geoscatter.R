#' Geospatial Scatter Plot
#'
#' Creates a scatter plot on a map using location data, with circle markers representing values.
#' The location names should be provided as rownames of the input data.
#'
#' @inherit GeographicMap
#' @param x A matrix, two-dimensional array, table or vector, containing the
#'   data to be plotted. The \code{\link{rownames}} (or \code{\link{names}} in
#'   the case of a vector) should contain the names of the geographic entities
#'   to be plotted.
#' @param country Character string optionally stating the country that the locations are from.
#' @param high.resolution Specifically request a high resolution map. Otherwise
#' the resolution of the map is chosen automatically based on the resolution required
#' for the requested countries or regions.
#' @param show.missing.regions Logical; Whether to plot regions not included in
#'    \code{x} with values of \code{NA}. Used only by \code{"leaflet"}.
#' @param treat.NA.as.0 Plots any \code{NA} values in the data as having a zero value.
#' @param colors A vector of colors used for the circles. If a single color is provided,
#'   all circles will use that color. If multiple colors are provided, they will be
#'   used to create a color gradient based on values.
#' @param ocean.color The color used for oceans (or background).
#' @param color.NA The color used to represent missing values.
#' @param global.font.family Character; font family for all occurrences of any
#' font attribute for the chart unless specified individually.
#' @param global.font.color Global font color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).
#' @param legend.title The text to appear above the legend.
#' @param values.hovertext.format A string representing a d3 formatting code.
#' See https://github.com/d3/d3/blob/master/API.md#number-formats-d3-format
#' @param values.bounds.minimum Numeric; specifies the minimum value for circle sizing.
#' @param values.bounds.maximum Numeric; specifies the maximum value for circle sizing.
#' @param mapping.package Either \code{"leaflet"} (default) or \code{"plotly"}.
#' @param background If \code{"mapping.package"} is \code{"leaflet"}, add a background
#' tile from openstreetmaps.
#' @param zip.country One of \code{"Automatic"}, \code{"USA"}, \code{"UK"} or \code{"Australia"}.
#' @param legend.show Logical; Whether to display a legend with the color/size scale.
#' @param legend.font.family Font family of legend.
#' @param legend.font.color Font color of legend.
#' @param legend.font.size Font size of legend.
#' @param hovertext.font.size Font size for hover text.
#' @param circle.size.multiplier Numeric multiplier for circle sizes (default: 10000).
#' @param circle.stroke.color Color for circle borders.
#' @param circle.stroke.width Width of circle borders.
#' @param circle.fill.opacity Opacity of circle fill (0-1).
#' @return an HTML widget for \code{"leaflet"} or a \code{"plotly"} object.
#' @examples
#' data <- seq(4)
#' names(data) <- c("Berlin", "London", "Madrid", "Paris")
#' GeoScatter(data)
#' cafes <- 1:4
#' names(cafes) <- c("22 Union Street, Pyrmont", "Social Brew Cafe, Pyrmont", "306 Harris St, Pyrmont", "1 Central Park Ave, Chippendale")
#' GeoScatter(cafes, colors = flipChartBasics::ChartColors(4, "Reds"))
#' @importFrom leaflet leaflet addTiles addCircleMarkers colorNumeric addLegend fitBounds labelFormat tileOptions
#' @importFrom tidygeocoder geocode geo
#' @importFrom verbs Sum
#' @importFrom flipU StopForUserError
#' @importFrom htmltools browsable tagList tags htmlDependency
#' @importFrom flipChartBasics StripAlphaChannel
#' @importFrom digest digest
#' @export
GeoScatter <- function(x,
                      country = "",
                      high.resolution = FALSE,
                      show.missing.regions = TRUE,
                      treat.NA.as.0 = FALSE,
                      colors = c("#CCF3FF", "#23B0DB"),
                      ocean.color = "#DDDDDD",
                      opacity = 1,
                      color.NA = "#808080",
                      global.font.family = "Arial",
                      global.font.color = "#2C2C2C",
                      legend.show = TRUE,
                      legend.title = "",
                      legend.font.family = global.font.family,
                      legend.font.color = global.font.color,
                      legend.font.size = 14,
                      values.hovertext.format = "",
                      values.bounds.minimum = NULL,
                      values.bounds.maximum = NULL,
                      mapping.package = "leaflet",
                      background = TRUE,
                      title = "",
                      title.font.family = global.font.family,
                      title.font.color = global.font.color,
                      title.font.size = 16,
                      subtitle = "",
                      subtitle.font.family = global.font.family,
                      subtitle.font.color = global.font.color,
                      subtitle.font.size = 12,
                      footer = "",
                      footer.font.family = global.font.family,
                      footer.font.color = global.font.color,
                      footer.font.size = 8,
                      footer.wrap = TRUE,
                      footer.wrap.nchar = 100,
                      hovertext.font.family = global.font.family,
                      hovertext.font.size = 11,
                      zip.country = "Automatic",
                      circle.size.multiplier = 10000,
                      circle.stroke.color = "white",
                      circle.stroke.width = 1,
                      circle.fill.opacity = 0.7)
{
    # Process input parameters similar to GeographicMap
    values.bounds.minimum <- charToNumeric(values.bounds.minimum)
    values.bounds.maximum <- charToNumeric(values.bounds.maximum)
    
    if (isPercentData(x) && isAutoFormat(values.hovertext.format))
        values.hovertext.format <- paste0(values.hovertext.format, "%")
    if (values.hovertext.format == ".0")
        values.hovertext.format <- "~.0f"
    if (grepl("[0-9]$", values.hovertext.format))
        values.hovertext.format <- paste0(values.hovertext.format, "f")
    if (values.hovertext.format == "%")
        values.hovertext.format <- ".0%"
    
    table <- cleanMapInput(checkMatrixNames(x))
    
    if (treat.NA.as.0)
        table[is.na(table)] <- 0
        
    # Get default parameter values
    if (is.null(hovertext.font.family))
        hovertext.font.family <- global.font.family
    if (is.null(legend.font.family))
        legend.font.family <- global.font.family
    if (length(legend.font.size) == 0)
        legend.font.size <- 14
    if (length(hovertext.font.size) == 0)
        hovertext.font.size <- 11
    if (is.null(opacity))
        opacity <- 1.0
    
    # Strip alpha channel from colors (similar to GeographicMap)
    colors <- StripAlphaChannel(colors)
    
    locations <- rownames(table)
    
    # Geocode the locations with caching
    geocoded_data <- get_cached_geocoding(locations)
    
    failed_geocoding <- is.na(geocoded_data$lat) | is.na(geocoded_data$long)
    if (sum(failed_geocoding) == length(locations)) {
        StopForUserError("Input data does not contain valid location names as the rownames.")
    }
    
    # Create data frame with geocoded locations
    df <- data.frame(
        location = locations,
        latitude = geocoded_data$lat,
        longitude = geocoded_data$long,
        values = as.numeric(table[, 1]),
        stringsAsFactors = FALSE
    )
    
    # Remove failed geocoding results
    if (any(failed_geocoding)) {
        failed_names <- locations[failed_geocoding]
        warning("Failed to geocode the following locations: ", 
                paste(failed_names, collapse = ", "))
        df <- df[!failed_geocoding, ]
    }
    
    # Handle bounds
    min.value <- min(df$values, na.rm = TRUE)
    max.value <- max(df$values, na.rm = TRUE)
    if (treat.NA.as.0) {
        min.value <- min(0, min.value)
        df$values[is.na(df$values)] <- 0
    }
    
    # Set bounds if not specified
    if (!is.null(values.bounds.minimum) && values.bounds.minimum > min.value)
        warning("Minimum value must be smaller than ", min.value, ".")
    if (!is.null(values.bounds.maximum) && values.bounds.maximum < max.value)
        warning("Maximum value must be larger than ", max.value, ".")
    values.bounds.minimum <- min(values.bounds.minimum %||% min.value, min.value)
    values.bounds.maximum <- max(values.bounds.maximum %||% max.value, max.value)
    
    # Decide formatting for hovertext
    statistic <- attr(table, "statistic", exact = TRUE)
    if (is.null(statistic)) statistic <- ""
    
    if (values.hovertext.format == "" && grepl("%)?$", statistic))
        values.hovertext.format <- ".0%"
    if (percentFromD3(values.hovertext.format)) {
        format.function <- FormatAsPercent
        decimals <- decimalsFromD3(values.hovertext.format, 0)
        mult <- 100
        suffix <- "%"
    } else {
        format.function <- FormatAsReal
        decimals <- decimalsFromD3(values.hovertext.format, 2)
        mult <- 1
        suffix <- ""
    }
    
    legend.show <- setShowLegend(legend.show)
    
    # Pass to appropriate mapping function
    if (mapping.package == "leaflet") {
        map <- leafletGeoScatter(df, colors, opacity, values.bounds.minimum, values.bounds.maximum,
                                color.NA, legend.show, legend.title, legend.font.family,
                                legend.font.color, legend.font.size, mult, decimals, suffix,
                                values.hovertext.format, treat.NA.as.0, format.function,
                                background, ocean.color, hovertext.font.family, hovertext.font.size,
                                circle.size.multiplier, circle.stroke.color, circle.stroke.width,
                                circle.fill.opacity)
    } else {
        # mapping.package == "plotly" - could be implemented later
        stop("Plotly mapping for GeoScatter is not yet implemented. Use mapping.package = 'leaflet'.")
    }
    
    result <- list(htmlwidget = map)
    class(result) <- "StandardChart"
    attr(result, "ChartType") <- "Geographic Scatter"
    result
}

# Helper function to create leaflet geo scatter plot
#' @importFrom leaflet leaflet addTiles addCircleMarkers colorNumeric addLegend fitBounds labelFormat
#' @importFrom htmltools browsable tagList tags htmlDependency
leafletGeoScatter <- function(df, colors, opacity, min.value, max.value, color.NA,
                             legend.show, legend.title, legend.font.family,
                             legend.font.color, legend.font.size, mult, decimals, suffix,
                             values.hovertext.format, treat.NA.as.0, format.function,
                             background, ocean.color, hovertext.font.family, hovertext.font.size,
                             circle.size.multiplier, circle.stroke.color, circle.stroke.width,
                             circle.fill.opacity) {
    
    # Create color palette
    if (length(colors) == 1) {
        # Single color for all points
        circle.colors <- rep(colors[1], nrow(df))
        pal <- NULL
    } else {
        # Color gradient based on values
        pal <- colorNumeric(palette = colors, domain = c(min.value, max.value), na.color = color.NA)
        circle.colors <- pal(df$values)
    }
    
    # Calculate circle sizes (radius in meters for leaflet)
    value.range <- max.value - min.value
    if (value.range == 0) {
        circle.sizes <- rep(circle.size.multiplier / 10, nrow(df))
    } else {
        # Scale sizes between 10% and 100% of multiplier
        normalized.values <- (df$values - min.value) / value.range
        circle.sizes <- (normalized.values * 0.9 + 0.1) * circle.size.multiplier
    }
    
    # Create hover text
    hover.text <- paste0("<strong>Location:</strong> ", df$location, "<br/>",
                        "<strong>Value:</strong> ", 
                        format.function(df$values, decimals = decimals,
                                      comma.for.thousands = commaFromD3(values.hovertext.format)))
    
    # Create the leaflet map
    map <- leaflet(df)
    
    # Add background tiles if requested
    if (background) {
        map <- addTiles(map, options = tileOptions(opacity = 1.0))
    } else {
        # Add minimal background
        map <- addTiles(map, options = tileOptions(opacity = 0.0))
    }
    
    # Add circle markers
    map <- addCircleMarkers(map,
        lng = ~longitude,
        lat = ~latitude,
        radius = circle.sizes / 1000,  # Convert to appropriate size for leaflet
        color = circle.stroke.color,
        weight = circle.stroke.width,
        fillColor = circle.colors,
        fillOpacity = circle.fill.opacity,
        popup = hover.text,
        opacity = opacity
    )
    
    # Fit bounds to show all points
    if (nrow(df) > 0) {
        map <- fitBounds(map, 
                        lng1 = min(df$longitude, na.rm = TRUE), 
                        lat1 = min(df$latitude, na.rm = TRUE), 
                        lng2 = max(df$longitude, na.rm = TRUE), 
                        lat2 = max(df$latitude, na.rm = TRUE))
    }
    
    # Add legend if requested and using color gradient
    if (legend.show && !is.null(pal)) {
        map <- addLegend(map, "bottomright", pal = pal, values = df$values,
                        title = legend.title,
                        labFormat = labelFormat(suffix = suffix,
                                              big.mark = ifelse(commaFromD3(values.hovertext.format), ",", "")),
                        opacity = opacity,
                        na.label = ifelse(treat.NA.as.0, "0", "NA"))
    }
    
    return(map)
}

# Helper function - null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Global cache for geocoded locations
.geoscatter_cache <- new.env(parent = emptyenv())

#' Get cached geocoding results or perform new geocoding
#' @param locations Vector of location names to geocode
#' @return Data frame with geocoded results
get_cached_geocoding <- function(locations) {
    # Create a unique key for this set of locations
    locations_key <- create_locations_key(locations)
    
    # Check if we have cached results for these exact locations
    if (exists(locations_key, envir = .geoscatter_cache)) {
        cat("Using cached geocoding results for", length(locations), "locations.\n")
        return(get(locations_key, envir = .geoscatter_cache))
    }
    
    # Check for partial matches in cache
    cached_results <- get_partial_cached_results(locations)
    new_locations <- setdiff(locations, names(cached_results))
    
    if (length(new_locations) == 0) {
        cat("All", length(locations), "locations found in cache.\n")
        # All locations are cached, return combined results
        geocoded_data <- combine_cached_results(locations, cached_results)
    } else {
        cat("Geocoding", length(new_locations), "new locations...\n")
        if (length(new_locations) < length(locations)) {
            cat("(", length(locations) - length(new_locations), "locations found in cache)\n")
        }
        
        # Geocode only the new locations
        tryCatch({
            new_geocoded <- tidygeocoder::geo(address = new_locations, method = "osm")
        }, error = function(e) {
            stop("Failed to geocode locations. Please check that tidygeocoder package is installed and location names are valid.")
        })
        
        # Cache the new results individually
        cache_new_results(new_locations, new_geocoded)
        
        # Combine cached and new results
        all_results <- c(cached_results, setNames(split(new_geocoded[, c("lat", "long")], seq_len(nrow(new_geocoded))), new_locations))
        geocoded_data <- combine_cached_results(locations, all_results)
    }
    
    # Cache the complete result set
    assign(locations_key, geocoded_data, envir = .geoscatter_cache)
    
    return(geocoded_data)
}

#' Create a unique key for a set of locations
#' @param locations Vector of location names
#' @return String key
create_locations_key <- function(locations) {
    # Sort locations to ensure consistent key regardless of order
    sorted_locations <- sort(tolower(trimws(locations)))
    # Create hash-like key
    paste0("locations_", digest::digest(sorted_locations, algo = "xxhash32"))
}

#' Get partial cached results for locations
#' @param locations Vector of location names to check
#' @return Named list of cached lat/long pairs
get_partial_cached_results <- function(locations) {
    cached_results <- list()
    
    # Check each location individually in cache
    for (loc in locations) {
        loc_key <- paste0("single_", tolower(trimws(loc)))
        if (exists(loc_key, envir = .geoscatter_cache)) {
            cached_results[[loc]] <- get(loc_key, envir = .geoscatter_cache)
        }
    }
    
    return(cached_results)
}

#' Cache new geocoding results individually
#' @param locations Vector of location names
#' @param geocoded_data Data frame with geocoding results
cache_new_results <- function(locations, geocoded_data) {
    for (i in seq_along(locations)) {
        loc_key <- paste0("single_", tolower(trimws(locations[i])))
        result <- list(
            lat = geocoded_data$lat[i], 
            long = geocoded_data$long[i]
        )
        assign(loc_key, result, envir = .geoscatter_cache)
    }
}

#' Combine cached and new results in the correct order
#' @param locations Original location names in order
#' @param all_results Named list of lat/long results
#' @return Data frame in tidygeocoder format
combine_cached_results <- function(locations, all_results) {
    # Create result data frame in the same format as tidygeocoder::geo
    result <- data.frame(
        address = locations,
        lat = numeric(length(locations)),
        long = numeric(length(locations)),
        stringsAsFactors = FALSE
    )
    
    for (i in seq_along(locations)) {
        loc <- locations[i]
        if (loc %in% names(all_results)) {
            result$lat[i] <- all_results[[loc]]$lat
            result$long[i] <- all_results[[loc]]$long
        } else {
            result$lat[i] <- NA
            result$long[i] <- NA
        }
    }
    
    return(result)
}

#' Clear the geocoding cache
#' @param pattern Optional pattern to match cache keys (default: clear all)
#' @export
clear_geocoding_cache <- function(pattern = NULL) {
    if (is.null(pattern)) {
        # Clear entire cache
        rm(list = ls(envir = .geoscatter_cache), envir = .geoscatter_cache)
        cat("Geocoding cache cleared.\n")
    } else {
        # Clear matching entries
        keys <- ls(envir = .geoscatter_cache)
        matching_keys <- keys[grepl(pattern, keys)]
        if (length(matching_keys) > 0) {
            rm(list = matching_keys, envir = .geoscatter_cache)
            cat("Cleared", length(matching_keys), "cache entries matching pattern:", pattern, "\n")
        } else {
            cat("No cache entries found matching pattern:", pattern, "\n")
        }
    }
}

#' Get information about the geocoding cache
#' @export
geocoding_cache_info <- function() {
    cache_keys <- ls(envir = .geoscatter_cache)
    single_keys <- sum(grepl("^single_", cache_keys))
    locations_keys <- sum(grepl("^locations_", cache_keys))
    
    cat("Geocoding cache information:\n")
    cat("- Individual locations cached:", single_keys, "\n")
    cat("- Location sets cached:", locations_keys, "\n")
    cat("- Total cache entries:", length(cache_keys), "\n")
    
    # Estimate cache size
    cache_size <- object.size(.geoscatter_cache)
    cat("- Approximate cache size:", format(cache_size, units = "auto"), "\n")
    
    invisible(list(
        single_locations = single_keys,
        location_sets = locations_keys,
        total_entries = length(cache_keys),
        cache_size = cache_size
    ))
}