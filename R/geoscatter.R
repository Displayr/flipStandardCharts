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
#' names(data) <- c("New York", "London", "Tokyo", "Paris")
#' GeoScatter(data)
#' @importFrom leaflet leaflet addTiles addCircleMarkers colorNumeric addLegend fitBounds labelFormat tileOptions
#' @importFrom tidygeocoder geocode geo
#' @importFrom verbs Sum
#' @importFrom flipU StopForUserError
#' @importFrom htmltools browsable tagList tags htmlDependency
#' @importFrom flipChartBasics StripAlphaChannel
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
    
    # Geocode the locations
    tryCatch({
        geocoded_data <- tidygeocoder::geo(address = locations, method = "osm")
    }, error = function(e) {
        stop("Failed to geocode locations. Please check that tidygeocoder package is installed and location names are valid.")
    })
    
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