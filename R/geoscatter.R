#' @importFrom leaflet leaflet
#' @importFrom tidygeocoder geocode
#'  res <- geo( address = c("Tokyo", "Manhatten", "Perth"),  method = "osm" )

# Helper function to geocode station names
geocode_stations <- function(station_names) {
  # Check if tidygeocoder is available
  if (!requireNamespace("tidygeocoder", quietly = TRUE)) {
    stop("Package 'tidygeocoder' is required for geocoding. Please install it with: install.packages('tidygeocoder')")
  }
  
  # Create a data frame with station names
  locations_df <- data.frame(name = station_names, stringsAsFactors = FALSE)
  
  # Geocode the locations
  tryCatch({
    geocoded <- tidygeocoder::geocode(locations_df, address = name, method = "osm")
    return(geocoded)
  }, error = function(e) {
    warning("Geocoding failed: ", e$message)
    # Return a data frame with NA coordinates if geocoding fails
    return(data.frame(
      name = station_names,
      lat = NA,
      long = NA,
      stringsAsFactors = FALSE
    ))
  })
}

GeoScatter <- function(x, station_names = NULL, station_types = NULL, geocode_new = FALSE)
{
  # Default data
  default_stations <- c("North Creek", "South Pond", "East Lake", "Hoboken Waterfront")
  default_lats <- c(40.7128, 40.6892, 40.7580, 40.7439)
  default_lons <- c(-74.0060, -74.0445, -73.9855, -74.0323)
  default_types <- c("River", "Pond", "Lake", "Waterfront")
  
  if (!is.null(station_names) && geocode_new) {
    # Geocode new station names
    cat("Geocoding station names...\n")
    geocoded_data <- geocode_stations(station_names)
    
    # Check for failed geocoding (NA coordinates)
    failed_geocoding <- is.na(geocoded_data$lat) | is.na(geocoded_data$long)
    if (any(failed_geocoding)) {
      warning("Failed to geocode the following stations: ", 
              paste(geocoded_data$name[failed_geocoding], collapse = ", "))
    }
    
    # Use provided station types or default to "Unknown"
    if (is.null(station_types)) {
      station_types <- rep("Unknown", length(station_names))
    } else if (length(station_types) != length(station_names)) {
      warning("Length of station_types doesn't match station_names. Using 'Unknown' for missing types.")
      station_types <- c(station_types, rep("Unknown", length(station_names) - length(station_types)))[1:length(station_names)]
    }
    
    water_data_df <- data.frame(
      station_name = geocoded_data$name,
      latitude = geocoded_data$lat,
      longitude = geocoded_data$long,
      station_type = station_types,
      stringsAsFactors = FALSE
    )
    
    # Remove rows with NA coordinates
    water_data_df <- water_data_df[!is.na(water_data_df$latitude) & !is.na(water_data_df$longitude), ]
    
  } else {
    # Use default data or provided coordinates
    if (!is.null(station_names)) {
      warning("station_names provided but geocode_new=FALSE. Using default coordinates. Set geocode_new=TRUE to geocode new names.")
    }
    
    water_data_df <- data.frame(
      station_name = default_stations,
      latitude = default_lats,
      longitude = default_lons,
      station_type = default_types,
      stringsAsFactors = FALSE
    )
  }
  
  # Calculate center point for map view
  center_lat <- mean(water_data_df$latitude, na.rm = TRUE)
  center_lon <- mean(water_data_df$longitude, na.rm = TRUE)
  
  leaflet(data = water_data_df) %>%
    addTiles() %>%
    setView(lng = center_lon, lat = center_lat, zoom = 10) %>%
    addMarkers(~longitude, ~latitude, popup = ~station_name, label = ~station_type) 
}