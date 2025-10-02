#' Generate GeoJSON polygons for counties automatically
#' 
#' This file provides functions to automatically create GeoJSON polygons for counties
#' using various data sources and APIs.
#'
#' @importFrom sf st_read st_as_sf st_transform st_write
#' @importFrom dplyr filter mutate select
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom httr GET content

# Method 1: Using US Census Tiger/Line Shapefiles (Most Accurate)
#' Download and process county boundaries from US Census
#'
#' @param state_fips The FIPS code for the state (e.g., "36" for New York)
#' @param county_names Vector of county names to extract
#' @param year Census year (default: 2022)
#' @return sf object with county polygons
get_census_county_polygons <- function(state_fips, county_names = NULL, year = 2022) {
  # Check if required packages are available
  required_packages <- c("sf", "dplyr")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    stop("Required packages missing: ", paste(missing_packages, collapse = ", "), 
         "\nInstall with: install.packages(c('", paste(missing_packages, collapse = "', '"), "'))")
  }
  
  # Construct Census Tiger/Line URL
  base_url <- "https://www2.census.gov/geo/tiger/TIGER"
  shapefile_url <- paste0(base_url, year, "/COUNTY/tl_", year, "_us_county.zip")
  
  # Create temporary directory
  temp_dir <- tempdir()
  zip_file <- file.path(temp_dir, "counties.zip")
  
  tryCatch({
    # Download the shapefile
    cat("Downloading US county boundaries...\n")
    download.file(shapefile_url, zip_file, mode = "wb", quiet = FALSE)
    
    # Extract the zip file
    unzip(zip_file, exdir = temp_dir)
    
    # Find the .shp file
    shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)[1]
    
    # Read the shapefile
    counties <- sf::st_read(shp_file, quiet = TRUE)
    
    # Filter by state
    state_counties <- counties[counties$STATEFP == state_fips, ]
    
    # Filter by county names if provided
    if (!is.null(county_names)) {
      # Clean county names (remove "County" suffix if present)
      clean_names <- gsub("\\s+County\\s*$", "", county_names, ignore.case = TRUE)
      clean_sf_names <- gsub("\\s+County\\s*$", "", state_counties$NAME, ignore.case = TRUE)
      
      state_counties <- state_counties[clean_sf_names %in% clean_names, ]
    }
    
    # Transform to WGS84 (standard lat/lon)
    state_counties <- sf::st_transform(state_counties, 4326)
    
    return(state_counties)
    
  }, error = function(e) {
    stop("Failed to download or process census data: ", e$message)
  }, finally = {
    # Clean up temporary files
    unlink(c(zip_file, list.files(temp_dir, pattern = "tl_.*", full.names = TRUE)), 
           recursive = TRUE, force = TRUE)
  })
}

# Method 2: Using Natural Earth Data (Good for international)
#' Get county/admin boundaries from Natural Earth
#'
#' @param country Country name or ISO code
#' @param admin_names Vector of administrative division names
#' @param resolution Resolution: "10m", "50m", or "110m" (higher = more detailed)
#' @return sf object with admin polygons
get_natural_earth_polygons <- function(country = "United States of America", 
                                     admin_names = NULL, 
                                     resolution = "10m") {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required. Install with: install.packages('sf')")
  }
  
  # Natural Earth URL for admin divisions
  ne_url <- paste0("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/",
                   resolution, "/cultural/ne_", resolution, "_admin_1_states_provinces.zip")
  
  temp_dir <- tempdir()
  zip_file <- file.path(temp_dir, "ne_admin.zip")
  
  tryCatch({
    cat("Downloading Natural Earth administrative boundaries...\n")
    download.file(ne_url, zip_file, mode = "wb", quiet = FALSE)
    unzip(zip_file, exdir = temp_dir)
    
    shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)[1]
    admin_divisions <- sf::st_read(shp_file, quiet = TRUE)
    
    # Filter by country
    country_divisions <- admin_divisions[admin_divisions$ADMIN == country | 
                                      admin_divisions$ISO_A2 == country, ]
    
    # Filter by admin names if provided
    if (!is.null(admin_names)) {
      country_divisions <- country_divisions[country_divisions$NAME %in% admin_names, ]
    }
    
    return(country_divisions)
    
  }, error = function(e) {
    stop("Failed to download Natural Earth data: ", e$message)
  }, finally = {
    unlink(c(zip_file, list.files(temp_dir, pattern = "ne_.*", full.names = TRUE)), 
           recursive = TRUE, force = TRUE)
  })
}

# Method 3: Using OpenStreetMap Nominatim API (Simpler but less detailed)
#' Get county boundary from OpenStreetMap Nominatim
#'
#' @param county_names Vector of county names
#' @param state_name State name for context
#' @param country Country name (default: "United States")
#' @return List of polygon coordinates
get_osm_county_polygons <- function(county_names, state_name, country = "United States") {
  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Packages 'httr' and 'jsonlite' are required. Install with: install.packages(c('httr', 'jsonlite'))")
  }
  
  results <- list()
  
  for (county in county_names) {
    # Construct search query
    query <- paste(county, "County", state_name, country, sep = ", ")
    
    # Nominatim API URL
    url <- "https://nominatim.openstreetmap.org/search"
    
    tryCatch({
      cat("Geocoding:", county, "...\n")
      
      response <- httr::GET(url, query = list(
        q = query,
        format = "json",
        polygon_geojson = 1,
        limit = 1
      ))
      
      # Add delay to respect API limits
      Sys.sleep(1)
      
      if (httr::status_code(response) == 200) {
        content <- httr::content(response, "text", encoding = "UTF-8")
        data <- jsonlite::fromJSON(content)
        
        if (length(data) > 0 && !is.null(data$geojson)) {
          results[[county]] <- data$geojson[1, ]
        } else {
          warning("No polygon found for: ", county)
          results[[county]] <- NULL
        }
      } else {
        warning("API request failed for: ", county)
        results[[county]] <- NULL
      }
      
    }, error = function(e) {
      warning("Error geocoding ", county, ": ", e$message)
      results[[county]] <- NULL
    })
  }
  
  return(results)
}

# Method 4: Convert to your GeoJSON format
#' Convert sf object to your specific GeoJSON format
#'
#' @param sf_data sf object with county polygons
#' @param pop_data Optional data frame with population data (columns: county, pop)
#' @return List in your GeoJSON format
convert_to_your_geojson <- function(sf_data, pop_data = NULL) {
  if (!requireNamespace("sf", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Packages 'sf' and 'jsonlite' are required.")
  }
  
  features <- list()
  
  for (i in 1:nrow(sf_data)) {
    row <- sf_data[i, ]
    
    # Get population if available
    pop <- NA
    if (!is.null(pop_data)) {
      pop_match <- pop_data[pop_data$county == row$NAME, "pop"]
      if (length(pop_match) > 0) pop <- pop_match[1]
    }
    
    # Convert geometry to coordinates
    coords <- sf::st_coordinates(row)
    
    # Group coordinates by polygon/hole
    if (nrow(coords) > 0) {
      # This is a simplified conversion - you might need to adjust based on your needs
      coord_list <- split(coords[, 1:2], coords[, 3])
      coord_arrays <- lapply(coord_list, function(x) {
        matrix_coords <- as.matrix(x)
        lapply(1:nrow(matrix_coords), function(j) c(matrix_coords[j, 1], matrix_coords[j, 2]))
      })
      
      feature <- list(
        type = "Feature",
        id = i,
        properties = list(
          id = ifelse(is.null(row$GEOID), as.character(i), row$GEOID),
          pop = if (is.na(pop)) NULL else pop,
          county = paste(row$NAME, "County"),
          state = ifelse(is.null(row$STATE_NAME), "Unknown", row$STATE_NAME)
        ),
        geometry = list(
          type = "MultiPolygon",
          coordinates = list(coord_arrays)
        )
      )
      
      features[[i]] <- feature
    }
  }
  
  geojson <- list(
    type = "FeatureCollection",
    features = features
  )
  
  return(geojson)
}

# Main function to generate county GeoJSON
#' Generate GeoJSON for a list of counties
#'
#' @param county_names Vector of county names
#' @param state_name State name (for US counties)
#' @param state_fips State FIPS code (for US counties, e.g., "36" for NY)
#' @param method Method to use: "census", "natural_earth", or "osm"
#' @param output_file Optional file path to save the GeoJSON
#' @param pop_data Optional population data
#' @return GeoJSON list or saves to file
generate_county_geojson <- function(county_names, 
                                  state_name = "New York", 
                                  state_fips = "36",
                                  method = "census",
                                  output_file = NULL,
                                  pop_data = NULL) {
  
  cat("Generating GeoJSON for", length(county_names), "counties using", method, "method...\n")
  
  if (method == "census") {
    sf_data <- get_census_county_polygons(state_fips, county_names)
    geojson <- convert_to_your_geojson(sf_data, pop_data)
    
  } else if (method == "natural_earth") {
    sf_data <- get_natural_earth_polygons("United States of America", county_names)
    geojson <- convert_to_your_geojson(sf_data, pop_data)
    
  } else if (method == "osm") {
    osm_data <- get_osm_county_polygons(county_names, state_name)
    # Convert OSM data to your format (would need additional processing)
    stop("OSM method not fully implemented yet")
    
  } else {
    stop("Unknown method. Use 'census', 'natural_earth', or 'osm'")
  }
  
  # Save to file if specified
  if (!is.null(output_file)) {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Package 'jsonlite' required for saving JSON")
    }
    
    jsonlite::write_json(geojson, output_file, pretty = TRUE, auto_unbox = TRUE)
    cat("GeoJSON saved to:", output_file, "\n")
  }
  
  return(geojson)
}

# Example usage function
#' Example of how to use the county polygon generator
#'
#' @export
example_usage <- function() {
  # Example 1: Generate polygons for specific New York counties
  counties <- c("Albany", "Erie", "Monroe", "Suffolk")
  
  # Method 1: Using Census data (recommended)
  tryCatch({
    geojson <- generate_county_geojson(
      county_names = counties,
      state_name = "New York",
      state_fips = "36",
      method = "census",
      output_file = "my_counties.geojson"
    )
    cat("Success! Generated polygons for", length(counties), "counties\n")
  }, error = function(e) {
    cat("Error:", e$message, "\n")
    cat("Try installing required packages: install.packages(c('sf', 'dplyr'))\n")
  })
  
  # Example 2: With population data
  pop_data <- data.frame(
    county = c("Albany", "Erie", "Monroe", "Suffolk"),
    pop = c(306945, 919866, 749606, 1499738)
  )
  
  # Example 3: Generate for all counties in a state
  # all_ny_counties <- generate_county_geojson(
  #   county_names = NULL,  # NULL means all counties
  #   state_fips = "36",
  #   method = "census"
  # )
}