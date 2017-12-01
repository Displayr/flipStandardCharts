#' Base mapping function
#'
#' Creates a map with a table as an input, using shading to represent the values
#' of regions on the map.
#'
#' @param table A matrix, two-dimensional array, table or vector, containing the
#'   data to be plotted. The \code{\link{rownames}} (or \code{\link{names}} in
#'   the case of a vector) should contain the names of the geographic entities
#'   to be plotted.
#' @param coords The coordinates to be mapped.
#' @param treat.NA.as.0 Plots any \code{NA} values in the data and any
#'   geographical entities without data as having a zero value.
#' @param only.show.regions.in.table When TRUE, only geographic entities that
#'   are included in the table are shown on the map.
#' @param add.detail Display names of geographical entities on the map. When
#'   TRUE, it also changes the appearance of the map, making the map wrap
#'   around. The only way to prevent this is to resize the map.
#' @param remove.last.column Deletes the last column of the table prior to
#'   creating the map, unless the table is a vector or only has one column.
#' @param remove.last.row Deletes the bottom row of the table prior to creating
#'   the map, unless the table is a vector or only has one row.
#' @param colors A vector of two colors, which are used as endpoints in
#'   interpolating colors.
#' @param ocean.color The color used for oceans, used only by \code{plotly}.
#' @param color.NA The color used to represent missing values. Not used when
#'   \code{treat.NA.as.0}, is set to missing.
#' @param legend.title The text to appear above the legend.
#' @param remove.regions The regions to remove, even if they are in the table.
#' @param unmatched.regions.is.error If there are regions in \code{table} that
#'   are not found in \code{coords}, if this is \code{TRUE} it will cause an
#'   error, otherwise just print a message.
#' @param name.map A mapping between incorrect and correct names, useful for
#'   automatically fixing names that a commonly misspecified. Should be a list
#'   where the keys are the correct names and the values are vectors of
#'   incorrect names that should be changed.
#' @param mapping.package Either \code{"leaflet"} (better graphics, more country
#' maps) or \code{"plotly"} (faster).
#' @param high.resolution Specifically request a high resolution map. Otherwise
#' the resolution of the map is chosen automatically based on the resolution required
#' for the requested countries or regions.
#' @param map.type One of \code{"continents"}, \code{"countries"}, \code{"regions"} or
#' the name of a country, which respectively plot a world map by continent, a world
#' map by country, a map of USA by region, or a single country map by state.
#'
#' @details This function is based on the \code{leaflet} package. See
#'   \url{https://rstudio.github.io/leaflet/} for an overview of this package
#'   and how to use it without using these functions.
#' @importFrom stringr str_trim
#' @importFrom stats as.formula
#' @importFrom utils data
#' @importFrom leaflet addPolygons leaflet addLayersControl layersControlOptions
#' @importFrom leaflet addTiles colorNumeric addLegend labelFormat setView highlightOptions
#' @importFrom plotly plot_geo colorbar layout add_trace %>% toRGB config
#' @importFrom grDevices colorRamp rgb
#' @export
BaseMap <- function(table,
                    coords,
                    treat.NA.as.0 = FALSE,
                    only.show.regions.in.table = FALSE,
                    add.detail = FALSE,
                    remove.last.column = FALSE,
                    remove.last.row = FALSE,
                    colors = c("#CCF3FF", "#23B0DB"),
                    ocean.color = "#DDDDDD",
                    color.NA = "#808080",
                    legend.title = "",
                    remove.regions = NULL,
                    unmatched.regions.is.error = TRUE,
                    name.map = NULL,
                    mapping.package = "leaflet",
                    high.resolution = FALSE,
                    map.type = "countries")
{
    table <- cleanMapInput(table)

    if (remove.last.column && ncol(table) > 1)
        table <- table[, -ncol(table), drop = FALSE]

    if (remove.last.row)
        table <- table[-nrow(table), , drop = FALSE]

    if (treat.NA.as.0)
        table[is.na(table)] <- 0

    statistic <- attr(table, "statistic", exact = TRUE)
    if (is.null(statistic))
        statistic <- ""

    # Tidying some names.
    if (!is.null(name.map))
    {
        for (correct in names(name.map))
        {
            incorrect <- name.map[[correct]]
            matches <- match(tolower(incorrect), tolower(rownames(table)))

            if (!all(is.na(matches)))
                rownames(table)[matches[!is.na(matches)]] <- correct
        }
    }

    structure <- ifelse(map.type == "continents", "continent", "name")
    coords[[structure]] <- as.character(coords[[structure]])

    if (map.type == "regions")   # updated table to states
    {
        states <- coords[[structure]]
        regions <- us.regions$Region[match(states, us.regions$State)]
        table <- table[match(regions, rownames(table)), , drop = FALSE]
        rownames(table) <- states
    }

    if (!is.null(remove.regions) && remove.regions != "")
    {
        remove.regions <- str_trim(unlist(strsplit(remove.regions, ",", fixed = TRUE)))
        if (!is.null(name.map))
        {
            for (region in names(name.map))
            {
                alt <- name.map[[region]]
                matches <- match(alt, remove.regions)

                if (!all(is.na(matches)))
                    remove.regions[matches[!is.na(matches)]] <- region
            }
        }

        coords <- coords[!(coords[[structure]] %in% remove.regions), ]
        table <- table[!(rownames(table) %in% remove.regions), , drop = FALSE]
    }

    if (only.show.regions.in.table)
        coords <- coords[coords[[structure]] %in% rownames(table), ]

    # Checking to see if input data is OK.
    if (treat.NA.as.0)
    {
        table <- table[apply(table, 1, max, na.rm = TRUE) > 0, , drop = FALSE]
    }

    table.names <- rownames(table)
    coords.names <- tolower(coords[[structure]])
    incorrect.names <- !tolower(table.names) %in% coords.names

    if (any(incorrect.names))
    {
        msg <- paste("Unmatched region names:", paste(table.names[incorrect.names], collapse = ", "))
        if (unmatched.regions.is.error)
            stop(msg)
        else
            message(msg)
    }

    # Splicing data onto coordinate data.frame.
    country.lookup <- match(coords.names, tolower(table.names))
    categories <- colnames(table)
    n.categories <- length(categories)
    for (i in 1:n.categories)
    {
        new.var <- table[country.lookup, i]

        if(treat.NA.as.0)
            new.var[is.na(new.var)] <- 0

        coords$table <- new.var
        names(coords)[ncol(coords)] <- paste("table", i, sep = "")
    }

    # Creating a variable for use in scaling the legend.
    min.value <- min(table, na.rm = TRUE)
    if (treat.NA.as.0 && nrow(table) < nrow(coords))
        min.value <- min(0, min.value)

    coords$table.max <- apply(table, 1, max)[country.lookup]
    if (treat.NA.as.0)
        coords$table.max[is.na(coords$table.max)] <- 0

    min.in.table.max <- min(coords$table.max , na.rm = TRUE)
    if (min.value < min.in.table.max) #Replacing the minimum with the global minimum.
        coords$table.max[match(min.in.table.max, coords$table.max)] <- min.value
    max.range <- max(coords$table.max, na.rm = TRUE)

    if (mapping.package == "leaflet") {

        # Creating the map
        map <- leaflet(coords)
        opacity <- 1
        if (add.detail)
        {
            opacity <- 0.2
            map <- addTiles(map)
        }
        .pal <- colorNumeric(palette = colors, domain = c(min.value, max.range),
                             na.color = color.NA)
        .rev.pal <- colorNumeric(palette = rev(colors), domain = c(min.value, max.range),
                             na.color = color.NA)

        # reverse label ordering so high values are at top
        reverseLabelFormat = function(...){
            function(type = "numeric", cuts){
                cuts <- sort(cuts, decreasing = T)
            }
        }

        map <- addLegend(map, "bottomright", pal = .rev.pal, values = ~table.max,
                         title = legend.title,
                         labFormat = reverseLabelFormat(),
                         opacity = opacity,
                         na.label = ifelse(treat.NA.as.0, "0", "NA"))
        highlight.options <- highlightOptions(weight = 5, color = "#666",
                                              dashArray = "",
                                              fillOpacity = 0.7,
                                              bringToFront = TRUE)

        if (n.categories == 1)
        {
            map <- addPolygons(map, stroke = FALSE, smoothFactor = 0.2,
                               fillOpacity = opacity, fillColor = ~.pal(coords$table1),
                               highlightOptions = highlight.options,
                               label = paste0(coords$name, ": ", coords$table1))
        }
        else
        {
            for (i in 1:n.categories)
            {
                cl <- as.formula(paste("~.pal(table", i, ")", sep = ""))
                map <- addPolygons(map, stroke = FALSE, smoothFactor = 0.2,
                                   fillOpacity = opacity, color = cl, group = categories[i],
                                   highlightOptions = highlight.options,
                                   label = paste0(coords$name, ": ", coords[[paste("table", i, sep = "")]]))
            }
            map <- addLayersControl(map, baseGroups = categories,
                                    options = layersControlOptions(collapsed = FALSE))
        }

        # Centre on the contiguous states
        if (map.type == "United States of America")
            map <- setView(map, -96, 37.8, zoom = 4)

        map

    } else {        # mapping.package == "plotly"

        df <- data.frame(table)

        if (ncol(df) > 1)
        {
            warning("Only the first series will be shown when package is 'plotly'. Change to 'leaflet' to show multiple series.")
        }
        lataxis <- NULL
        if (map.type == "countries")
        {
            locationmode <- "country names"
            lataxis <- list(range = c(-55, 75))
            scope <- "world"

            if (treat.NA.as.0)  # add rows of zeros for missing countries
            {
                missing.countries <- names(name.map)[!tolower(names(name.map)) %in% tolower(rownames(df))]
                zeros.matrix <- matrix(rep(0, length(missing.countries) * ncol(df)), ncol = ncol(df))
                colnames(zeros.matrix) <- colnames(df)
                rownames(zeros.matrix) <- missing.countries
                df <- rbind(df, zeros.matrix)
            }
        }
        else if (map.type == "United States of America" || map.type == "regions")
        {
            locationmode <- "USA-states"
            lataxis <- NULL
            scope <- "usa"

            # Convert names to 2 letter state codes required by plotly
            for (full.state.name in names(name.map))
            {
                all.state.names <- c(full.state.name, name.map[[full.state.name]])
                matches <- match(tolower(all.state.names), tolower(rownames(df)))

                if (!all(is.na(matches)))
                    rownames(df)[matches[!is.na(matches)]] <- all.state.names[nchar(all.state.names) == 2]
                else if (treat.NA.as.0)  # add row of zeros for this state
                {
                    df <- rbind(df, rep(0, ncol(df)))
                    rownames(df)[nrow(df)] <- all.state.names[nchar(all.state.names) == 2]
                }
            }
        }
        else
            stop("Only world and USA state or region maps are available with plotly.")

        if (treat.NA.as.0)  # set NA color to zero color
        {
            color.zero <- colorRamp(colors)(0 - min(0, min.value) / (max.range - min(0, min.value)))
            color.NA <- rgb(color.zero, maxColorValue = 255)
        }

        opacity <- 0.5
        bdry <- list(color = "#666666", width = 0)  # no boundary line between shaded regions

        # specify map projection/options
        g <- list(
            scope = scope,
            showframe = FALSE,
            showcoastlines = TRUE,
            showland = TRUE,
            landcolor = color.NA,
            showcountries = TRUE,
            countrycolor = "#666666",  # boundary line between NA regions
            countrywidth = 0.25,
            showocean = TRUE,
            oceancolor = ocean.color,
            showlakes = TRUE,
            lakecolor = ocean.color,
            projection = list(type = 'Mercator'),
            resolution = ifelse(high.resolution, 50, 110),
            lataxis = lataxis,
            bgcolor = toRGB("white", 0))  # transparent

        p <- plot_geo(df,
                      locationmode = locationmode
            ) %>%

            add_trace(
                z = df[, 1], color = df[, 1], colors = colors,
                text = NULL, locations = rownames(df),
                marker = list(line = bdry)
            ) %>%

            colorbar(title = legend.title,
                     separatethousands = TRUE,
                     x = 1
            ) %>%

            config(displayModeBar = F) %>%

            layout(
                title = "",
                geo = g,
                margin = list(l = 0, r = 0, t = 0, b = 0, pad = 0),
                paper_bgcolor = 'transparent'
            )

        p$sizingPolicy$browser$padding <- 0  # remove padding

        p
    }
}


#' \code{GeographicRegionRowNames} Names of geographic regions.
#'
#' Returns the list of unique geographic names that can be used when creating a
#' WorldMap.
#'
#' @param type The name of the geographic region type. See
#'   \code{\link{GeographicRegionTypes}}
#'
#' @examples
#' GeographicRegionRowNames("name")
#' GeographicRegionRowNames("continent")
#'
#' @export
GeographicRegionRowNames <- function(type)
{
    #data("map.coordinates.50", package = "flipGeographicCoordinates")
    # Make sure the dataset gets loaded
    #invisible(map.coordinates.50)

    requireNamespace("sp")
    type.names <- map.coordinates.50[[type]]

    if (is.factor(type.names))
        levels(type.names)
    else
        unique(type.names)
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
# # Reading the coordinates.
# getCoordinates <- function()
# {
#     return(rgdal::readOGR("https://raw.github.com/datasets/geo-boundaries-world-110m/master/countries.geojson", "OGRGeoJSON"))
# }


#' Get the states in a country
#'
#' When mapping sthe states of a country you need to match the state names exactly.
#' You can use this function to look up the correct names of the states for the
#' country that you are interested in.
#'
#' @param country The country to look at
#' @export
#' @seealso \code{\link{GeographicRegionRowNames}}
StatesInCountry <- function(country)
{
    country <- tidyCountryName(country)
    levels(droplevels(subset(admin1.coordinates, admin1.coordinates$admin == country)$name))
}

#' Standardize country name
#' @param country The country to search for
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

    if (!(country %in% levels(admin1.coordinates$admin)))
        stop("Country '", country, "' not found.")

    return(country)
}


#' Find the name of a country based upon a vector of state names
#'
#' @param states Character vector of states.
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


cleanMapInput <- function(table)
{
    # Correcting rowname errors for country names.
    # Neatening the data.
    statistic <- attr(table, "statistic", exact = TRUE)

    table.name <- deparse(substitute(table))
    if (is.null(dim(table)) || length(dim(table)) == 1) # better than is.vector()
    {
        if(is.null(names(table)))
            stop(paste(table.name, "has no names."))

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

    if (all(!is.na(suppressWarnings(as.numeric(rownames(table))))) && statistic == "Text")
        stop(paste(table.name, "contains text and has numeric row names. Did you mean to convert this table to percentages?"))

    if (is.null(statistic))
        attr(table, "statistic") <- statistic

    table
}




