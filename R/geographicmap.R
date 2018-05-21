#' Plot a Geographic Map
#'
#' Creates a map with a table as input, using shading to represent the values of
#' countries or states on the map.
#'
#' @param x A matrix, two-dimensional array, table or vector, containing the
#'   data to be plotted. The \code{\link{rownames}} (or \code{\link{names}} in
#'   the case of a vector) should contain the names of the geographic entities
#'   to be plotted.
#' @param country Character string optionally stating the country that the states are from, if
#' \code{map.type} is \code{"states"}.
#' @param high.resolution Specifically request a high resolution map. Otherwise
#' the resolution of the map is chosen automatically based on the resolution required
#' for the requested countries or regions.
#' @param treat.NA.as.0 Plots any \code{NA} values in the data and any
#'   geographical entities without data as having a zero value.
#' @param colors A vector of two colors, which are used as endpoints in
#'   interpolating colors.
#' @param ocean.color The color used for oceans, used only by \code{"plotly"}.
#' @param color.NA The color used to represent missing values. Not used when
#'   \code{treat.NA.as.0}, is set to missing.
#' @param global.font.family Character; font family for all occurrences of any
#' font attribute for the chart unless specified individually.
#' @param global.font.color Global font color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. #' rgb(0, 0, 0, maxColorValue = 255)).
#' @param legend.title The text to appear above the legend.
#' @param values.hovertext.format A string representing a d3 formatting code.
#' See https://github.com/d3/d3/blob/master/API.md#number-formats-d3-format
#' @param values.bounds.minimum Numeric; specifies the minimum value in the colorscale.
#'          If not specified, this will be automatically determined from the data.
#'          If the value specified is larger than the minimum in the data then
#'          the specified value will be ignored.
#' @param values.bounds.maximum Numeric; specifies the maximum value in the colorscale.
#' @param mapping.package Either \code{"leaflet"} (better graphics, wider range of
#' maps) or \code{"plotly"} (faster).
#' @param background If \code{"mapping.package"} is \code{"leaflet"}, add a background
#' tile from opensteetmaps.
#' @param zip.country One of \code{"Automatic"}, \code{"USA"}, \code{"UK"} or \code{"Australia."}
#' If \code{"Automatic"} an attempt is made to infer the country from the data.
#' @param legend.show Logical; Whether to display a legend with the color scale.
#' @param legend.font.family Font family of legend. Only used with \code{plotly} object.
#' @param legend.font.color Font color of legend. Only used with \code{plotly} object.
#' @param legend.font.size Font size of legend. Only used with \code{plotly} object.
#' @return an HTML widget for \code{"leaflet"} or a \code{"plotly"} object.
#' @examples
#' data <- seq(4)
#' names(data) <- c("France", "China", "Brazil", "Canada")
#' GeographicMap(data)
#' @export
GeographicMap <- function(x,
                          country,
                          high.resolution = FALSE,
                          treat.NA.as.0 = FALSE,
                          colors = c("#CCF3FF", "#23B0DB"),
                          ocean.color = "#DDDDDD",
                          color.NA = "#808080",
                          global.font.family = "Arial",
                          global.font.color = rgb(44, 44, 44, maxColorValue = 255),
                          legend.show = TRUE,
                          legend.title = "",
                          legend.font.family = global.font.family,
                          legend.font.color = global.font.color,
                          legend.font.size = 10,
                          values.hovertext.format = "",
                          values.bounds.minimum = NULL,
                          values.bounds.maximum = NULL,
                          mapping.package = "leaflet",
                          background = FALSE,
                          zip.country = "Automatic")
{
    requireNamespace("sp")
    values.bounds.minimum <- charToNumeric(values.bounds.minimum)
    values.bounds.maximum <- charToNumeric(values.bounds.maximum)

    table <- cleanMapInput(x)

    # Find map.type from rownames
    names <- tolower(rownames(table))
    if (any(names %in% c("northeast", "midwest", "south", "west")))
        map.type <- "regions"
    else if (any(names %in% c("africa", "asia", "europe", "north america", "oceania", "south america")))
        map.type <- "continents"
    else if (any(names %in% tolower(CountriesOrContinents("country"))) || all(nchar(rownames(table)) == 3))
        map.type <- "countries"
    else
        map.type <- postcodesOrStates(names, zip.country)
    if (grepl("postcode", map.type, fixed = TRUE))
        rownames(table) <- tidyPostcodes(names, map.type)

    # Get the coordinate and name data
    if (map.type == "countries" || map.type == "continents")
    {
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
        name.map <- admin0.name.map.by.admin
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
        map.type <- country
        remove.regions <- NULL
    }
    else if (map.type == "regions")
    {
        coords <- subset(admin1.coordinates, admin1.coordinates$admin == "United States of America")
        name.map <- admin1.name.map[["United States of America"]]
        remove.regions <- NULL
        states <- coords[["name"]]    # updated table to states
        regions <- us.regions$Region[match(states, us.regions$State)]
        table <- table[match(tolower(regions), tolower(rownames(table))), , drop = FALSE]
        rownames(table) <- states
    }
    else if (map.type == "aus_postcodes")
    {
        coords <- australia.postcodes
        remove.regions <- name.map <- NULL
    }
    else if (map.type == "us_postcodes")
    {
        coords <- us.postcodes
        remove.regions <- name.map <- NULL
    }
    else if (map.type == "uk_postcodes")
    {
        coords <- uk.postcodes
        remove.regions <- name.map <- NULL
    }


    if (treat.NA.as.0)
        table[is.na(table)] <- 0

    statistic <- attr(table, "statistic", exact = TRUE)
    if (is.null(statistic))
        statistic <- ""

    # Fix unmatched names
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

    structure <- switch(map.type, continents = "continent", countries = "admin", "name")
    coords[[structure]] <- as.character(coords[[structure]])

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

    table.names <- rownames(table)
    coords.names <- tolower(coords[[structure]])
    incorrect.names <- !tolower(table.names) %in% coords.names

    if (any(incorrect.names))
    {
        msg <- paste("Unmatched region names:", paste(table.names[incorrect.names], collapse = ", "))
        stop(msg)
    }

    # Splicing data onto coordinate data.frame.
    country.lookup <- match(coords.names, tolower(table.names))
    categories <- colnames(table)
    n.categories <- length(categories)
    if (mapping.package == "plotly" && n.categories > 1)
    {
        n.categories <- 1
        table <- table[, 1, drop = FALSE]
        warning("Only the first series will be shown when package is 'plotly'. Change to 'leaflet' to show multiple series.")
    }
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
    if (treat.NA.as.0)
        min.value <- min(0, min.value)

    coords$table.max <- if(NCOL(table) != 1 && mapping.package != "plotly")
        apply(table, 1, max, na.rm = TRUE)[country.lookup]
    else
        table[, 1][country.lookup]
    if (treat.NA.as.0)
        coords$table.max[is.na(coords$table.max)] <- 0

    min.in.table.max <- min(coords$table.max , na.rm = TRUE)
    if (min.value < min.in.table.max) #Replacing the minimum with the global minimum.
        coords$table.max[match(min.in.table.max, coords$table.max)] <- min.value
    max.range <- max(coords$table.max, na.rm = TRUE)

    # Decide formatting for hovertext
    if (values.hovertext.format == "" && grepl("%", statistic, fixed = TRUE))
        values.hovertext.format <- ".0%"
    if (percentFromD3(values.hovertext.format))
    {
        format.function <- FormatAsPercent
        decimals <- decimalsFromD3(values.hovertext.format, 0)
        mult <- 100
        suffix <- "%"
    }
    else
    {
        format.function <- FormatAsReal
        decimals <- decimalsFromD3(values.hovertext.format, 2)
        mult <- 1
        suffix <- ""
    }
    if (!is.null(values.bounds.minimum) && values.bounds.minimum > min.value)
        warning("Minimum value must be smaller than ", min.value, ".")
    if (!is.null(values.bounds.maximum) && values.bounds.maximum < max.range)
        warning("Maximum value must be larger than ", max.range, ".")
    values.bounds.minimum <- min(values.bounds.minimum, min.value)
    values.bounds.maximum <- max(values.bounds.maximum, max.range)

    # Pass all data to a function specific to the package
    if (mapping.package == "leaflet") {

        map <- leafletMap(coords, colors, values.bounds.minimum, values.bounds.maximum,
                   color.NA, legend.show, legend.title, mult, decimals, suffix,
                   values.hovertext.format, treat.NA.as.0, n.categories, categories,
                   format.function, map.type, background)

    } else {        # mapping.package == "plotly"

        font <- list(family = legend.font.family, color = legend.font.color,
                     size = legend.font.size)
        map <- plotlyMap(table, name.map, colors, values.bounds.minimum,
                   values.bounds.maximum, color.NA, legend.show,
                   legend.title, mult, decimals, suffix, values.hovertext.format,
                   treat.NA.as.0, n.categories, categories, format.function, map.type,
                  ocean.color, high.resolution, font)
    }

    result <- list(htmlwidget = map)
    class(result) <- "StandardChart"
    result
}


# Helper function to plot the leaflet map
#' @importFrom leaflet leaflet colorNumeric addLegend labelFormat highlightOptions addPolygons
#' @importFrom leaflet addLayersControl layersControlOptions setView addTiles tileOptions
#' @importFrom sp proj4string spTransform
#' @importFrom stats as.formula
leafletMap <- function(coords, colors, min.value, max.range, color.NA, legend.show,
                       legend.title, mult, decimals, suffix, values.hovertext.format,
                       treat.NA.as.0, n.categories, categories, format.function, map.type, background)
{
    max.values <- unique(coords$table.max[!is.na(coords$table.max)])
    if (length(max.values) == 1)
        max.values <- c(max.values, max.values * 1.1)

    # Creating the map
    map <- leaflet(coords)

    # Attribution requires background, which may be transparent
    attribution <- switch(map.type, aus_postcodes = "Based on ABS data",
                            uk_postcodes = "<a href='www.opendoorlogistics.com'>opendoorlogistics.com</a>",
                           "")
    map <- addTiles(map, attribution = attribution, options = tileOptions(opacity = as.numeric(background)))

    opacity <- 1
    .pal <- colorNumeric(palette = colors, domain = c(min.value, max.range),
                         na.color = color.NA)
    .rev.pal <- colorNumeric(palette = rev(colors), domain = c(min.value, max.range),
                             na.color = color.NA)

    if (legend.show)
    {
        map <- addLegend(map, "bottomright", pal = .rev.pal, values = c(min.value, max.values),
                         title = legend.title,
                         # reverse label ordering so high values are at top
                         labFormat = labelFormat(transform = function(x) sort(x * mult, decreasing = TRUE),
                                                 digits = decimals,
                                                 suffix = suffix,
                                                 big.mark = ifelse(commaFromD3(values.hovertext.format), ",", "")),
                         opacity = opacity,
                         na.label = ifelse(treat.NA.as.0, "0", "NA"))
    }
    highlight.options <- highlightOptions(weight = 5, color = "#666",
                                          fillOpacity = 0.7,
                                          bringToFront = TRUE)

    # Add an outline of USA to fill gaps in zip code areas
    if (map.type == "us_postcodes")
    {
        country <- "United States of America"
        country.coords <- spTransform(map.coordinates.50[map.coordinates.50$name == country, ], proj4string(coords))
        country.coords$color <- ifelse(treat.NA.as.0, 0, NA)
        map <- addPolygons(map, stroke = FALSE, smoothFactor = 0.2,
                            fillOpacity = opacity, fillColor = ~.pal(country.coords$color),
                            data = country.coords)
    }

    if (n.categories == 1)
    {
        map <- addPolygons(map, stroke = FALSE, smoothFactor = 0.2,
                           fillOpacity = opacity, fillColor = ~.pal(coords$table1),
                           highlightOptions = highlight.options,
                           label = paste0(coords$name, ": ", format.function(coords$table1,
                                                                             decimals = decimals,
                                                                             comma.for.thousands = commaFromD3(values.hovertext.format))))
    }
    else
    {
        for (i in 1:n.categories)
        {
            cl <- as.formula(paste("~.pal(table", i, ")", sep = ""))
            map <- addPolygons(map, stroke = FALSE, smoothFactor = 0.2,
                               fillOpacity = opacity, color = cl, group = categories[i],
                               highlightOptions = highlight.options,
                               label = paste0(coords$name, ": ",
                                              format.function(coords[[paste("table", i, sep = "")]],
                                                              decimals = decimals,
                                                              comma.for.thousands = commaFromD3(values.hovertext.format))))
        }
        map <- addLayersControl(map, baseGroups = categories,
                                options = layersControlOptions(collapsed = FALSE))
    }

    # Centre on the contiguous states
    if (map.type == "United States of America" || map.type == "regions")
        map <- setView(map, -96, 37.8, zoom = 4)

    map

}


# Helper function to plot the plotly map
#' @importFrom plotly plot_geo colorbar
plotlyMap <- function(table, name.map, colors, min.value, max.range, color.NA, legend.show,
           legend.title, mult, decimals, suffix, values.hovertext.format,
           treat.NA.as.0, n.categories, categories, format.function, map.type,
           ocean.color, high.resolution, font)
{
    df <- data.frame(table)
    df <- df[!is.na(df[, 1]), , drop = FALSE]  # avoid warning for NA

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
        stop("Only world and USA state or region maps are available with 'plotly' package.",
             " Change to 'leaflet' to map other types.")

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

        add_trace(#hoverinfo = "text", # should display 'text' only but causes all hovertext to disappear
            z = df[, 1],
            zmin = min.value,
            zmax = max.range,
            color = df[, 1],
            colors = colors,
            locations = rownames(df),
            text = format.function(df[, 1], decimals = decimals, comma.for.thousands = commaFromD3(values.hovertext.format)),
            marker = list(line = bdry)
        ) %>%

        config(displayModeBar = F) %>%

        layout(
            title = "",
            geo = g,
            margin = list(l = 0, r = 0, t = 0, b = 0, pad = 0),
            paper_bgcolor = 'transparent'
        )

    if (legend.show)
        p <- colorbar(p, title = legend.title, tickfont = font, titlefont = font,
                separatethousands = commaFromD3(values.hovertext.format), x = 1)
    else
        p <- hide_colorbar(p)

    p$sizingPolicy$browser$padding <- 0  # remove padding

    p
}

