#' Plot a Geographic Map
#'
#' Creates a map with a table as input, using shading to represent the values of
#' countries or states on the map.
#'
#' @inherit Column
#' @param x A matrix, two-dimensional array, table or vector, containing the
#'   data to be plotted. The \code{\link{rownames}} (or \code{\link{names}} in
#'   the case of a vector) should contain the names of the geographic entities
#'   to be plotted.
#' @param country Character string optionally stating the country that the states are from, if
#' \code{map.type} is \code{"states"}.
#' @param high.resolution Specifically request a high resolution map. Otherwise
#' the resolution of the map is chosen automatically based on the resolution required
#' for the requested countries or regions.
#' @param show.missing.regions Logical; Whether to plot regions not included in
#'    \code{x} with values of \code{NA}. Used only by \code{"leaflet"}.
#' @param treat.NA.as.0 Plots any \code{NA} values in the data and any
#'   geographical entities without data as having a zero value.
#' @param colors A vector of two colors, which are used as endpoints in
#'   interpolating colors.
#' @param ocean.color The color used for oceans (or background).
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
#' @param zip.country One of \code{"Automatic"}, \code{"USA"}, \code{"UK"} or \code{"Australia"}.
#' If \code{"Automatic"} an attempt is made to infer the country from the data.
#' @param legend.show Logical; Whether to display a legend with the color scale.
#' @param legend.font.family Font family of legend.
#' @param legend.font.color Font color of legend. Only used with \code{plotly} object.
#' @param legend.font.size Font size of legend. Changing the defaults for leaflet object can give strange spacing.
#' @param hovertext.font.size Only used with \code{plotly} object.
#' @return an HTML widget for \code{"leaflet"} or a \code{"plotly"} object.
#' @examples
#' data <- seq(4)
#' names(data) <- c("France", "China", "Brazil", "Canada")
#' GeographicMap(data)
#' @importFrom verbs Sum
#' @importFrom flipU StopForUserError
#' @export
GeographicMap <- function(x,
                          country,
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
                          background = FALSE,
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
                          zip.country = "Automatic")
{
    requireNamespace("sp")
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

    # Find map.type from rownames
    names <- tolower(rownames(table))

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
        opacity <- if (mapping.package == "leaflet" && background) 0.5 else 1.0
    if (mapping.package == "leaflet")
        color.warning <- "Alpha values in selected colors were not used in color scale. Adjust 'opacity' instead"
    else
        color.warning <- "Alpha values in colors for Geographic Map with plotly are ignored."
    colors <- StripAlphaChannel(colors)

    # Check for defined formats first, or if country or zip.country are specified.
    map.type <- definedFormatMapTypes(names, zip.country)
    if (!missing(country) && country != "")
    {
        map.type <- "states"
        country <- tidyCountryName(country)
    }

    if (map.type == "unknown")
    {
        types <- list(regions = c("northeast", "midwest", "south", "west"),
                      continents = c("africa", "asia", "europe", "north america", "oceania", "south america"),
                      aus_areas = tolower(australia.areas$name),
                      countries = tolower(CountriesOrContinents("country")))
        match.counts <- lapply(types, function(x) length(intersect(x, names)))
        country <- FindCountryFromRegions(names)
        match.counts[["states"]] <- attr(country, "matches")
        if (max(unlist(match.counts)) == 0)
            StopForUserError("No rows of the input data were matched with geographic entity names.")
        map.type <- names(which.max(match.counts))
    }

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
    else if (map.type == "aus_areas")
    {
        coords <- australia.areas
        remove.regions <- NULL
        name.map <- australiaAreasNameMap()
    }
    else
        StopForUserError("Unrecognized map.type")

    if (treat.NA.as.0)
        table[is.na(table)] <- 0

    statistic <- attr(table, "statistic", exact = TRUE)
    if (is.null(statistic))
        statistic <- ""

    # Fix unmatched names
    if (!is.null(name.map))
    {
        correct.all <- names(name.map)
        for (correct in correct.all)
        {
            incorrect <- setdiff(name.map[[correct]], correct.all)
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

    coords.names <- tolower(coords[[structure]])
    incorrect.names <- !tolower(rownames(table)) %in% coords.names
    n.unmatched.names <- Sum(incorrect.names)
    if (n.unmatched.names > 0.75 * nrow(table))
        warning(paste0(n.unmatched.names, " rows of the input data were not matched with",
                       " geographic entity names. Please check that the data you are plotting is one of:",
                       " countries; states of a country; continents; US regions; or US, UK or Australian zip codes."))
    if (any(incorrect.names))
    {
        msg <- paste("Unmatched region names:", paste(rownames(table)[incorrect.names], collapse = ", "))
        warning(msg)
    }
    if (all(incorrect.names) && zip.country != "Automatic")
        StopForUserError("No names in the data were matched to zip codes in your selected country.")

    table <- table[!incorrect.names, , drop = FALSE]
    table.names <- rownames(table)

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
        apply(table, 1, function(x) ifelse(!all(is.na(x)), max(x, na.rm = TRUE), NA))[country.lookup]
    else
        table[, 1][country.lookup]
    if (treat.NA.as.0)
        coords$table.max[is.na(coords$table.max)] <- 0

    min.in.table.max <- min(coords$table.max , na.rm = TRUE)
    if (min.value < min.in.table.max) #Replacing the minimum with the global minimum.
        coords$table.max[match(min.in.table.max, coords$table.max)] <- min.value
    max.range <- max(coords$table.max, na.rm = TRUE)

    # Remove regions not in input data
    if (!show.missing.regions)
        coords <- coords[!is.na(country.lookup), ]

    # Decide formatting for hovertext
    if (values.hovertext.format == "" && grepl("%)?$", statistic))
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
    legend.show <- setShowLegend(legend.show)

    # Pass all data to a function specific to the package
    if (mapping.package == "leaflet") {

        map <- leafletMap(coords, colors, opacity, values.bounds.minimum, values.bounds.maximum,
                   color.NA, legend.show, legend.title, legend.font.family,
                   legend.font.color, legend.font.size, mult, decimals, suffix,
                   values.hovertext.format, treat.NA.as.0, n.categories, categories,
                   format.function, map.type, background, ocean.color,
                   hovertext.font.family, hovertext.font.size, show.missing.regions)

    } else
    {
        # mapping.package == "plotly"
        map <- plotlyMap(table, name.map, colors, opacity, values.bounds.minimum,
                   values.bounds.maximum, color.NA, legend.show,
                   legend.title, mult, decimals, suffix, values.hovertext.format,
                   treat.NA.as.0, n.categories, categories, format.function, map.type,
                   ocean.color, high.resolution, title, subtitle, footer,
                   legend.font = list(family=legend.font.family, color=legend.font.color, size=legend.font.size),
                   title.font = list(family=title.font.family, color=title.font.color, size=title.font.size),
                   subtitle.font = list(family=subtitle.font.family, color=subtitle.font.color, size=subtitle.font.size),
                   footer.font = list(family=footer.font.family, color=footer.font.color, size=footer.font.size),
                   footer.wrap = footer.wrap, footer.wrap.nchar = footer.wrap.nchar,
                   hovertext.font.family, hovertext.font.size)
    }

    result <- list(htmlwidget = map)
    class(result) <- "StandardChart"
    attr(result, "ChartType") <- "Filled Map"
    result
}


# Helper function to plot the leaflet map
#' @importFrom leaflet leaflet colorNumeric addLegend labelFormat highlightOptions addPolygons
#' @importFrom leaflet addLayersControl layersControlOptions setView fitBounds addTiles tileOptions
#' @importFrom sp proj4string spTransform
#' @importFrom stats as.formula
#' @importFrom htmltools browsable tagList tags htmlDependency
leafletMap <- function(coords, colors, opacity, min.value, max.range, color.NA,
                       legend.show, legend.title, legend.font.family,
                       legend.font.color, legend.font.size,
                       mult, decimals, suffix, values.hovertext.format,
                       treat.NA.as.0, n.categories, categories, format.function, map.type,
                       background, ocean.color, hovertext.font.family, hovertext.font.size,
                       show.missing.regions)
{
    coords.with.values <- which(!is.na(coords$table.max))
    max.values <- unique(coords$table.max[coords.with.values])
    if (length(max.values) == 1)
        max.values <- c(max.values, max.values * 1.1)

    # If we are close to the anti meridian, wrap coords and polygons
    wrap.antimeridian <- FALSE
    if ("longitude" %in% colnames(coords@data) && map.type != "United States of America" &&
        map.type != "regions") {
        lng <- coords@data$longitude
        if (any(lng > 170)) {
            wrap.antimeridian <- TRUE
            .wrapAntiMeridian <- function(x) ifelse(x < 0, 360 + x, x)
            coords@data$longitude <- .wrapAntiMeridian(lng)
            for (i in 1:length(coords@polygons)) {
                n.poly <- length(coords@polygons[[i]]@Polygons)
                for (j in 1:n.poly) {
                    coords@polygons[[i]]@Polygons[[j]]@coords[,1] <- .wrapAntiMeridian(coords@polygons[[i]]@Polygons[[j]]@coords[,1])
                    coords@polygons[[i]]@Polygons[[j]]@labpt[1] <- .wrapAntiMeridian(coords@polygons[[i]]@Polygons[[j]]@labpt[1])
                }
                coords@polygons[[i]]@labpt[1] <- .wrapAntiMeridian(coords@polygons[[i]]@labpt[1])
            }
        }
    }

    # Creating the map
    map <- leaflet(coords)

    # Attribution requires background, which may be transparent
    attribution <- switch(map.type, aus_postcodes = "Based on ABS data",
                            aus_areas = "Based on ABS data",
                            uk_postcodes = "<a href='www.opendoorlogistics.com'>opendoorlogistics.com</a>",
                           "")
    map <- addTiles(map, attribution = attribution, options = tileOptions(opacity = as.numeric(background)))

    #opacity <- 1
    .pal <- colorNumeric(palette = colors, domain = c(min.value, max.range),
                         na.color = color.NA)
    .rev.pal <- colorNumeric(palette = colors, domain = c(min.value, max.range),
                             na.color = color.NA, reverse = TRUE)

    if (legend.show)
    {
        map <- addLegend(map, "bottomright", pal = .rev.pal, values = c(max.values, min.value),
                         title = legend.title,
                         # reverse label ordering so high values are at top
                         labFormat = labelFormat(transform = function(x) sort(x * mult, decreasing = TRUE),
                                                 digits = 3, # seems to work like an upper bound
                                                 suffix = suffix,
                                                 big.mark = ifelse(commaFromD3(values.hovertext.format), ",", "")),
                         opacity = opacity,
                         na.label = ifelse(treat.NA.as.0, "0", "NA"))
    }
    highlight.options <- highlightOptions(weight = 5, color = "#666",
                                          fillOpacity = 0.7,
                                          bringToFront = TRUE)

    # Add an outline of USA to fill gaps in zip code areas
    if (map.type == "us_postcodes" && show.missing.regions)
    {
        country <- "United States of America"
        # suppress warnings caused by sp update from PROJ4 to PROJ6
        suppressWarnings(country.coords <- spTransform(map.coordinates.50[map.coordinates.50$name == country, ],
            proj4string(coords)))
        country.coords$color <- ifelse(treat.NA.as.0, 0, NA)
        map <- addPolygons(map, stroke = FALSE, smoothFactor = 0.2,
                            fillOpacity = opacity, fillColor = ~.pal(country.coords$color),
                            data = country.coords)
    }

    ## DS-4143: When plotting U.S.A. regions, override hover text so that
    ##   it displays U.S. region names instead of state names
    if (map.type == "regions" && !anyNA(idx <- match(coords$name, us.regions[["State"]])))
        location.label <- as.character(us.regions[["Region"]][idx])
    else
        location.label <- coords$name

    if (n.categories == 1)
    {
        map <- addPolygons(map, stroke = FALSE, smoothFactor = 0.2,
                           fillOpacity = opacity, fillColor = ~.pal(coords$table1),
                           highlightOptions = highlight.options,
                           label = paste0(location.label, ": ", format.function(coords$table1,
                                                                             decimals = decimals,
                                                                             comma.for.thousands = commaFromD3(values.hovertext.format))))
        categoryControls <- ""
    }
    else
    {
        for (i in 1:n.categories)
        {
            cl <- as.formula(paste("~.pal(table", i, ")", sep = ""))
            map <- addPolygons(map, stroke = FALSE, smoothFactor = 0.2,
                               fillOpacity = opacity, color = cl, group = categories[i],
                               highlightOptions = highlight.options,
                               label = paste0(location.label, ": ",
                                              format.function(coords[[paste("table", i, sep = "")]],
                                                              decimals = decimals,
                                                              comma.for.thousands = commaFromD3(values.hovertext.format))))
            categoryControls <- paste0("
                document.querySelector('.leaflet-control-layers-expanded').style.backgroundColor = 'transparent';
                document.querySelector('.leaflet-control-layers-expanded').style.border = 'none';
                document.querySelector('.leaflet-control-layers-expanded').style.color = '", legend.font.color, "';")
        }
        map <- addLayersControl(map, baseGroups = categories,
                                options = layersControlOptions(collapsed = FALSE))
    }

    # Centre on the contiguous states, avoiding Alaska and Hawaii
    # This is for state and regional maps, not postcodes
    if (map.type == "United States of America" || map.type == "regions") {
        map <- setView(map, -96, 37.8, zoom = 4)
    } else if (wrap.antimeridian) {
        # Manually set zoom level to fit to modified coords
        lng <- coords@data$longitude
        ltd <- coords@data$latitude
        lng.rng <- range(lng, na.rm = TRUE)
        ltd.rng <- range(ltd, na.rm = TRUE)
        map <- fitBounds(map, lng.rng[1], ltd.rng[1], lng.rng[2], ltd.rng[2])
     }

    # Make legend semi-opaque if background is used to make it easier to read
    panel.bg <- if (background) 'rgba(220,220,220,0.4)' else 'transparent'

    # old versions of Geo. Map Standard R could pass NULL for this
    if (is.null(ocean.color))
        ocean.color <- formals(GeographicMap)$ocean.color
    if (is.null(legend.font.color))
        legend.font.color <- formals(GeographicMap)$global.font.color
    if (is.null(legend.font.family))
        legend.font.family <- formals(GeographicMap)$global.font.family
    if (is.null(legend.font.size))
        legend.font.size <- formals(GeographicMap)$legend.font.size

    ## Read in custom css, modify user-specified values,
    ##   write to temp. file, and add it as a dependency of the widget
    ## Note, this could also be achieved (less robustly) using
    ##   htmlwidgets::prependContent(map, "<style>...</style>")
    css <- readLines(system.file("assets", "css", "leaflet-custom.css",
                                 package = "flipStandardCharts"))
    for (var in c("panel.bg", "ocean.color", "legend.font.color",
                  "legend.font.family", "legend.font.size"))
        css <- sub(paste0("/[*]", var, "[*]/"), get(var), css)
    tf <- tempfile(fileext = ".css")
    writeLines(css, con = tf)
    dep <- htmlDependency(
          name = "displayr-leaflet-css",
          version = "0.0.1",
        src = c(file = file.path(dirname(tf))),
        stylesheet = basename(tf),
    #      attachment = basename(tf),
         all_files = FALSE
    )
    # As of leaflet version 2.2.0, HTML dependencies for leaflet() are included
    # in the dependencies item of the htmlwidget
    ndeps <- length(map$dependencies)
    if (ndeps == 0)
        map$dependencies <- list(dep)
    else
        map$dependencies[[ndeps + 1]] <- dep

    return(map)
}


# Helper function to plot the plotly map
#' @importFrom plotly plot_geo colorbar
plotlyMap <- function(table, name.map, colors, opacity, min.value, max.range, color.NA, legend.show,
           legend.title, mult, decimals, suffix, values.hovertext.format,
           treat.NA.as.0, n.categories, categories, format.function, map.type,
           ocean.color, high.resolution, title, subtitle, footer,
           legend.font, title.font, subtitle.font, footer.font,
           footer.wrap, footer.wrap.nchar, hovertext.font.family, hovertext.font.size)
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
        StopForUserError("Only world and USA state or region maps are available with 'plotly' package.",
                         " Change to 'leaflet' to map other types.")

    if (treat.NA.as.0)  # set NA color to zero color
    {
        color.zero <- colorRamp(colors)(0 - min(0, min.value) / (max.range - min(0, min.value)))
        color.NA <- rgb(color.zero, maxColorValue = 255)
    }

    line.color <- rgb(0.4, 0.4, 0.4, alpha = opacity)
    bdry <- list(color = rgb(0.4,0.4,0.4, alpha = opacity), width = 0)  # no boundary line between shaded regions

    # specify map projection/options
    g <- list(
        scope = scope,
        showframe = FALSE,
        showcoastlines = TRUE,
        showland = opacity == 1.0, # color will show through transparency
        landcolor = color.NA,
        showcountries = TRUE,
        coastlinecolor = if (ocean.color == color.NA) line.color else ocean.color,
        coastlinewidth = 0.25,
        countrycolor = line.color,
        countrywidth = 0.25,
        subunitcolor = line.color,
        subunitwidth = 0.25,
        showocean = opacity == 1.0,
        oceancolor = ocean.color,
        showlakes = opacity == 1.0,
        lakecolor = ocean.color,
        projection = list(type = 'Mercator'),
        resolution = ifelse(high.resolution, 50, 110),
        lataxis = lataxis,
        bgcolor = ocean.color)

    p <- plot_geo(df, locationmode = locationmode)
    ## DS-4143: When plotting U.S.A. regions, plotly recognizes state names,
    ##  but hovertext should use region names if that's what the user has supplied
    hover.text <- format.function(df[, 1], decimals = decimals,
                                  comma.for.thousands = commaFromD3(values.hovertext.format))
    hover.info <- "location+text"
    if (map.type == "regions" && scope == "usa")
    {
        regions <- us.regions[["Region"]][match(rownames(df), us.regions[["Code"]])]
        hover.text <- paste0(regions, " - ", hover.text)
        hover.info <- "text"
    }

    p <- add_trace(p, hoverinfo = hover.info,
            z = df[, 1],
            zmin = min.value,
            zmax = max.range,
            color = df[, 1],
            colors = rgb(t(col2rgb(colors)),maxColorValue = 255, alpha = opacity), # opacity ignored by plotly
            locations = rownames(df),
            text = hover.text,
            marker = list(line = bdry)
        )
    if (legend.show)
        p <- colorbar(p, title = legend.title, x = 1, y = 0.5, yanchor = "middle",
                outlinewidth = 0, ypad = 0,
                tickfont = legend.font, titlefont = legend.font,
                tickformat = values.hovertext.format,
                separatethousands = commaFromD3(values.hovertext.format))
    else
        p <- hide_colorbar(p)

     footer <- autoFormatLongLabels(footer, footer.wrap, footer.wrap.nchar, truncate=FALSE)
     margins <- list(l = 0, r = 0, t = 10, b = 0, pad = 0)
     margins <- setMarginsForText(margins, title, subtitle, footer,
                    title.font$size, subtitle.font$size, footer.font$size)
     p <- config(p, displayModeBar = FALSE)
     p$sizingPolicy$browser$padding <- 0
     p <- layout(p, geo = g, margin = margins,
            annotations = list(setSubtitle(subtitle, subtitle.font, margins),
                               setTitle(title, title.font, margins),
                               setFooter(footer, footer.font, margins)),
            hoverlabel = list(namelength = -1, bordercolor = "transparent",
                              font = list(family = hovertext.font.family, color = "white",
                                          size = hovertext.font.size)),
            paper_bgcolor = 'transparent'
        )
    p
}
