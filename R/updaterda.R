# # This can be used to load in the existing "sysdata.rda" file, modify it and save it again.
#
# # load existing .rda file
# setwd("C:/Users/jake.NUMDOM2/Git packages/flipStandardCharts/R")
# load("sysdata.rda")
#
# # do something to manipualte/correct/extend data
#
# # e.g. extract table with HTTP GET
# library(httr)
# library(XML)
# url <- "https://developers.google.com/public-data/docs/canonical/countries_csv"
# r <- GET(url)
# country.center.coords <- readHTMLTable(doc = content(r, "text"))[[1]]
#
# # save back rda file with all (possibly updated) objects
# devtools::use_data(missing110,
#                    admin1.name.map,
#                    admin0.name.map.by.name,
#                    admin0.name.map.by.admin,
#                    admin1.coordinates,
#                    map.coordinates.50,
#                    map.coordinates.110,
#                    country.coordinates,
#                    ISO_3166_1,
#                    ISO_3166_2,
#                    country.center.coords,
#                    us.regions,
#                    internal = TRUE, overwrite = TRUE)
