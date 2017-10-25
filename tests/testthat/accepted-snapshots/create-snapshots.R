library(plotly)
library(webshot)

# This script is used to create the pngs which are compared against
# new snap-shots when devtools::check is run
# Note that this script needs to be run manually
# Figures are created in the current directory

# Question? How machine-sensitive are the image fingerprints
# plotly::export only handles plotly charts
# but this uses webshot package, which should handle any widgets

library(flipStandardCharts)
xx <- structure(1:10, .Names=letters[1:10])
pp <- Column(xx)
export(pp$plotly.plot, file="column-default.png")
