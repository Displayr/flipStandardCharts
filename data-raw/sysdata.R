# This script contains the data about the image data used in
# PictoStdChart() and SinglePicto()
# Data is stored as a data.frame available only internally
# Note that PictoChart takes URLs, and does not use imageURL

# Ratios are calculated from SVG file: viewBox="0 0 width height"
imageWHRatio <- c(apple=0.884,baby=1.0,banana=1.124,bank=1.039,barn=1.358,
                  beer=0.584,book=1,
                  bread=1,building=0.928,cake=1.005,car=1.693,cash=0.9,
                  chicken=0.890,church=0.812,citygate=0.998, computer=1.347,cow=1.293,
                  cross=1, cup=1.398,cutlery=1.398,dna=0.475,elephant=1.431,
                  fuel=1.431,glass=0.750,globe=1.431,government=0.717,
                  graduation=1.440,gun=1.595,heart=1.052,house=1.052,
                  idea=1.00,law=1,medicine=1,money=1,patient=0.704,
                  police=1.0,renewenergy=1.152,road=0.803,
                  rocket=0.803, rubbish=0.875,sickperson=0.556,
                  soldier=0.510,soup=0.851,sport=0.680,stack=16.317,
                  star=1.103,stickman=0.372,stickwoman=0.458,
                  testtube=1,thumbsup=1.048,thumbsdown=1.048,tick=1.301,
                  tomato=1.004,tools=1,trafficlight=0.594,
                  train=0.792,tree=0.872,truck=1.731,tv=1,user=0.860,
                  waterdrop=0.646,weight=0.998,wine=0.523,
                  trump=1.167, clinton=1.00,
                  circle=1, square=1)

image.names <- names(imageWHRatio)
imageURL <- sprintf("https://displayrcors.azureedge.net/images/%s_grey.svg", image.names)
names(imageURL) <- image.names
imageURL["circle"] <- ""
imageURL["square"] <- ""

save(imageURL, imageWHRatio, file = "R/sysdata.rda")
