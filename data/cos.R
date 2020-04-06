setwd("~/Desktop/studia/2nd_year/GEOG2025/data")

#load packages
library(tiff)
require(gdistance)

h <- function(r, min, max) {
  rr <- r[]
  rr[rr < min | rr > max] <- NA
  r[] <- rr
  r
}


Access<- raster("accessibility_to_cities_2015_v1/accessibility_to_cities_2015_v1.0.tif")
Pop <- raster("gpw-v4-population-density-2015/gpw-v4-population-density_2015.tif")

sample.A<-h(Access,10,100)
sample.P<-h(Pop,10,100)

map <- sample.A*sample.P
png("over5hrs_map.png", width=20, height=10, units="cm", res=200)
plot(map, col="black")
dev.off()
