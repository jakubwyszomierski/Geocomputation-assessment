setwd("~/Desktop/studia/2nd_year/GEOG2025/data/week5_tracks_data")
library("rgdal")
library("ggplot2")
library("png")
library("maptools")
wrld <- readOGR(".", "ne_110m_admin_0_countries")
btitle <- readPNG("brit_titles.png")
compass <- readPNG("windrose.png")
bdata <- read.csv("british_shipping_example.csv")
View(bdata)
summary(bdata)
xquiet <- scale_x_continuous("", breaks = NULL)
yquiet <- scale_y_continuous("", breaks = NULL)
quiet <- list(xquiet, yquiet)
wrld.f <- fortify(wrld, region = "sov_a3")
base <- ggplot(wrld.f, aes(x=long, y=lat))
wrld <- c(geom_polygon(aes(group=group), size=0.1, colour="black", fill="#D6BF86", data=wrld.f, alpha=1))
base+wrld + quiet

route <- c(geom_path(aes(long, lat, group=paste(bdata$trp, bdata$group.regroup, sep=".")), colour="red", size=0.2, data = bdata, alpha=0.5, lineend = "round"))

base + route + wrld + theme(panel.background = element_rect(fill='#BAC4B9', colour = 'black')) + annotation_raster(btitle, xmin=30, xmax=140, ymin=51, ymax=87) + annotation_raster(compass, xmin=65, xmax=105, ymin=25, ymax=65) + coord_equal() + quiet

earth <- readPNG("earth_raster.png")
base + annotation_raster(earth, xmin=-180, xmax=180, ymin=-90, ymax=90) + route + theme(panel.background=element_rect(fill='#BAC4B9', colour='black')) + annotation_raster(compass, xmin=65, xmax=105, ymin=25, ymax=65) +coord_equal() + quiet

library("plyr")
input <- read.table("wu03ew_v1.csv", sep = ",", header = T)
input <- input[,1:3]
names(input) <- c("origin", "destination", "total")
summary(input)
centroids <- read.csv("msoa_popweightedcentroids.csv")
or.xy <- merge(input, centroids, by.x="origin", by.y="Code")
names(or.xy) <- c("origin", "destination", "trips", "o_name", "oX", "oY")
dest.xy <- merge(or.xy, centroids, by.x="destination", by.y="Code")
names(dest.xy) <- c("origin", "destination", "trips", "o_name", "oX", "oY", "d_name", "dX", "dY")

ggplot(dest.xy[which(dest.xy$trips>10),], aes(oX, oY)) + geom_segment(aes(x=oX, y=oY, xend=dX, yend=dY, alpha=trips), col="white") + scale_alpha_continuous(range=c(0.03, 0.3)) + theme(panel.background = element_rect(fill = 'black', colour = 'black')) + quiet + coord_equal()