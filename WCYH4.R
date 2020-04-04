setwd("~/Desktop/studia/2nd_year/GEOG2025/assessment/WCYH4/data")

#load packages
library(sp)
library(rgdal)
library(ggplot2)
library(tmap)
library(gdata)
library(raster)
library(adehabitatHR)
library(tmaptools)
library(psych)
library(RColorBrewer)
library(spdep)
library(GISTools)
library(spgwr)
library(fpc)
library(gridExtra)
library(grid)
library(gtable)
library(spatstat)
library(dismo)


#load datasets
#binding datasets from June to December, 2016
stop_and_search1 <- read.csv("stop_and_search/2016-06/2016-06-metropolitan-stop-and-search.csv")
stop_and_search2 <- read.csv("stop_and_search/2016-07/2016-07-metropolitan-stop-and-search.csv")
stop_and_search3 <- read.csv("stop_and_search/2016-08/2016-08-metropolitan-stop-and-search.csv")
stop_and_search4 <- read.csv("stop_and_search/2016-09/2016-09-metropolitan-stop-and-search.csv")
stop_and_search5 <- read.csv("stop_and_search/2016-10/2016-10-metropolitan-stop-and-search.csv")
stop_and_search6 <- read.csv("stop_and_search/2016-11/2016-11-metropolitan-stop-and-search.csv")
stop_and_search7 <- read.csv("stop_and_search/2016-12/2016-12-metropolitan-stop-and-search.csv")
stop_and_search <- rbind(stop_and_search1, stop_and_search2, stop_and_search3, 
                         stop_and_search4, stop_and_search5, stop_and_search6, 
                         stop_and_search7)
d.index <- read.xls("ID_2015_for_London.xls", sheet=2)
drug.misuse.deaths <- read.xls("localauthoritiesregistrations201416final.xls", sheet=3, header=TRUE)
drug.misuse <- read.xls("drug-misuse-borough.xls", sheet=6)
census.data1 <- read.xls("lsoa-data_2011.xls", sheet=1)
census.data2 <- read.xls("lsoa-data_2011.xls", sheet=2)
#merging census data
census.data <- merge(census.data1, census.data2, by="Codes")
crime <- read.csv("LSOA_Crime.csv")

#load shapefiles and set coordinates system
boroughs.map <- readOGR("statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")
projection(boroughs.map) <- CRS("+init=epsg:27700")
Tower.Hamlets <- readOGR("TowerHamlets/shapefiles/Tower_Hamlets_lsoa11.shp")
projection(Tower.Hamlets) <- CRS("+init=epsg:27700")

#drug misuse deaths London 2014-2016
#Age-standardised mortality rate per 100,000 population
#ates are not calculated where there are fewer than 3 deaths, 
#as rates based on such low numbers are susceptible to inaccurate interpretation. (Westminster)
drug.misuse.deaths <- drug.misuse.deaths[1:394,]
drug.misuse.deaths[drug.misuse.deaths==":"] <- NA
drug.misuse.deaths$Rate <- as.numeric(drug.misuse.deaths$Rate)
drug.misuse.deaths$Deaths <- as.numeric(drug.misuse.deaths$Deaths)
summary(drug.misuse.deaths)
names(boroughs.map)
names(drug.misuse.deaths)
boroughs.drug.misuse.deaths <- merge(boroughs.map, drug.misuse.deaths, by.x="GSS_CODE", by.y="Area.Code")

#map of rate of deaths related to drug misuse in London
 tm_shape(boroughs.drug.misuse.deaths) + 
  tm_fill("Rate", style="pretty",n=5, palette="Greys",title="Death rate", colorNA="White")+ 
  tm_borders(col="black", alpha = 0.6, lwd=1.4) + 
  tm_compass(size = 10, type="8star", position = c("right", "bottom")) + 
  tm_layout(legend.text.size = 1.2, legend.title.size= 1.9, 
            legend.position=c("left", "bottom"), frame = FALSE,
            title="Drug misuse deaths in London",
            title.size = 1.7)

#function with table of the 5 observations with the highest rate, ordered by the second variable (rate)
my.table <- function(data, variable1, variable2, font_size, colname1, colname2){
df <- data.frame(data@data[,variable1], data@data[,variable2])
df <- df[order(-data@data[,variable2]),]
death.rates <- head(df, n=5)
death.rates.g <- tableGrob(death.rates, rows = NULL, cols=c(colname1, colname2),theme = ttheme_default(base_size = font_size))
death.rates.g <- gtable_add_grob(death.rates.g,grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)), t = 2, b = nrow(death.rates.g), 
                                 l = 1, r = ncol(death.rates.g))
death.rates.g <- gtable_add_grob(death.rates.g,grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)), t = 1, l = 1, r = ncol(death.rates.g)) 
grid.draw(death.rates.g)
}
my.table(boroughs.drug.misuse.deaths, "Borough", "Rate", 20 , "Borough", "Drug misuse death rate")
grid.newpage()

#drug misuse
#Estimated crude rate of opiate and/or crack cocaine users, per 1,000 aged 15-64
summary(drug.misuse)
names(drug.misuse)
names(drug.misuse) <- c("Code", "Authority", "Number of drug users", "Population", "Rate", "Lower_95_CI","Upper_95_CI")
drug.misuse$Rate <- as.numeric(drug.misuse$Rate)
boroughs.drug.misuse <- merge(boroughs.map, drug.misuse, by.x="GSS_CODE", by.y="Code")

#map of drug misuse in London
tm_shape(boroughs.drug.misuse) +
  tm_fill("Rate", n=8, palette="BuPu",title="Rate of drug users")+ 
  tm_borders(col="grey30", alpha = 0.6, lwd=1.4) + 
  tm_compass(size = 10, type="8star", position = c("right", "bottom")) + 
  tm_layout(legend.text.size = 1.2, legend.title.size= 1.9, 
            legend.position=c("left", "bottom"), frame = FALSE,
            title.size = 1.75, title = "Drug users in London")

#table of 5 boroughs with the highesr rates
my.table(boroughs.drug.misuse, "Authority", "Rate", 20, "Borough", "Drug misuse rate")
grid.newpage()

#stop and search
stop_and_search <-subset(stop_and_search, select=c("Type", "Date", 
                                                   "Longitude","Latitude", "Gender", "Age.range",
                                                   "Officer.defined.ethnicity", 
                                                   "Legislation", "Object.of.search", 
                                                   "Outcome"))
stop_and_search <- subset(stop_and_search,
                          Legislation=="Misuse of Drugs Act 1971 (section 23)")
#excluding those without coordinates
stop_and_search[stop_and_search==''] <- NA
stop_and_search.c <-subset(stop_and_search, Latitude!="NA")
stop_and_search.c <-subset(stop_and_search.c, Longitude!="NA")
summary(stop_and_search.c)
#creating SpatialPointsDataFrame
summary(data.frame(as.factor(stop_and_search.c$Longitude), as.factor(stop_and_search.c$Latitude)))
stop_and_search.sp <-SpatialPointsDataFrame(stop_and_search.c[,3:4], 
                                            stop_and_search.c, proj4string = CRS("+init=EPSG:4326"))
stop_and_search.sp <- spTransform(stop_and_search.sp, CRS("+init=epsg:27700"))

#subseting points to observations only within Tower Hamlets borough
stop_and_search.sp.T <- stop_and_search.sp[Tower.Hamlets,]
summary(stop_and_search.sp.T)
plot(stop_and_search.sp.T)

#function which creates a map of Kernel Density Estimatation for a chosen SpatialPointsDataFrame object
KDE <- function(SPDF, map,point.size, range1, range2, range3, range4, color, title, grid){
  kde <- kernelUD(SPDF, h="href", grid = grid)
  tm_shape(map) + 
    tm_fill(col = "#f0f0f0") + tm_borders(alpha=.8, col = color)+
    tm_shape(SPDF) + tm_dots(col = "black", size=point.size) + tm_shape(getverticeshr(kde, percent = range1)) + 
    tm_borders(alpha=.6, col = "tomato1", lwd = 2) + tm_fill(alpha=.1, col = "tomato1") +
    tm_shape(getverticeshr(kde, percent = range2)) + tm_borders(alpha=.65, col = "tomato2", lwd = 2) + 
    tm_fill(alpha=.15, col = "tomato2") + tm_shape(getverticeshr(kde, percent = range3)) + 
    tm_borders(alpha=.7, col = "tomato3", lwd = 2) + 
    tm_fill(alpha=.2, col = "tomato3") + tm_shape(getverticeshr(kde, percent = range4)) + 
    tm_borders(alpha=.75, col = "tomato4", lwd = 2) + 
    tm_fill(alpha=.25, col = "tomato4") + tm_scale_bar(position="left", size=0.6)+
    tm_layout(frame=FALSE, title = title, title.size = 1.3) +
    tm_compass(size = 7, type="8star", position = c("right", "bottom"))
}
#map of KDE at the city level
KDE(stop_and_search.sp, boroughs.map, 0.02, 75, 50, 25, 5, "royalblue1", 
    "Kernel Density Estimation for London", 1000)

#map of KDE at the borough level
KDE(stop_and_search.sp.T, Tower.Hamlets, 0.125, 75, 50, 25, 5, "royalblue1", 
    "Kernel Density Estimation 
    for Tower Hamlets", 1000)

#KDE for Tower Hamlets
kde <- kernelUD(stop_and_search.sp.T, h="href", grid = 2000)

#ranges for Kernel density estimates
range75 <- getverticeshr(kde, percent = 75) 
range50 <- getverticeshr(kde, percent = 50) 
range25 <- getverticeshr(kde, percent = 25)
range5 <- getverticeshr(kde, percent = 5)

#KDE for Tower
KDE.for.Tower <-  tm_shape(stop_and_search.sp.T) + tm_dots(col = "black", size=0.125) +tm_shape(range75) + 
  tm_borders(alpha=.6, col = "tomato1", lwd = 2) + tm_fill(alpha=.1, col = "tomato1") +
  tm_shape(range50) + tm_borders(alpha=.65, col = "tomato2", lwd = 2) + 
  tm_fill(alpha=.15, col = "tomato2") + tm_shape(range25) + 
  tm_borders(alpha=.7, col = "tomato3", lwd = 2) + 
  tm_fill(alpha=.2, col = "tomato3") + tm_shape(range5) + 
  tm_borders(alpha=.75, col = "tomato4", lwd = 2) + 
  tm_fill(alpha=.25, col = "tomato4") + tm_scale_bar(position="left", size=0.6) 

#Index of deprivation in Tower Hamlets and deprivation
names(Tower.Hamlets)
Tower.Hamlets.d.index <- merge(Tower.Hamlets, d.index, by.x="LSOA11CD", by.y="LSOA.code..2011.")
names(Tower.Hamlets.d.index)
tm_shape(Tower.Hamlets.d.index) + 
  tm_fill("IMD.Decile..where.1.is.most.deprived.10..of.LSOAs.",
          palette = "Purples", 
          title="IMD Decile")+
  tm_borders(col="grey30", alpha = 0.6, lwd=1.4)+ 
  tm_layout(legend.text.size = 1.25, legend.title.size= 1.75, 
            legend.position=c(0, 0.05), frame = FALSE, title = "Ranking of most deprived LSOAs")  + 
  tm_compass(size = 8, type="8star", position = c("right", "bottom"))  + KDE.for.Tower

#merge census data with stop and search data
names(census.data)
census.data <- census.data[,1:129]
names(Tower.Hamlets)
Tower.Hamlets.census.data <- merge(Tower.Hamlets, census.data, by.x="LSOA11CD", by.y="Codes")

#KDE + unemployment in Tower Hamlets
tm_shape(Tower.Hamlets.census.data) + 
  tm_fill("Unemployment.Rate",n=6, 
          palette = "Greens", title="Unemployment Rate")+
  tm_borders(col="grey30", alpha = 0.6, lwd=1.4) + 
  tm_compass(size = 7, type="8star", position = c("right", "bottom")) + 
  tm_layout(legend.text.size = 1.2, legend.title.size= 1.7, 
            legend.position=c(0, 0.05), frame = FALSE)  + KDE.for.Tower

#KDE + BAME in Tower Hamlets
names(Tower.Hamlets.census.data)
tm_shape(Tower.Hamlets.census.data) + 
  tm_fill("BAME....",n=6, 
          palette = "Blues", title="% of Black and Minority Ethnic")+
  tm_borders(col="grey30", alpha = 0.6, lwd=1.4) + 
  tm_compass(size = 7, type="8star", position = c("right", "bottom")) + 
  tm_layout(legend.text.size = 1.2, legend.title.size= 1.7, 
            legend.position=c(0, 0.05), frame = FALSE)  + KDE.for.Tower

#ethnicity of stopped person defined by officer
summary(stop_and_search$Officer.defined.ethnicity)
freqtable <- table(stop_and_search$Officer.defined.ethnicity)
prop.table(freqtable)

#GSTAT function
Gstat.function <- function(SPDF, variable, title){
  nb <- dnearneigh(coordinates(SPDF),0,700)
  nb_lw <- nb2listw(nb, style = 'B')
  local_g <- localG(SPDF@data[,variable], nb_lw)
  SPDF@data <- cbind(SPDF@data, as.matrix(local_g))
  names(SPDF)[ncol(SPDF)] <- "gstat"
  tm_shape(SPDF) + tm_fill("gstat", palette = "RdBu", style = "pretty") + 
    tm_borders(alpha=.4) +tm_layout(frame=FALSE) + tm_scale_bar(position="left", size=0.5) +
    tm_layout(title = title , legend.text.size = 1, legend.title.size= 1.5, 
              legend.position=c(0, 0.05), frame = FALSE)+  
    tm_compass(size = 6, type="8star", position = c("right", "bottom"))
}
Gstat.function(Tower.Hamlets.census.data, "Unemployment.Rate", "Gstat for Unemployment Rate") + KDE.for.Tower
Gstat.function(Tower.Hamlets.census.data, "BAME....", "Gstat for Black and Minority Ethnic") + KDE.for.Tower

#quadrant count
window <- as.owin(Tower.Hamlets)
stop_and_search.sp.T.ppp <- ppp(x=stop_and_search.sp.T@coords[,1], y=stop_and_search.sp.T@coords[,2], window = window)
plot(quadratcount(stop_and_search.sp.T.ppp, nx=20, ny=20), col="red")

#count points in polygons
Tower.Hamlets.census.data@data$points.in.polygon <- poly.counts(stop_and_search.sp.T, Tower.Hamlets)
Tower.Hamlets.census.data@data$points.in.polygon
max(Tower.Hamlets.census.data@data$points.in.polygon)
google.map <- gmap("Tower Hamlets, London", type='satellite')
plot(google.map)
 tm_shape(Tower.Hamlets.census.data) + tm_fill("points.in.polygon", n=6, title = "Points in polygon", alpha=1) + 
  tm_borders(col="grey30", alpha = 0.65, lwd=1.4) + 
  tm_compass(size = 7, type="8star", position = c("right", "bottom")) + 
  tm_layout(legend.text.size = 1.25,legend.title.size= 1.75, 
            legend.position=c(0, 0.05), frame = FALSE, legend.text.color = "black") +
  tm_shape(google.map) + tm_raster(alpha = 0.72)

#crime
summary(crime)
crime <- subset(crime, Major.Category=="Drugs")
crime$sum2016 <-  crime$X201606 + crime$X201607+crime$X201608 + crime$X201609 + crime$X201610 + crime$X201611 + crime$X201612
crime <- aggregate(sum2016 ~ LSOA.Code, data=crime, sum)

#Tower Hamlets
Tower.Hamlets.census.data.crime <- merge(Tower.Hamlets.census.data, crime,by.x="LSOA11CD", by.y="LSOA.Code")
sum(is.na(Tower.Hamlets.census.data.crime@data$sum2016))
Tower.Hamlets.census.data.crime@data[is.na(Tower.Hamlets.census.data.crime@data)] <- 0

#crime + KDE
tm_shape(Tower.Hamlets.census.data.crime) + 
  tm_fill("sum2016",n=8, 
          palette = "Spectral", title="Crime")+
  tm_borders(col="grey30", alpha = 0.6, lwd=1.4) + 
  tm_compass(size = 7, type="8star", position = c("right", "bottom")) + 
  tm_layout(title="Drug-related crime",title.size = 2, legend.text.size = 1.2, legend.title.size= 1.7, 
            legend.position=c(0, 0.05), frame = FALSE) + KDE.for.Tower

#Geographically weighted regression models between points in polygon and chosen variable
GWR.function <- function(SPDF,fill, variable1, variable2, variable3, legend.title, title){
  GWRbandwidth <- gwr.sel(SPDF@data[,variable1] ~ SPDF@data[,variable2]+SPDF@data[,variable3], 
                          data=SPDF,adapt=T)
  gwr.model = gwr(SPDF@data[,variable1]~ SPDF@data[,variable2]+SPDF@data[,variable3], 
                  data = SPDF, adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 
  results <-as.data.frame(gwr.model$SDF)
  SPDF@data <- cbind(SPDF@data, as.matrix(results))
  tm_shape(SPDF) + tm_fill(fill, n=7, title=legend.title) + tm_borders(col="grey30", alpha = 0.6, lwd=1.4) + 
    tm_compass(size = 7, type="8star", position = c("right", "bottom")) + tm_scale_bar(position="left", size=0.6)+
    tm_layout(title=title,legend.text.size = 1.2,legend.title.size= 1.7, 
              legend.position=c(0,0.05), frame = FALSE)
}
GWR.function(Tower.Hamlets.census.data.crime, "SPDF.data...variable2.", "sum2016"  ,"Unemployment.Rate",
             "BAME....", "Coefficient", "GWR for Unemployment Rate")
GWR.function(Tower.Hamlets.census.data.crime, "SPDF.data...variable3.", "sum2016"  ,"Unemployment.Rate", 
             "BAME....", "Coefficient", "GWR for Black and Minority Ethnic")

#R-squared
GWR.function(Tower.Hamlets.census.data.crime, "localR2","sum2016", "Unemployment.Rate", "BAME....", "Local R2", "")