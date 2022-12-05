rm(list=ls(all=TRUE))

library(sf)
library(terra)
library(spatialEco)

unreported<-read.csv(file.choose(),header=T,sep=",")

xcolumn<-"xcentroid"
ycolumn<-"ycentroid"
tothcolumn<-"total_hours"
intnormcolumn<-"intensity_normalised"

names(unreported)[names(unreported) == xcolumn] <- "x"
names(unreported)[names(unreported) == ycolumn] <- "y"
names(unreported)[names(unreported) == tothcolumn] <- "totalh"
names(unreported)[names(unreported) == intnormcolumn] <- "intensity_normalised"

unreported<-unreported[,c("x","y","totalh","intensity_normalised")]

unreportedh<-unreported[which(unreported$intensity_normalised=='high'),]
xmax <- max(unreported$x)
xmin <- min(unreported$x)
ymax <- max(unreported$y)
ymin <- min(unreported$y)



unreportedh <- st_as_sf(unreportedh, coords = c("x", "y"), crs = 4326,
                  agr = "constant")


# Weighted KDE using cadmium and extent with automatic bandwidth selection
( e <- st_bbox(unreportedh)[c(xmin,xmax,ymin,ymax)] )
totalh.kde <- sf.kde(x = unreportedh, y = unreportedh$totalh, ref = e,
                      standardize = TRUE,
                      scale.factor = 100, res=0.01)
plot(totalh.kde)
