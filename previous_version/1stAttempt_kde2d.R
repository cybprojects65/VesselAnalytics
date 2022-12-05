rm(list=ls(all=TRUE))


library(MASS)
library(raster)

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

#nx<-round((max(unreported$x)-min(unreported$x))/0.1)
#ny<-round((max(unreported$y)-min(unreported$y))/0.1)

nx<-(max(unreported$x)-min(unreported$x))/0.1
ny<-(max(unreported$y)-min(unreported$y))/0.1

#k = kde2d(unreportedh[,1],unreportedh[,2], h=0.1, n=c(nx,ny), lims = c(range(unreported$x), range(unreported$y)))

k = kde2d(unreportedh[,1],unreportedh[,2], h=c(width.SJ(unreported$x), width.SJ(unreported$y) ), n=c(67,55), lims = c(c(12.22, 18.92), c(40.20, 45.70)))

#k = kde2d(unreportedh[,1],unreportedh[,2], h=0.1, n=c(67,55), lims = c(c(12.22, 18.92), c(40.20, 45.70)))

print(max(k$z))

r = raster(k)
r[r < 0.0001 ] <- NA

plot(r)

writeRaster(r,"unreported_norm.tiff",overwrite=T)


