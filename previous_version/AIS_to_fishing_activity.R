### estimating fishing activity ###
rm(list=ls(all=TRUE))

library(raster)
library(robis)
library(sf)
library(lwgeom)
library(maps)
library(sqldf)
library(cowplot)
library(ggplot2)
library(DT)
library(sqldf)

##input data
cat("1. reading file (could take some minutes) \n")

#inputTable<-"Med-region-5min-Fishing-vessels-2019_01.csv"
inputTable<-"Med-region-5min-Fishing-vessels-2019_01.csv"
dataVessel<-read.csv(inputTable,header=T,sep=",")

xcolumn<-"LONGITUDE"
ycolumn<-"LATITUDE"
speedcolumn<-"SPEED"
vesselidcolumn<-"MMSI"
datetimecolumn<-"DATETIME"
datecolumn<-"DATE.UTC."
timecolumn<-"TIME.UTC."

## load data
names(dataVessel)[names(dataVessel) == xcolumn] <- "x"
names(dataVessel)[names(dataVessel) == ycolumn] <- "y"
names(dataVessel)[names(dataVessel) == speedcolumn] <- "speed"
names(dataVessel)[names(dataVessel) == vesselidcolumn] <- "vesselid"
names(dataVessel)[names(dataVessel) == datecolumn] <- "date"
names(dataVessel)[names(dataVessel) == timecolumn] <- "time"
dataVessel<-dataVessel[,c("x","y","speed","vesselid","date","time")]

#set adriatic boundaries
library(plyr)
bbx0=12.2
bbx1=19.6
bby0=40.2
bby1=46
bx2<-17.820077215541005
bx1<-10.934812459263048
by2<-39.42654592951941
by1<-43.393009920210915


m_bb<-(by2-by1)/(bx2-bx1)
q_bb<-((bx2*by1)-(bx1*by2))/(bx2-bx1)

dataVessel_bb<-subset(dataVessel, (x >=bbx0  & x <= bbx1 & y >= bby0 & y<=bby1))
dataVessel_bb<-dataVessel_bb[which(dataVessel_bb$y>((dataVessel_bb$x*m_bb)+q_bb)),]

rm(dataVessel)

cat("2. downsampling to resolution")
res<-0.01
cat("downsampling to:",res,"degrees\n")
dataVessel_bb$xcentroid<-round(dataVessel_bb$x,digits = 2)
dataVessel_bb$ycentroid<-round(dataVessel_bb$y,digits = 2)

cat("3. adding the bathymetry information for each X and Y \n")

fileBathy <- "depth_b2f62dfb-7b4b-428e-8601-4d1089308e14.nc"#"http://thredds.d4science.org/thredds/dodsC/public/netcdf/depth_b2f62dfb-7b4b-428e-8601-4d1089308e14.nc"
cat("\taccessing bathymetry remote file\n")
dat.multi<-suppressWarnings(brick(fileBathy))
cat("\tretrieving centroid columns\n")
pt <-dataVessel_bb[,(ncol(dataVessel_bb)-1):ncol(dataVessel_bb)] #take the last two columns

# visualize
#plot(dat.multi[[1]])
#points(pt)

cat("\textracting bathymetry values for points\n")
dataVessel_bb$depth<-raster::extract(dat.multi[[1]], pt)
rm(pt)
rm(dat.multi)

cat("4. classifying fishing activity by speed and bathymetry\n")

dataVessel_bb$fishing_activity<-"UNK"
dataVessel_bb$fishing_activity[which(dataVessel_bb$speed>2 & dataVessel_bb$speed<=4 & dataVessel_bb$depth>=500)]<-"Trawling"
dataVessel_bb$fishing_activity[which(dataVessel_bb$speed>2 & dataVessel_bb$speed<=4 & dataVessel_bb$depth<500)]<-"Midwater-Trawling"
dataVessel_bb$fishing_activity[which(dataVessel_bb$speed<=2)]<-"Hauling"
dataVessel_bb$fishing_activity[which(dataVessel_bb$speed>4)]<-"Steaming"

cat("5. selecting only trawling vessels\n")
fishing_vessels<-unique(
  dataVessel_bb$vesselid[ which( (dataVessel_bb$fishing_activity=="Trawling") | (dataVessel_bb$fishing_activity=="Midwater-Trawling") ) ]
)

dataVessel_bb_fishing_vessels<-dataVessel_bb[which(dataVessel_bb$vesselid %in% fishing_vessels),]

fishing_trajectories_per_vessel<-sqldf(paste0("select vesselid, count(fishing_activity) as c_act from dataVessel_bb_fishing_vessels where fishing_activity='Trawling' OR fishing_activity='Midwater-Trawling' group by vesselid"),drv="SQLite")
densfish<-density(fishing_trajectories_per_vessel$c_act)
densfish<-data.frame(x=densfish$x,y=densfish$y)
densfish<-densfish[which(densfish$x<1000),]

threshold_for_vessels<-round(densfish$x[which(densfish$y==max(densfish$y))]) #max of the low density of fishing vessels' fishing points
cat("Minimum number of fishing locations to include a vessel in the analysis:",threshold_for_vessels,"\n")
really_fishing_vessels<-fishing_trajectories_per_vessel[which(fishing_trajectories_per_vessel$c_act>threshold_for_vessels),]$vesselid 
cat("Number of fishing vessels to include:",length(really_fishing_vessels),"over",length(unique(dataVessel_bb$vesselid)),"(",length(really_fishing_vessels)*100/length(unique(dataVessel_bb$vesselid)),"%)\n")
dataVessel_bb_fishing_vessels<-dataVessel_bb_fishing_vessels[which(dataVessel_bb_fishing_vessels$vesselid %in% really_fishing_vessels),]

cat("6. saving\n")
save(dataVessel_bb_fishing_vessels, file = gsub(".csv","_classified.Rdata",inputTable))
