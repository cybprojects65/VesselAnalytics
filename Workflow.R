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
library(lubridate)
library(plyr)
library(dplyr)
library(MASS)
library(ncdf4)
library(robis)
library(zip)

status<-function(stat){
  fileConn<-file("status.txt")
  writeLines(paste0(stat), fileConn)
  close(fileConn)
}

#input
inputTable<-"Med-region-5min-Fishing-vessels-2019_01_prepared.csv"
xcolumn<-"x"
ycolumn<-"y"
speedcolumn<-"speed"
vesselidcolumn<-"vesselid"
datetimecolumn<-"datetime"
low_speed_prior<-2
high_speed_prior<-4

#FIXED PARAMETERS
delete_harbours<-T
external_defined_classification<-F
if (external_defined_classification){
  ul_ratio<-c(0.0693,0.504)
  tot_hours<-c(0.269,2.552)
  u_hours<-c(0.100,0.447)
  r_hours<-c(0.264,2.486)
}
min_minutes_gap<-31
max_minutes_gap<-220
too_close_to_harbour<-1.2 #in NM
outputFolder<-"vessel_analysis_output"
outputZip<-"vessel_analysis.zip"

if (!dir.exists(outputFolder))
  dir.create(outputFolder)
outputTable<-paste0(outputFolder,"/",basename(inputTable))

#input data preprocessing
cat("###DATA PREPROCESSING\n")
cat("Reading file (could take some minutes) \n")
dataVessel<-read.csv(inputTable,header=T,sep=",")

status(5)

cat("\tExtracting required columns\n")
names(dataVessel)[names(dataVessel) == xcolumn] <- "x"
names(dataVessel)[names(dataVessel) == ycolumn] <- "y"
names(dataVessel)[names(dataVessel) == speedcolumn] <- "speed"
names(dataVessel)[names(dataVessel) == vesselidcolumn] <- "vesselid"
names(dataVessel)[names(dataVessel) == datetimecolumn] <- "datetime"
dataVessel<-dataVessel[,c("x","y","speed","vesselid","datetime")]

low_speed_prior_tolerance<-(low_speed_prior)-(0.5*low_speed_prior)
high_speed_prior_tolerance<-(high_speed_prior)+(0.5*high_speed_prior)


cat("Downsampling to 0.01 deg")
res<-0.01
res_heatmap<-0.1
cat("Preparing downsampling to:",res,"degrees\n")
dataVessel$xcentroid<-round(dataVessel$x,digits = 2)
dataVessel$ycentroid<-round(dataVessel$y,digits = 2)

cat("Adding bathymetry information for each X and Y \n")

fileBathy <- "depth_b2f62dfb-7b4b-428e-8601-4d1089308e14.nc"# from"http://thredds.d4science.org/thredds/dodsC/public/netcdf/depth_b2f62dfb-7b4b-428e-8601-4d1089308e14.nc"
cat("\taccessing bathymetry file\n")
dat.multi<-suppressWarnings(brick(fileBathy))
cat("\tretrieving centroid columns\n")
pt <-dataVessel[,(ncol(dataVessel)-1):ncol(dataVessel)] #take the last two columns

cat("\tExtracting bathymetry values for points\n")
dataVessel$depth<-raster::extract(dat.multi[[1]], pt)
rm(pt)
rm(dat.multi)
cat("\tRemoving points on land\n")
if ( length(is.na(dataVessel$depth))>0)
  dataVessel<-dataVessel[-which(is.na(dataVessel$depth)),]
cat("\tRemoving non realistic speed\n")
dataVessel<-dataVessel[which(dataVessel$speed<20),]
cat("\tRemoving duplicates\n")
dataVessel<-distinct(dataVessel)

dataVessel$fishing_activity<-"UNK"
dataVessel$fishing_activity[which(dataVessel$speed>=low_speed_prior & dataVessel$speed<=high_speed_prior)]<-"Fishing"
dataVessel$fishing_activity[which(dataVessel$speed<low_speed_prior)]<-"Hauling"
dataVessel$fishing_activity[which(dataVessel$speed>high_speed_prior)]<-"Steaming"

fishing_vessels<-unique(
  dataVessel$vesselid[ which( (dataVessel$fishing_activity=="Fishing") ) ]
)

cat("\tRemoving never-fishing vessels based on prior speed range\n")
dataVessel<-dataVessel[which(dataVessel$vesselid %in% fishing_vessels),]

cat("\tRemoving few track-fishing vessels based on prior speed range\n")
fishing_trajectories_per_vessel_total<-sqldf(paste0("select vesselid, count(fishing_activity) as c_act from dataVessel where fishing_activity='Fishing' group by vesselid"),drv="SQLite")
fishing_trajectories_per_vessel<-fishing_trajectories_per_vessel_total[which(fishing_trajectories_per_vessel_total$c_act<1000),]
trawls<-fishing_trajectories_per_vessel$c_act[which(fishing_trajectories_per_vessel$c_act>0)]
lognormal_limit_tracks<-exp(mean(log(trawls)))-0.2*exp(mean(log(trawls)))
lognormal_limit_tracks<-max(1,lognormal_limit_tracks)
intensively_fishing_vessels<-fishing_trajectories_per_vessel_total[which(fishing_trajectories_per_vessel_total$c_act>lognormal_limit_tracks),]$vesselid 
cat("Number of fishing vessels to include:",length(intensively_fishing_vessels),"over",length(unique(dataVessel$vesselid)),"(",length(intensively_fishing_vessels)*100/length(unique(dataVessel$vesselid)),"%)\n")
dataVessel<-dataVessel[which(dataVessel$vesselid %in% intensively_fishing_vessels),]


if (delete_harbours){
cat("\tRemoving points too close to harbours\n")
#harbors and ports taken from http://data.tools4msp.eu/layers/geonode:ports_harbor and exported into XY points with QGIS
ports<-read.csv("cleaned_ports_QGIS.csv")
#out-harbor selection
xycentroids_comp<-data.frame(dataVessel$xcentroid, dataVessel$ycentroid)
xycentroids_comp$xy<-paste(xycentroids_comp$dataVessel.xcentroid,xycentroids_comp$dataVessel.ycentroid,sep=";")
xycentroids<-distinct(xycentroids_comp)
xycentroidstring<-unique(xycentroids_comp$xy)

in_port<-sapply(1:dim(xycentroids)[1], function(i){
  
  row<-xycentroids[i,]
  dx<-row$dataVessel.xcentroid-ports$X
  dy<-row$dataVessel.ycentroid-ports$Y
  
  d<-sqrt((dx*dx)+(dy*dy))
  
  miles<-min(d)*111.1/1.852
  if (miles<too_close_to_harbour)
    return(T)
  else
    return(F)
},simplify = T)

if (length(which(in_port))>0){
  xycentroids_ports<-xycentroids[which(in_port),]
  idx_ports<-which(xycentroids_comp$xy %in% xycentroids_ports$xy)
  if (length(idx_ports)>0)
    dataVessel<-dataVessel[-idx_ports,]
}
}#end if delete harbours

cat("Saving cleaned file\n")
write.csv(dataVessel,file = gsub(".csv","_cleaned_and_preclassified.csv",outputTable),row.names = F)
status(10)
#vessel activity classification
cat("###Classifying fishing activity by prior speed range\n")
cat("Revising prior speed range\n")
speeddistr<-dataVessel$speed
speeddistr<-speeddistr[which(speeddistr<high_speed_prior_tolerance & speeddistr>low_speed_prior_tolerance)]
fit_params <- fitdistr(speeddistr,"logistic")
sigma<-as.numeric(fit_params$estimate['scale']*pi/sqrt(3))
upper_speed_thr = (fit_params$estimate['location']+1.96*sigma)
lower_speed_thr = (fit_params$estimate['location']-1.96*sigma)
upper_speed_thr = max(high_speed_prior_tolerance,upper_speed_thr)
upper_speed_thr = min(upper_speed_thr,high_speed_prior_tolerance)
lower_speed_thr = min(lower_speed_thr,low_speed_prior_tolerance)
lower_speed_thr = max(low_speed_prior_tolerance,lower_speed_thr)
cat("\tNew vessel fishing speed boundaries :",lower_speed_thr,"<speed_logistic<",upper_speed_thr,"\n")

dataVessel$fishing_activity<-"UNK"
dataVessel$fishing_activity[which(dataVessel$speed>=lower_speed_thr & dataVessel$speed<=upper_speed_thr)]<-"Fishing"
dataVessel$fishing_activity[which(dataVessel$speed<lower_speed_thr)]<-"Hauling"
dataVessel$fishing_activity[which(dataVessel$speed>upper_speed_thr)]<-"Steaming"

#gap filling
cat("Gap classification - identifying gaps to rebuild\n")
fishing_vessels<-unique(
  dataVessel$vesselid[ which( (dataVessel$fishing_activity=="Fishing") ) ]
)
fishing_vessels<-unique(dataVessel$vesselid)

vessel_list<-list()
counter<-1
sampling_period<-Inf
for (vid in fishing_vessels){
  #prepare time column
  dataVessel_vid<-dataVessel[dataVessel$vesselid==vid,]
  transformedDate<-as.POSIXlt(dataVessel_vid$datetime,tryFormats = c("%m/%d/%Y %I:%M:%S %p","%m/%d/%Y",
                                                                       "%Y-%m-%dT%H:%M:%SZ",
                                                                       "%Y-%m-%d %H:%M:%OS",
                                                                       "%Y/%m/%d %H:%M:%OS",
                                                                       "%Y-%m-%d %H:%M",
                                                                       "%Y/%m/%d %H:%M",
                                                                       "%Y-%m-%dT%H:%M:%OS",
                                                                       "%Y/%m/%dT%H:%OS",
                                                                       "%Y-%m-%dT%H:%M",
                                                                       "%Y/%m/%dT%H:%M",
                                                                       "%Y-%m-%d",
                                                                       "%Y/%m/%d"), tz="GMT")
  transformedDatef<-format(transformedDate,"%m/%d/%Y %H:%M:%S")
  dataVessel_vid$datetimeposix<- as.POSIXlt(transformedDatef,"%m/%d/%Y %H:%M:%S",tz="GMT")
  dataVessel_vid<-dataVessel_vid[order(dataVessel_vid$datetimeposix),]
  rm (transformedDatef)
  #time column difference
  time2<-dataVessel_vid$datetimeposix
  time2[2:length(time2)]<-dataVessel_vid$datetimeposix[1:dim(dataVessel_vid)[1]-1]
  dataVessel_vid$timediff_min<-as.numeric(difftime(dataVessel_vid$datetimeposix, time2, "GMT", units = c("mins")))
  min_time_diff<-mean(dataVessel_vid$timediff_min[which(dataVessel_vid$timediff_min<min_minutes_gap & dataVessel_vid$timediff_min>0)])
  if (!is.nan(min_time_diff) && (min_time_diff<sampling_period) )
    sampling_period<-min_time_diff

    #point classification
  dataVessel_vid$point_gap<-"unknown"
  dataVessel_vid$point_gap[which(dataVessel_vid$timediff_min>=max_minutes_gap)]<-"new_track_start"
  dataVessel_vid$point_gap[which(dataVessel_vid$timediff_min>=min_minutes_gap & dataVessel_vid$timediff_min<max_minutes_gap)]<-"gap"
  dataVessel_vid$point_gap[which(dataVessel_vid$timediff_min<min_minutes_gap)]<-"clear"
  dataVessel_vid$point_gap[which(dataVessel_vid$timediff_min==0)]<-"track_start"
  dataVessel_vid$datetime<-NULL
  dataVessel_vid$date<-NULL
  dataVessel_vid$time<-NULL
  cat("\tOrdered ",counter,"vessels of",length(fishing_vessels)," :",round(counter*100/length(fishing_vessels)),"% \n" )
  vessel_list[[counter]]<-dataVessel_vid
  counter<-counter+1
}#end loop on vessels

sampling_period<-round(sampling_period)
cat("\tDetected sampling period",sampling_period,"min \n")

cat("###Gap rebuilding\n")
cat("Rebuilding gaps and speeds\n")

vessel_reconstructed<-sapply(1:length(fishing_vessels), function(counter){
  vid<- fishing_vessels[counter]
  
  dataVesselNewOrd<-vessel_list[[counter]]
  gap_indices<-which(dataVesselNewOrd$point_gap=="gap")
  
  if (length(gap_indices)>0){
    gap_indices_prev<-gap_indices-1
    
    timegap_minutes<-dataVesselNewOrd$timediff_min[gap_indices]
    time_minutes<-dataVesselNewOrd$datetimeposix[gap_indices_prev]
    timegap_minutes_prev<-dataVesselNewOrd$timediff_min[gap_indices_prev]
    x_prev<-dataVesselNewOrd$x[gap_indices_prev]
    y_prev<-dataVesselNewOrd$y[gap_indices_prev]
    x_curr<-dataVesselNewOrd$x[gap_indices]
    y_curr<-dataVesselNewOrd$y[gap_indices]
    distancex<-dataVesselNewOrd$x[gap_indices]-dataVesselNewOrd$x[gap_indices_prev]
    distancey<-dataVesselNewOrd$y[gap_indices]-dataVesselNewOrd$y[gap_indices_prev]
    #calc avg speed
    speed<-sqrt((distancex*distancex)+(distancey*distancey))/timegap_minutes #deg/min
    speed<-speed*111.1*60/1.852
    #calc course
    n_time_steps<-round(timegap_minutes/sampling_period)
    distance_stepx<-distancex/n_time_steps
    distance_stepy<-distancey/n_time_steps
    m<-(distancey)/(distancex)
    #fill gaps
    list_of_gap_filled<-sapply(1:length(timegap_minutes), function(idx){
      if (is.nan(m[idx])){
        return(NA)
      } else if (is.infinite(m[idx])){
        distance_stepx[idx]<-0
      } else if (m[idx] == 0){
        distance_stepy[idx]<-0
      }
      activity<-"Other"
      if (speed[idx]>lower_speed_thr & speed[idx]<=upper_speed_thr){
        activity<-"Fishing"
      }
      
      time_indices<-c(1:(n_time_steps[idx]-1))
      additionsx<-time_indices*distance_stepx[idx]
      additionsy<-time_indices*distance_stepy[idx]
      distancex_add<-x_prev[idx]+additionsx
      distancey_add<-y_prev[idx]+additionsy
      times<-time_minutes[idx]+(time_indices*minutes(sampling_period))
      time_gaps<-(time_indices*sampling_period)
      
      temp_df<-data.frame(x=distancex_add,
                          y=distancey_add,
                          speed=speed[idx],
                          vesselid=vid,
                          xcentroid=round(distancex_add,2),
                          ycentroid=round(distancey_add,2),
                          depth=NA,fishing_activity=activity,
                          datetimeposix=times,
                          timediff_min=time_gaps,
                          point_gap="gap_rebuilt")
      
      return (temp_df)
    }, simplify = F)
    
    list_of_gap_filled[[length(list_of_gap_filled)+1]]<-dataVesselNewOrd
    list_of_gap_filled[which(is.na(list_of_gap_filled))]<-NULL
    dataVesselNewOrd <- ldply(list_of_gap_filled, data.frame)
    #reorder
    dataVesselNewOrd<-dataVesselNewOrd[order(dataVesselNewOrd$datetimeposix),]
    
  }#end if gaps are available
  
  timeseq<-dataVesselNewOrd$datetimeposix
  timeseq[2:length(timeseq)]<-dataVesselNewOrd$datetimeposix[1:dim(dataVesselNewOrd)[1]-1]
  dataVesselNewOrd$timediff_hours_filled<-as.numeric(difftime(dataVesselNewOrd$datetimeposix, timeseq, "GMT", units = c("hours")))
  cat("\tFilled ",counter,"vessels of",length(fishing_vessels)," :",round(counter*100/length(fishing_vessels)),"% \n" )
  return(dataVesselNewOrd)
}, simplify = F)

dataVessel<-ldply(vessel_reconstructed, data.frame)

cat("Saving gap filled and classified files\n")
write.csv(dataVessel,file = gsub(".csv","_gap_filled.csv",outputTable),row.names = F)
status(50)
#data aggregation
cat("###Data aggregation\n")
cat("Extracting unreported, reported, and total fishing points\n")
unreported_fishing_points<-dataVessel[which(dataVessel$point_gap=="gap_rebuilt" & dataVessel$fishing_activity=="Fishing"),]
all_fishing_points<-dataVessel[which(dataVessel$fishing_activity=="Fishing"),]
reported_fishing_points<-dataVessel[which(dataVessel$point_gap!="gap_rebuilt" & 
                                            dataVessel$fishing_activity=="Fishing"),]

#aggregate by summing total time differences
cat("Aggregating at 0.01 deg\n")
unreported_cells<-sqldf(paste0("select xcentroid, ycentroid, sum(timediff_hours_filled) as total_hours from unreported_fishing_points group by xcentroid,ycentroid"),drv="SQLite")
all_fishing_cells<-sqldf(paste0("select xcentroid, ycentroid, sum(timediff_hours_filled) as total_hours from all_fishing_points group by xcentroid,ycentroid"),drv="SQLite")
reported_cells<-sqldf(paste0("select xcentroid, ycentroid, sum(timediff_hours_filled) as total_hours from reported_fishing_points group by xcentroid,ycentroid"),drv="SQLite")

cat("Calculating ratios\n")
ratio_all_fishing_cells<-all_fishing_cells
ratio_all_fishing_cells$xy<-paste(ratio_all_fishing_cells$xcentroid,ratio_all_fishing_cells$ycentroid,sep=";")
xy<-ratio_all_fishing_cells$xy
unreported_cells$xy<-paste(unreported_cells$xcentroid,unreported_cells$ycentroid,sep=";")
ratios<-sapply(1:length(xy), function(idx){
  xyc<-xy[idx]
  unrep_idx<-which(unreported_cells$xy==xyc)
  th<-all_fishing_cells[idx,]$total_hours
  #if the cell exists in the unreported dataset do the ratio
  if (length(unrep_idx)>0 && th>0){
    ur<-unreported_cells[unrep_idx,]$total_hours
    ratio<-ur/th
  }else{
    ratio<-0#1/ratio_all_fishing_cells[idx,]$total_hours
  }
  return (ratio)
},simplify = T)
#update the total hours with ratios, delete xy column, and change the name
ratio_all_fishing_cells$total_hours<-ratios
ratio_all_fishing_cells$xy<-NULL
names(ratio_all_fishing_cells)<-c("xcentroid","ycentroid","ratio_unreported_total_hours")
#remove infinite values
infinite_cells<-which(is.infinite(ratio_all_fishing_cells$ratio_unreported_total_hours))
if (length(infinite_cells)>0){
  ratio_all_fishing_cells<-ratio_all_fishing_cells[-which(is.infinite(ratio_all_fishing_cells$ratio_unreported_total_hours)),]
}

unreported_cells<-unreported_cells[,-which(names(unreported_cells) == "xy")]

cat("Calculating classifying aggregations\n")
#classification of the ranges:
calc_intensity_ranges<-function(hours){
  hours<-hours[which(hours>0)]
  mean.log  <- mean(log(hours))
  sd.log    <- sd(log(hours))
  gm<-exp(mean.log)
  lower_thr<-exp(mean.log-1*sd.log)
  upper_thr<-exp(mean.log+1*sd.log)
  return (c(lower_thr,upper_thr))
}


if (!external_defined_classification){
  ul_ratio<-calc_intensity_ranges(ratio_all_fishing_cells$ratio_unreported_total_hours)
}

ratio_all_fishing_cells$intensity<-"medium"
ratio_all_fishing_cells$intensity[which(ratio_all_fishing_cells$ratio_unreported_total_hours<ul_ratio[1])]<-"low"
ratio_all_fishing_cells$intensity[which(ratio_all_fishing_cells$ratio_unreported_total_hours>ul_ratio[2])]<-"high"
ratio_all_fishing_cells$intensity_int<-1
ratio_all_fishing_cells$intensity_int[which(ratio_all_fishing_cells$ratio_unreported_total_hours<ul_ratio[1])]<-0
ratio_all_fishing_cells$intensity_int[which(ratio_all_fishing_cells$ratio_unreported_total_hours>ul_ratio[2])]<-2
cat("\t",ul_ratio[1],"<ratio<",ul_ratio[2],"\n")

if (!external_defined_classification){
  tot_hours<-calc_intensity_ranges(all_fishing_cells$total_hours)
}

all_fishing_cells$intensity<-"medium"
all_fishing_cells$intensity[which(all_fishing_cells$total_hours<tot_hours[1])]<-"low"
all_fishing_cells$intensity[which(all_fishing_cells$total_hours>tot_hours[2])]<-"high"
all_fishing_cells$intensity_int<-1
all_fishing_cells$intensity_int[which(all_fishing_cells$total_hours<tot_hours[1])]<-0
all_fishing_cells$intensity_int[which(all_fishing_cells$total_hours>tot_hours[2])]<-2
cat("\t",tot_hours[1],"<total hours<",tot_hours[2],"\n")

if (!external_defined_classification){
  u_hours<-calc_intensity_ranges(unreported_cells$total_hours)
}

unreported_cells$intensity<-"medium"
unreported_cells$intensity[which(unreported_cells$total_hours<u_hours[1])]<-"low"
unreported_cells$intensity[which(unreported_cells$total_hours>u_hours[2])]<-"high"
unreported_cells$intensity_int<-1
unreported_cells$intensity_int[which(unreported_cells$total_hours<u_hours[1])]<-0
unreported_cells$intensity_int[which(unreported_cells$total_hours>u_hours[2])]<-2
cat("\t",u_hours[1],"<unreported hours<",u_hours[2],"\n")

if (!external_defined_classification){
  r_hours<-calc_intensity_ranges(reported_cells$total_hours)
}

reported_cells$intensity<-"medium"
reported_cells$intensity[which(reported_cells$total_hours<r_hours[1])]<-"low"
reported_cells$intensity[which(reported_cells$total_hours>r_hours[2])]<-"high"
reported_cells$intensity_int<-1
reported_cells$intensity_int[which(reported_cells$total_hours<r_hours[1])]<-0
reported_cells$intensity_int[which(reported_cells$total_hours>r_hours[2])]<-2
cat("\t",r_hours[1],"<reported hours<",r_hours[2],"\n")


cat("Saving aggregations\n")
write.csv(ratio_all_fishing_cells,file = gsub(".csv","_ratio_cells.csv",outputTable),row.names = F)
write.csv(unreported_cells,file = gsub(".csv","_unreported_fishing_cells.csv",outputTable),row.names = F)
write.csv(reported_cells,file = gsub(".csv","_reported_fishing_cells.csv",outputTable),row.names = F)
write.csv(all_fishing_cells,file = gsub(".csv","_total_fishing_cells.csv",outputTable),row.names = F)
status(70)
#heatmap production
cat("###Hotspot production at",res_heatmap,"deg resolution\n")
cat("Selecting highly unreported locations\n")
unreported_cells<-unreported_cells[,c("xcentroid","ycentroid","total_hours")]
names(unreported_cells) <- c("x","y","totalh")
unreportedh<-unreported_cells[which(unreported_cells$totalh>u_hours[2]),]

nx<-round(((max(unreportedh$x)-min(unreportedh$x))+1)/res_heatmap)
ny<-round(((max(unreportedh$y)-min(unreportedh$y)+1))/res_heatmap)
cat("Producing hotspots through kde2d\n")
k = kde2d(
  unreportedh[,1],
  unreportedh[,2], 
  h=c(width.SJ(unreportedh$x), width.SJ(unreportedh$y) ), 
  n=c(nx,ny), 
  lims = c(c(min(unreportedh$x), max(unreportedh$x)), c(min(unreportedh$y), max(unreportedh$y))))

#print(max(k$z))
r = raster(k)
#densr<-density(r)
#min_r_value<-as.numeric(quantile(densr$x)[2])
#r[r < min_r_value] <- NA
#plot(r)
writeRaster(r,filename = gsub(".csv","_unreported_fishing_hotspots.tiff",outputTable),overwrite=T)

#linearly separate and classify hotspots
cat("Producing categorised hotspots\n")
subdivision_size<-(max(k$z)-min(k$z))/4
r_categorised<-r
r_categorised[r_categorised < subdivision_size]<-NA
r_categorised[ (
  (r_categorised>=subdivision_size) & 
    (r_categorised < (2*subdivision_size)))
     ] <- 0
r_categorised[ (
  (r_categorised>=(2*subdivision_size)) & 
    (r_categorised < (3*subdivision_size)))
  ] <- 1
r_categorised[ (
  (r_categorised>=(3*subdivision_size)) )
] <- 2
#plot(r_categorised)

cat("Saving unreported activity hotspots\n")
writeRaster(r_categorised,filename = gsub(".csv","_unreported_fishing_hotspots_categorised.tiff",outputTable),overwrite=T)
status(80)
##Stock and ETP species retrieval

cat("###Stock and ETP extraction\n")
min_x_in_raster<-r@extent[1]
max_x_in_raster<-r@extent[2]
min_y_in_raster<-r@extent[3]
max_y_in_raster<-r@extent[4]

cat("\tTaking the highest hotspots\n")
resolution = res(r)[1]
boundingbox = paste0("POLYGON ((",
                     min_x_in_raster," ",min_y_in_raster,", ",
                     min_x_in_raster," ",max_y_in_raster,", ",
                     max_x_in_raster," ",max_y_in_raster,", ",
                     max_x_in_raster," ",min_y_in_raster,", ",
                     min_x_in_raster," ",min_y_in_raster,
                     "))")

#build an x,y,value grid at the given resolution
xseq<-seq(from=min_x_in_raster,to=max_x_in_raster,by=resolution)
yseq<-seq(from=min_y_in_raster,to=max_y_in_raster,by=resolution)
grid_of_points<-expand.grid(x = xseq, y = yseq)#combine the x and y coordinate to generate pairs
grid_values<-raster::extract(x=r,y=grid_of_points,method='simple')
grid_of_points$values<-grid_values
if (length(which(is.na(grid_of_points$values)))>0)
  grid_of_points<-grid_of_points[-which(is.na(grid_of_points$values)),]
#taking high hotspot points
grid_of_points_high_values<-grid_of_points[-which(grid_of_points$values<(3*subdivision_size) ),]
cat("\tReading the GRSF\n")
#select the stocks with a geometry
FAOlist<-read.delim("ASFIS_sp_2022_geom.txt",header=T,sep=",",encoding="UTF-8")
FAOlist<-FAOlist[,c("TAXOCODE","Scientific_name","geometry")]
names(FAOlist)[names(FAOlist) == "Scientific_name"] <- "scientificName"
names(FAOlist)[names(FAOlist) == "geometry"] <- "geometries"
FAOlist<-FAOlist[-which(nchar(as.character(FAOlist$geometries))==0),]
#cast geometries to ST objects
FAOlist_SF<-st_as_sf(FAOlist,wkt="geometries")
FAOlist_SF %>% st_cast()
fao_geometries<-FAOlist_SF$geometries

cat("\tExtracting stocks from the GRSF\n")
grid_of_points_high_values$points_wkt<-paste0("POINT(",grid_of_points_high_values$x," ",grid_of_points_high_values$y,")")
point_geometries<-st_as_sfc(grid_of_points_high_values$points_wkt)
#select the stocks with a geometry

stocks<-c()
intersections<-c()
geomcounter<-1
#for each geometry, intersect it with the grid points
for (sp_geom in fao_geometries){
  
  intersection<-st_as_text(st_intersection(st_make_valid(sp_geom),point_geometries))
  #if the point falls in the geometry, annotate the stock as present in the fishing area
  if (nchar(as.character(intersection))>0 && !grepl("EMPTY",intersection)){
    stocks<-c(stocks, as.character(FAOlist_SF$scientificName[geomcounter])) 
    intersections<-c(intersections,intersection)
  }
  geomcounter<-geomcounter+1  
}

status(85)
cat("\tIntersecting with OBIS\n")
#get the really observed species in the bounding box
speciesinbb="speciesobis.Rdata"
cat("\tReading OBIS\n")
if (!file.exists(speciesinbb)){
  speciesOBIS<-occurrence(geometry=boundingbox) 
  #IUCN<-checklist(geometry=boundingbox,redlist=TRUE)
  save(list = c("speciesOBIS"),file=speciesinbb)
}else{
  load(file = speciesinbb)
}

cat("\tSelecting the intersection of the specie between the two datasources\n")
#select the marine species in obis
speciesOBIS=speciesOBIS[speciesOBIS$marine=="TRUE",]
#select the fao stocks present and observed in the area
stocks_observed<-sort(stocks[which(stocks %in% unique(speciesOBIS$scientificName))])

cat("\tChecking the ETP status\n")
#select the fao stocks that are present and observed in the area and are in the IUCN red list
threatened_species<-unique(speciesOBIS$scientificName[which( (speciesOBIS$category == 'VU') | 
                                                               (speciesOBIS$category == 'EN') | 
                                                               (speciesOBIS$category == 'CR')
)])

stocks_redlist<-sort(stocks[which(stocks %in% threatened_species)])
stocks_observed_df<-data.frame(scientific_names=stocks_observed)
stocks_observed_df$is_threatened<-F
stocks_observed_df$is_threatened[which(stocks_observed %in% stocks_redlist)]<-T

cat("Saving stock and ETP species information\n")
write.csv(stocks_observed_df,file = gsub(".csv","_stocks_and_ETP_status.csv",outputTable),row.names = F)

status(90)
cat("Zipping output to",outputZip,"\n")
files2zip <- dir(outputFolder, full.names = TRUE)
zip(zipfile = outputZip, files = files2zip)

cat("All done.\n")

