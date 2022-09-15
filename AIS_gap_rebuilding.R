### estimating fishing activity ###
rm(list=ls(all=TRUE))
library(lubridate)
library(raster)
library(dplyr)
library(MASS)

t0<-Sys.time()

inputTable<-"Med-region-5min-Fishing-vessels-2019_01.csv"
load(file = gsub(".csv","_classified.Rdata",inputTable))

cat("prefiltering\n")
#prefiltering
dataVessel_bb_fishing_vessels<-dataVessel_bb_fishing_vessels[which(dataVessel_bb_fishing_vessels$speed<20),]
dataVessel_bb_fishing_vessels<-distinct(dataVessel_bb_fishing_vessels)
dataVessel_bb_fishing_vessels<-dataVessel_bb_fishing_vessels[-which(is.na(dataVessel_bb_fishing_vessels$depth)),]

#fit speed distribution to a logit
speeddistr<-dataVessel_bb_fishing_vessels$speed
speeddistr<-speeddistr[which(speeddistr<5 & speeddistr>1)]
fit_params <- fitdistr(speeddistr,"logistic")
#fit <- dlogis(x, location=fit_params3$estimate['location'], scale=fit_params3$estimate['scale'])
sigma<-as.numeric(fit_params$estimate['scale']*pi/sqrt(3))
upper_speed_thr = (fit_params$estimate['location']+1.96*sigma)
lower_speed_thr = (fit_params$estimate['location']-1.96*sigma)
cat("vessel fishing speed boundaries :",lower_speed_thr,"<speed_logit<",upper_speed_thr,"\n")
upper_speed_thr = max(4,upper_speed_thr)
upper_speed_thr = min(upper_speed_thr,5)
lower_speed_thr = min(2,lower_speed_thr)
lower_speed_thr = max(1,lower_speed_thr)

#ordering
fishing_vessels<-unique(dataVessel_bb_fishing_vessels$vesselid)

cat("1. adjusting time\n")
dataVessel_bb_fishing_vessels_gap<-NA

counter<-1
#pre-classification
vessel_list<-list()
for (vid in fishing_vessels){
  #prepare time column
  dataVesselNewOrd<-dataVessel_bb_fishing_vessels[dataVessel_bb_fishing_vessels$vesselid==vid,]
  dataVesselNewOrd$datetime<-as.character(paste(dataVesselNewOrd$date,dataVesselNewOrd$time))
  transformedDate<-as.POSIXlt(dataVesselNewOrd$datetime,tryFormats = c("%m/%d/%Y %I:%M:%S %p","%m/%d/%Y",
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
  dataVesselNewOrd$datetimeposix<- as.POSIXlt(transformedDatef,"%m/%d/%Y %H:%M:%S",tz="GMT")
  dataVesselNewOrd<-dataVesselNewOrd[order(dataVesselNewOrd$datetimeposix),]
  rm (transformedDatef)
  #time column difference
  time2<-dataVesselNewOrd$datetimeposix
  time2[2:length(time2)]<-dataVesselNewOrd$datetimeposix[1:dim(dataVesselNewOrd)[1]-1]
  dataVesselNewOrd$timediff_min<-as.numeric(difftime(dataVesselNewOrd$datetimeposix, time2, "GMT", units = c("mins")))
  #point classification
  dataVesselNewOrd$point_gap<-"unknown"
  dataVesselNewOrd$point_gap[which(dataVesselNewOrd$timediff_min>=1440)]<-"new_track_start" #after 24h
  dataVesselNewOrd$point_gap[which(dataVesselNewOrd$timediff_min>=31 & dataVesselNewOrd$timediff_min<1440)]<-"gap" #after 30 min
  dataVesselNewOrd$point_gap[which(dataVesselNewOrd$timediff_min<31)]<-"clear" #after 30 min
  dataVesselNewOrd$point_gap[which(dataVesselNewOrd$timediff_min==0)]<-"track_start" #after 30 min
  dataVesselNewOrd$datetime<-NULL
  dataVesselNewOrd$date<-NULL
  dataVesselNewOrd$time<-NULL
  #data frame accumulation
  #if (counter == 1)
  # dataVessel_bb_fishing_vessels_gap<-dataVesselNewOrd
  #else
  # dataVessel_bb_fishing_vessels_gap<-rbind(dataVessel_bb_fishing_vessels_gap,dataVesselNewOrd)
  
  cat("Processed ",counter,"vessels of",length(fishing_vessels)," :",round(counter*100/length(fishing_vessels)),"% \n" )
  
  vessel_list[[counter]]<-dataVesselNewOrd
  
  counter<-counter+1
}#end loop on vessels

#reconstruction
t01<-Sys.time()
cat("Reconstructing routes\n")
dataVessel_reconstructed<-NA
test<-NA

#harbors and ports taken from http://data.tools4msp.eu/layers/geonode:ports_harbor and exported into XY points with QGIS
ports<-read.csv("ports_harbor_punctual.csv")

#vessel_reconstructed<-sapply(1:length(fishing_vessels), function(counter){
vessel_reconstructed<-sapply(1:1, function(counter){  
  vid<- fishing_vessels[counter]
  
  dataVesselNewOrd<-vessel_list[[counter]] #dataVessel_bb_fishing_vessels_gap[dataVessel_bb_fishing_vessels_gap$vesselid==vid,]
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
    speedx<-distancex/timegap_minutes #deg/min
    speedy<-distancey/timegap_minutes #deg/min
    speedx<-speedx*111.1*60/1.852 #kn
    speedy<-speedx*111.1*60/1.852 #kn
    speed<-sqrt((distancex*distancex)+(distancey*distancey))/timegap_minutes #deg/min
    speed<-speed*111.1*60/1.852
    distance_stepx<-distancex*5/timegap_minutes
    distance_stepy<-distancey*5/timegap_minutes
    m<-(distancey)/(distancex)
    n_time_steps<-round(timegap_minutes/5)
    
    list_of_gap_filled<-sapply(1:length(timegap_minutes), function(idx){
      if (is.nan(m[idx])){
        #cat("NULL presence in ",idx,"\n")
        return(NA)
        #distance_stepx[idx]<-0
        #distance_stepy[idx]<-0
      } else if (is.infinite(m[idx])){
        distance_stepx[idx]<-0
        #distance_stepy<-distance_step
      } else if (m[idx] == 0){
        distance_stepy[idx]<-0
        #distance_stepy<-distance_step
      }
      time_indices<-c(1:(n_time_steps[idx]-1))
      additionsx<-time_indices*distance_stepx[idx]
      additionsy<-time_indices*distance_stepy[idx]
      distancex_add<-x_prev[idx]+additionsx
      distancey_add<-y_prev[idx]+additionsy
      times<-time_minutes[idx]+(time_indices*minutes(5))
      time_gaps<-(time_indices*5)
      activity<-"Other"
      if (speed[idx]>lower_speed_thr & speed[idx]<=upper_speed_thr){
        activity<-"Fishing"
        #cat("Unreported fishing activity present in dataset",counter,"\n")
      }
      temp_df<-data.frame(x=distancex_add,y=distancey_add,speed=speed[idx],
                          vesselid=vid,
                          xcentroid=round(distancex_add,2),ycentroid=round(distancey_add,2),
                          #xcentroid=NA,ycentroid=NA,
                          depth=NA,fishing_activity=activity,
                          datetimeposix=times,
                          timediff_min=time_gaps,
                          point_gap="gap_rebuilt")
      
      return (temp_df)
    }, simplify = F)
    
    list_of_gap_filled[[length(list_of_gap_filled)+1]]<-dataVesselNewOrd
    list_of_gap_filled[which(is.na(list_of_gap_filled))]<-NULL
    #testlist<<-list_of_gap_filled
    #list_of_gap_filled<-list_of_gap_filled[-which(sapply(list_of_gap_filled, is.null))]
    dataVesselNewOrd <- ldply(list_of_gap_filled, data.frame)
    #reorder
    dataVesselNewOrd<-dataVesselNewOrd[order(dataVesselNewOrd$datetimeposix),]
    test<<-dataVesselNewOrd
  }#end if gaps are available
  cat("Processed ",counter,"vessels of",length(fishing_vessels)," :",round(counter*100/length(fishing_vessels)),"% \n" )
  
  return(dataVesselNewOrd)
  #break
  #counter=counter+1
  
}, simplify = F)

dataVessel_reconstructed<-ldply(vessel_reconstructed, data.frame)
dataVessel_reconstructed$potentiallyfishing<-NA
speeddistr<-dataVessel_reconstructed$speed
speeddistridx<-which(speeddistr>lower_speed_thr & speeddistr<upper_speed_thr)
dataVessel_reconstructed$potentiallyfishing[speeddistridx]<-"Y"
dataVessel_reconstructed$potentiallyfishing[-speeddistridx]<-"N"

t11<-Sys.time()

cat("Ordering Finished in\n")
print(t01-t0)

cat("Gap filling Finished in\n")
print(t11-t01)

cat("Gap filling Finished in\n")
print(t11-t01)

#out-harbor selection
xycentroids_comp<-data.frame(dataVessel_reconstructed$xcentroid, dataVessel_reconstructed$ycentroid)
xycentroids<-distinct(xycentroids_comp)

in_port<-sapply(1:dim(xycentroids)[1], function(i){
  
  row<-xycentroids[i,]
  dx<-row$dataVessel_reconstructed.xcentroid-ports$X
  dy<-row$dataVessel_reconstructed.ycentroid-ports$Y
  
  d<-sqrt((dx*dx)+(dy*dy))
  
  miles<-min(d)*111.1/1.852
  if (miles<1.5)
    return(T)
  else
    return(F)
},simplify = T)

xycentroids_ports<-xycentroids[which(in_port),]
idx_ports<-which( (xycentroids_comp$dataVessel_reconstructed.xcentroid %in% xycentroids_ports$dataVessel_reconstructed.xcentroid) & 
                    (xycentroids_comp$dataVessel_reconstructed.ycentroid %in% xycentroids_ports$dataVessel_reconstructed.ycentroid))

dataVessel_reconstructed<-dataVessel_reconstructed[-idx_ports,]

t12<-Sys.time()

cat("Out-harbor selection in in\n")
print(t12-t11)

write.csv(dataVessel_reconstructed,file = gsub(".csv","_gap_filled.csv",inputTable),row.names = F)



t1<-Sys.time()

cat("All Finished in\n")
print(t1-t0)

