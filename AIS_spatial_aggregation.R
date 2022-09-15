rm(list=ls(all=TRUE))
library(sqldf)
library(lubridate)

inputTable<-"Med-region-5min-Fishing-vessels-2019_01.csv"
load(file = gsub(".csv","_gap_filled.Rdata",inputTable))

unreported_fishing_points<-dataVessel_reconstructed[which(dataVessel_reconstructed$point_gap=="gap_rebuilt" & dataVessel_reconstructed$potentiallyfishing=="Y"),]
all_fishing_points<-dataVessel_reconstructed[which(dataVessel_reconstructed$potentiallyfishing=="Y"),]
reported_fishing_points<-dataVessel_reconstructed[which(dataVessel_reconstructed$point_gap!="gap_rebuilt" & dataVessel_reconstructed$potentiallyfishing=="Y"),]

print(dim(all_fishing_points)[1] == dim(reported_fishing_points)[1]+dim(unreported_fishing_points)[1])

unreported_cells<-sqldf(paste0("select xcentroid, ycentroid, sum(timediff_hours_estimated) as total_hours from unreported_fishing_points group by xcentroid,ycentroid"),drv="SQLite")
all_fishing_cells<-sqldf(paste0("select xcentroid, ycentroid, sum(timediff_hours_estimated) as total_hours from all_fishing_points group by xcentroid,ycentroid"),drv="SQLite")
reported_cells<-sqldf(paste0("select xcentroid, ycentroid, sum(timediff_hours_estimated) as total_hours from reported_fishing_points group by xcentroid,ycentroid"),drv="SQLite")

ratio_all_fishing_cells<-all_fishing_cells
ratio_all_fishing_cells$xy<-paste(ratio_all_fishing_cells$xcentroid,ratio_all_fishing_cells$ycentroid,sep=";")
xy<-ratio_all_fishing_cells$xy
unreported_cells$xy<-paste(unreported_cells$xcentroid,unreported_cells$ycentroid,sep=";")

ratios<-sapply(1:length(xy), function(idx){
  xyc<-xy[idx]
  unrep_idx<-which(unreported_cells$xy==xyc)
  
  if (length(unrep_idx)>0){
    if (ratio_all_fishing_cells[idx,]$total_hours==0){
      ratio<-0
    }else{
      ratio<-unreported_cells[unrep_idx,]$total_hours/ratio_all_fishing_cells[idx,]$total_hours
    }
  }else{
    ratio<-1/ratio_all_fishing_cells[idx,]$total_hours
  }
  return (ratio)
},simplify = T)

ratio_all_fishing_cells$total_hours<-ratios
ratio_all_fishing_cells$xy<-NULL
names(ratio_all_fishing_cells)<-c("xcentroid","ycentroid","ratio_unreported_total_hours")
ratio_all_fishing_cells<-ratio_all_fishing_cells[-which(is.infinite(ratio_all_fishing_cells$ratio_unreported_total_hours)),]
#ratio_cells<-sqldf(paste0("select a.xcentroid, a.ycentroid, a.total_hours/b.total_hours from unreported_cells as a join reported_cells as b group by a.xcentroid,a.ycentroid"),drv="SQLite")

write.csv(ratio_all_fishing_cells,file = gsub(".csv","_ratio_cells.csv",inputTable),row.names = F)

