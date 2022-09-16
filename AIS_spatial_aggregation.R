rm(list=ls(all=TRUE))
library(sqldf)
library(lubridate)

inputTable<-"Med-region-5min-Fishing-vessels-2019_01.csv"
load(file = gsub(".csv","_gap_filled.Rdata",inputTable))

#extract fishing activity subsets based on labels
unreported_fishing_points<-dataVessel_reconstructed[which(dataVessel_reconstructed$point_gap=="gap_rebuilt" & dataVessel_reconstructed$potentiallyfishing=="Y"),]
all_fishing_points<-dataVessel_reconstructed[which(dataVessel_reconstructed$potentiallyfishing=="Y"),]
reported_fishing_points<-dataVessel_reconstructed[which(dataVessel_reconstructed$point_gap!="gap_rebuilt" & dataVessel_reconstructed$potentiallyfishing=="Y"),]
#check if the subsets are coherent
print(dim(all_fishing_points)[1] == dim(reported_fishing_points)[1]+dim(unreported_fishing_points)[1])

#aggregate by summing total time differences
unreported_cells<-sqldf(paste0("select xcentroid, ycentroid, sum(timediff_hours_estimated) as total_hours from unreported_fishing_points group by xcentroid,ycentroid"),drv="SQLite")
all_fishing_cells<-sqldf(paste0("select xcentroid, ycentroid, sum(timediff_hours_estimated) as total_hours from all_fishing_points group by xcentroid,ycentroid"),drv="SQLite")
reported_cells<-sqldf(paste0("select xcentroid, ycentroid, sum(timediff_hours_estimated) as total_hours from reported_fishing_points group by xcentroid,ycentroid"),drv="SQLite")

#aggregate by summing total time differences
ratio_all_fishing_cells<-all_fishing_cells
ratio_all_fishing_cells$xy<-paste(ratio_all_fishing_cells$xcentroid,ratio_all_fishing_cells$ycentroid,sep=";")
#extract cells from all fishing locations
xy<-ratio_all_fishing_cells$xy
#extract all unreported cells
unreported_cells$xy<-paste(unreported_cells$xcentroid,unreported_cells$ycentroid,sep=";")

#calculate ratios for each all-fishing location cell

ratios<-sapply(1:length(xy), function(idx){
  xyc<-xy[idx]
  unrep_idx<-which(unreported_cells$xy==xyc)
  th<-all_fishing_cells[idx,]$total_hours
  #if the cell exists in the unreported dataset do the ratio
  if (length(unrep_idx)>0){
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
#save all data
write.csv(ratio_all_fishing_cells,file = gsub(".csv","_ratio_cells.csv",inputTable),row.names = F)
write.csv(unreported_cells,file = gsub(".csv","_unreported_fishing_cells.csv",inputTable),row.names = F)
write.csv(reported_cells,file = gsub(".csv","_reported_fishing_cells.csv",inputTable),row.names = F)
write.csv(all_fishing_cells,file = gsub(".csv","_total_fishing_cells.csv",inputTable),row.names = F)

