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

#classification of the ranges:
calc_intensity_ranges<-function(hours){

  vec<-hours[which(hours>0)]
  fit_params <- fitdistr(vec,"lognormal")
  dr<-density(vec)
  x <- dr$x
  it <- dlnorm(x, fit_params$estimate['meanlog'], fit_params$estimate['sdlog'])
  upper_thr = exp(fit_params$estimate['meanlog']+1*fit_params$estimate['sdlog'])
  lower_thr = exp(fit_params$estimate['meanlog']-1*fit_params$estimate['sdlog'])
  return (c(lower_thr,upper_thr))
    
}

ul_ratio<-calc_intensity_ranges(ratio_all_fishing_cells$ratio_unreported_total_hours)
ratio_all_fishing_cells$intensity<-"medium"
ratio_all_fishing_cells$intensity[which(ratio_all_fishing_cells$ratio_unreported_total_hours<ul_ratio[1])]<-"low"
ratio_all_fishing_cells$intensity[which(ratio_all_fishing_cells$ratio_unreported_total_hours>ul_ratio[2])]<-"high"
cat(ul_ratio[1],"<ratio<",ul_ratio[2],"\n")

tot_hours<-calc_intensity_ranges(all_fishing_cells$total_hours)
all_fishing_cells$intensity<-"medium"
all_fishing_cells$intensity[which(all_fishing_cells$total_hours<tot_hours[1])]<-"low"
all_fishing_cells$intensity[which(all_fishing_cells$total_hours>tot_hours[2])]<-"high"
cat(tot_hours[1],"<total hours<",tot_hours[2],"\n")

u_hours<-calc_intensity_ranges(unreported_cells$total_hours)
unreported_cells$intensity<-"medium"
unreported_cells$intensity[which(unreported_cells$total_hours<u_hours[1])]<-"low"
unreported_cells$intensity[which(unreported_cells$total_hours>u_hours[2])]<-"high"

cat(u_hours[1],"<unreported hours<",u_hours[2],"\n")

r_hours<-calc_intensity_ranges(reported_cells$total_hours)
reported_cells$intensity<-"medium"
reported_cells$intensity[which(reported_cells$total_hours<r_hours[1])]<-"low"
reported_cells$intensity[which(reported_cells$total_hours>r_hours[2])]<-"high"

cat(r_hours[1],"<reported hours<",r_hours[2],"\n")



#intensity classification normalised on Max Ul1 between months

#August range
ul_ratio<-calc_intensity_ranges(ratio_all_fishing_cells$ratio_unreported_total_hours)
ratio_all_fishing_cells$intensity_normalised<-"medium"
ratio_all_fishing_cells$intensity_normalised[which(ratio_all_fishing_cells$ratio_unreported_total_hours<0.0693245)]<-"low"
ratio_all_fishing_cells$intensity_normalised[which(ratio_all_fishing_cells$ratio_unreported_total_hours>0.5039603)]<-"high"

#April range
tot_hours<-calc_intensity_ranges(all_fishing_cells$total_hours)
all_fishing_cells$intensity_normalised<-"medium"
all_fishing_cells$intensity_normalised[which(all_fishing_cells$total_hours<0.2685877)]<-"low"
all_fishing_cells$intensity_normalised[which(all_fishing_cells$total_hours>2.55176)]<-"high"

#December range
u_hours<-calc_intensity_ranges(unreported_cells$total_hours)
unreported_cells$intensity_normalised<-"medium"
unreported_cells$intensity_normalised[which(unreported_cells$total_hours<0.1003729)]<-"low"
unreported_cells$intensity_normalised[which(unreported_cells$total_hours>0.4465267)]<-"high"

#April range
r_hours<-calc_intensity_ranges(reported_cells$total_hours)
reported_cells$intensity_normalised<-"medium"
reported_cells$intensity_normalised[which(reported_cells$total_hours<0.2641011)]<-"low"
reported_cells$intensity_normalised[which(reported_cells$total_hours>2.486207)]<-"high"



#Intensity Categorisation
ratio_all_fishing_cells$catint<-"1"
ratio_all_fishing_cells$catint[which(ratio_all_fishing_cells$intensity=="low")]<-0
ratio_all_fishing_cells$catint[which(ratio_all_fishing_cells$intensity=="high")]<-2

all_fishing_cells$catint<-"1"
all_fishing_cells$catint[which(all_fishing_cells$intensity=="low")]<-0
all_fishing_cells$catint[which(all_fishing_cells$intensity=="high")]<-2

unreported_cells$catint<-"1"
unreported_cells$catint[which(unreported_cells$intensity=="low")]<-0
unreported_cells$catint[which(unreported_cells$intensity=="high")]<-2

reported_cells$catint<-"1"
reported_cells$catint[which(reported_cells$intensity=="low")]<-0
reported_cells$catint[which(reported_cells$intensity=="high")]<-2

#Intensity_normalised Categorisation
ratio_all_fishing_cells$catintnorm<-"1"
ratio_all_fishing_cells$catintnorm[which(ratio_all_fishing_cells$intensity_normalised=="low")]<-0
ratio_all_fishing_cells$catintnorm[which(ratio_all_fishing_cells$intensity_normalised=="high")]<-2

all_fishing_cells$catintnorm<-"1"
all_fishing_cells$catintnorm[which(all_fishing_cells$intensity_normalised=="low")]<-0
all_fishing_cells$catintnorm[which(all_fishing_cells$intensity_normalised=="high")]<-2

unreported_cells$catintnorm<-"1"
unreported_cells$catintnorm[which(unreported_cells$intensity_normalised=="low")]<-0
unreported_cells$catintnorm[which(unreported_cells$intensity_normalised=="high")]<-2

reported_cells$catintnorm<-"1"
reported_cells$catintnorm[which(reported_cells$intensity_normalised=="low")]<-0
reported_cells$catintnorm[which(reported_cells$intensity_normalised=="high")]<-2





#save all data
write.csv(ratio_all_fishing_cells,file = gsub(".csv","_ratio_cells.csv",inputTable),row.names = F)
write.csv(unreported_cells,file = gsub(".csv","_unreported_fishing_cells.csv",inputTable),row.names = F)
write.csv(reported_cells,file = gsub(".csv","_reported_fishing_cells.csv",inputTable),row.names = F)
write.csv(all_fishing_cells,file = gsub(".csv","_total_fishing_cells.csv",inputTable),row.names = F)

