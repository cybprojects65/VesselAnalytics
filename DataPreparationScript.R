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
dataVessel_bb$datetime<-as.character(paste(dataVessel_bb$date,dataVessel_bb$time))

dataVessel_bb<-dataVessel_bb[,c("x","y","speed","vesselid","datetime")]

preparedTable<-gsub(".csv","_prepared.csv",inputTable)
write.csv(x= dataVessel_bb, file = preparedTable,row.names = F)

